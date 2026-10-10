# =============================================================================
# EXTRACCIÓN NARRATIVA — MEA CULPA
# Segunda pasada de extracción: temporalidad intra-episodio y mecanismos causales
# Genera 5 tablas nuevas (10-14) que se unen a la base actual por id_episodio
# =============================================================================
#
# PRERREQUISITOS:
#   · mea_culpa_base_textos.csv debe estar en el directorio de trabajo
#   · ANTHROPIC_API_KEY configurada (igual que en extraccion_personajes.R)
#   · install.packages(c("httr2", "jsonlite", "dplyr", "readr", "purrr", "stringr"))
#
# TABLAS QUE GENERA:
#   10_fases_narrativas.csv       — eventos en secuencia (formato largo)
#   11_mecanismos_causales.csv    — mecanismo causal del crimen (1 fila/episodio)
#   12_condiciones_estructurales.csv — condiciones de fondo (formato largo)
#   13_habilitadores.csv          — factores habilitadores (formato largo)
#   14_vulnerabilidades.csv       — vulnerabilidades por víctima (formato largo)
# =============================================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)
library(purrr)
library(stringr)

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && (is.na(a) || identical(a, "null"))) return(b)
  a
}


# =============================================================================
# CONFIGURACIÓN
# =============================================================================

API_KEY        <- Sys.getenv("ANTHROPIC_API_KEY")
# API_KEY <- "sk-ant-..."   # ← Opción B: pegar aquí temporalmente  (nunca subir la clave a GitHub)

MODELO         <- "claude-sonnet-5"
SCHEMA_VERSION <- "narrativa_1.0"

ARCHIVO_CSV    <- "mea_culpa_base_textos.csv"
CARPETA_JSON   <- "extraccion_json_narrativa"   # carpeta separada del schema v2.0

ARCHIVO_FASES  <- "10_fases_narrativas.csv"
ARCHIVO_MECA   <- "11_mecanismos_causales.csv"
ARCHIVO_COND   <- "12_condiciones_estructurales.csv"
ARCHIVO_HABIL  <- "13_habilitadores.csv"
ARCHIVO_VULN   <- "14_vulnerabilidades.csv"

PAUSA_SEG      <- 2
MODO_PRUEBA    <- FALSE


# =============================================================================
# PROMPT
# =============================================================================

PROMPT_SISTEMA <- "Eres un analista experto en narrativa criminal y criminología cualitativa.
Tu tarea es extraer la estructura temporal y causal de episodios del programa
'Mea Culpa' (TVN Chile, años 90-2000), un docudrama que relata casos reales de
crímenes, tragedias familiares y marginalidad social chilena.
El texto proviene de subtítulos auto-generados: sin mayúsculas consistentes,
sin puntuación, con errores ocasionales de transcripción.
Analiza con criterio criminológico. Marca con null lo que no puedas inferir
con seguridad desde el texto. No inventes ni rellenes con suposiciones.
Responde ÚNICAMENTE con JSON válido. Sin texto adicional, sin bloques de código."

construir_prompt <- function(texto) {
  paste0(
'Analiza este capítulo de Mea Culpa y extrae su estructura narrativa y causal.

── TAXONOMÍAS OBLIGATORIAS ─────────────────────────────────────────────────

FASES (campo "fase" en fases_narrativas):
  antecedentes | escalada | detonante | crimen | consecuencias | proceso_legal

TIPO_EVENTO (campo "tipo_evento" — etiqueta corta 2-4 palabras, consistente
entre episodios, en minúsculas con guión bajo):
  Ejemplos: inicio_convivencia | perdida_empleo | primera_agresion |
  agresion_repetida | amenaza_explicita | intento_separacion | denuncia_ignorada |
  descubrimiento_cuerpo | fuga_victimario | condena | confesion | inicio_adiccion |
  control_economico | infidelidad | deuda_impagable | discusion_final |
  nacimiento_hijo | historia_abuso_previo | trauma_sobreviviente
  (usa estas o crea otras con el mismo formato si el texto lo requiere)

TIPO_MECANISMO (campo único, elegir UNO):
  acumulacion_tension | crimen_oportunista | crimen_planificado |
  crimen_pasional | negligencia_grave | crimen_instrumental | crimen_accidental

PATRON_RELACIONAL (campo único, elegir UNO):
  violencia_intrafamiliar | conflicto_entre_pares | abuso_autoridad |
  explotacion_economica | crimen_contra_extrano | violencia_sexual | crimen_organizado

CONDICIONES_ESTRUCTURALES (lista, usar solo las que aparecen en el texto):
  pobreza | hacinamiento | desempleo | familia_disfuncional | violencia_previa |
  adiccion | aislamiento_social | marginalidad | abandono_institucional |
  impunidad_previa | desigualdad_genero | vulnerabilidad_infancia |
  migracion | endeudamiento

HABILITADORES (lista, usar solo los que aparecen en el texto):
  impunidad_previa | aislamiento_victima | dependencia_economica |
  complicidad_entorno | ausencia_institucional | acceso_armas |
  alcohol_drogas | desequilibrio_poder

TIPOS_VULNERABILIDAD (lista por víctima):
  dependencia_economica | aislamiento_social | aislamiento_geografico |
  vulnerabilidad_menor | vulnerabilidad_mayor | genero_femenino |
  adiccion | violencia_normalizada | vinculo_afectivo_con_victimario

── REGLAS ───────────────────────────────────────────────────────────────────
• fases_narrativas: ordenar cronológicamente según el relato (no según cómo
  lo narra el conductor). Incluir entre 4 y 10 eventos clave.
• Usar nombres de personas tal como aparecen en el texto (no IDs).
• tipo_evento debe ser consistente: si en otro episodio hubo "primera_agresion",
  usar exactamente esa etiqueta para el mismo tipo de evento.
• Si una fase no ocurre en este episodio (ej: no hay proceso_legal), omitirla.
• Condiciones y habilitadores: solo los que tienen evidencia textual concreta.
• Si un dato no está en el texto → null o lista vacía []. No inventar.
─────────────────────────────────────────────────────────────────────────────

{
  "schema_version": "narrativa_1.0",

  "fases_narrativas": [
    {
      "orden": 1,
      "fase": "antecedentes",
      "tipo_evento": "inicio_convivencia",
      "actores": ["Nombre1", "Nombre2"],
      "descripcion": "máx 35 palabras describiendo qué ocurre en este momento"
    }
  ],

  "mecanismo_causal": {
    "tipo_mecanismo": "acumulacion_tension",
    "patron_relacional": "violencia_intrafamiliar",
    "factor_detonante": "máx 25 palabras — el evento específico que precipita el crimen",
    "descripcion_mecanismo": "máx 60 palabras — la lógica causal completa del caso",
    "condiciones_estructurales": ["pobreza", "violencia_previa"],
    "habilitadores": ["impunidad_previa", "dependencia_economica"]
  },

  "vulnerabilidades": [
    {
      "victima": "Nombre de la víctima",
      "tipos": ["dependencia_economica", "aislamiento_social"],
      "descripcion": "máx 30 palabras sobre cómo operó la vulnerabilidad"
    }
  ]
}

── TEXTO DEL CAPÍTULO ──────────────────────────────────────────────────────
', texto, '
────────────────────────────────────────────────────────────────────────────')
}


# =============================================================================
# LLAMADA A LA API
# =============================================================================

parsear_json_respuesta <- function(texto_crudo) {
  if (is.null(texto_crudo) || nchar(texto_crudo) == 0)
    stop("La API devolvió texto vacío")

  json_str <- texto_crudo |>
    str_remove_all("```json\\s*") |>
    str_remove_all("```\\s*") |>
    str_trim()

  inicio <- str_locate(json_str, "\\{")[1, "start"]
  cierres <- str_locate_all(json_str, "\\}")[[1]]
  if (nrow(cierres) == 0)
    stop("No se encontró cierre de JSON. Chars: ", nchar(json_str))
  fin <- cierres[nrow(cierres), "end"]

  if (is.na(inicio) || is.na(fin))
    stop("No se encontró JSON válido en la respuesta")

  json_limpio <- substr(json_str, inicio, fin) |>
    # Eliminar comas colgantes antes de } o ] (error frecuente del modelo)
    str_replace_all(",\\s*([}\\]])", "\\1")

  fromJSON(json_limpio, simplifyVector = FALSE)
}

llamar_claude <- function(texto, api_key = API_KEY, modelo = MODELO) {

  respuesta <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    req_body_json(list(
      model      = modelo,
      max_tokens = 16000,
      system     = PROMPT_SISTEMA,
      messages   = list(
        list(role = "user", content = construir_prompt(texto))
      )
    )) |>
    req_timeout(180) |>
    req_retry(max_tries = 3, backoff = ~ 30) |>
    req_perform()

  body <- resp_body_json(respuesta)

  if (is.null(body$content) || length(body$content) == 0)
    stop("La API devolvió content vacío. stop_reason: ",
         body$stop_reason %||% "desconocido")

  bloques_texto <- Filter(function(b) identical(b$type, "text"), body$content)
  if (length(bloques_texto) == 0)
    stop("No se encontró bloque de texto. Tipos: ",
         paste(sapply(body$content, `[[`, "type"), collapse = ", "))

  if (identical(body$stop_reason, "max_tokens"))
    warning("⚠ Respuesta cortada por max_tokens")

  parsear_json_respuesta(bloques_texto[[1]]$text)
}


# =============================================================================
# PROCESAR UN CAPÍTULO (con checkpoint)
# =============================================================================

procesar_capitulo <- function(fila, api_key = API_KEY, modelo = MODELO) {

  id   <- paste0(fila$temporada, "_cap", sprintf("%02d", as.integer(fila$capitulo)))
  ruta <- file.path(CARPETA_JSON, paste0(id, ".json"))

  if (file.exists(ruta)) {
    resultado <- tryCatch(read_json(ruta, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(resultado) && identical(resultado$schema_version, SCHEMA_VERSION)) {
      message("  ↩  Ya procesado: ", id)
      resultado$meta <- list(
        temporada   = fila$temporada,
        capitulo    = as.integer(fila$capitulo),
        video_id    = fila$video_id,
        id_episodio = id
      )
      return(resultado)
    } else {
      message("  ↻  Reprocesando: ", id)
      file.remove(ruta)
    }
  }

  if (is.na(fila$texto) || nchar(fila$texto) < 200) {
    message("  ⚠  Sin texto: ", id)
    return(NULL)
  }

  message("  →  Procesando: ", id,
          "  (", fila$n_palabras, " palabras)",
          "  [", format(Sys.time(), "%H:%M:%S"), "]")

  resultado <- tryCatch({
    res <- llamar_claude(fila$texto, api_key, modelo)
    res$schema_version <- SCHEMA_VERSION
    write_json(res, ruta, pretty = TRUE, auto_unbox = TRUE)
    res$meta <- list(
      temporada   = fila$temporada,
      capitulo    = as.integer(fila$capitulo),
      video_id    = fila$video_id,
      id_episodio = id
    )
    Sys.sleep(PAUSA_SEG)
    res
  }, error = function(e) {
    message("  ✗  Error en ", id, ": ", conditionMessage(e))
    NULL
  })

  resultado
}


# =============================================================================
# HELPERS
# =============================================================================

expandir_lista <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  vals <- unlist(x)
  vals[!is.na(vals) & vals != "null" & nchar(vals) > 0]
}

colapsar_actores <- function(x) {
  vals <- expandir_lista(x)
  if (length(vals) == 0) return(NA_character_)
  paste(vals, collapse = " ; ")
}

aplanar_seguro <- function(lista, fn, nombre) {
  map_dfr(lista, function(r) {
    tryCatch(fn(r), error = function(e) {
      message("  ⚠ Error al aplanar ", nombre, " / ",
              r$meta$id_episodio %||% "?", ": ", conditionMessage(e))
      NULL
    })
  })
}


# =============================================================================
# FUNCIONES DE APLANADO — 5 TABLAS
# =============================================================================

# 10 FASES NARRATIVAS — una fila por evento por episodio
aplanar_fases <- function(res) {
  if (is.null(res) || length(res$fases_narrativas %||% list()) == 0) return(NULL)
  meta <- res$meta
  map_dfr(res$fases_narrativas, function(f) {
    tibble(
      id_episodio  = meta$id_episodio,
      temporada    = meta$temporada,
      capitulo     = meta$capitulo,
      orden        = as.integer(f$orden       %||% NA_integer_),
      fase         = as.character(f$fase       %||% NA_character_),
      tipo_evento  = as.character(f$tipo_evento %||% NA_character_),
      actores      = colapsar_actores(f$actores),
      descripcion  = as.character(f$descripcion %||% NA_character_)
    )
  })
}

# 11 MECANISMOS CAUSALES — una fila por episodio
aplanar_mecanismo <- function(res) {
  if (is.null(res) || is.null(res$mecanismo_causal)) return(NULL)
  m    <- res$mecanismo_causal
  meta <- res$meta
  tibble(
    id_episodio          = meta$id_episodio,
    temporada            = meta$temporada,
    capitulo             = meta$capitulo,
    tipo_mecanismo       = as.character(m$tipo_mecanismo      %||% NA_character_),
    patron_relacional    = as.character(m$patron_relacional    %||% NA_character_),
    factor_detonante     = as.character(m$factor_detonante     %||% NA_character_),
    descripcion_mecanismo= as.character(m$descripcion_mecanismo %||% NA_character_)
  )
}

# 12 CONDICIONES ESTRUCTURALES — formato largo (una fila por condición)
aplanar_condiciones <- function(res) {
  if (is.null(res) || is.null(res$mecanismo_causal)) return(NULL)
  conds <- expandir_lista(res$mecanismo_causal$condiciones_estructurales)
  if (length(conds) == 0) return(NULL)
  tibble(
    id_episodio           = res$meta$id_episodio,
    temporada             = res$meta$temporada,
    capitulo              = res$meta$capitulo,
    condicion_estructural = conds
  )
}

# 13 HABILITADORES — formato largo (una fila por habilitador)
aplanar_habilitadores <- function(res) {
  if (is.null(res) || is.null(res$mecanismo_causal)) return(NULL)
  habs <- expandir_lista(res$mecanismo_causal$habilitadores)
  if (length(habs) == 0) return(NULL)
  tibble(
    id_episodio  = res$meta$id_episodio,
    temporada    = res$meta$temporada,
    capitulo     = res$meta$capitulo,
    habilitador  = habs
  )
}

# 14 VULNERABILIDADES — una fila por víctima + tipo de vulnerabilidad
aplanar_vulnerabilidades <- function(res) {
  if (is.null(res) || length(res$vulnerabilidades %||% list()) == 0) return(NULL)
  meta <- res$meta
  map_dfr(res$vulnerabilidades, function(v) {
    tipos <- expandir_lista(v$tipos)
    if (length(tipos) == 0) tipos <- NA_character_
    tibble(
      id_episodio        = meta$id_episodio,
      temporada          = meta$temporada,
      capitulo           = meta$capitulo,
      victima            = as.character(v$victima     %||% NA_character_),
      tipo_vulnerabilidad= tipos,
      descripcion        = as.character(v$descripcion %||% NA_character_)
    )
  })
}


# =============================================================================
# EJECUCIÓN PRINCIPAL
# =============================================================================

main <- function() {

  if (API_KEY == "")
    stop("Falta ANTHROPIC_API_KEY.\n",
         "Opción A: file.edit('~/.Renviron') → ANTHROPIC_API_KEY=sk-ant-... → reiniciar R\n",
         "Opción B: descomentar la línea API_KEY <- '...' en CONFIGURACIÓN")

  dir.create(CARPETA_JSON, showWarnings = FALSE)

  db <- read_csv(ARCHIVO_CSV, show_col_types = FALSE) |>
    filter(!is.na(texto), nchar(texto) > 200) |>
    arrange(temporada, capitulo)

  archivos_json <- list.files(CARPETA_JSON, "\\.json$", full.names = TRUE)
  jsons_ok <- if (length(archivos_json) == 0) 0L else {
    sum(sapply(archivos_json, function(f) {
      j <- tryCatch(read_json(f, simplifyVector = FALSE), error = function(e) NULL)
      isTRUE(identical(j$schema_version, SCHEMA_VERSION))
    }))
  }

  # Estimación de costo (schema más pequeño que v2.0 → ~800 tokens output/cap)
  tokens_input  <- sum(db$n_palabras) * 1.4
  tokens_output <- nrow(db) * 800
  costo_haiku   <- (tokens_input * 0.80 + tokens_output * 4.00) / 1e6
  costo_sonnet  <- (tokens_input * 3.00 + tokens_output * 15.0) / 1e6

  message("════════════════════════════════════════════")
  message("  Mea Culpa — Extracción Narrativa")
  message("════════════════════════════════════════════")
  message("  Capítulos con texto  : ", nrow(db))
  message("  Ya procesados        : ", jsons_ok)
  message("  Pendientes           : ", nrow(db) - jsons_ok)
  message("  Modelo               : ", MODELO)
  message("  Costo estimado:")
  message("    claude-haiku-4-5   : $", round(costo_haiku, 2))
  message("    claude-sonnet-5    : $", round(costo_sonnet, 2))
  if (MODO_PRUEBA) message("  ⚠ MODO PRUEBA: 3 capítulos")
  message("════════════════════════════════════════════\n")

  filas <- if (MODO_PRUEBA) db[1:min(3, nrow(db)), ] else db

  resultados <- vector("list", nrow(filas))
  for (i in seq_len(nrow(filas))) {
    resultados[[i]] <- procesar_capitulo(filas[i, ])
  }
  resultados <- compact(resultados)
  message("\n✓ Capítulos procesados: ", length(resultados))

  message("Construyendo tablas...")
  fases          <- aplanar_seguro(resultados, aplanar_fases,            "fases")
  mecanismos     <- aplanar_seguro(resultados, aplanar_mecanismo,        "mecanismo")
  condiciones    <- aplanar_seguro(resultados, aplanar_condiciones,      "condiciones")
  habilitadores  <- aplanar_seguro(resultados, aplanar_habilitadores,    "habilitadores")
  vulnerabilidades <- aplanar_seguro(resultados, aplanar_vulnerabilidades, "vulnerabilidades")

  write_excel_csv(fases,           ARCHIVO_FASES)
  write_excel_csv(mecanismos,      ARCHIVO_MECA)
  write_excel_csv(condiciones,     ARCHIVO_COND)
  write_excel_csv(habilitadores,   ARCHIVO_HABIL)
  write_excel_csv(vulnerabilidades,ARCHIVO_VULN)

  message("\n════════════════ RESUMEN ════════════════")
  message(sprintf("  %-40s %4d filas", ARCHIVO_FASES,  nrow(fases)))
  message(sprintf("  %-40s %4d filas", ARCHIVO_MECA,   nrow(mecanismos)))
  message(sprintf("  %-40s %4d filas", ARCHIVO_COND,   nrow(condiciones)))
  message(sprintf("  %-40s %4d filas", ARCHIVO_HABIL,  nrow(habilitadores)))
  message(sprintf("  %-40s %4d filas", ARCHIVO_VULN,   nrow(vulnerabilidades)))

  if (nrow(mecanismos) > 0) {
    message("\n── Tipos de mecanismo ──────────────────")
    print(sort(table(mecanismos$tipo_mecanismo), decreasing = TRUE))

    message("\n── Patrones relacionales ───────────────")
    print(sort(table(mecanismos$patron_relacional), decreasing = TRUE))

    message("\n── Condiciones estructurales más frecuentes ──")
    print(sort(table(condiciones$condicion_estructural), decreasing = TRUE))

    message("\n── Habilitadores más frecuentes ────────")
    print(sort(table(habilitadores$habilitador), decreasing = TRUE))

    message("\n── Tipos de vulnerabilidad ─────────────")
    print(sort(table(vulnerabilidades$tipo_vulnerabilidad), decreasing = TRUE))

    message("\n── Fases narrativas más frecuentes ─────")
    print(sort(table(fases$fase), decreasing = TRUE))

    message("\n── Tipos de evento más frecuentes ──────")
    print(sort(table(fases$tipo_evento), decreasing = TRUE)[1:15])
  }

  invisible(list(
    fases            = fases,
    mecanismos       = mecanismos,
    condiciones      = condiciones,
    habilitadores    = habilitadores,
    vulnerabilidades = vulnerabilidades
  ))
}


# =============================================================================
# CORRER
# Para prueba inicial: cambiar MODO_PRUEBA <- TRUE arriba
# Para producción: MODO_PRUEBA <- FALSE
# =============================================================================

datos_narrativa <- main()
