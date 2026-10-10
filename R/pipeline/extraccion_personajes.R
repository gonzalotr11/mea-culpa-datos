# =============================================================================
# EXTRACCIÓN DE PERSONAJES, ENTIDADES Y RELACIONES — MEA CULPA v2.0
# =============================================================================
#
# CAMBIOS RESPECTO A v1.0:
#   · Red socio-material: tabla entidades_no_humanas + relaciones_persona_entidad
#   · IDs globales: id_personaje_global = id_episodio_P01, id_relacion = id_episodio_R001
#   · Roles separados: rol_narrativo (posición en relato) vs rol_criminologico
#   · Asimetría de poder: from_domina / to_domina (reemplaza A_domina / B_domina)
#   · Categoría de delito primaria y secundaria
#   · Tablas largo para vínculos y temas
#   · Detección de JSONs del schema v1.0 → reprocesa automáticamente
#   · 9 tablas de salida en vez de 3
#
# SETUP INICIAL (solo una vez):
#   install.packages(c("httr2", "jsonlite", "dplyr", "readr", "purrr", "stringr"))
#
# API KEY — dos opciones:
#   Opción A (recomendada): guardar en .Renviron
#     file.edit("~/.Renviron")  →  agregar línea:  ANTHROPIC_API_KEY=sk-ant-...
#     Reiniciar R después de guardar.
#   Opción B (temporal):
#     Descomentar la línea API_KEY <- "sk-ant-..." más abajo.
#
# COSTO ESTIMADO v2.0: ~$2–3 USD para 123 capítulos con claude-haiku-4-5.
#   (schema más extenso → ~30% más tokens de output que v1.0)
# =============================================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)
library(purrr)
library(stringr)

# Operador null-coalescing: devuelve b si a es NULL, NA, vacío, o el string "null"
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && (is.na(a) || identical(a, "null"))) return(b)
  a
}


# =============================================================================
# CONFIGURACIÓN
# =============================================================================

API_KEY      <- Sys.getenv("ANTHROPIC_API_KEY")
# API_KEY <- "sk-ant-..."   # ← Opción B: pegar aquí temporalmente  (nunca subir la clave a GitHub)

# MODELO     <- "claude-haiku-4-5-20251001"   # rápido — OJO: max 8192 tokens output
MODELO       <- "claude-sonnet-5"             # recomendado para schema v2.0 (64K output)

SCHEMA_VERSION <- "2.0"                        # detecta JSONs v1.0 y los reprocesa

ARCHIVO_CSV  <- "mea_culpa_base_textos.csv"
CARPETA_JSON <- "extraccion_json"

# Tablas de salida (9 archivos)
ARCHIVO_EPI          <- "01_episodios.csv"
ARCHIVO_DELITOS      <- "02_episodios_delitos.csv"
ARCHIVO_TEMAS        <- "03_episodios_temas.csv"
ARCHIVO_PERS         <- "04_personajes.csv"
ARCHIVO_REL          <- "05_relaciones.csv"
ARCHIVO_VIN_PRAC     <- "06_vinculos_practicos.csv"
ARCHIVO_VIN_AFEC     <- "07_vinculos_afectivos.csv"
ARCHIVO_ENTIDADES    <- "08_entidades.csv"
ARCHIVO_PERS_ENT     <- "09_relaciones_persona_entidad.csv"

PAUSA_SEG    <- 2      # segundos entre llamadas API
MODO_PRUEBA  <- FALSE  # TRUE = solo procesa los primeros 3 capítulos


# =============================================================================
# PROMPT v2.0
# =============================================================================

PROMPT_SISTEMA <- "Eres un analista experto en narrativa televisiva y periodismo chileno.
Tu tarea es extraer información estructurada de transcripciones de subtítulos
del programa 'Mea Culpa' (TVN Chile, años 90-2000), un docudrama que relata
casos reales de crímenes, tragedias familiares y marginalidad social chilena.
El conductor entrevista al protagonista y otros involucrados; se mezclan
narración, testimonio en primera persona y reconstrucción dramatizada.
Los textos vienen de subtítulos auto-generados: sin mayúsculas consistentes,
sin puntuación, con errores ocasionales de transcripción.
Extrae con criterio periodístico. Marca con null lo que no puedas inferir
con seguridad. No inventes ni rellenes con suposiciones.
Responde ÚNICAMENTE con JSON válido. Sin texto adicional, sin bloques de código."

construir_prompt <- function(texto) {
  paste0(
'Analiza este capítulo de Mea Culpa y completa el schema JSON.

── REGLAS ──────────────────────────────────────────────────────────────────
PERSONAJES (máx. 12):
• Incluir solo quienes tienen peso narrativo real: mencionados >1 vez Y con
  al menos un atributo identificable.
• Para personas sin nombre usa descripción relacional: "Madre de X", "Vecino".
• ses_nivel: 1=miseria/exclusión · 2=clase baja · 3=clase media ·
  4=clase media-alta · 5=clase alta. Basar SOLO en evidencia textual.
• rol_narrativo (posición en el relato): protagonista | antagonista |
  testigo | apoyo_protagonista | figura_autoridad | personaje_menor
• rol_criminologico (función en el crimen): victima | victimario |
  complice | autoridad | tercero_afectado | sin_rol_criminal
  (puede diferir del narrativo; una víctima puede ser protagonista)

ENTIDADES NO HUMANAS (máx. 10):
• Objetos, lugares, instituciones o sustancias con peso causal o simbólico
  en la historia. Omitir menciones incidentales sin función narrativa.
• tipos: lugar | organizacion | objeto_material | sustancia |
  objeto_economico | documento | vehiculo | arma
• funcion_narrativa: instrumento_del_delito | objeto_disputado |
  recurso_supervivencia | fuente_de_conflicto | medio_de_control |
  evidencia | lugar_del_delito | espacio_vulnerabilidad |
  institucion_restauradora | institucion_fallida | simbolo_moral |
  recurso_estatus | medio_movilidad | medio_ocultamiento

RELACIONES PERSONA–PERSONA (máx. 20):
• Solo pares con interacción concreta descrita en el texto.
• "from" y "to" son ids de personaje (P01, P02…).
• asimetria_poder: from_domina | to_domina | par | variable | incierta

RELACIONES PERSONA–ENTIDAD:
• Vincular personajes con entidades cuando el vínculo tiene relevancia
  narrativa o explica la dinámica del caso.
• tipo_vinculo: vive_en | trabaja_en | posee | usa | debe | roba |
  administra | controla | amenaza_con | compra | vende | consume |
  victima_en | victimario_en | denuncia_en | frecuenta

GENERAL:
• IDs de personaje: P01, P02… (consistentes en toda la respuesta)
• IDs de entidad: E01, E02… (consistentes en toda la respuesta)
• Si un dato no está en el texto → null. No inventar.
────────────────────────────────────────────────────────────────────────────

{
  "schema_version": "2.0",

  "episodio": {
    "resumen": "string de ~50 palabras describiendo el caso central",
    "periodo_temporal": "año exacto o aproximado (ej: 1989, fines_80s, inicio_90s)",
    "locacion_principal": "ciudad o comuna donde ocurre la historia",
    "locaciones_otras": ["otras ciudades o lugares mencionados"],
    "tipo_caso": "UNO DE: delito_violento | abuso_familiar | delito_economico | caso_politico | drama_sentimental | abandono_negligencia | adiccion | marginalidad | otro",
    "categoria_delito_principal": "UNO DE: homicidio_intencional | homicidio_accidental | femicidio | parricidio | homicidio_serial | violencia_sexual | abuso_infantil | violencia_intrafamiliar | robo_asalto | secuestro | trafico_drogas | fraude_estafa | caso_politico | abandono_negligencia | drama_sin_delito | otro",
    "categoria_delito_secundaria": "null o MISMO VOCABULARIO si hay un segundo delito relevante",
    "hay_homicidio": true,
    "hay_menor_involucrado": false,
    "n_victimas": "UNO DE: 1 | 2_a_5 | mas_de_5 | null",
    "temas": ["temas secundarios: pobreza, exilio, infidelidad, drogas, etc."]
  },

  "personajes": [
    {
      "id": "P01",
      "nombre": "Nombre Apellido — o descripción relacional si es anónimo",
      "sexo": "M | F | ND",
      "grupo_etario": "nino | joven | adulto | mayor | ND",
      "rol_narrativo": "protagonista | antagonista | testigo | apoyo_protagonista | figura_autoridad | personaje_menor",
      "rol_criminologico": "victima | victimario | complice | autoridad | tercero_afectado | sin_rol_criminal",
      "es_victima": true,
      "es_victimario": false,
      "es_complice": false,
      "ocupacion": "string o null",
      "ses_nivel": 2,
      "ses_evidencia": "máx 10 palabras con evidencia textual del nivel socioeconómico",
      "descripcion": "máx 20 palabras sobre su rol en el capítulo"
    }
  ],

  "entidades_no_humanas": [
    {
      "id": "E01",
      "nombre": "nombre del lugar, objeto, institución o sustancia",
      "tipo": "lugar | organizacion | objeto_material | sustancia | objeto_economico | documento | vehiculo | arma",
      "subtipo": "texto libre — ej: vivienda_informal, deuda_informal, empresa_formal",
      "funcion_narrativa": "instrumento_del_delito | objeto_disputado | recurso_supervivencia | fuente_de_conflicto | medio_de_control | evidencia | lugar_del_delito | espacio_vulnerabilidad | institucion_restauradora | institucion_fallida | simbolo_moral | recurso_estatus | medio_movilidad | medio_ocultamiento",
      "centralidad": "baja | media | alta",
      "descripcion": "máx 15 palabras sobre su rol en el episodio"
    }
  ],

  "relaciones": [
    {
      "from": "P01",
      "to": "P02",
      "simetrica": true,
      "vinculos_afectivos": ["amor | odio | miedo | dependencia | resentimiento | lastima | proteccion | desconfianza | admiracion | indiferencia"],
      "vinculos_practicos": ["cohabitan | parentesco | trabajan_juntos | relacion_economica | patron_empleado | complicidad_criminal | victimario_victima | vecinos | relacion_legal | conocidos"],
      "asimetria_poder": "from_domina | to_domina | par | variable | incierta",
      "descripcion": "máx 15 palabras sobre la naturaleza del vínculo"
    }
  ],

  "relaciones_persona_entidad": [
    {
      "personaje": "P01",
      "entidad": "E01",
      "tipo_vinculo": "vive_en | trabaja_en | posee | usa | debe | roba | administra | controla | amenaza_con | compra | vende | consume | victima_en | victimario_en | denuncia_en | frecuenta",
      "asimetria": "persona_domina | entidad_condiciona | institucion_domina | variable",
      "descripcion": "máx 10 palabras"
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

  ubicaciones_cierre <- str_locate_all(json_str, "\\}")[[1]]
  if (nrow(ubicaciones_cierre) == 0)
    stop("No se encontró cierre de JSON (posible EOF prematuro). ",
         "Chars recibidos: ", nchar(json_str))
  fin <- ubicaciones_cierre[nrow(ubicaciones_cierre), "end"]

  if (is.na(inicio) || is.na(fin))
    stop("No se encontró JSON válido en la respuesta")

  substr(json_str, inicio, fin) |>
    fromJSON(simplifyVector = FALSE)
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
      # 32K tokens: Sonnet-5 usa extended thinking por defecto; thinking + output caben con margen.
      # Los bloques "thinking" se filtran automáticamente más abajo.
      max_tokens = 32000,
      system     = PROMPT_SISTEMA,
      messages   = list(
        list(role = "user", content = construir_prompt(texto))
      )
    )) |>
    req_timeout(180) |>
    req_retry(max_tries = 3, backoff = ~ 30) |>
    req_perform()

  body <- resp_body_json(respuesta)

  # Verificar que la respuesta tiene contenido de texto
  if (is.null(body$content) || length(body$content) == 0)
    stop("La API devolvió content vacío. stop_reason: ",
         body$stop_reason %||% "desconocido")

  # Sonnet-5 puede devolver un bloque "thinking" antes del texto — buscar el bloque de tipo "text"
  bloques_texto <- Filter(function(b) identical(b$type, "text"), body$content)
  if (length(bloques_texto) == 0)
    stop("No se encontró bloque de texto en la respuesta. ",
         "Tipos recibidos: ", paste(sapply(body$content, `[[`, "type"), collapse = ", "),
         " | stop_reason: ", body$stop_reason %||% "desconocido")
  bloque <- bloques_texto[[1]]

  # Advertir si el output fue cortado
  if (!is.null(body$stop_reason) && body$stop_reason == "max_tokens")
    warning("⚠ Respuesta cortada por max_tokens — considera reducir el schema")

  parsear_json_respuesta(bloque$text)
}


# =============================================================================
# PROCESAR UN CAPÍTULO (con checkpoint y detección de schema antiguo)
# =============================================================================

procesar_capitulo <- function(fila, api_key = API_KEY, modelo = MODELO) {

  id   <- paste0(fila$temporada, "_cap", sprintf("%02d", as.integer(fila$capitulo)))
  ruta <- file.path(CARPETA_JSON, paste0(id, ".json"))

  # Checkpoint: verificar si existe Y es schema v2.0
  if (file.exists(ruta)) {
    resultado <- tryCatch(read_json(ruta, simplifyVector = FALSE), error = function(e) NULL)

    if (!is.null(resultado) &&
        !is.null(resultado$schema_version) &&
        resultado$schema_version == SCHEMA_VERSION) {
      message("  ↩  Ya procesado (v2.0): ", id)
      resultado$meta <- list(
        temporada   = fila$temporada,
        capitulo    = as.integer(fila$capitulo),
        video_id    = fila$video_id,
        id_episodio = id
      )
      return(resultado)
    } else {
      message("  ↻  Schema antiguo, reprocesando: ", id)
      file.remove(ruta)
    }
  }

  # Sin texto → saltar
  if (is.na(fila$texto) || nchar(fila$texto) < 200) {
    message("  ⚠  Sin texto: ", id)
    return(NULL)
  }

  message("  →  Procesando: ", id,
          "  (", fila$n_palabras, " palabras)",
          "  [", format(Sys.time(), "%H:%M:%S"), "]")

  resultado <- tryCatch({

    res <- llamar_claude(fila$texto, api_key, modelo)

    # Forzar schema_version en el JSON guardado
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
# HELPERS PARA APLANAR
# =============================================================================

# Colapsar lista/vector en string separado por " ; "
colapsar <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  vals <- unlist(x)
  vals <- vals[!is.na(vals) & vals != "null"]
  if (length(vals) == 0) return(NA_character_)
  paste(vals, collapse = " ; ")
}

# Expandir lista en tibble de una columna (formato largo)
expandir_lista <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  vals <- unlist(x)
  vals[!is.na(vals) & vals != "null"]
}


# =============================================================================
# FUNCIONES DE APLANADO — 9 TABLAS
# =============================================================================

# 01 EPISODIOS — una fila por capítulo
aplanar_episodio <- function(res) {
  if (is.null(res)) return(NULL)
  epi  <- res$episodio
  meta <- res$meta
  tibble(
    id_episodio                = meta$id_episodio,
    temporada                  = meta$temporada,
    capitulo                   = meta$capitulo,
    video_id                   = meta$video_id,
    resumen                    = epi$resumen                    %||% NA_character_,
    periodo_temporal           = epi$periodo_temporal           %||% NA_character_,
    locacion_principal         = epi$locacion_principal         %||% NA_character_,
    locaciones_otras           = colapsar(epi$locaciones_otras),
    tipo_caso                  = epi$tipo_caso                  %||% NA_character_,
    categoria_delito_principal = epi$categoria_delito_principal %||% NA_character_,
    categoria_delito_secundaria= epi$categoria_delito_secundaria %||% NA_character_,
    hay_homicidio              = epi$hay_homicidio              %||% NA,
    hay_menor_involucrado      = epi$hay_menor_involucrado      %||% NA,
    n_victimas                 = as.character(epi$n_victimas    %||% NA_character_),
    n_personajes               = length(res$personajes %||% list()),
    n_entidades                = length(res$entidades_no_humanas %||% list()),
    n_relaciones_pp            = length(res$relaciones %||% list()),
    n_relaciones_pe            = length(res$relaciones_persona_entidad %||% list())
  )
}

# 02 EPISODIOS_DELITOS — formato largo (una fila por categoría de delito)
aplanar_episodio_delitos <- function(res) {
  if (is.null(res)) return(NULL)
  epi  <- res$episodio
  meta <- res$meta

  filas <- list()

  cat_ppal <- epi$categoria_delito_principal %||% NULL
  if (!is.null(cat_ppal) && !is.na(cat_ppal) && cat_ppal != "null") {
    filas[[length(filas) + 1]] <- tibble(
      id_episodio      = meta$id_episodio,
      categoria_delito = as.character(cat_ppal),
      es_principal     = TRUE
    )
  }

  cat_sec <- epi$categoria_delito_secundaria %||% NULL
  if (!is.null(cat_sec) && !is.na(cat_sec) && cat_sec != "null") {
    filas[[length(filas) + 1]] <- tibble(
      id_episodio      = meta$id_episodio,
      categoria_delito = as.character(cat_sec),
      es_principal     = FALSE
    )
  }

  if (length(filas) == 0) return(NULL)
  bind_rows(filas)
}

# 03 EPISODIOS_TEMAS — formato largo (una fila por tema)
aplanar_episodio_temas <- function(res) {
  if (is.null(res)) return(NULL)
  epi  <- res$episodio
  meta <- res$meta

  temas <- expandir_lista(epi$temas)
  if (length(temas) == 0) return(NULL)

  tibble(
    id_episodio = meta$id_episodio,
    tema        = temas,
    orden       = seq_along(temas)
  )
}

# 04 PERSONAJES — una fila por personaje por episodio
aplanar_personajes <- function(res) {
  if (is.null(res) || is.null(res$personajes) || length(res$personajes) == 0) return(NULL)
  meta <- res$meta
  map_dfr(res$personajes, function(p) {
    id_local  <- p$id %||% NA_character_
    id_global <- if (!is.na(id_local)) paste0(meta$id_episodio, "_", id_local) else NA_character_
    tibble(
      id_episodio          = meta$id_episodio,
      temporada            = meta$temporada,
      capitulo             = meta$capitulo,
      id_personaje         = id_local,
      id_personaje_global  = id_global,
      nombre               = p$nombre           %||% NA_character_,
      sexo                 = p$sexo             %||% NA_character_,
      grupo_etario         = p$grupo_etario     %||% NA_character_,
      rol_narrativo        = p$rol_narrativo    %||% NA_character_,
      rol_criminologico    = p$rol_criminologico %||% NA_character_,
      es_victima           = p$es_victima       %||% NA,
      es_victimario        = p$es_victimario    %||% NA,
      es_complice          = p$es_complice      %||% NA,
      ocupacion            = p$ocupacion        %||% NA_character_,
      ses_nivel            = as.integer(p$ses_nivel %||% NA_integer_),
      ses_evidencia        = p$ses_evidencia    %||% NA_character_,
      descripcion          = p$descripcion      %||% NA_character_
    )
  })
}

# 05 RELACIONES PERSONA–PERSONA — una fila por par
aplanar_relaciones <- function(res) {
  if (is.null(res) || is.null(res$relaciones) || length(res$relaciones) == 0) return(NULL)
  meta <- res$meta
  imap_dfr(res$relaciones, function(r, i) {
    from_local <- r$from %||% r$de %||% NA_character_  # retrocompat. con campo "de"
    to_local   <- r$to   %||% r$a  %||% NA_character_  # retrocompat. con campo "a"
    id_rel     <- paste0(meta$id_episodio, "_R", sprintf("%03d", i))
    tibble(
      id_relacion          = id_rel,
      id_episodio          = meta$id_episodio,
      temporada            = meta$temporada,
      capitulo             = meta$capitulo,
      from_personaje       = from_local,
      to_personaje         = to_local,
      from_personaje_global= if (!is.na(from_local)) paste0(meta$id_episodio, "_", from_local) else NA_character_,
      to_personaje_global  = if (!is.na(to_local))   paste0(meta$id_episodio, "_", to_local)   else NA_character_,
      simetrica            = r$simetrica        %||% NA,
      asimetria_poder      = r$asimetria_poder  %||% NA_character_,
      descripcion          = r$descripcion      %||% NA_character_
    )
  })
}

# 06 VÍNCULOS PRÁCTICOS — formato largo (una fila por tipo de vínculo práctico)
aplanar_vinculos_practicos <- function(res) {
  if (is.null(res) || is.null(res$relaciones) || length(res$relaciones) == 0) return(NULL)
  meta <- res$meta
  imap_dfr(res$relaciones, function(r, i) {
    vinculos <- expandir_lista(r$vinculos_practicos)
    if (length(vinculos) == 0) return(NULL)
    id_rel <- paste0(meta$id_episodio, "_R", sprintf("%03d", i))
    tibble(
      id_relacion     = id_rel,
      id_episodio     = meta$id_episodio,
      vinculo_practico = vinculos
    )
  })
}

# 07 VÍNCULOS AFECTIVOS — formato largo (una fila por tipo de vínculo afectivo)
aplanar_vinculos_afectivos <- function(res) {
  if (is.null(res) || is.null(res$relaciones) || length(res$relaciones) == 0) return(NULL)
  meta <- res$meta
  imap_dfr(res$relaciones, function(r, i) {
    vinculos <- expandir_lista(r$vinculos_afectivos)
    if (length(vinculos) == 0) return(NULL)
    id_rel <- paste0(meta$id_episodio, "_R", sprintf("%03d", i))
    tibble(
      id_relacion      = id_rel,
      id_episodio      = meta$id_episodio,
      vinculo_afectivo = vinculos
    )
  })
}

# 08 ENTIDADES NO HUMANAS — una fila por entidad por episodio
aplanar_entidades <- function(res) {
  if (is.null(res) || is.null(res$entidades_no_humanas) ||
      length(res$entidades_no_humanas) == 0) return(NULL)
  meta <- res$meta
  map_dfr(res$entidades_no_humanas, function(e) {
    id_local  <- e$id %||% NA_character_
    id_global <- if (!is.na(id_local)) paste0(meta$id_episodio, "_", id_local) else NA_character_
    tibble(
      id_episodio        = meta$id_episodio,
      temporada          = meta$temporada,
      capitulo           = meta$capitulo,
      id_entidad         = id_local,
      id_entidad_global  = id_global,
      nombre             = e$nombre            %||% NA_character_,
      tipo               = e$tipo              %||% NA_character_,
      subtipo            = e$subtipo           %||% NA_character_,
      funcion_narrativa  = e$funcion_narrativa %||% NA_character_,
      centralidad        = e$centralidad       %||% NA_character_,
      descripcion        = e$descripcion       %||% NA_character_
    )
  })
}

# 09 RELACIONES PERSONA–ENTIDAD — una fila por vínculo persona↔entidad
aplanar_relaciones_persona_entidad <- function(res) {
  if (is.null(res) || is.null(res$relaciones_persona_entidad) ||
      length(res$relaciones_persona_entidad) == 0) return(NULL)
  meta <- res$meta
  imap_dfr(res$relaciones_persona_entidad, function(r, i) {
    p_local <- r$personaje %||% NA_character_
    e_local <- r$entidad   %||% NA_character_
    tibble(
      id_episodio          = meta$id_episodio,
      temporada            = meta$temporada,
      capitulo             = meta$capitulo,
      id_personaje         = p_local,
      id_entidad           = e_local,
      id_personaje_global  = if (!is.na(p_local)) paste0(meta$id_episodio, "_", p_local) else NA_character_,
      id_entidad_global    = if (!is.na(e_local)) paste0(meta$id_episodio, "_", e_local) else NA_character_,
      tipo_vinculo         = r$tipo_vinculo %||% NA_character_,
      asimetria            = r$asimetria    %||% NA_character_,
      descripcion          = r$descripcion  %||% NA_character_
    )
  })
}


# =============================================================================
# EJECUCIÓN PRINCIPAL
# =============================================================================

main <- function() {

  if (API_KEY == "") {
    stop(
      "Falta ANTHROPIC_API_KEY.\n",
      "Opción A: file.edit('~/.Renviron') → agregar ANTHROPIC_API_KEY=sk-ant-... → reiniciar R\n",
      "Opción B: descomentar la línea API_KEY <- '...' en CONFIGURACIÓN"
    )
  }

  dir.create(CARPETA_JSON, showWarnings = FALSE)

  # Cargar base de textos
  db <- read_csv(ARCHIVO_CSV, show_col_types = FALSE) |>
    filter(!is.na(texto), nchar(texto) > 200) |>
    arrange(temporada, capitulo)

  # Contar JSONs por versión de schema
  jsons_existentes <- list.files(CARPETA_JSON, "\\.json$", full.names = TRUE)
  jsons_v2 <- sum(sapply(jsons_existentes, function(f) {
    j <- tryCatch(read_json(f, simplifyVector = FALSE), error = function(e) NULL)
    !is.null(j) && identical(j$schema_version, SCHEMA_VERSION)
  }))
  jsons_v1 <- length(jsons_existentes) - jsons_v2

  # Estimación de costo
  tokens_input  <- sum(db$n_palabras) * 1.4
  tokens_output <- nrow(db) * 2000          # v2.0: ~2000 tokens output/cap (más que v1)
  costo_haiku   <- (tokens_input * 0.80 + tokens_output * 4.00) / 1e6
  costo_sonnet  <- (tokens_input * 3.00 + tokens_output * 15.0) / 1e6

  message("════════════════════════════════════════════")
  message("  Mea Culpa — Extracción v2.0 (red socio-material)")
  message("════════════════════════════════════════════")
  message("  Capítulos con texto : ", nrow(db))
  message("  JSONs v2.0 (listos) : ", jsons_v2)
  if (jsons_v1 > 0)
    message("  JSONs v1.0 (a reprocesar): ", jsons_v1)
  message("  Modelo              : ", MODELO)
  message("  Costo estimado total:")
  message("    claude-haiku-4-5  : $", round(costo_haiku, 2))
  message("    claude-sonnet-5   : $", round(costo_sonnet, 2))
  if (MODO_PRUEBA) message("  ⚠ MODO PRUEBA: solo 3 capítulos")
  message("════════════════════════════════════════════\n")

  filas <- if (MODO_PRUEBA) db[1:min(3, nrow(db)), ] else db

  # Procesar capítulo por capítulo
  resultados <- vector("list", nrow(filas))
  for (i in seq_len(nrow(filas))) {
    resultados[[i]] <- procesar_capitulo(filas[i, ])
  }
  resultados <- compact(resultados)
  message("\n✓ Capítulos procesados: ", length(resultados))

  # Aplanar a 9 dataframes
  # Cada episodio se aplana individualmente con tryCatch: un error no para la exportación.
  message("Construyendo tablas...")

  aplanar_seguro <- function(lista_res, fn, nombre) {
    map_dfr(lista_res, function(r) {
      tryCatch(fn(r), error = function(e) {
        message("  ⚠ Error al aplanar ", nombre, " en ",
                r$meta$id_episodio %||% "?", ": ", conditionMessage(e))
        NULL
      })
    })
  }

  episodios           <- aplanar_seguro(resultados, aplanar_episodio,                  "episodio")
  episodios_delitos   <- aplanar_seguro(resultados, aplanar_episodio_delitos,           "delitos")
  episodios_temas     <- aplanar_seguro(resultados, aplanar_episodio_temas,             "temas")
  personajes          <- aplanar_seguro(resultados, aplanar_personajes,                 "personajes")
  relaciones          <- aplanar_seguro(resultados, aplanar_relaciones,                 "relaciones")
  vinculos_practicos  <- aplanar_seguro(resultados, aplanar_vinculos_practicos,         "vinculos_prac")
  vinculos_afectivos  <- aplanar_seguro(resultados, aplanar_vinculos_afectivos,         "vinculos_afec")
  entidades           <- aplanar_seguro(resultados, aplanar_entidades,                  "entidades")
  relaciones_pers_ent <- aplanar_seguro(resultados, aplanar_relaciones_persona_entidad, "rel_pers_ent")

  # Guardar CSVs con BOM para Excel
  write_excel_csv(episodios,           ARCHIVO_EPI)
  write_excel_csv(episodios_delitos,   ARCHIVO_DELITOS)
  write_excel_csv(episodios_temas,     ARCHIVO_TEMAS)
  write_excel_csv(personajes,          ARCHIVO_PERS)
  write_excel_csv(relaciones,          ARCHIVO_REL)
  write_excel_csv(vinculos_practicos,  ARCHIVO_VIN_PRAC)
  write_excel_csv(vinculos_afectivos,  ARCHIVO_VIN_AFEC)
  write_excel_csv(entidades,           ARCHIVO_ENTIDADES)
  write_excel_csv(relaciones_pers_ent, ARCHIVO_PERS_ENT)

  # Resumen
  message("\n════════════════ RESUMEN ════════════════")
  message(sprintf("  %-38s %4d filas", ARCHIVO_EPI,       nrow(episodios)))
  message(sprintf("  %-38s %4d filas", ARCHIVO_DELITOS,   nrow(episodios_delitos)))
  message(sprintf("  %-38s %4d filas", ARCHIVO_TEMAS,     nrow(episodios_temas)))
  message(sprintf("  %-38s %4d filas", ARCHIVO_PERS,      nrow(personajes)))
  message(sprintf("  %-38s %4d filas", ARCHIVO_REL,       nrow(relaciones)))
  message(sprintf("  %-38s %4d filas", ARCHIVO_VIN_PRAC,  nrow(vinculos_practicos)))
  message(sprintf("  %-38s %4d filas", ARCHIVO_VIN_AFEC,  nrow(vinculos_afectivos)))
  message(sprintf("  %-38s %4d filas", ARCHIVO_ENTIDADES, nrow(entidades)))
  message(sprintf("  %-38s %4d filas", ARCHIVO_PERS_ENT,  nrow(relaciones_pers_ent)))

  if (nrow(personajes) > 0 && nrow(episodios) > 0) {
    n_epi <- nrow(episodios)
    message("\n── Promedios por capítulo ──────────────")
    message(sprintf("  Personajes       : %.1f", nrow(personajes)          / n_epi))
    message(sprintf("  Entidades        : %.1f", nrow(entidades)           / n_epi))
    message(sprintf("  Relaciones P–P   : %.1f", nrow(relaciones)          / n_epi))
    message(sprintf("  Relaciones P–E   : %.1f", nrow(relaciones_pers_ent) / n_epi))

    message("\n── Roles criminológicos ────────────────")
    print(sort(table(personajes$rol_criminologico), decreasing = TRUE))

    message("\n── Roles narrativos ────────────────────")
    print(sort(table(personajes$rol_narrativo), decreasing = TRUE))

    message("\n── Tipos de entidad ────────────────────")
    if (nrow(entidades) > 0)
      print(sort(table(entidades$tipo), decreasing = TRUE))

    message("\n── Función narrativa entidades ─────────")
    if (nrow(entidades) > 0)
      print(sort(table(entidades$funcion_narrativa), decreasing = TRUE))

    message("\n── Tipos de caso ───────────────────────")
    print(sort(table(episodios$tipo_caso), decreasing = TRUE))

    message("\n── Delito principal ────────────────────")
    print(sort(table(episodios$categoria_delito_principal), decreasing = TRUE))
  }

  invisible(list(
    episodios           = episodios,
    episodios_delitos   = episodios_delitos,
    episodios_temas     = episodios_temas,
    personajes          = personajes,
    relaciones          = relaciones,
    vinculos_practicos  = vinculos_practicos,
    vinculos_afectivos  = vinculos_afectivos,
    entidades           = entidades,
    relaciones_pers_ent = relaciones_pers_ent
  ))
}


# =============================================================================
# CORRER
# Para una prueba inicial: cambiar MODO_PRUEBA <- TRUE arriba
# Para producción completa: MODO_PRUEBA <- FALSE
#
# NOTA: Los JSONs del schema v1.0 en extraccion_json/ se reprocessarán
# automáticamente al ejecutar este script.
# =============================================================================

datos <- main()
