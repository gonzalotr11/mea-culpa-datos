# =============================================================================
# PROCESAMIENTO DE SUBTÍTULOS MEA CULPA
# Lee todos los .vtt descargados → texto limpio por capítulo
# =============================================================================
# Directorio de trabajo esperado:
#   C:/Users/PC/OneDrive/Publicaciones/Proyecto Mea Culpa
# Estructura esperada:
#   subtitulos_raw/
#     temporada_01/ → *.es.vtt
#     temporada_02/ → *.es.vtt
#     ...
# =============================================================================

library(stringr)
library(stringi)   # para normalización Unicode NFC
library(dplyr)
library(readr)
library(purrr)

# -----------------------------------------------------------------------------
# PARSER VTT → TEXTO LIMPIO
# Formato YouTube karaoke: cada línea aparece 3 veces por ventana deslizante.
# Solución: eliminar tags → deduplicar consecutivos (rle) → filtrar marcadores.
# -----------------------------------------------------------------------------
parsear_vtt <- function(ruta_vtt) {
  if (!file.exists(ruta_vtt)) return(NA_character_)

  lineas <- readLines(ruta_vtt, encoding = "UTF-8", warn = FALSE)

  # --- Eliminar encabezado ---
  lineas <- lineas[!str_detect(lineas, "^WEBVTT|^Kind:|^Language:")]

  # --- Eliminar líneas de timestamp (contienen "-->") ---
  lineas <- lineas[!str_detect(lineas, "-->")]

  # --- Eliminar índices numéricos de cue ---
  lineas <- lineas[!str_detect(lineas, "^\\d+$")]

  # --- Eliminar todos los tags: <c>, </c>, <00:00:00.000>, align:..., etc. ---
  lineas <- str_remove_all(lineas, "<[^>]+>")

  # --- Eliminar marcadores de sonido: [Música], [Aplausos], [Risas], etc. ---
  lineas <- lineas[!str_detect(lineas, "^\\s*\\[.*\\]\\s*$")]

  # --- Eliminar líneas vacías o solo espacios ---
  lineas <- lineas[str_trim(lineas) != ""]

  # --- Deduplicar consecutivos (resuelve triple aparición del formato karaoke) ---
  lineas <- rle(lineas)$values

  # --- Unir en un solo bloque de texto ---
  texto <- paste(lineas, collapse = " ")
  texto <- str_squish(texto)

  # --- Normalización Unicode NFC (unifica variantes de tildes) ---
  stringi::stri_trans_nfc(texto)
}

# -----------------------------------------------------------------------------
# LECTURA DE ARCHIVOS
# -----------------------------------------------------------------------------

# Directorio raíz de los subtítulos
carpeta_raw <- "subtitulos_raw"

# Buscar todos los .vtt recursivamente
archivos_vtt <- list.files(
  path       = carpeta_raw,
  pattern    = "\\.vtt$",
  recursive  = TRUE,
  full.names = TRUE
)

if (length(archivos_vtt) == 0) {
  stop("No se encontraron archivos .vtt en: ", carpeta_raw,
       "\nVerifica que el directorio de trabajo sea correcto: ", getwd())
}

message("Archivos .vtt encontrados: ", length(archivos_vtt))

# -----------------------------------------------------------------------------
# EXTRACCIÓN DE METADATOS DESDE LA RUTA
# Estructura: subtitulos_raw/temporada_XX/VIDEO_ID.es.vtt
# -----------------------------------------------------------------------------
meta <- tibble(ruta = archivos_vtt) |>
  mutate(
    # Carpeta padre → temporada
    temporada = str_extract(ruta, "temporada_\\d+"),

    # Nombre de archivo sin extensiones → video_id
    archivo   = basename(ruta),
    video_id  = str_remove(archivo, "\\.(es|en|auto|[a-z]{2}).*\\.vtt$"),

    # Idioma del subtítulo (del nombre de archivo)
    idioma    = str_extract(archivo, "(?<=\\.)([a-z]{2}(-[A-Z0-9]+)?)(?=\\.vtt$)")
  )

# Si hay varios idiomas por video, preferir español
meta <- meta |>
  group_by(temporada, video_id) |>
  arrange(
    # Orden de preferencia: es > es-419 > es-MX > otros > en
    case_when(
      idioma == "es"     ~ 1,
      str_starts(idioma, "es") ~ 2,
      TRUE               ~ 3
    )
  ) |>
  slice(1) |>
  ungroup()

message("Capítulos únicos a procesar: ", nrow(meta))

# -----------------------------------------------------------------------------
# PROCESAMIENTO
# -----------------------------------------------------------------------------
message("\nProcesando...")

db <- meta |>
  mutate(
    texto      = map_chr(ruta, parsear_vtt),
    n_palabras = if_else(is.na(texto), 0L, str_count(texto, "\\S+"))
  ) |>
  # Ordenar: temporada → capítulo (por orden de archivo)
  arrange(temporada, video_id) |>
  # Número de capítulo dentro de cada temporada
  group_by(temporada) |>
  mutate(capitulo = row_number()) |>
  ungroup() |>
  # Columnas finales
  select(temporada, capitulo, video_id, idioma, n_palabras, texto)

# -----------------------------------------------------------------------------
# GUARDAR
# -----------------------------------------------------------------------------
# write_excel_csv agrega BOM (Byte Order Mark) → los tildes se ven bien al abrir en Excel
write_excel_csv(db, "mea_culpa_base_textos.csv")

# Resumen
resumen <- db |>
  group_by(temporada) |>
  summarise(
    capitulos      = n(),
    con_texto      = sum(!is.na(texto) & texto != ""),
    palabras_total = sum(n_palabras),
    .groups = "drop"
  )

message("\n=== RESUMEN ===")
print(resumen, n = Inf)

message("\n✓ Base guardada en: mea_culpa_base_textos.csv")
message("  Filas: ", nrow(db), " capítulos")
message("  Palabras totales: ", format(sum(db$n_palabras), big.mark = "."))

# Vista rápida: primeras palabras de cada capítulo
message("\n=== MUESTRA (primeros 100 chars por capítulo) ===")
db |>
  filter(!is.na(texto)) |>
  mutate(muestra = str_trunc(texto, 100)) |>
  select(temporada, capitulo, video_id, muestra) |>
  print(n = 20)
