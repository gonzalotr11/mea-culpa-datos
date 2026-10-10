# =============================================================================
# DESCARGA DE SUBTÍTULOS DE MEA CULPA (YouTube) CON R
# =============================================================================
# Requiere: yt-dlp instalado y en PATH
#   Instalar: winget install yt-dlp
#   O descargar: https://github.com/yt-dlp/yt-dlp/releases
# =============================================================================

library(stringr)
library(dplyr)
library(readr)
library(purrr)

# -----------------------------------------------------------------------------
# CONFIGURACIÓN
# -----------------------------------------------------------------------------

# Playlists por temporada (14 temporadas)
# Nota: se usa el formato playlist?list=ID (no watch?v=...) para que
# yt-dlp procese la playlist completa desde el primer capítulo.
PLAYLISTS <- list(
  temporada_01 = "https://www.youtube.com/playlist?list=PLd5MCGg25bV_HsjJ059ZhmAlYACRXHeKU",
  temporada_02 = "https://www.youtube.com/playlist?list=PLd5MCGg25bV_g230b51PsrC3_C0TyEtc8",
  temporada_03 = "https://www.youtube.com/playlist?list=PLd5MCGg25bV9ILtVrqfR077vr3O2P1key",
  temporada_04 = "https://www.youtube.com/playlist?list=PLd5MCGg25bV8abhMeZd4AEzmGyzW-2FWw",
  temporada_05 = "https://www.youtube.com/playlist?list=PLd5MCGg25bV9LAvhXX8tal5xnAQdghrlj",
  temporada_06 = "https://www.youtube.com/playlist?list=PLd5MCGg25bV-zXQzrq9n__ZMrYvhI2oiY",
  temporada_07 = "https://www.youtube.com/playlist?list=PLd5MCGg25bV-1U9NxmallFVOR3__wHygx",
  temporada_08 = "https://www.youtube.com/playlist?list=PLd5MCGg25bV9wQhf1z3Ox87JqtxK8ncHc",
  temporada_09 = "https://www.youtube.com/playlist?list=PLd5MCGg25bV8Q-goS3LloJhFjf0dQ8HSy",
  temporada_10 = "https://www.youtube.com/playlist?list=PLd5MCGg25bV9hoRzSTkQW9mR-5GSD4vNa",
  temporada_11 = "https://www.youtube.com/playlist?list=PLd5MCGg25bV9N0KiJ44cvtH6X4Gk8Jlqj",
  temporada_12 = "https://www.youtube.com/playlist?list=PLd5MCGg25bV_rPzvnhhnXI5_Sb9UA3wt8",
  temporada_13 = "https://www.youtube.com/playlist?list=PLd5MCGg25bV8O8xq95ZkbUvkD_1hYKxXM",
  temporada_14 = "https://www.youtube.com/playlist?list=PLd5MCGg25bV8N9dZzXOYr4BVSi_K38K_V"
)

# Carpeta donde se guardarán los archivos descargados
CARPETA_OUTPUT <- "subtitulos_raw"
ARCHIVO_DB     <- "mea_culpa_subtitulos.csv"

# Idioma preferido (es = español, es-419 = español latinoamérica)
IDIOMAS <- "es,es-419,es-MX,en"

# Ruta a yt-dlp (si no está en PATH, pon la ruta completa aquí)
YTDLP <- "yt-dlp"  # o "C:/Windows/System32/yt-dlp.exe"

# -----------------------------------------------------------------------------
# FUNCIONES
# -----------------------------------------------------------------------------

#' Verificar que yt-dlp está instalado
verificar_ytdlp <- function() {
  resultado <- tryCatch(
    system2(YTDLP, "--version", stdout = TRUE, stderr = FALSE),
    error = function(e) NULL
  )
  if (is.null(resultado)) {
    stop("yt-dlp no encontrado. Instálalo con: winget install yt-dlp")
  }
  message("yt-dlp versión: ", resultado)
  invisible(TRUE)
}

#' Obtener lista de videos de una playlist
obtener_videos_playlist <- function(url_playlist) {
  message("Obteniendo lista de videos de: ", url_playlist)

  args <- c(
    "--flat-playlist",
    "--print", "%(playlist_index)s|%(id)s|%(title)s|%(duration)s",
    "--no-warnings",
    url_playlist
  )

  lineas <- system2(YTDLP, args, stdout = TRUE, stderr = FALSE)

  if (length(lineas) == 0) {
    warning("No se encontraron videos en: ", url_playlist)
    return(NULL)
  }

  # Parsear output: índice|id|título|duración
  partes <- str_split_fixed(lineas, "\\|", 4)

  data.frame(
    indice   = as.integer(partes[, 1]),
    video_id = partes[, 2],
    titulo   = partes[, 3],
    duracion = as.integer(partes[, 4]),
    url      = paste0("https://www.youtube.com/watch?v=", partes[, 2]),
    stringsAsFactors = FALSE
  )
}

#' Descargar subtítulos de un video individual
descargar_subtitulos_video <- function(video_id, carpeta_destino, idiomas = IDIOMAS) {
  url <- paste0("https://www.youtube.com/watch?v=", video_id)

  args <- c(
    "--write-subs",          # subtítulos manuales (primera preferencia)
    "--write-auto-subs",     # subtítulos automáticos (fallback)
    "--sub-langs", idiomas,
    "--sub-format", "vtt",   # formato WebVTT (fácil de parsear)
    "--skip-download",       # NO descargar el video, solo subtítulos
    "--no-warnings",
    "--output", file.path(carpeta_destino, "%(id)s.%(ext)s"),
    url
  )

  resultado <- system2(YTDLP, args, stdout = TRUE, stderr = TRUE)

  # Buscar el archivo .vtt descargado
  archivos_vtt <- list.files(
    carpeta_destino,
    pattern = paste0(video_id, ".*\\.vtt$"),
    full.names = TRUE
  )

  if (length(archivos_vtt) == 0) {
    message("  ⚠ Sin subtítulos disponibles para: ", video_id)
    return(NULL)
  }

  # Preferir subtítulos en español si hay varios idiomas
  if (length(archivos_vtt) > 1) {
    es_idx <- which(str_detect(archivos_vtt, "\\.es"))
    if (length(es_idx) > 0) archivos_vtt <- archivos_vtt[es_idx[1]]
  }

  message("  ✓ Descargado: ", basename(archivos_vtt[1]))
  archivos_vtt[1]
}

#' Parsear archivo VTT → texto limpio
#'
#' Formato: YouTube usa VTT estilo "karaoke" con timestamps por palabra (<c>).
#' Cada línea aparece hasta 3 veces por el sistema de ventana deslizante:
#'   (1) cue principal con tags <c> (palabras creciendo)
#'   (2) cue de transición de 10ms (línea completada, limpia)
#'   (3) como contexto en el siguiente cue principal
#' Estrategia: eliminar tags → deduplicar consecutivos (rle) → filtrar marcadores.
#' Esto captura también la primera línea del episodio, que el método 10ms-only pierde.
parsear_vtt <- function(ruta_vtt) {
  if (is.null(ruta_vtt) || !file.exists(ruta_vtt)) return(NA_character_)

  lineas <- readLines(ruta_vtt, encoding = "UTF-8", warn = FALSE)

  # Eliminar encabezado WEBVTT
  lineas <- lineas[!str_detect(lineas, "^WEBVTT|^Kind:|^Language:")]

  # Eliminar líneas de timestamps (formato: 00:00:01.000 --> 00:00:05.000)
  lineas <- lineas[!str_detect(lineas, "-->")]

  # Eliminar líneas numéricas de índice de cue
  lineas <- lineas[!str_detect(lineas, "^\\d+$")]

  # Eliminar tags de YouTube: word-timestamps <00:00:00.000> y <c>...</c>
  lineas <- str_remove_all(lineas, "<[^>]+>")

  # Eliminar marcadores de sonido: [Música], [Aplausos], [Risas], etc.
  lineas <- lineas[!str_detect(lineas, "^\\s*\\[.*\\]\\s*$")]

  # Eliminar líneas vacías o solo espacios
  lineas <- lineas[str_trim(lineas) != ""]

  # Eliminar duplicados consecutivos (resuelve el patrón de triple aparición del VTT)
  lineas <- rle(lineas)$values

  # Unir todo como texto continuo
  texto <- paste(lineas, collapse = " ")
  texto <- str_squish(texto)

  texto
}

#' Procesar una temporada completa
procesar_temporada <- function(nombre_temporada, url_playlist, carpeta_base = CARPETA_OUTPUT) {
  message("\n========================================")
  message("PROCESANDO: ", nombre_temporada)
  message("========================================")

  # Crear carpeta para esta temporada
  carpeta_temp <- file.path(carpeta_base, nombre_temporada)
  dir.create(carpeta_temp, showWarnings = FALSE, recursive = TRUE)

  # Obtener lista de capítulos
  videos <- obtener_videos_playlist(url_playlist)
  if (is.null(videos)) return(NULL)

  message("Capítulos encontrados: ", nrow(videos))

  # Descargar y parsear subtítulos de cada capítulo
  resultados <- map_dfr(seq_len(nrow(videos)), function(i) {
    v <- videos[i, ]
    message("\nCapítulo ", v$indice, ": ", v$titulo)

    ruta_vtt <- descargar_subtitulos_video(v$video_id, carpeta_temp)
    texto    <- parsear_vtt(ruta_vtt)

    # Pequeña pausa para no saturar YouTube
    Sys.sleep(runif(1, 1.5, 3.5))

    data.frame(
      temporada  = nombre_temporada,
      capitulo   = v$indice,
      titulo     = v$titulo,
      video_id   = v$video_id,
      url        = v$url,
      duracion_s = v$duracion,
      texto      = texto,
      n_palabras = if (!is.na(texto)) str_count(texto, "\\S+") else 0L,
      stringsAsFactors = FALSE
    )
  })

  resultados
}

# -----------------------------------------------------------------------------
# EJECUCIÓN PRINCIPAL
# -----------------------------------------------------------------------------

main <- function() {
  verificar_ytdlp()
  dir.create(CARPETA_OUTPUT, showWarnings = FALSE)

  # Procesar todas las temporadas
  todas <- map2_dfr(
    names(PLAYLISTS),
    PLAYLISTS,
    procesar_temporada
  )

  # Guardar base de datos
  write_csv(todas, ARCHIVO_DB)
  message("\n✓ Base de datos guardada en: ", ARCHIVO_DB)
  message("Total de capítulos: ", nrow(todas))
  message("Con subtítulos: ", sum(!is.na(todas$texto)))

  # Resumen por temporada
  resumen <- todas |>
    group_by(temporada) |>
    summarise(
      capitulos       = n(),
      con_subtitulos  = sum(!is.na(texto)),
      palabras_total  = sum(n_palabras, na.rm = TRUE),
      .groups = "drop"
    )

  print(resumen)
  invisible(todas)
}

# Ejecutar
db <- main()


getwd()
