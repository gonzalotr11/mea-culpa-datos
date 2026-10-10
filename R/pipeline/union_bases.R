# =============================================================================
# UNIÓN DE BASES — MEA CULPA
# Une las 9 tablas de personajes/relaciones con las 5 tablas narrativas
# Genera: 00_master_episodios.csv + mea_culpa_completa.xlsx (15 hojas)
# =============================================================================

library(dplyr)
library(readr)
library(stringr)
library(openxlsx)
library(tidyr)

# =============================================================================
# 1. CARGAR LAS 14 TABLAS
# =============================================================================

message("Cargando tablas...")

episodios      <- read_csv("01_episodios.csv",                show_col_types = FALSE)
epi_delitos    <- read_csv("02_episodios_delitos.csv",        show_col_types = FALSE)
epi_temas      <- read_csv("03_episodios_temas.csv",          show_col_types = FALSE)
personajes     <- read_csv("04_personajes.csv",               show_col_types = FALSE)
relaciones     <- read_csv("05_relaciones.csv",               show_col_types = FALSE)
vinc_prac      <- read_csv("06_vinculos_practicos.csv",       show_col_types = FALSE)
vinc_afec      <- read_csv("07_vinculos_afectivos.csv",       show_col_types = FALSE)
entidades      <- read_csv("08_entidades.csv",                show_col_types = FALSE)
rel_pe         <- read_csv("09_relaciones_pers_ent.csv",      show_col_types = FALSE)
fases          <- read_csv("10_fases_narrativas.csv",         show_col_types = FALSE)
mecanismos     <- read_csv("11_mecanismos_causales.csv",      show_col_types = FALSE)
condiciones    <- read_csv("12_condiciones_estructurales.csv",show_col_types = FALSE)
habilitadores  <- read_csv("13_habilitadores.csv",            show_col_types = FALSE)
vulnerabilidades <- read_csv("14_vulnerabilidades.csv",       show_col_types = FALSE)

message("  Tablas cargadas: 14")

# =============================================================================
# 2. RESÚMENES POR EPISODIO DESDE LAS TABLAS LARGAS
# =============================================================================

# Fases: n_fases + lista de tipos de evento
resumen_fases <- fases |>
  group_by(id_episodio) |>
  summarise(
    n_fases       = n(),
    fases_presentes = paste(sort(unique(fase)), collapse = " ; "),
    tipos_evento   = paste(na.omit(tipo_evento), collapse = " ; "),
    .groups = "drop"
  )

# Condiciones estructurales: n + lista colapsada
resumen_condiciones <- condiciones |>
  group_by(id_episodio) |>
  summarise(
    n_condiciones           = n(),
    condiciones_lista       = paste(sort(condicion_estructural), collapse = " ; "),
    .groups = "drop"
  )

# Habilitadores: n + lista colapsada
resumen_habilitadores <- habilitadores |>
  group_by(id_episodio) |>
  summarise(
    n_habilitadores         = n(),
    habilitadores_lista     = paste(sort(habilitador), collapse = " ; "),
    .groups = "drop"
  )

# Vulnerabilidades: n víctimas + tipos únicos colapsados
resumen_vuln <- vulnerabilidades |>
  group_by(id_episodio) |>
  summarise(
    n_victimas_vulnerables  = n_distinct(victima, na.rm = TRUE),
    tipos_vulnerabilidad    = paste(sort(unique(tipo_vulnerabilidad)), collapse = " ; "),
    .groups = "drop"
  )

# Temas del episodio (de la tabla original)
resumen_temas <- epi_temas |>
  group_by(id_episodio) |>
  summarise(
    temas_lista = paste(tema, collapse = " ; "),
    .groups = "drop"
  )

# SES promedio de personajes por episodio
resumen_ses <- personajes |>
  filter(!is.na(ses_nivel)) |>
  group_by(id_episodio) |>
  summarise(
    ses_promedio = round(mean(ses_nivel, na.rm = TRUE), 2),
    ses_min      = min(ses_nivel, na.rm = TRUE),
    ses_max      = max(ses_nivel, na.rm = TRUE),
    .groups = "drop"
  )

# Proporción víctimas/victimarios por episodio
resumen_roles <- personajes |>
  group_by(id_episodio) |>
  summarise(
    n_victimas_pers    = sum(rol_criminologico == "victima",    na.rm = TRUE),
    n_victimarios_pers = sum(rol_criminologico == "victimario", na.rm = TRUE),
    n_complices_pers   = sum(rol_criminologico == "complice",   na.rm = TRUE),
    n_autoridades_pers = sum(rol_criminologico == "autoridad",  na.rm = TRUE),
    .groups = "drop"
  )

# =============================================================================
# 3. TABLA MAESTRA (1 FILA POR EPISODIO)
# =============================================================================

master <- episodios |>
  # Unir mecanismo causal (1:1)
  left_join(
    mecanismos |> select(id_episodio, tipo_mecanismo, patron_relacional,
                         factor_detonante, descripcion_mecanismo),
    by = "id_episodio"
  ) |>
  # Unir resúmenes de tablas largas
  left_join(resumen_fases,         by = "id_episodio") |>
  left_join(resumen_condiciones,   by = "id_episodio") |>
  left_join(resumen_habilitadores, by = "id_episodio") |>
  left_join(resumen_vuln,          by = "id_episodio") |>
  left_join(resumen_temas,         by = "id_episodio") |>
  left_join(resumen_ses,           by = "id_episodio") |>
  left_join(resumen_roles,         by = "id_episodio") |>
  # Reemplazar NAs en conteos por 0
  mutate(across(c(n_fases, n_condiciones, n_habilitadores,
                  n_victimas_vulnerables, n_victimas_pers,
                  n_victimarios_pers, n_complices_pers, n_autoridades_pers),
                ~ replace_na(.x, 0L))) |>
  arrange(temporada, capitulo)

# =============================================================================
# 4. DIAGNÓSTICO DE COBERTURA
# =============================================================================

n_total    <- nrow(master)
n_con_meca <- sum(!is.na(master$tipo_mecanismo))
n_con_fases<- sum(!is.na(master$n_fases) & master$n_fases > 0)

message("\n════════════════ COBERTURA ════════════════")
message(sprintf("  Episodios totales          : %d", n_total))
message(sprintf("  Con mecanismo causal       : %d (%.0f%%)",
                n_con_meca, 100 * n_con_meca / n_total))
message(sprintf("  Con fases narrativas       : %d (%.0f%%)",
                n_con_fases, 100 * n_con_fases / n_total))

# Episodios sin extracción narrativa
sin_narrativa <- master |>
  filter(is.na(tipo_mecanismo)) |>
  select(id_episodio, temporada, capitulo)

if (nrow(sin_narrativa) > 0) {
  message("\n  ⚠ Episodios sin extracción narrativa:")
  print(sin_narrativa, n = Inf)
}

# =============================================================================
# 5. EXPORTAR CSV MAESTRO
# =============================================================================

write_excel_csv(master, "00_master_episodios.csv")
message("\n✓ Guardado: 00_master_episodios.csv  (", nrow(master), " filas × ",
        ncol(master), " columnas)")

# =============================================================================
# 6. EXCEL COMPLETO CON 15 HOJAS
# =============================================================================

message("Construyendo Excel...")

todas_las_tablas <- list(
  "00_master"              = master,
  "01_episodios"           = episodios,
  "02_epi_delitos"         = epi_delitos,
  "03_epi_temas"           = epi_temas,
  "04_personajes"          = personajes,
  "05_relaciones"          = relaciones,
  "06_vinc_practicos"      = vinc_prac,
  "07_vinc_afectivos"      = vinc_afec,
  "08_entidades"           = entidades,
  "09_rel_pers_entidad"    = rel_pe,
  "10_fases_narrativas"    = fases,
  "11_mecanismos"          = mecanismos,
  "12_condiciones"         = condiciones,
  "13_habilitadores"       = habilitadores,
  "14_vulnerabilidades"    = vulnerabilidades
)

wb <- createWorkbook()

iwalk(todas_las_tablas, function(df, nombre) {
  addWorksheet(wb, nombre)
  writeDataTable(wb, nombre, df,
                 tableStyle    = "TableStyleMedium2",
                 withFilter    = TRUE)
  setColWidths(wb, nombre,
               cols   = seq_len(ncol(df)),
               widths = "auto")
})

saveWorkbook(wb, "mea_culpa_completa.xlsx", overwrite = TRUE)
message("✓ Guardado: mea_culpa_completa.xlsx  (15 hojas)")

# =============================================================================
# 7. RESUMEN ANALÍTICO
# =============================================================================

message("\n════════════ RESUMEN ANALÍTICO ════════════")

message("\n── Tipos de mecanismo ──────────────────────")
print(sort(table(master$tipo_mecanismo), decreasing = TRUE))

message("\n── Patrones relacionales ───────────────────")
print(sort(table(master$patron_relacional), decreasing = TRUE))

message("\n── Condiciones estructurales ───────────────")
print(sort(table(condiciones$condicion_estructural), decreasing = TRUE))

message("\n── Habilitadores ───────────────────────────")
print(sort(table(habilitadores$habilitador), decreasing = TRUE))

message("\n── Tipos de vulnerabilidad ─────────────────")
print(sort(table(vulnerabilidades$tipo_vulnerabilidad), decreasing = TRUE))

message("\n── Mecanismo × patrón relacional ───────────")
print(table(master$tipo_mecanismo, master$patron_relacional))

message("\n── SES promedio por tipo de mecanismo ──────")
master |>
  filter(!is.na(tipo_mecanismo), !is.na(ses_promedio)) |>
  group_by(tipo_mecanismo) |>
  summarise(ses_medio = round(mean(ses_promedio), 2), n = n(), .groups = "drop") |>
  arrange(ses_medio) |>
  print()

message("\n── Fases más frecuentes ────────────────────")
print(sort(table(fases$fase), decreasing = TRUE))

message("\n── Tipos de evento más frecuentes ──────────")
fases |>
  count(tipo_evento, sort = TRUE) |>
  filter(!is.na(tipo_evento)) |>
  slice_head(n = 20) |>
  print()

invisible(list(master = master, tablas = todas_las_tablas))