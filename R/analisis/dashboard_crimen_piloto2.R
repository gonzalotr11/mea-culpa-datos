# =============================================================================
# RED SOCIO-MATERIAL INTERACTIVA — MEA CULPA
# Requiere: install.packages(c("visNetwork", "igraph"))
# Correr desde el mismo directorio que las tablas CSV
# =============================================================================
install.packages("visNetwork")
install.packages("igraph")

library(dplyr)
library(readr)
library(stringr)
library(visNetwork)
library(igraph)

# =============================================================================
# 1. CARGAR TABLAS
# =============================================================================

episodios  <- read_csv("01_episodios.csv",           show_col_types = FALSE)
personajes <- read_csv("04_personajes.csv",           show_col_types = FALSE)
relaciones <- read_csv("05_relaciones.csv",           show_col_types = FALSE)
vp         <- read_csv("06_vinculos_practicos.csv",   show_col_types = FALSE)
va         <- read_csv("07_vinculos_afectivos.csv",   show_col_types = FALSE)
entidades  <- read_csv("08_entidades.csv",            show_col_types = FALSE)
rel_pe     <- read_csv("09_relaciones_pers_ent.csv",  show_col_types = FALSE)

# =============================================================================
# 2. SELECCIONAR EPISODIO MÁS RICO EN CONEXIONES
# =============================================================================

epi_sel <- episodios |> filter(id_episodio == "temporada_13_cap08") 

epi_id <- epi_sel$id_episodio

message("Episodio seleccionado: ", epi_id)
message("  Resumen: ", str_trunc(epi_sel$resumen, 120))

# Filtrar todas las tablas al episodio elegido
pers_e <- personajes |> filter(id_episodio == epi_id)
rel_e  <- relaciones |> filter(id_episodio == epi_id)
ent_e  <- entidades  |> filter(id_episodio == epi_id)
rpe_e  <- rel_pe     |> filter(id_episodio == epi_id)
vp_e   <- vp         |> filter(id_episodio == epi_id)
va_e   <- va         |> filter(id_episodio == epi_id)

# =============================================================================
# 3. PALETAS DE COLORES
# =============================================================================

color_rol <- c(
  victima          = "#e74c3c",   # rojo
  victimario       = "#1c2833",   # negro azulado
  complice         = "#e67e22",   # naranja
  autoridad        = "#2980b9",   # azul
  tercero_afectado = "#7f8c8d",   # gris medio
  sin_rol_criminal = "#d5d8dc"    # gris claro
)

color_entidad <- c(
  lugar            = "#1e8449",   # verde oscuro
  organizacion     = "#7d3c98",   # violeta
  objeto_material  = "#b7950b",   # dorado
  sustancia        = "#922b21",   # rojo oscuro
  objeto_economico = "#117a65",   # verde azulado
  documento        = "#5d6d7e",   # gris azulado
  vehiculo         = "#1a5276",   # azul oscuro
  arma             = "#c0392b"    # rojo
)

color_asimetria <- c(
  from_domina = "#e74c3c",
  to_domina   = "#2980b9",
  par         = "#aab7b8",
  variable    = "#e67e22",
  incierta    = "#d5d8dc"
)

# Helper para lookups seguros
lookup <- function(x, tabla, default = "#aab7b8") {
  res <- tabla[x]
  res[is.na(res)] <- default
  unname(res)
}

# =============================================================================
# 4. NODOS — PERSONAJES
# =============================================================================

nodos_pers <- pers_e |>
  mutate(
    id        = id_personaje_global,
    label     = str_wrap(coalesce(nombre, id_personaje), 14),
    shape     = "dot",
    color     = lookup(coalesce(rol_criminologico, "sin_rol_criminal"), color_rol),
    size      = 18 + pmax(0, pmin(4, coalesce(ses_nivel, 3L) - 1L)) * 7,
    font.size = 13,
    group     = coalesce(rol_criminologico, "sin_rol_criminal"),
    title     = paste0(
      "<b>", coalesce(nombre, id_personaje), "</b><br>",
      "<i>", coalesce(rol_narrativo, "—"), " / ", coalesce(rol_criminologico, "—"), "</i><br>",
      "Ocupación: ", coalesce(ocupacion, "—"), "<br>",
      "SES: ", coalesce(as.character(ses_nivel), "?"), " / 5<br>",
      if_else(!is.na(ses_evidencia), paste0("<i>", ses_evidencia, "</i><br>"), ""),
      if_else(!is.na(descripcion),  paste0("<br>", descripcion), "")
    )
  ) |>
  select(id, label, shape, color, size, font.size, group, title)

# =============================================================================
# 5. NODOS — ENTIDADES NO HUMANAS
# =============================================================================

tam_centralidad <- c(baja = 20, media = 30, alta = 42)

nodos_ent <- ent_e |>
  mutate(
    id        = id_entidad_global,
    label     = str_wrap(coalesce(nombre, id_entidad), 14),
    shape     = "square",
    color     = lookup(coalesce(tipo, "objeto_material"), color_entidad, "#aab7b8"),
    size      = unname(tam_centralidad[coalesce(centralidad, "media")]),
    font.size = 12,
    group     = paste0("[", coalesce(tipo, "objeto"), "]"),
    title     = paste0(
      "<b>", coalesce(nombre, id_entidad), "</b><br>",
      "<i>", coalesce(tipo, "—"), " · ", coalesce(subtipo, "—"), "</i><br>",
      "Función: <b>", coalesce(funcion_narrativa, "—"), "</b><br>",
      "Centralidad: ", coalesce(centralidad, "?"), "<br>",
      if_else(!is.na(descripcion), paste0("<br>", descripcion), "")
    )
  ) |>
  select(id, label, shape, color, size, font.size, group, title)

nodos <- bind_rows(nodos_pers, nodos_ent)

# =============================================================================
# 6. ARISTAS — PERSONA ↔ PERSONA
# =============================================================================

vp_res <- vp_e |>
  group_by(id_relacion) |>
  summarise(vp = paste(vinculo_practico, collapse = " · "), .groups = "drop")

va_res <- va_e |>
  group_by(id_relacion) |>
  summarise(va = paste(vinculo_afectivo, collapse = " · "), .groups = "drop")

aristas_pp <- rel_e |>
  left_join(vp_res, by = "id_relacion") |>
  left_join(va_res, by = "id_relacion") |>
  filter(
    from_personaje_global %in% nodos$id,
    to_personaje_global   %in% nodos$id
  ) |>
  mutate(
    from   = from_personaje_global,
    to     = to_personaje_global,
    color  = lookup(coalesce(asimetria_poder, "par"), color_asimetria),
    arrows = case_when(
      asimetria_poder == "from_domina" ~ "to",
      asimetria_poder == "to_domina"   ~ "from",
      TRUE                             ~ ""
    ),
    dashes = FALSE,
    width  = 2.5,
    title  = paste0(
      "<b>Poder: ", coalesce(asimetria_poder, "?"), "</b><br>",
      "Práctico: ", coalesce(vp, "—"), "<br>",
      "Afectivo: ", coalesce(va, "—"), "<br>",
      if_else(!is.na(descripcion), paste0("<i>", descripcion, "</i>"), "")
    )
  ) |>
  select(from, to, color, arrows, dashes, width, title)

# =============================================================================
# 7. ARISTAS — PERSONA ↔ ENTIDAD
# =============================================================================

aristas_pe <- rpe_e |>
  filter(
    id_personaje_global %in% nodos$id,
    id_entidad_global   %in% nodos$id
  ) |>
  mutate(
    from   = id_personaje_global,
    to     = id_entidad_global,
    color  = "#aab7b8",
    arrows = "to",
    dashes = TRUE,
    width  = 1.5,
    title  = paste0(
      "<b>", coalesce(tipo_vinculo, "usa"), "</b><br>",
      "Asimetría: ", coalesce(asimetria, "?"), "<br>",
      if_else(!is.na(descripcion), paste0("<i>", descripcion, "</i>"), "")
    )
  ) |>
  select(from, to, color, arrows, dashes, width, title)

aristas <- bind_rows(aristas_pp, aristas_pe)

# =============================================================================
# 8. VISUALIZACIÓN
# =============================================================================

subtitulo <- paste0(
  epi_id, " | T", epi_sel$temporada, " Cap", epi_sel$capitulo,
  " | Delito: ", coalesce(epi_sel$categoria_delito_principal, "—"),
  " | Período: ", coalesce(epi_sel$periodo_temporal, "—")
)

# Nodos de leyenda (uno por color/tipo)
leyenda_nodos <- bind_rows(
  tibble(
    label = names(color_rol),
    color = unname(color_rol),
    shape = "dot"
  ),
  tibble(
    label = names(color_entidad),
    color = unname(color_entidad),
    shape = "square"
  )
)

red <- visNetwork(
  nodes = nodos,
  edges = aristas,
  main  = list(text = "Red socio-material · Mea Culpa",
               style = "font-size:16px; font-weight:bold; text-align:center"),
  submain = list(text = subtitulo,
                 style = "font-size:12px; text-align:center; color:#555"),
  width  = "100%",
  height = "780px"
) |>
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
    nodesIdSelection = list(enabled = TRUE, main = "Seleccionar nodo"),
    selectedBy       = list(variable = "group", main = "Filtrar por grupo")
  ) |>
  visPhysics(
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(
      gravitationalConstant = -90,
      springLength          = 130,
      springConstant        = 0.05
    ),
    stabilization = list(enabled = TRUE, iterations = 250)
  ) |>
  visLayout(randomSeed = 42) |>
  visInteraction(navigationButtons = TRUE, tooltipDelay = 80, hover = TRUE) |>
  visLegend(
    addNodes  = leyenda_nodos,
    useGroups = FALSE,
    position  = "right",
    ncol      = 1,
    width     = 0.18
  )

red   # muestra en el Viewer de RStudio

# =============================================================================
# 9. TABLA DE CENTRALIDAD
# =============================================================================

g <- graph_from_data_frame(
  d        = aristas |> select(from, to),
  vertices = nodos   |> select(id, label, group),
  directed = FALSE
)

tabla_central <- tibble(
  nodo        = V(g)$name,
  etiqueta    = V(g)$label |> str_replace_all("\n", " "),
  tipo        = V(g)$group,
  conexiones  = degree(g),
  centralidad = round(betweenness(g, normalized = TRUE), 3)
) |>
  arrange(desc(conexiones), desc(centralidad))

message("\n══ Nodos más conectados en ", epi_id, " ══")
print(tabla_central, n = 30)

# Exportar tabla de centralidad
write_excel_csv(tabla_central, paste0("centralidad_", epi_id, ".csv"))
message("\n✓ Tabla guardada: centralidad_", epi_id, ".csv")
