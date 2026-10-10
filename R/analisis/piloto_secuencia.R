# =============================================================================
# VISUALIZACIÓN: ARCO NARRATIVO DE UN EPISODIO — MEA CULPA
# Requiere: install.packages(c("ggplot2", "patchwork", "tidyr"))
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(purrr)

EPI_ID <- "temporada_14_cap04"

# --- Cargar datos ---
pers_e  <- read_csv("04_personajes.csv",          show_col_types = FALSE) |> filter(id_episodio == EPI_ID)
fases_e <- read_csv("10_fases_narrativas.csv",    show_col_types = FALSE) |> filter(id_episodio == EPI_ID) |> arrange(orden)
meca_e  <- read_csv("11_mecanismos_causales.csv", show_col_types = FALSE) |> filter(id_episodio == EPI_ID)
epi_e   <- read_csv("01_episodios.csv",           show_col_types = FALSE) |> filter(id_episodio == EPI_ID)

# --- Paletas ---
FASES_ORDEN <- c("antecedentes","escalada","detonante","crimen","consecuencias","proceso_legal")
FASE_COLOR  <- c(antecedentes  = "#2980b9",
                 escalada      = "#e67e22",
                 detonante     = "#e74c3c",
                 crimen        = "#7b241c",
                 consecuencias = "#7d3c98",
                 proceso_legal = "#1a5276")
ROL_COLOR   <- c(victima          = "#e74c3c",
                 victimario       = "#212f3d",
                 complice         = "#e67e22",
                 autoridad        = "#2980b9",
                 tercero_afectado = "#95a5a6",
                 sin_rol_criminal = "#d5d8dc")

# =============================================================================
# PANEL 1: ARCO NARRATIVO — tiles por fase y orden
# =============================================================================

fases_plot <- fases_e |>
  mutate(
    fase  = factor(fase, levels = rev(FASES_ORDEN)),
    label = str_replace_all(tipo_evento, "_", "\n")
  )

p1 <- ggplot(fases_plot, aes(x = orden, y = fase)) +
  geom_tile(aes(fill = as.character(fase)),
            color = "white", linewidth = 1.3,
            width = 0.88, height = 0.80) +
  geom_text(aes(label = label),
            color = "white", size = 2.7,
            fontface = "bold", lineheight = 0.85) +
  scale_fill_manual(values = FASE_COLOR, name = "Fase") +
  scale_x_continuous(breaks = seq_len(nrow(fases_e)),
                     expand = expansion(add = 0.6)) +
  labs(
    title    = paste0("Arco narrativo · ", str_replace_all(EPI_ID, "_", " ")),
    subtitle = paste0(str_replace_all(epi_e$categoria_delito_principal, "_", " "),
                      "  ·  ", epi_e$periodo_temporal,
                      "  ·  ", epi_e$locacion_principal),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position   = "right",
    legend.key.size   = unit(0.45, "cm"),
    legend.text       = element_text(size = 8),
    panel.grid        = element_blank(),
    axis.text.x       = element_blank(),
    axis.text.y       = element_text(size = 9, face = "bold", color = "#2c3e50"),
    plot.title        = element_text(size = 12, face = "bold"),
    plot.subtitle     = element_text(size = 9,  color = "#555555"),
    plot.margin       = margin(10, 8, 2, 8)
  )

# =============================================================================
# PANEL 2: PRESENCIA DE PERSONAJES POR EVENTO
# =============================================================================

# Expandir actores (separados por " ; ")
participacion <- fases_e |>
  select(orden, actores) |>
  mutate(actor = str_split(actores, " ; ")) |>
  unnest(actor) |>
  mutate(actor = str_trim(actor)) |>
  filter(!is.na(actor), actor != "", actor != "NA") |>
  # Cruzar con personajes por primera palabra del nombre (match flexible)
  mutate(
    nombre_match = map_chr(actor, function(a) {
      primera <- str_split(a, " ")[[1]][1]
      m <- pers_e$nombre[str_detect(pers_e$nombre, regex(primera, ignore_case = TRUE))]
      if (length(m) > 0) m[1] else a
    })
  ) |>
  left_join(pers_e |> select(nombre, rol_criminologico),
            by = c("nombre_match" = "nombre"))

# Ordenar personajes: victimario arriba, luego víctimas, resto
orden_roles <- c("victimario","complice","victima","autoridad","tercero_afectado","sin_rol_criminal")
pers_niveles <- pers_e |>
  mutate(ord = match(coalesce(rol_criminologico, "sin_rol_criminal"), orden_roles)) |>
  arrange(ord) |>
  pull(nombre)

participacion <- participacion |>
  mutate(pers_factor = factor(nombre_match, levels = rev(pers_niveles)))

p2 <- ggplot(participacion, aes(x = orden, y = pers_factor)) +
  # Línea horizontal de referencia por personaje
  geom_hline(aes(yintercept = as.numeric(pers_factor)),
             color = "#ecf0f1", linewidth = 4) +
  # Puntos de participación
  geom_point(aes(color = coalesce(rol_criminologico, "sin_rol_criminal")),
             size = 5.5, alpha = 0.9) +
  geom_point(color = "white", size = 2) +
  scale_color_manual(values = ROL_COLOR, name = "Rol") +
  scale_x_continuous(breaks = seq_len(nrow(fases_e)),
                     expand = expansion(add = 0.6)) +
  labs(x = "Orden narrativo", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position   = "right",
    legend.key.size   = unit(0.45, "cm"),
    legend.text       = element_text(size = 8),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.text.y       = element_text(size = 8.5, color = "#2c3e50"),
    axis.text.x       = element_text(size = 8.5),
    plot.margin       = margin(2, 8, 6, 8)
  )

# =============================================================================
# PANEL 3: MECANISMO CAUSAL — caja de texto
# =============================================================================

meca_label <- paste0(
  "MECANISMO: ", str_replace_all(meca_e$tipo_mecanismo, "_", " "), "   ·   ",
  "PATRÓN: ", str_replace_all(meca_e$patron_relacional, "_", " "), "\n",
  "Detonante: ", meca_e$factor_detonante
)

p3 <- ggplot() +
  annotate("text", x = 0.5, y = 0.5,
           label   = str_wrap(meca_label, 110),
           size    = 3.1, hjust = 0.5, vjust = 0.5,
           color   = "#2c3e50", lineheight = 1.4) +
  xlim(0,1) + ylim(0,1) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#f4f6f7", color = "#d5d8dc", linewidth = 0.5),
    plot.margin     = margin(6, 15, 6, 15)
  )

# =============================================================================
# COMPOSICIÓN FINAL
# =============================================================================

viz_final <- p1 / p2 / p3 +
  plot_layout(heights = c(4, 2.5, 0.9)) +
  plot_annotation(
    caption = "Fuente: Mea Culpa (TVN Chile) · Extracción automatizada con Claude API",
    theme   = theme(plot.caption = element_text(size = 7.5, color = "#888"))
  )

viz_final

# Guardar PNG
ggsave(
  filename = paste0("arco_narrativo_", EPI_ID, ".png"),
  plot     = viz_final,
  width    = 15,
  height   = 9,
  dpi      = 220,
  bg       = "white"
)
message("✓ Guardado: arco_narrativo_", EPI_ID, ".png")