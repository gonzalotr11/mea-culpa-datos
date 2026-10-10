# ============================================================
# Análisis simple de redes — Proyecto Mea Culpa
# Pregunta:
# ¿En qué capítulo los vínculos económicos aparecen con mayor fuerza,
# y qué posición ocupan víctima, victimario y otros personajes en esa red?
#
# Autor: Gonzalo Torres / apoyo ChatGPT
# Directorio esperado:
# C:/Users/PC/OneDrive/Publicaciones/Proyecto Mea Culpa
# ============================================================


# ============================================================
# 0. Paquetes
# ============================================================

paquetes <- c(
  "readxl", "dplyr", "tidyr", "stringr", "janitor",
  "igraph", "ggraph", "ggplot2", "forcats", "scales", "writexl"
)

paquetes_faltantes <- paquetes[!paquetes %in% installed.packages()[, "Package"]]

if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(igraph)
library(ggraph)
library(ggplot2)
library(forcats)
library(scales)
library(writexl)


# ============================================================
# 1. Directorio y carga de datos
# ============================================================

# Si ya estás en este directorio, no necesitas setwd().
# setwd("C:/Users/PC/OneDrive/Publicaciones/Proyecto Mea Culpa")

cat("Directorio actual:\n")
print(getwd())

archivo <- "mea_culpa_base_completa.xlsx"

if (!file.exists(archivo)) {
  stop(
    "No encuentro el archivo 'mea_culpa_base_completa.xlsx' en el directorio actual. ",
    "Revisa getwd() o cambia el directorio con setwd()."
  )
}

cat("\nHojas disponibles en el Excel:\n")
print(excel_sheets(archivo))

episodios <- read_excel(archivo, sheet = "episodios") %>%
  clean_names()

personajes <- read_excel(archivo, sheet = "personajes") %>%
  clean_names()

relaciones <- read_excel(archivo, sheet = "relaciones") %>%
  clean_names()


# ============================================================
# 2. Revisión mínima de estructura
# ============================================================

cat("\nDimensiones de la base:\n")
cat("Episodios:", nrow(episodios), "\n")
cat("Personajes:", nrow(personajes), "\n")
cat("Relaciones:", nrow(relaciones), "\n")

cat("\nColumnas episodios:\n")
print(names(episodios))

cat("\nColumnas personajes:\n")
print(names(personajes))

cat("\nColumnas relaciones:\n")
print(names(relaciones))


# ============================================================
# 3. Funciones auxiliares
# ============================================================

normalizar_texto <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_replace_all("á", "a") %>%
    str_replace_all("é", "e") %>%
    str_replace_all("í", "i") %>%
    str_replace_all("ó", "o") %>%
    str_replace_all("ú", "u") %>%
    str_replace_all("ñ", "n") %>%
    str_replace_all("\\s+", "_") %>%
    str_trim()
}

as_logical_robusto <- function(x) {
  x_chr <- str_to_lower(str_trim(as.character(x)))
  
  case_when(
    x_chr %in% c("true", "t", "1", "si", "sí", "yes") ~ TRUE,
    x_chr %in% c("false", "f", "0", "no") ~ FALSE,
    TRUE ~ NA
  )
}

first_non_missing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) {
    return(NA)
  } else {
    return(x[1])
  }
}


# ============================================================
# 4. Normalización básica
# ============================================================

personajes_limpios <- personajes %>%
  mutate(
    id_episodio = str_trim(as.character(id_episodio)),
    id_personaje = str_to_upper(str_trim(as.character(id_personaje))),
    nombre = str_trim(as.character(nombre)),
    sexo = str_trim(as.character(sexo)),
    grupo_etario = normalizar_texto(grupo_etario),
    rol_narrativo = normalizar_texto(rol_narrativo),
    es_victima = as_logical_robusto(es_victima),
    es_victimario = as_logical_robusto(es_victimario),
    es_complice = as_logical_robusto(es_complice),
    ses_nivel = suppressWarnings(as.numeric(ses_nivel)),
    ocupacion = as.character(ocupacion),
    descripcion = as.character(descripcion)
  )

relaciones_limpias <- relaciones %>%
  mutate(
    id_episodio = str_trim(as.character(id_episodio)),
    de = str_to_upper(str_trim(as.character(de))),
    a = str_to_upper(str_trim(as.character(a))),
    simetrica = as_logical_robusto(simetrica),
    vinculos_afectivos = normalizar_texto(vinculos_afectivos),
    vinculos_practicos = normalizar_texto(vinculos_practicos),
    descripcion = normalizar_texto(descripcion),
    asimetria_poder = normalizar_texto(asimetria_poder)
  )

episodios_limpios <- episodios %>%
  mutate(
    id_episodio = str_trim(as.character(id_episodio)),
    temporada = as.character(temporada),
    capitulo = suppressWarnings(as.integer(capitulo)),
    tipo_caso = normalizar_texto(tipo_caso),
    categoria_delito = normalizar_texto(categoria_delito),
    temas = normalizar_texto(temas),
    resumen = as.character(resumen)
  )


# ============================================================
# 5. Identificar vínculos económicos
# ============================================================

# Esta expresión regular captura referencias directas o indirectas
# a relaciones económicas: deuda, dinero, trabajo, estafa, robo,
# rescate, extorsión, herencia, negocio, salario, etc.

patron_economico <- paste(
  c(
    "relacion_economica",
    "patron_empleado",
    "trabajan_juntos",
    "deuda",
    "deudas",
    "endeudamiento",
    "dinero",
    "prestamo",
    "pago",
    "pagos",
    "cobro",
    "cobrar",
    "estafa",
    "fraude",
    "robo",
    "asalt",
    "rescate",
    "extorsion",
    "herencia",
    "negocio",
    "salario",
    "sueldo",
    "empleador",
    "empleado",
    "transport",
    "trafico",
    "droga",
    "drogas",
    "venta",
    "compra",
    "botin",
    "propiedad"
  ),
  collapse = "|"
)

relaciones_economicas <- relaciones_limpias %>%
  mutate(
    texto_busqueda = paste(
      vinculos_practicos,
      vinculos_afectivos,
      descripcion,
      sep = " "
    ),
    vinculo_economico = str_detect(
      texto_busqueda,
      regex(patron_economico, ignore_case = TRUE)
    )
  )


# ============================================================
# 6. Ranking de episodios según vínculos económicos
# ============================================================

ranking_episodios_economicos <- relaciones_economicas %>%
  group_by(id_episodio) %>%
  summarise(
    n_relaciones = n(),
    n_relaciones_economicas = sum(vinculo_economico, na.rm = TRUE),
    prop_relaciones_economicas = n_relaciones_economicas / n_relaciones,
    .groups = "drop"
  ) %>%
  left_join(
    episodios_limpios %>%
      select(
        id_episodio,
        temporada,
        capitulo,
        tipo_caso,
        categoria_delito,
        resumen,
        temas
      ),
    by = "id_episodio"
  ) %>%
  arrange(desc(n_relaciones_economicas), desc(prop_relaciones_economicas))

cat("\nTop 15 episodios con más vínculos económicos:\n")

ranking_episodios_economicos %>%
  select(
    id_episodio,
    temporada,
    capitulo,
    tipo_caso,
    categoria_delito,
    n_relaciones,
    n_relaciones_economicas,
    prop_relaciones_economicas,
    temas
  ) %>%
  print(n = 15, width = Inf)


# ============================================================
# 7. Elegir episodio para análisis
# ============================================================

# Selección automática: capítulo con mayor número de vínculos económicos.
episodio_elegido <- ranking_episodios_economicos %>%
  filter(n_relaciones_economicas > 0) %>%
  slice(1) %>%
  pull(id_episodio)

# Si quieres elegir manualmente un capítulo, comenta la línea anterior
# y descomenta una de estas:
# episodio_elegido <- "temporada_03_cap07"
# episodio_elegido <- "temporada_XX_capXX"

cat("\nEpisodio elegido:", episodio_elegido, "\n")


# ============================================================
# 8. Preparar nodos del capítulo
# ============================================================

nodos_capitulo_raw <- personajes_limpios %>%
  filter(id_episodio == episodio_elegido) %>%
  mutate(
    etiqueta = case_when(
      !is.na(nombre) & str_trim(nombre) != "" ~ str_trim(nombre),
      TRUE ~ id_personaje
    ),
    rol_red = case_when(
      es_victimario == TRUE ~ "victimario",
      es_victima == TRUE ~ "victima",
      es_complice == TRUE ~ "complice",
      rol_narrativo == "figura_autoridad" ~ "figura_autoridad",
      TRUE ~ "otro"
    ),
    rol_red = factor(
      rol_red,
      levels = c("victimario", "victima", "complice", "figura_autoridad", "otro")
    ),
    ses_grupo = case_when(
      is.na(ses_nivel) ~ "SES no codificado",
      ses_nivel <= 2 ~ "SES bajo",
      ses_nivel == 3 ~ "SES medio",
      ses_nivel >= 4 ~ "SES alto",
      TRUE ~ "SES no codificado"
    )
  ) %>%
  filter(!is.na(id_personaje), id_personaje != "")

duplicados_nodos <- nodos_capitulo_raw %>%
  count(id_personaje, sort = TRUE) %>%
  filter(n > 1)

if (nrow(duplicados_nodos) > 0) {
  cat("\nHay IDs de personaje duplicados en el episodio elegido:\n")
  print(duplicados_nodos)
  
  cat("\nDetalle de duplicados:\n")
  nodos_capitulo_raw %>%
    semi_join(duplicados_nodos, by = "id_personaje") %>%
    arrange(id_personaje) %>%
    select(
      id_episodio,
      id_personaje,
      nombre,
      etiqueta,
      sexo,
      grupo_etario,
      rol_narrativo,
      es_victima,
      es_victimario,
      es_complice,
      ses_nivel,
      ocupacion
    ) %>%
    print(n = Inf)
}

# Colapsar duplicados, conservando una fila por personaje dentro del episodio.
nodos_capitulo <- nodos_capitulo_raw %>%
  group_by(id_episodio, id_personaje) %>%
  summarise(
    etiqueta = first_non_missing(etiqueta),
    sexo = first_non_missing(sexo),
    grupo_etario = first_non_missing(grupo_etario),
    rol_narrativo = first_non_missing(rol_narrativo),
    rol_red = first_non_missing(as.character(rol_red)),
    ses_nivel = first_non_missing(ses_nivel),
    ses_grupo = first_non_missing(ses_grupo),
    es_victima = any(es_victima == TRUE, na.rm = TRUE),
    es_victimario = any(es_victimario == TRUE, na.rm = TRUE),
    es_complice = any(es_complice == TRUE, na.rm = TRUE),
    ocupacion = first_non_missing(ocupacion),
    descripcion = first_non_missing(descripcion),
    .groups = "drop"
  ) %>%
  mutate(
    rol_red = factor(
      rol_red,
      levels = c("victimario", "victima", "complice", "figura_autoridad", "otro")
    ),
    ses_grupo = case_when(
      is.na(ses_nivel) ~ "SES no codificado",
      ses_nivel <= 2 ~ "SES bajo",
      ses_nivel == 3 ~ "SES medio",
      ses_nivel >= 4 ~ "SES alto",
      TRUE ~ "SES no codificado"
    )
  )


# ============================================================
# 9. Preparar aristas del capítulo
# ============================================================

aristas_capitulo <- relaciones_economicas %>%
  filter(id_episodio == episodio_elegido) %>%
  filter(!is.na(de), !is.na(a), de != "", a != "") %>%
  transmute(
    from = de,
    to = a,
    simetrica = simetrica,
    vinculo_economico = as.logical(vinculo_economico),
    tipo_arista = if_else(vinculo_economico, "vinculo economico", "otro vinculo"),
    vinculos_afectivos = vinculos_afectivos,
    vinculos_practicos = vinculos_practicos,
    asimetria_poder = asimetria_poder,
    descripcion = descripcion
  )

ids_nodos <- nodos_capitulo$id_personaje

aristas_invalidas <- aristas_capitulo %>%
  filter(!from %in% ids_nodos | !to %in% ids_nodos)

if (nrow(aristas_invalidas) > 0) {
  cat("\nHay aristas con IDs de personaje no encontrados. Se excluirán del grafo:\n")
  print(aristas_invalidas)
}

aristas_capitulo <- aristas_capitulo %>%
  filter(from %in% ids_nodos, to %in% ids_nodos)

cat("\nDiagnóstico final del capítulo elegido:\n")
cat("Episodio:", episodio_elegido, "\n")
cat("Nodos únicos:", nrow(nodos_capitulo), "\n")
cat("Aristas válidas:", nrow(aristas_capitulo), "\n")
cat("Duplicados restantes en nodos:", anyDuplicated(nodos_capitulo$id_personaje), "\n")


# ============================================================
# 10. Crear objeto de red total
# ============================================================

# Importante:
# En igraph, la primera columna del data frame de vertices
# debe ser el nombre del nodo. Por eso usamos transmute(name = id_personaje).

vertices_capitulo <- nodos_capitulo %>%
  transmute(
    name = id_personaje,
    id_episodio = id_episodio,
    etiqueta = etiqueta,
    sexo = sexo,
    grupo_etario = grupo_etario,
    rol_narrativo = rol_narrativo,
    rol_red = rol_red,
    ses_nivel = ses_nivel,
    ses_grupo = ses_grupo,
    es_victima = es_victima,
    es_victimario = es_victimario,
    es_complice = es_complice,
    ocupacion = ocupacion,
    descripcion = descripcion
  )

if (anyDuplicated(vertices_capitulo$name) > 0) {
  stop("Todavía hay nombres de nodos duplicados en vertices_capitulo$name.")
}

red_capitulo <- graph_from_data_frame(
  d = aristas_capitulo,
  vertices = vertices_capitulo,
  directed = TRUE
)

# Incorporar métricas como atributos de nodo.
V(red_capitulo)$grado_total <- degree(red_capitulo, mode = "all")
V(red_capitulo)$grado_entrada <- degree(red_capitulo, mode = "in")
V(red_capitulo)$grado_salida <- degree(red_capitulo, mode = "out")
V(red_capitulo)$intermediacion <- betweenness(
  red_capitulo,
  directed = TRUE,
  normalized = TRUE
)

# Asegurar atributos de arista.
E(red_capitulo)$vinculo_economico <- as.logical(E(red_capitulo)$vinculo_economico)
E(red_capitulo)$tipo_arista <- as.character(E(red_capitulo)$tipo_arista)

cat("\nObjeto de red total:\n")
print(red_capitulo)

cat("\nAtributos de nodos:\n")
print(vertex_attr_names(red_capitulo))

cat("\nAtributos de aristas:\n")
print(edge_attr_names(red_capitulo))


# ============================================================
# 11. Métricas de nodos en red total
# ============================================================

metricas_nodos <- tibble(
  id_personaje = V(red_capitulo)$name,
  nombre = V(red_capitulo)$etiqueta,
  rol_red = as.character(V(red_capitulo)$rol_red),
  sexo = V(red_capitulo)$sexo,
  ses_nivel = V(red_capitulo)$ses_nivel,
  grado_total = V(red_capitulo)$grado_total,
  grado_entrada = V(red_capitulo)$grado_entrada,
  grado_salida = V(red_capitulo)$grado_salida,
  intermediacion = V(red_capitulo)$intermediacion
) %>%
  arrange(desc(grado_total), desc(intermediacion))

cat("\nPersonajes más centrales en la red total:\n")
metricas_nodos %>%
  select(
    nombre,
    rol_red,
    sexo,
    ses_nivel,
    grado_total,
    grado_entrada,
    grado_salida,
    intermediacion
  ) %>%
  print(n = Inf)


# ============================================================
# 12. Información descriptiva del episodio
# ============================================================

info_episodio <- ranking_episodios_economicos %>%
  filter(id_episodio == episodio_elegido)

cat("\nInformación del episodio elegido:\n")
info_episodio %>%
  select(
    id_episodio,
    temporada,
    capitulo,
    tipo_caso,
    categoria_delito,
    n_relaciones,
    n_relaciones_economicas,
    prop_relaciones_economicas,
    resumen,
    temas
  ) %>%
  print(width = Inf)


# ============================================================
# 13. Plot red total
# ============================================================

set.seed(123)

plot_red <- ggraph(red_capitulo, layout = "fr") +
  geom_edge_link(
    aes(
      edge_width = vinculo_economico,
      edge_alpha = vinculo_economico,
      edge_linetype = tipo_arista
    ),
    arrow = arrow(length = unit(3, "mm"), type = "closed"),
    end_cap = circle(4, "mm"),
    show.legend = TRUE
  ) +
  geom_node_point(
    aes(
      size = grado_total,
      color = rol_red,
      shape = ses_grupo
    )
  ) +
  geom_node_text(
    aes(label = etiqueta),
    repel = TRUE,
    size = 3.2,
    max.overlaps = 100
  ) +
  scale_edge_width_manual(
    values = c(`FALSE` = 0.4, `TRUE` = 1.3),
    labels = c(`FALSE` = "Otro vínculo", `TRUE` = "Vínculo económico"),
    name = "Tipo de vínculo"
  ) +
  scale_edge_alpha_manual(
    values = c(`FALSE` = 0.35, `TRUE` = 0.9),
    labels = c(`FALSE` = "Otro vínculo", `TRUE` = "Vínculo económico"),
    name = "Tipo de vínculo"
  ) +
  scale_size_continuous(range = c(3, 9)) +
  labs(
    title = "Red relacional de un capítulo de Mea Culpa",
    subtitle = paste0(
      "Episodio: ", episodio_elegido,
      " | vínculos económicos: ",
      info_episodio$n_relaciones_economicas,
      " de ",
      info_episodio$n_relaciones
    ),
    color = "Rol en la red",
    shape = "Posición SES",
    size = "Grado total",
    edge_linetype = "Tipo de vínculo",
    caption = "Fuente: base Mea Culpa. Red construida a partir de relaciones entre personajes codificadas por episodio."
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8)
  )

print(plot_red)


# ============================================================
# 14. Crear y plotear subred económica
# ============================================================

aristas_econ_capitulo <- aristas_capitulo %>%
  filter(vinculo_economico == TRUE)

if (nrow(aristas_econ_capitulo) > 0) {
  
  nodos_econ <- nodos_capitulo %>%
    filter(id_personaje %in% unique(c(aristas_econ_capitulo$from, aristas_econ_capitulo$to)))
  
  vertices_econ <- nodos_econ %>%
    transmute(
      name = id_personaje,
      id_episodio = id_episodio,
      etiqueta = etiqueta,
      sexo = sexo,
      grupo_etario = grupo_etario,
      rol_narrativo = rol_narrativo,
      rol_red = rol_red,
      ses_nivel = ses_nivel,
      ses_grupo = ses_grupo,
      es_victima = es_victima,
      es_victimario = es_victimario,
      es_complice = es_complice,
      ocupacion = ocupacion,
      descripcion = descripcion
    )
  
  if (anyDuplicated(vertices_econ$name) > 0) {
    stop("Hay nombres de nodos duplicados en vertices_econ$name.")
  }
  
  red_econ <- graph_from_data_frame(
    d = aristas_econ_capitulo,
    vertices = vertices_econ,
    directed = TRUE
  )
  
  # Incorporar métricas como atributos de nodo en subred económica.
  V(red_econ)$grado_total_economico <- degree(red_econ, mode = "all")
  V(red_econ)$grado_entrada_economico <- degree(red_econ, mode = "in")
  V(red_econ)$grado_salida_economico <- degree(red_econ, mode = "out")
  V(red_econ)$intermediacion_economica <- betweenness(
    red_econ,
    directed = TRUE,
    normalized = TRUE
  )
  
  metricas_econ <- tibble(
    id_personaje = V(red_econ)$name,
    nombre = V(red_econ)$etiqueta,
    rol_red = as.character(V(red_econ)$rol_red),
    sexo = V(red_econ)$sexo,
    ses_nivel = V(red_econ)$ses_nivel,
    grado_total_economico = V(red_econ)$grado_total_economico,
    grado_entrada_economico = V(red_econ)$grado_entrada_economico,
    grado_salida_economico = V(red_econ)$grado_salida_economico,
    intermediacion_economica = V(red_econ)$intermediacion_economica
  ) %>%
    arrange(desc(grado_total_economico), desc(intermediacion_economica))
  
  cat("\nPersonajes más centrales en la subred económica:\n")
  metricas_econ %>%
    select(
      nombre,
      rol_red,
      sexo,
      ses_nivel,
      grado_total_economico,
      grado_entrada_economico,
      grado_salida_economico,
      intermediacion_economica
    ) %>%
    print(n = Inf)
  
  set.seed(123)
  
  plot_red_economica <- ggraph(red_econ, layout = "fr") +
    geom_edge_link(
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      end_cap = circle(4, "mm"),
      edge_width = 1,
      edge_alpha = 0.8,
      show.legend = FALSE
    ) +
    geom_node_point(
      aes(
        size = grado_total_economico,
        color = rol_red,
        shape = ses_grupo
      )
    ) +
    geom_node_text(
      aes(label = etiqueta),
      repel = TRUE,
      size = 3.4,
      max.overlaps = 100
    ) +
    scale_size_continuous(range = c(4, 10)) +
    labs(
      title = "Subred económica del capítulo",
      subtitle = paste0("Episodio: ", episodio_elegido),
      color = "Rol en la red",
      shape = "Posición SES",
      size = "Grado económico",
      caption = "Se muestran solo vínculos donde aparece relación económica, deuda, dinero, trabajo, estafa, robo, extorsión, herencia u otra referencia económica."
    ) +
    theme_void(base_size = 12) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 10),
      plot.caption = element_text(size = 8)
    )
  
  print(plot_red_economica)
  
} else {
  message("El capítulo elegido no tiene vínculos económicos según el criterio usado.")
  red_econ <- NULL
  metricas_econ <- NULL
  plot_red_economica <- NULL
}


# ============================================================
# 15. Guardar resultados
# ============================================================

dir.create("outputs_redes", showWarnings = FALSE)

ggsave(
  filename = file.path("outputs_redes", paste0("red_capitulo_", episodio_elegido, ".png")),
  plot = plot_red,
  width = 11,
  height = 8,
  dpi = 300
)

if (!is.null(plot_red_economica)) {
  ggsave(
    filename = file.path("outputs_redes", paste0("subred_economica_", episodio_elegido, ".png")),
    plot = plot_red_economica,
    width = 11,
    height = 8,
    dpi = 300
  )
}

salidas_excel <- list(
  ranking_episodios_economicos = ranking_episodios_economicos,
  info_episodio = info_episodio,
  nodos_capitulo = nodos_capitulo,
  aristas_capitulo = aristas_capitulo,
  metricas_nodos_red_total = metricas_nodos,
  aristas_economicas_capitulo = aristas_econ_capitulo
)

if (!is.null(metricas_econ)) {
  salidas_excel$metricas_nodos_subred_economica <- metricas_econ
}

write_xlsx(
  salidas_excel,
  path = file.path("outputs_redes", paste0("analisis_red_", episodio_elegido, ".xlsx"))
)


# ============================================================
# 16. Cierre analítico en consola
# ============================================================

cat("\n============================================================\n")
cat("Resumen del análisis\n")
cat("============================================================\n")

cat("Episodio analizado:", episodio_elegido, "\n")
cat("Relaciones totales:", info_episodio$n_relaciones, "\n")
cat("Relaciones económicas:", info_episodio$n_relaciones_economicas, "\n")
cat(
  "Proporción de relaciones económicas:",
  round(info_episodio$prop_relaciones_economicas, 3),
  "\n"
)

cat("\nPersonaje más central en la red total:\n")

metricas_nodos %>%
  slice(1) %>%
  select(
    nombre,
    rol_red,
    sexo,
    ses_nivel,
    grado_total,
    grado_entrada,
    grado_salida,
    intermediacion
  ) %>%
  print(width = Inf)

if (!is.null(metricas_econ)) {
  cat("\nPersonaje más central en la subred económica:\n")
  
  metricas_econ %>%
    slice(1) %>%
    select(
      nombre,
      rol_red,
      sexo,
      ses_nivel,
      grado_total_economico,
      grado_entrada_economico,
      grado_salida_economico,
      intermediacion_economica
    ) %>%
    print(width = Inf)
}

cat("\nArchivos guardados en la carpeta: outputs_redes\n")
cat("============================================================\n")









# ============================================================
# Plot red total sin nodos aislados
# ============================================================

red_capitulo_sin_aislados <- delete_vertices(
  red_capitulo,
  V(red_capitulo)[degree(red_capitulo, mode = "all") == 0]
)

set.seed(123)

plot_red_sin_aislados <- ggraph(red_capitulo_sin_aislados, layout = "fr") +
  geom_edge_link(
    aes(
      edge_width = vinculo_economico,
      edge_linetype = tipo_arista
    ),
    arrow = arrow(length = unit(3, "mm"), type = "closed"),
    end_cap = circle(4, "mm"),
    alpha = 0.75,
    show.legend = TRUE
  ) +
  geom_node_point(
    aes(
      size = grado_total,
      color = rol_red,
      shape = ses_grupo
    )
  ) +
  geom_node_text(
    aes(label = etiqueta),
    repel = TRUE,
    size = 3.2,
    max.overlaps = 100
  ) +
  scale_edge_width_manual(
    values = c(`FALSE` = 0.4, `TRUE` = 1.3),
    labels = c(`FALSE` = "Otro vínculo", `TRUE` = "Vínculo económico"),
    name = "Tipo de vínculo"
  ) +
  scale_size_continuous(range = c(3, 9)) +
  labs(
    title = "Red relacional del capítulo, sin nodos aislados",
    subtitle = paste0(
      "Episodio: ", episodio_elegido,
      " | vínculos económicos: ",
      info_episodio$n_relaciones_economicas,
      " de ",
      info_episodio$n_relaciones
    ),
    color = "Rol en la red",
    shape = "Posición SES",
    size = "Grado total",
    edge_linetype = "Tipo de vínculo",
    caption = "Se excluyen personajes sin vínculos codificados en la red del episodio."
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8)
  )

plot_red_sin_aislados