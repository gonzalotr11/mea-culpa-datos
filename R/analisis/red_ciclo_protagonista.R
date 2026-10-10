# =============================================================================
# RED + CICLO DE VIDA — MEA CULPA
# Panel izquierdo: timeline biográfico del protagonista (eventos por fase)
# Panel derecho:   red socio-material interactiva que se actualiza al
#                  seleccionar una fase o un evento específico
#
# Paquetes: dplyr, readr, stringr, purrr, jsonlite
# Uso: ajustar EPI_ID y correr desde el directorio con los CSV
# =============================================================================

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(jsonlite)

# ─── CONFIGURACIÓN ────────────────────────────────────────────────────────────

EPI_ID <- "temporada_14_cap04"   # ← cambiar por episodio deseado

MOMENTOS_ORDEN  <- c("antecedentes","escalada","detonante","crimen","consecuencias","proceso_legal")
MOMENTO_LABEL   <- c(antecedentes="Antecedentes", escalada="Escalada",
                     detonante="Detonante",        crimen="Crimen",
                     consecuencias="Consecuencias",proceso_legal="Proceso legal")
MOMENTO_COLOR   <- c(antecedentes="#2980b9", escalada="#e67e22",
                     detonante="#e74c3c",    crimen="#7b241c",
                     consecuencias="#7d3c98",proceso_legal="#1a5276")
MOMENTO_PERIODO <- c(antecedentes="Infancia · Adultez temprana",
                     escalada    ="Pre-crimen",
                     detonante   ="Punto de quiebre",
                     crimen      ="El crimen",
                     consecuencias="Descubrimiento",
                     proceso_legal="Proceso judicial")

ROL_COLOR  <- c(victimario="#e8c547",  victima="#e74c3c",  complice="#e67e22",
                autoridad="#2980b9",   tercero_afectado="#7f8c8d", sin_rol_criminal="#bdc3c7")
ENT_COLOR  <- c(arma="#c0392b",        lugar="#1e8449",    organizacion="#7d3c98",
                objeto_material="#b7950b", objeto_economico="#117a65",
                documento="#5d6d7e",   vehiculo="#1a5276", sustancia="#922b21")
ASIM_COLOR <- c(from_domina="#e74c3c", to_domina="#2980b9",
                par="#aab7b8",         variable="#e67e22", incierta="#566573")

lookup <- function(x, tabla, def = "#aab7b8") {
  r <- tabla[as.character(x)]; r[is.na(r)] <- def; unname(r)
}

# ─── CARGAR DATOS ─────────────────────────────────────────────────────────────

leer_f <- function(f, epi) {
  if (!file.exists(f)) { message("⚠ no encontrado: ", f); return(tibble()) }
  df <- read_csv(f, show_col_types = FALSE)
  if ("id_episodio" %in% names(df)) filter(df, id_episodio == epi) else df
}

epi_e   <- leer_f("01_episodios.csv",           EPI_ID)
pers_e  <- leer_f("04_personajes.csv",           EPI_ID)
rel_e   <- leer_f("05_relaciones.csv",           EPI_ID)
vp_e    <- leer_f("06_vinculos_practicos.csv",   EPI_ID)
va_e    <- leer_f("07_vinculos_afectivos.csv",   EPI_ID)
ent_e   <- leer_f("08_entidades.csv",            EPI_ID)
rpe_e   <- leer_f("09_relaciones_pers_ent.csv",  EPI_ID)
fases_e <- leer_f("10_fases_narrativas.csv",     EPI_ID)
meca_e  <- leer_f("11_mecanismos_causales.csv",  EPI_ID)

USAR_NARRATIVA <- nrow(fases_e) > 0
message(if (USAR_NARRATIVA) "✓ Fases narrativas disponibles"
        else "ℹ Sin fases — construyendo desde relaciones")

vp_res <- vp_e |> group_by(id_relacion) |>
  summarise(vp = paste(vinculo_practico, collapse = " · "), .groups = "drop")
va_res <- va_e |> group_by(id_relacion) |>
  summarise(va = paste(vinculo_afectivo,  collapse = " · "), .groups = "drop")

# ─── PROTAGONISTA ─────────────────────────────────────────────────────────────

protagonista <- pers_e |>
  mutate(pts = (rol_narrativo == "protagonista") * 2 + (rol_criminologico == "victimario")) |>
  slice_max(pts, n = 1, with_ties = FALSE)

prot_id     <- protagonista$id_personaje_global
prot_nombre <- protagonista$nombre
prot_primer <- str_split(prot_nombre, " ")[[1]][1]
message("Protagonista: ", prot_nombre)

# ─── ASIGNAR MOMENTOS A NODOS ─────────────────────────────────────────────────

m_pers <- function(rol, etario) {
  case_when(
    rol    == "victimario"                          ~ "antecedentes",
    rol    == "autoridad"                           ~ "consecuencias",
    rol    == "victima"                             ~ "escalada",
    rol    == "complice"                            ~ "escalada",
    etario %in% c("mayor","adulto_mayor")           ~ "antecedentes",
    TRUE                                            ~ "antecedentes"
  )
}
m_ent <- function(fun, tipo) {
  case_when(
    fun  == "instrumento_del_delito"  ~ "crimen",
    fun  == "lugar_del_delito"        ~ "crimen",
    fun  == "fuente_de_conflicto"     ~ "escalada",
    fun  == "objeto_disputado"        ~ "escalada",
    fun  == "contexto_victimizacion"  ~ "escalada",
    fun  == "institucion_fallida"     ~ "detonante",
    fun  == "recurso_estatus"         ~ "antecedentes",
    tipo == "arma"                    ~ "crimen",
    tipo == "documento"               ~ "escalada",
    TRUE                              ~ "antecedentes"
  )
}
idx_m  <- function(m) match(m, MOMENTOS_ORDEN)
max_m  <- function(a, b) MOMENTOS_ORDEN[pmax(coalesce(idx_m(a), 1L), coalesce(idx_m(b), 1L))]

if (USAR_NARRATIVA) {
  nombre_a_id <- function(n) {
    p <- str_split(n, " ")[[1]][1]
    m <- pers_e$id_personaje_global[str_detect(pers_e$nombre, regex(p, ignore_case = TRUE))]
    if (length(m) > 0) m[1] else NA_character_
  }
  actor_m <- fases_e |>
    mutate(act = map(str_split(coalesce(actores,""), " ; "),
                     ~ str_trim(.x[.x != "" & .x != "NA"]))) |>
    unnest(act) |>
    left_join(pers_e |> select(nombre, id_personaje_global), by = c("act" = "nombre")) |>
    filter(!is.na(id_personaje_global)) |>
    group_by(id_personaje_global) |>
    slice_min(orden, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(id_personaje_global, momento_inicio = fase)

  pers_e <- pers_e |>
    left_join(actor_m, by = "id_personaje_global") |>
    mutate(momento_inicio = coalesce(momento_inicio, "antecedentes"))

  ent_m <- rpe_e |>
    left_join(pers_e |> select(id_personaje_global, mp = momento_inicio), by = "id_personaje_global") |>
    group_by(id_entidad_global) |>
    summarise(momento_inicio = MOMENTOS_ORDEN[min(idx_m(mp), na.rm = TRUE)], .groups = "drop")

  ent_e <- ent_e |>
    left_join(ent_m, by = "id_entidad_global") |>
    mutate(momento_inicio = coalesce(momento_inicio,
           m_ent(coalesce(funcion_narrativa,"recurso_estatus"), coalesce(tipo,"objeto_material"))))
} else {
  pers_e <- pers_e |> mutate(
    momento_inicio = m_pers(coalesce(rol_criminologico,"sin_rol_criminal"),
                            coalesce(grupo_etario,"adulto"))
  )
  ent_e <- ent_e |> mutate(
    momento_inicio = m_ent(coalesce(funcion_narrativa,"recurso_estatus"), coalesce(tipo,"objeto_material"))
  )
}

# ─── NODOS ────────────────────────────────────────────────────────────────────

nodos_pers <- pers_e |> transmute(
  id             = id_personaje_global,
  label          = str_wrap(coalesce(nombre, id_personaje), 13),
  shape          = "dot",
  colorOriginal  = lookup(coalesce(rol_criminologico,"sin_rol_criminal"), ROL_COLOR),
  sizeOriginal   = 22 + pmax(0, pmin(4, coalesce(ses_nivel,3L)-1L)) * 7,
  group          = coalesce(rol_criminologico,"sin_rol_criminal"),
  momento_inicio = momento_inicio,
  es_prot        = id_personaje_global == prot_id,
  title = paste0(
    "<div style='font:12px/1.5 sans-serif;max-width:210px;padding:2px'>",
    "<b>", coalesce(nombre,id_personaje), "</b><br>",
    "<i style='color:#aaa'>", coalesce(rol_narrativo,""), " / ", coalesce(rol_criminologico,""), "</i><br>",
    "SES ", coalesce(as.character(ses_nivel),"?"),"/5 · ", coalesce(ocupacion,"—"), "<br>",
    "<span style='color:#aaa;font-size:11px'>", coalesce(descripcion,""), "</span>",
    "<br><span style='color:", MOMENTO_COLOR[momento_inicio], ";font-size:10px'>▸ ",
    MOMENTO_LABEL[momento_inicio], "</span></div>"
  )
) |> mutate(color = colorOriginal, size = sizeOriginal,
            borderWidth = if_else(es_prot, 4L, 2L))

nodos_ent <- ent_e |> transmute(
  id             = id_entidad_global,
  label          = str_wrap(coalesce(nombre, id_entidad), 13),
  shape          = "square",
  colorOriginal  = lookup(coalesce(tipo,"objeto_material"), ENT_COLOR),
  sizeOriginal   = case_when(centralidad=="alta"~40, centralidad=="media"~28, TRUE~20),
  group          = paste0("[", coalesce(tipo,"objeto"), "]"),
  momento_inicio = momento_inicio,
  es_prot        = FALSE,
  title = paste0(
    "<div style='font:12px/1.5 sans-serif;max-width:210px;padding:2px'>",
    "<b>", coalesce(nombre,id_entidad), "</b><br>",
    "<i style='color:#aaa'>", coalesce(tipo,""), " · ", coalesce(subtipo,""), "</i><br>",
    "Función: <b>", coalesce(funcion_narrativa,"—"), "</b><br>",
    "<span style='color:#aaa;font-size:11px'>", coalesce(descripcion,""), "</span>",
    "<br><span style='color:", MOMENTO_COLOR[momento_inicio], ";font-size:10px'>▸ ",
    MOMENTO_LABEL[momento_inicio], "</span></div>"
  )
) |> mutate(color = colorOriginal, size = sizeOriginal, borderWidth = 2L)

nodos <- bind_rows(nodos_pers, nodos_ent)

# ─── ARISTAS ──────────────────────────────────────────────────────────────────

m_map <- setNames(c(nodos_pers$momento_inicio, nodos_ent$momento_inicio),
                  c(nodos_pers$id,             nodos_ent$id))

aristas_pp <- rel_e |>
  left_join(vp_res, by = "id_relacion") |>
  left_join(va_res, by = "id_relacion") |>
  filter(from_personaje_global %in% nodos$id, to_personaje_global %in% nodos$id) |>
  transmute(
    id             = row_number(),
    from           = from_personaje_global,
    to             = to_personaje_global,
    colorOriginal  = lookup(coalesce(asimetria_poder,"par"), ASIM_COLOR),
    arrows         = case_when(asimetria_poder=="from_domina"~"to",
                               asimetria_poder=="to_domina"~"from", TRUE~""),
    dashes         = FALSE,
    widthOriginal  = 2.5,
    momento_inicio = max_m(m_map[from_personaje_global], m_map[to_personaje_global]),
    title = paste0(
      "<div style='font:11px/1.5 sans-serif;max-width:200px'>",
      "<b>", coalesce(asimetria_poder,"?"), "</b><br>",
      "Práctico: ", coalesce(vp,"—"), "<br>Afectivo: ", coalesce(va,"—"), "<br>",
      "<i style='color:#aaa'>", coalesce(descripcion,""), "</i></div>"
    )
  ) |> mutate(color = colorOriginal, width = widthOriginal)

aristas_pe <- rpe_e |>
  filter(id_personaje_global %in% nodos$id, id_entidad_global %in% nodos$id) |>
  transmute(
    id             = row_number() + 500L,
    from           = id_personaje_global,
    to             = id_entidad_global,
    colorOriginal  = "#6c757d",
    arrows         = "to",
    dashes         = TRUE,
    widthOriginal  = 1.5,
    momento_inicio = max_m(m_map[id_personaje_global], m_map[id_entidad_global]),
    title = paste0(
      "<div style='font:11px/1.5 sans-serif;max-width:200px'>",
      "<b>", coalesce(tipo_vinculo,"usa"), "</b><br>",
      "<i style='color:#aaa'>", coalesce(descripcion,""), "</i></div>"
    )
  ) |> mutate(color = colorOriginal, width = widthOriginal)

aristas <- bind_rows(aristas_pp, aristas_pe)

# ─── EVENTOS BIOGRÁFICOS ──────────────────────────────────────────────────────

nombre_a_id_fn <- function(n) {
  p <- str_split(n," ")[[1]][1]
  m <- pers_e$id_personaje_global[str_detect(pers_e$nombre, regex(p, ignore_case=TRUE))]
  if (length(m) > 0) m[1] else NA_character_
}

if (USAR_NARRATIVA) {
  # Entidades por fase del protagonista
  ent_por_fase <- rpe_e |>
    filter(id_personaje_global == prot_id) |>
    left_join(ent_e |> select(id_entidad_global, funcion_narrativa), by = "id_entidad_global") |>
    mutate(fase_asig = case_when(
      funcion_narrativa == "instrumento_del_delito" ~ "crimen",
      funcion_narrativa == "lugar_del_delito"       ~ "crimen",
      funcion_narrativa == "fuente_de_conflicto"    ~ "escalada",
      funcion_narrativa == "objeto_disputado"       ~ "escalada",
      funcion_narrativa == "institucion_fallida"    ~ "detonante",
      TRUE ~ NA_character_)) |> filter(!is.na(fase_asig))

  fases_usadas <- character(0)
  eventos_raw  <- fases_e |> arrange(orden) |>
    mutate(
      titulo    = str_to_sentence(str_replace_all(coalesce(tipo_evento,"evento"),"_"," ")),
      desc_txt  = coalesce(descripcion, tipo_evento, ""),
      act_nombres = map(str_split(coalesce(actores,""), " ; "),
                        ~ str_trim(.x[.x != "" & .x != "NA"])),
      act_ids   = map(act_nombres, ~ na.omit(map_chr(.x, nombre_a_id_fn))),
      ent_ids   = map(fase, function(f) {
        if (f %in% fases_usadas) return(character(0))
        fases_usadas <<- c(fases_usadas, f)
        ent_por_fase |> filter(fase_asig == f) |> pull(id_entidad_global)
      }),
      prot_ok   = map_lgl(act_nombres,
                          ~ any(str_detect(.x, regex(prot_primer, ignore_case=TRUE))))
    ) |>
    transmute(orden, fase, titulo, descripcion = desc_txt,
              act_ids, ent_ids, prot_ok)
} else {
  rel_prot <- rel_e |>
    filter(from_personaje_global == prot_id | to_personaje_global == prot_id) |>
    left_join(vp_res, by="id_relacion") |> left_join(va_res, by="id_relacion") |>
    mutate(alter = if_else(from_personaje_global==prot_id, to_personaje_global, from_personaje_global)) |>
    left_join(pers_e |> select(id_personaje_global, anombre=nombre, arul=rol_criminologico, adesc=descripcion),
              by=c("alter"="id_personaje_global")) |>
    mutate(fase=case_when(arul=="victima"~"escalada", arul=="autoridad"~"proceso_legal", TRUE~"antecedentes"),
           titulo=paste0("Vínculo con ",anombre), descripcion=coalesce(descripcion,adesc,""),
           act_ids=map2(from_personaje_global,to_personaje_global,~c(.x,.y)),
           ent_ids=list(character(0)), prot_ok=TRUE) |>
    select(fase,titulo,descripcion,act_ids,ent_ids,prot_ok)

  rpe_prot <- rpe_e |> filter(id_personaje_global==prot_id) |>
    left_join(ent_e |> select(id_entidad_global, enombre=nombre, etipo=tipo, efun=funcion_narrativa, edesc=descripcion),
              by="id_entidad_global") |>
    mutate(fase=case_when(efun=="instrumento_del_delito"~"crimen", efun=="lugar_del_delito"~"crimen",
                          efun=="fuente_de_conflicto"~"escalada", efun=="objeto_disputado"~"escalada",
                          efun=="institucion_fallida"~"detonante", efun=="recurso_estatus"~"antecedentes",
                          etipo=="arma"~"crimen", etipo=="documento"~"escalada", TRUE~"antecedentes"),
           titulo=paste0(str_to_sentence(str_replace_all(coalesce(tipo_vinculo,"uso"),"_"," "))," — ",enombre),
           descripcion=coalesce(descripcion,edesc,""),
           act_ids=list(c(prot_id)), ent_ids=map(id_entidad_global,~c(.x)), prot_ok=TRUE) |>
    select(fase,titulo,descripcion,act_ids,ent_ids,prot_ok)

  eventos_raw <- bind_rows(rel_prot, rpe_prot) |>
    arrange(match(fase, MOMENTOS_ORDEN)) |>
    mutate(orden = row_number())
}

# Enriquecer eventos con objetos de actores/entidades para el HTML
badge_actor <- function(ids) {
  map(ids[!is.na(ids)], function(i) {
    p <- pers_e[pers_e$id_personaje_global == i, ]
    if (nrow(p) == 0) return(NULL)
    list(id    = i, label = str_split(p$nombre[1]," ")[[1]][1],
         color = lookup(coalesce(p$rol_criminologico[1],"sin_rol_criminal"), ROL_COLOR),
         prot  = i == prot_id)
  }) |> compact()
}
badge_ent <- function(ids) {
  map(ids[!is.na(ids)], function(i) {
    e <- ent_e[ent_e$id_entidad_global == i, ]
    if (nrow(e) == 0) return(NULL)
    list(id    = i, label = str_trunc(e$nombre[1], 16),
         color = lookup(coalesce(e$tipo[1],"objeto_material"), ENT_COLOR))
  }) |> compact()
}

eventos_lista <- eventos_raw |>
  rowwise() |>
  mutate(
    actores_obj   = list(badge_actor(act_ids)),
    entidades_obj = list(badge_ent(ent_ids)),
    all_ids       = list(unique(c(act_ids[!is.na(act_ids)], ent_ids[!is.na(ent_ids)])))
  ) |>
  ungroup() |>
  transmute(
    orden, fase, titulo, descripcion,
    actores   = actores_obj,
    entidades = entidades_obj,
    node_ids  = all_ids,
    prot_ok,
    fase_color = unname(MOMENTO_COLOR[fase]),
    fase_label = unname(MOMENTO_LABEL[fase])
  )

# ─── SERIALIZACIÓN ────────────────────────────────────────────────────────────

nodos_json   <- toJSON(nodos,          auto_unbox = TRUE)
aristas_json <- toJSON(aristas,        auto_unbox = TRUE)
eventos_json <- toJSON(eventos_lista,  auto_unbox = TRUE)

# nodeLookup: evento_orden → [node_ids] para enlazar click de evento con la red
node_lookup  <- setNames(eventos_lista$node_ids, as.character(eventos_lista$orden))
lookup_json  <- toJSON(node_lookup, auto_unbox = FALSE)

fases_presentes <- MOMENTOS_ORDEN[MOMENTOS_ORDEN %in% eventos_lista$fase]
colores_js <- paste0("{", paste(map_chr(MOMENTOS_ORDEN, ~sprintf('"%s":"%s"',.x,MOMENTO_COLOR[.x])), collapse=","), "}")
periodos_js <- paste0("{", paste(map_chr(MOMENTOS_ORDEN, ~sprintf('"%s":"%s"',.x,MOMENTO_PERIODO[.x])), collapse=","), "}")

# Metadatos
delito_txt  <- str_replace_all(coalesce(epi_e$categoria_delito_principal[1],""),"_"," ")
periodo_txt <- coalesce(epi_e$periodo_temporal[1],"")
locac_txt   <- coalesce(epi_e$locacion_principal[1],"")
meca_txt <- if (USAR_NARRATIVA && nrow(meca_e) > 0) paste0(
  str_replace_all(coalesce(meca_e$tipo_mecanismo[1],""),"_"," "), " · ",
  str_replace_all(coalesce(meca_e$patron_relacional[1],""),"_"," "), " · ",
  coalesce(meca_e$factor_detonante[1],"")) else ""

nav_btns_html <- paste(
  map_chr(fases_presentes, ~sprintf(
    '<button class="nav-btn" data-fase="%s" onclick="selectFase(\'%s\')" style="--fc:%s">%s</button>',
    .x,.x,MOMENTO_COLOR[.x],MOMENTO_LABEL[.x])),
  collapse="\n  ")

# ─── HTML ─────────────────────────────────────────────────────────────────────

html <- paste0('<!DOCTYPE html>
<html lang="es">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Red · Ciclo de vida · ', prot_nombre, '</title>
<script src="https://cdnjs.cloudflare.com/ajax/libs/vis/4.21.0/vis.min.js"></script>
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/vis/4.21.0/vis.min.css">
<style>
*{box-sizing:border-box;margin:0;padding:0;}
:root{--bg:#111418;--bg2:#0d1117;--bg3:#141820;--bd:#1e2633;
      --text:#ecf0f1;--muted:#7f8c8d;--dim:#566573;}
body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif;
     background:var(--bg);color:var(--text);height:100vh;
     display:flex;flex-direction:column;overflow:hidden;}

/* HEADER */
#hdr{background:var(--bg2);padding:8px 16px;border-bottom:2px solid #e74c3c;
     flex-shrink:0;display:flex;align-items:center;gap:12px;}
.badge{background:#e74c3c;color:#fff;font-size:9px;font-weight:700;
       padding:2px 7px;border-radius:3px;letter-spacing:.6px;text-transform:uppercase;}
#hdr h1{font-size:14px;font-weight:600;}
#hdr h1 strong{color:#e8c547;}
#hdr .meta{margin-left:auto;font-size:10px;color:var(--muted);text-align:right;line-height:1.7;}

/* NAV DE FASES */
#fase-nav{background:var(--bg2);padding:6px 14px;border-bottom:1px solid var(--bd);
          flex-shrink:0;display:flex;gap:6px;flex-wrap:wrap;align-items:center;}
.nav-btn{background:transparent;color:var(--muted);border:1px solid #2c3e50;
         border-radius:4px;padding:4px 11px;font-size:10px;font-weight:600;
         cursor:pointer;transition:all .15s;letter-spacing:.3px;}
.nav-btn:hover{color:var(--text);border-color:var(--dim);}
.nav-btn.activo{background:var(--fc,#2c3e50);border-color:var(--fc,#566573);
                color:#fff;font-weight:700;}
#btn-todos{border-color:#2c3e50;}
#btn-todos.activo{background:#1e2633;color:#fff;}
.fase-info{margin-left:auto;font-size:10px;color:var(--dim);transition:all .2s;}

/* MAIN SPLIT */
#main{display:flex;flex:1;min-height:0;}

/* PANEL IZQUIERDO — timeline */
#left{width:36%;min-width:260px;max-width:380px;border-right:1px solid var(--bd);
      display:flex;flex-direction:column;overflow:hidden;}
#tl-hdr{padding:7px 12px;background:var(--bg2);border-bottom:1px solid var(--bd);
        font-size:10px;font-weight:700;color:var(--dim);letter-spacing:.4px;
        text-transform:uppercase;flex-shrink:0;}
#tl-scroll{flex:1;overflow-y:auto;padding-bottom:20px;}
#tl-scroll::-webkit-scrollbar{width:4px;}
#tl-scroll::-webkit-scrollbar-track{background:var(--bg);}
#tl-scroll::-webkit-scrollbar-thumb{background:#2c3e50;border-radius:2px;}

/* SPINE (línea vertical) */
.tl-fase{position:relative;padding-left:28px;}
.tl-fase::before{content:"";position:absolute;left:12px;top:0;bottom:0;
                 width:2px;background:var(--fc,#2c3e50);}

/* Banner de fase */
.fase-banner{padding:10px 0 6px 10px;display:flex;align-items:center;gap:8px;
             position:sticky;top:0;background:var(--bg);z-index:2;}
.fase-dot{width:12px;height:12px;border-radius:50%;background:var(--fc);
          border:2px solid var(--bg);flex-shrink:0;margin-left:-6px;margin-right:6px;
          box-shadow:0 0 0 2px var(--fc);}
.fase-tag{background:var(--fc);color:#fff;font-size:9px;font-weight:700;
          padding:2px 7px;border-radius:3px;letter-spacing:.5px;text-transform:uppercase;}
.fase-periodo{font-size:10px;color:var(--muted);font-style:italic;}

/* Evento */
.ev{margin:0 0 4px 0;padding:8px 10px 8px 12px;
    border-left:3px solid transparent;cursor:pointer;
    transition:background .12s,border-color .12s;}
.ev:hover{background:rgba(255,255,255,.04);}
.ev.activo{background:var(--fc-bg,rgba(41,128,185,.12));border-left-color:var(--fc,#2980b9);}
.ev-header{display:flex;align-items:flex-start;gap:6px;margin-bottom:4px;}
.ev-num{font-size:9px;color:var(--dim);min-width:18px;padding-top:2px;}
.ev-titulo{font-size:11px;font-weight:600;line-height:1.35;flex:1;}
.prot-pip{width:6px;height:6px;border-radius:50%;background:#e8c547;
           flex-shrink:0;margin-top:4px;title:"Protagonista";}
.ev-desc{font-size:10px;color:var(--muted);line-height:1.45;
          margin:3px 0 5px 24px;display:-webkit-box;-webkit-line-clamp:2;
          -webkit-box-orient:vertical;overflow:hidden;}
.ev-badges{display:flex;flex-wrap:wrap;gap:3px;margin-left:24px;}
.ba{display:inline-flex;align-items:center;gap:3px;padding:1px 6px;
    border-radius:8px;font-size:9px;font-weight:600;
    background:color-mix(in srgb,var(--bc) 15%,transparent);
    border:1px solid color-mix(in srgb,var(--bc) 30%,transparent);
    color:var(--bc);}
.ba.prot{border-width:2px;font-weight:800;}
.ba .dot{width:6px;height:6px;border-radius:50%;background:var(--bc);flex-shrink:0;}
.be{display:inline-flex;align-items:center;gap:3px;padding:1px 6px;
    border-radius:2px;font-size:9px;font-weight:600;
    background:color-mix(in srgb,var(--bc) 12%,transparent);
    border:1px solid color-mix(in srgb,var(--bc) 28%,transparent);color:var(--bc);}
.be .sq{width:6px;height:6px;border-radius:1px;background:var(--bc);flex-shrink:0;}

/* PANEL DERECHO — red */
#right{flex:1;display:flex;flex-direction:column;position:relative;}
#net-info{padding:5px 14px;background:var(--bg3);border-bottom:1px solid var(--bd);
          flex-shrink:0;display:flex;align-items:center;gap:10px;font-size:11px;}
#net-info .fase-pill{padding:2px 9px;border-radius:3px;font-size:10px;font-weight:700;
                      color:#fff;letter-spacing:.4px;background:#2c3e50;transition:background .2s;}
#net-info .stats{color:var(--muted);font-size:10px;}
#net-info .ev-desc-net{color:var(--dim);font-size:10px;font-style:italic;
                        max-width:400px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;}
#reset-btn{margin-left:auto;background:transparent;border:1px solid #2c3e50;
           color:var(--muted);font-size:10px;padding:3px 9px;border-radius:3px;
           cursor:pointer;transition:all .15s;}
#reset-btn:hover{color:var(--text);border-color:var(--dim);}
#mynetwork{flex:1;}

/* Leyenda */
#legend{position:absolute;bottom:12px;right:12px;background:rgba(13,17,23,.92);
        border:1px solid var(--bd);border-radius:6px;padding:9px 13px;font-size:10px;
        color:#bdc3c7;}
#legend h4{font-size:9px;color:var(--dim);text-transform:uppercase;letter-spacing:.5px;
           margin-bottom:5px;}
#legend h4+h4{margin-top:8px;}
.le{display:flex;align-items:center;gap:6px;margin-bottom:3px;}
.ld{width:9px;height:9px;border-radius:50%;flex-shrink:0;}
.ls{width:9px;height:9px;border-radius:1px;flex-shrink:0;}
</style>
</head>
<body>

<div id="hdr">
  <div class="badge">Mea Culpa</div>
  <h1>Ciclo de vida &nbsp;·&nbsp; <strong>', prot_nombre, '</strong></h1>
  <div class="meta">', delito_txt, '<br>', periodo_txt, ' · ', locac_txt, '</div>
</div>

<div id="fase-nav">
  <button id="btn-todos" class="nav-btn activo" onclick="selectFase(\'todos\')">Todos</button>
  ', nav_btns_html, '
  <span class="fase-info" id="fase-info-txt"></span>
</div>

<div id="main">

  <!-- TIMELINE -->
  <div id="left">
    <div id="tl-hdr">Eventos biográficos &nbsp;·&nbsp; click para enfocar la red</div>
    <div id="tl-scroll" id="tl-scroll"></div>
  </div>

  <!-- RED -->
  <div id="right">
    <div id="net-info">
      <span class="fase-pill" id="fase-pill">Todos los momentos</span>
      <span class="stats" id="net-stats"></span>
      <span class="ev-desc-net" id="ev-desc-net"></span>
      <button id="reset-btn" onclick="selectFase(\'todos\')">↺ restablecer</button>
    </div>
    <div id="mynetwork"></div>
    <div id="legend">
      <h4>Personas</h4>
      <div class="le"><div class="ld" style="background:#e8c547;border:1px solid #aaa"></div>Protagonista</div>
      <div class="le"><div class="ld" style="background:#e74c3c"></div>Víctima</div>
      <div class="le"><div class="ld" style="background:#e67e22"></div>Cómplice</div>
      <div class="le"><div class="ld" style="background:#2980b9"></div>Autoridad</div>
      <div class="le"><div class="ld" style="background:#7f8c8d"></div>Tercero</div>
      <div class="le"><div class="ld" style="background:#bdc3c7"></div>Sin rol</div>
      <h4>Entidades</h4>
      <div class="le"><div class="ls" style="background:#c0392b"></div>Arma</div>
      <div class="le"><div class="ls" style="background:#1e8449"></div>Lugar</div>
      <div class="le"><div class="ls" style="background:#7d3c98"></div>Organización</div>
      <div class="le"><div class="ls" style="background:#5d6d7e"></div>Documento</div>
      <div class="le"><div class="ls" style="background:#b7950b"></div>Objeto</div>
      <div class="le"><div class="ls" style="background:#117a65"></div>Económico</div>
    </div>
  </div>
</div>

<script>
var nodesData  = new vis.DataSet(', nodos_json, ');
var edgesData  = new vis.DataSet(', aristas_json, ');
var eventos    = ', eventos_json, ';
var nodeLookup = ', lookup_json, ';

var FASES   = ["antecedentes","escalada","detonante","crimen","consecuencias","proceso_legal"];
var FCOLORS = ', colores_js, ';
var FPERIODS= ', periodos_js, ';
var FLABELS = {antecedentes:"Antecedentes",escalada:"Escalada",detonante:"Detonante",
               crimen:"Crimen",consecuencias:"Consecuencias",proceso_legal:"Proceso legal"};

var allNodes = nodesData.get();
var allEdges = edgesData.get();
var nodeMIdx = {};
allNodes.forEach(function(n){ nodeMIdx[n.id] = FASES.indexOf(n.momento_inicio); });

var estadoActual = {fase:"todos", eventoId: null};

// ── VIS.JS NETWORK ──────────────────────────────────────────────────────────

var network = new vis.Network(
  document.getElementById("mynetwork"),
  {nodes: nodesData, edges: edgesData},
  {
    physics:{
      solver:"forceAtlas2Based",
      forceAtlas2Based:{gravitationalConstant:-85,springLength:120,springConstant:.04,avoidOverlap:.5},
      stabilization:{iterations:260,updateInterval:20}
    },
    interaction:{hover:true,tooltipDelay:80,navigationButtons:true},
    nodes:{shadow:{enabled:true,color:"rgba(0,0,0,.5)",size:7},
           font:{color:"#ecf0f1",size:12}},
    edges:{smooth:{type:"continuous"}},
    layout:{randomSeed:42}
  }
);

network.on("stabilizationIterationsDone", function(){
  network.setOptions({physics:{enabled:false}});
  actualizarStats();
});

network.on("selectNode", function(props){
  if (!props.nodes.length) return;
  var nid = props.nodes[0];
  // Resaltar evento(s) donde aparece este nodo
  var evIds = [];
  Object.keys(nodeLookup).forEach(function(k){
    if ((nodeLookup[k]||[]).indexOf(nid) !== -1) evIds.push(parseInt(k));
  });
  if (evIds.length) resaltarEventosEnTimeline(evIds);
});

// ── TIMELINE ────────────────────────────────────────────────────────────────

function renderTimeline(){
  var scroll = document.getElementById("tl-scroll");
  scroll.innerHTML = "";

  var grupos = {};
  eventos.forEach(function(ev){
    if (!grupos[ev.fase]) grupos[ev.fase] = [];
    grupos[ev.fase].push(ev);
  });

  FASES.filter(function(f){ return grupos[f]; }).forEach(function(fase){
    var fc = FCOLORS[fase]||"#555";
    var faseDiv = document.createElement("div");
    faseDiv.className = "tl-fase";
    faseDiv.setAttribute("data-fase", fase);
    faseDiv.style.setProperty("--fc", fc);

    // Banner
    var banner = document.createElement("div");
    banner.className = "fase-banner";
    banner.innerHTML =
      \'<div class="fase-dot"></div>\' +
      \'<span class="fase-tag">\' + (FLABELS[fase]||fase) + \'</span>\' +
      \'<span class="fase-periodo">\' + (FPERIODS[fase]||"") + \'</span>\';
    banner.onclick = function(){ selectFase(fase); };
    faseDiv.appendChild(banner);

    // Eventos
    grupos[fase].forEach(function(ev){
      var card = document.createElement("div");
      card.className = "ev" + (ev.prot_ok ? " prot-ev" : "");
      card.setAttribute("data-orden", ev.orden);
      card.setAttribute("data-fase", fase);
      card.style.setProperty("--fc", fc);
      card.style.setProperty("--fc-bg", "rgba("+hexToRgb(fc)+",0.1)");

      var badgesHTML = "";
      if ((ev.actores||[]).length || (ev.entidades||[]).length){
        badgesHTML = \'<div class="ev-badges">\';
        (ev.actores||[]).forEach(function(a){
          badgesHTML += \'<span class="ba\' + (a.prot?" prot":"") +
            \'" style="--bc:\' + a.color + \'">\' +
            \'<span class="dot"></span>\' + a.label + \'</span>\';
        });
        (ev.entidades||[]).forEach(function(e){
          badgesHTML += \'<span class="be" style="--bc:\' + e.color + \'">\' +
            \'<span class="sq"></span>\' + e.label + \'</span>\';
        });
        badgesHTML += \'</div>\';
      }

      card.innerHTML =
        \'<div class="ev-header">\' +
        \'<span class="ev-num">#\' + ev.orden + \'</span>\' +
        \'<span class="ev-titulo">\' + (ev.titulo||"") + \'</span>\' +
        (ev.prot_ok ? \'<div class="prot-pip"></div>\' : "") +
        \'</div>\' +
        (ev.descripcion ? \'<div class="ev-desc">\' + ev.descripcion + \'</div>\' : "") +
        badgesHTML;

      card.onclick = function(){ selectEvento(ev.orden, ev); };
      faseDiv.appendChild(card);
    });

    scroll.appendChild(faseDiv);
  });
}

// ── SELECCIÓN DE FASE ────────────────────────────────────────────────────────

function selectFase(fase){
  estadoActual = {fase: fase, eventoId: null};
  var maxIdx = (fase === "todos") ? 99 : FASES.indexOf(fase);

  // Red: actualizar nodos
  nodesData.update(allNodes.map(function(n){
    var ni = nodeMIdx[n.id] === -1 ? 0 : nodeMIdx[n.id];
    if (fase === "todos")
      return {id:n.id, hidden:false, color:n.colorOriginal, borderWidth:n.es_prot?5:2, size:n.sizeOriginal};
    if (ni > maxIdx)
      return {id:n.id, hidden:true};
    if (ni === maxIdx)
      return {id:n.id, hidden:false,
              color:{background:n.colorOriginal, border:"white",
                     highlight:{background:n.colorOriginal,border:"white"}},
              borderWidth:n.es_prot?6:5, size:n.sizeOriginal*1.12};
    return {id:n.id, hidden:false,
            color:{background:"#232b3a", border:"#3a4a5e",
                   highlight:{background:"#2d3a4d",border:"#5a6a7e"}},
            borderWidth:1, size:n.sizeOriginal*0.8};
  }));

  // Red: actualizar aristas
  edgesData.update(allEdges.map(function(e){
    var fi = FASES.indexOf(e.momento_inicio); if (fi===-1) fi=0;
    var fh = (nodeMIdx[e.from]||0) > maxIdx;
    var th = (nodeMIdx[e.to]  ||0) > maxIdx;
    if (fase !== "todos" && (fi > maxIdx || fh || th))
      return {id:e.id, hidden:true};
    var curr = (fi === maxIdx && fase !== "todos");
    return {id:e.id, hidden:false,
            color: curr ? e.colorOriginal : "#2a3445",
            width: curr ? e.widthOriginal*1.3 : e.widthOriginal*0.45,
            dashes:e.dashes};
  }));

  // Fit camera
  if (fase !== "todos"){
    var visibles = allNodes.filter(function(n){ return (nodeMIdx[n.id]||0) <= maxIdx; }).map(function(n){return n.id;});
    if (visibles.length) network.fit({nodes:visibles, animation:{duration:500,easingFunction:"easeInOutQuad"}});
  } else {
    network.fit({animation:{duration:400,easingFunction:"easeInOutQuad"}});
  }

  // Info bar
  var fc = fase === "todos" ? "#2c3e50" : (FCOLORS[fase]||"#555");
  document.getElementById("fase-pill").textContent = fase==="todos"?"Todos los momentos":(FLABELS[fase]||fase);
  document.getElementById("fase-pill").style.background = fc;
  document.getElementById("ev-desc-net").textContent = fase==="todos" ? "" : (FPERIODS[fase]||"");

  // Botones nav
  document.querySelectorAll(".nav-btn").forEach(function(b){
    var isA = (b.dataset.fase||"todos") === fase || (b.id==="btn-todos" && fase==="todos");
    b.classList.toggle("activo", isA);
    if (isA && fase !== "todos") { b.style.background=fc; b.style.borderColor=fc; b.style.color="#fff"; }
    else if (isA) { b.style.background="#1e2633"; b.style.borderColor="#566573"; b.style.color="#fff"; }
    else { b.style.background="transparent"; b.style.borderColor="#2c3e50"; b.style.color="#7f8c8d"; }
  });

  // Timeline: quitar activo, resaltar fase
  document.querySelectorAll(".ev").forEach(function(el){
    el.classList.remove("activo");
    var ef = el.getAttribute("data-fase");
    el.style.opacity = (fase === "todos" || ef === fase) ? "1" : "0.35";
  });
  document.querySelectorAll(".tl-fase").forEach(function(el){
    el.style.opacity = (fase === "todos" || el.getAttribute("data-fase") === fase) ? "1" : "0.4";
  });

  actualizarStats();
}

// ── SELECCIÓN DE EVENTO ──────────────────────────────────────────────────────

function selectEvento(orden, ev){
  estadoActual = {fase: ev.fase, eventoId: orden};
  var ids = nodeLookup[String(orden)] || [];

  // Red: resaltar solo participantes del evento
  nodesData.update(allNodes.map(function(n){
    var activo = ids.indexOf(n.id) !== -1;
    if (activo)
      return {id:n.id, hidden:false,
              color:{background:n.colorOriginal, border:"white",
                     highlight:{background:n.colorOriginal,border:"white"}},
              borderWidth:n.es_prot?6:5, size:n.sizeOriginal*1.15};
    return {id:n.id, hidden:false,
            color:{background:"#1a1d23", border:"#1e2633",
                   highlight:{background:"#1e2633",border:"#2c3e50"}},
            borderWidth:1, size:n.sizeOriginal*0.7};
  }));

  edgesData.update(allEdges.map(function(e){
    var fa = ids.indexOf(e.from)!==-1, ta = ids.indexOf(e.to)!==-1;
    if (fa && ta)
      return {id:e.id, hidden:false, color:e.colorOriginal, width:e.widthOriginal*1.4, dashes:e.dashes};
    return {id:e.id, hidden:false, color:"#1a1d23", width:0.4, dashes:e.dashes};
  }));

  if (ids.length) network.fit({nodes:ids, animation:{duration:500,easingFunction:"easeInOutQuad"}});

  // Info bar
  var fc = FCOLORS[ev.fase]||"#555";
  document.getElementById("fase-pill").textContent = "#"+orden+" · "+ev.titulo;
  document.getElementById("fase-pill").style.background = fc;
  document.getElementById("ev-desc-net").textContent = ev.descripcion||"";

  // Timeline: resaltar evento activo
  document.querySelectorAll(".ev").forEach(function(el){
    var isA = parseInt(el.getAttribute("data-orden")) === orden;
    el.classList.toggle("activo", isA);
    el.style.opacity = isA ? "1" : "0.4";
  });

  document.querySelectorAll(".tl-fase").forEach(function(el){
    el.style.opacity = el.getAttribute("data-fase") === ev.fase ? "1" : "0.35";
  });

  actualizarStats();
}

function resaltarEventosEnTimeline(evIds){
  document.querySelectorAll(".ev").forEach(function(el){
    var ord = parseInt(el.getAttribute("data-orden"));
    el.classList.toggle("activo", evIds.indexOf(ord)!==-1);
    el.style.opacity = evIds.indexOf(ord)!==-1 ? "1" : "0.5";
  });
}

function actualizarStats(){
  var visibles = allNodes.filter(function(n){ return !n.hidden; });
  document.getElementById("net-stats").textContent =
    visibles.length + " nodo" + (visibles.length!==1?"s":"") + " visibles";
}

// ── UTILIDADES ───────────────────────────────────────────────────────────────

function hexToRgb(hex){
  var r=/^#?([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2})$/i.exec(hex);
  return r ? parseInt(r[1],16)+","+parseInt(r[2],16)+","+parseInt(r[3],16) : "80,80,80";
}

// INIT
renderTimeline();
actualizarStats();
</script>
</body>
</html>')

# ─── GUARDAR ──────────────────────────────────────────────────────────────────

pref <- str_to_lower(str_replace_all(prot_primer,"[^a-zA-Z]",""))
salida <- paste0("red_ciclo_", pref, "_", EPI_ID, ".html")
writeLines(html, salida, useBytes = TRUE)

message("\n✓ Guardado: ", salida)
message("  Eventos: ", nrow(eventos_lista), " | Nodos: ", nrow(nodos),
        " (", nrow(nodos_pers)," personas + ", nrow(nodos_ent)," entidades)",
        " | Aristas: ", nrow(aristas))
message("\n  Momentos:")
for (m in MOMENTOS_ORDEN) {
  n <- sum(eventos_lista$fase == m)
  if (n > 0) message(sprintf("  %-20s %2d eventos  %s", MOMENTO_LABEL[m], n, strrep("█", n)))
}
