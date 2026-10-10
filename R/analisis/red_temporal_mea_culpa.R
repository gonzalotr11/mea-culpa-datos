# =============================================================================
# RED SOCIO-MATERIAL TEMPORAL — MEA CULPA
# Genera un HTML interactivo donde la red se revela momento a momento:
# antecedentes → escalada → detonante → crimen → consecuencias → proceso_legal
#
# Paquetes: dplyr, readr, stringr, purrr, jsonlite
# Uso: ajustar EPI_ID abajo y correr desde el directorio con los CSV
# =============================================================================

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(jsonlite)

# ─── CONFIGURACIÓN ────────────────────────────────────────────────────────────

EPI_ID <- "temporada_14_cap04"   # ← cambiar por episodio deseado

MOMENTOS_ORDEN <- c("antecedentes","escalada","detonante","crimen","consecuencias","proceso_legal")
MOMENTO_LABEL  <- c(
  antecedentes  = "Antecedentes",  escalada      = "Escalada",
  detonante     = "Detonante",     crimen        = "Crimen",
  consecuencias = "Consecuencias", proceso_legal = "Proceso legal"
)
MOMENTO_COLOR  <- c(
  antecedentes  = "#2980b9", escalada      = "#e67e22",
  detonante     = "#e74c3c", crimen        = "#7b241c",
  consecuencias = "#7d3c98", proceso_legal = "#1a5276"
)

# ─── PALETAS ──────────────────────────────────────────────────────────────────

ROL_COLOR <- c(
  victimario="#1c2833", victima="#e74c3c", complice="#e67e22",
  autoridad="#2980b9", tercero_afectado="#7f8c8d", sin_rol_criminal="#d5d8dc"
)
ENT_COLOR <- c(
  arma="#c0392b", lugar="#1e8449", organizacion="#7d3c98",
  objeto_material="#b7950b", objeto_economico="#117a65",
  documento="#5d6d7e", vehiculo="#1a5276", sustancia="#922b21"
)
ASIM_COLOR <- c(
  from_domina="#e74c3c", to_domina="#2980b9",
  par="#aab7b8", variable="#e67e22", incierta="#d5d8dc"
)

lookup <- function(x, tabla, default = "#aab7b8") {
  res <- tabla[as.character(x)]; res[is.na(res)] <- default; unname(res)
}

# ─── CARGAR DATOS ─────────────────────────────────────────────────────────────

leer <- function(f) if (file.exists(f)) read_csv(f, show_col_types = FALSE) else { message("⚠ no encontrado: ", f); tibble() }
leer_f <- function(f, epi) { df <- leer(f); if (nrow(df) > 0) filter(df, id_episodio == epi) else df }

epi_e  <- leer_f("01_episodios.csv",           EPI_ID)
pers_e <- leer_f("04_personajes.csv",           EPI_ID)
rel_e  <- leer_f("05_relaciones.csv",           EPI_ID)
vp_e   <- leer_f("06_vinculos_practicos.csv",   EPI_ID)
va_e   <- leer_f("07_vinculos_afectivos.csv",   EPI_ID)
ent_e  <- leer_f("08_entidades.csv",            EPI_ID)
rpe_e  <- leer_f("09_relaciones_pers_ent.csv",  EPI_ID)

# Narrativos (opcionales — producidos por extraccion_narrativa.R)
fases_e <- leer_f("10_fases_narrativas.csv",    EPI_ID)
meca_e  <- leer_f("11_mecanismos_causales.csv", EPI_ID)
USAR_NARRATIVA <- nrow(fases_e) > 0

message(if (USAR_NARRATIVA) "✓ Usando fases narrativas para momentos" else
        "ℹ Fases no disponibles — usando heurísticos de momento")

# ─── ASIGNACIÓN DE MOMENTOS ───────────────────────────────────────────────────

# Heurístico para personajes (cuando no hay fases)
m_pers <- function(rol, etario) {
  case_when(
    rol    == "victimario"                     ~ "antecedentes",
    rol    == "autoridad"                      ~ "consecuencias",
    rol    == "victima"                        ~ "escalada",
    rol    == "complice"                       ~ "escalada",
    etario %in% c("mayor","adulto_mayor")      ~ "antecedentes",
    TRUE                                       ~ "antecedentes"
  )
}

# Heurístico para entidades (cuando no hay fases)
m_ent <- function(funcion, tipo) {
  case_when(
    funcion == "instrumento_del_delito"  ~ "crimen",
    funcion == "lugar_del_delito"        ~ "crimen",
    funcion == "fuente_de_conflicto"     ~ "escalada",
    funcion == "objeto_disputado"        ~ "escalada",
    funcion == "contexto_victimizacion"  ~ "escalada",
    funcion == "institucion_fallida"     ~ "detonante",
    funcion == "recurso_estatus"         ~ "antecedentes",
    tipo    == "arma"                    ~ "crimen",
    tipo    == "documento"               ~ "escalada",
    TRUE                                 ~ "antecedentes"
  )
}

idx_m <- function(m) match(m, MOMENTOS_ORDEN)   # convierte momento a índice ordinal
max_m <- function(a, b) MOMENTOS_ORDEN[pmax(coalesce(idx_m(a),1L), coalesce(idx_m(b),1L))]

if (USAR_NARRATIVA) {
  # Derivar momento de primera aparición desde fases
  actor_m <- fases_e |>
    mutate(actores = map(str_split(coalesce(actores,""), " ; "),
                         ~ str_trim(.x[.x != "" & .x != "NA"]))) |>
    unnest(actores) |>
    left_join(pers_e |> select(nombre, id_personaje_global), by = c("actores" = "nombre")) |>
    filter(!is.na(id_personaje_global)) |>
    group_by(id_personaje_global) |>
    slice_min(orden, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(id_personaje_global, momento_inicio = fase)

  pers_e <- pers_e |>
    left_join(actor_m, by = "id_personaje_global") |>
    mutate(momento_inicio = coalesce(momento_inicio, "antecedentes"))

  # Entidades: momento del actor más temprano que las conecta
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
    momento_inicio = m_pers(coalesce(rol_criminologico,"sin_rol_criminal"), coalesce(grupo_etario,"adulto"))
  )
  ent_e <- ent_e |> mutate(
    momento_inicio = m_ent(coalesce(funcion_narrativa,"recurso_estatus"), coalesce(tipo,"objeto_material"))
  )
}

# ─── NODOS ────────────────────────────────────────────────────────────────────

vp_res <- vp_e |> group_by(id_relacion) |> summarise(vp = paste(vinculo_practico, collapse = " · "), .groups = "drop")
va_res <- va_e |> group_by(id_relacion) |> summarise(va = paste(vinculo_afectivo,  collapse = " · "), .groups = "drop")

nodos_pers <- pers_e |> transmute(
  id             = id_personaje_global,
  label          = str_wrap(coalesce(nombre, id_personaje), 14),
  shape          = "dot",
  colorOriginal  = lookup(coalesce(rol_criminologico, "sin_rol_criminal"), ROL_COLOR),
  sizeOriginal   = 20 + pmax(0, pmin(4, coalesce(ses_nivel, 3L) - 1L)) * 7,
  group          = coalesce(rol_criminologico, "sin_rol_criminal"),
  momento_inicio = momento_inicio,
  title = paste0(
    "<div style='font-family:sans-serif;max-width:220px;line-height:1.5'>",
    "<b>", coalesce(nombre, id_personaje), "</b><br>",
    "<i style='color:#aaa'>", coalesce(rol_narrativo,""), " / ", coalesce(rol_criminologico,""), "</i><br>",
    "SES: ", coalesce(as.character(ses_nivel),"?"), "/5 · ", coalesce(ocupacion,"—"), "<br>",
    "<small>", coalesce(descripcion,""), "</small>",
    "<br><span style='color:", MOMENTO_COLOR[momento_inicio], ";font-size:10px'>▸ ",
    MOMENTO_LABEL[momento_inicio], "</span></div>"
  )
) |> mutate(color = colorOriginal, size = sizeOriginal)

nodos_ent <- ent_e |> transmute(
  id             = id_entidad_global,
  label          = str_wrap(coalesce(nombre, id_entidad), 14),
  shape          = "square",
  colorOriginal  = lookup(coalesce(tipo, "objeto_material"), ENT_COLOR),
  sizeOriginal   = case_when(centralidad == "alta" ~ 42, centralidad == "media" ~ 30, TRUE ~ 20),
  group          = paste0("[", coalesce(tipo, "objeto"), "]"),
  momento_inicio = momento_inicio,
  title = paste0(
    "<div style='font-family:sans-serif;max-width:220px;line-height:1.5'>",
    "<b>", coalesce(nombre, id_entidad), "</b><br>",
    "<i style='color:#aaa'>", coalesce(tipo,""), " · ", coalesce(subtipo,""), "</i><br>",
    "Función: <b>", coalesce(funcion_narrativa,"—"), "</b><br>",
    "<small>", coalesce(descripcion,""), "</small>",
    "<br><span style='color:", MOMENTO_COLOR[momento_inicio], ";font-size:10px'>▸ ",
    MOMENTO_LABEL[momento_inicio], "</span></div>"
  )
) |> mutate(color = colorOriginal, size = sizeOriginal)

nodos <- bind_rows(nodos_pers, nodos_ent)

# ─── ARISTAS ──────────────────────────────────────────────────────────────────

# Lookup de momento por id de nodo
m_map <- setNames(c(nodos_pers$momento_inicio, nodos_ent$momento_inicio),
                  c(nodos_pers$id,             nodos_ent$id))

aristas_pp <- rel_e |>
  left_join(vp_res, by = "id_relacion") |>
  left_join(va_res, by = "id_relacion") |>
  filter(from_personaje_global %in% nodos$id, to_personaje_global %in% nodos$id) |>
  transmute(
    from           = from_personaje_global,
    to             = to_personaje_global,
    colorOriginal  = lookup(coalesce(asimetria_poder, "par"), ASIM_COLOR),
    arrows         = case_when(asimetria_poder=="from_domina"~"to", asimetria_poder=="to_domina"~"from", TRUE~""),
    dashes         = FALSE,
    widthOriginal  = 2.5,
    momento_inicio = max_m(m_map[from_personaje_global], m_map[to_personaje_global]),
    title = paste0(
      "<div style='font-family:sans-serif;max-width:200px'>",
      "<b>", coalesce(asimetria_poder,"?"), "</b><br>",
      "Práctico: ", coalesce(vp,"—"), "<br>Afectivo: ", coalesce(va,"—"), "<br>",
      "<i style='color:#aaa'>", coalesce(descripcion,""), "</i></div>"
    )
  ) |> mutate(color = colorOriginal, width = widthOriginal)

aristas_pe <- rpe_e |>
  filter(id_personaje_global %in% nodos$id, id_entidad_global %in% nodos$id) |>
  transmute(
    from           = id_personaje_global,
    to             = id_entidad_global,
    colorOriginal  = "#6c757d",
    arrows         = "to",
    dashes         = TRUE,
    widthOriginal  = 1.5,
    momento_inicio = max_m(m_map[id_personaje_global], m_map[id_entidad_global]),
    title = paste0(
      "<div style='font-family:sans-serif;max-width:200px'>",
      "<b>", coalesce(tipo_vinculo,"usa"), "</b><br>",
      "<i style='color:#aaa'>", coalesce(descripcion,""), "</i></div>"
    )
  ) |> mutate(color = colorOriginal, width = widthOriginal)

aristas <- bind_rows(aristas_pp, aristas_pe) |> mutate(id = row_number())

# ─── METADATA DEL EPISODIO ────────────────────────────────────────────────────

titulo_txt   <- str_replace_all(EPI_ID, "_", " ")
delito_txt   <- str_replace_all(coalesce(epi_e$categoria_delito_principal[1], ""), "_", " ")
periodo_txt  <- coalesce(epi_e$periodo_temporal[1], "")
locacion_txt <- coalesce(epi_e$locacion_principal[1], "")
resumen_txt  <- coalesce(epi_e$resumen[1], "")

meca_txt <- if (USAR_NARRATIVA && nrow(meca_e) > 0) paste0(
  str_replace_all(coalesce(meca_e$tipo_mecanismo[1],""), "_"," "),
  " · ", str_replace_all(coalesce(meca_e$patron_relacional[1],""), "_"," "),
  " · ", coalesce(meca_e$factor_detonante[1],"")
) else ""

# ─── SERIALIZACIÓN JSON ───────────────────────────────────────────────────────

nodos_json   <- toJSON(nodos,   auto_unbox = TRUE)
aristas_json <- toJSON(aristas, auto_unbox = TRUE)

# Botones de momento (solo los que tienen al menos un nodo)
fases_presentes <- MOMENTOS_ORDEN[MOMENTOS_ORDEN %in% unique(c(nodos$momento_inicio, aristas$momento_inicio))]
botones_html <- paste(
  map_chr(fases_presentes, function(f) {
    sprintf('<button class="btn-m" data-m="%s" onclick="setMomento(\'%s\')" style="border-color:%s">%s</button>',
            f, f, MOMENTO_COLOR[f], MOMENTO_LABEL[f])
  }),
  collapse = "\n    "
)

colores_js <- paste0(
  "{", paste(
    map_chr(MOMENTOS_ORDEN, ~ sprintf('"%s":"%s"', .x, MOMENTO_COLOR[.x])),
    collapse = ","
  ), "}"
)

# ─── GENERAR HTML ─────────────────────────────────────────────────────────────

html <- paste0('<!DOCTYPE html>
<html lang="es">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Red Temporal · ', titulo_txt, '</title>
<script src="https://cdnjs.cloudflare.com/ajax/libs/vis/4.21.0/vis.min.js"></script>
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/vis/4.21.0/vis.min.css">
<style>
*{box-sizing:border-box;margin:0;padding:0;}
body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif;
     background:#111418;color:#ecf0f1;height:100vh;display:flex;flex-direction:column;overflow:hidden;}

#hdr{background:#0d1117;padding:8px 16px;border-bottom:2px solid #e74c3c;
     display:flex;align-items:center;gap:12px;flex-shrink:0;}
#hdr .badge{background:#e74c3c;color:white;font-size:9px;font-weight:700;
             padding:2px 7px;border-radius:3px;letter-spacing:.6px;text-transform:uppercase;}
#hdr h1{font-size:14px;font-weight:600;}
#hdr h1 em{color:#e74c3c;font-style:normal;}
#hdr .meta{font-size:10px;color:#7f8c8d;margin-left:auto;text-align:right;line-height:1.6;}

#meca{background:#141820;padding:5px 16px;font-size:10px;color:#95a5a6;
      border-bottom:1px solid #1e2633;flex-shrink:0;}
#meca b{color:#e67e22;}

#momento-bar{background:#0d1117;padding:7px 16px;border-bottom:1px solid #1e2633;
             display:flex;align-items:center;gap:8px;flex-wrap:wrap;flex-shrink:0;}
.btn-m{background:transparent;color:#7f8c8d;border:1px solid #2c3e50;
       border-radius:4px;padding:4px 11px;font-size:11px;font-weight:600;
       cursor:pointer;transition:all .18s;letter-spacing:.3px;}
.btn-m:hover{color:#ecf0f1;border-color:#566573;}
.btn-m.activo{color:white;font-weight:700;}
#btn-todos{border-color:#566573;color:#aaa;}
#btn-todos.activo{background:#2c3e50;color:white;}

#stats{margin-left:auto;font-size:10px;color:#566573;display:flex;gap:10px;}
#stat-visible{color:#7f8c8d;}
#stat-new{font-weight:700;}

#net-wrap{flex:1;position:relative;min-height:0;}
#mynetwork{width:100%;height:100%;}

#legend{position:absolute;top:10px;right:10px;background:rgba(13,17,23,.92);
        border:1px solid #1e2633;border-radius:6px;padding:10px 14px;font-size:10px;
        color:#bdc3c7;min-width:138px;}
#legend h4{font-size:9px;color:#566573;text-transform:uppercase;letter-spacing:.5px;
           margin-bottom:5px;}
#legend h4+h4{margin-top:9px;}
.le{display:flex;align-items:center;gap:6px;margin-bottom:3px;}
.ld{width:10px;height:10px;border-radius:50%;flex-shrink:0;}
.ls{width:10px;height:10px;border-radius:2px;flex-shrink:0;}

#resumen-btn{position:absolute;bottom:10px;left:10px;background:rgba(13,17,23,.88);
             border:1px solid #2c3e50;border-radius:4px;padding:4px 10px;
             font-size:10px;color:#7f8c8d;cursor:pointer;}
#resumen-btn:hover{color:#ecf0f1;}
#resumen-box{position:absolute;bottom:40px;left:10px;max-width:330px;
             background:rgba(13,17,23,.92);border:1px solid #2c3e50;border-radius:6px;
             padding:10px 12px;font-size:10px;color:#95a5a6;line-height:1.55;display:none;}

.vis-tooltip{background:#1a1d23 !important;color:#ecf0f1 !important;
             border:1px solid #2c3e50 !important;border-radius:4px !important;
             padding:6px 10px !important;font-size:11px !important;}
</style>
</head>
<body>

<div id="hdr">
  <div class="badge">Mea Culpa</div>
  <h1>', titulo_txt, ' &nbsp;·&nbsp; <em>', delito_txt, '</em></h1>
  <div class="meta">', periodo_txt, '&ensp;·&ensp;', locacion_txt, '</div>
</div>
', if (meca_txt != "") paste0('<div id="meca"><b>Mecanismo:</b> ', meca_txt, '</div>') else '', '
<div id="momento-bar">
  <button id="btn-todos" class="btn-m activo" onclick="setMomento(\'todos\')">Todos</button>
  ', botones_html, '
  <div id="stats">
    <span id="stat-visible"></span>
    <span id="stat-new"></span>
  </div>
</div>

<div id="net-wrap">
  <div id="mynetwork"></div>

  <div id="legend">
    <h4>Personas</h4>
    <div class="le"><div class="ld" style="background:#1c2833;border:1px solid #555"></div>Victimario</div>
    <div class="le"><div class="ld" style="background:#e74c3c"></div>Víctima</div>
    <div class="le"><div class="ld" style="background:#e67e22"></div>Cómplice</div>
    <div class="le"><div class="ld" style="background:#2980b9"></div>Autoridad</div>
    <div class="le"><div class="ld" style="background:#7f8c8d"></div>Tercero afectado</div>
    <div class="le"><div class="ld" style="background:#d5d8dc"></div>Sin rol criminal</div>
    <h4>Entidades</h4>
    <div class="le"><div class="ls" style="background:#c0392b"></div>Arma</div>
    <div class="le"><div class="ls" style="background:#1e8449"></div>Lugar</div>
    <div class="le"><div class="ls" style="background:#7d3c98"></div>Organización</div>
    <div class="le"><div class="ls" style="background:#5d6d7e"></div>Documento</div>
    <div class="le"><div class="ls" style="background:#b7950b"></div>Objeto</div>
    <div class="le"><div class="ls" style="background:#117a65"></div>Económico</div>
    <h4>Aristas P–P</h4>
    <div class="le"><div class="ld" style="background:#e74c3c;border-radius:0;height:2px;width:16px"></div>domina →</div>
    <div class="le"><div class="ld" style="background:#aab7b8;border-radius:0;height:2px;width:16px"></div>par</div>
    <div class="le"><div class="ld" style="background:#e67e22;border-radius:0;height:2px;width:16px"></div>variable</div>
  </div>

  <div id="resumen-box">', resumen_txt, '</div>
  <button id="resumen-btn" onclick="toggleResumen()">&#128221; resumen</button>
</div>

<script>
var nodesData  = new vis.DataSet(', nodos_json, ');
var edgesData  = new vis.DataSet(', aristas_json, ');

var FASES = ["antecedentes","escalada","detonante","crimen","consecuencias","proceso_legal"];
var FASES_COLOR = ', colores_js, ';

var allNodes = nodesData.get();
var allEdges = edgesData.get();

// Precalcular indice de momento por nodo
var nodeMIdx = {};
allNodes.forEach(function(n) {
  nodeMIdx[n.id] = FASES.indexOf(n.momento_inicio);
  if (nodeMIdx[n.id] === -1) nodeMIdx[n.id] = 0;
});

// Inicializar red
var network = new vis.Network(
  document.getElementById("mynetwork"),
  {nodes: nodesData, edges: edgesData},
  {
    physics:{
      solver:"forceAtlas2Based",
      forceAtlas2Based:{gravitationalConstant:-85,springLength:130,springConstant:.045,avoidOverlap:.4},
      stabilization:{iterations:280,updateInterval:20}
    },
    interaction:{hover:true,tooltipDelay:80,navigationButtons:true},
    nodes:{
      borderWidth:2, borderWidthSelected:4,
      shadow:{enabled:true,color:"rgba(0,0,0,.45)",size:7},
      font:{color:"#ecf0f1",size:12}
    },
    edges:{smooth:{type:"continuous"}},
    layout:{randomSeed:42},
    configure:{enabled:false}
  }
);

network.on("stabilizationIterationsDone", function() {
  network.setOptions({physics:{enabled:false}});
  updateStats("todos");
});

// ── Filtrado temporal ──────────────────────────────────────────────────────

function setMomento(m) {
  var maxIdx = (m === "todos") ? 99 : FASES.indexOf(m);

  var nodeUpdates = allNodes.map(function(n) {
    var nIdx = nodeMIdx[n.id];
    if (m === "todos") {
      return {id:n.id, hidden:false, color:n.colorOriginal,
              borderWidth:2, size:n.sizeOriginal};
    }
    if (nIdx > maxIdx) {
      return {id:n.id, hidden:true};
    } else if (nIdx === maxIdx) {
      // Nuevo en este momento: borde blanco brillante
      return {
        id:n.id, hidden:false,
        color:{background:n.colorOriginal,
               border:"white",
               highlight:{background:n.colorOriginal,border:"white"},
               hover:{background:n.colorOriginal,border:"white"}},
        borderWidth:5,
        size:n.sizeOriginal * 1.12
      };
    } else {
      // Pasado: atenuado
      return {
        id:n.id, hidden:false,
        color:{background:"#252c3a",border:"#3a4455",
               highlight:{background:"#252c3a",border:"#5a6a7e"},
               hover:{background:"#2e3848",border:"#5a6a7e"}},
        borderWidth:1,
        size:n.sizeOriginal * 0.82
      };
    }
  });
  nodesData.update(nodeUpdates);

  var edgeUpdates = allEdges.map(function(e) {
    var fIdx = FASES.indexOf(e.momento_inicio);
    if (fIdx === -1) fIdx = 0;
    var fromHid = nodeMIdx[e.from] > maxIdx;
    var toHid   = nodeMIdx[e.to]   > maxIdx;
    if (m !== "todos" && (fIdx > maxIdx || fromHid || toHid)) {
      return {id:e.id, hidden:true};
    }
    var isCurrent = (m !== "todos" && fIdx === maxIdx);
    return {
      id:e.id, hidden:false,
      color:isCurrent ? e.colorOriginal : "#2e3848",
      width:isCurrent ? e.widthOriginal * 1.3 : e.widthOriginal * 0.5,
      dashes:e.dashes
    };
  });
  edgesData.update(edgeUpdates);

  // Estilos de botones
  document.querySelectorAll(".btn-m").forEach(function(b) {
    var bm = b.dataset.m || "todos";
    var isActive = bm === m;
    b.classList.toggle("activo", isActive);
    if (isActive && m !== "todos") {
      b.style.background = FASES_COLOR[m];
      b.style.borderColor = FASES_COLOR[m];
      b.style.color = "white";
    } else if (isActive) {
      b.style.background = "#2c3e50";
      b.style.borderColor = "#566573";
      b.style.color = "white";
    } else {
      b.style.background = "transparent";
      b.style.borderColor = "#2c3e50";
      b.style.color = "#7f8c8d";
    }
  });

  updateStats(m);
}

function updateStats(m) {
  var maxIdx = (m === "todos") ? 99 : FASES.indexOf(m);
  var visible = allNodes.filter(function(n) { return nodeMIdx[n.id] <= maxIdx; });
  var nuevo   = (m === "todos") ? allNodes : allNodes.filter(function(n) { return nodeMIdx[n.id] === maxIdx; });
  document.getElementById("stat-visible").textContent =
    visible.length + " nodo" + (visible.length !== 1 ? "s" : "") + " visibles";
  document.getElementById("stat-new").textContent =
    nuevo.length > 0 ? ("+" + nuevo.length + " en este momento") : "";
  if (m !== "todos") {
    document.getElementById("stat-new").style.color = FASES_COLOR[m] || "#aaa";
  } else {
    document.getElementById("stat-new").style.color = "#7f8c8d";
  }
}

// Resumen del caso
function toggleResumen() {
  var box = document.getElementById("resumen-box");
  var btn = document.getElementById("resumen-btn");
  var visible = box.style.display === "block";
  box.style.display = visible ? "none" : "block";
  btn.style.display = visible ? "block" : "none";
}

// Arrancar con todos los nodos visibles
updateStats("todos");
</script>
</body>
</html>')

# ─── GUARDAR ──────────────────────────────────────────────────────────────────

archivo_salida <- paste0("red_temporal_", EPI_ID, ".html")
writeLines(html, archivo_salida, useBytes = TRUE)

message("\n✓ Guardado: ", archivo_salida)
message("  Abre el archivo en tu navegador para verlo")
message("\n  Nodos por momento:")
conteo <- table(nodos$momento_inicio)[MOMENTOS_ORDEN]
conteo[is.na(conteo)] <- 0
for (m in MOMENTOS_ORDEN) {
  bar <- strrep("█", conteo[m])
  message(sprintf("  %-15s  %2d  %s", MOMENTO_LABEL[m], conteo[m], bar))
}
