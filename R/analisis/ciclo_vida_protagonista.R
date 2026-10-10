# =============================================================================
# CICLO DE VIDA DEL PROTAGONISTA — MEA CULPA
# Timeline biográfico interactivo del victimario principal.
# Ordena cronológicamente todos los antecedentes disponibles
# y genera un HTML navegable por fase.
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
                     detonante="Detonante", crimen="Crimen",
                     consecuencias="Consecuencias", proceso_legal="Proceso legal")
MOMENTO_COLOR   <- c(antecedentes="#2980b9", escalada="#e67e22",
                     detonante="#e74c3c", crimen="#7b241c",
                     consecuencias="#7d3c98", proceso_legal="#1a5276")
MOMENTO_PERIODO <- c(antecedentes="Infancia · Adultez temprana",
                     escalada="Acumulación pre-crimen",
                     detonante="Punto de quiebre",
                     crimen="El crimen",
                     consecuencias="Descubrimiento",
                     proceso_legal="Proceso judicial")

ROL_COLOR <- c(victimario="#1c2833", victima="#e74c3c", complice="#e67e22",
               autoridad="#2980b9", tercero_afectado="#7f8c8d", sin_rol_criminal="#bdc3c7")
ENT_COLOR <- c(arma="#c0392b", lugar="#1e8449", organizacion="#7d3c98",
               objeto_material="#b7950b", objeto_economico="#117a65",
               documento="#5d6d7e", vehiculo="#1a5276", sustancia="#922b21")

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
message(if (USAR_NARRATIVA) "✓ Usando fases narrativas" else
        "ℹ Sin fases narrativas — construyendo desde relaciones y entidades")

# ─── IDENTIFICAR PROTAGONISTA ─────────────────────────────────────────────────

protagonista <- pers_e |>
  mutate(puntaje = (rol_narrativo == "protagonista") * 2 + (rol_criminologico == "victimario")) |>
  slice_max(puntaje, n = 1, with_ties = FALSE)

prot_id      <- protagonista$id_personaje_global
prot_nombre  <- protagonista$nombre
prot_primer  <- str_split(prot_nombre, " ")[[1]][1]

message("Protagonista: ", prot_nombre)

# ─── HELPERS ──────────────────────────────────────────────────────────────────

vp_res <- vp_e |> group_by(id_relacion) |>
  summarise(vp = paste(vinculo_practico, collapse = " · "), .groups = "drop")
va_res <- va_e |> group_by(id_relacion) |>
  summarise(va = paste(vinculo_afectivo,  collapse = " · "), .groups = "drop")

# Convierte nombre a id_personaje_global por coincidencia del primer nombre
nombre_a_id <- function(nombre_str) {
  primer <- str_split(nombre_str, " ")[[1]][1]
  m <- pers_e$id_personaje_global[
    str_detect(pers_e$nombre, regex(primer, ignore_case = TRUE))]
  if (length(m) > 0) m[1] else NA_character_
}

# Convierte vector de IDs a lista de objetos {nombre, color, rol, es_prot}
ids_a_actores <- function(ids) {
  map(ids[!is.na(ids)], function(i) {
    p <- pers_e[pers_e$id_personaje_global == i, ]
    if (nrow(p) == 0) return(NULL)
    list(id      = i,
         label   = str_split(p$nombre[1], " ")[[1]][1],
         nombre  = p$nombre[1],
         rol     = coalesce(p$rol_criminologico[1], "sin_rol_criminal"),
         color   = unname(ROL_COLOR[coalesce(p$rol_criminologico[1], "sin_rol_criminal")]),
         es_prot = i == prot_id)
  }) |> compact()
}

ids_a_entidades <- function(ids) {
  map(ids[!is.na(ids)], function(i) {
    e <- ent_e[ent_e$id_entidad_global == i, ]
    if (nrow(e) == 0) return(NULL)
    list(id     = i,
         label  = str_trunc(e$nombre[1], 18),
         nombre = e$nombre[1],
         tipo   = coalesce(e$tipo[1], "objeto_material"),
         color  = unname(ENT_COLOR[coalesce(e$tipo[1], "objeto_material")]))
  }) |> compact()
}

# ─── CONSTRUIR EVENTOS BIOGRÁFICOS ────────────────────────────────────────────

if (USAR_NARRATIVA) {

  # ── Modo 1: desde fases_narrativas ──────────────────────────────────────────

  # Para cada evento, parsear actores y vincular a entidades si es posible
  ent_por_fase <- rpe_e |>
    filter(id_personaje_global == prot_id) |>
    left_join(ent_e |> select(id_entidad_global, funcion_narrativa), by = "id_entidad_global") |>
    mutate(fase_asignada = case_when(
      funcion_narrativa == "instrumento_del_delito" ~ "crimen",
      funcion_narrativa == "lugar_del_delito"       ~ "crimen",
      funcion_narrativa == "fuente_de_conflicto"    ~ "escalada",
      funcion_narrativa == "objeto_disputado"       ~ "escalada",
      funcion_narrativa == "institucion_fallida"    ~ "detonante",
      TRUE ~ NA_character_
    )) |> filter(!is.na(fase_asignada))

  eventos_raw <- fases_e |>
    arrange(orden) |>
    mutate(
      titulo = str_to_sentence(str_replace_all(coalesce(tipo_evento, "evento"), "_", " ")),
      desc_txt = coalesce(descripcion, tipo_evento, ""),

      # Parsear actores
      act_nombres = map(str_split(coalesce(actores, ""), " ; "),
                        ~ str_trim(.x[.x != "" & .x != "NA"])),
      act_ids = map(act_nombres, ~ na.omit(map_chr(.x, nombre_a_id))),

      # Entidades: asociar las del protagonista a la primera ocurrencia de su fase
      ent_ids_raw = map(fase, function(f) {
        e <- ent_por_fase |> filter(fase_asignada == f) |> pull(id_entidad_global)
        e
      }),

      prot_presente = map_lgl(act_nombres,
                               ~ any(str_detect(.x, regex(prot_primer, ignore_case = TRUE))))
    )

  # Evitar repetir entidades: asignarlas solo al primer evento de cada fase
  fases_ya_usadas <- character(0)
  for (i in seq_len(nrow(eventos_raw))) {
    f <- eventos_raw$fase[i]
    if (f %in% fases_ya_usadas) {
      eventos_raw$ent_ids_raw[[i]] <- character(0)
    } else {
      fases_ya_usadas <- c(fases_ya_usadas, f)
    }
  }

  eventos_bio <- eventos_raw |>
    transmute(
      orden, fase, titulo, descripcion = desc_txt,
      act_ids, ent_ids = ent_ids_raw, prot_presente
    )

} else {

  # ── Modo 2: desde relaciones + entidades del protagonista ────────────────────

  rel_prot <- rel_e |>
    filter(from_personaje_global == prot_id | to_personaje_global == prot_id) |>
    left_join(vp_res, by = "id_relacion") |>
    left_join(va_res, by = "id_relacion") |>
    mutate(alter_id = if_else(from_personaje_global == prot_id,
                              to_personaje_global, from_personaje_global)) |>
    left_join(pers_e |> select(id_personaje_global, alter_nombre = nombre,
                                alter_etario = grupo_etario, alter_rol = rol_criminologico,
                                alter_desc = descripcion),
              by = c("alter_id" = "id_personaje_global")) |>
    mutate(
      fase = case_when(
        alter_rol == "victima"                          ~ "escalada",
        alter_rol == "autoridad"                        ~ "proceso_legal",
        coalesce(asimetria_poder,"par") == "par" &
          alter_etario %in% c("adulto","adulta","joven") ~ "antecedentes",
        TRUE ~ "antecedentes"
      ),
      titulo = paste0("Vínculo con ", alter_nombre),
      descripcion = coalesce(descripcion, alter_desc, ""),
      act_ids = map2(from_personaje_global, to_personaje_global, ~ c(.x, .y)),
      ent_ids = list(character(0)),
      prot_presente = TRUE
    ) |>
    select(fase, titulo, descripcion, act_ids, ent_ids, prot_presente)

  rpe_prot <- rpe_e |>
    filter(id_personaje_global == prot_id) |>
    left_join(ent_e |> select(id_entidad_global, ent_nombre = nombre, ent_tipo = tipo,
                               ent_funcion = funcion_narrativa, ent_desc = descripcion),
              by = "id_entidad_global") |>
    mutate(
      fase = case_when(
        ent_funcion == "instrumento_del_delito" ~ "crimen",
        ent_funcion == "lugar_del_delito"       ~ "crimen",
        ent_funcion == "fuente_de_conflicto"    ~ "escalada",
        ent_funcion == "objeto_disputado"       ~ "escalada",
        ent_funcion == "institucion_fallida"    ~ "detonante",
        ent_funcion == "recurso_estatus"        ~ "antecedentes",
        ent_tipo    == "arma"                   ~ "crimen",
        ent_tipo    == "documento"              ~ "escalada",
        TRUE ~ "antecedentes"
      ),
      titulo = paste0(str_to_sentence(str_replace_all(coalesce(tipo_vinculo,"uso"), "_"," ")),
                      ": ", ent_nombre),
      descripcion = coalesce(descripcion, ent_desc, ""),
      act_ids = list(c(prot_id)),
      ent_ids = map(id_entidad_global, ~ c(.x)),
      prot_presente = TRUE
    ) |>
    select(fase, titulo, descripcion, act_ids, ent_ids, prot_presente)

  eventos_bio <- bind_rows(rel_prot, rpe_prot) |>
    arrange(match(fase, MOMENTOS_ORDEN)) |>
    mutate(orden = row_number())
}

# ─── SERIALIZAR EVENTOS A JSON ────────────────────────────────────────────────

eventos_lista <- eventos_bio |>
  rowwise() |>
  mutate(
    actores_obj   = list(ids_a_actores(act_ids)),
    entidades_obj = list(ids_a_entidades(ent_ids))
  ) |>
  ungroup() |>
  transmute(
    orden, fase,
    titulo, descripcion,
    actores   = actores_obj,
    entidades = entidades_obj,
    prot_presente,
    fase_color = unname(MOMENTO_COLOR[fase]),
    fase_label = unname(MOMENTO_LABEL[fase])
  )

eventos_json <- toJSON(eventos_lista, auto_unbox = TRUE)

# Fases presentes en este episodio
fases_presentes <- MOMENTOS_ORDEN[MOMENTOS_ORDEN %in% eventos_bio$fase]

# Metadatos del episodio
periodo_txt  <- coalesce(epi_e$periodo_temporal[1],  "")
locacion_txt <- coalesce(epi_e$locacion_principal[1],"")
delito_txt   <- str_replace_all(coalesce(epi_e$categoria_delito_principal[1],""), "_"," ")
resumen_txt  <- str_replace_all(coalesce(epi_e$resumen[1],""), "'", "&#39;")

meca_txt <- if (USAR_NARRATIVA && nrow(meca_e) > 0) paste0(
  str_replace_all(coalesce(meca_e$tipo_mecanismo[1],""), "_"," "), " · ",
  str_replace_all(coalesce(meca_e$patron_relacional[1],""), "_"," "), " · ",
  coalesce(meca_e$factor_detonante[1],"")
) else ""

# HTML de botones de fase
botones_html <- paste(c(
  '<button class="btn-f activo" data-f="todos" onclick="filtrar(\'todos\')">Todos</button>',
  map_chr(fases_presentes, ~ sprintf(
    '<button class="btn-f" data-f="%s" onclick="filtrar(\'%s\')" style="--fc:%s">%s</button>',
    .x, .x, MOMENTO_COLOR[.x], MOMENTO_LABEL[.x]
  ))
), collapse = "\n    ")

colores_js <- paste0("{",
  paste(map_chr(MOMENTOS_ORDEN, ~ sprintf('"%s":"%s"',.x,MOMENTO_COLOR[.x])), collapse=","),
"}")

periodo_js <- paste0("{",
  paste(map_chr(MOMENTOS_ORDEN, ~ sprintf('"%s":"%s"',.x,MOMENTO_PERIODO[.x])), collapse=","),
"}")

# ─── GENERAR HTML ─────────────────────────────────────────────────────────────

html <- paste0('<!DOCTYPE html>
<html lang="es">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Ciclo de vida · ', prot_nombre, '</title>
<style>
*{box-sizing:border-box;margin:0;padding:0;}
:root{--bg:#111418;--bg2:#0d1117;--bg3:#141820;--border:#1e2633;
      --text:#ecf0f1;--muted:#7f8c8d;--subtle:#566573;}
body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif;
     background:var(--bg);color:var(--text);min-height:100vh;}

/* HEADER */
#hdr{background:var(--bg2);padding:10px 20px;border-bottom:2px solid #e74c3c;
     display:flex;align-items:center;gap:14px;position:sticky;top:0;z-index:100;}
.badge{background:#e74c3c;color:white;font-size:9px;font-weight:700;
       padding:2px 7px;border-radius:3px;letter-spacing:.7px;text-transform:uppercase;flex-shrink:0;}
#hdr h1{font-size:14px;font-weight:600;line-height:1.3;}
#hdr h1 strong{color:#e74c3c;}
#hdr .meta{margin-left:auto;font-size:10px;color:var(--muted);text-align:right;line-height:1.7;flex-shrink:0;}

/* MECA BAR */
#meca-bar{background:var(--bg3);padding:5px 20px;font-size:10px;color:var(--muted);
          border-bottom:1px solid var(--border);}
#meca-bar b{color:#e67e22;}

/* FILTROS */
#filtros{background:var(--bg2);padding:7px 20px;border-bottom:1px solid var(--border);
         display:flex;gap:7px;flex-wrap:wrap;align-items:center;position:sticky;top:52px;z-index:99;}
.btn-f{background:transparent;color:var(--muted);border:1px solid #2c3e50;
       border-radius:4px;padding:4px 12px;font-size:10px;font-weight:600;
       cursor:pointer;transition:all .15s;letter-spacing:.3px;}
.btn-f:hover{color:var(--text);border-color:#566573;}
.btn-f.activo{background:var(--fc,#2c3e50);border-color:var(--fc,#566573);color:white;}
#filtros .contador{margin-left:auto;font-size:10px;color:var(--subtle);}

/* TIMELINE */
#timeline{max-width:820px;margin:0 auto;padding:30px 20px 60px;
          position:relative;}

/* Spine: línea vertical izquierda */
#spine{position:absolute;left:48px;top:0;bottom:0;width:3px;
       background:linear-gradient(to bottom,
         #2980b9 0%,#2980b9 33%,
         #e67e22 33%,#e67e22 55%,
         #e74c3c 55%,#e74c3c 62%,
         #7b241c 62%,#7b241c 72%,
         #7d3c98 72%,#7d3c98 86%,
         #1a5276 86%,#1a5276 100%);}

/* Banner de fase */
.fase-banner{position:relative;margin:28px 0 16px 72px;display:flex;align-items:center;gap:10px;}
.fase-banner::before{content:"";position:absolute;left:-24px;top:50%;transform:translateY(-50%);
  width:12px;height:12px;border-radius:50%;background:var(--fc);
  box-shadow:0 0 0 3px var(--bg),0 0 0 5px var(--fc);}
.fase-banner .fase-tag{background:var(--fc);color:white;font-size:10px;font-weight:700;
  padding:3px 9px;border-radius:3px;letter-spacing:.5px;text-transform:uppercase;}
.fase-banner .fase-periodo{font-size:11px;color:var(--muted);font-style:italic;}

/* Evento */
.evento{position:relative;margin-left:72px;margin-bottom:20px;
        background:var(--bg3);border:1px solid var(--border);
        border-left:3px solid var(--fc);border-radius:0 6px 6px 0;
        padding:12px 14px;transition:opacity .25s,filter .25s;}
.evento::before{content:"";position:absolute;left:-27px;top:16px;
  width:9px;height:9px;border-radius:50%;background:var(--fc);border:2px solid var(--bg);}
.evento.protagonista-presente .evento-num{color:var(--fc);}
.evento.dimmed{opacity:.22;filter:grayscale(.6);}

.evento-header{display:flex;align-items:flex-start;gap:10px;margin-bottom:7px;}
.evento-num{font-size:9px;font-weight:700;color:var(--subtle);min-width:22px;
            margin-top:2px;text-align:right;}
.evento-titulo{font-size:13px;font-weight:600;color:var(--text);line-height:1.35;flex:1;}
.prot-dot{width:8px;height:8px;border-radius:50%;background:#e74c3c;
           flex-shrink:0;margin-top:5px;title:"Protagonista presente";}

.evento-desc{font-size:11px;color:#95a5a6;line-height:1.55;margin-left:32px;margin-bottom:9px;}

/* Badges de actores y entidades */
.badges{display:flex;flex-wrap:wrap;gap:5px;margin-left:32px;}
.badge-actor{display:inline-flex;align-items:center;gap:4px;padding:2px 8px;
             border-radius:10px;font-size:10px;font-weight:600;
             background:color-mix(in srgb,var(--bc) 18%,transparent);
             border:1px solid color-mix(in srgb,var(--bc) 40%,transparent);
             color:var(--bc);}
.badge-actor .dot{width:7px;height:7px;border-radius:50%;background:var(--bc);flex-shrink:0;}
.badge-actor.is-prot{font-weight:800;
  background:color-mix(in srgb,var(--bc) 28%,transparent);
  border-width:2px;}
.badge-ent{display:inline-flex;align-items:center;gap:4px;padding:2px 8px;
           border-radius:3px;font-size:10px;font-weight:600;
           background:color-mix(in srgb,var(--bc) 15%,transparent);
           border:1px solid color-mix(in srgb,var(--bc) 35%,transparent);
           color:var(--bc);}
.badge-ent .sq{width:7px;height:7px;border-radius:1px;background:var(--bc);flex-shrink:0;}

/* Resumen flotante */
#resumen-wrap{background:var(--bg3);border-top:1px solid var(--border);
              padding:14px 20px;font-size:11px;color:var(--muted);line-height:1.6;}
#resumen-wrap strong{color:var(--text);}
</style>
</head>
<body>

<div id="hdr">
  <div class="badge">Mea Culpa</div>
  <h1>Ciclo de vida &nbsp;·&nbsp; <strong>', prot_nombre, '</strong></h1>
  <div class="meta">', delito_txt, '<br>', periodo_txt, ' · ', locacion_txt, '</div>
</div>
', if (meca_txt != "") paste0('<div id="meca-bar"><b>Mecanismo:</b> ', meca_txt, '</div>'), '

<div id="filtros">
  ', botones_html, '
  <span class="contador" id="contador"></span>
</div>

<div id="timeline">
  <div id="spine"></div>
  <div id="eventos-container"></div>
</div>

<div id="resumen-wrap">
  <strong>Resumen del caso:</strong> ', resumen_txt, '
</div>

<script>
var eventos = ', eventos_json, ';
var FASES_COLOR  = ', colores_js, ';
var FASES_PERIODO = ', periodo_js, ';
var FASES_ORDEN = ["antecedentes","escalada","detonante","crimen","consecuencias","proceso_legal"];
var FASES_LABEL = {
  antecedentes:"Antecedentes", escalada:"Escalada", detonante:"Detonante",
  crimen:"Crimen", consecuencias:"Consecuencias", proceso_legal:"Proceso legal"
};

var filtroActual = "todos";

function renderTimeline() {
  var container = document.getElementById("eventos-container");
  container.innerHTML = "";

  // Agrupar eventos por fase en orden
  var fases_vistas = [];
  var grupos = {};
  eventos.forEach(function(ev) {
    if (!grupos[ev.fase]) { grupos[ev.fase] = []; fases_vistas.push(ev.fase); }
    grupos[ev.fase].push(ev);
  });
  // Mantener orden canónico
  var fases_ordenadas = FASES_ORDEN.filter(function(f) { return grupos[f]; });

  fases_ordenadas.forEach(function(fase) {
    var color = FASES_COLOR[fase] || "#aaa";
    var periodo = FASES_PERIODO[fase] || "";

    // Banner de fase
    var banner = document.createElement("div");
    banner.className = "fase-banner";
    banner.setAttribute("data-fase", fase);
    banner.style.setProperty("--fc", color);
    banner.innerHTML =
      \'<span class="fase-tag">\' + (FASES_LABEL[fase]||fase) + \'</span>\' +
      \'<span class="fase-periodo">\' + periodo + \'</span>\';
    container.appendChild(banner);

    // Eventos de esta fase
    grupos[fase].forEach(function(ev) {
      var card = document.createElement("div");
      card.className = "evento" + (ev.prot_presente ? " protagonista-presente" : "");
      card.setAttribute("data-fase", fase);
      card.style.setProperty("--fc", color);

      // Header del evento
      var headerHtml =
        \'<div class="evento-header">\' +
        \'<span class="evento-num">#\' + ev.orden + \'</span>\' +
        \'<span class="evento-titulo">\' + (ev.titulo||"") + \'</span>\' +
        (ev.prot_presente ? \'<div class="prot-dot" title="Protagonista presente"></div>\' : "") +
        \'</div>\';

      // Descripción
      var descHtml = ev.descripcion
        ? \'<div class="evento-desc">\' + ev.descripcion + \'</div>\'
        : "";

      // Badges actores
      var badgesHtml = "";
      var hasActores  = ev.actores  && ev.actores.length  > 0;
      var hasEntidades = ev.entidades && ev.entidades.length > 0;
      if (hasActores || hasEntidades) {
        badgesHtml = \'<div class="badges">\';
        if (hasActores) {
          ev.actores.forEach(function(a) {
            badgesHtml +=
              \'<span class="badge-actor\' + (a.es_prot ? " is-prot" : "") +
              \'" style="--bc:\' + a.color + \'">\' +
              \'<span class="dot"></span>\' + a.label + \'</span>\';
          });
        }
        if (hasEntidades) {
          ev.entidades.forEach(function(e) {
            badgesHtml +=
              \'<span class="badge-ent" style="--bc:\' + e.color + \'">\' +
              \'<span class="sq"></span>\' + e.label + \'</span>\';
          });
        }
        badgesHtml += \'</div>\';
      }

      card.innerHTML = headerHtml + descHtml + badgesHtml;
      container.appendChild(card);
    });
  });

  actualizarContador();
}

function filtrar(f) {
  filtroActual = f;

  // Botones
  document.querySelectorAll(".btn-f").forEach(function(b) {
    var isActive = (b.dataset.f === f);
    b.classList.toggle("activo", isActive);
    if (isActive && f !== "todos") b.style.setProperty("--fc", FASES_COLOR[f] || "#555");
  });

  // Eventos y banners
  document.querySelectorAll(".evento, .fase-banner").forEach(function(el) {
    var elFase = el.getAttribute("data-fase");
    el.classList.toggle("dimmed", f !== "todos" && elFase !== f);
  });

  actualizarContador();
}

function actualizarContador() {
  var total = eventos.length;
  var visibles = filtroActual === "todos"
    ? total
    : eventos.filter(function(e) { return e.fase === filtroActual; }).length;
  document.getElementById("contador").textContent =
    visibles + " evento" + (visibles !== 1 ? "s" : "") +
    (filtroActual !== "todos" ? " en esta fase" : " en total");
}

// Actualizar gradiente del spine según fases presentes
(function() {
  var fases = FASES_ORDEN.filter(function(f) {
    return eventos.some(function(e) { return e.fase === f; });
  });
  if (fases.length === 0) return;
  var step = 100 / fases.length;
  var stops = fases.map(function(f, i) {
    var c = FASES_COLOR[f];
    return c+" "+(i*step)+"%, "+c+" "+((i+1)*step)+"%";
  });
  document.getElementById("spine").style.background =
    "linear-gradient(to bottom," + stops.join(",") + ")";
})();

renderTimeline();
</script>
</body>
</html>')

# ─── GUARDAR ──────────────────────────────────────────────────────────────────

nombre_archivo <- str_to_lower(str_replace_all(prot_primer, "[^a-zA-Z]", ""))
salida <- paste0("ciclo_vida_", nombre_archivo, "_", EPI_ID, ".html")
writeLines(html, salida, useBytes = TRUE)

message("\n✓ Guardado: ", salida)
message("  Eventos construidos: ", nrow(eventos_bio))
message("\n  Distribución por fase:")
dist <- table(factor(eventos_bio$fase, levels = MOMENTOS_ORDEN))
for (f in MOMENTOS_ORDEN) {
  n <- as.integer(dist[f])
  if (n > 0) message(sprintf("  %-20s  %2d  %s", MOMENTO_LABEL[f], n, strrep("█", n)))
}
