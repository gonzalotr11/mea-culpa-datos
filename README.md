# Mea Culpa, como datos

Base de datos relacional construida a partir de los subtítulos de 121 episodios (14 temporadas) del programa de televisión chileno *Mea Culpa*, y un sitio web para explorarla.

> **Nombres ficticios.** Los nombres y apodos de todos los personajes fueron reemplazados por nombres ficticios en todas las tablas y textos (ver `scripts/anonimizar.py`). Los lugares, fechas y demás variables se conservan. Como los casos de *Mea Culpa* son públicos, esto es una seudonimización: un caso puede reconocerse por su contexto aunque no aparezcan los nombres reales.

Cada episodio es un relato largo sobre un delito. La base lo descompone en unidades que se pueden contar y cruzar: quién aparece, cómo se relaciona con quién, qué lugares, objetos e instituciones importan, en qué orden ocurren los hechos y qué condiciones hicieron posible el crimen. El proyecto sirve como ejemplo de un ejercicio de **texto a datos relacionales** para estudiantes de psicología y ciencias sociales.

**Sitio:** https://gonzalotr11.github.io/mea-culpa-datos/

---

## Qué hay en el repositorio

```
.
├── index.html                  Sitio explorador (abre directo en el navegador)
├── assets/
│   └── data.js                 Datos del sitio, generados desde data/csv/
├── data/
│   ├── csv/                    Las 15 tablas de la base (00 a 14)
│   └── fuentes_temporadas.csv  Listas de reproducción de YouTube por temporada
├── scripts/
│   ├── build_site_data.py      Regenera assets/data.js a partir de los CSV
│   └── anonimizar.py           Reemplaza los nombres reales por ficticios (se corre sobre la base original, que no se publica)
└── R/
    ├── pipeline/               Scripts que construyeron la base, en orden
    └── analisis/               Exploraciones y visualizaciones en R
```

## El explorador

`index.html` ofrece tres formas de entrar al archivo:

- **Portada:** los 121 episodios como una constelación ordenada por temporada. El color indica el mecanismo del crimen y el tamaño, cuántas personas aparecen en la historia. Cada punto abre su ficha.
- **Recorridos:** cinco hilos temáticos (violencia que escala en casa, abandono institucional e impunidad, dependencia económica, infancias en riesgo, pobreza). Cada uno muestra cuántos episodios reúne, qué mecanismo predomina, el nivel socioeconómico de las víctimas, qué condiciones se repiten más que en el resto del archivo y tres historias para empezar.
- **Pregúntale al archivo:** una frase con huecos (tipo de relación, condición de fondo, factor habilitador, mecanismo). Al completarla se ilumina la constelación, se actualiza el conteo y se muestra qué otras condiciones acompañan a esos casos.
- **Archivo completo:** buscador y filtros por temporada, delito, mecanismo, patrón relacional, condiciones, habilitadores y temas.

La **ficha de cada episodio** pone primero la red de relaciones: personas y entidades, con el rol de cada persona, su nivel socioeconómico, el tono afectivo de cada vínculo y la asimetría de poder. Se puede ver en anillos o en disposición libre, acercar, desplazar y abrir en pantalla completa; al tocar una persona o una línea se ve el detalle. Siguen el hilo narrativo en seis fases, los factores del caso y las filas de cada tabla que alimentan la ficha.

El sitio es un solo archivo HTML sin servidor ni dependencias de instalación. Para verlo localmente basta con hacer doble clic en `index.html`. Contiene casos de homicidio, violencia sexual y abuso infantil; la portada lo advierte.

## Modelo de datos

Todas las tablas se unen por `id_episodio` (por ejemplo, `temporada_01_cap01`). Personajes, entidades y relaciones tienen además identificadores locales (`P01`, `E01`, `R001`) y globales (`temporada_01_cap01_P01`).

| Tabla | Filas | Unidad de análisis | Claves |
|---|---:|---|---|
| `00_master_episodios` | 121 | Vista ancha: un episodio con 39 variables resumen | `id_episodio` |
| `01_episodios` | 121 | Episodio | `id_episodio` |
| `02_episodios_delitos` | 221 | Episodio × categoría de delito | `id_episodio` |
| `03_episodios_temas` | 885 | Episodio × tema (texto libre) | `id_episodio` |
| `04_personajes` | 1.001 | Persona del relato | `id_personaje_global` |
| `05_relaciones` | 1.089 | Par dirigido de personas | `id_relacion`, `from_/to_personaje` |
| `06_vinculos_practicos` | 1.640 | Relación × vínculo práctico | `id_relacion` |
| `07_vinculos_afectivos` | 1.729 | Relación × vínculo afectivo | `id_relacion` |
| `08_entidades` | 1.013 | Lugar, organización, arma, sustancia u objeto | `id_entidad_global` |
| `09_relaciones_pers_ent` | 1.292 | Persona × entidad | `id_personaje`, `id_entidad` |
| `10_fases_narrativas` | 1.221 | Evento ordenado dentro del episodio | `id_episodio`, `orden` |
| `11_mecanismos_causales` | 120 | Mecanismo y factor detonante del episodio | `id_episodio` |
| `12_condiciones_estructurales` | 547 | Episodio × condición de contexto | `id_episodio` |
| `13_habilitadores` | 451 | Episodio × factor habilitador | `id_episodio` |
| `14_vulnerabilidades` | 579 | Víctima × tipo de vulnerabilidad | `id_episodio`, `victima` |

Variables destacadas de `04_personajes`: `rol_narrativo`, `rol_criminologico`, `sexo`, `grupo_etario`, `ocupacion`, `ses_nivel` (nivel socioeconómico de 1 a 5) y `ses_evidencia` (el texto que justifica esa clasificación).

## Cómo se construyó

```
Subtítulos de YouTube → texto limpio por episodio → extracción estructurada → 15 tablas
```

Scripts en `R/pipeline/`, en orden de ejecución:

1. `descargar_subtitulos_mea_culpa.R`: descarga los subtítulos en español de cada episodio con `yt-dlp`, a partir de las listas de reproducción de `data/fuentes_temporadas.csv`.
2. `procesar_subtitulos.R`: limpia los archivos `.vtt` y genera un texto por capítulo.
3. `extraccion_personajes.R`: primera pasada de extracción con la API de Claude. Produce las tablas 01 a 09 (episodios, personajes, relaciones y entidades).
4. `extraccion_narrativa.R`: segunda pasada de extracción. Produce las tablas 10 a 14 (fases narrativas, mecanismos y factores).
5. `union_bases.R`: une las 14 tablas, crea `00_master_episodios.csv` y la planilla con las 15 hojas.

Los scripts de extracción leen la clave de la API desde la variable de entorno `ANTHROPIC_API_KEY`, que se configura en `~/.Renviron`. No escribas la clave dentro de los scripts.

Paquetes de R usados: `dplyr`, `readr`, `purrr`, `stringr`, `stringi`, `tidyr`, `httr2`, `jsonlite`, `openxlsx`. Para los análisis: `igraph`, `visNetwork`, `ggraph`, `ggplot2`, `patchwork`, `readxl`, `writexl`, `janitor`, `forcats`, `scales`.

Los scripts usan rutas relativas a la carpeta donde se ejecutan. Para correr los análisis sobre este repositorio, fija el directorio de trabajo en `data/csv/` o ajusta las rutas.

## Actualizar el sitio después de cambiar los datos

Si modificas algún CSV en `data/csv/`, regenera los datos del sitio desde la raíz del repositorio:

```bash
python scripts/build_site_data.py
```

El script solo requiere Python 3, sin paquetes adicionales. Reescribe `assets/data.js` y avisa si hay filas cuyo `id_episodio` no existe en `01_episodios`.

## Publicar el sitio en GitHub Pages

1. Sube el contenido de esta carpeta a un repositorio nuevo en GitHub.
2. En el repositorio, entra a **Settings → Pages**.
3. En *Build and deployment*, elige **Deploy from a branch**, la rama `main` y la carpeta `/ (root)`. Guarda.
4. En uno o dos minutos el sitio queda disponible en `https://<tu-usuario>.github.io/<nombre-del-repo>/`.

Cada episodio tiene su propio enlace directo, por ejemplo `.../#temporada_01_cap03`.

## Limitaciones conocidas

- T11·C04 y T12·C09 tienen fases narrativas (tablas 10 a 14), pero faltan en `01_episodios` y en las tablas de personajes y relaciones.
- T14·C05, C06 y C07 no tienen fases narrativas ni mecanismo causal.
- `03_episodios_temas` es texto libre: 438 etiquetas distintas, que bajan a 393 al unificar tildes y guiones bajos. El explorador muestra los temas unificados.
- 120 personajes (12%) no tienen nivel socioeconómico asignado.
- Los personajes que son figuras públicas también tienen nombres ficticios, distintos en cada episodio.
- Dos episodios no tienen ningún personaje marcado como protagonista y 35 tienen más de uno. En la red, el centro es el protagonista con más conexiones.
- La codificación fue hecha por un modelo de lenguaje a partir de subtítulos automáticos. Es útil para explorar y enseñar, pero no reemplaza una codificación validada por personas.
- `extraccion_personajes.R` nombra la tabla 09 como `09_relaciones_persona_entidad.csv`; en la base final el archivo se llama `09_relaciones_pers_ent.csv`.

## Fuente

Los episodios provienen de las listas de reproducción públicas de *Mea Culpa* en YouTube (ver `data/fuentes_temporadas.csv`). Este repositorio no incluye los subtítulos ni las transcripciones completas, solo los datos derivados de ellos.

## Autor

Gonzalo Torres
