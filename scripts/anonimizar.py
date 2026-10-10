"""Crea una copia anonimizada de la base Mea Culpa reemplazando los nombres de personajes
en todas las tablas y columnas de texto. No modifica los archivos originales."""
import csv, re, os, sys, random, unicodedata, collections, json

SRC, OUT = sys.argv[1], sys.argv[2]
FILES = sorted(f for f in os.listdir(SRC) if re.match(r'^(0\d|1[0-4])_.*\.csv$', f))
T = {}
for f in FILES:
    with open(os.path.join(SRC, f), encoding='utf-8-sig', newline='') as fh:
        rd = csv.DictReader(fh); T[f] = (rd.fieldnames, list(rd))

WORD = r"A-Za-zÁÉÍÓÚÜÑáéíóúüñ"
ROLE = set("""Madre Padre Hijo Hija Hijos Hijas Don Doña Juez Jueza Magistrado El La Los Las Lo Señora Señor Sra Sr Doctor Doctora Dr Dra
Detective Vecino Vecina Vecinos Gerente Dueña Dueño Nueva Nuevo Abuela Abuelo Abuelos Hermano Hermana Hermanos Hermanas Tío Tía Tíos
Comentarista Entrevistador Entrevistadora Analista Apoderada Apoderado Travestis Carabinero Carabineros Teniente Sargento Cabo Capitán
Coronel General Fiscal Abogado Abogada Psicólogo Psicóloga Psiquiatra Policía Policías Inspector Comisario Sacerdote Cura Monja Profesor
Profesora Director Directora Jefe Jefa Amigo Amiga Amigos Amigas Esposa Esposo Novia Novio Pareja Mujer Mujeres Hombre Hombres Niño Niña Niños
Bebé Familia Familiares Familiar Testigo Testigos Víctima Víctimas Otro Otra Otros Primo Prima Sobrino Sobrina Suegra Suegro Cuñado Cuñada
Compañero Compañera Compañeros Funcionario Funcionaria Funcionarios Guardia Gendarme Gendarmes Médico Médica Enfermera Enfermero Conductor
Conductora Narrador Narradora Voz Autor Autora Cómplice Cómplices Joven Jóvenes Anciana Anciano Menor Mayor Primer Primera Segundo Segunda
Tercer Medio Medios Hermanastro Hermanastra Padrastro Madrastra Patrón Patrona Empleada Empleado Empleador Jornalero Obrero Comerciante
Chofer Taxista Dueños Agente Agentes Oficial Oficiales Soldado Soldados Militar Militares Funcionarios Juzgado Tribunal Ministro Ministra
Presidente Presidenta Alcalde Alcaldesa Diputado Senador Periodista Reportero Cliente Clienta Clientes Pastor Párroco Obispo Hermanito
Hermanita Bebés Grupo Banda Pandilla Integrantes Miembros Mamá Papá Tata Abuelita Abuelito Señorita Hija/o Investigaciones PDI OS Unidad
Equipo Personal Defensor Defensora Querellante Perito Forense Sexólogo Criminólogo Delator Matrona Partera Vidente Brujo Bruja Curandero
Curandera Santera Gitano Gitana Gitanos Mapuche Desconocido Desconocida Desconocidos Sujeto Sujetos Individuo Individuos Persona Personas
Varios Varias Alias Apodo Sin Nombre Ex Exesposa Exesposo Exnovio Exnovia Novios Esposos Matrimonio Recluso Reclusos Interno Internos Reo Reos
Preso Presos Sicario Sicarios Asesino Asesina Asesinos Ladrón Ladrones Delincuente Delincuentes Traficante Traficantes Dealer Proxeneta
Prostituta Prostitutas Hermanas/os Y De Del Al A En Con Por Para Su Sus Un Una Administradora Administrador Arrendadora Arrendador Brigada Colega Colegas Contacto Conviviente Especialista Experto Experta Fotógrafo Hijastra Hijastro Investigador Investigadora Nana Padres Secretaria Subcomisario Violadores Plaza Liceo Obispo""".split())
PARTICLE = {'de', 'del', 'la', 'las', 'los', 'y', 'e', 'da', 'van', 'von', 'di'}
DESC_WORDS = re.compile(r"\b(madre|padre|hij[oa]s?|herman[oa]s?|abuel[oa]s?|t[ií][oa]|espos[oa]|amig[oa]|novi[oa]|pareja|vecin[oa]|"
                        r"conductor[a]?|funcionari[oa]|testigo|delator|abusador|sex[oó]logo|voz|oficial|primer|segund[oa]|familiar|"
                        r"anciana?|soldado|biol[oó]gico|materno|paterno|sin nombre|señora|don|doña|de|en)\b", re.I)
HEADS = re.compile(r"(?:\b(?:Papa|San|Santa|Santo|Virgen|Calle|calle|Avenida|avenida|Av\.|Villa|villa|Población|población|Hospital|Clínica|"
                   r"Colegio|Liceo|Escuela|Instituto|Universidad|Fundación|Hogar|Plaza|Parque|Cerro|Puente|Puerto|Pasaje|pasaje|Estación|"
                   r"Isla|Lago|Río|río|Fundo|fundo|Hacienda|Cárcel|Penitenciaría|Cementerio|Iglesia|Parroquia|Capilla|Barrio|barrio|Sector|"
                   r"sector|Campamento|campamento|Teatro|Cine|Restaurante|Hotel|Motel|Industrias|Banco|Mina)\s+)$")
NEVER = {'Dios', 'Cristo', 'Jesucristo', 'Virgen', 'Chile', 'Concepción', 'Estado', 'Iglesia', 'Corte', 'Satanás',
         'Lucifer', 'Navidad', 'Carabineros', 'Gendarmería', 'Investigaciones', 'Mar', 'Pacífico', 'Andes', 'Biobío', 'Maule'}
DROP = {('temporada_01_cap10', 'Chillán'), ('temporada_04_cap09', 'Angol'), ('temporada_04_cap09', 'Huequén')}
ALIAS_BAD = {'la abuela', 'cooperativa', 'apodo', 'las gemelas', 'rescatista', 'asistente social', 'administrador del club croata', 'la niña', '13 años', 'bebé', 'suegro'}
BARE_BAD = {'abuela', 'loco', 'logo', 'gemelas', 'croata', 'niña', 'carpeta', 'pato', 'truco', 'gato', 'gallina', 'chino', 'pabellón', 'feo'}   # lugares mal leídos como nombres
KEEP_IF_FOLLOWED = {('temporada_01_cap10', 'Castro'): r"\s*\(Chilo", ('temporada_12_cap05', 'Santiago'): r",\s*Chile"}
FULLCOLS = {('04_personajes.csv','nombre'), ('14_vulnerabilidades.csv','victima')}

GIVEN_M = """Alonso Ignacio Matías Tomás Benjamín Vicente Joaquín Cristóbal Sebastián Nicolás Felipe Diego Rodrigo Andrés Gonzalo Claudio
Mauricio Patricio Marcelo Rubén Hugo Héctor Ricardo Raúl Sergio Óscar Eduardo Alberto Julio Mario Arturo Gustavo Enrique Fabián Hernán
Iván Germán Leonel Renato Gastón Rolando Osvaldo Nelson Wladimir Elías Moisés Samuel Daniel Gabriel Rafael Emilio Esteban Adrián Ulises
Bruno Lucas Martín Agustín Damián Emiliano Fernando Ramiro Rogelio Teodoro Valentín Abel Aníbal Baltasar Ciro Darío Efraín Fidel Gerardo
Horacio Isaías Jacobo Lisandro Maximiliano Néstor Octavio Plácido Reinaldo Saúl Tobías Ubaldo Vladimir Wenceslao Zacarías Amador Benito
Camilo Danilo Eliseo Froilán Gilberto Humberto Ismael Leandro Marcial Nolberto Orlando Pascual Remigio Silvio Tristán Uriel Víctor""".split()
GIVEN_F = """Camila Valentina Fernanda Javiera Constanza Catalina Daniela Francisca Antonia Isidora Florencia Josefa Macarena Carolina Paulina
Marcela Verónica Claudia Lorena Patricia Ximena Soledad Viviana Pamela Alejandra Andrea Gabriela Natalia Mónica Cecilia Gloria Silvia Teresa
Raquel Irene Inés Olga Nora Elena Alicia Beatriz Berta Celia Delia Edith Elvira Estela Eugenia Fabiola Fresia Gilda Hilda Ivonne Jimena
Leonor Lidia Lucía Maite Miriam Nélida Norma Ofelia Pilar Regina Rosario Sandra Sara Susana Tamara Úrsula Vilma Yolanda Zulema Amanda
Bárbara Belén Carla Denisse Emilia Ester Fabiana Gisela Graciela Jacqueline Karen Laura Loreto Magaly Margarita Nancy Noemí Orfelina
Priscila Romina Ruth Selena Tatiana Uberlinda Vanesa Wilma Yasna Zoila Adela Bernarda Clemencia Dora Eliana Flor Herminia Iris Juana""".split()
SURN = """Aguilera Alarcón Alvarado Araya Arancibia Arriagada Astudillo Bravo Bustos Cáceres Calderón Campusano Cárdenas Carrasco Castillo
Cerda Cisternas Contreras Cortés Cuevas Díaz Donoso Escobar Espinoza Farías Figueroa Fuentes Gallardo Garrido Gómez González Guerrero
Gutiérrez Henríquez Herrera Hidalgo Ibáñez Jara Jiménez Lagos Leiva López Maldonado Marín Martínez Medina Mella Méndez Miranda Molina Morales
Moreno Muñoz Navarro Núñez Olivares Ortiz Osorio Pacheco Palma Paredes Pérez Pino Poblete Quezada Quiroz Ramírez Reyes Riquelme Rivas
Rodríguez Rojas Saavedra Salazar Salinas Sánchez Sandoval Sepúlveda Silva Soto Tapia Toledo Torres Troncoso Ulloa Valdés Valenzuela Vargas
Vásquez Vega Venegas Vera Vergara Villalobos Yáñez Zamora Zúñiga Abarca Acevedo Acuña Alfaro Allende Altamirano Andrade Ávila Barra Barrera
Becerra Belmar Bórquez Briones Cabezas Cabrera Canales Carvajal Castro Catalán Cid Concha Córdova Correa Daza Delgado Duarte Echeverría
Fernández Flores Fuenzalida Gajardo Godoy Guzmán Inostroza Lara Lillo Lobos Luna Mansilla Meza Montecinos Montoya Mora Moya Naranjo Neira
Ojeda Orellana Ortega Oyarzún Peña Pizarro Ponce Retamal Rivera Robles Rubio Ruiz Salas Santana Solís Suazo Toro Urrutia Vidal Villegas""".split()
ALIAS = """el Zorzal|el Tordo|el Quique|el Lalo|el Cucho|el Pelao|el Chico Lucho|el Tuerto|el Manco|el Rucio Mario|el Guatón Pepe|el Flaco Tito|
la Coni|la Pili|la Negra Rosa|la Chica Male|el Cholo|el Tata|el Nene|el Pollo|el Coto|el Pancho|el Pato Loco|el Mono|el Cachorro|el Charqui|
el Huaso|el Pibe|el Turco|el Chuncho|el Lucho|el Coné|la Pelusa|la Tati|el Chato|el Pirata|el Pájaro|el Gallo Fino|el Canario|el Lobo""".replace('\n','').split('|')

def norm(s):
    s = unicodedata.normalize('NFD', s); return ''.join(c for c in s if unicodedata.category(c) != 'Mn').lower()

pers = T['04_personajes.csv'][1]
eps = sorted({r['id_episodio'] for f in T for r in T[f][1] if 'id_episodio' in r})
first_tokens = collections.Counter()
for p in pers:
    m = re.findall(rf"[A-ZÁÉÍÓÚÑ][{WORD}]+", re.sub(r'\(.*?\)|["\'“”‘’].*?["\'“”‘’]', ' ', p['nombre']))
    m = [t for t in m if t not in ROLE]
    if m: first_tokens[m[0]] += 1
GIVEN_SET = set(first_tokens) | set(GIVEN_M) | set(GIVEN_F) | {'José', 'María', 'Juan', 'Luis', 'Carlos', 'Ana', 'Rosa', 'Luz'}
REAL_ALL = set()

def parse(name):
    """Devuelve (corridas de tokens de nombre, frases alias)."""
    aliases, runs = [], []
    s = name
    for q in re.findall(r'["“”]([^"“”]+)["“”]|[\'‘’]([^\'‘’]+)[\'‘’]', s):
        a = (q[0] or q[1]).strip()
        for part in re.split(r'\s*/\s*', a): aliases.append(part)
    s = re.sub(r'["“”][^"“”]+["“”]|[\'‘’][^\'‘’]+[\'‘’]', ' ', s)
    for par in re.findall(r'\(([^)]*)\)', s):
        par = re.sub(r'^\s*alias\s+', '', par, flags=re.I).strip()
        if not par: continue
        caps = re.findall(rf"[A-ZÁÉÍÓÚÑ][{WORD}]+", par)
        if not caps and not DESC_WORDS.search(par) and re.match(r'^(el|la|los|las)\s', par, re.I):
            aliases.append(par)                       # alias en minúscula, p. ej. "el yoyo"
        else:
            runs += capital_runs(par)
    s = re.sub(r'\(.*?\)', ' ', s)
    s = re.sub(r',?\s*alias\s+.*$', ' ', s, flags=re.I)
    s = re.sub(r'\s+—\s+.*$', ' ', s)
    m = re.match(rf"^\s*(El|La|Los|Las)\s+([{WORD}\s]+?)\s*$", s)
    if m and not any(t in GIVEN_SET for t in m.group(2).split()):
        aliases.append(s.strip())
    else:
        runs += capital_runs(s)
    return runs, [a for a in aliases if len(a) > 1]

INST = set("""Liceo Colegio Escuela Politécnico Hospital Universidad Banco Instituto Fundación Hogar Servicio Juzgado Tribunal Corte Iglesia
Municipalidad Consultorio Policlínico Centro Clínica Cárcel Penitenciaría Ministerio Programa Sename SENAME Ejército Armada Fuerza Partido
Movimiento Frente Juventud Empresa Compañía Club Sindicato Comisaría Tenencia Brigada Departamento Oficina Registro Gobierno Estado Poder
Congreso Senado Cámara Junta Comité Asociación Agrupación Vicaría Cruz Teletón Radio Canal Diario Revista Mercado Feria Terminal Aeropuerto
Hostal Residencial Pensión Población Villa Campamento Fundo Hacienda Parroquia Capilla Templo Seminario Convento""".split())
_LOWER = collections.Counter()
def _build_lower():
    for f, tbl in T.items():
        for r in tbl[1]:
            for v in r.values():
                for w in re.findall(r"(?<![A-Za-zÁÉÍÓÚÜÑáéíóúüñ])[a-záéíóúüñ]{4,}", v or ''): _LOWER[w] += 1
_build_lower()
def capital_runs(s):
    toks = re.findall(rf"[{WORD}]+|[^\s{WORD}]+", s)
    runs, cur = [], []
    for t in toks:
        if re.match(rf"^[A-ZÁÉÍÓÚÑ][{WORD}]+$", t) and t not in ROLE and not (len(t) > 1 and t.isupper()):
            cur.append(t)
        elif t.lower() in PARTICLE and cur:
            cur.append(t.lower() if t[0].islower() else t)
        else:
            if cur: runs.append(cur); cur = []
    if cur: runs.append(cur)
    clean = []
    for r in runs:
        while r and r[-1].lower() in PARTICLE: r = r[:-1]
        r = [t for t in r if t.lower() not in PARTICLE]
        if r and r[0] in INST: continue
        while r and r[0] not in GIVEN_SET and _LOWER[r[0].lower()] >= 3: r = r[1:]
        if r: clean.append(r)
    return clean

def classify(run):
    out = []
    for i, t in enumerate(run):
        if i == 0 or (i == 1 and t in GIVEN_SET and len(run) >= 3) or (i == 1 and len(run) == 2 and t in GIVEN_SET and run[0] in ('José','María','Juan','Luis','Ana')):
            out.append((t, 'g'))
        else:
            out.append((t, 's'))
    if len(run) == 1 and run[0] not in GIVEN_SET and run[0] in first_tokens and first_tokens[run[0]] == 0:
        out = [(run[0], 's')]
    return out

GSEX = collections.defaultdict(collections.Counter)
def gender_of(tok, sexo):
    if sexo in ('M', 'F'): return sexo
    if GSEX.get(tok): return GSEX[tok].most_common(1)[0][0]
    return 'F' if tok.endswith('a') else 'M'

for p in pers:
    _r = parse(p['nombre'])[0]
    if _r and p['sexo'] in ('M', 'F'): GSEX[_r[0][0]][p['sexo']] += 1
for p in pers:
    runs, al = parse(p['nombre'])
    for r in runs: REAL_ALL.update(r)
    for a in al: REAL_ALL.update(re.findall(rf"[{WORD}]+", a))
REAL_N = {norm(x) for x in REAL_ALL}
GM = [g for g in GIVEN_M if norm(g) not in REAL_N]; GF = [g for g in GIVEN_F if norm(g) not in REAL_N]
SU = [s for s in SURN if norm(s) not in REAL_N]
AL = [a for a in ALIAS if not any(norm(w) in REAL_N for w in a.split() if w[0].isupper())]

ep_loc = collections.defaultdict(set)
class _NS(set):
    def __contains__(self, t): return set.__contains__(self, t) or norm(t) in {norm(x) for x in self}
for r in T['01_episodios.csv'][1]:
    for t in re.findall(rf"[A-ZÁÉÍÓÚÑ][{WORD}]+", r['locacion_principal'] + ' ' + r['locaciones_otras']): ep_loc[r['id_episodio']].add(t)
ep_loc = collections.defaultdict(_NS, {k: _NS(v) for k, v in ep_loc.items()})

EXTRA = collections.defaultdict(list)   # corridas adicionales detectadas en el texto
def ep_texts(ep):
    for f, (cols, rows) in T.items():
        for r in rows:
            if r.get('id_episodio') == ep:
                for c in cols:
                    if not re.match(r'^(id_|temporada$|capitulo$|video_id$|locacion|locaciones)', c): yield f, c, r[c]
for ep in eps:
    known = set()
    for p in pers:
        if p['id_episodio'] == ep:
            for r in parse(p['nombre'])[0]: known.update(r)
    known -= {t for e2, t in DROP if e2 == ep}
    for f, c, v in ep_texts(ep):
        if c == 'actores':
            for part in v.split(';'):
                for r in capital_runs(part):
                    r = [t for t in r if t not in NEVER and t not in ep_loc[ep]]
                    if r and any(t not in known for t in r): EXTRA[ep].append(r)
        if known:
            _A = {'a':'[aá]','e':'[eé]','i':'[ií]','o':'[oó]','u':'[uúü]','á':'[aá]','é':'[eé]','í':'[ií]','ó':'[oó]','ú':'[uúü]'}
            alt = '|'.join(''.join(_A.get(ch, re.escape(ch)) for ch in t) for t in sorted(known, key=len, reverse=True))
            for m in re.finditer(r"(?<![%s])(%s)((?:\s+[A-ZÁÉÍÓÚÑ][%s]+)+)" % (WORD, alt, WORD), v):
                tail = [t for t in m.group(2).split() if t not in ROLE and t not in NEVER and t not in ep_loc[ep]]
                cut = []
                for t in tail:
                    if t in ROLE or t in ep_loc[ep]: break
                    cut.append(t)
                new = [t for t in cut if t not in known]
                if new: EXTRA[ep].append([m.group(1)] + cut)
ALIAS_EXTRA = collections.defaultdict(list)
_AP = re.compile(r"(?:conocid[oa] como|apodad[oa]|alias|apodo)\s+['\"‘“]([^'\"’”]{2,40})['\"’”]", re.I)
_ALLTXT = ' '.join((v or '').lower() for f, tbl in T.items() for r in tbl[1] for v in r.values())
def _common(phrase):
    n_p = len(re.findall(r"(?<![%s])%s(?![%s])" % (WORD, re.escape(phrase.lower()), WORD), _ALLTXT))
    ws = [w for w in re.findall(rf"[{WORD}]+", phrase) if w.lower() not in ('el', 'la', 'los', 'las', 'de', 'del')]
    return all(w[0].islower() and _LOWER[w.lower()] - n_p >= 3 for w in ws)
for ep in eps:
    for f, c, v in ep_texts(ep):
        for m in _AP.finditer(v):
            a = m.group(1).strip()
            if not _common(a): ALIAS_EXTRA[ep].append(a)
maps, alias_maps, key_rows, collisions, pmaps = {}, {}, [], [], {}
for _ep, _runs in EXTRA.items():
    for _r in _runs: REAL_ALL.update(_r)
REAL_N = {norm(x) for x in REAL_ALL}
GM = [g for g in GM if norm(g) not in REAL_N]; GF = [g for g in GF if norm(g) not in REAL_N]
SU = [x for x in SU if norm(x) not in REAL_N]
for ep in eps:
    rnd = random.Random('meaculpa-' + ep)
    gm, gf, su, al = GM[:], GF[:], SU[:], AL[:]
    for L in (gm, gf, su, al): rnd.shuffle(L)
    tm, am, pm = {}, {}, {}
    sex = {}
    eps_p = [p for p in pers if p['id_episodio'] == ep]
    parsed = [(p, *parse(p['nombre'])) for p in eps_p]
    for p, runs, als in parsed:
        for r in runs[:1]:
            for t, k in classify(r):
                if k == 'g': sex.setdefault(t, p['sexo'])
    parsed += [({'nombre': ' '.join(r), 'sexo': ''}, [r], []) for r in EXTRA[ep]]
    parsed += [({'nombre': a, 'sexo': ''}, [], [a]) for a in ALIAS_EXTRA[ep]]
    for p, runs, als in parsed:
        for ri, r in enumerate(runs):
            for t, k in classify(r):
                if t in tm: continue
                if any(e2 == ep and norm(t2) == norm(t) for e2, t2 in DROP): continue
                if norm(t) in {norm(x) for x in ep_loc[ep]} and ri > 0 and p.get('sexo') == '': continue
                same = next((v for k2, v in tm.items() if norm(k2) == norm(t)), None)
                if same: tm[t] = same; continue
                if t in NEVER:
                    if t not in pm:
                        collisions.append((ep, t, p['nombre']))
                        g = gender_of(t, sex.get(t, p['sexo'] if ri == 0 else ''))
                        pm[t] = (gf if g == 'F' else gm).pop() if k == 'g' else su.pop()
                    continue
                if k == 'g':
                    g = gender_of(t, sex.get(t, p['sexo'] if ri == 0 else ''))
                    tm[t] = (gf if g == 'F' else gm).pop()
                else:
                    tm[t] = su.pop()
        for a in als:
            if a.lower() in ALIAS_BAD: continue
            if a.lower() not in am:
                want = 'la' if a.lower().startswith('la ') or (p.get('sexo') == 'F' and not a.lower().startswith('el ')) else 'el'
                pick = next((x for x in reversed(al) if x.startswith(want + ' ')), al[-1]); al.remove(pick)
                am[a.lower()] = pick
                bare = re.sub(r'^(el|la|los|las)\s+', '', a, flags=re.I)
                if bare != a and bare.lower() not in BARE_BAD and len(bare) > 3 and not _common(bare) and bare.lower() not in am:
                    am[bare.lower()] = re.sub(r'^(el|la)\s+', '', pick)
    maps[ep], alias_maps[ep], pmaps[ep] = tm, am, pm

ACC = {'a':'[aá]','á':'[aá]','e':'[eé]','é':'[eé]','i':'[ií]','í':'[ií]','o':'[oó]','ó':'[oó]','u':'[uúü]','ú':'[uúü]','ü':'[uúü]',
       'A':'[AÁ]','Á':'[AÁ]','E':'[EÉ]','É':'[EÉ]','I':'[IÍ]','Í':'[IÍ]','O':'[OÓ]','Ó':'[OÓ]','U':'[UÚ]','Ú':'[UÚ]'}
def acc(t): return ''.join(ACC.get(ch, re.escape(ch)) for ch in t)
MASK = re.compile(r"\b(?:Papa|San|Santa|Santo|Virgen|Calle|calle|Avenida|avenida|Av\.|Villa|villa|Población|población|Hospital|Clínica|"
                  r"Colegio|Liceo|Escuela|Instituto|Universidad|Fundación|Plaza|Parque|Cerro|Puente|Puerto|Pasaje|pasaje|Estación|Isla|"
                  r"Lago|Río|Fundo|Hacienda|Cementerio|Parroquia|Capilla|Teatro|Hotel|Motel|Industrias|Banco|Mina|Maternidad)"
                  r"\s+[A-ZÁÉÍÓÚÑ][A-Za-zÁÉÍÓÚÜÑáéíóúüñ]+(?:\s+(?:de\s+|del\s+)?(?:[A-ZÁÉÍÓÚÑ][A-Za-zÁÉÍÓÚÜÑáéíóúüñ]+|II|III|XXIII))*")
def anon_name_map(ep, full=False):
    tm, am, pm = maps.get(ep, {}), alias_maps.get(ep, {}), pmaps.get(ep, {})
    allm = {**pm, **tm}
    pats = []
    if allm:
        alt = '|'.join(acc(t) for t in sorted(allm, key=len, reverse=True))
        nall = {norm(k): v for k, v in allm.items()}
        span = re.compile(r"(?<![%s])(?:%s)(?:(?:\s+(?:de|del|la|los|y)\s+|\s+)(?:%s))+(?![%s])" % (WORD, alt, alt, WORD))
        pats.append(('s', span, alt))
    if full and pm: tm = allm
    if am:
        pa = re.compile(r"(?<![%s])(%s)(?![%s])" % (WORD, '|'.join(re.escape(a) for a in sorted(am, key=len, reverse=True)), WORD), re.I)
        pats.append(('a', pa))
    if tm:
        pt = re.compile(r"(?<![%s])(%s)(?![%s])" % (WORD, '|'.join(acc(t) for t in sorted(tm, key=len, reverse=True)), WORD))
        ntm = {norm(k): (k, v) for k, v in tm.items()}
        pats.append(('t', pt))
    def apply(text):
        if not text or text == 'NA': return text
        masks = []
        def _mask(m):
            masks.append(m.group(0)); return '\ue000%d\ue001' % (len(masks) - 1)
        text = MASK.sub(_mask, text)
        out = _apply(text)
        return re.sub('\ue000(\\d+)\ue001', lambda m: masks[int(m.group(1))], out)
    def _apply(text):
        for item in pats:
            kind, pat = item[0], item[1]
            if kind == 's':
                tokre = re.compile(r"(?<![%s])(%s)(?![%s])" % (WORD, item[2], WORD))
                _t = text
                text = pat.sub(lambda m: m.group(0) if HEADS.search(_t[:m.start()]) else tokre.sub(lambda t: nall[norm(t.group(1))], m.group(0)), text)
                continue
            def rep(m):
                if kind == 'a':
                    return am[m.group(1).lower()]
                pre = text_ref[0][:m.start()]
                if HEADS.search(pre): return m.group(1)
                key, val = ntm[norm(m.group(1))]
                la = KEEP_IF_FOLLOWED.get((ep, key))
                if la and re.match(la, text_ref[0][m.end():]): return m.group(1)
                return val
                return tm[m.group(1)]
            text_ref = [text]
            text = pat.sub(rep, text)
        return text
    return apply

SKIP_COL = re.compile(r'^(id_|temporada$|capitulo$|video_id$|from_personaje|to_personaje|orden$|es_|hay_|n_|ses_nivel$|ses_min|ses_max|ses_promedio|simetrica$)')
appliers = {ep: anon_name_map(ep) for ep in eps}
appliers_full = {ep: anon_name_map(ep, True) for ep in eps}
os.makedirs(OUT, exist_ok=True)
changed = collections.Counter()
for f, (cols, rows) in T.items():
    newrows = []
    for r in rows:
        ap = appliers.get(r.get('id_episodio'))
        nr = dict(r)
        if ap:
            for c in cols:
                if not SKIP_COL.match(c):
                    v = (appliers_full[r['id_episodio']] if (f, c) in FULLCOLS else ap)(r[c])
                    if v != r[c]: changed[f] += 1
                    nr[c] = v
        newrows.append(nr)
    with open(os.path.join(OUT, f), 'w', encoding='utf-8-sig', newline='') as fh:
        w = csv.DictWriter(fh, fieldnames=cols, quoting=csv.QUOTE_MINIMAL); w.writeheader(); w.writerows(newrows)
    T[f] = (cols, rows, newrows)

# clave de correspondencia
for p in pers:
    ep = p['id_episodio']; new = next(r for r in T['04_personajes.csv'][2] if r['id_personaje_global'] == p['id_personaje_global'])
    key_rows.append([ep, p['id_personaje_global'], p['nombre'], new['nombre']])
json.dump(dict(changed=changed, collisions=collisions, maps={k: v for k, v in maps.items()}, alias=alias_maps),
          open('reporte.json', 'w'), ensure_ascii=False, indent=1)
with open('clave.csv', 'w', encoding='utf-8-sig', newline='') as fh:
    w = csv.writer(fh); w.writerow(['id_episodio', 'id_personaje_global', 'nombre_real', 'nombre_ficticio']); w.writerows(key_rows)
print('celdas modificadas por tabla:', dict(changed)); print('tokens protegidos (no reemplazados):', len(collisions))
