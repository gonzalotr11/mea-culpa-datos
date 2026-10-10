"""
Genera assets/data.js (los datos que usa index.html) a partir de las tablas en data/csv/.
Uso, desde la raíz del repositorio:  python scripts/build_site_data.py
Solo usa la biblioteca estándar de Python 3.
"""
import csv,json,unicodedata,collections,os
ROOT=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CSV=os.path.join(ROOT,'data','csv')
D={}
for f in sorted(os.listdir(CSV)):
    if f.endswith('.csv') and not f.startswith('00_'):
        with open(os.path.join(CSV,f),encoding='utf-8-sig',newline='') as fh:
            D[f[:-4]]=list(csv.DictReader(fh))
def s(v):
    v='' if v is None else str(v).strip()
    return '' if v in ('NA','NaN','None') else v
def b(v): return s(v).lower()=='true'
def norm(t):
    t=unicodedata.normalize('NFD',s(t).lower()); t=''.join(c for c in t if unicodedata.category(c)!='Mn')
    return ' '.join(t.replace('_',' ').split())
by=lambda sh:collections.defaultdict(list)
eps=[]
idx={}
orph=collections.Counter()
class G(dict):
    def __missing__(self,k):
        orph[k]+=1; return collections.defaultdict(list)

for e in D['01_episodios']:
    eid=s(e['id_episodio']); 
    o=dict(id=eid,t=int(s(e['temporada'])[-2:]),c=int(s(e['capitulo'])),res=s(e['resumen']),per=s(e['periodo_temporal']),loc=s(e['locacion_principal']),
      locs=s(e['locaciones_otras']),caso=s(e['tipo_caso']),hom=b(e['hay_homicidio']),men=b(e['hay_menor_involucrado']),nv=s(e['n_victimas']),
      del_=[],tem=[],temraw=[],P=[],R=[],E=[],RE=[],F=[],mec=None,cond=[],hab=[],vul=[])
    eps.append(o); idx[eid]=o
idx=G(idx)
for r in D['02_episodios_delitos']: idx[s(r['id_episodio'])]['del_'].append([s(r['categoria_delito']),b(r['es_principal'])])
for r in sorted(D['03_episodios_temas'],key=lambda r:int(s(r['orden']) or 0)):
    o=idx[s(r['id_episodio'])]; n=norm(r['tema'])
    if n and n not in o['tem']: o['tem'].append(n)
for r in D['04_personajes']:
    idx[s(r['id_episodio'])]['P'].append(dict(id=s(r['id_personaje']),n=s(r['nombre']),sx=s(r['sexo']),edad=s(r['grupo_etario']),rn=s(r['rol_narrativo']),rc=s(r['rol_criminologico']),
      occ=s(r['ocupacion']),ses=s(r['ses_nivel']),se=s(r['ses_evidencia']),d=s(r['descripcion'])))
vp=collections.defaultdict(list); va=collections.defaultdict(list)
for r in D['06_vinculos_practicos']: vp[s(r['id_relacion'])].append(s(r['vinculo_practico']))
for r in D['07_vinculos_afectivos']: va[s(r['id_relacion'])].append(norm(r['vinculo_afectivo']))
for r in D['05_relaciones']:
    rid=s(r['id_relacion'])
    idx[s(r['id_episodio'])]['R'].append(dict(id=rid,a=s(r['from_personaje']),b=s(r['to_personaje']),sim=b(r['simetrica']),asi=s(r['asimetria_poder']),d=s(r['descripcion']),vp=vp[rid],va=va[rid]))
for r in D['08_entidades']:
    idx[s(r['id_episodio'])]['E'].append(dict(id=s(r['id_entidad']),n=s(r['nombre']),tp=s(r['tipo']),st=s(r['subtipo']),fn=s(r['funcion_narrativa']),cen=s(r['centralidad']),d=s(r['descripcion'])))
for r in D['09_relaciones_pers_ent']:
    idx[s(r['id_episodio'])]['RE'].append(dict(p=s(r['id_personaje']),e=s(r['id_entidad']),v=s(r['tipo_vinculo']),asi=s(r['asimetria']),d=s(r['descripcion'])))
for r in D['10_fases_narrativas']:
    idx[s(r['id_episodio'])]['F'].append(dict(o=int(s(r['orden'])),f=s(r['fase']),ev=s(r['tipo_evento']),act=s(r['actores']),d=s(r['descripcion'])))
for r in D['11_mecanismos_causales']:
    idx[s(r['id_episodio'])]['mec']=dict(tipo=s(r['tipo_mecanismo']),pat=s(r['patron_relacional']),det=s(r['factor_detonante']),d=s(r['descripcion_mecanismo']))
for r in D['12_condiciones_estructurales']: idx[s(r['id_episodio'])]['cond'].append(s(r['condicion_estructural']))
for r in D['13_habilitadores']: idx[s(r['id_episodio'])]['hab'].append(s(r['habilitador']))
for r in D['14_vulnerabilidades']: idx[s(r['id_episodio'])]['vul'].append(dict(v=s(r['victima']),tp=s(r['tipo_vulnerabilidad']),d=s(r['descripcion'])))
for o in eps:
    o['F'].sort(key=lambda x:x['o'])
    o['del']=o.pop('del_'); o.pop('temraw')
    o['cond']=list(dict.fromkeys(o['cond'])); o['hab']=list(dict.fromkeys(o['hab']))
eps.sort(key=lambda o:(o['t'],o['c']))

counts={k:len(v) for k,v in D.items()}
playlists={}
pf=os.path.join(ROOT,'data','fuentes_temporadas.csv')
if os.path.exists(pf):
    with open(pf,encoding='utf-8-sig',newline='') as fh:
        for r in csv.DictReader(fh): playlists[int(r['temporada'])]=r['playlist']
out=dict(eps=eps,counts=counts,playlists=playlists,orphans=dict(orph))
with open(os.path.join(ROOT,'assets','data.js'),'w',encoding='utf-8') as fh:
    fh.write('// Archivo generado por scripts/build_site_data.py. No editar a mano.\n')
    fh.write('window.MC_DATA='+json.dumps(out,ensure_ascii=False,separators=(',',':'))+';\n')
print(f"{len(eps)} episodios -> assets/data.js")
if orph: print('Aviso: filas con id_episodio ausente en 01_episodios:',dict(orph))
