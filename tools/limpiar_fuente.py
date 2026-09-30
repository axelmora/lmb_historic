"""Limpia el xlsx fuente sin tocar su estructura:
 - quita acentos de los NOMBRES DE EQUIPO en todas las hojas (standings, series, catálogo, long);
 - corrige 'Dorados deChihuahua'; normaliza y quita duplicados en catalogo.nombres_usados;
 - restaura marcadores que Excel convirtió en fecha (2-1 -> texto '2-1');
 - convierte el texto 'NA' en celdas vacías.
Uso: python tools/limpiar_fuente.py entrada.xlsx salida.xlsx"""
import sys, re, unicodedata, datetime
from openpyxl import load_workbook

def quita(s):
    return unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode()

def nombre(s):
    s = re.sub(r"\b(de|del)(?=[A-Z])", r"\1 ", quita(str(s)))
    return re.sub(r"\s+", " ", s).strip()

COLS = {"standings_1925_2026": ["equipo"], "series_result": ["ganador_equipo_catalogo", "perdedor_equipo_catalogo"],
        "equipos_long_format": ["equipo_canonico", "nombre_temporada"], "catalogo_equipos": ["equipo"]}
def main(src, dst):
    wb = load_workbook(src); log = []
    for ws in wb.worksheets:
        hdr = {c.value: c.column for c in ws[1] if c.value}
        na = 0
        for row in ws.iter_rows(min_row=2):
            for c in row:
                if isinstance(c.value, str) and c.value.strip() == "NA": c.value = None; na += 1
        if na: log.append(f"{ws.title}: {na} celdas 'NA' -> vacías")
        ch = 0
        for col in COLS.get(ws.title, []):
            for r in range(2, ws.max_row + 1):
                c = ws.cell(r, hdr[col])
                if isinstance(c.value, str):
                    n = nombre(c.value)
                    if n != c.value: c.value = n; ch += 1
        if ws.title == "catalogo_equipos":
            k = hdr["nombres_usados"]
            for r in range(2, ws.max_row + 1):
                c = ws.cell(r, k)
                if isinstance(c.value, str):
                    parts = [nombre(p) for p in re.split(r"\s*;\s*", c.value) if p.strip()]
                    n = "; ".join(dict.fromkeys(parts))
                    if n != c.value: c.value = n; ch += 1
            t = hdr["temporadas"]
            for r in range(2, ws.max_row + 1):
                c = ws.cell(r, t)
                if isinstance(c.value, (int, float)): c.value = str(int(c.value)); ch += 1
                elif isinstance(c.value, str):
                    n = c.value.replace("–", "-").replace(" ", "")
                    n = re.sub(r";", "; ", n)
                    if n != c.value: c.value = n; ch += 1
        if ws.title == "series_result":
            m = hdr["marcador"]; f = 0
            for r in range(2, ws.max_row + 1):
                c = ws.cell(r, m)
                if isinstance(c.value, (datetime.datetime, datetime.date)):
                    c.value = f"{c.value.month}-{c.value.day}"; c.number_format = "@"; f += 1
            log.append(f"series_result: {f} marcadores restaurados de fecha a texto")
        log.append(f"{ws.title}: {ch} textos de equipo normalizados")
    wb.save(dst); print("\n".join(log))
main(*sys.argv[1:3])
