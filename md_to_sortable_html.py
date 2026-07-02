#!/usr/bin/env python3
"""Convert a Markdown file containing a single table into a sortable HTML page."""
import re, sys, html as html_mod

def parse_num(s):
    s = s.strip()
    if 'n.a.' in s:
        return None
    m = re.match(r'([\d.]+(?:e[+\-]\d+)?)', s)
    if m:
        try:
            return float(m.group(1))
        except Exception:
            pass
    return None

src = open(sys.argv[1], encoding='latin-1').read()
lines = src.splitlines()

title = ""
headers = None
rows = []

for line in lines:
    if re.match(r'^##\s', line):
        title = line.lstrip('#').strip()
        continue
    if not line.startswith('|'):
        continue
    cells = [c.strip() for c in line.split('|')[1:-1]]
    if not cells:
        continue
    if all(re.match(r'^[-: ]+$', c) for c in cells):
        continue  # separator row
    if headers is None:
        headers = cells
    else:
        rows.append(cells)

if not headers:
    sys.exit("No table found in " + sys.argv[1])

def is_numeric_col(i):
    vals = [parse_num(r[i]) for r in rows if i < len(r)]
    numeric = [v for v in vals if v is not None]
    return len(numeric) > len(vals) / 2

numeric_cols_py = [is_numeric_col(i) for i in range(len(headers))]
numeric_cols_js = '[' + ','.join('true' if v else 'false' for v in numeric_cols_py) + ']'

h = html_mod.escape

thead = ''.join(f'  <th onclick="sortTable({i})">{h(hdr)}</th>\n' for i, hdr in enumerate(headers))
tbody = ''
for row in rows:
    tbody += '<tr>' + ''.join(f'<td>{h(cell)}</td>' for cell in row) + '</tr>\n'

out = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>{h(title)}</title>
<style>
  body {{ font-family: sans-serif; padding: 1em; }}
  h2 {{ margin-bottom: 0.5em; }}
  table {{ border-collapse: collapse; font-size: 0.85em; }}
  th, td {{ border: 1px solid #ccc; padding: 4px 10px; white-space: nowrap; }}
  th {{ background: #eef; cursor: pointer; user-select: none; }}
  th:hover {{ background: #ccf; }}
  th.asc::after {{ content: " \\25B2"; }}
  th.desc::after {{ content: " \\25BC"; }}
  tr:nth-child(even) td {{ background: #f7f7f7; }}
  td {{ text-align: right; }}
  td:first-child {{ text-align: left; }}
</style>
</head>
<body>
<h2>{h(title)}</h2>
<table id="t">
<thead><tr>
{thead}</tr></thead>
<tbody>
{tbody}</tbody>
</table>
<script>
const NUM = {numeric_cols_js};
let col = -1, dir = 1;
function parseVal(s, num) {{
  s = s.trim();
  if (!num) return s.toLowerCase();
  if (s.indexOf('n.a.') !== -1) return -Infinity;
  const m = s.match(/^([\\d.]+(?:e[+\\-]\\d+)?)/);
  return m ? parseFloat(m[1]) : -Infinity;
}}
function sortTable(c) {{
  const tbl = document.getElementById('t');
  const ths = tbl.querySelectorAll('th');
  dir = (col === c) ? -dir : 1;
  col = c;
  ths.forEach((th, i) => {{ th.className = (i === c) ? (dir === 1 ? 'asc' : 'desc') : ''; }});
  const tb = tbl.querySelector('tbody');
  const rs = Array.from(tb.querySelectorAll('tr'));
  rs.sort((a, b) => {{
    const av = parseVal(a.cells[c].textContent, NUM[c]);
    const bv = parseVal(b.cells[c].textContent, NUM[c]);
    return av < bv ? -dir : av > bv ? dir : 0;
  }});
  rs.forEach(r => tb.appendChild(r));
}}
</script>
</body>
</html>
"""

out_path = sys.argv[1].replace('.md', '.html')
open(out_path, 'w', encoding='utf-8', newline='\n').write(out)
print(f"Written: {out_path}")
