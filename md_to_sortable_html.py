#!/usr/bin/env python3
"""Convert a Markdown file with tables into an HTML page.
The last (largest) table is made sortable; earlier tables are static."""
import re, sys, html as html_mod

def md_inline(s):
    """Convert basic Markdown inline formatting to HTML."""
    s = html_mod.escape(s)
    s = re.sub(r'\*\*(.+?)\*\*', r'<strong>\1</strong>', s)
    s = re.sub(r'`([^`]+)`', r'<code>\1</code>', s)
    return s

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

src = open(sys.argv[1], encoding='utf-8').read()
lines = src.splitlines()

h = html_mod.escape

# Collect title and all tables {headers, rows, heading}
title = ""
tables = []       # list of dicts: {heading, headers, rows}
cur_heading = ""
cur_headers = None
cur_rows = []

def flush():
    if cur_headers:
        tables.append({'heading': cur_heading, 'headers': cur_headers, 'rows': list(cur_rows)})

for line in lines:
    if re.match(r'^#{1,6}\s', line):
        flush()
        cur_headers = None
        cur_rows = []
        cur_heading = re.sub(r'^#{1,6}\s+', '', line).strip()
        if not title:
            title = cur_heading
        continue
    if not line.startswith('|'):
        continue
    cells = [c.strip() for c in line.split('|')[1:-1]]
    if not cells:
        continue
    if all(re.match(r'^[-: ]+$', c) for c in cells):
        continue  # separator row
    if cur_headers is None:
        cur_headers = cells
    else:
        cur_rows.append(cells)

flush()

if not tables:
    sys.exit("No tables found in " + sys.argv[1])

# The sortable table is the one with the most rows
sortable_idx = max(range(len(tables)), key=lambda i: len(tables[i]['rows']))

def is_numeric_col(rows, i):
    vals = [parse_num(r[i]) for r in rows if i < len(r)]
    numeric = [v for v in vals if v is not None]
    return len(numeric) > len(vals) / 2

def render_static_table(tbl):
    out = ''
    if tbl['heading']:
        out += f'<h3>{h(tbl["heading"])}</h3>\n'
    out += '<table>\n<thead><tr>'
    out += ''.join(f'<th>{md_inline(c)}</th>' for c in tbl['headers'])
    out += '</tr></thead>\n<tbody>\n'
    for row in tbl['rows']:
        out += '<tr>' + ''.join(f'<td>{md_inline(c)}</td>' for c in row) + '</tr>\n'
    out += '</tbody>\n</table>\n'
    return out

def render_sortable_table(tbl, table_id):
    headers = tbl['headers']
    rows = tbl['rows']
    numeric_cols_py = [is_numeric_col(rows, i) for i in range(len(headers))]
    numeric_cols_js = '[' + ','.join('true' if v else 'false' for v in numeric_cols_py) + ']'

    out = ''
    if tbl['heading']:
        out += f'<h3>{h(tbl["heading"])}</h3>\n'
    out += f'<table id="{table_id}">\n<thead><tr>\n'
    out += ''.join(f'  <th onclick="sortTable(\'{table_id}\',{i})">{h(hdr)}</th>\n'
                   for i, hdr in enumerate(headers))
    out += '</tr></thead>\n<tbody>\n'
    for row in rows:
        out += '<tr>' + ''.join(f'<td>{h(c)}</td>' for c in row) + '</tr>\n'
    out += '</tbody>\n</table>\n'

    out += f"""<script>
(function() {{
  const NUM = {numeric_cols_js};
  let col = -1, dir = 1;
  function parseVal(s, num) {{
    s = s.trim();
    if (!num) return s.toLowerCase();
    if (s.indexOf('n.a.') !== -1) return -Infinity;
    const m = s.match(/^([\\d.]+(?:e[+\\-]\\d+)?)/);
    return m ? parseFloat(m[1]) : -Infinity;
  }}
  window.sortTable = function(id, c) {{
    const tbl = document.getElementById(id);
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
  }};
}})();
</script>
"""
    return out

body = ''
for i, tbl in enumerate(tables):
    if i == sortable_idx:
        body += render_sortable_table(tbl, 'perf-table')
    else:
        body += render_static_table(tbl)

out = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>{h(title)}</title>
<style>
  body {{ font-family: sans-serif; padding: 1em; }}
  h2, h3 {{ margin-bottom: 0.4em; }}
  table {{ border-collapse: collapse; font-size: 0.85em; margin-bottom: 1.5em; }}
  th, td {{ border: 1px solid #ccc; padding: 4px 10px; white-space: nowrap; }}
  th {{ background: #eef; }}
  #perf-table th {{ cursor: pointer; user-select: none; }}
  #perf-table th:hover {{ background: #ccf; }}
  th.asc::after {{ content: " \\25B2"; }}
  th.desc::after {{ content: " \\25BC"; }}
  tr:nth-child(even) td {{ background: #f7f7f7; }}
  td {{ text-align: right; }}
  td:first-child {{ text-align: left; }}
</style>
</head>
<body>
<h2>{h(title)}</h2>
{body}</body>
</html>
"""

out_path = sys.argv[1].replace('.md', '.html')
open(out_path, 'w', encoding='utf-8', newline='\n').write(out)
print(f"Written: {out_path}")
