#!/usr/bin/env python3
"""Convert a Markdown file with tables into an HTML page.
The last (largest) table is made sortable; earlier tables are static."""
import re, sys, html as html_mod
from urllib.parse import quote

def md_inline(s):
    """Convert basic Markdown inline formatting to HTML."""
    s = html_mod.escape(s)
    s = re.sub(r'\[([^\]]+)\]\(([^)]+)\)', r'<a href="\2">\1</a>', s)
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

# Preserve Markdown blocks in their original order. The largest table is sortable.
title = ""
blocks = []       # ('heading', level, text), ('paragraph', text), ('table', table)
tables = []
cur_heading = ""
paragraph = []
current_table = None

def flush_paragraph():
    global paragraph
    if paragraph:
        blocks.append(('paragraph', ' '.join(paragraph)))
        paragraph = []

def flush_table():
    global current_table
    if current_table is not None and current_table['headers'] is not None:
        tables.append(current_table)
        blocks.append(('table', current_table))
    current_table = None

for line in lines:
    heading = re.match(r'^(#{1,6})\s+(.*)$', line)
    if heading:
        flush_paragraph()
        flush_table()
        level = len(heading.group(1))
        text = heading.group(2).strip()
        blocks.append(('heading', level, text))
        cur_heading = text
        if not title:
            title = text
        continue

    if line.startswith('|'):
        flush_paragraph()
        if current_table is None:
            current_table = {'heading': cur_heading, 'headers': None, 'rows': []}
        cells = [c.strip() for c in line.split('|')[1:-1]]
        if not cells or all(re.fullmatch(r'[-: ]+', c) for c in cells):
            continue
        if current_table['headers'] is None:
            current_table['headers'] = cells
        else:
            current_table['rows'].append(cells)
        continue

    flush_table()
    if not line.strip():
        flush_paragraph()
    else:
        paragraph.append(line.strip())

flush_paragraph()
flush_table()

if not tables:
    sys.exit("No tables found in " + sys.argv[1])

# The sortable table is the one with the most rows
sortable_idx = max(range(len(tables)), key=lambda i: len(tables[i]['rows']))

def is_numeric_col(rows, i):
    values = [r[i].strip() for r in rows if i < len(r)]
    present = [v for v in values if v and 'n.a.' not in v]
    return bool(present) and all(parse_num(v) is not None for v in present)

def render_cell(tbl, column, value):
    if (tbl['heading'] == 'Results' and
            column < len(tbl['headers']) and tbl['headers'][column] in ('Grammar', 'Grammar Name')):
        url = 'https://github.com/antlr/grammars-v4/tree/master/' + quote(value, safe='/')
        return f'<a href="{html_mod.escape(url, quote=True)}">{md_inline(value)}</a>'
    if (column < len(tbl['headers']) and tbl['headers'][column] == 'Combined'):
        normalized = value.strip().lower()
        if normalized in ('yes', 'no'):
            return f'<span class="combined-{normalized}">{normalized}</span>'
    return md_inline(value)

def render_static_table(tbl):
    out = ''

    out += '<table class="key-table">\n<thead><tr>'
    out += ''.join(f'<th>{md_inline(c)}</th>' for c in tbl['headers'])
    out += '</tr></thead>\n<tbody>\n'
    for row in tbl['rows']:
        out += '<tr>' + ''.join(f'<td style="text-align:left">{render_cell(tbl, i, c)}</td>'
                                  for i, c in enumerate(row)) + '</tr>\n'
    out += '</tbody>\n</table>\n'
    return out

def comparison_classes(headers, row):
    """Highlight the best and worst available mean within each metric group."""
    classes = {}
    for names, lower_is_better in (
        ({'TTN', 'TTT', 'TTR', 'TTJ', 'TTD', 'TTG'}, True),
        ({'PRN', 'PRT', 'PRR', 'PRJ', 'PRD', 'PRG'}, False),
    ):
        values = {
            i: parse_num(row[i])
            for i, header in enumerate(headers)
            if header.split()[0] in names and i < len(row)
        }
        values = {i: value for i, value in values.items() if value is not None}
        if len(values) < 2 or min(values.values()) == max(values.values()):
            continue
        best = min(values.values()) if lower_is_better else max(values.values())
        worst = max(values.values()) if lower_is_better else min(values.values())
        for i, value in values.items():
            if value == best:
                classes[i] = 'metric-best'
            elif value == worst:
                classes[i] = 'metric-worst'
    return classes

def render_sortable_table(tbl, table_id):
    headers = tbl['headers']
    rows = tbl['rows']
    numbered = bool(headers and headers[0] == 'Number')
    numeric_cols_py = [hdr != 'Combined' and is_numeric_col(rows, i)
                       for i, hdr in enumerate(headers)]
    numeric_cols_js = '[' + ','.join('true' if v else 'false' for v in numeric_cols_py) + ']'

    out = ''

    out += f'<table id="{table_id}">\n<thead><tr>\n'
    for i, hdr in enumerate(headers):
        if numbered and i == 0:
            out += f'  <th class="row-number">{h(hdr)}</th>\n'
        else:
            out += f'  <th onclick="sortTable(\'{table_id}\',{i})">{h(hdr)}</th>\n'
    out += '</tr></thead>\n<tbody>\n'
    for row in rows:
        classes = comparison_classes(headers, row)
        out += '<tr>' + ''.join(
            f'<td class="{classes[i]}">{render_cell(tbl, i, c)}</td>'
            if i in classes else f'<td>{render_cell(tbl, i, c)}</td>'
            for i, c in enumerate(row)) + '</tr>\n'
    out += '</tbody>\n</table>\n'

    out += f"""<script>
(function() {{
  const NUM = {numeric_cols_js};
  const NUMBERED = {str(numbered).lower()};
  let col = -1, dir = 1;
  function parseVal(s, num) {{
    s = s.trim();
    if (!num) return s.toLowerCase();
    if (s.indexOf('n.a.') !== -1) return -Infinity;
    const m = s.match(/^([\\d.]+(?:e[+\\-]\\d+)?)/);
    return m ? parseFloat(m[1]) : -Infinity;
  }}
  window.sortTable = function(id, c) {{
    if (NUMBERED && c === 0) return;
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
    rs.forEach((r, i) => {{
      tb.appendChild(r);
      if (NUMBERED) r.cells[0].textContent = String(i + 1);
    }});
  }};
}})();
</script>
"""
    return out

body = ''
for block in blocks:
    if block[0] == 'heading':
        _, level, text = block
        body += f'<h{level}>{md_inline(text)}</h{level}>\n'
    elif block[0] == 'paragraph':
        body += f'<p>{md_inline(block[1])}</p>\n'
    else:
        tbl = block[1]
        body += (render_sortable_table(tbl, 'perf-table')
                 if tables[sortable_idx] is tbl else render_static_table(tbl))

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
  #perf-table th:not(.row-number) {{ cursor: pointer; user-select: none; }}
  #perf-table th:not(.row-number):hover {{ background: #ccf; }}
  th.asc::after {{ content: " \\25B2"; }}
  th.desc::after {{ content: " \\25BC"; }}
  tr:nth-child(even) td {{ background: #f7f7f7; }}
  td {{ text-align: right; }}
  td:first-child {{ text-align: left; }}
  .key-table td {{ text-align: left; }}
  .combined-yes {{ color: #176b38; font-weight: 600; }}
  .combined-no {{ color: #8d2932; font-weight: 600; }}
  #perf-table td.metric-best {{ background: #d9f2df; color: #176b38; font-weight: 600; }}
  #perf-table td.metric-worst {{ background: #f9dddd; color: #8d2932; font-weight: 600; }}
</style>
</head>
<body>
{body}</body>
</html>
"""

out_path = sys.argv[1].replace('.md', '.html')
open(out_path, 'w', encoding='utf-8', newline='\n').write(out)
print(f"Written: {out_path}")
