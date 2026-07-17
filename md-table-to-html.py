#!/usr/bin/env python3
"""Convert the last table in performance.md to an HTML page equivalent to performance.html.
Output goes to stdout.

Usage: python md-table-to-html.py [performance.md]
"""
import sys
import html as html_module
import io

# Ensure stdout can emit UTF-8 (needed on Windows where default is cp1252)
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")

GITHUB_BASE = "https://github.com/antlr/grammars-v4/tree/master"

STATIC_HEAD = """\
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>Performance Summary (N=3 runs, mean \u00b1 SEM)</title>
<style>
  body { font-family: sans-serif; padding: 1em; }
  h2, h3 { margin-bottom: 0.4em; }
  table { border-collapse: collapse; font-size: 0.85em; margin-bottom: 1.5em; }
  th, td { border: 1px solid #ccc; padding: 4px 10px; white-space: nowrap; }
  th { background: #eef; }
  #perf-table th { cursor: pointer; user-select: none; }
  #perf-table th:hover { background: #ccf; }
  th.asc::after { content: " \\25B2"; }
  th.desc::after { content: " \\25BC"; }
  tr:nth-child(even) td { background: #f7f7f7; }
  td { text-align: right; }
  td:first-child { text-align: left; }
  .key-table td { text-align: left; }
</style>
</head>
<body>
<h2>Performance Summary (N=3 runs, mean \u00b1 SEM)</h2>
<h3>Column key</h3>
<table class="key-table">
<thead><tr><th>Label</th><th>Description</th></tr></thead>
<tbody>
<tr><td style="text-align:left"><code>PT</code></td><td style="text-align:left"><strong>Parse time</strong> \u2014 sum of the parse-only time across all input files</td></tr>
<tr><td style="text-align:left"><code>OT</code></td><td style="text-align:left"><strong>Overhead time</strong> \u2014 <code>TT \u2212 PT</code>; time spent on file I/O, lexer/parser construction, and post-parse work</td></tr>
<tr><td style="text-align:left"><code>TT</code></td><td style="text-align:left"><strong>Total time</strong> \u2014 overall wall-clock time for the entire run</td></tr>
<tr><td style="text-align:left"><code>TPS</code></td><td style="text-align:left"><strong>Tokens per second</strong> \u2014 <code>total tokens / PT</code>; pure parse throughput across all files</td></tr>
<tr><td style="text-align:left"><code>Post-warmup TPS</code></td><td style="text-align:left">TPS computed from files 2\u2026N only, excluding the first (warm-up) run; <code>n.a.</code> if only one file was parsed</td></tr>
<tr><td style="text-align:left"><code>Post-warmup Speed Up</code></td><td style="text-align:left">Ratio of Post-warmup TPS to first-file TPS, showing the JIT/runtime warm-up benefit; <code>n.a.</code> if only one file was parsed</td></tr>
</tbody>
</table>
<h3>Results</h3>
<p>Runtime of examples/ on AMD Ryzen 7 2700 Eight-Core Processor;
   16GB DDR4; Samsung SSD 990 EVO Plus 2TB; Windows: Version
   10.0.26200.7623 (this is a Windows 11 Insider Preview build); .NET
   SDK: 10.0.301.</p>\
"""

STATIC_SCRIPT = """\
<script>
(function() {
  const NUM = [false,true,true,true,true,true,true];
  let col = -1, dir = 1;
  function parseVal(s, num) {
    s = s.trim();
    if (!num) return s.toLowerCase();
    if (s.indexOf('n.a.') !== -1) return -Infinity;
    const m = s.match(/^([\\d.]+(?:e[+\\-]\\d+)?)/);
    return m ? parseFloat(m[1]) : -Infinity;
  }
  window.sortTable = function(id, c) {
    const tbl = document.getElementById(id);
    const ths = tbl.querySelectorAll('th');
    dir = (col === c) ? -dir : 1;
    col = c;
    ths.forEach((th, i) => { th.className = (i === c) ? (dir === 1 ? 'asc' : 'desc') : ''; });
    const tb = tbl.querySelector('tbody');
    const rs = Array.from(tb.querySelectorAll('tr'));
    rs.sort((a, b) => {
      const av = parseVal(a.cells[c].textContent, NUM[c]);
      const bv = parseVal(b.cells[c].textContent, NUM[c]);
      return av < bv ? -dir : av > bv ? dir : 0;
    });
    rs.forEach(r => tb.appendChild(r));
  };
})();
</script>
</body>
</html>\
"""


def parse_last_table(path):
    """Return (headers, data_rows) from the last markdown table in path.
    Each row is a list of cell strings.
    """
    with open(path, encoding="utf-8") as f:
        lines = f.readlines()

    # Collect all contiguous blocks of pipe-rows; keep the last one.
    last_block = []
    current = []
    for line in lines:
        if line.lstrip().startswith("|"):
            current.append(line.strip())
        else:
            if current:
                last_block = current
            current = []
    if current:
        last_block = current

    if len(last_block) < 2:
        raise ValueError("No table found in " + path)

    def split_row(row):
        # Strip leading/trailing | then split
        return [c.strip() for c in row.strip("|").split("|")]

    headers = split_row(last_block[0])
    # last_block[1] is the separator line --- skip it
    rows = [split_row(line) for line in last_block[2:]]
    return headers, rows


def main():
    md_path = sys.argv[1] if len(sys.argv) > 1 else "performance.md"
    headers, rows = parse_last_table(md_path)

    out = []
    out.append(STATIC_HEAD)
    out.append('\n<table id="perf-table">')
    out.append("<thead><tr>")
    for i, h in enumerate(headers):
        out.append(f"  <th onclick=\"sortTable('perf-table',{i})\">{html_module.escape(h)}</th>")
    out.append("</tr></thead>")
    out.append("<tbody>")
    for row in rows:
        cells = []
        for i, cell in enumerate(row):
            if i == 0:
                # Grammar name — wrap in GitHub link
                escaped = html_module.escape(cell)
                url = f"{GITHUB_BASE}/{cell}"
                cells.append(f'<td><a href="{url}">{escaped}</a></td>')
            else:
                cells.append(f"<td>{html_module.escape(cell)}</td>")
        out.append("<tr>" + "".join(cells) + "</tr>")
    out.append("</tbody>")
    out.append("</table>")
    out.append(STATIC_SCRIPT)

    print("\n".join(out))


if __name__ == "__main__":
    main()
