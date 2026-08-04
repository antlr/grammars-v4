import re
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker

data = []

with open("performance.md", "r", encoding="utf-8") as f:
    for line in f:
        line = line.strip()
        if not line.startswith("|"):
            continue
        cols = [c.strip() for c in line.split("|")]
        cols = [c for c in cols if c]  # remove empty from leading/trailing |
        if len(cols) < 8:
            continue
        grammar = cols[0]
        # Skip header rows
        if grammar in ("Grammar", "Label", "---", "------"):
            continue
        if "---" in grammar:
            continue

        post_warmup_tps_raw = cols[5]
        total_ambig_raw = cols[7]

        # Skip n.a. rows
        if "n.a." in post_warmup_tps_raw:
            continue

        # Parse "value ± sem" or "value || value" patterns
        def parse_value(s):
            s = s.strip()
            # Handle "0 || 0" or "value || value" (parse error / timeout rows)
            if "||" in s:
                return None
            # Take the part before "±"
            part = s.split("±")[0].strip()
            try:
                return float(part)
            except ValueError:
                return None

        tps = parse_value(post_warmup_tps_raw)
        ambig = parse_value(total_ambig_raw)

        if tps is None or ambig is None:
            continue

        data.append((grammar, tps, ambig))

print(f"Parsed {len(data)} grammars with post-warmup TPS data.")

grammars = [d[0] for d in data]
tps_vals  = [d[1] for d in data]
ambig_vals = [d[2] for d in data]

fig, ax = plt.subplots(figsize=(12, 7))

sc = ax.scatter(ambig_vals, tps_vals, alpha=0.65, s=40, color="steelblue", edgecolors="none")

# Annotate notable outliers (top TPS or high ambiguity)
tps_threshold = sorted(tps_vals, reverse=True)[int(len(tps_vals) * 0.05)]  # top 5%
ambig_threshold = sorted(ambig_vals, reverse=True)[int(len(ambig_vals) * 0.05)]  # top 5% ambiguity

labeled = set()
for g, t, a in data:
    if (t >= tps_threshold or a >= ambig_threshold) and g not in labeled:
        ax.annotate(g, (a, t), fontsize=7, alpha=0.8,
                    xytext=(4, 4), textcoords="offset points")
        labeled.add(g)

ax.set_xscale("symlog", linthresh=1)
ax.set_yscale("log")
ax.set_xlabel("Total Ambiguities", fontsize=12)
ax.set_ylabel("Post-warmup TPS", fontsize=12)
ax.set_title("Post-warmup TPS vs Total Ambiguities", fontsize=14)
ax.xaxis.set_major_formatter(ticker.ScalarFormatter())
ax.yaxis.set_major_formatter(ticker.ScalarFormatter())
ax.grid(True, which="both", linestyle="--", alpha=0.4)

plt.tight_layout()
plt.savefig("perf_tps_vs_ambig.png", dpi=150)
print("Saved perf_tps_vs_ambig.png")
plt.show()
