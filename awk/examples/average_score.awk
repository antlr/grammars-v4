#!/usr/bin/awk -f
BEGIN {
	total = 0
	count = 0
}
{
	name = $1
	score = $2
	total += score
	count++
	scores[name] = score
}
END {
	avg = total / count
	printf("Average score: %.2f\n", avg)
	for (name in scores) {
		if (scores[name] > avg) {
			printf("%s: %.2f\n", name, scores[name])
		}
	}
}
