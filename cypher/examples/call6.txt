CALL db.labels() YIELD label, foo, bar
WHERE label CONTAINS 'User' AND foo + bar = foo
RETURN count(label) AS numLabels