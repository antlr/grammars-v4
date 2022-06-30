CALL db.labels() YIELD label
WHERE label CONTAINS 'User'
RETURN count(label) AS numLabels