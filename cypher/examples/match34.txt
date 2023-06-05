MATCH (a)
WHERE all(a IN []WHERE a.prop )
RETURN 1