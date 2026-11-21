MATCH (a)-[:LIKES*2]->(b)
MATCH (a)-[:LIKES*1..3]->(c)
MATCH (a)-[:LIKES*0]->(d)
RETURN b, c, d
