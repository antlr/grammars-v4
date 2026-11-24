-- ONLINE, OFFLINE, JSON_ARRAYAGG and JSON_OBJECTAGG are not reserved: they can be used as identifiers.
CREATE TABLE t_words (online INT, offline INT, json_arrayagg INT, json_objectagg INT);
SELECT online, offline, json_arrayagg, json_objectagg FROM t_words;
-- Followed by an opening parenthesis, JSON_ARRAYAGG and JSON_OBJECTAGG are still the aggregate functions (OVER needs them).
SELECT JSON_ARRAYAGG(online) OVER (), JSON_OBJECTAGG(json_arrayagg, json_objectagg) OVER () FROM t_words;
