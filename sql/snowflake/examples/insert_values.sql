CREATE TEMPORARY TABLE insert_values_t (id INT, name VARCHAR);
INSERT INTO insert_values_t VALUES (1, 'a');
INSERT INTO insert_values_t VALUES (1, 'a'), (2, 'b');
INSERT INTO insert_values_t (id, name) VALUES (3, 'c'), (4, 'd'), (5, 'e');
INSERT OVERWRITE INTO insert_values_t (id, name) VALUES (6, 'f'), (7, 'g'), (8, 'h'), (9, NULL);
SELECT id, name FROM insert_values_t ORDER BY id;
