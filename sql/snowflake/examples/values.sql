SELECT $1, $2 FROM VALUES ('a', 'b');
SELECT $1, $2 FROM VALUES ('a', 'b') a;
SELECT $1, $2 FROM VALUES ('a', 'b') as a;
SELECT $1, $2 FROM (VALUES ('a', 'b'));
SELECT $1, $2 FROM (VALUES ('a', 'b')) a;
SELECT $1, $2 FROM (VALUES ('a', 'b')) as a;
SELECT $1, $2 FROM (VALUES ('a', 'b'),('c','d')) as a;
SELECT f, g FROM (VALUES ('a', 'b'),('c','d')) a(f,g);
SELECT f, g FROM (VALUES ('a', 'b'),('c','d')) as a(f,g);
