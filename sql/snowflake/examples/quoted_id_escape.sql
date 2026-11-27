SELECT 1 AS "a""b";
SELECT "a""b", t."say ""hi""" FROM (SELECT 1 AS "a""b", 2 AS "say ""hi""") AS t;
SELECT 'a""b' AS "c""d", '"' AS "e", """" FROM (SELECT 3 AS """") AS t;
CREATE TEMPORARY TABLE "quoted ""id"" escape" ("col ""1""" INT, plain INT);
INSERT INTO "quoted ""id"" escape" ("col ""1""", plain) VALUES (1, 2);
SELECT q."col ""1""", plain FROM "quoted ""id"" escape" AS q WHERE "col ""1""" = 1;
