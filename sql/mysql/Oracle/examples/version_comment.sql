-- Code in a /*!NNNNN ... */ comment is parsed when NNNNN is at most the server version (80200 by default)
-- and skipped when it is higher. Each statement is a syntax error if its comment is handled the other way.
SELECT /*!50000 1 */;
SELECT /*!80200 1 */, 2;
SELECT 1 /*!90000 FROM */;
