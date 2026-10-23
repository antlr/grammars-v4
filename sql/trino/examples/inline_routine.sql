WITH
  FUNCTION abc(x integer)
    RETURNS integer
    RETURN x * 2
SELECT abc(21);