CREATE FUNCTION top_protected_f RETURN NUMBER
ACCESSIBLE BY (TRIGGER top_trusted_f) AS
BEGIN RETURN 0.5; END top_protected_f;
