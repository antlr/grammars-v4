-- Distinct UDT for Euro conversion

CREATE TYPE euro
    AS DECIMAL(8, 2)
    FINAL
    METHOD toUS()
        RETURNS us_dollar CAST FROM DECIMAL(8,2)
        LANGUAGE C
        DETERMINISTIC
        NO SQL
        RETURNS NULL ON NULL INPUT;

-- Distinct UDT for Dollar conversion
CREATE TYPE SYSUDTLIB.us_dollar
    AS DECIMAL(8, 2)
    FINAL
    METHOD toEuro()
        RETURNS euro CAST FROM DECIMAL(8,2)
        LANGUAGE C
        DETERMINISTIC
        NO SQL
        RETURNS NULL ON NULL INPUT;
