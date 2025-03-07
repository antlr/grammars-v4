-- Structured UDT with 2 attributes
CREATE TYPE address AS (
    street VARCHAR(20),
    zip CHARACTER(5) )
    NOT FINAL
    CONSTRUCTOR METHOD address( VARCHAR(20), CHARACTER(5) )
        RETURNS address
        SPECIFIC address_constructor_1
        SELF AS RESULT
        LANGUAGE C
        PARAMETER STYLE SQL
        DETERMINISTIC
        NO SQL,
    INSTANCE METHOD city()
        RETURNS VARCHAR(20)
        LANGUAGE C
        PARAMETER STYLE SQL
        DETERMINISTIC
        NO SQL,
    METHOD state()
        RETURNS CHARACTER(2)
        SPECIFIC address_state
        RETURNS NULL ON NULL INPUT
        LANGUAGE C
        PARAMETER STYLE SQL
        DETERMINISTIC
        NO SQL,
    METHOD in_state(CHARACTER(2))
        RETURNS CHARACTER(1)
        LANGUAGE C
        PARAMETER STYLE SQL
        DETERMINISTIC
        NO SQL;

-- Structured UDT from predefined data
CREATE TYPE SYSUDTLIB.person AS (
    ssn CHARACTER(11),
    first_name VARCHAR(20),
    middle_name VARCHAR(20),
    last_name VARCHAR(20),
    domicile address )
    NOT FINAL;
