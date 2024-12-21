-- compile
ALTER TYPE address COMPILE;

-- compile only
ALTER TYPE address COMPILE ONLY;

-- add attributes to structured UDT
ALTER TYPE address
    ADD ATTRIBUTE country VARCHAR(15), continent VARCHAR(20);

-- add instance methods to structured UDT
ALTER TYPE euro
    ADD INSTANCE METHOD
        toYen()
            RETURNS yen
            LANGUAGE C
            PARAMETER STYLE SQL
            DETERMINISTIC
            NO SQL,
        toPeso()
            RETURNS peso
            LANGUAGE C
            PARAMETER STYLE SQL
            DETERMINISTIC
            NO SQL;

-- drop attribute from structured UDT
ALTER TYPE address
    DROP ATTRIBUTE country;

-- drop a method from a structured UDT using it's specific name
ALTER TYPE polygon
    DROP SPECIFIC METHOD SYSUDTLIB.polygon_mbr;
