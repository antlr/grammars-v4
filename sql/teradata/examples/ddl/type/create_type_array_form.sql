-- one-dimensional array type
CREATE TYPE phonenumbers_arr AS
    CHARACTER(12) ARRAY[10]
DEFAULT NULL;

-- one-dimensional varray type
CREATE TYPE phonenumbers_arr AS
    VARRAY(10) OF CHARACTER(12);

-- three-dimensional array type
CREATE TYPE cube_arr AS
    INTEGER ARRAY [1:5][1:7][1:20]
DEFAULT NULL;

-- three-dimensional varray type with maximum size and negative lower bound
CREATE TYPE cube_arr AS
    VARYING ARRAY(1:5)(7)(-10:20) OF INTEGER
DEFAULT NULL;
