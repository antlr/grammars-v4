-- Creating a Named Error Table
CREATE ERROR TABLE e FOR t;

-- Creating an Error Table for a Row-Level Security Table
CREATE ERROR TABLE err_table_1 FOR table_1_RLS_constraints
NO RLS;

-- Creating an Error Table with a Default Name
CREATE ERROR TABLE FOR t;
