CREATE TABLE patients (
  id timeuuid PRIMARY KEY,
  name text MASKED WITH mask_inner(1, null),
  birth date MASKED WITH mask_default()
);

