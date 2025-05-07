-- Using a Constant Expression to Specify the SPOOL Space for a Profile
MODIFY PROFILE research_and_development AS
    DEFAULT DATABASE = it_dev,
    PASSWORD = (EXPIRE = 0),
    TEMPORARY = 3000000*(HASHAMP()+1);

-- Removing UDT Transform Groups from Profile
MODIFY PROFILE DR_PROF AS TRANSFORM ();
