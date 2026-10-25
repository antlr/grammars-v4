CREATE FUNCTION dummy() RETURNS VOID AS
$$
BEGIN
 RAISE NOTICE 'Calling cs_create_job(%)', v_job_id;
RAISE EXCEPTION 'Nonexistent ID --> %', user_id
      USING HINT = 'Please check your user ID';
RAISE 'Duplicate user ID: %', user_id USING ERRCODE = 'unique_violation';
RAISE 'Duplicate user ID: %', user_id USING ERRCODE = '23505';
RAISE division_by_zero;
RAISE SQLSTATE '22012';
RAISE unique_violation USING MESSAGE = 'Duplicate user ID: ' || user_id;

END;
$$
LANGUAGE plpgsql;
