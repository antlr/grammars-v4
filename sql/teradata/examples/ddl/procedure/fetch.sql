CREATE PROCEDURE prc()
BEGIN
    FETCH cur_job INTO v_database_name;
    FETCH cur_job INTO v_database_name, v_table_name, v_job_id;
    FETCH FROM cur_job INTO v_database_name, v_table_name, v_job_id;
    FETCH FIRST FROM cur_job INTO v_database_name, v_table_name, v_job_id;
END;