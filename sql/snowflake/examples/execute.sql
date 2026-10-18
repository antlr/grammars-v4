EXECUTE IMMEDIATE $$
BEGIN
    select 1 as c;
    select 2 as d;
    select '$' as e;
END
$$;
