SELECT pg_current_xact_id_if_assigned() IS NOT DISTINCT FROM xid8 :'pg_current_xact_id';

SELECT pg_xact_status(:committed::text::xid8) AS committed;
SELECT pg_xact_status(:rolledback::text::xid8) AS rolledback;
SELECT pg_xact_status(:inprogress::text::xid8) AS inprogress;

SELECT test_future_xid_status((:inprogress + 10000)::text::xid8);
