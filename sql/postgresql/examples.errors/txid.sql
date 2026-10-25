SELECT txid_current_if_assigned() IS NOT DISTINCT FROM BIGINT :'txid_current';

SELECT txid_current_if_assigned() IS NOT DISTINCT FROM BIGINT :'txid_current';

SELECT txid_current() AS committed \gset
SELECT txid_current() AS rolledback \gset

SELECT txid_current() AS inprogress \gset
SELECT txid_status(:committed) AS committed;
SELECT txid_status(:rolledback) AS rolledback;
SELECT txid_status(:inprogress) AS inprogress;

SELECT test_future_xid_status(:inprogress + 10000);

