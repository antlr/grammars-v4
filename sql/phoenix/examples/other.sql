/* EXPLAIN */
explain select * from t;
explain delete from t;
explain upsert into t select * from s;

/* GRANT, REVOKE */
GRANT 'R' ON SCHEMA s TO GROUP 'g';
GRANT 'R' ON t TO GROUP 'g';

revoke from 'u';
revoke from group 'u';
revoke on schema s from 'u';

/* */
