rollback;
rollback work;
rollback to savepoint;
rollback work to savepoint;
rollback work to savepoint sp1;

savepoint sp1 UNIQUE ON ROLLBACK RETAIN CURSORS;
savepoint sp1 ON ROLLBACK RETAIN CURSORS;
savepoint sp1 ON ROLLBACK RETAIN CURSORS ON ROLLBACK RETAIN LOCKS ;

RELEASE SAVEPOINT  sp1;
RELEASE TO SAVEPOINT  sp1;
