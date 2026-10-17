SELECT col1.f1, col2, col3.f1.f2.f3 FROM table1;
SELECT col1.f1[0], col2, col3[2].f2.f3, col4[4] FROM table1;
SELECT CAST(ROW(11, 12) AS ROW(COL0 INTEGER, COL1 INTEGER)).col0;
