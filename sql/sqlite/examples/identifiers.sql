SELECT 名, 色 FROM 猫;

SELECT * FROM 本;

SELECT * FROM msg WHERE data = 'こんにちわ';

SELECT * FROM msg WHERE data = :データ;

-- VACUUM filename can be an arbitrary SQL expression that evaluates to a string (https://sqlite.org/lang_vacuum.html)
VACUUM INTO 'file.db';
