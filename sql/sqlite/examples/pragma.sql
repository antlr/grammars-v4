PRAGMA foreign_keys = ON;
PRAGMA foreign_keys = OFF;
PRAGMA main.journal_mode = DELETE;
PRAGMA temp_store = DEFAULT;
PRAGMA secure_delete(ON);
PRAGMA cache_size = -2000;
PRAGMA encoding = 'UTF-8';
PRAGMA table_info(t);
PRAGMA integrity_check;
