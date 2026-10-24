lexer grammar TeradataSQLLexer;

options {
    caseInsensitive = true;
}

/*
    Restricted words, grouped by the category of Teradata.
    The following lists are actual as of Teradata version 17.10.
    Values are copied in the same case as in syslib.SQLRestrictedWords view.
*/
/* Teradata reserved word */
ABORT                     : 'ABORT';
ABORTSESSION              : 'ABORTSESSION';
ABS                       : 'ABS';
ACCESS_LOCK               : 'ACCESS_LOCK';
ACCOUNT                   : 'ACCOUNT';
ACOS                      : 'ACOS';
ACOSH                     : 'ACOSH';
ADD                       : 'ADD';
ADD_MONTHS                : 'ADD_MONTHS';
ADMIN                     : 'ADMIN';
AFTER                     : 'AFTER';
AGGREGATE                 : 'AGGREGATE';
ALL                       : 'ALL';
ALTER                     : 'ALTER';
AMP                       : 'AMP';
AND                       : 'AND';
ANSIDATE                  : 'ANSIDATE';
ANY                       : 'ANY';
ARGLPAREN                 : 'ARGLPAREN';
AS                        : 'AS';
ASC                       : 'ASC';
ASIN                      : 'ASIN';
ASINH                     : 'ASINH';
AT                        : 'AT';
ATAN                      : 'ATAN';
ATAN2                     : 'ATAN2';
ATANH                     : 'ATANH';
ATOMIC                    : 'ATOMIC';
AUTHORIZATION             : 'AUTHORIZATION';
AVE                       : 'AVE';
AVERAGE                   : 'AVERAGE';
AVG                       : 'AVG';
BEFORE                    : 'BEFORE';
BEGIN                     : 'BEGIN';
BETWEEN                   : 'BETWEEN';
BIGINT                    : 'BIGINT';
BINARY                    : 'BINARY';
BLOB                      : 'BLOB';
BOTH                      : 'BOTH';
BT                        : 'BT';
BUT                       : 'BUT';
BY                        : 'BY';
BYTE                      : 'BYTE';
BYTEINT                   : 'BYTEINT';
BYTES                     : 'BYTES';
CALL                      : 'CALL';
CASE                      : 'CASE';
CASE_N                    : 'CASE_N';
CASESPECIFIC              : 'CASESPECIFIC';
CAST                      : 'CAST';
CD                        : 'CD';
CHAR                      : 'CHAR';
CHAR_LENGTH               : 'CHAR_LENGTH';
CHAR2HEXINT               : 'CHAR2HEXINT';
CHARACTER                 : 'CHARACTER';
CHARACTER_LENGTH          : 'CHARACTER_LENGTH';
CHARACTERS                : 'CHARACTERS';
CHARS                     : 'CHARS';
CHECK                     : 'CHECK';
CHECKPOINT                : 'CHECKPOINT';
CLASS                     : 'CLASS';
CLOB                      : 'CLOB';
CLOSE                     : 'CLOSE';
CLUSTER                   : 'CLUSTER';
CM                        : 'CM';
COALESCE                  : 'COALESCE';
COLLATION                 : 'COLLATION';
COLLECT                   : 'COLLECT';
COLUMN                    : 'COLUMN';
COMMENT                   : 'COMMENT';
COMMIT                    : 'COMMIT';
COMPRESS                  : 'COMPRESS';
CONNECT                   : 'CONNECT';
CONSTRAINT                : 'CONSTRAINT';
CONSTRUCTOR               : 'CONSTRUCTOR';
CONSUME                   : 'CONSUME';
CONTAINS                  : 'CONTAINS';
CONTINUE                  : 'CONTINUE';
CONVERT_TABLE_HEADER      : 'CONVERT_TABLE_HEADER';
CORR                      : 'CORR';
COS                       : 'COS';
COSH                      : 'COSH';
COUNT                     : 'COUNT';
COVAR_POP                 : 'COVAR_POP';
COVAR_SAMP                : 'COVAR_SAMP';
CREATE                    : 'CREATE';
CROSS                     : 'CROSS';
CS                        : 'CS';
CSUM                      : 'CSUM';
CT                        : 'CT';
CTCONTROL                 : 'CTCONTROL';
CUBE                      : 'CUBE';
CURRENT                   : 'CURRENT';
CURRENT_DATE              : 'CURRENT_DATE';
CURRENT_ROLE              : 'CURRENT_ROLE';
CURRENT_TIME              : 'CURRENT_TIME';
CURRENT_TIMESTAMP         : 'CURRENT_TIMESTAMP';
CURRENT_USER              : 'CURRENT_USER';
CURSOR                    : 'CURSOR';
CV                        : 'CV';
CYCLE                     : 'CYCLE';
DATABASE                  : 'DATABASE';
DATABLOCKSIZE             : 'DATABLOCKSIZE';
DATE                      : 'DATE';
DATEFORM                  : 'DATEFORM';
DAY                       : 'DAY';
DEALLOCATE                : 'DEALLOCATE';
DEC                       : 'DEC';
DECIMAL                   : 'DECIMAL';
DECLARE                   : 'DECLARE';
DEFAULT                   : 'DEFAULT';
DEFERRED                  : 'DEFERRED';
DEGREES                   : 'DEGREES';
DEL                       : 'DEL';
DELETE                    : 'DELETE';
DESC                      : 'DESC';
DETERMINISTIC             : 'DETERMINISTIC';
DIAGNOSTIC                : 'DIAGNOSTIC';
DICTIONARY                : 'DICTIONARY';
DISABLED                  : 'DISABLED';
DISTINCT                  : 'DISTINCT';
DO                        : 'DO';
DOMAIN                    : 'DOMAIN';
DOUBLE                    : 'DOUBLE';
DROP                      : 'DROP';
DUAL                      : 'DUAL';
DUMP                      : 'DUMP';
DYNAMIC                   : 'DYNAMIC';
EACH                      : 'EACH';
ECHO                      : 'ECHO';
ELSE                      : 'ELSE';
ELSEIF                    : 'ELSEIF';
ENABLED                   : 'ENABLED';
END                       : 'END';
EQ                        : 'EQ';
EQUALS                    : 'EQUALS';
ERROR                     : 'ERROR';
ERRORFILES                : 'ERRORFILES';
ERRORTABLES               : 'ERRORTABLES';
ESCAPE                    : 'ESCAPE';
ET                        : 'ET';
EXCEPT                    : 'EXCEPT';
EXEC                      : 'EXEC';
EXECUTE                   : 'EXECUTE';
EXISTS                    : 'EXISTS';
EXIT                      : 'EXIT';
EXP                       : 'EXP';
EXPAND                    : 'EXPAND';
EXPANDING                 : 'EXPANDING';
EXPLAIN                   : 'EXPLAIN';
EXTERNAL                  : 'EXTERNAL';
EXTRACT                   : 'EXTRACT';
FALLBACK                  : 'FALLBACK';
FASTEXPORT                : 'FASTEXPORT';
FETCH                     : 'FETCH';
FIRST                     : 'FIRST';
FLOAT                     : 'FLOAT';
FLUSH                     : 'FLUSH';
FOR                       : 'FOR';
FOREIGN                   : 'FOREIGN';
FORMAT                    : 'FORMAT';
FOUND                     : 'FOUND';
FREESPACE                 : 'FREESPACE';
FROM                      : 'FROM';
FULL                      : 'FULL';
FUNCTION                  : 'FUNCTION';
FUNCTIONDESCRIPTOR        : 'FUNCTIONDESCRIPTOR';
GE                        : 'GE';
GENERATED                 : 'GENERATED';
GET                       : 'GET';
GIVE                      : 'GIVE';
GRANT                     : 'GRANT';
GRAPHIC                   : 'GRAPHIC';
GROUP                     : 'GROUP';
GROUPING                  : 'GROUPING';
GT                        : 'GT';
HANDLER                   : 'HANDLER';
HASH                      : 'HASH';
HASHAMP                   : 'HASHAMP';
HASHBAKAMP                : 'HASHBAKAMP';
HASHBUCKET                : 'HASHBUCKET';
HASHROW                   : 'HASHROW';
HAVING                    : 'HAVING';
HELP                      : 'HELP';
HOUR                      : 'HOUR';
ID2BIGINT                 : 'ID2BIGINT';
IDENTITY                  : 'IDENTITY';
IF                        : 'IF';
IMMEDIATE                 : 'IMMEDIATE';
IN                        : 'IN';
INCONSISTENT              : 'INCONSISTENT';
INDEX                     : 'INDEX';
INITIATE                  : 'INITIATE';
INNER                     : 'INNER';
INOUT                     : 'INOUT';
INPUT                     : 'INPUT';
INS                       : 'INS';
INSERT                    : 'INSERT';
INSTANCE                  : 'INSTANCE';
INSTEAD                   : 'INSTEAD';
INT                       : 'INT';
INTEGER                   : 'INTEGER';
INTEGERDATE               : 'INTEGERDATE';
INTERSECT                 : 'INTERSECT';
INTERVAL                  : 'INTERVAL';
INTO                      : 'INTO';
IS                        : 'IS';
ITERATE                   : 'ITERATE';
JAR                       : 'JAR';
JOIN                      : 'JOIN';
JOURNAL                   : 'JOURNAL';
KEY                       : 'KEY';
KURTOSIS                  : 'KURTOSIS';
LANGUAGE                  : 'LANGUAGE';
LARGE                     : 'LARGE';
LE                        : 'LE';
LEADING                   : 'LEADING';
LEAVE                     : 'LEAVE';
LEFT                      : 'LEFT';
LIKE                      : 'LIKE';
LIMIT                     : 'LIMIT';
LN                        : 'LN';
LOADING                   : 'LOADING';
LOCAL                     : 'LOCAL';
LOCATOR                   : 'LOCATOR';
LOCK                      : 'LOCK';
LOCKING                   : 'LOCKING';
LOG                       : 'LOG';
LOGGING                   : 'LOGGING';
LOGON                     : 'LOGON';
LONG                      : 'LONG';
LOOP                      : 'LOOP';
LOWER                     : 'LOWER';
LT                        : 'LT';
MACRO                     : 'MACRO';
MAP                       : 'MAP';
MAVG                      : 'MAVG';
MAX                       : 'MAX';
MAXIMUM                   : 'MAXIMUM';
MCHARACTERS               : 'MCHARACTERS';
MDIFF                     : 'MDIFF';
MERGE                     : 'MERGE';
METHOD                    : 'METHOD';
MIN                       : 'MIN';
MINDEX                    : 'MINDEX';
MINIMUM                   : 'MINIMUM';
MINUS                     : 'MINUS';
MINUTE                    : 'MINUTE';
MLINREG                   : 'MLINREG';
MLOAD                     : 'MLOAD';
MOD                       : 'MOD';
MODE                      : 'MODE';
MODIFIES                  : 'MODIFIES';
MODIFY                    : 'MODIFY';
MONITOR                   : 'MONITOR';
MONRESOURCE               : 'MONRESOURCE';
MONSESSION                : 'MONSESSION';
MONTH                     : 'MONTH';
MSUBSTR                   : 'MSUBSTR';
MSUM                      : 'MSUM';
MULTISET                  : 'MULTISET';
NAMED                     : 'NAMED';
NATURAL                   : 'NATURAL';
NE                        : 'NE';
NEW                       : 'NEW';
NEW_TABLE                 : 'NEW_TABLE';
NEXT                      : 'NEXT';
NO                        : 'NO';
NONE                      : 'NONE';
NONTEMPORAL               : 'NONTEMPORAL';
NORMALIZE                 : 'NORMALIZE';
NOT                       : 'NOT';
NOWAIT                    : 'NOWAIT';
NULL                      : 'NULL';
NULLIF                    : 'NULLIF';
NULLIFZERO                : 'NULLIFZERO';
NUMBER                    : 'NUMBER';
NUMERIC                   : 'NUMERIC';
OBJECT                    : 'OBJECT';
OBJECTS                   : 'OBJECTS';
OCTET_LENGTH              : 'OCTET_LENGTH';
OF                        : 'OF';
OFF                       : 'OFF';
OLD                       : 'OLD';
OLD_TABLE                 : 'OLD_TABLE';
ON                        : 'ON';
ONLY                      : 'ONLY';
OPEN                      : 'OPEN';
OPTION                    : 'OPTION';
OR                        : 'OR';
ORDER                     : 'ORDER';
ORDERING                  : 'ORDERING';
OUT                       : 'OUT';
OUTER                     : 'OUTER';
OVER                      : 'OVER';
OVERLAPS                  : 'OVERLAPS';
OVERRIDE                  : 'OVERRIDE';
PARAMETER                 : 'PARAMETER';
PASSWORD                  : 'PASSWORD';
PERCENT                   : 'PERCENT';
PERCENT_RANK              : 'PERCENT_RANK';
PERM                      : 'PERM';
PERMANENT                 : 'PERMANENT';
POSITION                  : 'POSITION';
PRECISION                 : 'PRECISION';
PREPARE                   : 'PREPARE';
PRESERVE                  : 'PRESERVE';
PRIMARY                   : 'PRIMARY';
PRIVILEGES                : 'PRIVILEGES';
PROCEDURE                 : 'PROCEDURE';
PROFILE                   : 'PROFILE';
PROTECTION                : 'PROTECTION';
PUBLIC                    : 'PUBLIC';
QUALIFIED                 : 'QUALIFIED';
QUALIFY                   : 'QUALIFY';
QUANTILE                  : 'QUANTILE';
QUEUE                     : 'QUEUE';
RADIANS                   : 'RADIANS';
RANDOM                    : 'RANDOM';
RANGE_N                   : 'RANGE_N';
RANK                      : 'RANK';
READS                     : 'READS';
REAL                      : 'REAL';
RECURSIVE                 : 'RECURSIVE';
REFERENCES                : 'REFERENCES';
REFERENCING               : 'REFERENCING';
REGR_AVGX                 : 'REGR_AVGX';
REGR_AVGY                 : 'REGR_AVGY';
REGR_COUNT                : 'REGR_COUNT';
REGR_INTERCEPT            : 'REGR_INTERCEPT';
REGR_R2                   : 'REGR_R2';
REGR_SLOPE                : 'REGR_SLOPE';
REGR_SXX                  : 'REGR_SXX';
REGR_SXY                  : 'REGR_SXY';
REGR_SYY                  : 'REGR_SYY';
RELATIVE                  : 'RELATIVE';
RELEASE                   : 'RELEASE';
RENAME                    : 'RENAME';
REPEAT                    : 'REPEAT';
REPLACE                   : 'REPLACE';
REPLCONTROL               : 'REPLCONTROL';
REPLICATION               : 'REPLICATION';
REQUEST                   : 'REQUEST';
RESIGNAL                  : 'RESIGNAL';
RESTART                   : 'RESTART';
RESTORE                   : 'RESTORE';
RESULT                    : 'RESULT';
RESUME                    : 'RESUME';
RET                       : 'RET';
RETRIEVE                  : 'RETRIEVE';
RETURN                    : 'RETURN';
RETURNS                   : 'RETURNS';
REVALIDATE                : 'REVALIDATE';
REVOKE                    : 'REVOKE';
RIGHT                     : 'RIGHT';
RIGHTS                    : 'RIGHTS';
ROLE                      : 'ROLE';
ROLLBACK                  : 'ROLLBACK';
ROLLFORWARD               : 'ROLLFORWARD';
ROLLUP                    : 'ROLLUP';
ROW                       : 'ROW';
ROW_NUMBER                : 'ROW_NUMBER';
ROWID                     : 'ROWID';
ROWS                      : 'ROWS';
SAMPLE                    : 'SAMPLE';
SAMPLEID                  : 'SAMPLEID';
SCROLL                    : 'SCROLL';
SECOND                    : 'SECOND';
SEL                       : 'SEL';
SELECT                    : 'SELECT';
SESSION                   : 'SESSION';
SET                       : 'SET';
SETRESRATE                : 'SETRESRATE';
SETS                      : 'SETS';
SETSESSRATE               : 'SETSESSRATE';
SHOW                      : 'SHOW';
SIGNAL                    : 'SIGNAL';
SIN                       : 'SIN';
SINH                      : 'SINH';
SKEW                      : 'SKEW';
SMALLINT                  : 'SMALLINT';
SOME                      : 'SOME';
SOUNDEX                   : 'SOUNDEX';
SPECIFIC                  : 'SPECIFIC';
SPOOL                     : 'SPOOL';
SQL                       : 'SQL';
SQLEXCEPTION              : 'SQLEXCEPTION';
SQLTEXT                   : 'SQLTEXT';
SQLWARNING                : 'SQLWARNING';
SQRT                      : 'SQRT';
SS                        : 'SS';
START                     : 'START';
STARTUP                   : 'STARTUP';
STATEMENT                 : 'STATEMENT';
STATISTICS                : 'STATISTICS';
STDDEV_POP                : 'STDDEV_POP';
STDDEV_SAMP               : 'STDDEV_SAMP';
STEPINFO                  : 'STEPINFO';
STRING_CS                 : 'STRING_CS';
SUBSCRIBER                : 'SUBSCRIBER';
SUBSTR                    : 'SUBSTR';
SUBSTRING                 : 'SUBSTRING';
SUM                       : 'SUM';
SUMMARY                   : 'SUMMARY';
SUSPEND                   : 'SUSPEND';
TABLE                     : 'TABLE';
TAN                       : 'TAN';
TANH                      : 'TANH';
TBL_CS                    : 'TBL_CS';
TD_ANYTYPE                : 'TD_ANYTYPE';
TD_AUTHID                 : 'TD_AUTHID';
TD_HOST                   : 'TD_HOST';
TD_ROWLOADID              : 'TD_ROWLOADID';
TD_ROWREVISION            : 'TD_ROWREVISION';
TD_ROWSIZE                : 'TD_ROWSIZE';
TD_VALIST                 : 'TD_VALIST';
TEMPORARY                 : 'TEMPORARY';
TERMINATE                 : 'TERMINATE';
THEN                      : 'THEN';
THRESHOLD                 : 'THRESHOLD';
TIME                      : 'TIME';
TIMESTAMP                 : 'TIMESTAMP';
TIMEZONE_HOUR             : 'TIMEZONE_HOUR';
TIMEZONE_MINUTE           : 'TIMEZONE_MINUTE';
TITLE                     : 'TITLE';
TO                        : 'TO';
TOP                       : 'TOP';
TRACE                     : 'TRACE';
TRAILING                  : 'TRAILING';
TRANSACTION               : 'TRANSACTION';
TRANSACTIONTIME           : 'TRANSACTIONTIME';
TRANSFORM                 : 'TRANSFORM';
TRANSLATE                 : 'TRANSLATE';
TRANSLATE_CHK             : 'TRANSLATE_CHK';
TRIGGER                   : 'TRIGGER';
TRIM                      : 'TRIM';
TYPE                      : 'TYPE';
UC                        : 'UC';
UDTCASTAS                 : 'UDTCASTAS';
UDTCASTLPAREN             : 'UDTCASTLPAREN';
UDTMETHOD                 : 'UDTMETHOD';
UDTTYPE                   : 'UDTTYPE';
UDTUSAGE                  : 'UDTUSAGE';
UESCAPE                   : 'UESCAPE';
UNDEFINED                 : 'UNDEFINED';
UNDO                      : 'UNDO';
UNION                     : 'UNION';
UNIQUE                    : 'UNIQUE';
UNTIL                     : 'UNTIL';
UNTIL_CHANGED             : 'UNTIL_CHANGED';
UNTIL_CLOSED              : 'UNTIL_CLOSED';
UPD                       : 'UPD';
UPDATE                    : 'UPDATE';
UPPER                     : 'UPPER';
UPPERCASE                 : 'UPPERCASE';
USER                      : 'USER';
USING                     : 'USING';
VALIDTIME                 : 'VALIDTIME';
VALUE                     : 'VALUE';
VALUES                    : 'VALUES';
VAR_POP                   : 'VAR_POP';
VAR_SAMP                  : 'VAR_SAMP';
VARBYTE                   : 'VARBYTE';
VARCHAR                   : 'VARCHAR';
VARGRAPHIC                : 'VARGRAPHIC';
VARIANT_TYPE              : 'VARIANT_TYPE';
VARYING                   : 'VARYING';
VIEW                      : 'VIEW';
VOLATILE                  : 'VOLATILE';
WHEN                      : 'WHEN';
WHERE                     : 'WHERE';
WHILE                     : 'WHILE';
WIDTH_BUCKET              : 'WIDTH_BUCKET';
WITH                      : 'WITH';
WITHOUT                   : 'WITHOUT';
WORK                      : 'WORK';
XMLPLAN                   : 'XMLPLAN';
YEAR                      : 'YEAR';
ZEROIFNULL                : 'ZEROIFNULL';
ZONE                      : 'ZONE';

/* Teradata future reserved words */
ALIAS                     : 'ALIAS';
DESCRIPTOR                : 'DESCRIPTOR';
GO                        : 'GO';
GOTO                      : 'GOTO';
INDICATOR                 : 'INDICATOR';
PRIVATE                   : 'PRIVATE';
WAIT                      : 'WAIT';

/* Nonreserved words */
ABORTSESSIONS             : 'AbortSessions';
ABSENT                    : 'ABSENT';
ACCESS                    : 'ACCESS';
ACCORDING                 : 'ACCORDING';
ACCUMULATE                : 'ACCUMULATE';
AG                        : 'AG';
AGGGEOMINTERSECTION       : 'AggGeomIntersection';
AGGGEOMUNION              : 'AggGeomUnion';
ALLDBQL                   : 'ALLDBQL';
ALLOCATE                  : 'ALLOCATE';
ALLOCATION                : 'ALLOCATION';
ALLOW                     : 'ALLOW';
ALLPARAMS                 : 'ALLPARAMS';
ALLTDWM                   : 'ALLTDWM';
ALWAYS                    : 'ALWAYS';
AMPCOUNT                  : 'AMPCOUNT';
ANALYSIS                  : 'ANALYSIS';
ANCHOR                    : 'ANCHOR';
ANCHOR_HOUR               : 'ANCHOR_HOUR';
ANCHOR_MILLISECOND        : 'ANCHOR_MILLISECOND';
ANCHOR_MINUTE             : 'ANCHOR_MINUTE';
ANCHOR_SECOND             : 'ANCHOR_SECOND';
APPLNAME                  : 'APPLNAME';
ARCHIVE                   : 'ARCHIVE';
ARRAY                     : 'ARRAY';
ARRAY_ADD                 : 'ARRAY_ADD';
ARRAY_AGG                 : 'ARRAY_AGG';
ARRAY_AVG                 : 'ARRAY_AVG';
ARRAY_COMPARE             : 'ARRAY_COMPARE';
ARRAY_CONCAT              : 'ARRAY_CONCAT';
ARRAY_COUNT_DISTINCT      : 'ARRAY_COUNT_DISTINCT';
ARRAY_DIV                 : 'ARRAY_DIV';
ARRAY_EQ                  : 'ARRAY_EQ';
ARRAY_GE                  : 'ARRAY_GE';
ARRAY_GET                 : 'ARRAY_GET';
ARRAY_GT                  : 'ARRAY_GT';
ARRAY_LE                  : 'ARRAY_LE';
ARRAY_LT                  : 'ARRAY_LT';
ARRAY_MAX                 : 'ARRAY_MAX';
ARRAY_MIN                 : 'ARRAY_MIN';
ARRAY_MOD                 : 'ARRAY_MOD';
ARRAY_MUL                 : 'ARRAY_MUL';
ARRAY_NE                  : 'ARRAY_NE';
ARRAY_SUB                 : 'ARRAY_SUB';
ARRAY_SUM                 : 'ARRAY_SUM';
ARRAY_UPDATE              : 'ARRAY_UPDATE';
ARRAY_UPDATE_STRIDE       : 'ARRAY_UPDATE_STRIDE';
ASCII                     : 'ASCII';
ASSIGNMENT                : 'ASSIGNMENT';
ATTR                      : 'ATTR';
ATTRIBUTE                 : 'ATTRIBUTE';
ATTRIBUTES                : 'ATTRIBUTES';
ATTRIBUTION               : 'ATTRIBUTION';
ATTRS                     : 'ATTRS';
AUTH                      : 'AUTH';
AUTO                      : 'AUTO';
AUTOTEMP                  : 'AUTOTEMP';
AVRO                      : 'AVRO';
BIT_LENGTH                : 'BIT_LENGTH';
BITAND                    : 'BITAND';
BITNOT                    : 'BITNOT';
BITOR                     : 'BITOR';
BITXOR                    : 'BITXOR';
BLOCKCOMPRESSION          : 'BLOCKCOMPRESSION';
BLOCKCOMPRESSIONALGORITHM : 'BLOCKCOMPRESSIONALGORITHM';
BLOCKCOMPRESSIONLEVEL     : 'BLOCKCOMPRESSIONLEVEL';
BOM                       : 'BOM';
BOTTOM                    : 'BOTTOM';
BSON                      : 'BSON';
C                         : 'C';
CALENDAR                  : 'CALENDAR';
CALLED                    : 'CALLED';
CALLER                    : 'CALLER';
CAMSET                    : 'camset';
CAMSET_L                  : 'camset_l';
CAPTURE                   : 'CAPTURE';
CARDINALITY               : 'CARDINALITY';
CEIL                      : 'CEIL';
CEILING                   : 'CEILING';
CHANGERATE                : 'CHANGERATE';
CHARACTERISTICS           : 'CHARACTERISTICS';
CHARSET                   : 'CHARSET';
CHARSET_COLL              : 'CHARSET_COLL';
CHECKSUM                  : 'CHECKSUM';
CHR                       : 'CHR';
CLASS_ORIGIN              : 'CLASS_ORIGIN';
CLICKLAG                  : 'CLICKLAG';
CLIENT                    : 'CLIENT';
CNT                       : 'CNT';
COLOCATE                  : 'COLOCATE';
COLUMNMETA                : 'COLUMNMETA';
COLUMNS                   : 'COLUMNS';
COLUMNSPERINDEX           : 'COLUMNSPERINDEX';
COLUMNSPERJOININDEX       : 'COLUMNSPERJOININDEX';
COMMAND_FUNCTION          : 'COMMAND_FUNCTION';
COMMAND_FUNCTION_CODE     : 'COMMAND_FUNCTION_CODE';
COMPARISON                : 'COMPARISON';
COMPILE                   : 'COMPILE';
CONCAT                    : 'CONCAT';
CONCURRENT                : 'CONCURRENT';
CONDITION                 : 'CONDITION';
CONDITION_IDENTIFIER      : 'CONDITION_IDENTIFIER';
CONDITION_NUMBER          : 'CONDITION_NUMBER';
CONTAINED                 : 'CONTAINED';
CONTAINEDTOKEN            : 'CONTAINEDTOKEN';
CONTENT                   : 'CONTENT';
CONTIGUOUS                : 'CONTIGUOUS';
COST                      : 'COST';
COSTS                     : 'COSTS';
COUNTSET                  : 'COUNTSET';
CPP                       : 'CPP';
CPUTIME                   : 'CPUTIME';
CPUTIMENORM               : 'CPUTIMENORM';
CREATEDATASET             : 'CREATEDATASET';
CREATOR                   : 'CREATOR';
CUME_DIST                 : 'CUME_DIST';
CURDATE                   : 'CURDATE';
CURTIME                   : 'CURTIME';
DATA                      : 'DATA';
DATASET                   : 'DATASET';
DAY_OF_CALENDAR           : 'day_of_calendar';
DAY_OF_MONTH              : 'day_of_month';
DAY_OF_WEEK               : 'day_of_week';
DAY_OF_YEAR               : 'day_of_year';
DAYNUMBER_OF_CALENDAR     : 'DayNumber_Of_Calendar';
DAYNUMBER_OF_MONTH        : 'DayNumber_Of_Month';
DAYNUMBER_OF_WEEK         : 'DayNumber_Of_Week';
DAYNUMBER_OF_YEAR         : 'DayNumber_Of_Year';
DAYOCCURRENCE_OF_MONTH    : 'DayOccurrence_Of_Month';
DBA                       : 'DBA';
DBC                       : 'DBC';
DEBUG                     : 'DEBUG';
DECAMSET                  : 'decamset';
DECAMSET_L                : 'decamset_l';
DECODE                    : 'DECODE';
DECOMPRESS                : 'DECOMPRESS';
DEFINER                   : 'DEFINER';
DELIMITER                 : 'DELIMITER';
DELTA_T                   : 'DELTA_T';
DEMOGRAPHICS              : 'DEMOGRAPHICS';
DENIALS                   : 'DENIALS';
DENSE                     : 'DENSE';
DENSE_RANK                : 'DENSE_RANK';
DESCRIBE                  : 'DESCRIBE';
DETAILED                  : 'DETAILED';
DIAGNOSTICS               : 'DIAGNOSTICS';
DIGITS                    : 'DIGITS';
DIMENSION                 : 'DIMENSION';
DOCUMENT                  : 'DOCUMENT';
DOT                       : 'DOT';
DOWN                      : 'DOWN';
DR                        : 'DR';
DUPCOUNT                  : 'DUPCOUNT';
DUPCOUNTCUM               : 'DUPCOUNTCUM';
EBCDIC                    : 'EBCDIC';
EDITDISTANCE              : 'EDITDISTANCE';
ELAPSEDSEC                : 'ELAPSEDSEC';
ELAPSEDTIME               : 'ELAPSEDTIME';
ELEMENT                   : 'ELEMENT';
ELZS_H                    : 'ELZS_H';
EMITNULL                  : 'EMITNULL';
EMPTY                     : 'EMPTY';
EMPTY_BLOB                : 'EMPTY_BLOB';
EMPTY_CLOB                : 'EMPTY_CLOB';
ENCODE                    : 'ENCODE';
ENCODING                  : 'ENCODING';
ENCRYPT                   : 'ENCRYPT';
ERRORS                    : 'ERRORS';
ERRORTBL                  : 'ERRORTBL';
EVENTCOLUMN               : 'EVENTCOLUMN';
EXCEPTION                 : 'EXCEPTION';
EXCL                      : 'EXCL';
EXCLUDE                   : 'EXCLUDE';
EXCLUDING                 : 'EXCLUDING';
EXCLUSIVE                 : 'EXCLUSIVE';
EXPIRE                    : 'EXPIRE';
EXPORT                    : 'EXPORT';
EXPORTWIDTH               : 'EXPORTWIDTH';
FALSE                     : 'FALSE';
FEATUREINFO               : 'FEATUREINFO';
FILE                      : 'FILE';
FILL                      : 'FILL';
FILTER                    : 'FILTER';
FINAL                     : 'FINAL';
FIRST_NOTNULL             : 'FIRST_NOTNULL';
FIRST_VALUE               : 'FIRST_VALUE';
FLOOR                     : 'FLOOR';
FOLLOWING                 : 'FOLLOWING';
FOREIGNFUNCTION           : 'FOREIGNFUNCTION';
FORTOKEN                  : 'FORTOKEN';
FRIDAY                    : 'FRIDAY';
FROM_BYTES                : 'FROM_BYTES';
FUNCTIONPARAMETER         : 'FUNCTIONPARAMETER';
G                         : 'G';
GETBIT                    : 'GETBIT';
GETPSFVERSION             : 'GetPSFVersion';
GETQUERYBAND              : 'GetQueryBand';
GETQUERYBANDVALUE         : 'GetQueryBandValue';
GETTIMEZONEDISPLACEMENT   : 'GetTimeZoneDisplacement';
GLOBAL                    : 'GLOBAL';
GLOP                      : 'GLOP';
GREATEST                  : 'Greatest';
HIGH                      : 'HIGH';
HOST                      : 'HOST';
IDENTIFYDATABASE          : 'IdentifyDatabase';
IDENTIFYSESSION           : 'IdentifySession';
IDENTIFYTABLE             : 'IdentifyTable';
IDENTIFYUSER              : 'IdentifyUser';
IFP                       : 'IFP';
IGNORE                    : 'IGNORE';
IMMEDIATELY               : 'IMMEDIATELY';
IMPORT                    : 'IMPORT';
INCLUDE                   : 'INCLUDE';
INCLUDING                 : 'INCLUDING';
INCREMENT                 : 'INCREMENT';
INCREMENTAL               : 'INCREMENTAL';
INDENT                    : 'INDENT';
INDEXESPERTABLE           : 'INDEXESPERTABLE';
INDEXMAINTMODE            : 'INDEXMAINTMODE';
INIT                      : 'INIT';
INITCAP                   : 'INITCAP';
INLINE                    : 'INLINE';
INSTANTIABLE              : 'INSTANTIABLE';
INSTR                     : 'INSTR';
INTERNAL                  : 'INTERNAL';
INVOKER                   : 'INVOKER';
IOCOUNT                   : 'IOCOUNT';
IPARTITION                : 'IPARTITION';
ISOLATED                  : 'ISOLATED';
ISOLATION                 : 'ISOLATION';
JAVA                      : 'JAVA';
JIS_COLL                  : 'JIS_COLL';
JSON                      : 'JSON';
JSON_AGG                  : 'JSON_AGG';
JSON_COMPOSE              : 'JSON_COMPOSE';
K                         : 'K';
KANJI1                    : 'KANJI1';
KANJISJIS                 : 'KANJISJIS';
KBYTE                     : 'KBYTE';
KBYTES                    : 'KBYTES';
KEEP                      : 'KEEP';
KILOBYTES                 : 'KILOBYTES';
LAG                       : 'LAG';
LAST                      : 'LAST';
LAST_DAY                  : 'Last_Day';
LAST_NOTNULL              : 'LAST_NOTNULL';
LAST_VALUE                : 'LAST_VALUE';
LATIN                     : 'LATIN';
LDIFF                     : 'LDIFF';
LEAD                      : 'LEAD';
LEAST                     : 'Least';
LENGTH                    : 'LENGTH';
LEVEL                     : 'LEVEL';
LIST                      : 'LIST';
LOAD                      : 'LOAD';
LOCATE                    : 'LOCATE';
LOCKEDUSEREXPIRE          : 'LOCKEDUSEREXPIRE';
LOW                       : 'LOW';
LPAD                      : 'LPAD';
LTRIM                     : 'LTRIM';
LZCOMP                    : 'lzcomp';
LZCOMP_L                  : 'lzcomp_L';
LZDECOMP                  : 'lzdecomp';
LZDECOMP_L                : 'lzdecomp_L';
M                         : 'M';
MAD                       : 'MAD';
MANUAL                    : 'MANUAL';
MAPPING                   : 'MAPPING';
MATCHED                   : 'MATCHED';
MAX_CHOOSE                : 'MAX_CHOOSE';
MAXCHAR                   : 'MAXCHAR';
MAXINTERVALS              : 'MAXINTERVALS';
MAXLOGONATTEMPTS          : 'MAXLOGONATTEMPTS';
MAXVALUE                  : 'MAXVALUE';
MAXVALUELENGTH            : 'MAXVALUELENGTH';
MEDIAN                    : 'MEDIAN';
MEDIUM                    : 'MEDIUM';
MEETS                     : 'MEETS';
MEMBER                    : 'MEMBER';
MERGEBLOCKRATIO           : 'MERGEBLOCKRATIO';
MESSAGE_LENGTH            : 'MESSAGE_LENGTH';
MESSAGE_TEXT              : 'MESSAGE_TEXT';
MIN_CHOOSE                : 'MIN_CHOOSE';
MINCHAR                   : 'MINCHAR';
MINVALUE                  : 'MINVALUE';
MODIFIED                  : 'MODIFIED';
MONDAY                    : 'MONDAY';
MONITORQUERYBAND          : 'MonitorQueryBand';
MONITORSESSIONRATE        : 'MonitorSessionRate';
MONITORVERSION            : 'MonitorVersion';
MONTH_BEGIN               : 'MONTH_BEGIN';
MONTH_END                 : 'MONTH_END';
MONTH_OF_CALENDAR         : 'month_of_calendar';
MONTH_OF_QUARTER          : 'month_of_quarter';
MONTH_OF_YEAR             : 'month_of_year';
MONTHNUMBER_OF_CALENDAR   : 'MonthNumber_Of_Calendar';
MONTHNUMBER_OF_QUARTER    : 'MonthNumber_Of_Quarter';
MONTHNUMBER_OF_YEAR       : 'MonthNumber_Of_Year';
MONTHS_BETWEEN            : 'Months_Between';
MORE_                     : 'MORE';
MULTINATIONAL             : 'MULTINATIONAL';
NAME                      : 'NAME';
NAMESPACE                 : 'NAMESPACE';
NEVER                     : 'NEVER';
NEXT_DAY                  : 'NEXT_DAY';
NGRAM                     : 'NGRAM';
NIL                       : 'NIL';
NODDLTEXT                 : 'NODDLTEXT';
NODE                      : 'NODE';
NONOPTCOST                : 'NONOPTCOST';
NONOPTINIT                : 'NONOPTINIT';
NONSEQUENCED              : 'NONSEQUENCED';
NORIGHT                   : 'NORIGHT';
NOSEXTRACTVARFROMPATH     : 'NOSEXTRACTVARFROMPATH';
NOTATION                  : 'NOTATION';
NOW                       : 'NOW';
NPATH                     : 'NPATH';
NTH                       : 'NTH';
NULLS                     : 'NULLS';
NUMFPFNS                  : 'NUMFPFNS';
NUMTODSINTERVAL           : 'NUMTODSINTERVAL';
NUMTOYMINTERVAL           : 'NUMTOYMINTERVAL';
NVL                       : 'nvl';
NVL2                      : 'nvl2';
NVP                       : 'NVP';
OA                        : 'OA';
OADD_MONTHS               : 'OAdd_Months';
OCOUNT                    : 'OCOUNT';
ODELETE                   : 'ODELETE';
OEXISTS                   : 'OEXISTS';
OEXTEND                   : 'OEXTEND';
OFIRST                    : 'OFIRST';
OLAST                     : 'OLAST';
OLD_NEW_TABLE             : 'OLD_NEW_TABLE';
OLIMIT                    : 'OLIMIT';
ONEXT                     : 'ONEXT';
ONLINE                    : 'ONLINE';
OPRIOR                    : 'OPRIOR';
OPTIONS                   : 'OPTIONS';
ORDERBYVALUES             : 'ORDERBYVALUES';
ORDERED_ANALYTIC          : 'ORDERED_ANALYTIC';
ORDINALITY                : 'ORDINALITY';
OREPLACE                  : 'OREPLACE';
OTRANSLATE                : 'OTRANSLATE';
OTRIM                     : 'OTRIM';
OVERLAYS                  : 'OVERLAYS';
OWNER                     : 'OWNER';
P_INTERSECT               : 'P_INTERSECT';
P_NORMALIZE               : 'P_NORMALIZE';
PARAMID                   : 'PARAMID';
PARAMINFO                 : 'PARAMINFO';
PARENT                    : 'PARENT';
PARTITION                 : 'PARTITION';
PARTITION_L               : PARTITION '#L' DIGIT DIGIT?;
PARTITIONED               : 'PARTITIONED';
PARTITIONNAMES            : 'PARTITIONNAMES';
PASS                      : 'PASS';
PASSING                   : 'PASSING';
PATH_GENERATOR            : 'PATH_GENERATOR';
PATH_START                : 'PATH_START';
PATH_SUMMARIZER           : 'PATH_SUMMARIZER';
PATTERN                   : 'PATTERN';
PERCENTILE                : 'PERCENTILE';
PERCENTILE_CONT           : 'PERCENTILE_CONT';
PERCENTILE_DISC           : 'PERCENTILE_DISC';
PERIOD                    : 'PERIOD';
PIVOT                     : 'PIVOT';
PORTION                   : 'PORTION';
POWER                     : 'POWER';
PRECEDES                  : 'PRECEDES';
PRECEDING                 : 'PRECEDING';
PREFIX                    : 'PREFIX';
PRINT                     : 'PRINT';
PRIOR                     : 'PRIOR';
PROTECTED                 : 'PROTECTED';
QUARTER_BEGIN             : 'QUARTER_BEGIN';
QUARTER_END               : 'QUARTER_END';
QUARTER_OF_CALENDAR       : 'quarter_of_calendar';
QUARTER_OF_YEAR           : 'quarter_of_year';
QUARTERNUMBER_OF_CALENDAR : 'QuarterNumber_Of_Calendar';
QUARTERNUMBER_OF_YEAR     : 'QuarterNumber_Of_Year';
QUERY                     : 'QUERY';
QUERY_BAND                : 'QUERY_BAND';
QUOTECHAR                 : 'QUOTECHAR';
RANDOMIZED                : 'RANDOMIZED';
RANGE                     : 'RANGE';
RANGE_L                   : RANGE '#L' DIGIT DIGIT?;
RAPIDFIRE                 : 'RAPIDFIRE';
RDIFF                     : 'RDIFF';
READ                      : 'READ';
RECALC                    : 'RECALC';
REGEXP_INSTR              : 'regexp_instr';
REGEXP_REPLACE            : 'regexp_replace';
REGEXP_SIMILAR            : 'regexp_similar';
REGEXP_SUBSTR             : 'regexp_substr';
REPLACEMENT               : 'REPLACEMENT';
RESET                     : 'RESET';
RESPECT                   : 'RESPECT';
RESTRICTWORDS             : 'RESTRICTWORDS';
RETAIN                    : 'RETAIN';
RETURNED_SQLSTATE         : 'RETURNED_SQLSTATE';
RETURNING                 : 'RETURNING';
REUSE                     : 'REUSE';
ROOT                      : 'ROOT';
ROTATELEFT                : 'ROTATELEFT';
ROTATERIGHT               : 'ROTATERIGHT';
ROUND                     : 'Round';
ROW_COUNT                 : 'ROW_COUNT';
ROWIDGEN                  : 'ROWIDGEN';
ROWIDGEN2                 : 'ROWIDGEN2';
RPAD                      : 'RPAD';
RTRIM                     : 'RTRIM';
RU                        : 'RU';
RULES                     : 'RULES';
RULESET                   : 'RULESET';
SAMPLES                   : 'SAMPLES';
SATURDAY                  : 'SATURDAY';
SCHEMA                    : 'SCHEMA';
SCRIPT                    : 'SCRIPT';
SCRIPT_COMMAND            : 'SCRIPT_COMMAND';
SEARCHSPACE               : 'SEARCHSPACE';
SEARCHUIFDBPATH           : 'SEARCHUIFDBPATH';
SECURITY                  : 'SECURITY';
SEED                      : 'SEED';
SELF                      : 'SELF';
SEQ                       : 'SEQ';
SEQUENCE                  : 'SEQUENCE';
SEQUENCED                 : 'SEQUENCED';
SERIALIZABLE              : 'SERIALIZABLE';
SERVER                    : 'SERVER';
SESSIONIZE                : 'SESSIONIZE';
SETBIT                    : 'SETBIT';
SETRESOURCERATE           : 'SetResourceRate';
SETSESSIONACCOUNT         : 'SetSessionAccount';
SETSESSIONRATE            : 'SetSessionRate';
SHARE                     : 'SHARE';
SHIFTLEFT                 : 'SHIFTLEFT';
SHIFTRIGHT                : 'SHIFTRIGHT';
SIGN                      : 'SIGN';
SIZE                      : 'SIZE';
SNAPPY_COMPRESS           : 'SNAPPY_COMPRESS';
SNAPPY_DECOMPRESS         : 'SNAPPY_DECOMPRESS';
SOURCE                    : 'SOURCE';
SPARSE                    : 'SPARSE';
SPECCHAR                  : 'SPECCHAR';
SPL                       : 'SPL';
SQLSTATE                  : 'SQLSTATE';
SR                        : 'SR';
ST_GEOMETRY               : 'ST_GEOMETRY';
STAT                      : 'STAT';
STATIC                    : 'STATIC';
STATS                     : 'STATS';
STATSUSAGE                : 'STATSUSAGE';
STORAGE                   : 'STORAGE';
STRIP                     : 'STRIP';
STRTOK                    : 'STRTOK';
STYLE                     : 'STYLE';
SUBBITSTR                 : 'SUBBITSTR';
SUBCLASS_ORIGIN           : 'SUBCLASS_ORIGIN';
SUCCEEDS                  : 'SUCCEEDS';
SUMMARYONLY               : 'SUMMARYONLY';
SUNDAY                    : 'SUNDAY';
SYMBOLS                   : 'SYMBOLS';
SYSTEM                    : 'SYSTEM';
SYSTEM_TIME               : 'SYSTEM_TIME';
SYSTEMTEST                : 'SYSTEMTEST';
TARGET                    : 'TARGET';
TD_ARRAY2P                : 'TD_ARRAY2P';
TD_DATASET                : 'TD_DATASET';
TD_DAY_OF_CALENDAR        : 'td_day_of_calendar';
TD_DAY_OF_MONTH           : 'td_day_of_month';
TD_DAY_OF_WEEK            : 'td_day_of_week';
TD_DAY_OF_YEAR            : 'td_day_of_year';
TD_GENERAL                : 'TD_GENERAL';
TD_GETTIMEBUCKET          : 'TD_GETTIMEBUCKET';
TD_INTERNAL               : 'TD_INTERNAL';
TD_LZ_COMPRESS            : 'TD_LZ_COMPRESS';
TD_LZ_DECOMPRESS          : 'TD_LZ_DECOMPRESS';
TD_MONTH_OF_CALENDAR      : 'td_month_of_calendar';
TD_MONTH_OF_QUARTER       : 'td_month_of_quarter';
TD_MONTH_OF_YEAR          : 'td_month_of_year';
TD_QUARTER_OF_CALENDAR    : 'td_quarter_of_calendar';
TD_QUARTER_OF_YEAR        : 'td_quarter_of_year';
TD_TIME_BUCKET_NUMBER     : 'TD_TIME_BUCKET_NUMBER';
TD_WEEK_OF_CALENDAR       : 'td_week_of_calendar';
TD_WEEK_OF_MONTH          : 'td_week_of_month';
TD_WEEK_OF_YEAR           : 'td_week_of_year';
TD_WEEKDAY_OF_MONTH       : 'td_weekday_of_month';
TD_YEAR_OF_CALENDAR       : 'td_year_of_calendar';
TDWMEVENT                 : 'TDWMEVENT';
TDWMEXCEPTION             : 'TDWMEXCEPTION';
TDWMHISTORY               : 'TDWMHISTORY';
TEMPORAL_DATE             : 'TEMPORAL_DATE';
TEMPORAL_TIMESTAMP        : 'TEMPORAL_TIMESTAMP';
TEXT                      : 'TEXT';
THRESHOLDPERCENT          : 'THRESHOLDPERCENT';
THROUGH                   : 'THROUGH';
THURSDAY                  : 'THURSDAY';
TIES                      : 'TIES';
TIMECODE                  : 'TIMECODE';
TIMECOLUMN                : 'TIMECOLUMN';
TIMEOUT                   : 'TIMEOUT';
TIMESTAMPCOLUMN           : 'TIMESTAMPCOLUMN';
TO_BYTE                   : 'TO_BYTE';
TO_BYTES                  : 'TO_BYTES';
TO_CHAR                   : 'TO_CHAR';
TO_DATE                   : 'TO_DATE';
TO_DSINTERVAL             : 'TO_DSINTERVAL';
TO_NUMBER                 : 'TO_NUMBER';
TO_TIMESTAMP              : 'TO_TIMESTAMP';
TO_TIMESTAMP_TZ           : 'TO_TIMESTAMP_TZ';
TO_YMINTERVAL             : 'TO_YMINTERVAL';
TOTOKEN                   : 'TOTOKEN';
TPA                       : 'TPA';
TRANSACTION_ACTIVE        : 'TRANSACTION_ACTIVE';
TRANSUNICODETOUTF8        : 'TransUnicodeToUTF8';
TRANSUTF8TOUNICODE        : 'TransUTF8ToUnicode';
TRUE                      : 'TRUE';
TRUNC                     : 'Trunc';
TRUST_ONLY                : 'TRUST_ONLY';
TTGRANULARITY             : 'TTGRANULARITY';
TUESDAY                   : 'TUESDAY';
UBJSON                    : 'UBJSON';
UCASE                     : 'UCASE';
UDFSEARCHPATH             : 'UDFSEARCHPATH';
UNBOUNDED                 : 'UNBOUNDED';
UNCOMMITTED               : 'UNCOMMITTED';
UNICODE                   : 'UNICODE';
UNKNOWN                   : 'UNKNOWN';
UNPIVOT                   : 'UNPIVOT';
USE                       : 'USE';
USECOUNT                  : 'USECOUNT';
UTILITYINFO               : 'UTILITYINFO';
VARRAY                    : 'VARRAY';
VERBOSE                   : 'VERBOSE';
VERSION                   : 'VERSION';
VERSIONING                : 'VERSIONING';
WARNING                   : 'WARNING';
WEDNESDAY                 : 'WEDNESDAY';
WEEK_BEGIN                : 'WEEK_BEGIN';
WEEK_END                  : 'WEEK_END';
WEEK_OF_CALENDAR          : 'week_of_calendar';
WEEK_OF_MONTH             : 'week_of_month';
WEEK_OF_YEAR              : 'week_of_year';
WEEKDAY_OF_MONTH          : 'weekday_of_month';
WEEKNUMBER_OF_CALENDAR    : 'WeekNumber_Of_Calendar';
WEEKNUMBER_OF_MONTH       : 'WeekNumber_Of_Month';
WEEKNUMBER_OF_QUARTER     : 'WeekNumber_Of_Quarter';
WEEKNUMBER_OF_YEAR        : 'WeekNumber_Of_Year';
WHITESPACE                : 'WHITESPACE';
WINDOWSIZE                : 'WINDOWSIZE';
WITHIN                    : 'WITHIN';
WORKLOAD                  : 'WORKLOAD';
WRITE                     : 'WRITE';
XML                       : 'XML';
XMLAGG                    : 'XMLAGG';
XMLATTRIBUTES             : 'XMLATTRIBUTES';
XMLCOMMENT                : 'XMLCOMMENT';
XMLCONCAT                 : 'XMLCONCAT';
XMLDECLARATION            : 'XMLDECLARATION';
XMLDOCUMENT               : 'XMLDOCUMENT';
XMLELEMENT                : 'XMLELEMENT';
XMLFOREST                 : 'XMLFOREST';
XMLNAMESPACES             : 'XMLNAMESPACES';
XMLPARSE                  : 'XMLPARSE';
XMLPI                     : 'XMLPI';
XMLQUERY                  : 'XMLQUERY';
XMLSCHEMA                 : 'XMLSCHEMA';
XMLSERIALIZE              : 'XMLSERIALIZE';
XMLTABLE                  : 'XMLTABLE';
XMLTEXT                   : 'XMLTEXT';
XMLTYPE                   : 'XMLTYPE';
XMLVALIDATE               : 'XMLVALIDATE';
YEAR_BEGIN                : 'YEAR_BEGIN';
YEAR_END                  : 'YEAR_END';
YEAR_OF_CALENDAR          : 'year_of_calendar';
YEARNUMBER_OF_CALENDAR    : 'YearNumber_Of_Calendar';
ZLIB                      : 'ZLIB';

/* Extra nonreserved words not listed in SQLRestrictedWords
   Don't forget to add to nonreserved_word rule!
*/
BUCKET                  : 'BUCKET';
COMMITTED               : 'COMMITTED';
CREATEXML               : 'CREATEXML';
CS_LATIN                : '_LATIN';
CS_UNICODE              : '_UNICODE';
CS_KANJISJIS            : '_KANJISJIS';
CS_GRAPHIC              : '_GRAPHIC';
CSV                     : 'CSV';
CSVLD                   : 'CSVLD';
DATASIZE                : 'DATASIZE';
DAYOFMONTH              : 'DAYOFMONTH';
DAYS                    : 'DAYS';
DEFINITION              : 'DEFINITION';
DELETED                 : 'DELETED';
FAST                    : 'FAST';
LISTAGG                 : 'LISTAGG';
PATH                    : 'PATH';
REGEXP_SPLIT_TO_TABLE   : 'REGEXP_SPLIT_TO_TABLE';
REVERSE                 : 'REVERSE';
SAS                     : 'SAS';
SQLTABLE                : 'SQLTABLE';
STRTOK_SPLIT_TO_TABLE   : 'STRTOK_SPLIT_TO_TABLE';
SYSLIB                  : 'SYSLIB';
SYSUDTLIB               : 'SYSUDTLIB';
TD_SERVER_DB            : 'TD_SERVER_DB';
TD_SYSFNLIB             : 'TD_SYSFNLIB';
TD_SYSXML               : 'TD_SYSXML';
TIMEDATEWZCONTROL       : 'TIMEDATEWZCONTROL';
TRUST                   : 'TRUST';
TRYCAST                 : 'TRYCAST';
UDT                     : 'UDT';
USAGE                   : 'USAGE';
VARIANT                 : 'VARIANT';
WEEK                    : 'WEEK';
WIDTH                   : 'WIDTH';
XMLPUBLISH              : 'XMLPUBLISH';
XMLPUBLISH_STREAM       : 'XMLPUBLISH_STREAM';
XMLSPLIT                : 'XMLSPLIT';
// translation mappings
LATIN_TO_UNICODE        : 'LATIN_TO_UNICODE';
UNICODE_TO_LATIN        : 'UNICODE_TO_LATIN';
LOCALE_TO_UNICODE       : 'LOCALE_TO_UNICODE';
UNICODE_TO_LOCALE       : 'UNICODE_TO_LOCALE';
// Json methods
ASBSON                  : 'ASBSON';
ASBSONTEXT              : 'ASBSONTEXT';
COMBINE                 : 'COMBINE';
EXISTVALUE              : 'EXISTVALUE';
JSONEXTRACT             : 'JSONEXTRACT';
JSONEXTRACTVALUE        : 'JSONEXTRACTVALUE';
JSONEXTRACTLARGEVALUE   : 'JSONEXTRACTLARGEVALUE';
KEYCOUNT                : 'KEYCOUNT';
METADATA                : 'METADATA';
STORAGE_SIZE            : 'STORAGE_SIZE';
// XML methods
CREATESCHEMABASEDXML    : 'CREATESCHEMABASEDXML';
CREATENONSCHEMABASEDXML : 'CREATENONSCHEMABASEDXML';
EXISTSNODE              : 'EXISTSNODE';
ISCONTENT               : 'ISCONTENT';
ISDOCUMENT              : 'ISDOCUMENT';
ISSCHEMAVALID           : 'ISSCHEMAVALID';
ISSCHEMAVALIDATED       : 'ISSCHEMAVALIDATED';
XMLEXTRACT              : 'XMLEXTRACT';
XMLTRANSFORM            : 'XMLTRANSFORM';
// found in CREATE GLOBAL TEMPORARY TRACE TABLE
PROC_ID                 : 'PROC_ID';
// found in CREATE FOREIGN TABLE
LOCATION                : 'LOCATION';
PAYLOAD                 : 'PAYLOAD';
TRUSTED                 : 'TRUSTED';
PATHPATTERN             : 'PATHPATTERN';
MANIFEST                : 'MANIFEST';
ROWFORMAT               : 'ROWFORMAT';
STOREDAS                : 'STOREDAS';
HEADER                  : 'HEADER';
STRIP_EXTERIOR_SPACES   : 'STRIP_EXTERIOR_SPACES';
STRIP_ENCLOSING_CHAR    : 'STRIP_ENCLOSING_CHAR';
// found in CREATE ERROR TABLE
RLS                     : 'RLS';
// found in Load Isolation statements
SINGLE                  : 'SINGLE';
MULTIPLE                : 'MULTIPLE';
// found in compress/decompress functions
JSON_COMPRESS           : 'JSON_COMPRESS';
JSON_DECOMPRESS         : 'JSON_DECOMPRESS';
TS_COMPRESS             : 'TS_COMPRESS';
TS_DECOMPRESS           : 'TS_DECOMPRESS';
// found in map functions
CONTIGUOUSMAPAMPS       : 'CONTIGUOUSMAPAMPS';
SPARSEMAPAMPS           : 'SPARSEMAPAMPS';
SPARSETABLEAMPS         : 'SPARSETABLEAMPS';
// found in array functions
UNNEST                  : 'UNNEST';
// found in table operators
CALCMATRIX              : 'CALCMATRIX';
PHRASE                  : 'PHRASE';
CALCTYPE                : 'CALCTYPE';
OUTPUT                  : 'OUTPUT';
NULL_HANDLING           : 'NULL_HANDLING';
READ_NOS                : 'READ_NOS';
BUFFERSIZE              : 'BUFFERSIZE';
RETURNTYPE              : 'RETURNTYPE';
SAMPLE_PERC             : 'SAMPLE_PERC';
FULLSCAN                : 'FULLSCAN';
TD_UNPIVOT              : 'TD_UNPIVOT';
VALUE_COLUMNS           : 'VALUE_COLUMNS';
UNPIVOT_COLUMN          : 'UNPIVOT_COLUMN';
COLUMN_LIST             : 'COLUMN_LIST';
COLUMN_ALIAS_LIST       : 'COLUMN_ALIAS_LIST';
INCLUDE_NULLS           : 'INCLUDE_NULLS';
WRITE_NOS               : 'WRITE_NOS';
NAMING                  : 'NAMING';
MANIFESTFILE            : 'MANIFESTFILE';
MANIFESTONLY            : 'MANIFESTONLY';
OVERWRITE               : 'OVERWRITE';
INCLUDE_ORDERING        : 'INCLUDE_ORDERING';
INCLUDE_HASHBY          : 'INCLUDE_HASHBY';
MAXOBJECTSIZE           : 'MAXOBJECTSIZE';
COMPRESSION             : 'COMPRESSION';
// found in json functions
ARRAY_TO_JSON           : 'ARRAY_TO_JSON';
BSON_CHECK              : 'BSON_CHECK';
GEOJSONFROMGEOM         : 'GEOJSONFROMGEOM';
GEOMFROMGEOJSON         : 'GEOMFROMGEOJSON';
JSON_CHECK              : 'JSON_CHECK';
JSONGETVALUE            : 'JSONGETVALUE';
JSONMETADATA            : 'JSONMETADATA';
NVP2JSON                : 'NVP2JSON';
// found in json table operators
TD_JSONSHRED            : 'TD_JSONSHRED';
JSON_KEYS               : 'JSON_KEYS';
JSON_TABLE              : 'JSON_TABLE';
DEPTH                   : 'DEPTH';
QUOTES                  : 'QUOTES';
ROWEXPR                 : 'ROWEXPR';
COLEXPR                 : 'COLEXPR';
RETURNTYPES             : 'RETURNTYPES';
NOCASE                  : 'NOCASE';
TRUNCATE                : 'TRUNCATE';

LINK                    : 'LINK';

/*
    Identifiers
*/
OBJECT_NAME : UNQUOTED_OBJECT_NAME | QUOTED_OBJECT_NAME ;

/*
    Literals
*/
UNSIGNED_INTEGER : DIGIT+ ;

HEX_BYTE_LITERAL : '\'' HEX_VALUE ('\'XB'|'\'XBV''\'XBF') ;

HEX_INTEGER_LITERAL : '\'' HEX_VALUE ('\'X'|'\'XI'|'\'XI1'|'\'XI2'|'\'XI4'|'\'XI8') ;

FLOAT_LITERAL : FLOAT_FRAGMENT ('E' ' '* ('-'|'+')? ' '* DIGIT+)? ;

DATE_STRING : '\'' DATE_VALUE '\'' ;

TIME_STRING : '\'' TIME_VALUE '\'' ;

TIMESTAMP_STRING : '\'' DATE_VALUE ' ' TIME_VALUE '\'' ;

PERIOD_STRING
    : '\'(' (DATE_STRING|TIME_STRING|TIMESTAMP_STRING)
      ' '* (','|'-') ' '*
      (DATE_STRING|TIME_STRING|TIMESTAMP_STRING|UNTIL_CHANGED|UNTIL_CLOSED)
      ')\''
    ;

UNICODE_STRING_LEADING : 'U&\'' UNICODE_VALUE '\'' ;

CHAR_STRING : ('\'' CHAR_VALUE '\'' | '\'\'') ;

HEX_STRING : '\'' HEX_VALUE ('\'XC'|'\'XCV'|'\'XCF') ;

PASSWORD_STRING : PASSWORD_CHARACTER+ ;

/*
    Other
*/
// Symbols
SEMICOLON : ';' ;
COLON : ':' ;
COMMA : ',' ;
DOT_ : '.' ;
AT_SIGN : '@' ;
CARET : '^';
QUESTION_MARK : '?' ;
OPEN_PAR : '(' ;
CLOSE_PAR : ')' ;
OPEN_SQ_BRACKET : '[';
CLOSE_SQ_BRACKET : ']';
CONCATENATE : '||' ;
BROKEN_CONCATENATE: '¦¦' ;
MUL_SIGN : '*' ;
DIV_SIGN : '/' ;
PLUS_SIGN : '+' ;
MINUS_SIGN : '-' ;
EXPONENTIATION : '**' ;
EQUALS_SIGN : '=' ;
NOT_EQUALS_SIGN : '<>' ;
NOT_EQUALS_SIGN_TD : '^=' ;
LT_SIGN : '<' ;
LE_SIGN : '<=' ;
GT_SIGN : '>' ;
GE_SIGN : '>=' ;

SINGLE_LINE_COMMENT : '--' ~('\r' | '\n')* NEWLINE_EOF -> skip ;
MULTI_LINE_COMMENT :  '/*' .*? '*/' -> skip ;

WS : [ \t\r\n\ufeff]+ -> skip ;


fragment NEWLINE_EOF : NEWLINE | EOF ;
fragment NEWLINE : '\r'? '\n' ;
fragment DIGIT : [0-9];
fragment FLOAT_FRAGMENT : (DIGIT* '.'? DIGIT+ | DIGIT+ '.');
fragment HEX_DIGIT : [0-9A-F] ;
fragment DATE_VALUE : DIGIT DIGIT DIGIT DIGIT '-' DIGIT DIGIT '-' DIGIT DIGIT ;
fragment TIME_VALUE
    : DIGIT DIGIT ':' DIGIT DIGIT ':' DIGIT DIGIT
      ('.' DIGIT+)? (('-'|'+') DIGIT DIGIT ':' DIGIT DIGIT)?
    ;
fragment CHARACTER_SET_PREFIX : ('_Latin'|'_Unicode'|'_Graphic'|'_KanjiSJIS') ;
fragment CHAR_VALUE : ('\'\''|~('\''))+ ;
fragment HEX_VALUE : HEX_DIGIT+ ;
fragment UNICODE_VALUE
    : (  UNICODE_ESC_CHAR UNICODE_ESC_CHAR
      | UNICODE_ESC_CHAR HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
      | UNICODE_ESC_CHAR '+' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
      | '\'\''
      | ' '
      | CHAR_VALUE
      )+ ;
/*
A single character from the session character set to use as the Unicode escape character in the delimited character literal.
The character must be within the printable ASCII range of Unicode characters (U+0021 through U+007E),
with the following exceptions:
• QUOTATION MARK (U+0022)
• APOSTROPHE (U+0027)
• PLUS SIGN (U+002B)
• hexadecimal digit (0-9, a-f, or A-F):
    ◦ U+0030 to U+0039
    ◦ U+0041 to U+0046
    ◦ U+0061 to U+0066
You can also specify the YEN SIGN (U+00A5) and WON SIGN (U+20A9) as the Unicode escape character.
*/
// ! #-& (-* ,-/ :-@ G-Z [-` {-DEL YEN_SIGN WON_SIGN (excluding lowercase letters and A-F)
fragment UNICODE_ESC_CHAR
    : [\u0021\u0023-\u0026\u0028-\u002A\u002C-\u002F\u003A-\u0040\u0047-\u005A\u005B-\u0060\u007B-\u007E\u00A5\u20A9]
    ;

// UNQUOTED OBJECT NAME
/*
An object name not enclosed in quotation marks must be composed of an identifier-
start character followed by a sequence of identifier-start or identifier extend characters,
up to the maximum object name length limit.
Note:
Characters in object names not enclosed in quotation marks must also be in the
session character set.
Identifier start characters must be contained in the session character set and belong
to one of the following Unicode General Category classes:
• Upper-case letters [Lu]
• Lower-case letters [Ll]
• Title-case letters [Lt]
• Modifier letters [Lm]
• Other letters ([Lo]
• Letter numbers [Nl]
Or be one of the following characters:
• NUMBER SIGN (U+0023) #
• DOLLAR SIGN (U+0024) $
• LOW LINE (U+005F) _
• INVERTED EXCLAMATION MARK (U+00A1)
• OVERLINE (U+203E)
• EURO SIGN (U+20AC)
• KATAKANA-HIRAGANA VOICED SOUND MARK (U+309B)
• KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK (U+309C)
• FULLWIDTH NUMBER SIGN (U+FF03)
• FULLWIDTH DOLLAR SIGN (U+FF04)
• FULLWIDTH LOW LINE (U+FF3F) */

// \p{L} - Letter class (Lu | Ll | Lt | Lm | Lo)
fragment IDENTIFIER_START_CHARACTER : [\p{L}\p{Nl}#$_\u00A1\u203E\u20AC\u309B\u309C\uFF03\uFF04\uFF3F] ;

/*
Identifier-extender characters must be in the session character set and belong to one
of the following Unicode General Category classes:
• Non-spacing marks [Mn]
• Spacing combing marks [Mc]
• Decimal numbers [Nd]
• Connector punctuations [Pc]
• Formatting codes [Cf]
Note:
The MIDDLE DOT character (U+00B7) is also a valid identifier-extender character.
*/

fragment IDENTIFIER_EXTEND_CHARACTER : [\p{Mn}\p{Mc}\p{Nd}\p{Pc}\p{Cf}\u00B7] ;
fragment UNQUOTED_OBJECT_NAME : IDENTIFIER_START_CHARACTER (IDENTIFIER_START_CHARACTER | IDENTIFIER_EXTEND_CHARACTER)* ;

// QUOTED OBJECT NAME
/*
A string literal is required for object names that:
• Have an identifier-extender character as the first character.
• Include the white space character, SPACE (U+0020)
• Are Teradata keywords
In addition, object names that contain any character from the following classes must
be enclosed in quotation marks, unless the character explicitly appears in the list of
allowed characters:
• Other, Control [Cc]
• Other, Not Assigned [Cn] No characters in this category appear in UNICODE character repertoire.
• Other, Private Use [Co]
• Other, Surrogate [Cs]
• Letter, Cased [LC] -- already defined in IDENTIFIER_START_CHARACTER
• Mark, Enclosing [Me]
• Number, Other [No]
• Punctuation, Dash [Pd]
• Punctuation, Close [Pe]
• Punctuation, Final quotation marks [Pf] (may behave like Ps or Pe depending on
usage)
• Punctuation, Initial quotation marks [Pi] (may behave like Ps or Pe depending on
usage)
• Punctuation, Other [Po]
• Punctuation, Open [Ps]
• Symbol, Currency [Sc]
• Symbol, Modifier [Sk]
• Symbol, Math [Sm]
• Symbol, Other [So]
• Separator, Line [Zl]
• Separator, Paragraph [Zp]
• Separator, Space [Zs]
Note:
When used as part of an object name, quotation marks must be represented as a
sequence of two QUOTATION MARKS characters (U+0022). Each set of two
quotation marks is counted as one character when calculating the name size limit.

The following characters cannot appear in an object name:
• NULL (U+0000)
• SUBSTITUTE character (U+001A)
• REPLACEMENT CHARACTER (U+FFFD)
• Compatibility ideographs (U+FA6C, U+FA6F, U+FAD0, FAD1, FAD5, FAD6, and FAD7)
*/

fragment QUOTED_IDENTIFIER_CHARACTER
    : ~["\u0000\u001A\uFFFD\uFA6C\uFA6F\uFAD0\uFAD1\uFAD5\uFAD6\uFAD7] ;

fragment QUOTED_OBJECT_NAME
    : '"' QUOTED_IDENTIFIER_CHARACTER+ '"'
    ;

/*
    The PasswordDigits parameter determines how you can use the digits 0 through 9, in a password.

    The PasswordSpecChar parameter determines how you can use ASCII special characters in a password,
with these options:
    • Special characters are allowed/not allowed/required
    • Passwords must contain at least one alpha character
    • No password can contain the database username
    • Passwords must contain a mixture of upper/lower case letters
    • Upper includes all UNICODE characters in General Category Class Lu.
    • Lower includes all UNICODE characters in General Category Class Ll.
    • Alpha includes all UNICODE characters that are in either Upper (Lu) or Lower (Ll)
    • Special indicates characters that are neither Alpha nor any of the characters 0 through 9 (U+0030 –
U+0039). By this definition, un-cased letters (General Category Class Lo) are considered Special (for
example, Arabic, Chinese or Hebrew characters). Likewise numeric digits (General Category Class
Nd) other than the characters 0 through 9 (U+0030 – U+0039) are classified as Special.
*/
fragment PASSWORD_CHARACTER
    : [0-9\p{Lu}\p{Ll}\p{Lo}\p{Nd}]
    ;
