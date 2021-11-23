/**
 * Oracle(c) PL/SQL 11g Parser
 *
 * Copyright (c) 2009-2011 Alexandre Porcelli <alexandre.porcelli@gmail.com>
 * Copyright (c) 2015-2019 Ivan Kochurkin (KvanTTT, kvanttt@gmail.com, Positive Technologies).
 * Copyright (c) 2017 Mark Adams <madams51703@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

lexer grammar PlSqlLexer;

options {
    superClass=PlSqlLexerBase;
}

@lexer::postinclude {
#include <PlSqlLexerBase.h>
}

fragment A
   : [aA]
   ;

fragment B
   : [bB]
   ;

fragment C
   : [cC]
   ;

fragment D
   : [dD]
   ;

fragment E
   : [eE]
   ;

fragment F
   : [fF]
   ;

fragment G
   : [gG]
   ;

fragment H
   : [hH]
   ;

fragment I
   : [iI]
   ;

fragment J
   : [jJ]
   ;

fragment K
   : [kK]
   ;

fragment L
   : [lL]
   ;

fragment M
   : [mM]
   ;

fragment N
   : [nN]
   ;

fragment O
   : [oO]
   ;

fragment P
   : [pP]
   ;

fragment Q
   : [qQ]
   ;

fragment R
   : [rR]
   ;

fragment S
   : [sS]
   ;

fragment T
   : [tT]
   ;

fragment U
   : [uU]
   ;

fragment V
   : [vV]
   ;

fragment W
   : [wW]
   ;

fragment X
   : [xX]
   ;

fragment Y
   : [yY]
   ;

fragment Z
   : [zZ]
   ;


ABORT                                             :A B O R T ;
ACCESS                                            :A C C E S S ;
ACCESSED                                          :A C C E S S E D ;
ACCOUNT                                           :A C C O U N T ;
ACTIVATE                                          :A C T I V A T E ;
ADD                                               :A D D ;
ADMIN                                             :A D M I N ;
ADMINISTER                                        :A D M I N I S T E R ;
ADVANCED                                          :A D V A N C E D ;
ADVISE                                            :A D V I S E ;
ADVISOR                                           :A D V I S O R ;
AFTER                                             :A F T E R ;
AGENT                                             :A G E N T ;
AGGREGATE                                         :A G G R E G A T E ;
ALL                                               :A L L ;
ALLOCATE                                          :A L L O C A T E ;
ALLOW                                             :A L L O W ;
ALTER                                             :A L T E R ;
ALWAYS                                            :A L W A Y S ;
ANALYZE                                           :A N A L Y Z E ;
AND                                               :A N D ;
ANY                                               :A N Y ;
ANYSCHEMA                                         :A N Y S C H E M A ;
APPLY                                             :A P P L Y ;
ARCHIVE                                           :A R C H I V E ;
ARCHIVELOG                                        :A R C H I V E L O G ;
ARRAY                                             :A R R A Y ;
AS                                                :A S ;
ASC                                               :A S C ;
ASSOCIATE                                         :A S S O C I A T E ;
ASYNCHRONOUS                                      :A S Y N C H R O N O U S ;
AT                                                :A T ;
ATTRIBUTE                                         :A T T R I B U T E ;
ATTRIBUTES                                        :A T T R I B U T E S ;
AUDIT                                             :A U D I T ;
AUTHENTICATED                                     :A U T H E N T I C A T E D ;
AUTHENTICATION                                    :A U T H E N T I C A T I O N ;
AUTHID                                            :A U T H I D ;
AUTO                                              :A U T O ;
AUTOALLOCATE                                      :A U T O A L L O C A T E ;
AUTOEXTEND                                        :A U T O E X T E N D ;
AUTOMATIC                                         :A U T O M A T I C ;
AUTONOMOUS_TRANSACTION                            :A U T O N O M O U S [_] T R A N S A C T I O N ;
AVAILABILITY                                      :A V A I L A B I L I T Y ;
AVG                                               :A V G ;
A_LETTER                                          :A ;
BACKUP                                            :B A C K U P ;
BASIC                                             :B A S I C ;
BASICFILE                                         :B A S I C F I L E ;
BATCH                                             :B A T C H ;
BECOME                                            :B E C O M E ;
BEFORE                                            :B E F O R E ;
BEGIN                                             :B E G I N ;
BETWEEN                                           :B E T W E E N ;
BIGFILE                                           :B I G F I L E ;
BINARY                                            :B I N A R Y ;
BITMAP                                            :B I T M A P ;
BLOCK                                             :B L O C K ;
BLOCKSIZE                                         :B L O C K S I Z E ;
BODY                                              :B O D Y ;
BOTH                                              :B O T H ;
BREADTH                                           :B R E A D T H ;
BUFFER_POOL                                       :B U F F E R [_] P O O L ;
BUILD                                             :B U I L D ;
BUILTIN                                           :B U I L T I N ;
BULK                                              :B U L K ;
BY                                                :B Y ;
BYTE                                              :B Y T E ;
CACHE                                             :C A C H E ;
CALL                                              :C A L L ;
CANCEL                                            :C A N C E L ;
CANONICAL                                         :C A N O N I C A L ;
CAPACITY                                          :C A P A C I T Y ;
CASCADE                                           :C A S C A D E ;
CASE                                              :C A S E ;
CAST                                              :C A S T ;
CELL_FLASH_CACHE                                  :C E L L [_] F L A S H [_] C A C H E ;
CERTIFICATE                                       :C E R T I F I C A T E ;
CHAINED                                           :C H A I N E D ;
CHANGE                                            :C H A N G E ;
CHAR                                              :C H A R ;
CHARACTER                                         :C H A R A C T E R ;
CHAR_CS                                           :C H A R [_] C S ;
CHECK                                             :C H E C K ;
CHECKPOINT                                        :C H E C K P O I N T ;
CHR                                               :C H R ;
CHUNK                                             :C H U N K ;
CLASS                                             :C L A S S ;
CLEAR                                             :C L E A R ;
CLOB                                              :C L O B ;
CLOBS                                             :C L O B S ;
CLONE                                             :C L O N E ;
CLOSE                                             :C L O S E ;
CLUSTER                                           :C L U S T E R ;
COALESCE                                          :C O A L E S C E ;
COLLATION                                         :C O L L A T I O N ;
COLLECT                                           :C O L L E C T ;
COLUMN                                            :C O L U M N ;
COLUMNS                                           :C O L U M N S ;
COLUMN_VALUE                                      :C O L U M N [_] V A L U E ;
COMMENT                                           :C O M M E N T ;
COMMIT                                            :C O M M I T ;
COMMITTED                                         :C O M M I T T E D ;
COMPACT                                           :C O M P A C T ;
COMPATIBILITY                                     :C O M P A T I B I L I T Y ;
COMPILE                                           :C O M P I L E ;
COMPLETE                                          :C O M P L E T E ;
COMPOUND                                          :C O M P O U N D ;
COMPRESS                                          :C O M P R E S S ;
COMPUTE                                           :C O M P U T E ;
CONNECT                                           :C O N N E C T ;
CONNECT_BY_ROOT                                   :C O N N E C T [_] B Y [_] R O O T ;
CONSIDER                                          :C O N S I D E R ;
CONSISTENT                                        :C O N S I S T E N T ;
CONSTANT                                          :C O N S T A N T ;
CONSTRAINT                                        :C O N S T R A I N T ;
CONSTRAINTS                                       :C O N S T R A I N T S ;
CONSTRUCTOR                                       :C O N S T R U C T O R ;
CONTAINER                                         :C O N T A I N E R ;
CONTAINER_DATA                                    :C O N T A I N E R [_] D A T A ;
CONTENT                                           :C O N T E N T ;
CONTEXT                                           :C O N T E X T ;
CONTINUE                                          :C O N T I N U E ;
CONTROLFILE                                       :C O N T R O L F I L E ;
CONVERT                                           :C O N V E R T ;
CORR                                              :C O R R ;
CORRUPTION                                        :C O R R U P T I O N ;
CORRUPT_XID                                       :C O R R U P T [_] X I D ;
CORRUPT_XID_ALL                                   :C O R R U P T [_] X I D [_] A L L ;
COST                                              :C O S T ;
COUNT                                             :C O U N T ;
COVAR_                                            :C O V A R [_] ;
CREATE                                            :C R E A T E ;
CREATION                                          :C R E A T I O N ;
CRITICAL                                          :C R I T I C A L ;
CROSS                                             :C R O S S ;
CROSSEDITION                                      :C R O S S E D I T I O N ;
CUBE                                              :C U B E ;
CUME_DIST                                         :C U M E [_] D I S T ;
CURRENT                                           :C U R R E N T ;
CURRENT_USER                                      :C U R R E N T [_] U S E R ;
CURSOR                                            :C U R S O R ;
CUSTOMDATUM                                       :C U S T O M D A T U M ;
CYCLE                                             :C Y C L E ;
C_LETTER                                          :C ;
DANGLING                                          :D A N G L I N G ;
DATA                                              :D A T A ;
DATABASE                                          :D A T A B A S E ;
DATAFILE                                          :D A T A F I L E ;
DATAFILES                                         :D A T A F I L E S ;
DATE                                              :D A T E ;
DAY                                               :D A Y ;
DAYS                                              :D A Y S ;
DBA_RECYCLEBIN                                    :D B A [_] R E C Y C L E B I N ;
DBTIMEZONE                                        :D B T I M E Z O N E ;
DB_ROLE_CHANGE                                    :D B [_] R O L E [_] C H A N G E ;
DDL                                               :D D L ;
DEALLOCATE                                        :D E A L L O C A T E ;
DEBUG                                             :D E B U G ;
DECLARE                                           :D E C L A R E ;
DECOMPOSE                                         :D E C O M P O S E ;
DECREMENT                                         :D E C R E M E N T ;
DECRYPT                                           :D E C R Y P T ;
DEDUPLICATE                                       :D E D U P L I C A T E ;
DEFAULT                                           :D E F A U L T ;
DEFAULTS                                          :D E F A U L T S ;
DEFERRABLE                                        :D E F E R R A B L E ;
DEFERRED                                          :D E F E R R E D ;
DEFINER                                           :D E F I N E R ;
DELEGATE                                          :D E L E G A T E ;
DELETE                                            :D E L E T E ;
DEMAND                                            :D E M A N D ;
DENSE_RANK                                        :D E N S E [_] R A N K ;
DEPTH                                             :D E P T H ;
DESC                                              :D E S C ;
DETERMINISTIC                                     :D E T E R M I N I S T I C ;
DICTIONARY                                        :D I C T I O N A R Y ;
DIMENSION                                         :D I M E N S I O N ;
DIRECTORY                                         :D I R E C T O R Y ;
DIRECT_PATH                                       :D I R E C T [_] P A T H ;
DISABLE                                           :D I S A B L E ;
DISALLOW                                          :D I S A L L O W ;
DISASSOCIATE                                      :D I S A S S O C I A T E ;
DISCONNECT                                        :D I S C O N N E C T ;
DISTINCT                                          :D I S T I N C T ;
DISTINGUISHED                                     :D I S T I N G U I S H E D ;
DISTRIBUTE                                        :D I S T R I B U T E ;
DISTRIBUTED                                       :D I S T R I B U T E D ;
DML                                               :D M L ;
DOCUMENT                                          :D O C U M E N T ;
DOUBLE                                            :D O U B L E ;
DOWNGRADE                                         :D O W N G R A D E ;
DROP                                              :D R O P ;
DUPLICATE                                         :D U P L I C A T E ;
EACH                                              :E A C H ;
EDITION                                           :E D I T I O N ;
EDITIONABLE                                       :E D I T I O N A B L E ;
EDITIONING                                        :E D I T I O N I N G ;
EDITIONS                                          :E D I T I O N S ;
ELEMENT                                           :E L E M E N T ;
ELSE                                              :E L S E ;
ELSIF                                             :E L S I F ;
EMPTY                                             :E M P T Y ;
ENABLE                                            :E N A B L E ;
ENCODING                                          :E N C O D I N G ;
ENCRYPT                                           :E N C R Y P T ;
ENCRYPTION                                        :E N C R Y P T I O N ;
END                                               :E N D ;
ENFORCED                                          :E N F O R C E D ;
ENTERPRISE                                        :E N T E R P R I S E ;
ENTITYESCAPING                                    :E N T I T Y E S C A P I N G ;
ERR                                               :E R R ;
ERROR                                             :E R R O R ;
ERRORS                                            :E R R O R S ;
ESCAPE                                            :E S C A P E ;
EVALNAME                                          :E V A L N A M E ;
EXCEPT                                            :E X C E P T ;
EXCEPTION                                         :E X C E P T I O N ;
EXCEPTIONS                                        :E X C E P T I O N S ;
EXCEPTION_INIT                                    :E X C E P T I O N [_] I N I T ;
EXCLUDE                                           :E X C L U D E ;
EXCLUDING                                         :E X C L U D I N G ;
EXCLUSIVE                                         :E X C L U S I V E ;
EXECUTE                                           :E X E C U T E ;
EXEMPT                                            :E X E M P T ;
EXISTS                                            :E X I S T S ;
EXIT                                              :E X I T ;
EXPIRE                                            :E X P I R E ;
EXPLAIN                                           :E X P L A I N ;
EXTENT                                            :E X T E N T ;
EXTERNAL                                          :E X T E R N A L ;
EXTERNALLY                                        :E X T E R N A L L Y ;
EXTRACT                                           :E X T R A C T ;
FAILED                                            :F A I L E D ;
FAILURE                                           :F A I L U R E ;
FALSE                                             :F A L S E ;
FAST                                              :F A S T ;
FETCH                                             :F E T C H ;
FILE                                              :F I L E ;
FILESYSTEM_LIKE_LOGGING                           :F I L E S Y S T E M [_] L I K E [_] L O G G I N G ;
FINAL                                             :F I N A L ;
FINISH                                            :F I N I S H ;
FIPSFLAG                                          :F I P S F L A G ;
FIRST                                             :F I R S T ;
FIRST_VALUE                                       :F I R S T [_] V A L U E ;
FLASHBACK                                         :F L A S H B A C K ;
FLASH_CACHE                                       :F L A S H [_] C A C H E ;
//FLOAT                                             :F L O A T ;
FOLDER                                            :F O L D E R ;
FOLLOWING                                         :F O L L O W I N G ;
FOLLOWS                                           :F O L L O W S ;
FOR                                               :F O R ;
FORALL                                            :F O R A L L ;
FORCE                                             :F O R C E ;
FOREIGN                                           :F O R E I G N ;
FORWARD                                           :F O R W A R D ;
FREELIST                                          :F R E E L I S T ;
FREELISTS                                         :F R E E L I S T S ;
FREEPOOLS                                         :F R E E P O O L S ;
FRESH                                             :F R E S H ;
FROM                                              :F R O M ;
FULL                                              :F U L L ;
FUNCTION                                          :F U N C T I O N ;
FUNCTIONS                                         :F U N C T I O N S ;
GENERATED                                         :G E N E R A T E D ;
GLOBAL                                            :G L O B A L ;
GLOBALLY                                          :G L O B A L L Y ;
GLOBAL_NAME                                       :G L O B A L [_] N A M E ;
GOTO                                              :G O T O ;
GRANT                                             :G R A N T ;
GREATEST                                          :G R E A T E S T ;
GROUP                                             :G R O U P ;
GROUPING                                          :G R O U P I N G ;
GROUPS                                            :G R O U P S ;
GUARANTEE                                         :G U A R A N T E E ;
GUARD                                             :G U A R D ;
HASH                                              :H A S H ;
HASHKEYS                                          :H A S H K E Y S ;
HAVING                                            :H A V I N G ;
HEAP                                              :H E A P ;
HIDE                                              :H I D E ;
HIERARCHY                                         :H I E R A R C H Y ;
HIGH                                              :H I G H ;
HOUR                                              :H O U R ;
ID                                                :I D ;
IDENTIFIED                                        :I D E N T I F I E D ;
IDENTIFIER                                        :I D E N T I F I E R ;
IDENTITY                                          :I D E N T I T Y ;
IF                                                :I F ;
IGNORE                                            :I G N O R E ;
ILM                                               :I L M ;
IMMEDIATE                                         :I M M E D I A T E ;
IN                                                :I N ;
INCLUDE                                           :I N C L U D E ;
INCLUDING                                         :I N C L U D I N G ;
INCREMENT                                         :I N C R E M E N T ;
INDENT                                            :I N D E N T ;
INDEX                                             :I N D E X ;
INDEXED                                           :I N D E X E D ;
INDEXES                                           :I N D E X E S ;
INDEXTYPE                                         :I N D E X T Y P E ;
INDEXTYPES                                        :I N D E X T Y P E S ;
INDICATOR                                         :I N D I C A T O R ;
INDICES                                           :I N D I C E S ;
INFINITE                                          :I N F I N I T E ;
INHERIT                                           :I N H E R I T ;
INITIAL                                           :I N I T I A L ;
INITIALIZED                                       :I N I T I A L I Z E D ;
INITIALLY                                         :I N I T I A L L Y ;
INITRANS                                          :I N I T R A N S ;
INLINE                                            :I N L I N E ;
INMEMORY                                          :I N M E M O R Y ;
INNER                                             :I N N E R ;
INOUT                                             :I N O U T ;
INSERT                                            :I N S E R T ;
INSTANCE                                          :I N S T A N C E ;
INSTANTIABLE                                      :I N S T A N T I A B L E ;
INSTEAD                                           :I N S T E A D ;
INTERSECT                                         :I N T E R S E C T ;
INTERVAL                                          :I N T E R V A L ;
INTO                                              :I N T O ;
INVALIDATE                                        :I N V A L I D A T E ;
INVISIBLE                                         :I N V I S I B L E ;
IS                                                :I S ;
ISOLATION                                         :I S O L A T I O N ;
ITERATE                                           :I T E R A T E ;
JAVA                                              :J A V A ;
JOB                                               :J O B ;
JOIN                                              :J O I N ;
KEEP                                              :K E E P ;
KEEP_DUPLICATES                                   :K E E P [_] D U P L I C A T E S ;
KEY                                               :K E Y ;
LAG                                               :L A G ;
LANGUAGE                                          :L A N G U A G E ;
LAST                                              :L A S T ;
LAST_VALUE                                        :L A S T [_] V A L U E ;
LEAD                                              :L E A D ;
LEADING                                           :L E A D I N G ;
LEAST                                             :L E A S T ;
LEFT                                              :L E F T ;
LESS                                              :L E S S ;
LEVEL                                             :L E V E L ;
LEVELS                                            :L E V E L S ;
LIBRARY                                           :L I B R A R Y ;
LIKE                                              :L I K E ;
LIKE2                                             :L I K E [2];
LIKE4                                             :L I K E [4];
LIKEC                                             :L I K E C ;
LIMIT                                             :L I M I T ;
LINK                                              :L I N K ;
LIST                                              :L I S T ;
LISTAGG                                           :L I S T A G G ;
LOB                                               :L O B ;
LOBS                                              :L O B S ;
LOCAL                                             :L O C A L ;
LOCATION                                          :L O C A T I O N ;
LOCATOR                                           :L O C A T O R ;
LOCK                                              :L O C K ;
LOCKED                                            :L O C K E D ;
LOG                                               :L O G ;
LOGFILE                                           :L O G F I L E ;
LOGFILES                                          :L O G F I L E S ;
LOGGING                                           :L O G G I N G ;
LOGICAL                                           :L O G I C A L ;
LOGMINING                                         :L O G M I N I N G ;
LOGOFF                                            :L O G O F F ;
LOGON                                             :L O G O N ;
LONG                                              :L O N G ;
LOOP                                              :L O O P ;
LOW                                               :L O W ;
MAIN                                              :M A I N ;
MANAGE                                            :M A N A G E ;
MANAGED                                           :M A N A G E D ;
MANAGEMENT                                        :M A N A G E M E N T ;
MANUAL                                            :M A N U A L ;
MAP                                               :M A P ;
MAPPING                                           :M A P P I N G ;
MASTER                                            :M A S T E R ;
MATCHED                                           :M A T C H E D ;
MATERIALIZED                                      :M A T E R I A L I Z E D ;
MAX                                               :M A X ;
MAXEXTENTS                                        :M A X E X T E N T S ;
MAXIMIZE                                          :M A X I M I Z E ;
MAXSIZE                                           :M A X S I Z E ;
MAXTRANS                                          :M A X T R A N S ;
MAXVALUE                                          :M A X V A L U E ;
MEASURE                                           :M E A S U R E ;
MEASURES                                          :M E A S U R E S ;
MEDIAN                                            :M E D I A N ;
MEDIUM                                            :M E D I U M ;
MEMBER                                            :M E M B E R ;
MEMCOMPRESS                                       :M E M C O M P R E S S ;
MEMORY                                            :M E M O R Y ;
MERGE                                             :M E R G E ;
METADATA                                          :M E T A D A T A ;
MIN                                               :M I N ;
MINEXTENTS                                        :M I N E X T E N T S ;
MINIMIZE                                          :M I N I M I Z E ;
MINIMUM                                           :M I N I M U M ;
MINING                                            :M I N I N G ;
MINUS                                             :M I N U S ;
MINUTE                                            :M I N U T E ;
MINVALUE                                          :M I N V A L U E ;
MOD                                               :M O D ;
MODE                                              :M O D E ;
MODEL                                             :M O D E L ;
MODIFICATION                                      :M O D I F I C A T I O N ;
MODIFY                                            :M O D I F Y ;
MONITORING                                        :M O N I T O R I N G ;
MONTH                                             :M O N T H ;
MONTHS                                            :M O N T H S ;
MOUNT                                             :M O U N T ;
MOVE                                              :M O V E ;
MOVEMENT                                          :M O V E M E N T ;
MULTISET                                          :M U L T I S E T ;
NAME                                              :N A M E ;
NAMESPACE                                         :N A M E S P A C E ;
NAN                                               :N A N ;
NATURAL                                           :N A T U R A L ;
NAV                                               :N A V ;
NCHAR_CS                                          :N C H A R [_] C S ;
NESTED                                            :N E S T E D ;
NETWORK                                           :N E T W O R K ;
NEVER                                             :N E V E R ;
NEW                                               :N E W ;
NEXT                                              :N E X T ;
NO                                                :N O ;
NOARCHIVELOG                                      :N O A R C H I V E L O G ;
NOAUDIT                                           :N O A U D I T ;
NOCACHE                                           :N O C A C H E ;
NOCOMPRESS                                        :N O C O M P R E S S ;
NOCOPY                                            :N O C O P Y ;
NOCYCLE                                           :N O C Y C L E ;
NODELAY                                           :N O D E L A Y ;
NOENTITYESCAPING                                  :N O E N T I T Y E S C A P I N G ;
NOGUARANTEE                                       :N O G U A R A N T E E ;
NOLOGGING                                         :N O L O G G I N G ;
NOMAPPING                                         :N O M A P P I N G ;
NOMAXVALUE                                        :N O M A X V A L U E ;
NOMINIMIZE                                        :N O M I N I M I Z E ;
NOMINVALUE                                        :N O M I N V A L U E ;
NOMONITORING                                      :N O M O N I T O R I N G ;
NONE                                              :N O N E ;
NONEDITIONABLE                                    :N O N E D I T I O N A B L E ;
NONSCHEMA                                         :N O N S C H E M A ;
NOORDER                                           :N O O R D E R ;
NOPARALLEL                                        :N O P A R A L L E L ;
NORELY                                            :N O R E L Y ;
NORESETLOGS                                       :N O R E S E T L O G S ;
NOREVERSE                                         :N O R E V E R S E ;
NORMAL                                            :N O R M A L ;
NOROWDEPENDENCIES                                 :N O R O W D E P E N D E N C I E S ;
NOSCHEMACHECK                                     :N O S C H E M A C H E C K ;
NOSORT                                            :N O S O R T ;
NOT                                               :N O T ;
NOTHING                                           :N O T H I N G ;
NOTIFICATION                                      :N O T I F I C A T I O N ;
NOVALIDATE                                        :N O V A L I D A T E ;
NOWAIT                                            :N O W A I T ;
NTILE                                             :N T I L E ;
NULLS                                             :N U L L S ;
NULL_                                             :N U L L ;
OBJECT                                            :O B J E C T ;
OF                                                :O F ;
OFF                                               :O F F ;
OFFLINE                                           :O F F L I N E ;
OFFSET                                            :O F F S E T ;
OID                                               :O I D ;
OIDINDEX                                          :O I D I N D E X ;
OLD                                               :O L D ;
OLTP                                              :O L T P ;
ON                                                :O N ;
ONLINE                                            :O N L I N E ;
ONLY                                              :O N L Y ;
OPEN                                              :O P E N ;
OPERATOR                                          :O P E R A T O R ;
OPTIMAL                                           :O P T I M A L ;
OPTION                                            :O P T I O N ;
OR                                                :O R ;
ORADATA                                           :O R A D A T A ;
ORDER                                             :O R D E R ;
ORDINALITY                                        :O R D I N A L I T Y ;
ORGANIZATION                                      :O R G A N I Z A T I O N ;
OSERROR                                           :O S E R R O R ;
OUT                                               :O U T ;
OUTER                                             :O U T E R ;
OUTLINE                                           :O U T L I N E ;
OVER                                              :O V E R ;
OVERFLOW                                          :O V E R F L O W ;
OVERRIDING                                        :O V E R R I D I N G ;
PACKAGE                                           :P A C K A G E ;
PACKAGES                                          :P A C K A G E S ;
PARALLEL                                          :P A R A L L E L ;
PARALLEL_ENABLE                                   :P A R A L L E L [_] E N A B L E ;
PARAMETERS                                        :P A R A M E T E R S ;
PARENT                                            :P A R E N T ;
PARTITION                                         :P A R T I T I O N ;
PARTITIONS                                        :P A R T I T I O N S ;
PASSING                                           :P A S S I N G ;
PASSWORD                                          :P A S S W O R D ;
PATH                                              :P A T H ;
PCTFREE                                           :P C T F R E E ;
PCTINCREASE                                       :P C T I N C R E A S E ;
PCTTHRESHOLD                                      :P C T T H R E S H O L D ;
PCTUSED                                           :P C T U S E D ;
PCTVERSION                                        :P C T V E R S I O N ;
PERCENTILE_CONT                                   :P E R C E N T I L E [_] C O N T ;
PERCENTILE_DISC                                   :P E R C E N T I L E [_] D I S C ;
PERCENT_CHARSET                                   : '%' SPACE* C H A R S E T ;
PERCENT_FOUND                                     :'%' SPACE* F O U N D ;
PERCENT_ISOPEN                                    :'%' SPACE* I S O P E N ;
PERCENT_KEYWORD                                   :P E R C E N T ;
PERCENT_NOTFOUND                                  :'%' SPACE* N O T F O U N D ;
PERCENT_RANK                                      :P E R C E N T [_] R A N K ;
PERCENT_ROWCOUNT                                  :'%' SPACE* R O W C O U N T ;
PERCENT_ROWTYPE                                   :'%' SPACE* R O W T Y P E ;
PERCENT_TYPE                                      :'%' SPACE* T Y P E ;
PERFORMANCE                                       :P E R F O R M A N C E ;
PERMANENT                                         :P E R M A N E N T ;
PHYSICAL                                          :P H Y S I C A L ;
PIPE                                              :P I P E ;
PIPELINED                                         :P I P E L I N E D ;
PIVOT                                             :P I V O T ;
PLAN                                              :P L A N ;
PLUGGABLE                                         :P L U G G A B L E ;
POLICY                                            :P O L I C Y ;
PRAGMA                                            :P R A G M A ;
PREBUILT                                          :P R E B U I L T ;
PRECEDES                                          :P R E C E D E S ;
PRECEDING                                         :P R E C E D I N G ;
PRECISION                                         :P R E C I S I O N ;
PREDICTION                                        :P R E D I C T I O N ;
PREDICTION_BOUNDS                                 :P R E D I C T I O N [_] B O U N D S ;
PREDICTION_COST                                   :P R E D I C T I O N [_] C O S T ;
PREDICTION_DETAILS                                :P R E D I C T I O N [_] D E T A I L S ;
PREDICTION_PROBABILITY                            :P R E D I C T I O N [_] P R O B A B I L I T Y ;
PREDICTION_SET                                    :P R E D I C T I O N [_] S E T ;
PREPARE                                           :P R E P A R E ;
PRESENT                                           :P R E S E N T ;
PRESERVE                                          :P R E S E R V E ;
PRIMARY                                           :P R I M A R Y ;
PRIOR                                             :P R I O R ;
PRIORITY                                          :P R I O R I T Y ;
PRIVILEGE                                         :P R I V I L E G E ;
PRIVILEGES                                        :P R I V I L E G E S ;
PROCEDURAL                                        :P R O C E D U R A L ;
PROCEDURE                                         :P R O C E D U R E ;
PROCESS                                           :P R O C E S S ;
PROFILE                                           :P R O F I L E ;
PROGRAM                                           :P R O G R A M ;
PROTECTION                                        :P R O T E C T I O N ;
PUBLIC                                            :P U B L I C ;
PURGE                                             :P U R G E ;
QUERY                                             :Q U E R Y ;
QUOTA                                             :Q U O T A ;
RAISE                                             :R A I S E ;
RANGE                                             :R A N G E ;
RANK                                              :R A N K ;
RATIO_TO_REPORT                                   :R A T I O [_] T O [_] R E P O R T ;
RAW                                               :R A W ;
READ                                              :R E A D ;
READS                                             :R E A D S ;
//REAL                                              :R E A L ;
REBUILD                                           :R E B U I L D ;
RECORD                                            :R E C O R D ;
RECORDS_PER_BLOCK                                 :R E C O R D S [_] P E R [_] B L O C K ;
RECOVER                                           :R E C O V E R ;
RECOVERY                                          :R E C O V E R Y ;
RECYCLE                                           :R E C Y C L E ;
REDACTION                                         :R E D A C T I O N ;
REDUCED                                           :R E D U C E D ;
REF                                               :R E F ;
REFERENCE                                         :R E F E R E N C E ;
REFERENCES                                        :R E F E R E N C E S ;
REFERENCING                                       :R E F E R E N C I N G ;
REFRESH                                           :R E F R E S H ;
REGISTER                                          :R E G I S T E R ;
REGR_                                             :R E G R [_] ;
REJECT                                            :R E J E C T ;
REKEY                                             :R E K E Y ;
RELATIONAL                                        :R E L A T I O N A L ;
RELIES_ON                                         :R E L I E S [_] O N ;
RELY                                              :R E L Y ;
REMOVE                                            :R E M O V E ;
RENAME                                            :R E N A M E ;
REPLACE                                           :R E P L A C E ;
REPLICATION                                       :R E P L I C A T I O N ;
REQUIRED                                          :R E Q U I R E D ;
RESETLOGS                                         :R E S E T L O G S ;
RESIZE                                            :R E S I Z E ;
RESOURCE                                          :R E S O U R C E ;
RESPECT                                           :R E S P E C T ;
RESTRICTED                                        :R E S T R I C T E D ;
RESTRICT_REFERENCES                               :R E S T R I C T [_] R E F E R E N C E S ;
RESULT                                            :R E S U L T ;
RESULT_CACHE                                      :R E S U L T [_] C A C H E ;
RESUMABLE                                         :R E S U M A B L E ;
RETENTION                                         :R E T E N T I O N ;
RETURN                                            :R E T U R N ;
RETURNING                                         :R E T U R N I N G ;
REUSE                                             :R E U S E ;
REVERSE                                           :R E V E R S E ;
REVOKE                                            :R E V O K E ;
REWRITE                                           :R E W R I T E ;
RIGHT                                             :R I G H T ;
ROLE                                              :R O L E ;
ROLES                                             :R O L E S ;
ROLLBACK                                          :R O L L B A C K ;
ROLLUP                                            :R O L L U P ;
ROUND                                             :R O U N D ;
ROW                                               :R O W ;
ROWDEPENDENCIES                                   :R O W D E P E N D E N C I E S ;
ROWID                                             :R O W I D ;
ROWS                                              :R O W S ;
ROW_NUMBER                                        :R O W [_] N U M B E R ;
RULES                                             :R U L E S ;
SALT                                              :S A L T ;
SAMPLE                                            :S A M P L E ;
SAVE                                              :S A V E ;
SAVEPOINT                                         :S A V E P O I N T ;
SCHEDULER                                         :S C H E D U L E R ;
SCHEMACHECK                                       :S C H E M A C H E C K ;
SCN                                               :S C N ;
SCOPE                                             :S C O P E ;
SEARCH                                            :S E A R C H ;
SECOND                                            :S E C O N D ;
SECUREFILE                                        :S E C U R E F I L E ;
SEED                                              :S E E D ;
SEGMENT                                           :S E G M E N T ;
SELECT                                            :S E L E C T ;
SELECTIVITY                                       :S E L E C T I V I T Y ;
SELF                                              :S E L F ;
SEQUENCE                                          :S E Q U E N C E ;
SEQUENTIAL                                        :S E Q U E N T I A L ;
SERIALIZABLE                                      :S E R I A L I Z A B L E ;
SERIALLY_REUSABLE                                 :S E R I A L L Y [_] R E U S A B L E ;
SERVERERROR                                       :S E R V E R E R R O R ;
SERVICE                                           :S E R V I C E ;
SESSION                                           :S E S S I O N ;
SESSIONTIMEZONE                                   :S E S S I O N T I M E Z O N E ;
SET                                               :S E T ;
SETS                                              :S E T S ;
SETTINGS                                          :S E T T I N G S ;
SHARE                                             :S H A R E ;
SHARING                                           :S H A R I N G ;
SHOW                                              :S H O W ;
SHRINK                                            :S H R I N K ;
SHUTDOWN                                          :S H U T D O W N ;
SIBLINGS                                          :S I B L I N G S ;
//SIGNTYPE                                          :S I G N T Y P E ;
//SIMPLE_INTEGER                                    :S I M P L E [_] I N T E G E R ;
SINGLE                                            :S I N G L E ;
SIZE                                              :S I Z E ;
SKIP_                                             :S K I P ;
SMALLFILE                                         :S M A L L F I L E ;
//SMALLINT                                          :S M A L L I N T ;
SNAPSHOT                                          :S N A P S H O T ;
SOME                                              :S O M E ;
SORT                                              :S O R T ;
SOURCE                                            :S O U R C E ;
SPACE_KEYWORD                                     :S P A C E ;
SPATIAL                                           :S P A T I A L ;
SPECIFICATION                                     :S P E C I F I C A T I O N ;
SPLIT                                             :S P L I T ;
SQL                                               :S Q L ;
SQLDATA                                           :S Q L D A T A ;
SQLERROR                                          :S Q L E R R O R ;
STANDALONE                                        :S T A N D A L O N E ;
STANDBY                                           :S T A N D B Y ;
START                                             :S T A R T ;
STARTUP                                           :S T A R T U P ;
STATEMENT                                         :S T A T E M E N T ;
STATEMENTS                                        :S T A T E M E N T S ;
STATEMENT_ID                                      :S T A T E M E N T [_] I D ;
STATIC                                            :S T A T I C ;
STATISTICS                                        :S T A T I S T I C S ;
STDDEV                                            :S T D D E V ;
STOP                                              :S T O P ;
STORAGE                                           :S T O R A G E ;
STORE                                             :S T O R E ;
//STRING                                            :S T R I N G ;
STRUCTURE                                         :S T R U C T U R E ;
SUBMULTISET                                       :S U B M U L T I S E T ;
SUBPARTITION                                      :S U B P A R T I T I O N ;
SUBPARTITIONS                                     :S U B P A R T I T I O N S ;
SUBSTITUTABLE                                     :S U B S T I T U T A B L E ;
//SUBSTR                                            :S U B S T R ;
SUBTYPE                                           :S U B T Y P E ;
SUCCESS                                           :S U C C E S S ;
SUCCESSFUL                                        :S U C C E S S F U L ;
SUM                                               :S U M ;
SUPPLEMENTAL                                      :S U P P L E M E N T A L ;
SUSPEND                                           :S U S P E N D ;
SWITCH                                            :S W I T C H ;
SWITCHOVER                                        :S W I T C H O V E R ;
SYNCHRONOUS                                       :S Y N C H R O N O U S ;
SYNONYM                                           :S Y N O N Y M ;
SYSBACKUP                                         :S Y S B A C K U P ;
SYSDBA                                            :S Y S D B A ;
SYSDG                                             :S Y S D G ;
SYSGUID                                           :S Y S G U I D ;
SYSKM                                             :S Y S K M ;
SYSOPER                                           :S Y S O P E R ;
SYSTEM                                            :S Y S T E M ;
TABLE                                             :T A B L E ;
TABLES                                            :T A B L E S ;
TABLESPACE                                        :T A B L E S P A C E ;
TEMPFILE                                          :T E M P F I L E ;
TEMPLATE                                          :T E M P L A T E ;
TEMPORARY                                         :T E M P O R A R Y ;
TEST                                              :T E S T ;
THAN                                              :T H A N ;
THE                                               :T H E ;
THEN                                              :T H E N ;
THREAD                                            :T H R E A D ;
THROUGH                                           :T H R O U G H ;
TIER                                              :T I E R ;
TIES                                              :T I E S ;
TIME                                              :T I M E ;
TIMESTAMP                                         :T I M E S T A M P ;
//TIMESTAMP_LTZ_UNCONSTRAINED                       :T I M E S T A M P [_] L T Z [_] U N C O N S T R A I N E D ;
//TIMESTAMP_TZ_UNCONSTRAINED                        :T I M E S T A M P [_] T Z [_] U N C O N S T R A I N E D ;
//TIMESTAMP_UNCONSTRAINED                           :T I M E S T A M P [_] U N C O N S T R A I N E D ;
TIMEZONE                                          :T I M E Z O N E ;
//TIMEZONE_ABBR                                     :T I M E Z O N E [_] A B B R ;
//TIMEZONE_HOUR                                     :T I M E Z O N E [_] H O U R ;
//TIMEZONE_MINUTE                                   :T I M E Z O N E [_] M I N U T E ;
//TIMEZONE_REGION                                   :T I M E Z O N E [_] R E G I O N ;
TO                                                :T O ;
TO_CHAR                                           :T O [_] C H A R ;
TO_DATE                                           :T O [_] D A T E ;
TRACE                                             :T R A C E ;
TRACKING                                          :T R A C K I N G ;
TRAILING                                          :T R A I L I N G ;
TRANSACTION                                       :T R A N S A C T I O N ;
TRANSLATE                                         :T R A N S L A T E ;
TRANSLATION                                       :T R A N S L A T I O N ;
TREAT                                             :T R E A T ;
TRIGGER                                           :T R I G G E R ;
TRIGGERS                                          :T R I G G E R S ;
TRIM                                              :T R I M ;
TRUE                                              :T R U E ;
TRUNCATE                                          :T R U N C A T E ;
TRUSTED                                           :T R U S T E D ;
TUNING                                            :T U N I N G ;
TYPE                                              :T Y P E ;
TYPES                                             :T Y P E S ;
UNARCHIVED                                        :U N A R C H I V E D ;
UNBOUNDED                                         :U N B O U N D E D ;
UNDER                                             :U N D E R ;
UNDO                                              :U N D O ;
UNIFORM                                           :U N I F O R M ;
UNION                                             :U N I O N ;
UNIQUE                                            :U N I Q U E ;
UNLIMITED                                         :U N L I M I T E D ;
UNLOCK                                            :U N L O C K ;
UNPIVOT                                           :U N P I V O T ;
UNPLUG                                            :U N P L U G ;
UNRECOVERABLE                                     :U N R E C O V E R A B L E ;
UNTIL                                             :U N T I L ;
UNUSABLE                                          :U N U S A B L E ;
UNUSED                                            :U N U S E D ;
UPDATE                                            :U P D A T E ;
UPDATED                                           :U P D A T E D ;
UPGRADE                                           :U P G R A D E ;
UPSERT                                            :U P S E R T ;
//UROWID                                            :U R O W I D ;
USAGE                                             :U S A G E ;
USE                                               :U S E ;
USER                                              :U S E R ;
USERS                                             :U S E R S ;
USING                                             :U S I N G ;
VALIDATE                                          :V A L I D A T E ;
VALUE                                             :V A L U E ;
VALUES                                            :V A L U E S ;
//VARCHAR                                           :V A R C H A R ;
//VARCHAR2                                          :V A R C H A R [2];
VARIABLE                                          :V A R I A B L E ;
VARIANCE                                          :V A R I A N C E ;
VARRAY                                            :V A R R A Y ;
VARRAYS                                           :V A R R A Y S ;
VARYING                                           :V A R Y I N G ;
VAR_                                              :V A R [_] ;
VERSION                                           :V E R S I O N ;
VERSIONS                                          :V E R S I O N S ;
VIEW                                              :V I E W ;
VIRTUAL                                           :V I R T U A L ;
VISIBLE                                           :V I S I B L E ;
WAIT                                              :W A I T ;
WARNING                                           :W A R N I N G ;
WELLFORMED                                        :W E L L F O R M E D ;
WHEN                                              :W H E N ;
WHENEVER                                          :W H E N E V E R ;
WHERE                                             :W H E R E ;
WHILE                                             :W H I L E ;
WITH                                              :W I T H ;
WITHIN                                            :W I T H I N ;
WITHOUT                                           :W I T H O U T ;
WORK                                              :W O R K ;
WRITE                                             :W R I T E ;
XDB                                               :X D B ;
XML                                               :X M L ;
XMLAGG                                            :X M L A G G ;
XMLATTRIBUTES                                     :X M L A T T R I B U T E S ;
XMLCAST                                           :X M L C A S T ;
XMLCOLATTVAL                                      :X M L C O L A T T V A L ;
XMLELEMENT                                        :X M L E L E M E N T ;
XMLEXISTS                                         :X M L E X I S T S ;
XMLFOREST                                         :X M L F O R E S T ;
XMLINDEX                                          :X M L I N D E X ;
XMLNAMESPACES                                     :X M L N A M E S P A C E S ;
XMLPARSE                                          :X M L P A R S E ;
XMLPI                                             :X M L P I ;
XMLQUERY                                          :X M L Q U E R Y ;
XMLROOT                                           :X M L R O O T ;
XMLSCHEMA                                         :X M L S C H E M A ;
XMLSERIALIZE                                      :X M L S E R I A L I Z E ;
XMLTABLE                                          :X M L T A B L E ;
XMLTYPE                                           :X M L T Y P E ;
YEAR                                              :Y E A R ;
YEARS                                             :Y E A R S ;
YES                                               :Y E S ;
//YMINTERVAL_UNCONSTRAINED                          :Y M I N T E R V A L [_] U N C O N S T R A I N E D ;
ZONE                                              :Z O N E ;
INTERFACE                                         :I N T E R F A C E ;
SUBSTR                                            : S U B S T R ;
BEQUEATH                                          : B E Q U E A T H ;
DECODE                                            :D E C O D E ;
NVL                                               :N V L ;
SCHEMA                                            :S C H E M A ;


VARCHAR                                           :V A R C H A R ;
VARCHAR2                                          :V A R C H A R [2];
DEC                                               :D E C ;
DECIMAL                                           :D E C I M A L ;
FLOAT                                             :F L O A T ;
BINARY_DOUBLE                                     :B I N A R Y [_] D O U B L E ;
BINARY_FLOAT                                      :B I N A R Y [_] F L O A T ;
BINARY_INTEGER                                    :B I N A R Y [_] I N T E G E R ;
NATURALN                                          :N A T U R A L N ;
NCHAR                                             :N C H A R ;
NCLOB                                             :N C L O B ;
POSITIVE                                          :P O S I T I V E ;
POSITIVEN                                         :P O S I T I V E N ;
PLS_INTEGER                                       :P L S [_] I N T E G E R ;
SIGNTYPE                                          :S I G N T Y P E ;
SIMPLE_INTEGER                                    :S I M P L E [_] I N T E G E R ;
NUMBER                                            :N U M B E R ;
NUMERIC                                           :N U M E R I C ;
NVARCHAR2                                         :N V A R C H A R [2];
INT                                               :I N T ;
INTEGER                                           :I N T E G E R ;
SMALLINT                                          :S M A L L I N T ;
REAL                                              :R E A L ;
STRING                                            :S T R I N G ;
BOOLEAN                                           :B O O L E A N ;
UROWID                                            :U R O W I D ;
TIMEZONE_ABBR                                     :T I M E Z O N E [_] A B B R ;
TIMEZONE_HOUR                                     :T I M E Z O N E [_] H O U R ;
TIMEZONE_MINUTE                                   :T I M E Z O N E [_] M I N U T E ;
TIMEZONE_REGION                                   :T I M E Z O N E [_] R E G I O N ;
TIMESTAMP_LTZ_UNCONSTRAINED                       :T I M E S T A M P [_] L T Z [_] U N C O N S T R A I N E D ;
TIMESTAMP_TZ_UNCONSTRAINED                        :T I M E S T A M P [_] T Z [_] U N C O N S T R A I N E D ;
TIMESTAMP_UNCONSTRAINED                           :T I M E S T A M P [_] U N C O N S T R A I N E D ;
YMINTERVAL_UNCONSTRAINED                          :Y M I N T E R V A L [_] U N C O N S T R A I N E D ;
DSINTERVAL_UNCONSTRAINED                          :D S I N T E R V A L [_] U N C O N S T R A I N E D ;
BFILE                                             :B F I L E ;
BLOB                                              :B L O B ;
MLSLABEL                                          :M L S L A B E L ;





// Rule #358 <NATIONAL_CHAR_STRING_LIT> - subtoken typecast in <REGULAR_ID>, it also incorporates <character_representation>
//  Lowercase 'n' is a usual addition to the standard

NATIONAL_CHAR_STRING_LIT: 'N' '\'' (~('\'' | '\r' | '\n' ) | '\'' '\'' | NEWLINE)* '\'';

//  Rule #040 <BIT_STRING_LIT> - subtoken typecast in <REGULAR_ID>
//  Lowercase 'b' is a usual addition to the standard

BIT_STRING_LIT: 'B' ('\'' [01]* '\'')+;

//  Rule #284 <HEX_STRING_LIT> - subtoken typecast in <REGULAR_ID>
//  Lowercase 'x' is a usual addition to the standard

HEX_STRING_LIT: 'X' ('\'' [A-F0-9]* '\'')+;
DOUBLE_PERIOD:  '..';
PERIOD:         '.';

//{ Rule #238 <EXACT_NUM_LIT>
//  This rule is a bit tricky - it resolves the ambiguity with <PERIOD>
//  It also incorporates <mantisa> and <exponent> for the <APPROXIMATE_NUM_LIT>
//  Rule #501 <signed_integer> was incorporated directly in the token <APPROXIMATE_NUM_LIT>
//  See also the rule #617 <unsigned_num_lit>
/*
    : (
            UNSIGNED_INTEGER
            ( '.' UNSIGNED_INTEGER
            | {$type = UNSIGNED_INTEGER;}
            ) ( E ('+' | '-')? UNSIGNED_INTEGER {$type = APPROXIMATE_NUM_LIT;} )?
    | '.' UNSIGNED_INTEGER ( E ('+' | '-')? UNSIGNED_INTEGER {$type = APPROXIMATE_NUM_LIT;} )?
    )
    (D | F)?
    ;*/

UNSIGNED_INTEGER:    [0-9]+;
APPROXIMATE_NUM_LIT: FLOAT_FRAGMENT ('E' ('+'|'-')? (FLOAT_FRAGMENT | [0-9]+))? ('D' | 'F')?;

// Rule #--- <CHAR_STRING> is a base for Rule #065 <char_string_lit> , it incorporates <character_representation>
// and a superfluous subtoken typecasting of the "QUOTE"
CHAR_STRING: '\''  (~('\'' | '\r' | '\n') | '\'' '\'' | NEWLINE)* '\'';

// See https://livesql.oracle.com/apex/livesql/file/content_CIREYU9EA54EOKQ7LAMZKRF6P.html
// TODO: context sensitive string quotes (any characted after quote)
CHAR_STRING_PERL    : 'Q' '\'' (QS_ANGLE | QS_BRACE | QS_BRACK | QS_PAREN | QS_EXCLAM | QS_SHARP | QS_QUOTE | QS_DQUOTE) '\'' -> type(CHAR_STRING);
fragment QS_ANGLE   : '<' .*? '>';
fragment QS_BRACE   : '{' .*? '}';
fragment QS_BRACK   : '[' .*? ']';
fragment QS_PAREN   : '(' .*? ')';
fragment QS_EXCLAM  : '!' .*? '!';
fragment QS_SHARP   : '#' .*? '#';
fragment QS_QUOTE   : '\'' .*? '\'';
fragment QS_DQUOTE  : '"' .*? '"';

DELIMITED_ID: '"' (~('"' | '\r' | '\n') | '"' '"')+ '"' ;

PERCENT:                   '%';
AMPERSAND:                 '&';
LEFT_PAREN:                '(';
RIGHT_PAREN:               ')';
DOUBLE_ASTERISK:           '**';
ASTERISK:                  '*';
PLUS_SIGN:                 '+';
MINUS_SIGN:                '-';
COMMA:                     ',';
SOLIDUS:                   '/';
AT_SIGN:                   '@';
ASSIGN_OP:                 ':=';

BINDVAR
    : ':' SIMPLE_LETTER  (SIMPLE_LETTER | [0-9] | '_')*
    | ':' DELIMITED_ID  // not used in SQL but spotted in v$sqltext when using cursor_sharing
    | ':' UNSIGNED_INTEGER
    | QUESTION_MARK // not in SQL, not in Oracle, not in OCI, use this for JDBC
    ;

NOT_EQUAL_OP:              '!='
            |              '<>'
            |              '^='
            |              '~='
            ;
CARRET_OPERATOR_PART:      '^';
TILDE_OPERATOR_PART:       '~';
EXCLAMATION_OPERATOR_PART: '!';
GREATER_THAN_OP:           '>';
LESS_THAN_OP:              '<';
COLON:                     ':';
SEMICOLON:                 ';';

BAR:       '|';
EQUALS_OP: '=';

LEFT_BRACKET:  '[';
RIGHT_BRACKET: ']';

INTRODUCER: '_';

// Comments https://docs.oracle.com/cd/E11882_01/server.112/e41084/sql_elements006.htm

SINGLE_LINE_COMMENT: '--' ~('\r' | '\n')* NEWLINE_EOF                 -> channel(HIDDEN);
MULTI_LINE_COMMENT:  '/*' .*? '*/'                                    -> channel(HIDDEN);
// https://docs.oracle.com/cd/E11882_01/server.112/e16604/ch_twelve034.htm#SQPUG054
REMARK_COMMENT:      'REM' {self.IsNewlineAtPos(-4)}? 'ARK'? (' ' ~('\r' | '\n')*)? NEWLINE_EOF -> channel(HIDDEN);

// https://docs.oracle.com/cd/E11882_01/server.112/e16604/ch_twelve032.htm#SQPUG052
PROMPT_MESSAGE:      'PRO' {self.IsNewlineAtPos(-4)}? 'MPT'? (' ' ~('\r' | '\n')*)? NEWLINE_EOF;

// TODO: should starts with newline
START_CMD
    //: 'STA' 'RT'? SPACE ~('\r' | '\n')* NEWLINE_EOF
    // https://docs.oracle.com/cd/B19306_01/server.102/b14357/ch12002.htm
    // https://docs.oracle.com/cd/B19306_01/server.102/b14357/ch12003.htm
    : '@' {self.IsNewlineAtPos(-2)}? '@'? ~('\r' | '\n')* NEWLINE_EOF
    ;

REGULAR_ID: SIMPLE_LETTER (SIMPLE_LETTER | '$' | '_' | '#' | [0-9])*;

SPACES: [ \t\r\n]+ -> channel(HIDDEN);

// Fragment rules

fragment NEWLINE_EOF    : NEWLINE | EOF;
fragment QUESTION_MARK  : '?';
fragment SIMPLE_LETTER  : [A-Za-z];
fragment FLOAT_FRAGMENT : UNSIGNED_INTEGER* '.'? UNSIGNED_INTEGER+;
fragment NEWLINE        : '\r'? '\n';
fragment SPACE          : [ \t];

