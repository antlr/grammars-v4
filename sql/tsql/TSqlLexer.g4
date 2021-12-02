/*
T-SQL (Transact-SQL, MSSQL) grammar.
The MIT License (MIT).
Copyright (c) 2017, Mark Adams (madams51703@gmail.com)
Copyright (c) 2015-2017, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.
Copyright (c) 2016, Scott Ure (scott@redstormsoftware.com).
Copyright (c) 2016, Rui Zhang (ruizhang.ccs@gmail.com).
Copyright (c) 2016, Marcus Henriksson (kuseman80@gmail.com).
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

lexer grammar TSqlLexer;


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

// Basic keywords (from https://msdn.microsoft.com/en-us/library/ms189822.aspx)

ADD:                                   A D D ;
ALL:                                   A L L ;
ALTER:                                 A L T E R ;
AND:                                   A N D ;
ANY:                                   A N Y ;
AS:                                    A S ;
ASC:                                   A S C ;
AUTHORIZATION:                         A U T H O R I Z A T I O N ;
ACCELERATED_DATABASE_RECOVERY:         A C C E L E R A T E D '_' D A T A B A S E '_' R E C O V E R Y ;
BACKSLASH:                             '\\' ;
BACKUP:                                B A C K U P ;
BEGIN:                                 B E G I N ;
BETWEEN:                               B E T W E E N ;
BREAK:                                 B R E A K ;
BROWSE:                                B R O W S E ;
BULK:                                  B U L K ;
BY:                                    B Y ;
CASCADE:                               C A S C A D E ;
CASE:                                  C A S E ;
CHECK:                                 C H E C K ;
CHECKPOINT:                            C H E C K P O I N T ;
CLOSE:                                 C L O S E ;
CLUSTERED:                             C L U S T E R E D ;
COALESCE:                              C O A L E S C E ;
COLLATE:                               C O L L A T E ;
COLUMN:                                C O L U M N ;
COMMIT:                                C O M M I T ;
COMPUTE:                               C O M P U T E ;
CONSTRAINT:                            C O N S T R A I N T ;
CONTAINS:                              C O N T A I N S ;
CONTAINSTABLE:                         C O N T A I N S T A B L E ;
CONTINUE:                              C O N T I N U E ;
CONVERT:                               C O N V E R T ;
CREATE:                                C R E A T E ;
CROSS:                                 C R O S S ;
CURRENT:                               C U R R E N T ;
CURRENT_DATE:                          C U R R E N T '_' D A T E ;
CURRENT_TIME:                          C U R R E N T '_' T I M E ;
CURRENT_TIMESTAMP:                     C U R R E N T '_' T I M E S T A M P ;
CURRENT_USER:                          C U R R E N T '_' U S E R ;
CURSOR:                                C U R S O R ;
DATABASE:                              D A T A B A S E ;
DBCC:                                  D B C C ;
DEALLOCATE:                            D E A L L O C A T E ;
DECLARE:                               D E C L A R E ;
DEFAULT:                               D E F A U L T ;
DELETE:                                D E L E T E ;
DENY:                                  D E N Y ;
DESC:                                  D E S C ;
DISK:                                  D I S K ;
DISTINCT:                              D I S T I N C T ;
DISTRIBUTED:                           D I S T R I B U T E D ;
DOUBLE:                                D O U B L E ;
DOUBLE_BACK_SLASH:                     '\\\\';
DOUBLE_FORWARD_SLASH:                  '//';
DROP:                                  D R O P ;
DUMP:                                  D U M P ;
ELSE:                                  E L S E ;
END:                                   E N D ;
ERRLVL:                                E R R L V L ;
ESCAPE:                                E S C A P E ;
EXCEPT:                                E X C E P T ;
EXECUTE:                               (E X E C | E X E C U T E);
EXISTS:                                E X I S T S ;
EXIT:                                  E X I T ;
EXTERNAL:                              E X T E R N A L ;
FETCH:                                 F E T C H ;
FILE:                                  F I L E ;
FILLFACTOR:                            F I L L F A C T O R ;
FOR:                                   F O R ;
FOREIGN:                               F O R E I G N ;
FREETEXT:                              F R E E T E X T ;
FREETEXTTABLE:                         F R E E T E X T T A B L E ;
FROM:                                  F R O M ;
FULL:                                  F U L L ;
FUNCTION:                              F U N C T I O N ;
GOTO:                                  G O T O ;
GRANT:                                 G R A N T ;
GROUP:                                 G R O U P ;
HAVING:                                H A V I N G ;
HOLDLOCK:                              H O L D L O C K ;
IDENTITY:                              I D E N T I T Y ;
IDENTITY_INSERT:                       I D E N T I T Y '_' I N S E R T ;
IDENTITYCOL:                           I D E N T I T Y C O L ;
IF:                                    I F ;
IN:                                    I N ;
INDEX:                                 I N D E X ;
INNER:                                 I N N E R ;
INSERT:                                I N S E R T ;
INTERSECT:                             I N T E R S E C T ;
INTO:                                  I N T O ;
IS:                                    I S ;
JOIN:                                  J O I N ;
KEY:                                   K E Y ;
KILL:                                  K I L L ;
LEFT:                                  L E F T ;
LIKE:                                  L I K E ;
LINENO:                                L I N E N O ;
LOAD:                                  L O A D ;
MERGE:                                 M E R G E ;
NATIONAL:                              N A T I O N A L ;
NEGOTIATE:                             N E G O T I A T E ;
NOCHECK:                               N O C H E C K ;
NONCLUSTERED:                          N O N C L U S T E R E D ;
NOT:                                   N O T ;
NULL_:                                 N U L L ;
NULLIF:                                N U L L I F ;
OF:                                    O F ;
OFF:                                   O F F ;
OFFSETS:                               O F F S E T S ;
ON:                                    O N ;
OPEN:                                  O P E N ;
OPENDATASOURCE:                        O P E N D A T A S O U R C E ;
OPENQUERY:                             O P E N Q U E R Y ;
OPENROWSET:                            O P E N R O W S E T ;
OPENJSON:                              O P E N J S O N ;
OPENXML:                               O P E N X M L ;
OPTION:                                O P T I O N ;
OR:                                    O R ;
ORDER:                                 O R D E R ;
OUTER:                                 O U T E R ;
OVER:                                  O V E R ;
PERCENT:                               P E R C E N T ;
PIVOT:                                 P I V O T ;
PLAN:                                  P L A N ;
PRECISION:                             P R E C I S I O N ;
PRIMARY:                               P R I M A R Y ;
PRINT:                                 P R I N T ;
PROC:                                  P R O C ;
PROCEDURE:                             P R O C E D U R E ;
PUBLIC:                                P U B L I C ;
RAISERROR:                             R A I S E R R O R ;
RAW:                                   R A W ;
READ:                                  R E A D ;
READTEXT:                              R E A D T E X T ;
RECONFIGURE:                           R E C O N F I G U R E ;
REFERENCES:                            R E F E R E N C E S ;
REPLICATION:                           R E P L I C A T I O N ;
RESTORE:                               R E S T O R E ;
RESTRICT:                              R E S T R I C T ;
RETURN:                                R E T U R N ;
REVERT:                                R E V E R T ;
REVOKE:                                R E V O K E ;
RIGHT:                                 R I G H T ;
ROLLBACK:                              R O L L B A C K ;
ROWCOUNT:                              R O W C O U N T ;
ROWGUIDCOL:                            R O W G U I D C O L ;
RULE:                                  R U L E ;
SAVE:                                  S A V E ;
SCHEMA:                                S C H E M A ;
SECURITYAUDIT:                         S E C U R I T Y A U D I T ;
SELECT:                                S E L E C T ;
SEMANTICKEYPHRASETABLE:                S E M A N T I C K E Y P H R A S E T A B L E ;
SEMANTICSIMILARITYDETAILSTABLE:        S E M A N T I C S I M I L A R I T Y D E T A I L S T A B L E ;
SEMANTICSIMILARITYTABLE:               S E M A N T I C S I M I L A R I T Y T A B L E ;
SESSION_USER:                          S E S S I O N '_' U S E R ;
SET:                                   S E T ;
SETUSER:                               S E T U S E R ;
SHUTDOWN:                              S H U T D O W N ;
SOME:                                  S O M E ;
STATISTICS:                            S T A T I S T I C S ;
SYSTEM_USER:                           S Y S T E M '_' U S E R ;
TABLE:                                 T A B L E ;
TABLESAMPLE:                           T A B L E S A M P L E ;
TEXTSIZE:                              T E X T S I Z E ;
THEN:                                  T H E N ;
TO:                                    T O ;
TOP:                                   T O P ;
TRAN:                                  T R A N ;
TRANSACTION:                           T R A N S A C T I O N ;
TRIGGER:                               T R I G G E R ;
TRUNCATE:                              T R U N C A T E ;
TSEQUAL:                               T S E Q U A L ;
UNION:                                 U N I O N ;
UNIQUE:                                U N I Q U E ;
UNPIVOT:                               U N P I V O T ;
UPDATE:                                U P D A T E ;
UPDATETEXT:                            U P D A T E T E X T ;
USE:                                   U S E ;
USER:                                  U S E R ;
VALUES:                                V A L U E S ;
VARYING:                               V A R Y I N G ;
VIEW:                                  V I E W ;
WAITFOR:                               W A I T F O R ;
WHEN:                                  W H E N ;
WHERE:                                 W H E R E ;
WHILE:                                 W H I L E ;
WITH:                                  W I T H ;
WITHIN:                                W I T H I N ;
WRITETEXT:                             W R I T E T E X T ;
DOLLAR_PARTITION:                      '$' P A R T I T I O N ;
TRY_PARSE:                             T R Y '_' P A R S E;

//Keywords that can exist in ID etc
ABSOLUTE:                              A B S O L U T E ;
AT_KEYWORD:                            A T ;
ACCENT_SENSITIVITY:                    A C C E N T '_' S E N S I T I V I T Y ;
ACCESS:                                A C C E S S ;
ACTION:                                A C T I O N ;
ACTIVATION:                            A C T I V A T I O N ;
ACTIVE:                                A C T I V E ;
ADDRESS:                               A D D R E S S ;
AES_128:                               A E S '_128' ;
AES_192:                               A E S '_192' ;
AES_256:                               A E S '_256' ;
AFFINITY:                              A F F I N I T Y ;
AFTER:                                 A F T E R ;
AGGREGATE:                             A G G R E G A T E ;
ALGORITHM:                             A L G O R I T H M ;
ALLOW_ENCRYPTED_VALUE_MODIFICATIONS:   A L L O W '_' E N C R Y P T E D '_' V A L U E '_' M O D I F I C A T I O N S ;
ALLOW_SNAPSHOT_ISOLATION:              A L L O W '_' S N A P S H O T '_' I S O L A T I O N ;
ALLOWED:                               A L L O W E D ;
ANSI_NULL_DEFAULT:                     A N S I '_' N U L L '_' D E F A U L T ;
ANSI_NULLS:                            A N S I '_' N U L L S ;
ANSI_PADDING:                          A N S I '_' P A D D I N G ;
ANSI_WARNINGS:                         A N S I '_' W A R N I N G S ;
APPLICATION_LOG:                       A P P L I C A T I O N '_' L O G ;
APPLY:                                 A P P L Y ;
ARITHABORT:                            A R I T H A B O R T ;
ASSEMBLY:                              A S S E M B L Y ;
AUDIT:                                 A U D I T ;
AUDIT_GUID:                            A U D I T '_' G U I D ;
AUTO:                                  A U T O ;
AUTO_CLEANUP:                          A U T O '_' C L E A N U P ;
AUTO_CLOSE:                            A U T O '_' C L O S E ;
AUTO_CREATE_STATISTICS:                A U T O '_' C R E A T E '_' S T A T I S T I C S ;
AUTO_SHRINK:                           A U T O '_' S H R I N K ;
AUTO_UPDATE_STATISTICS:                A U T O '_' U P D A T E '_' S T A T I S T I C S ;
AUTO_UPDATE_STATISTICS_ASYNC:          A U T O '_' U P D A T E '_' S T A T I S T I C S '_' A S Y N C ;
AUTOGROW_ALL_FILES:                    A U T O G R O W '_' A L L '_' F I L E S ;
AUTOGROW_SINGLE_FILE:                  A U T O G R O W '_' S I N G L E '_' F I L E ;
AVAILABILITY:                          A V A I L A B I L I T Y ;
AVG:                                   A V G ;
BACKUP_PRIORITY:                       B A C K U P '_' P R I O R I T Y ;
BEGIN_DIALOG:                          B E G I N '_' D I A L O G ;
BIGINT:                                B I G I N T ;
BINARY_KEYWORD:                        B I N A R Y ;
BINARY_BASE64:                         B I N A R Y ' ' B A S E '64';
BINARY_CHECKSUM:                       B I N A R Y '_' C H E C K S U M ;
BINDING:                               B I N D I N G ;
BLOB_STORAGE:                          B L O B '_' S T O R A G E ;
BROKER:                                B R O K E R ;
BROKER_INSTANCE:                       B R O K E R '_' I N S T A N C E ;
BULK_LOGGED:                           B U L K '_' L O G G E D ;
CALLER:                                C A L L E R ;
CAP_CPU_PERCENT:                       C A P '_' C P U '_' P E R C E N T ;
CAST:                                  C A S T ;
TRY_CAST:                              T R Y '_' C A S T ;
CATALOG:                               C A T A L O G ;
CATCH:                                 C A T C H ;
CHANGE:                                C H A N G E ;
CHANGE_RETENTION:                      C H A N G E '_' R E T E N T I O N ;
CHANGE_TRACKING:                       C H A N G E '_' T R A C K I N G ;
CHECKSUM:                              C H E C K S U M ;
CHECKSUM_AGG:                          C H E C K S U M '_' A G G ;
CLEANUP:                               C L E A N U P ;
COLLECTION:                            C O L L E C T I O N ;
COLUMN_MASTER_KEY:                     C O L U M N '_' M A S T E R '_' K E Y ;
COLUMNSTORE:                           C O L U M N S T O R E ;
COMMITTED:                             C O M M I T T E D ;
COMPATIBILITY_LEVEL:                   C O M P A T I B I L I T Y '_' L E V E L ;
CONCAT:                                C O N C A T ;
CONCAT_NULL_YIELDS_NULL:               C O N C A T '_' N U L L '_' Y I E L D S '_' N U L L ;
CONTENT:                               C O N T E N T ;
CONTROL:                               C O N T R O L ;
COOKIE:                                C O O K I E ;
COUNT:                                 C O U N T ;
COUNT_BIG:                             C O U N T '_' B I G ;
COUNTER:                               C O U N T E R ;
CPU:                                   C P U ;
CREATE_NEW:                            C R E A T E '_' N E W ;
CREATION_DISPOSITION:                  C R E A T I O N '_' D I S P O S I T I O N ;
CREDENTIAL:                            C R E D E N T I A L ;
CRYPTOGRAPHIC:                         C R Y P T O G R A P H I C ;
CUME_DIST:                             C U M E '_' D I S T ;
CURSOR_CLOSE_ON_COMMIT:                C U R S O R '_' C L O S E '_' O N '_' C O M M I T ;
CURSOR_DEFAULT:                        C U R S O R '_' D E F A U L T ;
DATA:                                  D A T A ;
DATE_CORRELATION_OPTIMIZATION:         D A T E '_' C O R R E L A T I O N '_' O P T I M I Z A T I O N ;
DATEADD:                               D A T E A D D ;
DATEDIFF:                              D A T E D I F F ;
DATENAME:                              D A T E N A M E ;
DATEPART:                              D A T E P A R T ;
DAYS:                                  D A Y S ;
DB_CHAINING:                           D B '_' C H A I N I N G ;
DB_FAILOVER:                           D B '_' F A I L O V E R ;
DECRYPTION:                            D E C R Y P T I O N ;
DEFAULT_DOUBLE_QUOTE:                  ["] D E F A U L T ["] ;
DEFAULT_FULLTEXT_LANGUAGE:             D E F A U L T '_' F U L L T E X T '_' L A N G U A G E ;
DEFAULT_LANGUAGE:                      D E F A U L T '_' L A N G U A G E ;
DEFINITION:                            D E F I N I T I O N ;
DELAY:                                 D E L A Y ;
DELAYED_DURABILITY:                    D E L A Y E D '_' D U R A B I L I T Y ;
DELETED:                               D E L E T E D ;
DENSE_RANK:                            D E N S E '_' R A N K ;
DEPENDENTS:                            D E P E N D E N T S ;
DES:                                   D E S ;
DESCRIPTION:                           D E S C R I P T I O N ;
DESX:                                  D E S X ;
DHCP:                                  D H C P ;
DIALOG:                                D I A L O G ;
DIRECTORY_NAME:                        D I R E C T O R Y '_' N A M E ;
DISABLE:                               D I S A B L E ;
DISABLE_BROKER:                        D I S A B L E '_' B R O K E R ;
DISABLED:                              D I S A B L E D ;
DISK_DRIVE:                            [A-Za-z] [:] ;
DOCUMENT:                              D O C U M E N T ;
DYNAMIC:                               D Y N A M I C ;
ELEMENTS:                              E L E M E N T S ;
EMERGENCY:                             E M E R G E N C Y ;
EMPTY_:                                 E M P T Y ;
ENABLE:                                E N A B L E ;
ENABLE_BROKER:                         E N A B L E '_' B R O K E R ;
ENCRYPTED_VALUE:                       E N C R Y P T E D '_' V A L U E ;
ENCRYPTION:                            E N C R Y P T I O N ;
ENDPOINT_URL:                          E N D P O I N T '_' U R L ;
ERROR_BROKER_CONVERSATIONS:            E R R O R '_' B R O K E R '_' C O N V E R S A T I O N S ;
EXCLUSIVE:                             E X C L U S I V E ;
EXECUTABLE:                            E X E C U T A B L E ;
EXIST:                                 E X I S T ;
EXPAND:                                E X P A N D ;
EXPIRY_DATE:                           E X P I R Y '_' D A T E ;
EXPLICIT:                              E X P L I C I T ;
FAIL_OPERATION:                        F A I L '_' O P E R A T I O N ;
FAILOVER_MODE:                         F A I L O V E R '_' M O D E ;
FAILURE:                               F A I L U R E ;
FAILURE_CONDITION_LEVEL:               F A I L U R E '_' C O N D I T I O N '_' L E V E L ;
FAST:                                  F A S T ;
FAST_FORWARD:                          F A S T '_' F O R W A R D ;
FILEGROUP:                             F I L E G R O U P ;
FILEGROWTH:                            F I L E G R O W T H ;
FILENAME:                              F I L E N A M E ;
FILEPATH:                              F I L E P A T H ;
FILESTREAM:                            F I L E S T R E A M ;
FILTER:                                F I L T E R ;
FIRST:                                 F I R S T ;
FIRST_VALUE:                           F I R S T '_' V A L U E ;
FOLLOWING:                             F O L L O W I N G ;
FORCE:                                 F O R C E ;
FORCE_FAILOVER_ALLOW_DATA_LOSS:        F O R C E '_' F A I L O V E R '_' A L L O W '_' D A T A '_' L O S S ;
FORCED:                                F O R C E D ;
FORMAT:                                F O R M A T ;
FORWARD_ONLY:                          F O R W A R D '_' O N L Y ;
FULLSCAN:                              F U L L S C A N ;
FULLTEXT:                              F U L L T E X T ;
GB:                                    G B ;
GETDATE:                               G E T D A T E ;
GETUTCDATE:                            G E T U T C D A T E ;
GLOBAL:                                G L O B A L ;
GO_BATCH:                              [\r\n] *   [\t] *   G O [\t\n\r] ;
GO:                                    G O ;
GROUP_MAX_REQUESTS:                    G R O U P '_' M A X '_' R E Q U E S T S ;
GROUPING:                              G R O U P I N G ;
GROUPING_ID:                           G R O U P I N G '_' I D ;
HADR:                                  H A D R ;
HASH:                                  H A S H ;
HEALTH_CHECK_TIMEOUT:                  H E A L T H '_' C H E C K '_' T I M E O U T ;
HIGH:                                  H I G H ;
HONOR_BROKER_PRIORITY:                 H O N O R '_' B R O K E R '_' P R I O R I T Y ;
HOURS:                                 H O U R S ;
IDENTITY_VALUE:                        I D E N T I T Y '_' V A L U E ;
IGNORE_NONCLUSTERED_COLUMNSTORE_INDEX: I G N O R E '_' N O N C L U S T E R E D '_' C O L U M N S T O R E '_' I N D E X ;
IMMEDIATE:                             I M M E D I A T E ;
IMPERSONATE:                           I M P E R S O N A T E ;
IMPORTANCE:                            I M P O R T A N C E ;
INCLUDE_NULL_VALUES:                   I N C L U D E '_' N U L L '_' V A L U E S ;
INCREMENTAL:                           I N C R E M E N T A L ;
INITIATOR:                             I N I T I A T O R ;
INPUT:                                 I N P U T ;
INSENSITIVE:                           I N S E N S I T I V E ;
INSERTED:                              I N S E R T E D ;
INT:                                   I N T ;
IP:                                    I P ;
ISOLATION:                             I S O L A T I O N ;
JOB:                                   J O B ;
JSON:                                  J S O N ;
KB:                                    K B ;
KEEP:                                  K E E P ;
KEEPFIXED:                             K E E P F I X E D ;
KEY_SOURCE:                            K E Y '_' S O U R C E ;
KEYS:                                  K E Y S ;
KEYSET:                                K E Y S E T ;
LAG:                                   L A G ;
LAST:                                  L A S T ;
LAST_VALUE:                            L A S T '_' V A L U E ;
LEAD:                                  L E A D ;
LEVEL:                                 L E V E L ;
LIST:                                  L I S T ;
LISTENER:                              L I S T E N E R ;
LISTENER_URL:                          L I S T E N E R '_' U R L ;
LOB_COMPACTION:                        L O B '_' C O M P A C T I O N ;
LOCAL:                                 L O C A L ;
LOCATION:                              L O C A T I O N ;
LOCK:                                  L O C K ;
LOCK_ESCALATION:                       L O C K '_' E S C A L A T I O N ;
LOGIN:                                 L O G I N ;
LOOP:                                  L O O P ;
LOW:                                   L O W ;
MANUAL:                                M A N U A L ;
MARK:                                  M A R K ;
MATERIALIZED:                          M A T E R I A L I Z E D ;
MAX:                                   M A X ;
MAX_CPU_PERCENT:                       M A X '_' C P U '_' P E R C E N T ;
MAX_DOP:                               M A X '_' D O P ;
MAX_FILES:                             M A X '_' F I L E S ;
MAX_IOPS_PER_VOLUME:                   M A X '_' I O P S '_' P E R '_' V O L U M E ;
MAX_MEMORY_PERCENT:                    M A X '_' M E M O R Y '_' P E R C E N T ;
MAX_PROCESSES:                         M A X '_' P R O C E S S E S ;
MAX_QUEUE_READERS:                     M A X '_' Q U E U E '_' R E A D E R S ;
MAX_ROLLOVER_FILES:                    M A X '_' R O L L O V E R '_' F I L E S ;
MAXDOP:                                M A X D O P ;
MAXRECURSION:                          M A X R E C U R S I O N ;
MAXSIZE:                               M A X S I Z E ;
MB:                                    M B ;
MEDIUM:                                M E D I U M ;
MEMORY_OPTIMIZED_DATA:                 M E M O R Y '_' O P T I M I Z E D '_' D A T A ;
MESSAGE:                               M E S S A G E ;
MIN:                                   M I N ;
MIN_ACTIVE_ROWVERSION:                 M I N '_' A C T I V E '_' R O W V E R S I O N ;
MIN_CPU_PERCENT:                       M I N '_' C P U '_' P E R C E N T ;
MIN_IOPS_PER_VOLUME:                   M I N '_' I O P S '_' P E R '_' V O L U M E ;
MIN_MEMORY_PERCENT:                    M I N '_' M E M O R Y '_' P E R C E N T ;
MINUTES:                               M I N U T E S ;
MIRROR_ADDRESS:                        M I R R O R '_' A D D R E S S ;
MIXED_PAGE_ALLOCATION:                 M I X E D '_' P A G E '_' A L L O C A T I O N ;
MODE:                                  M O D E ;
MODIFY:                                M O D I F Y ;
MOVE:                                  M O V E ;
MULTI_USER:                            M U L T I '_' U S E R ;
NAME:                                  N A M E ;
NESTED_TRIGGERS:                       N E S T E D '_' T R I G G E R S ;
NEW_ACCOUNT:                           N E W '_' A C C O U N T ;
NEW_BROKER:                            N E W '_' B R O K E R ;
NEW_PASSWORD:                          N E W '_' P A S S W O R D ;
NEWNAME:                               N E W N A M E ;
NEXT:                                  N E X T ;
NO:                                    N O ;
NO_TRUNCATE:                           N O '_' T R U N C A T E ;
NO_WAIT:                               N O '_' W A I T ;
NOCOUNT:                               N O C O U N T ;
NODES:                                 N O D E S ;
NOEXPAND:                              N O E X P A N D ;
NON_TRANSACTED_ACCESS:                 N O N '_' T R A N S A C T E D '_' A C C E S S ;
NORECOMPUTE:                           N O R E C O M P U T E ;
NORECOVERY:                            N O R E C O V E R Y ;
NOTIFICATIONS:                         N O T I F I C A T I O N S ;
NOWAIT:                                N O W A I T ;
NTILE:                                 N T I L E ;
NUMANODE:                              N U M A N O D E ;
NUMBER:                                N U M B E R ;
NUMERIC_ROUNDABORT:                    N U M E R I C '_' R O U N D A B O R T ;
OBJECT:                                O B J E C T ;
OFFLINE:                               O F F L I N E ;
OFFSET:                                O F F S E T ;
OLD_ACCOUNT:                           O L D '_' A C C O U N T ;
ONLINE:                                O N L I N E ;
ONLY:                                  O N L Y ;
OPEN_EXISTING:                         O P E N '_' E X I S T I N G ;
OPTIMISTIC:                            O P T I M I S T I C ;
OPTIMIZE:                              O P T I M I Z E ;
OUT:                                   O U T ;
OUTPUT:                                O U T P U T ;
OVERRIDE:                              O V E R R I D E ;
OWNER:                                 O W N E R ;
OWNERSHIP:                             O W N E R S H I P ;
PAGE_VERIFY:                           P A G E '_' V E R I F Y ;
PARAMETERIZATION:                      P A R A M E T E R I Z A T I O N ;
PARTITION:                             P A R T I T I O N ;
PARTITIONS:                            P A R T I T I O N S ;
PARTNER:                               P A R T N E R ;
PATH:                                  P A T H ;
PERCENT_RANK:                          P E R C E N T '_' R A N K ;
PERCENTILE_CONT:                       P E R C E N T I L E '_' C O N T ;
PERCENTILE_DISC:                       P E R C E N T I L E '_' D I S C ;
POISON_MESSAGE_HANDLING:               P O I S O N '_' M E S S A G E '_' H A N D L I N G ;
POOL:                                  P O O L ;
PORT:                                  P O R T ;
PRECEDING:                             P R E C E D I N G ;
PRIMARY_ROLE:                          P R I M A R Y '_' R O L E ;
PRIOR:                                 P R I O R ;
PRIORITY:                              P R I O R I T Y ;
PRIORITY_LEVEL:                        P R I O R I T Y '_' L E V E L ;
PRIVATE:                               P R I V A T E ;
PRIVATE_KEY:                           P R I V A T E '_' K E Y ;
PRIVILEGES:                            P R I V I L E G E S ;
PROCEDURE_NAME:                        P R O C E D U R E '_' N A M E ;
PROPERTY:                              P R O P E R T Y ;
PROVIDER:                              P R O V I D E R ;
PROVIDER_KEY_NAME:                     P R O V I D E R '_' K E Y '_' N A M E ;
QUERY:                                 Q U E R Y ;
QUEUE:                                 Q U E U E ;
QUEUE_DELAY:                           Q U E U E '_' D E L A Y ;
QUOTED_IDENTIFIER:                     Q U O T E D '_' I D E N T I F I E R ;
RANGE:                                 R A N G E ;
RANK:                                  R A N K ;
RC2:                                   R C '2' ;
RC4:                                   R C '4' ;
RC4_128:                               R C '4_128' ;
READ_COMMITTED_SNAPSHOT:               R E A D '_' C O M M I T T E D '_' S N A P S H O T ;
READ_ONLY:                             R E A D '_' O N L Y ;
READ_ONLY_ROUTING_LIST:                R E A D '_' O N L Y '_' R O U T I N G '_' L I S T ;
READ_WRITE:                            R E A D '_' W R I T E ;
READONLY:                              R E A D O N L Y ;
READWRITE:                             R E A D W R I T E ;
REBUILD:                               R E B U I L D ;
RECEIVE:                               R E C E I V E ;
RECOMPILE:                             R E C O M P I L E ;
RECOVERY:                              R E C O V E R Y ;
RECURSIVE_TRIGGERS:                    R E C U R S I V E '_' T R I G G E R S ;
RELATIVE:                              R E L A T I V E ;
REMOTE:                                R E M O T E ;
REMOTE_SERVICE_NAME:                   R E M O T E '_' S E R V I C E '_' N A M E ;
REMOVE:                                R E M O V E ;
REORGANIZE:                            R E O R G A N I Z E ;
REPEATABLE:                            R E P E A T A B L E ;
REPLICA:                               R E P L I C A ;
REQUEST_MAX_CPU_TIME_SEC:              R E Q U E S T '_' M A X '_' C P U '_' T I M E '_' S E C ;
REQUEST_MAX_MEMORY_GRANT_PERCENT:      R E Q U E S T '_' M A X '_' M E M O R Y '_' G R A N T '_' P E R C E N T ;
REQUEST_MEMORY_GRANT_TIMEOUT_SEC:      R E Q U E S T '_' M E M O R Y '_' G R A N T '_' T I M E O U T '_' S E C ;
REQUIRED_SYNCHRONIZED_SECONDARIES_TO_COMMIT :   R E Q U I R E D '_' S Y N C H R O N I Z E D '_' S E C O N D A R I E S '_' T O '_' C O M M I T ;
RESERVE_DISK_SPACE:                    R E S E R V E '_' D I S K '_' S P A C E ;
RESOURCE:                              R E S O U R C E ;
RESOURCE_MANAGER_LOCATION:             R E S O U R C E '_' M A N A G E R '_' L O C A T I O N ;
RESTRICTED_USER:                       R E S T R I C T E D '_' U S E R ;
RETENTION:                             R E T E N T I O N ;
ROBUST:                                R O B U S T ;
ROOT:                                  R O O T ;
ROUTE:                                 R O U T E ;
ROW:                                   R O W ;
ROW_NUMBER:                            R O W '_' N U M B E R ;
ROWGUID:                               R O W G U I D ;
ROWS:                                  R O W S ;
SAMPLE:                                S A M P L E ;
SCHEMABINDING:                         S C H E M A B I N D I N G ;
SCOPED:                                S C O P E D ;
SCROLL:                                S C R O L L ;
SCROLL_LOCKS:                          S C R O L L '_' L O C K S ;
SEARCH:                                S E A R C H ;
SECONDARY:                             S E C O N D A R Y ;
SECONDARY_ONLY:                        S E C O N D A R Y '_' O N L Y ;
SECONDARY_ROLE:                        S E C O N D A R Y '_' R O L E ;
SECONDS:                               S E C O N D S ;
SECRET:                                S E C R E T ;
SECURABLES:                            S E C U R A B L E S ;
SECURITY:                              S E C U R I T Y ;
SECURITY_LOG:                          S E C U R I T Y '_' L O G ;
SEEDING_MODE:                          S E E D I N G '_' M O D E ;
SELF:                                  S E L F ;
SEMI_SENSITIVE:                        S E M I '_' S E N S I T I V E ;
SEND:                                  S E N D ;
SENT:                                  S E N T ;
SEQUENCE:                              S E Q U E N C E ;
SERIALIZABLE:                          S E R I A L I Z A B L E ;
SESSION_TIMEOUT:                       S E S S I O N '_' T I M E O U T ;
SETERROR:                              S E T E R R O R ;
SHARE:                                 S H A R E ;
SHOWPLAN:                              S H O W P L A N ;
SIGNATURE:                             S I G N A T U R E ;
SIMPLE:                                S I M P L E ;
SINGLE_USER:                           S I N G L E '_' U S E R ;
SIZE:                                  S I Z E ;
SMALLINT:                              S M A L L I N T ;
SNAPSHOT:                              S N A P S H O T ;
SPATIAL_WINDOW_MAX_CELLS:              S P A T I A L '_' W I N D O W '_' M A X '_' C E L L S ;
STANDBY:                               S T A N D B Y ;
START_DATE:                            S T A R T '_' D A T E ;
STATIC:                                S T A T I C ;
STATS_STREAM:                          S T A T S '_' S T R E A M ;
STATUS:                                S T A T U S ;
STATUSONLY:                            S T A T U S O N L Y ;
STDEV:                                 S T D E V ;
STDEVP:                                S T D E V P ;
STOPLIST:                              S T O P L I S T ;
STRING_AGG:                            S T R I N G '_' A G G ;
STUFF:                                 S T U F F ;
SUBJECT:                               S U B J E C T ;
SUBSCRIBE:                             S U B S C R I B E ;
SUBSCRIPTION:                          S U B S C R I P T I O N ;
SUM:                                   S U M ;
SUSPEND:                               S U S P E N D ;
SYMMETRIC:                             S Y M M E T R I C ;
SYNCHRONOUS_COMMIT:                    S Y N C H R O N O U S '_' C O M M I T ;
SYNONYM:                               S Y N O N Y M ;
SYSTEM:                                S Y S T E M ;
TAKE:                                  T A K E ;
TARGET_RECOVERY_TIME:                  T A R G E T '_' R E C O V E R Y '_' T I M E ;
TB:                                    T B ;
TEXTIMAGE_ON:                          T E X T I M A G E '_' O N ;
THROW:                                 T H R O W ;
TIES:                                  T I E S ;
TIME:                                  T I M E ;
TIMEOUT:                               T I M E O U T ;
TIMER:                                 T I M E R ;
TINYINT:                               T I N Y I N T ;
TORN_PAGE_DETECTION:                   T O R N '_' P A G E '_' D E T E C T I O N ;
TRACKING:                              T R A C K I N G ;
TRANSFORM_NOISE_WORDS:                 T R A N S F O R M '_' N O I S E '_' W O R D S ;
TRIPLE_DES:                            T R I P L E '_' D E S ;
TRIPLE_DES_3KEY:                       T R I P L E '_' D E S '_' '3'  K E Y ;
TRUSTWORTHY:                           T R U S T W O R T H Y ;
TRY:                                   T R Y ;
TSQL:                                  T S Q L ;
TWO_DIGIT_YEAR_CUTOFF:                 T W O '_' D I G I T '_' Y E A R '_' C U T O F F ;
TYPE:                                  T Y P E ;
TYPE_WARNING:                          T Y P E '_' W A R N I N G ;
UNBOUNDED:                             U N B O U N D E D ;
UNCOMMITTED:                           U N C O M M I T T E D ;
UNKNOWN:                               U N K N O W N ;
UNLIMITED:                             U N L I M I T E D ;
UNMASK:                                U N M A S K ;
UOW:                                   U O W ;
USING:                                 U S I N G ;
VALID_XML:                             V A L I D '_' X M L ;
VALIDATION:                            V A L I D A T I O N ;
VALUE:                                 V A L U E ;
VAR:                                   V A R ;
VARBINARY_KEYWORD:                     V A R B I N A R Y ;
VARP:                                  V A R P ;
VIEW_METADATA:                         V I E W '_' M E T A D A T A ;
VIEWS:                                 V I E W S ;
WAIT:                                  W A I T ;
WELL_FORMED_XML:                       W E L L '_' F O R M E D '_' X M L ;
WITHOUT_ARRAY_WRAPPER:                 W I T H O U T '_' A R R A Y '_' W R A P P E R ;
WORK:                                  W O R K ;
WORKLOAD:                              W O R K L O A D ;
XML:                                   X M L ;
XMLDATA:                               X M L D A T A ;
XMLNAMESPACES:                         X M L N A M E S P A C E S ;
XMLSCHEMA:                             X M L S C H E M A ;
XSINIL:                                X S I N I L ;
ZONE:                                  Z O N E ;
DOLLAR_ACTION:                         '$' A C T I O N ;

//More keywords
ABORT_AFTER_WAIT:                      A B O R T '_' A F T E R '_' W A I T ;
ABSENT:                                A B S E N T ;
ADMINISTER:                            A D M I N I S T E R ;
AES:                                   A E S ;
ALLOW_CONNECTIONS:                     A L L O W '_' C O N N E C T I O N S ;
ALLOW_MULTIPLE_EVENT_LOSS:             A L L O W '_' M U L T I P L E '_' E V E N T '_' L O S S ;
ALLOW_SINGLE_EVENT_LOSS:               A L L O W '_' S I N G L E '_' E V E N T '_' L O S S ;
ANONYMOUS:                             A N O N Y M O U S ;
APPEND:                                A P P E N D ;
APPLICATION:                           A P P L I C A T I O N ;
ASYMMETRIC:                            A S Y M M E T R I C ;
ASYNCHRONOUS_COMMIT:                   A S Y N C H R O N O U S '_' C O M M I T ;
AUTHENTICATE:                          A U T H E N T I C A T E ;
AUTHENTICATION:                        A U T H E N T I C A T I O N ;
AUTOMATED_BACKUP_PREFERENCE:           A U T O M A T E D '_' B A C K U P '_' P R E F E R E N C E ;
AUTOMATIC:                             A U T O M A T I C ;
AVAILABILITY_MODE:                     A V A I L A B I L I T Y '_' M O D E ;
BEFORE:                                B E F O R E ;
BLOCK:                                 B L O C K ;
BLOCKERS:                              B L O C K E R S ;
BLOCKSIZE:                             B L O C K S I Z E ;
BLOCKING_HIERARCHY:                    B L O C K I N G '_' H I E R A R C H Y ;
BUFFER:                                B U F F E R ;
BUFFERCOUNT:                           B U F F E R C O U N T ;
CACHE:                                 C A C H E ;
CALLED:                                C A L L E D ;
CERTIFICATE:                           C E R T I F I C A T E ;
CHANGETABLE:                           C H A N G E T A B L E ;
CHANGES:                               C H A N G E S ;
CHECK_POLICY:                          C H E C K '_' P O L I C Y ;
CHECK_EXPIRATION:                      C H E C K '_' E X P I R A T I O N ;
CLASSIFIER_FUNCTION:                   C L A S S I F I E R '_' F U N C T I O N ;
CLUSTER:                               C L U S T E R ;
COMPRESSION:                           C O M P R E S S I O N ;
CONNECT:                               C O N N E C T ;
CONNECTION:                            C O N N E C T I O N ;
CONFIGURATION:                         C O N F I G U R A T I O N ;
CONTAINMENT:                           C O N T A I N M E N T ;
CONTEXT:                               C O N T E X T ;
CONTINUE_AFTER_ERROR:                  C O N T I N U E '_' A F T E R '_' E R R O R ;
CONTRACT:                              C O N T R A C T ;
CONTRACT_NAME:                         C O N T R A C T '_' N A M E ;
CONVERSATION:                          C O N V E R S A T I O N ;
COPY_ONLY:                             C O P Y '_' O N L Y ;
CYCLE:                                 C Y C L E ;
DATA_COMPRESSION:                      D A T A '_' C O M P R E S S I O N ;
DATA_SOURCE:                           D A T A '_' S O U R C E ;
DATABASE_MIRRORING:                    D A T A B A S E '_' M I R R O R I N G ;
DATASPACE:                             D A T A S P A C E ;
DDL:                                   D D L ;
DEFAULT_DATABASE:                      D E F A U L T '_' D A T A B A S E ;
DEFAULT_SCHEMA:                        D E F A U L T '_' S C H E M A ;
DIAGNOSTICS:                           D I A G N O S T I C S ;
DIFFERENTIAL:                          D I F F E R E N T I A L ;
DTC_SUPPORT:                           D T C '_' S U P P O R T ;
ENABLED:                               E N A B L E D ;
ENDPOINT:                              E N D P O I N T ;
ERROR:                                 E R R O R ;
EVENT:                                 E V E N T ;
EVENTDATA:                             E V E N T D A T A   (   ) ;
EVENT_RETENTION_MODE:                  E V E N T '_' R E T E N T I O N '_' M O D E ;
EXECUTABLE_FILE:                       E X E C U T A B L E '_' F I L E ;
EXPIREDATE:                            E X P I R E D A T E ;
EXTENSION:                             E X T E N S I O N ;
EXTERNAL_ACCESS:                       E X T E R N A L '_' A C C E S S ;
FAILOVER:                              F A I L O V E R ;
FAILURECONDITIONLEVEL:                 F A I L U R E C O N D I T I O N L E V E L ;
FAN_IN:                                F A N '_' I N ;
FILE_SNAPSHOT:                         F I L E '_' S N A P S H O T ;
FORCESEEK:                             F O R C E S E E K ;
FORCE_SERVICE_ALLOW_DATA_LOSS:         F O R C E '_' S E R V I C E '_' A L L O W '_' D A T A '_' L O S S ;
GET:                                   G E T ;
GOVERNOR:                              G O V E R N O R ;
HASHED:                                H A S H E D ;
HEALTHCHECKTIMEOUT:                    H E A L T H C H E C K T I M E O U T ;
IIF:                                   I I F ;
IO:                                    I O ;
INCLUDE:                               I N C L U D E ;
INCREMENT:                             I N C R E M E N T ;
INFINITE:                              I N F I N I T E ;
INIT:                                  I N I T ;
INSTEAD:                               I N S T E A D ;
ISNULL:                                I S N U L L ;
KERBEROS:                              K E R B E R O S ;
KEY_PATH:                              K E Y '_' P A T H ;
KEY_STORE_PROVIDER_NAME:               K E Y '_' S T O R E '_' P R O V I D E R '_' N A M E ;
LANGUAGE:                              L A N G U A G E ;
LIBRARY:                               L I B R A R Y ;
LIFETIME:                              L I F E T I M E ;
LINKED:                                L I N K E D ;
LINUX:                                 L I N U X ;
LISTENER_IP:                           L I S T E N E R '_' I P ;
LISTENER_PORT:                         L I S T E N E R '_' P O R T ;
LOCAL_SERVICE_NAME:                    L O C A L '_' S E R V I C E '_' N A M E ;
LOG:                                   L O G ;
MASK:                                  M A S K ;
MATCHED:                               M A T C H E D ;
MASTER:                                M A S T E R ;
MAX_MEMORY:                            M A X '_' M E M O R Y ;
MAXTRANSFER:                           M A X T R A N S F E R ;
MAXVALUE:                              M A X V A L U E ;
MAX_DISPATCH_LATENCY:                  M A X '_' D I S P A T C H '_' L A T E N C Y ;
MAX_DURATION:                          M A X '_' D U R A T I O N ;
MAX_EVENT_SIZE:                        M A X '_' E V E N T '_' S I Z E ;
MAX_SIZE:                              M A X '_' S I Z E ;
MAX_OUTSTANDING_IO_PER_VOLUME:         M A X '_' O U T S T A N D I N G '_' I O '_' P E R '_' V O L U M E ;
MEDIADESCRIPTION:                      M E D I A D E S C R I P T I O N ;
MEDIANAME:                             M E D I A N A M E ;
MEMBER:                                M E M B E R ;
MEMORY_PARTITION_MODE:                 M E M O R Y '_' P A R T I T I O N '_' M O D E ;
MESSAGE_FORWARDING:                    M E S S A G E '_' F O R W A R D I N G ;
MESSAGE_FORWARD_SIZE:                  M E S S A G E '_' F O R W A R D '_' S I Z E ;
MINVALUE:                              M I N V A L U E ;
MIRROR:                                M I R R O R ;
MUST_CHANGE:                           M U S T '_' C H A N G E ;
NOFORMAT:                              N O F O R M A T ;
NOINIT:                                N O I N I T ;
NONE:                                  N O N E ;
NOREWIND:                              N O R E W I N D ;
NOSKIP:                                N O S K I P ;
NOUNLOAD:                              N O U N L O A D ;
NO_CHECKSUM:                           N O '_' C H E C K S U M ;
NO_COMPRESSION:                        N O '_' C O M P R E S S I O N ;
NO_EVENT_LOSS:                         N O '_' E V E N T '_' L O S S ;
NOTIFICATION:                          N O T I F I C A T I O N ;
NTLM:                                  N T L M ;
OLD_PASSWORD:                          O L D '_' P A S S W O R D ;
ON_FAILURE:                            O N '_' F A I L U R E ;
OPERATIONS:                            O P E R A T I O N S ;
PAGE:                                  P A G E ;
PARAM_NODE:                            P A R A M '_' N O D E ;
PARTIAL:                               P A R T I A L ;
PASSWORD:                              P A S S W O R D ;
PERMISSION_SET:                        P E R M I S S I O N '_' S E T ;
PER_CPU:                               P E R '_' C P U ;
PER_DB:                                P E R '_' D B ;
PER_NODE:                              P E R '_' N O D E ;
PERSISTED:                             P E R S I S T E D ;
PLATFORM:                              P L A T F O R M ;
POLICY:                                P O L I C Y ;
PREDICATE:                             P R E D I C A T E ;
PROCESS:                               P R O C E S S ;
PROFILE:                               P R O F I L E ;
PYTHON:                                P Y T H O N ;
R_:                                      R ;
READ_WRITE_FILEGROUPS:                 R E A D '_' W R I T E '_' F I L E G R O U P S ;
REGENERATE:                            R E G E N E R A T E ;
RELATED_CONVERSATION:                  R E L A T E D '_' C O N V E R S A T I O N ;
RELATED_CONVERSATION_GROUP:            R E L A T E D '_' C O N V E R S A T I O N '_' G R O U P ;
REQUIRED:                              R E Q U I R E D ;
RESET:                                 R E S E T ;
RESOURCES:                             R E S O U R C E S ;
RESTART:                               R E S T A R T ;
RESUME:                                R E S U M E ;
RETAINDAYS:                            R E T A I N D A Y S ;
RETURNS:                               R E T U R N S ;
REWIND:                                R E W I N D ;
ROLE:                                  R O L E ;
RSA_512:                               R S A '_512';
RSA_1024:                              R S A '_1024';
RSA_2048:                              R S A '_2048';
RSA_3072:                              R S A '_3072';
RSA_4096:                              R S A '_4096';
SAFETY:                                S A F E T Y ;
SAFE:                                  S A F E ;
SCHEDULER:                             S C H E D U L E R ;
SCHEME:                                S C H E M E ;
SCRIPT:                                S C R I P T ;
SERVER:                                S E R V E R ;
SERVICE:                               S E R V I C E ;
SERVICE_BROKER:                        S E R V I C E '_' B R O K E R ;
SERVICE_NAME:                          S E R V I C E '_' N A M E ;
SESSION:                               S E S S I O N ;
SETTINGS:                              S E T T I N G S ;
SHRINKLOG:                             S H R I N K L O G ;
SID:                                   S I D ;
SKIP_KEYWORD:                          S K I P ;
SOFTNUMA:                              S O F T N U M A ;
SOURCE:                                S O U R C E ;
SPECIFICATION:                         S P E C I F I C A T I O N ;
SPLIT:                                 S P L I T ;
SQL:                                   S Q L ;
SQLDUMPERFLAGS:                        S Q L D U M P E R F L A G S ;
SQLDUMPERPATH:                         S Q L D U M P E R P A T H ;
SQLDUMPERTIMEOUT:                      S Q L D U M P E R T I M E O U T S ;
STATE:                                 S T A T E ;
STATS:                                 S T A T S ;
START:                                 S T A R T ;
STARTED:                               S T A R T E D ;
STARTUP_STATE:                         S T A R T U P '_' S T A T E ;
STOP:                                  S T O P ;
STOPPED:                               S T O P P E D ;
STOP_ON_ERROR:                         S T O P '_' O N '_' E R R O R ;
SUPPORTED:                             S U P P O R T E D ;
SWITCH:                                S W I T C H ;
TAPE:                                  T A P E ;
TARGET:                                T A R G E T ;
TCP:                                   T C P ;
TRACE:                                 T R A C E ;
TRACK_CAUSALITY:                       T R A C K '_' C A U S A L I T Y ;
TRANSFER:                              T R A N S F E R ;
UNCHECKED:                             U N C H E C K E D ;
UNLOCK:                                U N L O C K ;
UNSAFE:                                U N S A F E ;
URL:                                   U R L ;
USED:                                  U S E D ;
VERBOSELOGGING:                        V E R B O S E L O G G I N G ;
VISIBILITY:                            V I S I B I L I T Y ;
WAIT_AT_LOW_PRIORITY:                  W A I T '_' A T '_' L O W '_' P R I O R I T Y ;
WINDOWS:                               W I N D O W S ;
WITHOUT:                               W I T H O U T ;
WITNESS:                               W I T N E S S ;
XACT_ABORT:                            X A C T '_' A B O R T ;
PAUSE :                                P A U S E ;
ABORT:                                 A B O R T ;
ANSI_DEFAULTS:                         A N S I '_' D E F A U L T S ;
ANSI_NULL_DFLT_OFF :                   A N S I '_' N U L L '_' D F L T '_' O F F ;
ANSI_NULL_DFLT_ON :                    A N S I '_' N U L L '_' D F L T '_' O N ;
ARITHIGNORE :                          A R I T H I G N O R E ;
FMTONLY :                              F M T O N L Y ;
FORCEPLAN :                            F O R C E P L A N ;
IMPLICIT_TRANSACTIONS :                I M P L I C I T '_' T R A N S A C T I O N S ;
PARSEONLY :                            P A R S E O N L Y ;
NOEXEC :                               N O E X E C ;
REMOTE_PROC_TRANSACTIONS:              R E M O T E '_' P R O C '_' T R A N S A C T I O N S ;
SHOWPLAN_ALL :                         S H O W P L A N '_' A L L ;
SHOWPLAN_TEXT :                        S H O W P L A N '_' T E X T ;
SHOWPLAN_XML :                         S H O W P L A N '_' X M L ;
IPV4_ADDR :                            I P V '4' '_' A D D R ;
IPV6_ADDR :                            I P V '6' '_' A D D R ;
VERSION :                              V E R S I O N ;


//Build-ins:
VARCHAR:                               V A R C H A R;
NVARCHAR:                              N V A R C H A R;


SPACE:              [ \t\r\n]+    -> skip;
// https://docs.microsoft.com/en-us/sql/t-sql/language-elements/slash-star-comment-transact-sql
COMMENT:            '/*' (COMMENT | .)*? '*/' -> channel(HIDDEN);
LINE_COMMENT:       '--' ~[\r\n]* -> channel(HIDDEN);

// TODO: ID can be not only Latin.
DOUBLE_QUOTE_ID:    '"' ~'"'+ '"';
SINGLE_QUOTE:       '\'';
SQUARE_BRACKET_ID:  '[' (~']' | ']' ']')* ']';
LOCAL_ID:           '@' ([A-Za-z_$@#0-9] | FullWidthLetter)*;
DECIMAL:             DEC_DIGIT+;
ID:                  ( [A-Za-z_#] | FullWidthLetter) ( [A-Za-z_#$@0-9] | FullWidthLetter )*;
STRING:              'N'? '\'' (~'\'' | '\'\'')* '\'';
BINARY:              '0' ('X'|'x') HEX_DIGIT*;
FLOAT:               DEC_DOT_DEC;
REAL:                (DECIMAL | DEC_DOT_DEC) (('E'|'e') [+-]? DEC_DIGIT+);

EQUAL:               '=';

GREATER:             '>';
LESS:                '<';
EXCLAMATION:         '!';

PLUS_ASSIGN:         '+=';
MINUS_ASSIGN:        '-=';
MULT_ASSIGN:         '*=';
DIV_ASSIGN:          '/=';
MOD_ASSIGN:          '%=';
AND_ASSIGN:          '&=';
XOR_ASSIGN:          '^=';
OR_ASSIGN:           '|=';

DOUBLE_BAR:          '||';
DOT:                 '.';
UNDERLINE:           '_';
AT:                  '@';
SHARP:               '#';
DOLLAR:              '$';
LR_BRACKET:          '(';
RR_BRACKET:          ')';
COMMA:               ',';
SEMI:                ';';
COLON:               ':';
DOUBLE_COLON:        '::';
STAR:                '*';
DIVIDE:              '/';
MODULE:              '%';
PLUS:                '+';
MINUS:               '-';
BIT_NOT:             '~';
BIT_OR:              '|';
BIT_AND:             '&';
BIT_XOR:             '^';

fragment LETTER:       [A-Za-z_];
fragment DEC_DOT_DEC:  (DEC_DIGIT+ '.' DEC_DIGIT+ |  DEC_DIGIT+ '.' | '.' DEC_DIGIT+);
fragment HEX_DIGIT:    [0-9A-Fa-f];
fragment DEC_DIGIT:    [0-9];


fragment FullWidthLetter
    : '\u00c0'..'\u00d6'
    | '\u00d8'..'\u00f6'
    | '\u00f8'..'\u00ff'
    | '\u0100'..'\u1fff'
    | '\u2c00'..'\u2fff'
    | '\u3040'..'\u318f'
    | '\u3300'..'\u337f'
    | '\u3400'..'\u3fff'
    | '\u4e00'..'\u9fff'
    | '\ua000'..'\ud7ff'
    | '\uf900'..'\ufaff'
    | '\uff00'..'\ufff0'
    // | '\u10000'..'\u1F9FF'  //not support four bytes chars
    // | '\u20000'..'\u2FA1F'
    ;
