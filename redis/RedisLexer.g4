// $antlr-format alignTrailingComments true, columnLimit 500, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine true, allowShortBlocksOnASingleLine true, minEmptyLines 0, alignSemicolons ownLine
// $antlr-format alignColons trailing, singleLineOverrulesHangingColon true, alignLexerCommands true, alignLabels true, alignTrailers true

lexer grammar RedisLexer;

options {
    caseInsensitive = true;
}

// Spaces

SPACE   : [ \t]+ -> channel (HIDDEN);
NEWLINE : ('\r' '\n'? | '\n');

// Common keywords

COPY        : 'COPY';
DB          : 'DB';
REPLACE     : 'REPLACE';
DEL         : 'DEL';
UNLINK      : 'UNLINK';
DUMP        : 'DUMP';
EXISTS      : 'EXISTS';
EXPIRE      : 'EXPIRE';
GT          : 'GT';
LT          : 'LT';
NX          : 'NX';
XX          : 'XX';
EXPIREAT    : 'EXPIREAT';
EXPIRETIME  : 'EXPIRETIME';
PEXPIRE     : 'PEXPIRE';
PEXPIREAT   : 'PEXPIREAT';
PEXPIRETIME : 'PEXPIRETIME';
KEYS        : 'KEYS';
MOVE        : 'MOVE';
OBJECT      : 'OBJECT';
ENCODING    : 'ENCODING';
FREQ        : 'FREQ';
IDLETIME    : 'IDLETIME';
REFCOUNT    : 'REFCOUNT';
PERSIST     : 'PERSIST';
TTL         : 'TTL';
PTTL        : 'PTTL';
RANDOMKEY   : 'RANDOMKEY';
RENAME      : 'RENAME';
RENAMENX    : 'RENAMENX';
SCAN        : 'SCAN';
MATCH       : 'MATCH';
COUNT       : 'COUNT';
TYPE        : 'TYPE';
TOUCH       : 'TOUCH';
WAIT        : 'WAIT';
LEFT        : 'LEFT';
RIGHT       : 'RIGHT';
BEFORE      : 'BEFORE';
AFTER       : 'AFTER';
RANK        : 'RANK';
MAXLEN      : 'MAXLEN';
LIMIT       : 'LIMIT';
MIN         : 'MIN';
MAX         : 'MAX';
CH          : 'CH';
WITHSCORE   : 'WITHSCORE';
WITHSCORES  : 'WITHSCORES';
WEIGHTS     : 'WEIGHTS';
AGGREGATE   : 'AGGREGATE';
SUM         : 'SUM';
BYSCORE     : 'BYSCORE';
BYLEX       : 'BYLEX';
REV         : 'REV';
FIELDS      : 'FIELDS';
WITHVALUES  : 'WITHVALUES';
NOVALUES    : 'NOVALUES';

// String keywords

SET      : 'SET';
GET      : 'GET';
INCR     : 'INCR';
INCRBY   : 'INCRBY';
DECR     : 'DECR';
DECRBY   : 'DECRBY';
EX       : 'EX';
PX       : 'PX';
EXAT     : 'EXAT';
PXAT     : 'PXAT';
KEEPTTL  : 'KEEPTTL';
APPEND   : 'APPEND';
GETDEL   : 'GETDEL';
GETEX    : 'GETEX';
GETRANGE : 'GETRANGE';
GETSET   : 'GETSET';
MGET     : 'MGET';
MSET     : 'MSET';
MSETNX   : 'MSETNX';
PSETEX   : 'PSETEX';
SETEX    : 'SETEX';
SETNX    : 'SETNX';
SETRANGE : 'SETRANGE';
STRLEN   : 'STRLEN';
SUBSTR   : 'SUBSTR';

// List keywords

LMOVE      : 'LMOVE';
BLMOVE     : 'BLMOVE';
LMPOP      : 'LMPOP';
BLMPOP     : 'BLMPOP';
LPOP       : 'LPOP';
BLPOP      : 'BLPOP';
RPOP       : 'RPOP';
BRPOP      : 'BRPOP';
RPOPLPUSH  : 'RPOPLPUSH';
BRPOPLPUSH : 'BRPOPLPUSH';
LINDEX     : 'LINDEX';
LINSERT    : 'LINSERT';
LLEN       : 'LLEN';
LPOS       : 'LPOS';
LPUSH      : 'LPUSH';
LPUSHX     : 'LPUSHX';
RPUSH      : 'RPUSH';
RPUSHX     : 'RPUSHX';
LRANGE     : 'LRANGE';
LREM       : 'LREM';
LSET       : 'LSET';
LTRIM      : 'LTRIM';

// Set keywords

SADD        : 'SADD';
SCARD       : 'SCARD';
SDIFF       : 'SDIFF';
SDIFFSTORE  : 'SDIFFSTORE';
SINTER      : 'SINTER';
SINTERCARD  : 'SINTERCARD';
SINTERSTORE : 'SINTERSTORE';
SISMEMBER   : 'SISMEMBER';
SMISMEMBER  : 'SMISMEMBER';
SMEMBERS    : 'SMEMBERS';
SMOVE       : 'SMOVE';
SPOP        : 'SPOP';
SRANDMEMBER : 'SRANDMEMBER';
SREM        : 'SREM';
SSCAN       : 'SSCAN';
SUNION      : 'SUNION';
SUNIONSTORE : 'SUNIONSTORE';

// Sorted set keywords

ZMPOP            : 'ZMPOP';
BZMPOP           : 'BZMPOP';
ZPOPMAX          : 'ZPOPMAX';
BZPOPMAX         : 'BZPOPMAX';
ZPOPMIN          : 'ZPOPMIN';
BZPOPMIN         : 'BZPOPMIN';
ZADD             : 'ZADD';
ZCARD            : 'ZCARD';
ZCOUNT           : 'ZCOUNT';
ZDIFF            : 'ZDIFF';
ZDIFFSTORE       : 'ZDIFFSTORE';
ZINCRBY          : 'ZINCRBY';
ZINTER           : 'ZINTER';
ZINTERCARD       : 'ZINTERCARD';
ZINTERSTORE      : 'ZINTERSTORE';
ZLEXCOUNT        : 'ZLEXCOUNT';
ZSCORE           : 'ZSCORE';
ZMSCORE          : 'ZMSCORE';
ZRANDMEMBER      : 'ZRANDMEMBER';
ZRANGE           : 'ZRANGE';
ZRANGEBYLEX      : 'ZRANGEBYLEX';
ZRANGEBYSCORE    : 'ZRANGEBYSCORE';
ZRANGESTORE      : 'ZRANGESTORE';
ZRANK            : 'ZRANK';
ZREM             : 'ZREM';
ZREMRANGEBYLEX   : 'ZREMRANGEBYLEX';
ZREMRANGEBYRANK  : 'ZREMRANGEBYRANK';
ZREMRANGEBYSCORE : 'ZREMRANGEBYSCORE';
ZREVRANGE        : 'ZREVRANGE';
ZREVRANGEBYLEX   : 'ZREVRANGEBYLEX';
ZREVRANGEBYSCORE : 'ZREVRANGEBYSCORE';
ZREVRANK         : 'ZREVRANK';
ZSCAN            : 'ZSCAN';
ZUNION           : 'ZUNION';
ZUNIONSTORE      : 'ZUNIONSTORE';

// Hash keywords

HDEL         : 'HDEL';
HEXISTS      : 'HEXISTS';
HEXPIRE      : 'HEXPIRE';
HPEXPIRE     : 'HPEXPIRE';
HEXPIREAT    : 'HEXPIREAT';
HPEXPIREAT   : 'HPEXPIREAT';
HEXPIRETIME  : 'HEXPIRETIME';
HPEXPIRETIME : 'HPEXPIRETIME';
HGET         : 'HGET';
HMGET        : 'HMGET';
HGETALL      : 'HGETALL';
HINCRBY      : 'HINCRBY';
HKEYS        : 'HKEYS';
HLEN         : 'HLEN';
HSET         : 'HSET';
HMSET        : 'HMSET';
HSETNX       : 'HSETNX';
HPERSIST     : 'HPERSIST';
HTTL         : 'HTTL';
HPTTL        : 'HPTTL';
HRANDFIELD   : 'HRANDFIELD';
HSCAN        : 'HSCAN';
HSTRLEN      : 'HSTRLEN';
HVALS        : 'HVALS';

// Streams keywords

XADD        : 'XADD';
XDEL        : 'XDEL';
XACK        : 'XACK';
XGROUP      : 'XGROUP';
XINFO       : 'XINFO';
XREAD       : 'XREAD';
XRANGE      : 'XRANGE';
XREVRANGE   : 'XREVRANGE';
XPENDING    : 'XPENDING';
XTRIM       : 'XTRIM';
XCLAIM      : 'XCLAIM';
XSETID      : 'XSETID';
BLOCK       : 'BLOCK';
STREAMS     : 'STREAMS';
GROUP       : 'GROUP';
NOACK       : 'NOACK';
ID          : 'ID';
MKSTREAM    : 'MKSTREAM';
CREATE      : 'CREATE';
DESTROY     : 'DESTROY';
DELCONSUMER : 'DELCONSUMER';
JUSTID      : 'JUSTID';
SETID       : 'SETID';

// Pub/Sub keywords

SUBSCRIBE    : 'SUBSCRIBE';
PSUBSCRIBE   : 'PSUBSCRIBE';
UNSUBSCRIBE  : 'UNSUBSCRIBE';
PUNSUBSCRIBE : 'PUNSUBSCRIBE';
PUBLISH      : 'PUBLISH';
SPUBLISH     : 'SPUBLISH';
SSUBSCRIBE   : 'SSUBSCRIBE';
SUNSUBSCRIBE : 'SUNSUBSCRIBE';
PUBSUB       : 'PUBSUB';

// TimeSeries (RedisTimeSeries) keywords

TS_ADD       : 'TS.ADD';
TS_MADD      : 'TS.MADD';
TS_CREATE    : 'TS.CREATE';
TS_ALTER     : 'TS.ALTER';
TS_INCRBY    : 'TS.INCRBY';
TS_DECRBY    : 'TS.DECRBY';
TS_DEL       : 'TS.DEL';
TS_INFO      : 'TS.INFO';
TS_MGET      : 'TS.MGET';
TS_RANGE     : 'TS.RANGE';
TS_REVRANGE  : 'TS.REVRANGE';
TS_MRANGE    : 'TS.MRANGE';
TS_QUERYINDEX: 'TS.QUERYINDEX';
RETENTION    : 'RETENTION';
LABELS       : 'LABELS';
COMPRESS     : 'COMPRESS';
UNCOMPRESSED : 'UNCOMPRESSED';
DUPLICATE_POLICY : 'DUPLICATE_POLICY';
CHUNK_SIZE   : 'CHUNK_SIZE';
AGGREGATION  : 'AGGREGATION';
FILTER       : 'FILTER';
WITHLABELS   : 'WITHLABELS';
ALIGN        : 'ALIGN';
GROUPBY      : 'GROUPBY';
REDUCE       : 'REDUCE';
BY           : 'BY';
FROM         : 'FROM';
TO           : 'TO';
TIMESTAMP    : 'TIMESTAMP';

// RedisGraph keywords

GRAPH_QUERY     : 'GRAPH.QUERY';
GRAPH_PROFILE   : 'GRAPH.PROFILE';
GRAPH_EXPLAIN   : 'GRAPH.EXPLAIN';
GRAPH_DELETE    : 'GRAPH.DELETE';
GRAPH_RO_QUERY  : 'GRAPH.RO_QUERY';

// RediSearch / Vector Search keywords

FT_CREATE    : 'FT.CREATE';
FT_SEARCH    : 'FT.SEARCH';
FT_ALTER     : 'FT.ALTER';
FT_DROPINDEX : 'FT.DROPINDEX';
VECTOR       : 'VECTOR';
HNSW         : 'HNSW';
FLAT         : 'FLAT';
DIM          : 'DIM';
DISTANCE_METRIC : 'DISTANCE_METRIC';
COSINE       : 'COSINE';
L2           : 'L2';
IP           : 'IP';
KNN          : 'KNN';
AS           : 'AS';
DIALECT      : 'DIALECT';
RETURN       : 'RETURN';
ON           : 'ON';
HASH         : 'HASH';
JSON         : 'JSON';
SCHEMA       : 'SCHEMA';
TEXT         : 'TEXT';
TAG          : 'TAG';
NUMERIC      : 'NUMERIC';
SORTBY       : 'SORTBY';
PARAMS       : 'PARAMS';
INFIELDS     : 'INFIELDS';
NOCONTENT    : 'NOCONTENT';
// WITHSCORES already defined above in sorted set section
HIGHLIGHT    : 'HIGHLIGHT';
SUMMARIZE    : 'SUMMARIZE';
ASC          : 'ASC';
DESC         : 'DESC';

PFADD        : 'PFADD';
PFCOUNT      : 'PFCOUNT';
PFMERGE      : 'PFMERGE';
BITCOUNT     : 'BITCOUNT';
BITFIELD     : 'BITFIELD';
BITOP        : 'BITOP';
BITPOS       : 'BITPOS';
GETBIT       : 'GETBIT';
SETBIT       : 'SETBIT';
GEOADD       : 'GEOADD';
GEODIST      : 'GEODIST';
GEOHASH      : 'GEOHASH';
GEORADIUS    : 'GEORADIUS';
GEORADIUSBYMEMBER : 'GEORADIUSBYMEMBER';
GEOSEARCH    : 'GEOSEARCH';
GEOSEARCHSTORE: 'GEOSEARCHSTORE';
MULTI        : 'MULTI';
EXEC         : 'EXEC';
DISCARD      : 'DISCARD';
UNWATCH      : 'UNWATCH';
WATCH        : 'WATCH';
EVAL         : 'EVAL';
EVALSHA      : 'EVALSHA';
SCRIPT       : 'SCRIPT';
LOAD         : 'LOAD';
FLUSH        : 'FLUSH';
KILL         : 'KILL';
FUNCTION     : 'FUNCTION';
DELETE       : 'DELETE';
LIST         : 'LIST';
FCALL        : 'FCALL';
FCALL_RO     : 'FCALL_RO';
ACL          : 'ACL';
CAT          : 'CAT';
GENPASS      : 'GENPASS';
GETUSER      : 'GETUSER';
LOG          : 'LOG';
HELP         : 'HELP';
CLUSTER      : 'CLUSTER';
CLIENT       : 'CLIENT';
CONFIG       : 'CONFIG';
INFO         : 'INFO';
MONITOR      : 'MONITOR';
SLOWLOG      : 'SLOWLOG';
TIME         : 'TIME';
ROLE         : 'ROLE';
MEMORY       : 'MEMORY';
COMMAND      : 'COMMAND';
MODULE       : 'MODULE';
PING         : 'PING';
AUTH         : 'AUTH';
HELLO        : 'HELLO';
QUIT         : 'QUIT';
SELECT       : 'SELECT';
BGREWRITEAOF : 'BGREWRITEAOF';
BGSAVE       : 'BGSAVE';
SAVE         : 'SAVE';
SHUTDOWN     : 'SHUTDOWN';
REPLICAOF    : 'REPLICAOF';
XAUTOCLAIM   : 'XAUTOCLAIM';
SETNAME      : 'SETNAME';

// Constructors symbols

SINGLE_QUOTE : '\'';
DOUBLE_QUOTE : '"';

// Literal Primitives

fragment DECIMAL_DIGIT: [0-9];

POSITIVE_DECIMAL_LITERAL : DECIMAL_DIGIT+;
DECIMAL_LITERAL          : '-'? POSITIVE_DECIMAL_LITERAL;
DECIMAL_SCORE_LITERAL    : ([(]? DECIMAL_LITERAL) | (('+' | '-')? 'inf');

// Identifiers
// Should be at the very bottom, for it is the most general token

fragment DOUBLE_QUOTE_STRING : '"' ( '\\' . | ~('"' | '\\' | '\n' | '\r'))* '"';
fragment SINGLE_QUOTE_STRING : '\'' ('\\' . | ~('\'' | '\\' | '\n' | '\r'))* '\'';
fragment BASE_IDENTIFIER     : ~(' ' | '\t' | '\'' | '"' | '\n' | '\r')+;

IDENTIFIER: BASE_IDENTIFIER | DOUBLE_QUOTE_STRING | SINGLE_QUOTE_STRING;