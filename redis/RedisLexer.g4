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