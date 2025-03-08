// $antlr-format columnLimit 500, minEmptyLines 1, maxEmptyLinesToKeep 1, useTab false, reflowComments false, breakBeforeBraces false
// $antlr-format keepEmptyLinesAtTheStartOfBlocks false, allowShortRulesOnASingleLine false, alignSemicolons hanging, alignColons hanging
// $antlr-format alignTrailingComments true

parser grammar RedisParser;

options {
    tokenVocab = RedisLexer;
}

root
    : commands? EOF
    ;

// We can omit newline only if it's the last statement
commands
    : command NEWLINE*
    | command NEWLINE+ commands
    ;

command
    : commonCommand
    | stringCommand
    | listCommand
    | setCommand
    | sortedSetCommand
    | hashCommand
    ;

commonCommand
    : copyCommand
    | deleteCommand
    | unlinkCommand
    | dumpCommand
    | existsCommand
    | expireCommand
    | expireAtCommand
    | expireTimeCommand
    | pExpireCommand
    | pExpireAtCommand
    | pExpireTimeCommand
    | keysCommand
    | moveCommand
    | objectCommand
    | persistCommand
    | ttlCommand
    | pTtlCommand
    | randomKeyCommand
    | renameCommand
    | renameNxCommand
    | scanCommand
    | touchCommand
    | typeCommand
    | waitCommand
    ;

stringCommand
    : stringSetCommand
    | getCommand
    | incrementCommand
    | incrementByCommand
    | decrementCommand
    | decrementByCommand
    | appendCommand
    | getDeleteCommand
    | getExCommand
    | getRangeCommand
    | getSetCommand
    | mGetCommand
    | mSetCommand
    | mSetNxCommand
    | pSetExCommand
    | setExCommand
    | setNxCommand
    | setRangeCommand
    | stringLengthCommand
    | substringCommand
    ;

listCommand
    : lmoveCommand
    | blmoveCommand
    | lmpopCommand
    | blmpopCommand
    | lpopCommand
    | blpopCommand
    | rpopCommand
    | brpopCommand
    | rpopLpushCommand
    | brpopLpushCommand
    | lindexCommand
    | linsertCommand
    | llenCommand
    | lposCommand
    | lpushCommand
    | lpushxCommand
    | rpushCommand
    | rpushxCommand
    | lrangeCommand
    | lremCommand
    | lsetCommand
    | ltrimCommand
    ;

setCommand
    : saddCommand
    | scardCommand
    | sdiffCommand
    | sdiffstoreCommand
    | sinterCommand
    | sintercardCommand
    | sinterstoreCommand
    | sismemberCommand
    | smismemberCommand
    | smembersCommand
    | smoveCommand
    | spopCommand
    | srandmemberCommand
    | sremCommand
    | sscanComman
    | sunionCommand
    | sunionstoreCommand
    ;

sortedSetCommand
    : zmpopCommand
    | bzmpopCommand
    | zpopmaxCommand
    | bzpopmaxCommand
    | zpopminCommand
    | bzpopminCommand
    | zaddCommand
    | zcardCommand
    | zcountCommand
    | zdiffCommand
    | zdiffstoreCommand
    | zincrbyCommand
    | zinterCommand
    | zintercardCommand
    | zinterstoreCommand
    | zlexcountCommand
    | zscoreCommand
    | zmscoreCommand
    | zrandmemberCommand
    | zrangeCommand
    | zrangebylexCommand
    | zrangebyscoreCommand
    | zrangestoreCommand
    | zrankCommand
    | zrevrankCommand
    | zremCommand
    | zremrangebylexCommand
    | zremrangebyrankCommand
    | zremrangebyscoreCommand
    | zrevrangeCommand
    | zrevrangebylexCommand
    | zrevrangebyscoreCommand
    | zscanCommand
    | zunionCommand
    | zunionstoreCommand
    ;

hashCommand
    : hdelCommand
    | hexistsCommand
    | hexpireCommand
    | hpexpireCommand
    | hexpireAtCommand
    | hpexpireAtCommand
    | hexpireTimeCommand
    | hpexpireTimeCommand
    | hgetCommand
    | hgetAllCommand
    | hincrByCommand
    | hkeysCommand
    | hlenCommand
    | hmgetCommand
    | hsetCommand
    | hmsetCommand
    | hsetnxCommand
    | hpersistCommand
    | httlCommand
    | hpttlCommand
    | hrandfieldCommand
    | hscanCommand
    | hstrlenCommand
    | hvalsCommand
    ;

hdelCommand
    : HDEL hashKeyName identifier+
    ;

hexistsCommand
    : HEXISTS hashKeyName identifier
    ;

hexpireCommand
    : HEXPIRE hashKeyName decimal expireOptions? fieldsClause
    ;

hpexpireCommand
    : HPEXPIRE hashKeyName decimal expireOptions? fieldsClause
    ;

fieldsClause
    : FIELDS POSITIVE_DECIMAL_LITERAL identifier+
    ;

hexpireAtCommand
    : HEXPIREAT hashKeyName decimal expireOptions? fieldsClause
    ;

hpexpireAtCommand
    : HPEXPIREAT hashKeyName decimal expireOptions? fieldsClause
    ;

hexpireTimeCommand
    : HEXPIRETIME hashKeyName fieldsClause
    ;

hpexpireTimeCommand
    : HPEXPIRETIME hashKeyName fieldsClause
    ;

hgetCommand
    : HGET hashKeyName identifier
    ;

hmgetCommand
    : HMGET hashKeyName identifier+
    ;

hgetAllCommand
    : HGETALL hashKeyName
    ;

hincrByCommand
    : HINCRBY hashKeyName identifier decimal
    ;

hkeysCommand
    : HKEYS hashKeyName
    ;

hlenCommand
    : HLEN hashKeyName
    ;

hsetCommand
    : HSET hashKeyName (identifier identifier)+
    ;

hmsetCommand
    : HMSET hashKeyName (identifier identifier)+
    ;

hsetnxCommand
    : HSETNX hashKeyName identifier identifier
    ;

hpersistCommand
    : HPERSIST hashKeyName fieldsClause
    ;

httlCommand
    : HTTL hashKeyName fieldsClause
    ;

hpttlCommand
    : HPTTL hashKeyName fieldsClause
    ;

hrandfieldCommand
    : HRANDFIELD hashKeyName (decimal WITHVALUES?)?
    ;

hscanCommand
    : HSCAN hashKeyName decimal matchClause? countClause? NOVALUES?
    ;

hstrlenCommand
    : HSTRLEN hashKeyName identifier
    ;

hvalsCommand
    : HVALS hashKeyName
    ;

zmpopCommand
    : ZMPOP POSITIVE_DECIMAL_LITERAL sortedSetKeyName+ minMaxClause countClause?
    ;

bzmpopCommand
    : BZMPOP POSITIVE_DECIMAL_LITERAL POSITIVE_DECIMAL_LITERAL sortedSetKeyName+ minMaxClause countClause?
    ;

zpopmaxCommand
    : ZPOPMAX sortedSetKeyName POSITIVE_DECIMAL_LITERAL?
    ;

bzpopmaxCommand
    : BZPOPMAX sortedSetKeyName+ POSITIVE_DECIMAL_LITERAL
    ;

zpopminCommand
    : ZPOPMIN sortedSetKeyName POSITIVE_DECIMAL_LITERAL?
    ;

bzpopminCommand
    : BZPOPMIN sortedSetKeyName+ POSITIVE_DECIMAL_LITERAL
    ;

minMaxClause
    : MIN
    | MAX
    ;

zaddCommand
    : ZADD sortedSetKeyName keyExistenceClause? keyUpdateClause? CH? INCR? scoreMemberClause+
    ;

keyUpdateClause
    : GT
    | LT
    ;

scoreMemberClause
    : decimal identifier
    ;

zcardCommand
    : ZCARD sortedSetKeyName
    ;

zcountCommand
    : ZCOUNT sortedSetKeyName decimalScore decimalScore
    ;

zdiffCommand
    : ZDIFF POSITIVE_DECIMAL_LITERAL sortedSetKeyName+ WITHSCORES?
    ;

zdiffstoreCommand
    : ZDIFFSTORE identifier POSITIVE_DECIMAL_LITERAL sortedSetKeyName+
    ;

zincrbyCommand
    : ZINCRBY sortedSetKeyName decimal identifier
    ;

zinterCommand
    : ZINTER POSITIVE_DECIMAL_LITERAL sortedSetKeyName+ weightsClause? aggregateClause? WITHSCORES?
    ;

zintercardCommand
    : ZINTERCARD POSITIVE_DECIMAL_LITERAL sortedSetKeyName+ limitClause?
    ;

zinterstoreCommand
    : ZINTERSTORE identifier POSITIVE_DECIMAL_LITERAL sortedSetKeyName+ weightsClause? aggregateClause?
    ;

weightsClause
    : WEIGHTS decimal+
    ;

aggregateClause
    : AGGREGATE (MIN | MAX | SUM)
    ;

zlexcountCommand
    : ZLEXCOUNT sortedSetKeyName lexicalScore lexicalScore
    ;

zscoreCommand
    : ZSCORE sortedSetKeyName identifier
    ;

zmscoreCommand
    : ZMSCORE sortedSetKeyName identifier+
    ;

zrandmemberCommand
    : ZRANDMEMBER sortedSetKeyName (decimal WITHSCORES?)?
    ;

zrangeCommand
    : ZRANGE sortedSetKeyName lexicalScore lexicalScore rangeTypeClause? REV? limitOffsetClause? WITHSCORES?
    ;

zrangebylexCommand
    : ZRANGEBYLEX sortedSetKeyName lexicalScore lexicalScore limitOffsetClause?
    ;

zrangebyscoreCommand
    : ZRANGEBYSCORE sortedSetKeyName decimalScore decimalScore WITHSCORES? limitOffsetClause?
    ;

zrangestoreCommand
    : ZRANGESTORE identifier sortedSetKeyName lexicalScore lexicalScore rangeTypeClause? REV? limitOffsetClause?
    ;

rangeTypeClause
    : BYSCORE
    | BYLEX
    ;

limitOffsetClause
    : LIMIT decimal decimal
    ;

zrankCommand
    : ZRANK sortedSetKeyName identifier WITHSCORE?
    ;

zrevrankCommand
    : ZREVRANK sortedSetKeyName identifier WITHSCORE?
    ;

zremCommand
    : ZREM sortedSetKeyName identifier+
    ;

zremrangebylexCommand
    : ZREMRANGEBYLEX sortedSetKeyName lexicalScore lexicalScore
    ;

zremrangebyrankCommand
    : ZREMRANGEBYRANK sortedSetKeyName decimal decimal
    ;

zremrangebyscoreCommand
    : ZREMRANGEBYSCORE sortedSetKeyName decimalScore decimalScore
    ;

zrevrangeCommand
    : ZREVRANGE sortedSetKeyName decimal decimal WITHSCORES?
    ;

zrevrangebylexCommand
    : ZREVRANGEBYLEX sortedSetKeyName lexicalScore lexicalScore limitOffsetClause?
    ;

zrevrangebyscoreCommand
    : ZREVRANGEBYSCORE sortedSetKeyName decimalScore decimalScore WITHSCORES? limitOffsetClause?
    ;

zscanCommand
    : ZSCAN sortedSetKeyName decimal matchClause? countClause?
    ;

zunionCommand
    : ZUNION POSITIVE_DECIMAL_LITERAL sortedSetKeyName+ weightsClause? aggregateClause? WITHSCORES?
    ;

zunionstoreCommand
    : ZUNIONSTORE identifier POSITIVE_DECIMAL_LITERAL sortedSetKeyName+ weightsClause? aggregateClause?
    ;

saddCommand
    : SADD setKeyName identifier+
    ;

scardCommand
    : SCARD setKeyName
    ;

sdiffCommand
    : SDIFF setKeyName+
    ;

sdiffstoreCommand
    : SDIFFSTORE identifier setKeyName+
    ;

sinterCommand
    : SINTER setKeyName+
    ;

sintercardCommand
    : SINTERCARD POSITIVE_DECIMAL_LITERAL setKeyName+ limitClause?
    ;

limitClause
    : LIMIT POSITIVE_DECIMAL_LITERAL
    ;

sinterstoreCommand
    : SINTERSTORE identifier setKeyName+
    ;

sismemberCommand
    : SISMEMBER setKeyName identifier
    ;

smismemberCommand
    : SMISMEMBER setKeyName identifier+
    ;

smembersCommand
    : SMEMBERS setKeyName
    ;

smoveCommand
    : SMOVE setKeyName setKeyName
    ;

spopCommand
    : SPOP setKeyName POSITIVE_DECIMAL_LITERAL?
    ;

srandmemberCommand
    : SRANDMEMBER setKeyName decimal?
    ;

sremCommand
    : SREM setKeyName identifier+
    ;

sscanComman
    : SSCAN setKeyName decimal matchClause? countClause?
    ;

sunionCommand
    : SUNION setKeyName+
    ;

sunionstoreCommand
    : SUNIONSTORE identifier setKeyName+
    ;

lmoveCommand
    : LMOVE listKeyName listKeyName leftOrRightClause leftOrRightClause
    ;

leftOrRightClause
    : LEFT
    | RIGHT
    ;

blmoveCommand
    : BLMOVE listKeyName listKeyName leftOrRightClause leftOrRightClause POSITIVE_DECIMAL_LITERAL
    ;

lmpopCommand
    : LMPOP POSITIVE_DECIMAL_LITERAL listKeyName+ leftOrRightClause countClause?
    ;

blmpopCommand
    : BLMPOP POSITIVE_DECIMAL_LITERAL POSITIVE_DECIMAL_LITERAL listKeyName+ leftOrRightClause countClause?
    ;

lpopCommand
    : LPOP listKeyName POSITIVE_DECIMAL_LITERAL?
    ;

blpopCommand
    : BLPOP listKeyName+ POSITIVE_DECIMAL_LITERAL
    ;

rpopCommand
    : RPOP listKeyName POSITIVE_DECIMAL_LITERAL?
    ;

brpopCommand
    : BRPOP listKeyName+ POSITIVE_DECIMAL_LITERAL
    ;

rpopLpushCommand
    : RPOPLPUSH listKeyName listKeyName
    ;

brpopLpushCommand
    : BRPOPLPUSH listKeyName listKeyName POSITIVE_DECIMAL_LITERAL
    ;

lindexCommand
    : LINDEX listKeyName decimal
    ;

linsertCommand
    : LINSERT listKeyName beforeOrAfterClause identifier identifier
    ;

beforeOrAfterClause
    : BEFORE
    | AFTER
    ;

llenCommand
    : LLEN listKeyName
    ;

lposCommand
    : LPOS listKeyName identifier rankClause? countClause? maxLenClause?
    ;

rankClause
    : RANK decimal
    ;

maxLenClause
    : MAXLEN POSITIVE_DECIMAL_LITERAL
    ;

lpushCommand
    : LPUSH listKeyName identifier+
    ;

lpushxCommand
    : LPUSHX listKeyName identifier+
    ;

rpushCommand
    : RPUSH listKeyName identifier+
    ;

rpushxCommand
    : RPUSHX listKeyName identifier+
    ;

lrangeCommand
    : LRANGE listKeyName decimal decimal
    ;

lremCommand
    : LREM listKeyName decimal identifier
    ;

lsetCommand
    : LSET listKeyName decimal identifier
    ;

ltrimCommand
    : LTRIM listKeyName decimal decimal
    ;

copyCommand
    : COPY keyName identifier dbClause? REPLACE?
    ;

dbClause
    : DB databaseName
    ;

databaseName
    : POSITIVE_DECIMAL_LITERAL
    ;

deleteCommand
    : DEL keyName+
    ;

unlinkCommand
    : UNLINK keyName+
    ;

dumpCommand
    : DUMP keyName
    ;

existsCommand
    : EXISTS keyName+
    ;

expireCommand
    : EXPIRE keyName decimal expireOptions?
    ;

expireAtCommand
    : EXPIREAT keyName decimal expireOptions?
    ;

pExpireCommand
    : PEXPIRE keyName decimal expireOptions?
    ;

pExpireAtCommand
    : PEXPIREAT keyName decimal expireOptions?
    ;

expireOptions
    : NX
    | XX
    | GT
    | LT
    ;

expireTimeCommand
    : EXPIRETIME keyName
    ;

pExpireTimeCommand
    : PEXPIRETIME keyName
    ;

keysCommand
    : KEYS keyPattern
    ;

moveCommand
    : MOVE keyName databaseName
    ;

objectCommand
    : OBJECT objectOptions keyName
    ;

objectOptions
    : ENCODING
    | FREQ
    | IDLETIME
    | REFCOUNT
    ;

persistCommand
    : PERSIST keyName
    ;

ttlCommand
    : TTL keyName
    ;

pTtlCommand
    : PTTL keyName
    ;

randomKeyCommand
    : RANDOMKEY
    ;

renameCommand
    : RENAME keyName identifier
    ;

renameNxCommand
    : RENAMENX keyName identifier
    ;

scanCommand
    : SCAN decimal matchClause? countClause? typeClause?
    ;

matchClause
    : MATCH keyPattern
    ;

countClause
    : COUNT POSITIVE_DECIMAL_LITERAL
    ;

typeClause
    : TYPE identifier
    ;

touchCommand
    : TOUCH keyName+
    ;

typeCommand
    : TYPE keyName
    ;

waitCommand
    : WAIT POSITIVE_DECIMAL_LITERAL POSITIVE_DECIMAL_LITERAL
    ;

stringSetCommand
    : SET stringKeyName identifier keyExistenceClause? GET? (expirationClause | KEEPTTL)?
    ;

keyExistenceClause
    : NX
    | XX
    ;

expirationClause
    : EX POSITIVE_DECIMAL_LITERAL
    | PX POSITIVE_DECIMAL_LITERAL
    | EXAT POSITIVE_DECIMAL_LITERAL
    | PXAT POSITIVE_DECIMAL_LITERAL
    ;

getCommand
    : GET stringKeyName
    ;

incrementCommand
    : INCR stringKeyName
    ;

incrementByCommand
    : INCRBY stringKeyName decimal
    ;

decrementCommand
    : DECR stringKeyName
    ;

decrementByCommand
    : DECRBY stringKeyName decimal
    ;

appendCommand
    : APPEND stringKeyName identifier
    ;

getDeleteCommand
    : GETDEL stringKeyName
    ;

getExCommand
    : GETEX stringKeyName (expirationClause | PERSIST)?
    ;

getRangeCommand
    : GETRANGE stringKeyName decimal decimal
    ;

getSetCommand
    : GETSET stringKeyName identifier
    ;

mGetCommand
    : MGET stringKeyName+
    ;

mSetCommand
    : MSET (stringKeyName identifier)+
    ;

mSetNxCommand
    : MSETNX (stringKeyName identifier)+
    ;

pSetExCommand
    : PSETEX stringKeyName POSITIVE_DECIMAL_LITERAL identifier
    ;

setExCommand
    : SETEX stringKeyName POSITIVE_DECIMAL_LITERAL identifier
    ;

setNxCommand
    : SETNX stringKeyName identifier
    ;

setRangeCommand
    : SETRANGE stringKeyName POSITIVE_DECIMAL_LITERAL identifier
    ;

stringLengthCommand
    : STRLEN stringKeyName
    ;

substringCommand
    : SUBSTR stringKeyName decimal decimal
    ;

decimal
    : POSITIVE_DECIMAL_LITERAL
    | DECIMAL_LITERAL
    ;

decimalScore
    : POSITIVE_DECIMAL_LITERAL
    | DECIMAL_LITERAL
    | DECIMAL_SCORE_LITERAL
    ;

identifier
    : IDENTIFIER
    | DECIMAL_LITERAL
    | POSITIVE_DECIMAL_LITERAL
    | DECIMAL_SCORE_LITERAL
    ;

lexicalScore
    : identifier
    ;

stringKeyName
    : identifier
    ;

listKeyName
    : identifier
    ;

setKeyName
    : identifier
    ;

sortedSetKeyName
    : identifier
    ;

hashKeyName
    : identifier
    ;

keyName
    : identifier
    ;

keyPattern
    : identifier # notProperPattern
    ;