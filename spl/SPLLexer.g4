/*
 [The "BSD licence"]
 Copyright (c) 2024 Clemens Sageder
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/ 

lexer grammar SPLLexer;

options { 
    caseInsensitive = true; 
}

channels { 
    WHITESPACE, 
    COMMENTS 
}

// Binary operators

ADD:  '+';
SUB:  '-';
MULT: '*';
DIV:  '/';
MOD:  '%';
POW:  '^';

AND:  'AND';
OR:   'OR';
NOT:  'NOT';

// Comparison operators

EQ:  '=';
NE:  '!=';
GT:  '>';
LT:  '<';
GE:  '>=';
LE:  '<=';

// Delimiters

PIPE:       '|';
LPAREN:     '(';
RPAREN:     ')';
LBRACK:     '[';
RBRACK:     ']';
COMMA:      ',';
COLON:      ':';
AT:         '@';
QUOTE:      '"';

// Keywords

AS:         'AS';
BY:         'BY';
OUTPUT:     'OUTPUT';
OUTPUTNEW:  'OUTPUTNEW';
IN:         'IN';
LIKE:       'LIKE';

// Commands

INIT_COMMAND // Commands that can start a query
    : 'datamodel'
    | 'datamodelsimple'
    | 'dbinspect'
    | 'eventcount'
    | 'from'
    | 'gentimes'
    | 'history'
    | 'inputcsv'
    | 'inputintelligence'
    | 'inputlookup'
    | 'makeresults'
    | 'metadata'
    | 'metasearch'
    | 'msearch'
    | 'rest'
    | 'savedsearch'
    | 'search'
    | 'tstats'
    ;

STD_COMMAND_AND_FUNCTION
    : 'lookup'
    | 'replace'
    | 'spath'
    ;

STD_COMMAND
    : 'abstract'
    | 'accum'
    | 'addcoltotals'
    | 'addinfo'
    | 'addtotals'
    | 'analyzefields'
    | 'anomalies'
    | 'anomalousvalue'
    | 'anomalydetection'
    | 'append'
    | 'appendcols'
    | 'appendpipe'
    | 'arules'
    | 'associate'
    | 'autoregress'
    | 'awssnsalert'
    | 'bin'
    | 'bucket'
    | 'bucketdir'
    | 'chart'
    | 'cluster'
    | 'cofilter'
    | 'collect'
    | 'concurrency'
    | 'contingency'
    | 'convert'
    | 'correlate'
    | 'ctable'
    | 'dbxquery'
    | 'dedup'
    | 'delete'
    | 'delta'
    | 'diff'
    | 'entitymerge'
    | 'erex'
    | 'eval'
    | 'eventstats'
    | 'extract'
    | 'fieldformat'
    | 'fields'
    | 'fieldsummary'
    | 'filldown'
    | 'fillnull'
    | 'findtypes'
    | 'folderize'
    | 'foreach'
    | 'format'
    | 'fromjson'
    | 'gauge'
    | 'geom'
    | 'geomfilter'
    | 'geostats'
    | 'head'
    | 'highlight'
    | 'iconify'
    | 'iplocation'
    | 'join'
    | 'kmeans'
    | 'kvform'
    | 'loadjob'
    | 'localize'
    | 'localop'
    | 'makecontinuous'
    | 'makemv'
    | 'map'
    | 'mcollect'
    | 'meventcollect'
    | 'mpreview'
    | 'mstats'
    | 'multikv'
    | 'multisearch'
    | 'mvcombine'
    | 'mvexpand'
    | 'nomv'
    | 'outlier'
    | 'outputcsv'
    | 'outputlookup'
    | 'outputtext'
    | 'overlap'
    | 'pivot'
    | 'predict'
    | 'rangemap'
    | 'rare'
    | 'regex'
    | 'reltime'
    | 'rename'
    | 'require'
    | 'return'
    | 'reverse'
    | 'rex'
    | 'rtorder'
    | 'run'
    | 'script'
    | 'scrub'
    | 'searchtxn'
    | 'selfjoin'
    | 'sendalert'
    | 'sendemail'
    | 'set'
    | 'setfields'
    | 'sichart'
    | 'sirare'
    | 'sistats'
    | 'sitimechart'
    | 'sitop'
    | 'snowincident'
    | 'snowincidentstream'
    | 'snowevent'
    | 'snoweventstream'
    | 'sort'
    | 'stats'
    | 'strcat'
    | 'streamstats'
    | 'table'
    | 'tags'
    | 'tail'
    | 'timechart'
    | 'timewrap'
    | 'tojson'
    | 'top'
    | 'transaction'
    | 'transpose'
    | 'trendline'
    | 'tscollect'
    | 'typeahead'
    | 'typelearner'
    | 'typer'
    | 'union'
    | 'uniq'
    | 'untable'
    | 'walklex'
    | 'where'
    | 'x11'
    | 'xmlkv'
    | 'xmlunescape'
    | 'xpath'
    | 'xyseries'
    ;
 
// Modifiers

MODIFIER_AND_FUNCTION
    : 'earliest'
    | 'latest'
    ;

TIME_AND_FUNCTION: 'now';

// Functions

FUNCTION
    // EVAL_FUNCTION

    : 'abs'
    | 'acos'
    | 'acosh'
    | 'asin'
    | 'asinh'
    | 'atan'
    | 'atan2'
    | 'atanh'
    | 'bit_and'
    | 'bit_or'
    | 'bit_not'
    | 'bit_xor'
    | 'bit_shift_left'
    | 'bit_shift_right'
    | 'case'
    | 'cidrmatch'
    | 'ceiling'
    | 'coalesce'
    | 'commands'
    | 'cos'
    | 'cosh'
    | 'exact'
    | 'exp'
    | 'false'
    | 'floor'
    | 'hypot'
    | 'if'
    | 'in'
    | 'ipmask'
    | 'isbool'
    | 'isint'
    | 'isnotnull'
    | 'isnull'
    | 'isnum'
    | 'isstr'
    | 'json_append'
    | 'json_array'
    | 'json_array_to_mv'
    | 'json_extend'
    | 'json_extract'
    | 'json_extract_exact'
    | 'json_keys'
    | 'json_object'
    | 'json_set'
    | 'json_set_exact'
    | 'json_valid'
    | 'len'
    | 'like'
    | 'ln'
    | 'log'
    | 'lower'
    | 'ltrim'
    | 'match'
    | 'md5'
    | 'mvappend'
    | 'mvcount'
    | 'mvdedup'
    | 'mvfilter'
    | 'mvfind'
    | 'mvindex'
    | 'mvjoin'
    | 'mvmap'
    | 'mvrange'
    | 'mvsort'
    | 'mvzip'
    | 'mv_to_json_array'
    | 'null'
    | 'nullif'
    | 'pi'
    | 'pow'
    | 'printf'
    | 'random'
    | 'relative_time'
    | 'round'
    | 'rtrim'
    | 'searchmatch'
    | 'sha1'
    | 'sha256'
    | 'sha512'
    | 'sigfig'
    | 'sin'
    | 'sinh'
    | 'split'
    | 'sqrt'
    | 'strftime'
    | 'strptime'
    | 'substr'
    | 'tan'
    | 'tanh'
    | 'time'
    | 'tonumber'
    | 'tostring'
    | 'trim'
    | 'true'
    | 'typeof'
    | 'upper'
    | 'urldecode'
    | 'validate'

    // STAT_FUNCTION

    | 'avg'
    | 'count'
    | 'distinct_count'
    | 'estdc'
    | 'estdc_error'
    | 'exactperc<percentile>'
    | 'max'
    | 'mean'
    | 'median'
    | 'min'
    | 'mode'
    | 'percentile<percentile>'
    | 'range'
    | 'stdev'
    | 'stdevp'
    | 'sum'
    | 'sumsq'
    | 'upperperc<percentile>'
    | 'var'
    | 'varp'
    | 'first'
    | 'last'
    | 'list'
    | 'values'
    | 'earliest_time'
    | 'latest_time'
    | 'per_day'
    | 'per_hour'
    | 'per_minute'
    | 'per_second'
    | 'rate'
    | 'rate_avg'
    | 'rate_sum'
    ;

// Literals

fragment TIME_CHAR 
    : 's' 
    | 'm' 
    | 'h' 
    | 'd' 
    | 'w' 
    | 'y' 
    | 'q' 
    | 'w0' 
    | 'w1' 
    | 'w2' 
    | 'w3' 
    | 'w4' 
    | 'w5' 
    | 'w6' 
    | 'w7' 
    | 'qtr' 
    | 'mon' 
    | 'week' 
    | 'year' 
    | 'quarter'
    ;

TIME
    : QUOTE? ([+|-] [0-9]+ TIME_CHAR) (AT TIME_CHAR)? QUOTE?
    | QUOTE? AT TIME_CHAR QUOTE?
    | QUOTE? [0-9][0-9] DIV [0-9][0-9] DIV [0-9][0-9][0-9][0-9] COLON [0-9][0-9] COLON [0-9][0-9] COLON [0-9][0-9] QUOTE?
    ;

NUMBER:     [0-9]+ ('.' [0-9]+)?;
STRING:     QUOTE ( ~["\\\r\n] | '\\' . )* QUOTE;
IDENTIFIER: [A-Z_.:] [A-Z_.:0-9]*;

// Other

WS:            [\r\t\n ]+ -> channel(WHITESPACE);
LINE_COMMENT:  '//' ~[\r\n]* -> channel(COMMENTS);
BLOCK_COMMENT: '/*' .*? '*/' -> channel(COMMENTS);
