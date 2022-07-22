lexer grammar EVMBLexer;

STOP
   : 'STOP'
   ;

ADD
   : 'ADD'
   ;

MUL
   : 'MUL'
   ;

SUB
   : 'SUB'
   ;

DIV
   : 'DIV'
   ;

SDIV
   : 'SDIV'
   ;

MOD
   : 'MOD'
   ;

SMOD
   : 'SMOD'
   ;

ADDMOD
   : 'ADDMOD'
   ;

MULMOD
   : 'MULMOD'
   ;

EXP
   : 'EXP'
   ;

SIGNEXTEND
   : 'SIGNEXTEND'
   ;

LT
   : 'LT'
   ;

GT
   : 'GT'
   ;

SLT
   : 'SLT'
   ;

SGT
   : 'SGT'
   ;

EQ
   : 'EQ'
   ;

ISZERO
   : 'ISZERO'
   ;

AND
   : 'AND'
   ;

OR
   : 'OR'
   ;

XOR
   : 'XOR'
   ;

NOT
   : 'NOT'
   ;

BYTE
   : 'BYTE'
   ;

SHL
   : 'SHL'
   ;

SHR
   : 'SHR'
   ;

SAR
   : 'SAR'
   ;

SHA3
   : 'SHA3'
   ;

ADDRESS
   : 'ADDRESS'
   ;

BALANCE
   : 'BALANCE'
   ;

ORIGIN
   : 'ORIGIN'
   ;

CALLER
   : 'CALLER'
   ;

CALLVALUE
   : 'CALLVALUE'
   ;

CALLDATALOAD
   : 'CALLDATALOAD'
   ;

CALLDATASIZE
   : 'CALLDATASIZE'
   ;

CALLDATACOPY
   : 'CALLDATACOPY'
   ;

CODESIZE
   : 'CODESIZE'
   ;

CODECOPY
   : 'CODECOPY'
   ;

GASPRICE
   : 'GASPRICE'
   ;

EXTCODESIZE
   : 'EXTCODESIZE'
   ;

EXTCODECOPY
   : 'EXTCODECOPY'
   ;

RETURNDATASIZE
   : 'RETURNDATASIZE'
   ;

RETURNDATACOPY
   : 'RETURNDATACOPY'
   ;

EXTCODEHASH
   : 'EXTCODEHASH'
   ;

BLOCKHASH
   : 'BLOCKHASH'
   ;

COINBASE
   : 'COINBASE'
   ;

TIMESTAMP
   : 'TIMESTAMP'
   ;

NUMBER
   : 'NUMBER'
   ;

DIFFICULTY
   : 'DIFFICULTY'
   ;

GASLIMIT
   : 'GASLIMIT'
   ;

CHAINID
   : 'CHAINID'
   ;

SELFBALANCE
   : 'SELFBALANCE'
   ;

BASEFEE
   : 'BASEFEE'
   ;

POP
   : 'POP'
   ;

MLOAD
   : 'MLOAD'
   ;

MSTORE
   : 'MSTORE'
   ;

MSTORE8
   : 'MSTORE8'
   ;

SLOAD
   : 'SLOAD'
   ;

SSTORE
   : 'SSTORE'
   ;

JUMP
   : 'JUMP'
   ;

JUMPI
   : 'JUMPI'
   ;

PC
   : 'PC'
   ;

MSIZE
   : 'MSIZE'
   ;

GAS
   : 'GAS'
   ;

JUMPDEST
   : 'JUMPDEST'
   ;

PUSH1
   : 'PUSH1' Head Hexs
   ;

PUSH2
   : 'PUSH2' Head Hexs Hexs
   ;

PUSH3
   : 'PUSH3' Head Hexs Hexs Hexs
   ;

PUSH4
   : 'PUSH4' Head Hexs Hexs Hexs Hexs
   ;

PUSH5
   : 'PUSH5' Head Hexs Hexs Hexs Hexs Hexs
   ;

PUSH6
   : 'PUSH6' Head Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH7
   : 'PUSH7' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH8
   : 'PUSH8' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH9
   : 'PUSH9' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH10
   : 'PUSH10' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH11
   : 'PUSH11' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH12
   : 'PUSH12' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH13
   : 'PUSH13' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH14
   : 'PUSH14' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH15
   : 'PUSH15' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH16
   : 'PUSH16' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH17
   : 'PUSH17' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH18
   : 'PUSH18' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH19
   : 'PUSH19' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH20
   : 'PUSH20' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH21
   : 'PUSH21' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH22
   : 'PUSH22' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH23
   : 'PUSH23' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH24
   : 'PUSH24' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH25
   : 'PUSH25' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH26
   : 'PUSH26' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH27
   : 'PUSH27' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH28
   : 'PUSH28' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH29
   : 'PUSH29' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH30
   : 'PUSH30' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH31
   : 'PUSH31' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

PUSH32
   : 'PUSH32' Head Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs Hexs
   ;

DUP1
   : 'DUP1'
   ;

DUP2
   : 'DUP2'
   ;

DUP3
   : 'DUP3'
   ;

DUP4
   : 'DUP4'
   ;

DUP5
   : 'DUP5'
   ;

DUP6
   : 'DUP6'
   ;

DUP7
   : 'DUP7'
   ;

DUP8
   : 'DUP8'
   ;

DUP9
   : 'DUP9'
   ;

DUP10
   : 'DUP10'
   ;

DUP11
   : 'DUP11'
   ;

DUP12
   : 'DUP12'
   ;

DUP13
   : 'DUP13'
   ;

DUP14
   : 'DUP14'
   ;

DUP15
   : 'DUP15'
   ;

DUP16
   : 'DUP16'
   ;

SWAP1
   : 'SWAP1'
   ;

SWAP2
   : 'SWAP2'
   ;

SWAP3
   : 'SWAP3'
   ;

SWAP4
   : 'SWAP4'
   ;

SWAP5
   : 'SWAP5'
   ;

SWAP6
   : 'SWAP6'
   ;

SWAP7
   : 'SWAP7'
   ;

SWAP8
   : 'SWAP8'
   ;

SWAP9
   : 'SWAP9'
   ;

SWAP10
   : 'SWAP10'
   ;

SWAP11
   : 'SWAP11'
   ;

SWAP12
   : 'SWAP12'
   ;

SWAP13
   : 'SWAP13'
   ;

SWAP14
   : 'SWAP14'
   ;

SWAP15
   : 'SWAP15'
   ;

SWAP16
   : 'SWAP16'
   ;

LOG0
   : 'LOG0'
   ;

LOG1
   : 'LOG1'
   ;

LOG2
   : 'LOG2'
   ;

LOG3
   : 'LOG3'
   ;

LOG4
   : 'LOG4'
   ;

JUMPTO
   : 'JUMPTO'
   ;

JUMPIF
   : 'JUMPIF'
   ;

JUMPSUB
   : 'JUMPSUB'
   ;

JUMPSUBV
   : 'JUMPSUBV'
   ;

BEGINSUB
   : 'BEGINSUB'
   ;

BEGINDATA
   : 'BEGINDATA'
   ;

RETURNSUB
   : 'RETURNSUB'
   ;

PUTLOCAL
   : 'PUTLOCAL'
   ;

GETLOCA
   : 'GETLOCA'
   ;

SLOADBYTES
   : 'SLOADBYTES'
   ;

SSTOREBYTES
   : 'SSTOREBYTES'
   ;

SSIZE
   : 'SSIZE'
   ;

CREATE
   : 'CREATE'
   ;

CALL
   : 'CALL'
   ;

CALLCODE
   : 'CALLCODE'
   ;

RETURN
   : 'RETURN'
   ;

DELEGATECALL
   : 'DELEGATECALL'
   ;

CALLBLACKBOX
   : 'CALLBLACKBOX'
   ;

STATICCALL
   : 'STATICCALL'
   ;

CREATE2
   : 'CREATE2'
   ;

TXEXECGAS
   : 'TXEXECGAS'
   ;

REVERT
   : 'REVERT'
   ;

INVALID
   : 'INVALID'
   ;

SELFDESTRUCT
   : 'SELFDESTRUCT'
   ;

UNKNOW
   : '\'' Hexs '\'' ' '? Unknown
   ;

fragment Unknown
   : '(' 'Unknown Opcode' ')'
   ;

fragment Head
   : ' ' '0' [xX]
   ;

fragment Hex
   : [0-9a-fA-F]
   ;

fragment Hexs
   : Hex Hex
   ;

WS
   : [ \n\t\r] -> channel(HIDDEN)
   ;

