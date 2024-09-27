pragma solidity >=0.7.0 <0.9.0;
contract Ballot {
    function x(address to) external{
        while (foo[to].delegate != address(0)) { //parser ambiguity in the condition
            to = foo[to].delegate;
        }
    }
}