pragma solidity >= 0.4.0;

contract Arithmetic{
    function add(uint x, uint y) public returns(uint) {
        return x + y;
    }

    function sub(uint x, uint y) public returns(uint){
        return x - y;
    }

    function mul(uint x, uint y) public returns(uint){
        return x * y;
    }
    function div(uint x, uint y) public returns(uint){
        return x / y;
    }

}