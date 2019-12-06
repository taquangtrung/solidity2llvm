pragma solidity >0.4.0;

contract Apple{
    uint money;
    constructor(uint x) public{

        uint[] memory array = new uint[](x);
    }
    function set(uint x) public{
        money = x;
    }
    /*
    function get() public returns(uint){
        return money;
    }
    */
}

contract Banana{
    uint money;
    constructor(uint x) public{
        Apple apple = new Apple(x);
        money = apple.money;
    }
    function set(uint x) public{
        money = x;
    }
    /*
    function get() public returns(uint) {
        return money;
    }
    */
}