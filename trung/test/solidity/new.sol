pragma solidity >0.4.0;

contract Apple{
    uint public money;
    constructor(uint x) public{

        //uint[] memory array = new uint[](x);
        money = x;
    }
    function set(uint x) public{
        money = x;
    }

    function get() public returns(uint){
        return money;
    }

}

contract Banana{
    uint money;
    constructor(uint x) public{
        Apple a = new Apple(x);
        money = a.money();
        money = a.get();
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