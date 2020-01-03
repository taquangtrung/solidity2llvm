pragma solidity >0.4.0;

contract Apple{
    uint public weight;
    constructor(uint x) public{
        weight = x;
    }
    function set(uint x) public{
        weight = x;
    }

    function get() public returns(uint){
        return weight;
    }

}

contract Banana{
    uint weight;
    uint length;
    constructor(uint x) public{
        Apple a = new Apple(x);
        //weight = a.weight();
        //a.set(x+1);
        //weight = a.weight();
    }
    function set(uint x) public{
        weight = x;
    }
    /*
    function get() public returns(uint) {
        return money;
    }
    */
}