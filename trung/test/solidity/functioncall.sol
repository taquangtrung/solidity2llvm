pragma solidity >0.4.0;

contract SimpleStorage {
    uint storedData;
    function gets ( uint a, uint b) public pure returns(uint, uint){
        return (a,b+1);
    }
    function get(uint a) public returns(uint){
        uint number = a;
        return a;
    }
    function set(uint x) public returns(uint){
        uint y = get(x);
        return y;
    }
    function sets(uint x) public returns(uint,uint,uint){
        storedData = x;
        (uint a,uint b) =  gets(x,x);
        //uint c = get(x);
        uint c = 1;
        return (a,b,c);
        // require(a > b);
    }

}
