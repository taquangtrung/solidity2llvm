pragma solidity >0.4.0;

contract SimpleStorage {
    uint storedData;
    function get_a ( int a ) public pure returns(int ){
        return a;
    }
    function set(uint x) public {
        storedData = x;
        int a = 1;
        get_a(a);
        int b = 2;
        int z = 3;
        int d = 4;
        if (a > b)
            z =  get_a(a);
        else
            z = b;

        d = a > b ?  get_a(a) : b;
        a > b ?  d = a : d = b;

    }
}
