pragma solidity ^0.4.0;

contract SimpleStorage {
    uint storedData;

    function set(uint x) public {
        storedData = x;
        int a = 1;
        int b = 2;
        int z = 3;
        if (a > b)
            z = a;
        else
            z = b;
    }
}
