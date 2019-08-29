pragma solidity >= 0.4.0;

contract SimpleStorage {
    uint storedData;

    function set(uint x) public {
        storedData = x;
        int n = 10;
        int a = 1;
        for (int i = 0; i < n; i++)
            a = a + i;
        a = a + 2;
    }
}
