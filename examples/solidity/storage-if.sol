pragma solidity ^0.4.0;

contract SimpleStorage {
    uint storedData;

    function set(uint x) public {
        storedData = x;
        if (x > 1)
            x = x + 1;
        else
            x = x + 2;
        x = x + 3;
    }

    function get() public view returns (uint) {
        return storedData;
    }
}
