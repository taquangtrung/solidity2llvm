pragma solidity ^0.4.0;

contract SimpleStorage {
    uint storedData;

    function set(uint x) public {
        storedData = x;
        if (x > x)
            x = x + x;
        else
            x = x + x;
        x = x + x;
    }

    function get() public view returns (uint) {
        return storedData;
    }
}
