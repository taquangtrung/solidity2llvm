pragma solidity ^0.4.0;

contract SimpleStorage {
    uint storedData;

    function set(uint x) public {
        storedData = x;
        x = x + 2;
        bytes32 s = "A";
        /* s = "B"; */
    }

    function get() public view returns (uint) {
        return storedData;
    }
}
