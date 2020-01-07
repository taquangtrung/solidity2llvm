pragma solidity >= 0.4.0;

contract SimpleStorage {
    uint storedData=2;

    function set(uint x) public {
        storedData = x;
        x = get();

        //bytes32 s = "A";
        /* s = "B"; */
    }

    function get() public view returns (uint) {
        return storedData;
    }
}
