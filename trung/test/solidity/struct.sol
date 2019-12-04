pragma solidity >= 0.4.0;

contract SimpleStruct {
    struct Foo {
        uint value;
        uint data;
    }

    uint storedData;

    function set(uint x) public {
        Foo a;
        x = x + 2;
        /* bytes32 s = "A"; */
        /* s = "B"; */
    }

    function get() public view returns (uint) {
        return storedData;
    }
}
