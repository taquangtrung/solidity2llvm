pragma solidity >= 0.4.0;

contract SimpleStruct {
    struct Foo {
        uint value;
        uint data;
    }

    uint storedData = 5;

    Foo a = Foo({value: 1, data: 2}) ;
    Foo b;

    function set(uint x) public {
        x = x + 2;
        /* s = "B"; */
    }

    function get() public view returns (uint) {
        return storedData;
    }
}
