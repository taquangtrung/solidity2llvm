pragma solidity >= 0.4.0;

contract SimpleStruct {

    struct Foo {
        uint value;
        uint data;
    }

    //uint storedData =1;
    uint storedData;

    function set(uint x) public {
        Foo memory a = Foo(x,2);
        x = x + 2;
        a.value = x;
        x = storedData + 1;
        storedData = 1;
        /* bytes32 s = "A"; */
        /* s = "B"; */
    }

    function get() public view returns (uint) {
        return storedData;
    }
}
