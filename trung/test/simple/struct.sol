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
    bytes32 s = "A";
    s = "B";
  }

  function foo() public {
    Foo memory m;
    m.value = 1;
    m.data = 2;
  }

  function get() public view returns (uint) {
    /* Foo storage z = Foo({value: 2, data: 10}); */
    /* a.value = 5; */
    return storedData;
  }
}
