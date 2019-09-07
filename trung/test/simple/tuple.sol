pragma solidity >0.4.23 <0.7.0;

contract C {
  uint[] data;

  function f() public pure returns (uint, bool, uint) {
    return (7, true, 2);
  }

  function g() public {
    int a = 2;
    (uint x, , uint y) = f();
    (x, y) = (y, x);
    (data.length, , ) = f();
  }
}
