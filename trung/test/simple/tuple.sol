pragma solidity >0.4.23 <0.7.0;

contract C {
  uint[] data;

  function f() public pure returns (uint, bool, uint) {
    return (7, true, 2);
  }

  function f2() public pure returns (uint, uint) {
    return (7, 2);
  }

  function g() public {
    int a = 2;
    (uint x, , uint y) = f();
    (uint n, uint m) = f2();
    (m,n) = f2();
    /* (x, y) = (y, x); */
    /* (data.length, , ) = f(); */
  }
}
