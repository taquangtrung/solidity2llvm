pragma solidity >0.4.23 <0.7.0;

contract C {
  function g() public {
    int a;
    (int x, int y) = (1,2);
    (x, y) = (y, x);
    (x, ) = (2, 3);
    (int z, int t) = (x, x);
    (z, t) = (y, y);
  }
}
