pragma solidity >0.4.23 <0.7.0;

contract C {
  function g() public {
    int a;
    (int x, int y) = (1,2);
    (x, y) = (y, x);
  }
}
