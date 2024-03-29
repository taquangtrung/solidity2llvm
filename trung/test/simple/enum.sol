// source: https://ethereum.stackexchange.com/questions/24086/how-do-enums-work

pragma solidity >= 0.4.4;

contract SimpleEnum {

  enum SomeData {DEFAULT, ONE, TWO}

  SomeData someData;

  function getSimpleEnum() public {
    someData = SomeData.DEFAULT;
  }

  function setValues(uint _value) public {
    /* require(uint(SomeData.TWO) >= _value); */
    uint x = _value;
    someData = SomeData(x);
  }

  function setValues2(uint _value) public {
    /* require(uint(SomeData.TWO) >= _value); */
    /* uint a = uint(SomeData.TWO); */
    /* require(_value >= 1); */
    someData = SomeData(_value);
  }

  function getValue() public returns (uint) {
    return uint(someData);
  }
}
