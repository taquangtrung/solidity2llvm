pragma solidity >0.4.0;

contract MappingExample {
    // map
    mapping(address => uint) public balances;
    // map with arrays
    //mapping(address => uint)[] public values;
    uint i = 0;

    function update(uint newBalance) public {
        balances[msg.sender] = newBalance;
       //values[i][msg.sender] = newBalance;
    }
}