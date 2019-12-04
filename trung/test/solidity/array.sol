pragma solidity >0.4.0;

contract SimpleArray {
   uint[10][5] x;

   function set(uint i, uint j) public{
        uint[][] memory array = new uint[][](i+3);
        //array[1] =new uint[](2);
        //array[i+1][i] = j;
        //uint[] memory array2 = new uint[](i);
    }
}
