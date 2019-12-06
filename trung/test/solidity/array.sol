pragma solidity >0.4.0;

contract SimpleArray {
    uint[10]x;
    uint256 n = 10;
    function set(uint i, uint j) public{
        uint[2][] memory array = new uint[2][](i+3);
        array[1] =new uint[](2);
        array[i+1][i] = j;
        uint[] memory array2 = new uint[](6);
        x[1]= 1;
        array2[j] = 2;
    }
   function initArray(uint k,uint l) public returns(uint){
        uint[][] memory arr = new uint[][](n);
        for (uint i=0; i < n; i++) {
            uint[] memory temp = new uint[](n);
            for(uint j = 0; j < n; j++){
                temp[j]=i+j;
            }
            arr[i] = temp;
        }
        return arr[k][l];
   }
}
