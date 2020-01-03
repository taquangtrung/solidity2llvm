pragma solidity >=0.4.0;

contract SimpleArray {
    uint[10]x;
    uint[10][20] y;
    uint256 n = 10;
    function set(uint i, uint j) public{
        uint[] memory array2 = new uint[](n);
        x[1]= 1;
        y[i][j] = i + j;
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
        arr[0][0] = 1;
        return arr[k][l];
   }
}
