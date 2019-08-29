# Gas estimation:

  - Testing with Solidity 0.5.11

  - Command:

    ```
    cd examples/solidity
    solc --gas storage.sol
    ```

  - Input:

    ```
    contract SimpleStorage {
      uint storedData;

      function set(uint x) public {
          storedData = x;
          x = x + 2;
          bytes32 s = "A";
          /* s = "B"; */
      }

      function get() public view returns (uint) {
          return storedData;
      }
    }
    ```

  - Output:

    ```
    ======= storage.sol:SimpleStorage =======
    Gas estimation:
    construction:
       99 + 48400 = 48499
    external:
       get():	413
       set(uint256):	20247
    ```
  - Gas details of AST:

    ```
    solc --ast storage.sol
    ```

# Output EVM assembly

  ```
  solc --asm storage.sol
  ```
