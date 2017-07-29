pragma solidity ^0.4.13;

contract ForLoop {
  function a() internal {}
  function() payable {
    for (int i = 0; i < 10; i++) {
      a();
    }
  }
}

contract Ternary {
  function x(bool y) payable returns (int) {
    return y ? 1 : 2;
  }
}
