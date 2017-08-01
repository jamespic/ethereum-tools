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

contract IfElse {
  function x(bool y) payable returns (int) {
    if (y) {
      return 1;
    } else {
      return 2;
    }
  }
}

contract SimpleIf {
  function x(bool y) payable returns (int) {
    int x = 1;
    if (y) {
      x = 2;
    }
    return x;
  }
}
