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
    int z = 1;
    if (y) {
      z = 2;
    }
    return z;
  }
}

contract Throw {
  function x(int a, int b) internal {
    assert((a + b == 0) || false);
  }

  function y() {
    x(1, 2);
  }
}

contract PayMe {
  function pay(address recipient) {
    recipient.transfer(this.balance - 1 wei);
  }
  function payMe() payable {
    msg.sender.transfer(this.balance - 1 wei);
  }
}

contract InsecureWallet {
    address owner;
    function initWallet(address newOwner) {
      owner = newOwner;
    }
    function withdraw(uint amount) {
      if (msg.sender == owner) {
        msg.sender.transfer(amount);
      }
    }
    function () payable {}
}

contract VulnerableRecursive {
  uint balance;
  function () payable {
    balance += msg.value;
  }
  function withdraw(uint value) {
    if (balance > value) {
      msg.sender.transfer(value);
      balance -= value;
    }
  }
}

contract SafeRecursive {
  uint balance;
  function () payable {
    balance += msg.value;
  }
  function withdraw(uint value) {
    if (balance > value) {
      balance -= value;
      msg.sender.transfer(value);
    }
  }
}
