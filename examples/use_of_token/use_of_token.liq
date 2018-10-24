type storage = unit

let%entry send (parameter : Token.TokenContract.instance * nat) storage =
  let token_contract, amount = parameter in
  let arg = Current.sender (), amount, (None : (Token.CallbackContract.instance * bytes) option) in
  let op = token_contract.transfer arg 0tz in 
  [op], storage