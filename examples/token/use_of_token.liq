  type storage = unit

  let%entry send (parameter : Token.Token.instance * nat) storage =
    let token_contract, amount = parameter in
    let op = token_contract.transfer 0tz (Current.sender (), amount) in 
    [op], storage