# liquidpacker
Package manager for liquidity of Tezos

### Requirements
```
Liquidity >= 4.01
```

### Liquidpacker Installation
```
opam pin https://github.com/catsigma/liquidpacker.git
liquidpacker setpath <your liquidity executable file path>
```

### Install package from local
```
liquidpacker install examples/foo
```
This will also install all the dependencies defined in the `liquidpacker` file in the package's root folder.

### Install package from git
```
liquidpacker install https://github.com/tezexchange/token-standard.git
```

### Build package
```
liquidpacker build examples/foo
liquidpacker build examples/use_of_token
```

### Liquidpacker file
```
(
  (name <the package name>)
  (version 0.1.0)
  (files <.liq file sequence>)
  (main <?module sequence>)
  (options <additional liquidity parameters>)
  (output <output dir name>)
  (deps 
    (<name> <directory location / git address> <version requirement>))
)
```

Example:
```
(
  (name foo)
  (version 0.1.0)
  (files a.liq b.liq)
  (main W X Y Q)
  (options "--json --no-annot")
  (output build)
  (deps 
    (bar ./../bar (>= 0.1))
    (token https://github.com/tezexchange/token-standard.git (<= 0.3)))
)
```