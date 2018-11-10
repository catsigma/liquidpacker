# liqpack
Package manager for liquidity of Tezos

### Requirements
```
Liquidity >= 4.01
```

### Liqpack Installation
```
opam pin liqpack https://github.com/catsigma/liqpack.git
liqpack setpath <your liquidity executable file path>
```

### Install package from local
```
liqpack install examples/foo
```
This will also install all the dependencies defined in the `liqpack` file in the package's root folder.

### Install package from git
```
liqpack install https://github.com/tezexchange/token-standard.git
```

### Build package
```
liqpack build examples/foo
liqpack build examples/use_of_token
```

### Liqpack file
```
(
  (name <the package name>)
  (version 0.1.0)
  (files <.liq file sequence>)
  (main <?module sequence>)
  (deps 
    (<name> <directory location / git address> <version requirement>))
)
```

Example:
```
(
  (name foo)
  (files a.liq b.liq)
  (main W X Y Q)
  (deps 
    (bar ./../bar (>= 0.1))
    (token https://github.com/tezexchange/token-standard.git (<= 0.3)))
)
```