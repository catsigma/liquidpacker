# liqpack
Package manager for liquidity of Tezos

### Liqpack Installation
```
opam install liqpack
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
  (files <.liq file sequence>)
  (main <?module sequence>)
  (deps 
    (<name> <directory location / git address>))
)
```

Example:
```
(
  (name foo)
  (files a.liq b.liq)
  (main W X Y Q)
  (deps 
    (bar ./../bar)
    (token https://github.com/tezexchange/token-standard.git))
)
```