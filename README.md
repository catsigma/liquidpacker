# liqpack
Package manager for liquidity of Tezos

## Installation
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

## Build package
```
liqpack build examples/foo
liqpack build examples/use_of_token
```
