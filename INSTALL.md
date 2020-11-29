# Installation instructions


* Install [nix](https://nixos.org/download.html)
* Clone repo and "cd dcc"
* Run the following bash commands
```bash
nix-shell
cabal configure
cabal build
```

* If you want to play around with the test cases:
```bash
nix-shell
cabal repl
```
and then (e.g. "import Test" and "test_eval2" in the repl)


## Misc

* For more info on using Nix for Haskell dev, see this [tutorial](https://github.com/Gabriel439/haskell-nix)
* You could also just use stack or cabal directly if you prefer