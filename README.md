# dcc
Decentralized cryptocurrency

Proof of concept for a blockchain with variable hash functions for ASIC resistance.  Any miner can use whatever hash function and acceptance criterion they want (in place of SHA256 and some number of leading zeros for bitcoin).  Every node must define its own "scale" function, measuring the difficulty of any hash function it chooses to accept.

```bash
cabal update
cabal install cereal cryptonite
```
