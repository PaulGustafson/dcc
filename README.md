# dcc

Proof of concept for a blockchain with variable hash functions for ASIC resistance.  Any miner can use whatever hash function they want (in place of SHA256 for bitcoin).  Every node must define its own "scale" function, measuring the difficulty of any hash function it chooses to accept.

### FAQ

**Is there a simple explanation for non-technical people?**

The basic idea is make the algorithm maximally decentralized.  Give as much power to the nodes as possible, then the miners, and the least to the core developers.   More specifically, any choice that can be delegated to nodes should be.  So the nodes get to choose how much to value each hash algos, the block rewards, and the block size.  Subject to market constraints determined by the nodes, the miners can use whatever hash algo, block reward, and block size they want -- all on the same chain.    The core devs should only make updates if there is something that can't be fixed by updating these parameters -- ideally never.


**If both mining and non-mining nodes must manually (subjectively) adjust their criteria for block acceptance, what impetus is there for the network to stabilize to any sort of objective state?** 

With current blockchains, we achieve "objective state" at the cost of centralized control of the algorithm by some core group of developers. This results in perverse incentives to maintain the status quo (c.f. bitcoin transaction cost problems).

I have no idea if the scaling functions will stabilize on a singular chain or not. However, it would be a Pareto improvement over the status quo. Indeed, you could embed BTC and ETH into DCC (with the proviso that all users must use the core-developer-chosen hash functions and scaling functions), and both chains could live side-by-side.


**People choose now by either installing or not installing the updates core devs propose. What’s wrong with the current system of forking?**

The advantage in this case would be that the update could be decentralized.  A node could say "To me, verthash blocks are worth twice as much as blocks with the old hashing algorithm".  Others could reject verthash, or only accept verthash after a certain block height.   The miners would choose to run verthash or the old hash algo depending on market value and their hardware capabilities.  Each node would measure the block height using its own value function,  so there could potentially be many forked chains (up to the number of distinct value functions, but probably less since many value functions will point to the same highest block).
I guess there would be exchange rates between the forked chains for a while, but I think most of them would quickly die out (just like with blockchains in general)


**As far as I understand there is no such thing like ASIC resistance. Whenever there is an incentive people will build ASICs. It doesn’t matter how complicated your algorithm is.**

DCC solves that by not defining any hashing algorithm at all. Every block includes its own hash algorithm source code. Every node decides for itself whether to accept the hash algorithm and how much to value it. If people suspect an algorithm is getting ASIC'd, they can devalue it (if they want -- maybe the majority of nodes want ASICs for some reason, and that's fine too). The point is to let the users to choose the algorithm, not a core group of developers.


**If every block includes source code for an arbitrary hashing algorithm, do you expect arbitrary nodes to run arbitrary code to synchronise with your block? How do you know if some block provider's hashing algorithm terminates? How does your blockchain become trustless if hashing involves arbitrary code execution?**

A node _could_ run arbitrary code, but unless it trusts the code's provenance then it should use a white-list.

**Why not just settle on a fixed-but-extensible list of hashing algorithms that are known to work well today?**

That's what a whitelist will be, but with one major difference: each node gets to pick its own whitelist, there's no built-in centralization via the "one true source code."

**If a block provider gets to choose their hashing algorithm arbitrarily, they could pick a non-uniform hashing algorithm that is easy for themselves and hard for others.** 

Yes, miners will choose hashing algorithms that are easy for themselves and hard for others, but the nodes get to value each block from their own perspective. If they think a miner is using a weird hash function, they can penalize it or just blacklist the function.


