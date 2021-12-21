# --- Day 16: Packet Decoder ---

Still Rust, thought it would be easy and quick. It isn't.

Had issues with recursive generics for the iterators
(overflow evaluating the requirement) hence the `dyn`.

Thankfully it was really easy to plug in the decode logic for part two.

```
% cargo run --release -q
The answer to the first part is: 951
The answer to the second part is: 902198718880
```
