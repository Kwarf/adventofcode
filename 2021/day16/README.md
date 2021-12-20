# --- Day 16: Packet Decoder ---

Still Rust, thought it would be easy and quick. It isn't.

Had issues with recursive generics for the iterators
(overflow evaluating the requirement) hence the `dyn`.

```
% cargo run --release -q
The answer to the first part is: 951
```
