# bblang[wip]
no loops(`For`, `While`...), just use recursion.  
Examples:
```rust
fn fib(x: i64)
    when x <= 2 then 1 
    otherwise fib(x - 1) + fib(x - 2);

fib(40);
```
define a macro:
```rust
macro hello
    when ($s: str) then "Hello" + $s
    when ($n: ident) then $n
    otherwise "";

print hello!("bblang");
let a: i64 = 1;
print hello!(a);
```
`map` as a macro:  
```rust
let arr = map!([1, 2, 3] x * x);
print arr
```

# Build
* ghc
* cabal
```
cabal run bblang
```

# Test
```
cabal test
```