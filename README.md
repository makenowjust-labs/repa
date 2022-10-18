# repa

Regular Expression meets Presburger Arithmetic.

## Example

```console
$ cargo build -p grepa
$ ./target/debug/grepa '^(a{a}|i{i}|[^ai])*$' 'i >= 4 and a >= 4 and i = a' /usr/share/dict/words
antitintinnabularian
```
