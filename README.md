# Unreadable Mlatu

An implementation of Mlatu which generalises the primitive operators
into a sort of meta-language with quote matching and binding.

This implementation has a long term goal towards building a higher
performance term rewriting engine.

Because this language deviates slightly from standard Mlatu and is
more generalised, I'm calling it "generalised Mlatu".

Here are the six Mlatu primitives implemented in generalised Mlatu:
```
*x + = (*x) (*x).     # copy
*x - = .              # discard
*x > = ((*x)).        # wrap
*x < = *x.            # unwrap
*x *y ~ = (*y) (*x).  # swap
*x *y , = (*x *y).    # combine
```

Quote patterns match terms of the form `(foo bar baz)`, that's to
say it matches a quote with any child terms, parentheses included.

### Future Ideas
- Term interning (assign a unique ID to every term for faster comparison)
- Use hashing for rule matching
- Radix tree for rule matching
- Investigate whether it's possible to reduce some expressions at definition time

