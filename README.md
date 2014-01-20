# ZeroMQ 4.0 bindings

API reference resides at http://orivej.github.io/pzmq/doc/index.html

See examples in `examples.lisp`.

## Merits

* Everything is documented, at least preliminarily.
* Handles interrupts in blocking calls, thanks to [Max Mikhanosha](https://github.com/7max): `msg-send` and `msg-recv` interrupted by GC are automatically restarted (option `*restart-interrupted-calls*`).

## Deficiencies

Everything not in `examples.lisp` or `tests.lisp` has not been tested.

Only those conveniences used in examples and tests have been designed and implemented.

Raw performance has not been fully optimized.  This optimization probably matters only for 50000 messages per second or more of 10 kbytes or less, though.

Barebone automatic tests.

## Special features (on top of basic features)

* WITH-CONTEXT and WITH-SOCKET accept options for CTX-NEW and SETSOCKOPT.
* WITH-CONTEXT establishes \*DEFAULT-CONTEXT\* for brevity in WITH-SOCKET.
* WITH-SOCKET establishes \*DEFAULT-CONTEXT\* when context is neigther specified nor found.  Nesting single socket in a context is rather common; combining them together in a single form fits nicely into the common WITH-* paradigm.
