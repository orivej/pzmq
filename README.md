# ZeroMQ 3.2+ bindings

API reference resides at http://orivej.github.com/pzmq/index.html

See examples in `examples.lisp`.

This release is **experimental** until all API is manually or automatically tested.

## Merits

* Complete ZeroMQ 3.2/3.3 API is covered, at least at low level.
* Everything is documented, at least preliminarily.
* Has some pleasant syntax sugar, aims for more.
* Handles interrupts in blocking calls, thanks to [Max Mikhanosha](https://github.com/7max): `msg-send` and `msg-recv` interrupted by GC are automatically restarted (option `*restart-interrupted-calls*`).

## Deficiencies

Everything not in `examples.lisp` has not been tested.

Only those conveniences useful in `examples.lisp` have been designed and implemented.

Raw performance has not yet been optimized.  This optimization probably matters only for 50000 messages per second or more of 10 kbytes or less, though.

Automatic tests have not been written.

## Special features (on top of basic features)

* WITH-CONTEXT and WITH-SOCKET accept options for CTX-NEW and SETSOCKOPT.
* WITH-CONTEXT establishes \*DEFAULT-CONTEXT\* for brevity in WITH-SOCKET.
* WITH-SOCKET establishes \*DEFAULT-CONTEXT\* when context is neigther specified nor found.  Nesting single socket in a context is rather common; combining them together in a single form fits nicely into the common WITH-* paradigm.

## References

For ZeroMQ 2, see [LISP-ZMQ](http://wandrian.net/lisp-zmq.html) by Nicolas Martyanoff.  I might have ported his version were I more interested in comparing ØMQ 2 with ØMQ 3 and less interested in adapting ØMQ 3 to Lisp on my own.
