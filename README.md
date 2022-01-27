# kinsley

As a follow up to my implmentation of [monkey](https://github.com/derekstride/monkey) in Rust, I've branched out to build
my own language implmentation. The focus of this project isn't to get a working language with features x, y, and z. It's
to explore compiler and virtual machine optimization. I would like each aspect of the features I implement to be
as well thought out and understood occasionally diving into the latest research on that topic.

Some guiding principles:

1. Explore - Prefer diving deep into a subject.
2. Performance - Prefer diving into optimizations over features.

## Register Allocation

For an example of the types of problems I've been exploring check out this blog post on [Register Allocation by Chris
Seaton](https://chrisseaton.com/truffleruby/register-allocation/). I'm building a register based virtual machine so I
implemented a Register allocator for compacting the registers used throughout the program. You can see a before and
after view of the registers live ranges used by compiler by running the `bin/register_reallocator.rs` program.

```
➜ kinsley/ cargo run --bin=register_reallocator -- examples/register_reallocator.kin

Before reassignment:
                                r0  r1  r2  r3  r4  r5  r6
  0: load!(0, 0)                *
  1: set_global!(0, 0)          *
  2: load!(1, 1)                *   *
  3: set_global!(1, 1)          *   *
  4: add!(2, 0, 1)              *   *   *
  5: load!(3, 2)                    *       *
  6: set_global!(2, 3)              *       *
  7: load!(4, 3)                    *       *   *
  8: set_global!(3, 4)              *       *   *
  9: add!(5, 1, 3)                  *       *   *   *
 10: add!(6, 3, 4)                          *   *       *

After reassignment:
                                r0  r1  r2  r3
  0: load!(0, 0)                *
  1: set_global!(0, 0)          *
  2: load!(1, 1)                *   *
  3: set_global!(1, 1)          *   *
  4: add!(2, 0, 1)              *   *   *
  5: load!(0, 2)                *   *
  6: set_global!(2, 0)          *   *
  7: load!(2, 3)                *   *   *
  8: set_global!(3, 2)          *   *   *
  9: add!(3, 1, 0)              *   *   *   *
 10: add!(1, 0, 2)              *   *   *

```
