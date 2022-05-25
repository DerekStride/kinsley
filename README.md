# kinsley

As a follow up to my implmentation of [monkey](https://github.com/derekstride/monkey) in Rust, I've branched out to build
my own language implmentation. The focus of this project isn't to get a working language with features x, y, and z. It's
to explore compiler and virtual machine optimization. I would like each aspect of the features I implement to be
as well thought out and understood occasionally diving into the latest research on that topic.

Some guiding principles:

1. Explore - Prefer diving deep into a subject.
2. Performance - Prefer diving into optimizations over features.

The project is named after my daughter, Kinsley. I've named the compiler `kinc` pronounced `KIN-SEE`. Kinsley source
files have the `.kin` extension name.

You can run the compiler via the `bin/kinc.rs` program. The compiler implements a variety of optimizations such as
Register allocation, constant propagation, and unused instruction removal.

```
Usage:
    cargo run --bin=kinc -- FILEPATH

➜ kinsley cargo run --bin=kinc examples/constant_propagation.kin
Bytecode:

Instructions:
0: load!(0, 0)
1: set_global!(0, 0)
2: load!(0, 1)
3: set_global!(1, 0)

Constants:
13
-13
```

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

## Constant Propagation

I might have the name of this optimization incorrect, I didn't know the term to search for but I think this is close
enough. That said this optimization precompute constants, e.g. converts `let x = 3 * 4;` into `let x = 12;` in the
bytecode. To see it in action try running the `bin/constant_propagation.rs` program.

```
➜ kinsley/ cat examples/constant_propagation.kin
let pos = 1 + 3 * 4;
let neg = -1 + 3 * -4;

➜ kinsley/ cargo run --bin=constant_propagation -- examples/constant_propagation.kin

Before Constant Propagation:
Bytecode:

Instructions:
0: load!(0, 0)
1: load!(1, 1)
2: load!(2, 2)
3: mul!(3, 1, 2)
4: add!(4, 0, 3)
5: set_global!(0, 4)
6: load!(5, 3)
7: Neg { dest: 6, src: 5 }
8: load!(7, 4)
9: load!(8, 5)
10: Neg { dest: 9, src: 8 }
11: mul!(10, 7, 9)
12: add!(11, 6, 10)
13: set_global!(1, 11)

Constants:
1
3
4
1
3
4

After Constant Propagation:
Bytecode:

Instructions:
0: load!(4, 0)
1: set_global!(0, 4)
2: load!(11, 1)
3: set_global!(1, 11)

Constants:
13
-13
```

## Unused Instruction Removal

This optimization removes instructions that are not used by the program. You can see it in action with the
`bin/unused_instruction_removal.rs` program.

```
➜ kinsley cat examples/unused_instruction_removal.kin
let a = 0;
let b = 1;
a + b;              // Unused
let c = 2;
let d = 3;
let e = b + c;
c + d;              // Unused

➜ kinsley cargo run --bin=unused_instruction_removal -- examples/unused_instruction_removal.kin
Before removal:

Instructions:
0: load!(0, 0)
1: set_global!(0, 0)
2: load!(1, 1)
3: set_global!(1, 1)
4: add!(2, 0, 1)        // Remove
5: load!(3, 2)
6: set_global!(2, 3)
7: load!(4, 3)
8: set_global!(3, 4)
9: add!(5, 1, 3)
10: set_global!(4, 5)
11: add!(6, 3, 4)       // Remove

After removal:

Instructions:
0: load!(0, 0)
1: set_global!(0, 0)
2: load!(1, 1)
3: set_global!(1, 1)
4: load!(3, 2)
5: set_global!(2, 3)
6: load!(4, 3)
7: set_global!(3, 4)
8: add!(5, 1, 3)
9: set_global!(4, 5)
```
