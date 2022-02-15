# Introduction
emacc is a RISC-V C compiler writtten in Rust.

# Supported Language Feature
- i32 integer
- binary operator: +, -, *, /, =, ==, !=, >, >=, <, <=
- logic operator: ||, &&, !
- Bitwise operator: |, &
- if-else
- while

# Feature
- clang-style ast printing
- mem2reg
- SSA-form IR
- riscv32 instruction selector
- linear-scan register allocator

# Build and run
Just run `cargo run <file name>` and you can see the output of every pass.

For example, given this code:
```c=
int sum(int a, int b) {
        while (b > 0) {
                a = a + b;
		b = b - 1;
        }
        return a;
}
```
and run:
```
$ cargo run test.c
```

You will see:
```
-TranslationUnit
 `-FunctionDefinition sum 'int ()(int, int)'
   `-CompoundStmt
     |-WhileStmt
     | |-BinOpExpr '>'
     | | |-Var: b 'int'
     | | `-Constant '0'
     | `-CompoundStmt
     |   |-BinOpExpr '='
     |   | |-Var: a 'int'
     |   | `-BinOpExpr '+'
     |   |   |-Var: a 'int'
     |   |   `-Var: b 'int'
     |   `-BinOpExpr '='
     |     |-Var: b 'int'
     |     `-BinOpExpr '-'
     |       |-Var: b 'int'
     |       `-Constant '1'
     `-ReturnStmt
       `-Var: a 'int'

i32 sum(i32 %0, i32 %1)

label %2:					;preds: 
  %3 i32* = alloca
  %4 i32* = alloca
  %3 i32* = store, %0 i32
  %4 i32* = store, %1 i32
  br, label %5

label %5:					;preds: label %2 label %8 
  %6 i32 = load, %4 i32*
  %7 i32 = cmp gt, %6 i32, 0 i32
  br, %7 i32, label %8, label %9

label %8:					;preds: label %5 
  %10 i32 = load, %3 i32*
  %11 i32 = load, %4 i32*
  %12 i32 = add, %10 i32, %11 i32
  %3 i32* = store, %12 i32
  %13 i32 = load, %4 i32*
  %14 i32 = sub, %13 i32, 1 i32
  %4 i32* = store, %14 i32
  br, label %5

label %9:					;preds: label %5 
  %15 i32 = load, %3 i32*
  return, %15 i32

-------------------after mem2reg--------------

i32 sum(i32 %0, i32 %1)

label %2:					;preds: 
  br, label %3

label %3:					;preds: label %2 label %7 
  %4 i32 = phi, [%1 i32, label %2], [%9 i32, label %7]
  %5 i32 = phi, [%0 i32, label %2], [%8 i32, label %7]
  %6 i32 = cmp gt, %4 i32, 0 i32
  br, %6 i32, label %7, label %10

label %7:					;preds: label %3 
  %8 i32 = add, %5 i32, %4 i32
  %9 i32 = sub, %4 i32, 1 i32
  br, label %3

label %10:					;preds: label %3 
  return, %5 i32
%12 i32*
-----------after instruction lowering--------

i32 sum(i32 %0, i32 %1)

label %2:					;preds: 
  %12 i32* = alloca
  %12 i32* = store, x1
  %1 i32 = copy, x11
  %0 i32 = copy, x10
  br, label %3

label %3:					;preds: label %2 label %7 
  %4 i32 = phi, [%1 i32, label %2], [%9 i32, label %7]
  %5 i32 = phi, [%0 i32, label %2], [%8 i32, label %7]
  %6 i32 = add, 0 i32, x0
  blt, %6 i32, %4 i32, label %7
  br, label %10

label %7:					;preds: label %3 
  %8 i32 = add, %5 i32, %4 i32
  %.9 i32 = add, 1 i32, x0
  %9 i32 = sub, %.9 i32, %4 i32
  br, label %3

label %10:					;preds: label %3 
  x1 = load, %12 i32*
  return, %5 i32
---------------after constants mov-----------------

i32 sum(i32 %0, i32 %1)

label %2:					;preds: 
  %12 i32* = alloca
  %12 i32* = store, x1
  %1 i32 = copy, x11
  %0 i32 = copy, x10
  br, label %3

label %3:					;preds: label %2 label %7 
  %4 i32 = phi, [%1 i32, label %2], [%9 i32, label %7]
  %5 i32 = phi, [%0 i32, label %2], [%8 i32, label %7]
  %6 i32 = add, 0 i32, x0
  blt, %6 i32, %4 i32, label %7
  br, label %10

label %7:					;preds: label %3 
  %8 i32 = add, %5 i32, %4 i32
  %.9 i32 = add, 1 i32, x0
  %9 i32 = sub, %.9 i32, %4 i32
  br, label %3

label %10:					;preds: label %3 
  x1 = load, %12 i32*
  return, %5 i32
--------------LiveOut------------
label %3: %5 i32 | %4 i32 | x0 |
label %7: x0 | %9 i32 | %8 i32 |
label %10:
label %2: x0 | %0 i32 | %1 i32 |
--------------tuOeviL------------
label %2: [0, 6)
label %3: [7, 11)
label %7: [12, 17)
label %10: [18, 21)
LiveInterval:
reg: %4 i32 | %1 i32 | %9 i32 |
def point: 7 | 12 | 3 | 15 |
live range: [7, 11) [12, 15) [3, 6) [15, 17)
bind:
alloc:x11


LiveInterval:
reg: %5 i32 | %0 i32 | %8 i32 |
def point: 7 | 12 | 18 | 4 | 13 |
live range: [7, 11) [12, 13) [18, 20) [4, 6) [13, 17)
bind: x10
alloc:x10


LiveInterval:
reg: %6 i32 |
def point: 8 |
live range: [8, 9)
bind:
alloc:x12


LiveInterval:
reg: %.9 i32 |
def point: 14 |
live range: [14, 15)
bind:
alloc:x12


LiveInterval:
reg: x0 |
def point: 0 | 7 | 12 |
live range: [0, 6) [7, 11) [12, 17)
bind:
alloc:x0


LiveInterval:
reg: x1 |
def point: 0 | 19 |
live range: [0, 2) [19, 19)
bind:
alloc:x1


LiveInterval:
reg: x10 |
def point: 0 |
live range: [0, 4)
bind:
alloc:x10


LiveInterval:
reg: x11 |
def point: 0 |
live range: [0, 3)
bind:
alloc:x11


---------------after register allocation-----------

i32 sum(i32 %0, i32 %1)

label %2:					;preds: 
  %12 i32* = alloca
  %12 i32* = store, x1
  br, label %3

label %3:					;preds: label %2 label %7 
  x11 = phi, [x11, label %2], [x11, label %7]
  x10 = phi, [x10, label %2], [x10, label %7]
  x12 = add, 0 i32, x0
  blt, x12, x11, label %7
  br, label %10

label %7:					;preds: label %3 
  x10 = add, x10, x11
  x12 = add, 1 i32, x0
  x11 = sub, x12, x11
  br, label %3

label %10:					;preds: label %3 
  x1 = load, %12 i32*
  return, x10
--------------after useless jmp removed------------

i32 sum(i32 %0, i32 %1)

label %2:					;preds: 
  %12 i32* = alloca
  %12 i32* = store, x1

label %3:					;preds: label %2 label %7 
  x11 = phi, [x11, label %2], [x11, label %7]
  x10 = phi, [x10, label %2], [x10, label %7]
  x12 = add, 0 i32, x0
  blt, x12, x11, label %7
  br, label %10

label %7:					;preds: label %3 
  x10 = add, x10, x11
  x12 = add, 1 i32, x0
  x11 = sub, x12, x11
  br, label %3

label %10:					;preds: label %3 
  x1 = load, %12 i32*
  return, x10
----------------------emit asm---------------------
.globl   sum
sum:
        addi    sp, sp, -16
        sw      ra, 12(sp)
.BB_1
        addi    a2, x0, 0
        blt     a2, a1, .BB_2
        j       .BB_3
.BB_2
        add     a0, a0, a1
        addi    a2, x0, 1
        sub     a1, a2, a1
        j       .BB_1
.BB_3
        lw      ra, 12(sp)
        addi    sp, sp, 16
        ret
```
