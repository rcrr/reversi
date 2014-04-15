;;;
;;; Copyright (c) 2014 Roberto Corradini

;;; This file is part of the reversi program
;;; http://github.com/rcrr/reversi

;;; This pogram takes origin from the blog entry written by
;;; Mike Vollmer and found at:
;;; http://recurial.com/programming/efficient-bit-scaning-in-lisp/

;;; This file, is distributed under the GNU GPL v3 license.

;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation; either version 3, or (at your option) any
;;; later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA

;;; Efficient bit-scanning in Lisp

;;; In this post I’m going to describe something I found quite exciting.
;;; I am in the process of developing a modern chess engine in Common Lisp, and I am trying to implement the low-level functionality as efficiently as possible.
;;; In this case, for one of the most performance-critical aspects of the program, I managed to achieve performance comparable to C.
;;; I am always happy when I can make Lisp run as fast as C, and this experience has left me in awe of SBCL.

;;; But first, some background on the problem I was trying to solve.

;;; It is common for modern chess-playing programs to represent chess boards with bits.
;;; A chess board is 8 by 8, meaning it has 64 cells. This means, if you have one 64-bit unsigned integer per piece type (one for white pawns, etc),
;;; you can represent the whole board using 1′s and 0′s in a very compact and efficient way. This type of board representation is called a “bitboard.”

;;; A huge bonus of bitboards is that there are bitwise operations you can do on boards to determine information really quickly.
;;; For example, to build a bitboard of all white pieces, you can and together the boards representing each white piece type.
;;; This can also be useful in evaluation: to determine if the white player has any pieces in the center of the board, for example,
;;; it can and the white bitboards with a pre-defined bitboard with 1′s in the center cells. If white has no pieces in that region, the operation will result in 0.

;;; One of the big ideas in bitboards is to avoid loops whenever possible. If the program ends up looping over the bits in a number, a lot of the benefit of using bitboards is lost.

;;; So, how would a program iterate over the pieces in a bitboard without looping over the bits? The answer is bit-scanning.
;;; It comes in two flavors: forward and backward bit-scanning. For this post I’m going to describe forward bit-scanning, but everything here applies equally to the reverse.

;;; Essentially, forward bit-scanning returns the index of the least significant 1 bit in a number.
;;; This is what you want if you want to iterate through only the 1 bits in a number.
;;; This happens a lot in many performance-critical places in a chess program, so it’s important that it be as fast as possible.
;;; Luckily, there are several techniques for optimizing it (including using a special hardware instruction on Intel CPUs… more on that later).

;;; For the most popular general approach, you need to be aware of something called de Bruijn sequences.
;;; This can be used to construct a hash function that determines the least significant 1 bit of a number.

;;; A 64-bit de Bruijn sequence contains 64 overlapped 6-bit sequences, forming a circle of 64 bits,
;;; where five leading zeros overlap five hidden trailing zeros.
;;; The 1 bit from the bitboard can be isolated through the intersection of the number and its two’s complement.
;;; From there, multiplying the 64-bit de Bruijn sequence with the isolated number yields a unique 6-bit sub-sequence.
;;; Those six bits can be extracted and used as the hash key to look up the index of the least significant 1 bit in a pre-computed table.

(in-package :cl-user)

;;; The following code is from the Chess Programming Wiki:

;;; const int index64[64] = {
;;;     0,  1, 48,  2, 57, 49, 28,  3,
;;;    61, 58, 50, 42, 38, 29, 17,  4,
;;;    62, 55, 59, 36, 53, 51, 43, 22,
;;;    45, 39, 33, 30, 24, 18, 12,  5,
;;;    63, 47, 56, 27, 60, 41, 37, 16,
;;;    54, 35, 52, 21, 44, 32, 23, 11,
;;;    46, 26, 40, 15, 34, 20, 31, 10,
;;;    25, 14, 19,  9, 13,  8,  7,  6
;;; };
;;; 
;;; int bitScanForward(U64 bb) {
;;;    const U64 debruijn64 = C64(0x03f79d71b4cb0a89);
;;;    assert (bb != 0);
;;;    return index64[((bb & -bb) * debruijn64) >> 58];
;;; }

;;; The math behind it is complicated and confusing, but the implementation is simple and fast.
;;; What might have been a loop through each bit in the number is now a few instructions and a single memory reference. That’s a big improvement, certainly.

;;; Now, this kind of thing is C’s bread and butter. The C programming language is great at low-level bit manipulation tasks like this.
;;; Lisp, on the other hand, is not usually thought of as a language good at low-level tasks.

;;; I briefly considered using bit vectors for bitboards in Lisp, and just accepting the performance penalty of looping.
;;; I knew that SBCL could easily do unboxed arithmetic on 64-bit integers, however, so I gave that a shot.

;;; The first thing to note about the above C code, if you didn’t notice, is that the code alternately treats the same value as signed and unsigned.
;;; In C, negation on an unsigned number works that way, which is what the algorithm calls for.

;;; I could have come up with a different way of doing the same operation, but I wanted to mimic the above C code as closely as I could,
;;; so I specified all the types in my function such that it switches between treating the number as unsigned and signed as necessary.

(defconstant table64 '(63  0 58  1 59 47 53  2
                       60 39 48 27 54 33 42  3
                       61 51 37 40 49 18 28 20
                       55 30 34 11 43 14 22  4
                       62 57 46 52 38 26 32 41
                       50 36 17 19 29 10 13 21
                       56 45 25 31 35 16  9 12
                       44 24 15  8 23  7  6  5))

(defconstant index64 (make-array 64 :element-type '(unsigned-byte 16)
                                 :initial-contents table64))

(defconstant debruijn64 #x07EDD5E59A4E28C2)
 
(defun bitscan-slow (bb)
  (aref index64 (mod (ash (* (logand bb (- bb)) debruijn64) -58) 64)))
 
(declaim (inline bitscan))
(defun bitscan (bb)
  (declare (optimize speed (safety 0) (debug 0) (space 0) (compilation-speed 0))
           (type (unsigned-byte 64) bb))
  (let* ((negbb (the (signed-byte 64) (- (the (signed-byte 64) bb))))
         (mulbb (the (unsigned-byte 64) (* (logand bb negbb) debruijn64)))
         (ashbb (the (unsigned-byte 64) (ash mulbb -58))))
    (declare (type (simple-array (unsigned-byte 16) (64)) index64))
    (aref index64 ashbb)))

;;; I started with the bitscan-slow function, and I added optimizations incrementally,
;;; checking the generated code with (disassemble #'bitscan) after each change.
;;; SBCL helpfully highlights places in the disassembled code where it is calling generic functions for arithmetic (like negation).
;;; To ensure that it knew the exact type of the number at each step, I ended up specifying the type using the for each sub-expression.
;;; This allowed me to control its representation exactly, even switching between unsigned to signed and back again. The generated code looks like this:

;;; 70:       488BC1           MOV RAX, RCX
;;; 73:       488BD0           MOV RDX, RAX
;;; 76:       48F7DA           NEG RDX
;;; 79:       488BC1           MOV RAX, RCX
;;; 7C:       4821D0           AND RAX, RDX
;;; 7F:       48F72562000000   MUL RAX, [RIP+98]
;;; 86:       48C1E83A         SHR RAX, 58
;;; 8A:       48D1E0           SHL RAX, 1
;;; 8D:       488B0D6CFFFFFF   MOV RCX, [RIP-148]
;;; 94:       0FB7540101       MOVZX EDX, WORD PTR [RCX+RAX+1]
;;; 99:       48D1E2           SHL RDX, 1
;;; 9C:       488BE5           MOV RSP, RBP
;;; 9F:       F8               CLC
;;; A0:       5D               POP RBP
;;; A1:       C3               RET

;;; You can trace through the assembly and match all the actions back to the lisp code. SBCL did exactly what I told it to do.
;;; The assembly code in this case is almost more compact than the Lisp code above, but the verbosity of the Lisp could easily be abstracted away with macros.T
;;; The important point is that I was able to make Lisp yield this compact, efficient machine code. No jumps, function calls, memory allocations, or anything like that.

;;; But I can do better.

;;; Somewhere near the beginning of this post I mentioned that Intel CPUs have a special instruction for bit-scanning.
;;; Actually, they have two. Bit-scan forward (bsf) and bit-scan reverse (bsr).

;;; In a language like C it’s relatively easy to include inline assembly code, though it’s dangerous.
;;; Most C compilers provide some kind of “intrinsic” support for instructions like bsf and bsr, however, and SBCL does not.

;;; Luckily, SBCL allows you to define your own “virtual operators” that let you teach SBCL’s compiler new tricks by specifying assembly code in s-expression form.
;;; This has huge implications beyond this blog post (you can have macros generate assembly language, for example).
;;; I just want a way to invoke the Intel bsf instruction. Turns out it was pretty easy, though there’s a noticeable lack of documentation on writing VOPs.

(declaim (optimize speed (safety 0)))
(sb-c:defknown %bsf ((unsigned-byte 64)) (unsigned-byte 32) (sb-c::movable sb-c::foldable sb-c::flushable))
(sb-c:define-vop (%bsf)
  (:policy :fast)
  (:translate %bsf)
  (:note "Scans forward for the first 1 bit")
  (:args (a :scs (sb-vm::unsigned-reg) :target b))
  (:arg-types sb-vm::unsigned-byte-64)
  (:results (b :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator
    0
    (sb-c::inst sb-vm::bsf b a)))
(defun bsf (a)
  (%bsf a))

;;; The function definition at the bottom seems paradoxical, but it’s just to “wrap” the operator in a function.
;;; I won’t go into the details of defknown or define-vop, but I will show you the assembly code generated for my new bitscan function:

;;; 90:       480FBCD2         BSF RDX, RDX
;;; 94:       48D1E2           SHL RDX, 1
;;; 97:       488BE5           MOV RSP, RBP
;;; 9A:       F8               CLC
;;; 9B:       5D               POP RBP
;;; 9C:       C3               RET

;;; That’s more like it!

;;; I briefly compared the performance of the Lisp code and the C code. I compared the de Bruijn versions,
;;; because using the programs using the built-in instruction were essentially identical in both cases.

;;;     GCC with -O0: 1250 ms
;;;     Ordinary GCC:  500 ms
;;;     SBCL:          490 ms
;;;     GCC with -O3:  150 ms

;;; The results were essentially what I expected: compiled with no extra flags with GCC, the C code was slightly slower than the Lisp code.
;;; When compiled with -O3, the C code was faster.

;;; To even detect these differences, however, I had to run the function thousands of times.

;;; I chose to use the bsr instruction in my chess program, and I came away from this experience very impressed with SBCL as a compiler and platform:
;;; not only was it able to produce machine code similar to that of GCC for the de Bruijn function,
;;; it was extendable enough to allow me to use a custom instruction freely and naturally in my code.

