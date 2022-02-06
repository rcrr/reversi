
        /**
        *
        * Kogge Stone module implementation.
        *
        * kogge_stone.s
        *
 	* This file is part of the reversi program
	* http://github.com/rcrr/reversi
	* 
	* Author: Roberto Corradini mailto:rob_corradini@yahoo.it
	* Copyright 2022 Roberto Corradini. All rights reserved.
	*
	* License:
	* 
	* This program is free software; you can redistribute it and/or modify it
	* under the terms of the GNU General Public License as published by the
	* Free Software Foundation; either version 3, or (at your option) any
	* later version.
	*   
	* This program is distributed in the hope that it will be useful,
	* but WITHOUT ANY WARRANTY; without even the implied warranty of
	* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
	* GNU General Public License for more details.
	*
	* You should have received a copy of the GNU General Public License
	* along with this program; if not, write to the Free Software
	* Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
	* or visit the site <http://www.gnu.org/licenses/>.
	* 
	*/

        .file   "kogge_stone.s"

        .globl  kost_max_of_three
        .globl  kost_lms
        .globl  kost_make_move
        
        #
        # kost_max_of_three
        #
        # A function that returns the maximum value of its three integer arguments.
        # The function has signature:
        #
        #   int64_t kost_max_of_three (int64_t x, int64_t y, int64_t z)
        #
        # Parameters have been passed in rdi, rsi, and rdx.
        # The return the value is in rax.
        #
        
        .section .text
kost_max_of_three:
        mov     %rdi, %rax              # result (rax) initially holds x
        cmp     %rsi, %rax              # is x less than y?
        cmovl   %rsi, %rax              # if so, set result to y
        cmp     %rdx, %rax              # is max(x,y) less than z?
        cmovl   %rdx, %rax              # if so, set result to z
        ret                             # the max will be in eax
        
        #
        # kost_lms
        #
        # Returns the legal move set of the board.
        # The function has signature:
        #
        #   int64_t kost_lms (int64_t *board)
        #
        #
        # This variant of the Kogge-Stone algorithm takes three bitboard as imput:
        #
        #  - generator  == player
        #  - propagator == opponent
        #  - blocker    == empty
        #
        # and returns the bitboard obtained by the blocker subset that is formed by
        # the blockers hit by the rays, in the eight directions, obtained from the generators
        # traveling along the propagators.
        #
        # Passing as generator the player square set, as propagator the opponent square set,
        # and as blocker the empty square set, the returned value is a square set of the
        # legal moves.
        #

        #
        .section .rodata.mask,"aM",@progbits,32
        #
        .align 32
.mask0:                                         
        .quad	0xfefefefefefefefe              # all_squares_except_column_a
	.quad	0x7f7f7f7f7f7f7f7f              # all_squares_except_column_h
	.quad	0xffffffffffffffff              # all_squares
	.quad	0xfefefefefefefefe              # all_squares_except_column_a
        #
        .align 32
.mask1:
        .quad   0x7f7f7f7f7f7f7f7f              # all_squares_except_column_h
        .quad   0xfefefefefefefefe              # all_squares_except_column_a
        .quad   0xffffffffffffffff              # all_squares
        .quad   0x7f7f7f7f7f7f7f7f              # all_squares_except_column_h
        #
        .align 32
.slide:
        .quad   1                               # Slide one square W  (shr) - E  (shl)
        .quad   7                               # Slide one square NE (shr) - SW (shl)
        .quad   8                               # Slide one square N  (shr) - S  (shl)
        .quad   9                               # Slide one square NW (shr) - SE (shl)

        .section .text
kost_lms:
        # rdi : mover
        # rsi : opponent
        # rdx : empties
        #
        vmovq           %rdi, %xmm15            # MOVD/MOVQ VEX.128.66.0F.W1 6E /r VMOVQ xmm1, r64/m64
        vpbroadcastq	%xmm15, %ymm0           # ymm0  - generator  - player
        vmovq           %rsi, %xmm15            #
        vpbroadcastq	%xmm15, %ymm2           # ymm2  - propagator - opponent
        vmovq           %rdx, %xmm15
        vpbroadcastq	%xmm15, %ymm4           # ymm4  - blocker    - empty
        #
        vmovdqa         .mask0(%rip), %ymm5     # ...
        vmovdqa         .mask1(%rip), %ymm6     # ...
        vmovdqa         .slide(%rip), %ymm1     # ...
        vmovdqa         %ymm1, %ymm7            # ymm7 is the dynamic slide value
        #
        # g: ymm10/ymm11
        # p: ymm12/ymm13
        # t: ymm14/ymm15 - tmp registers
        #
        vmovdqa         %ymm0, %ymm10           # g = generator
        vmovdqa         %ymm0, %ymm11           #
        vpand           %ymm2, %ymm5, %ymm12    # p = propagator & mask[i]
        vpand           %ymm2, %ymm6, %ymm13    #
        #
        # START of first slide.
        #
        # g |= p & ((g << slide_up_1[i]) >> slide_dw_1[i])
        #
        # t = (g << slide_up_1[i]) >> slide_dw_1[i])
        #
        vpsllvq         %ymm7, %ymm10, %ymm14  # ymm14 = ymm10 << ymm7
        vpsrlvq         %ymm7, %ymm11, %ymm15  # ymm15 = ymm11 >> ymm7
        #
        # t &= p
        #
        vpand           %ymm14, %ymm12, %ymm14 #
        vpand           %ymm15, %ymm13, %ymm15 #        
        #
        # g |= t
        #
        vpor            %ymm10, %ymm14, %ymm10 #
        vpor            %ymm11, %ymm15, %ymm11 #
        #
        # p &= ((p << slide_up_1[i]) >> slide_dw_1[i])
        #
        # t = ((p << slide_up_1[i]) >> slide_dw_1[i])
        #
        vpsllvq         %ymm7, %ymm12, %ymm14  # ymm14 = ymm10 << ymm7
        vpsrlvq         %ymm7, %ymm13, %ymm15  # ymm15 = ymm11 >> ymm7
        #
        # p &= t
        #
        vpand           %ymm14, %ymm12, %ymm12 #
        vpand           %ymm15, %ymm13, %ymm13 #
        #
        # END of first slide.
        #
        vpsllq          $1, %ymm7, %ymm7       # increment slide value: { 1, 7, 8, 9 } --> { 2, 14, 16, 18 }
        #
        # START of second slide.
        #
        # g |= p & ((g << slide_up_2[i]) >> slide_dw_2[i])
        #
        # t = ((g << slide_up_2[i]) >> slide_dw_2[i])
        #
        vpsllvq         %ymm7, %ymm10, %ymm14  # ymm14 = ymm10 << ymm7
        vpsrlvq         %ymm7, %ymm11, %ymm15  # ymm15 = ymm11 >> ymm7
        #
        # t &= p
        #
        vpand           %ymm14, %ymm12, %ymm14 #
        vpand           %ymm15, %ymm13, %ymm15 #        
        #
        # g |= t
        #
        vpor            %ymm10, %ymm14, %ymm10 #
        vpor            %ymm11, %ymm15, %ymm11 #
        #
        # p &= ((p << slide_up_2[i]) >> slide_dw_2[i])
        #
        # t = ((p << slide_up_2[i]) >> slide_dw_2[i])
        #
        vpsllvq         %ymm7, %ymm12, %ymm14  # ymm14 = ymm10 << ymm7
        vpsrlvq         %ymm7, %ymm13, %ymm15  # ymm15 = ymm11 >> ymm7
        #
        # p &= t
        #
        vpand           %ymm14, %ymm12, %ymm12 #
        vpand           %ymm15, %ymm13, %ymm13 #
        #
        # END of second slide.
        #
        vpsllq          $1, %ymm7, %ymm7       # increment slide value: { 2,  14, 16, 18 } --> { 4, 28, 32, 36 }
        #
        # START of third slide.
        #
        # g |= p & ((g << slide_up_4[i]) >> slide_dw_4[i])
        #
        # t = ((g << slide_up_4[i]) >> slide_dw_4[i])
        #
        vpsllvq         %ymm7, %ymm10, %ymm14  # ymm14 = ymm10 << ymm7
        vpsrlvq         %ymm7, %ymm11, %ymm15  # ymm15 = ymm11 >> ymm7
        #
        # t &= p
        #
        vpand           %ymm14, %ymm12, %ymm14 #
        vpand           %ymm15, %ymm13, %ymm15 #        
        #
        # g |= t
        #
        vpor            %ymm10, %ymm14, %ymm10 #
        vpor            %ymm11, %ymm15, %ymm11 #
        #
        # END of third slide.
        #
        #
        # START of fourth slide.
        #
        # g = ~generator & g
        #
        vpandn          %ymm10, %ymm0, %ymm10 #
        vpandn          %ymm11, %ymm0, %ymm11 #
        #
        # g = blocker & mask[i] & ((g << slide_up_1[i]) >> slide_dw_1[i])
        #
        # t = ((g << slide_up_1[i]) >> slide_dw_1[i])
        #
        vpsllvq         %ymm1, %ymm10, %ymm14  # ymm14 = ymm10 << ymm1
        vpsrlvq         %ymm1, %ymm11, %ymm15  # ymm15 = ymm11 >> ymm1
        #
        # t &= mask[i] 
        #
        vpand           %ymm14, %ymm5, %ymm14  #
        vpand           %ymm15, %ymm6, %ymm15  #        
        #
        # t &= blocker
        #
        vpand           %ymm14, %ymm4, %ymm14  #
        vpand           %ymm15, %ymm4, %ymm15  #        
        #
        # END of fourth slide.
        #
        #
        # result |= g
        #
	vpor	        %ymm14, %ymm15, %ymm14 # t[1:4] = t[1:4] | t[5:8] # reduce the eight values in ymm14 and ymm15 into four held by ymm14
	vmovdqa	        %xmm14, %xmm7          # u[1:2] = t[1:2]
	vextracti128    $0x1, %ymm14, %xmm14   # v[1:2] = t[3:4]
	vpor            %xmm14, %xmm7, %xmm14  # v[1:2] = v[1:2] | u[1:2]
	vpsrldq         $8, %xmm14, %xmm7      # u[2] = 0, u[1] = v[2]    # Shift xmm14 right by 8 bytes while shifting in 0s.
	vpor            %xmm7, %xmm14, %xmm14  # v[1] = v[1] | u[1]       # reduce the last two values into the first half of xmm14
	vmovq           %xmm14, %rax           # copy the first half of xmm14 into rax
        #
        # return result
        #
	vzeroupper
        ret
        #
        
kost_make_move:
        # rdi : move     : generator
        # rsi : opponent : propagator
        # rdx : mover    : blocker
        #
        vmovq           %rdi, %xmm15            # MOVD/MOVQ VEX.128.66.0F.W1 6E /r VMOVQ xmm1, r64/m64
        vpbroadcastq	%xmm15, %ymm0           # ymm0  - generator  - player
        vmovq           %rsi, %xmm15            #
        vpbroadcastq	%xmm15, %ymm2           # ymm2  - propagator - opponent
        vmovq           %rdx, %xmm15
        vpbroadcastq	%xmm15, %ymm4           # ymm4  - blocker    - empty
        #
        vmovdqa         .mask0(%rip), %ymm5     # ...
        vmovdqa         .mask1(%rip), %ymm6     # ...
        vmovdqa         .slide(%rip), %ymm1     # ...
        vmovdqa         %ymm1, %ymm7            # ymm7 is the dynamic slide value
        #
        # g: ymm10/ymm11
        # p: ymm12/ymm13
        # t: ymm14/ymm15 - tmp registers
        #
        # pro_base : ymm8/ymm9
        #
        vmovdqa         %ymm0, %ymm10           # g = generator
        vmovdqa         %ymm0, %ymm11           #
        vpand           %ymm2, %ymm5, %ymm8     # p = propagator & mask[i]
        vpand           %ymm2, %ymm6, %ymm9     #
        #
        # START of first forward slide.
        #
        # g |= p & ((g << slide_up_1[i]) >> slide_dw_1[i])
        #
        #   t = (g << slide_up_1[i]) >> slide_dw_1[i])
        #   t &= p
        #   g |= t
        #
        vpsllvq         %ymm7, %ymm10, %ymm14  # ymm14 = ymm10 << ymm7
        vpsrlvq         %ymm7, %ymm11, %ymm15  # ymm15 = ymm11 >> ymm7
        vpand           %ymm14, %ymm8, %ymm14  #
        vpand           %ymm15, %ymm9, %ymm15  #        
        vpor            %ymm10, %ymm14, %ymm10 #
        vpor            %ymm11, %ymm15, %ymm11 #
        #
        # p &= ((p << slide_up_1[i]) >> slide_dw_1[i])
        #
        #   t = ((p << slide_up_1[i]) >> slide_dw_1[i])
        #   p &= t
        #
        vpsllvq         %ymm7, %ymm8, %ymm14  # ymm14 = ymm10 << ymm7
        vpsrlvq         %ymm7, %ymm9, %ymm15  # ymm15 = ymm11 >> ymm7
        vpand           %ymm14, %ymm8, %ymm12 #
        vpand           %ymm15, %ymm9, %ymm13 #
        #
        # END of first forward slide.
        #
        vpsllq          $1, %ymm7, %ymm7       # increment slide value: { 1, 7, 8, 9 } --> { 2, 14, 16, 18 }
        #
        # START of second foward slide.
        #
        # g |= p & ((g << slide_up_2[i]) >> slide_dw_2[i])
        #
        #   t = ((g << slide_up_2[i]) >> slide_dw_2[i])
        #   t &= p
        #   g |= t
        #
        vpsllvq         %ymm7, %ymm10, %ymm14  # ymm14 = ymm10 << ymm7
        vpsrlvq         %ymm7, %ymm11, %ymm15  # ymm15 = ymm11 >> ymm7
        vpand           %ymm14, %ymm12, %ymm14 #
        vpand           %ymm15, %ymm13, %ymm15 #        
        vpor            %ymm10, %ymm14, %ymm10 #
        vpor            %ymm11, %ymm15, %ymm11 #
        #
        # p &= ((p << slide_up_2[i]) >> slide_dw_2[i])
        #
        #   t = ((p << slide_up_2[i]) >> slide_dw_2[i])
        #   p &= t
        #
        vpsllvq         %ymm7, %ymm12, %ymm14  # ymm14 = ymm10 << ymm7
        vpsrlvq         %ymm7, %ymm13, %ymm15  # ymm15 = ymm11 >> ymm7
        vpand           %ymm14, %ymm12, %ymm12 #
        vpand           %ymm15, %ymm13, %ymm13 #
        #
        # END of second forward slide.
        #
        vpsllq          $1, %ymm7, %ymm7       # increment slide value: { 2,  14, 16, 18 } --> { 4, 28, 32, 36 }
        #
        # START of third forward slide.
        #
        # g |= p & ((g << slide_up_4[i]) >> slide_dw_4[i])
        #
        #   t = ((g << slide_up_4[i]) >> slide_dw_4[i])
        #   t &= p
        #   g |= t
        #
        vpsllvq         %ymm7, %ymm10, %ymm14  # ymm14 = ymm10 << ymm7
        vpsrlvq         %ymm7, %ymm11, %ymm15  # ymm15 = ymm11 >> ymm7
        vpand           %ymm14, %ymm12, %ymm14 #
        vpand           %ymm15, %ymm13, %ymm15 #        
        vpor            %ymm10, %ymm14, %ymm10 #
        vpor            %ymm11, %ymm15, %ymm11 #
        #
        # END of third forward slide.
        #
        #
        # START of fourth forward slide.
        #
        # g &= ~generator
        #
        vpandn          %ymm10, %ymm0, %ymm10 #
        vpandn          %ymm11, %ymm0, %ymm11 #
        #
        # g = blocker & mask[i] & ((g << slide_up_1[i]) >> slide_dw_1[i])
        #
        #   t = ((g << slide_up_1[i]) >> slide_dw_1[i])
        #   t &= mask[i]
        #   t &= blocker
        #
        vpsllvq         %ymm1, %ymm10, %ymm14  # ymm14 = ymm10 << ymm1
        vpsrlvq         %ymm1, %ymm11, %ymm15  # ymm15 = ymm11 >> ymm1
        vpand           %ymm14, %ymm5, %ymm14  #
        vpand           %ymm15, %ymm6, %ymm15  #        
        vpand           %ymm14, %ymm4, %ymm10  #
        vpand           %ymm15, %ymm4, %ymm11  #        
        #
        # END of fourth forward slide.
        #
        # START of first backward slide.
        #
        # g |= p & ((g << slide_dw_1[i]) >> slide_up_1[i])
        #
        #   t = (g << slide_dw_1[i]) >> slide_up_1[i])
        #   t &= p
        #   g |= t
        #
        vpsrlvq         %ymm1, %ymm10, %ymm14  # ymm14 = ymm10 << ymm1
        vpsllvq         %ymm1, %ymm11, %ymm15  # ymm15 = ymm11 >> ymm1
        vpand           %ymm14, %ymm8, %ymm14  #
        vpand           %ymm15, %ymm9, %ymm15  #        
        vpor            %ymm10, %ymm14, %ymm10 #
        vpor            %ymm11, %ymm15, %ymm11 #
        #
        # p &= ((p << slide_dw_1[i]) >> slide_up_1[i])
        #
        #   t = ((p << slide_dw_1[i]) >> slide_up_1[i])
        #   p &= t
        #
        vpsrlvq         %ymm1, %ymm8, %ymm14  # ymm14 = ymm10 << ymm1
        vpsllvq         %ymm1, %ymm9, %ymm15  # ymm15 = ymm11 >> ymm1
        vpand           %ymm14, %ymm8, %ymm12 #
        vpand           %ymm15, %ymm9, %ymm13 #
        #
        # END of first forward slide.
        #
        vpsllq          $1, %ymm1, %ymm1       # increment slide value: { 1, 7, 8, 9 } --> { 2, 14, 16, 18 }
        #
        # START of second backward slide.
        #
        # g |= p & ((g << slide_dw_2[i]) >> slide_up_2[i])
        #
        #   t = ((g << slide_dw_2[i]) >> slide_up_2[i])
        #   t &= p
        #   g |= t
        #
        vpsrlvq         %ymm1, %ymm10, %ymm14  # ymm14 = ymm10 << ymm1
        vpsllvq         %ymm1, %ymm11, %ymm15  # ymm15 = ymm11 >> ymm1
        vpand           %ymm14, %ymm12, %ymm14 #
        vpand           %ymm15, %ymm13, %ymm15 #        
        vpor            %ymm10, %ymm14, %ymm10 #
        vpor            %ymm11, %ymm15, %ymm11 #
        #
        # p &= ((p << slide_dw_2[i]) >> slide_up_2[i])
        #
        #   t = ((p << slide_dw_2[i]) >> slide_up_2[i])
        #   p &= t
        #
        vpsrlvq         %ymm1, %ymm12, %ymm14  # ymm14 = ymm10 << ymm1
        vpsllvq         %ymm1, %ymm13, %ymm15  # ymm15 = ymm11 >> ymm1
        vpand           %ymm14, %ymm12, %ymm12 #
        vpand           %ymm15, %ymm13, %ymm13 #
        #
        # END of second backward slide.
        #
        vpsllq          $1, %ymm1, %ymm1       # increment slide value: { 2,  14, 16, 18 } --> { 4, 28, 32, 36 }
        #
        # START of third backward slide.
        #
        # g |= p & ((g << slide_dw_4[i]) >> slide_up_4[i])
        #
        #   t = ((g << slide_dw_4[i]) >> slide_up_4[i])
        #   t &= p
        #   g |= t
        #
        vpsrlvq         %ymm1, %ymm10, %ymm14  # ymm14 = ymm10 << ymm1
        vpsllvq         %ymm1, %ymm11, %ymm15  # ymm15 = ymm11 >> ymm1
        vpand           %ymm14, %ymm12, %ymm14 #
        vpand           %ymm15, %ymm13, %ymm15 #        
        vpor            %ymm10, %ymm14, %ymm10 #
        vpor            %ymm11, %ymm15, %ymm11 #
        #
        # END of third backward slide.
        #
        # accumulator |= g
        #
	vpor	        %ymm10, %ymm11, %ymm10 # t[1:4] = t[1:4] | t[5:8] # reduce the eight values in ymm10 and ymm11 into four held by ymm14
	vmovdqa	        %xmm10, %xmm7          # u[1:2] = t[1:2]
	vextracti128    $0x1, %ymm10, %xmm10   # v[1:2] = t[3:4]
	vpor            %xmm10, %xmm7, %xmm10  # v[1:2] = v[1:2] | u[1:2]
	vpsrldq         $8, %xmm10, %xmm7      # u[2] = 0, u[1] = v[2]    # Shift xmm14 right by 8 bytes while shifting in 0s.
	vpor            %xmm7, %xmm10, %xmm10  # v[1] = v[1] | u[1]       # reduce the last two values into the first half of xmm14
	vmovq           %xmm10, %rax           # copy the first half of xmm10 into rax
        #
        orq             %rdi, %rax             # add the generator to the result
        #
	vzeroupper
        ret
        #
        
        #
        # Registers
        #
        #       64    32    16     8 args ret preserved
        #
        # 00:  rax   eax    ax    al        x
        # 01:  rbx   ebx    bx    bl                  x
        # 02:  rcx   ecx    cx    cl    3 
        # 03:  rdx   edx    dx    dl    2
        # 04:  rsi   esi    si   sil    1
        # 05:  rdi   edi    di   dil    0
        # 06:  rbp   ebp    bp   bpl                  x
        # 07:  rsp   esp    sp   spl  
        # 08:   r8   r8d   r8w   r8b    4
        # 09:   r9   r9d   r9w   r9b    5
        # 10:  r10  r10d  r10w  r10b  
        # 11:  r11  r11d  r11w  r11b  
        # 12:  r12  r12d  r12w  r12b                  x
        # 13:  r13  r13d  r13w  r13b                  x
        # 14:  r14  r14d  r14w  r14b                  x
        # 15:  r15  r15d  r15w  r15b                  x
        #
        # For floating-point (float, double), xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7
        #
        # After the parameters are pushed, the call instruction is made,
        #   so when the called function gets control, the return address
        #   is at (%rsp), the first memory parameter is at 8(%rsp), etc.
        #
        # GDB
        #
        # In order to print the value of an AVX2 register with a given format:
        # (gdb) p/x $ymm0.v4_int64
