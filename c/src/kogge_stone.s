
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

        .globl  kost_lms
        .globl  kost_mm
        
        #
        # kost_lms
        #
        # Returns the legal move set of the board.
        # The function has signature:
        #
        #   int64_t kost_lms (uint64_t mover, uint64_t opponent, uint64_t empties)
        #
        #
        # This variant of the Kogge-Stone algorithm takes three bitboard as imput:
        #
        #  rdi : generator  : mover
        #  rsi : propagator : opponent
        #  rdx : blocker    : empties
        #
        # and returns the bitboard obtained by the blocker subset that is formed by
        # the blockers hit by the rays, in the eight directions, obtained from the generators
        # traveling along the propagators.
        #
        # Passing as generator the mover square set, as propagator the opponent square set,
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
        vmovdqa         .mask0(%rip), %ymm5     # ymm5 = mask0
        vmovdqa         .mask1(%rip), %ymm6     # ymm6 = mask1
        vmovdqa         .slide(%rip), %ymm1     # ymm1 = slide
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
        vpsllvq         %ymm1, %ymm10, %ymm14  # ymm14 = ymm10 << ymm1
        vpsrlvq         %ymm1, %ymm11, %ymm15  # ymm15 = ymm11 >> ymm1
        #
        # t &= p
        #
        vpand           %ymm14, %ymm12, %ymm14 # ymm14 &= ymm12
        vpand           %ymm15, %ymm13, %ymm15 # ymm15 &= ymm13
        #
        # g |= t
        #
        vpor            %ymm10, %ymm14, %ymm10 # ymm10 |= ymm14
        vpor            %ymm11, %ymm15, %ymm11 # ymm11 |= ymm15
        #
        # p &= ((p << slide_up_1[i]) >> slide_dw_1[i])
        #
        # t = ((p << slide_up_1[i]) >> slide_dw_1[i])
        #
        vpsllvq         %ymm1, %ymm12, %ymm14  # ymm14 = ymm12 << ymm1
        vpsrlvq         %ymm1, %ymm13, %ymm15  # ymm15 = ymm13 >> ymm1
        #
        # p &= t
        #
        vpand           %ymm14, %ymm12, %ymm12 # ymm12 &= ymm14
        vpand           %ymm15, %ymm13, %ymm13 # ymm13 &= ymm15
        #
        # END of first slide.
        #
        vpsllq          $1, %ymm1, %ymm7       # ymm7 = ymm1 << 1 - increment slide value: { 1, 7, 8, 9 } --> { 2, 14, 16, 18 }
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
        vpand           %ymm14, %ymm12, %ymm14 # ymm14 &= ymm12
        vpand           %ymm15, %ymm13, %ymm15 # ymm15 &= ymm13
        #
        # g |= t
        #
        vpor            %ymm10, %ymm14, %ymm10 # ymm10 |= ymm14
        vpor            %ymm11, %ymm15, %ymm11 # ymm11 |= ymm15
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
        vpand           %ymm14, %ymm12, %ymm12 # ymm12 &= ymm14
        vpand           %ymm15, %ymm13, %ymm13 # ymm13 &= ymm15
        #
        # END of second slide.
        #
        vpsllq          $1, %ymm7, %ymm7       # ymm7 = ymm7 << 1 - increment slide value: { 2,  14, 16, 18 } --> { 4, 28, 32, 36 }
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
        vpand           %ymm14, %ymm12, %ymm14 # ymm14 &= ymm12
        vpand           %ymm15, %ymm13, %ymm15 # ymm15 &= ymm13
        #
        # g |= t
        #
        vpor            %ymm10, %ymm14, %ymm10 # ymm10 |= ymm14
        vpor            %ymm11, %ymm15, %ymm11 # ymm11 |= ymm15
        #
        # END of third slide.
        #
        #
        # START of fourth slide.
        #
        # g = ~generator & g
        #
        vpandn          %ymm10, %ymm0, %ymm10 # ymm10 &= ~ymm0
        vpandn          %ymm11, %ymm0, %ymm11 # ymm11 &= ~ymm0
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
        vpand           %ymm14, %ymm5, %ymm14  # ymm14 &= ymm5
        vpand           %ymm15, %ymm6, %ymm15  # ymm15 &= ymm6
        #
        # t &= blocker
        #
        vpand           %ymm14, %ymm4, %ymm14  # ymm14 &= ymm4
        vpand           %ymm15, %ymm4, %ymm15  # ymm15 &= ymm4
        #
        # END of fourth slide.
        #
        #
        # result |= g
        #
        # Accomplish the horizontal accumulation of the results computed in the eight directions.
        #
        # Registers ymm14/ymm15 collects the 8 sets of legal moves upon which we need to compute the union
        # operation (bitwise OR).
        #
        # ymm14 : g[0:3] --- ymm15 : g[4:7]
        #
        # result = 0
        # for (int i = 0, i < 8, i++)
        #   result |= g[i]
        #
        # Step 1: reduce the eight values in ymm14 and ymm15 into four held by ymm14
        # Step 2: copy lower 128 bits of ymm14 into ymm7 (tmp)
        # Step 3: move upper 128 bits of ymm14 into lower 128 bits of ymm14
        # Step 4: reduce the values to two held in the lower 128 bits of ymm14
        # Step 5: shift xmm14 right by 8 bytes while shifting in 0s and save it to ymm7
        # Step 6: reduce the last two values into the first half of xmm14
        # Step 7: copy the first half of xmm14 into rax
        #
	vpor            %ymm14, %ymm15, %ymm14 # s1: g[0:3] = g[0:3] | g[4:7]
	vmovdqa	        %xmm14, %xmm7          # s2: t[0:1] = g[0:1]
	vextracti128    $0x1,   %ymm14, %xmm14 # s3: g[0:1] = g[2:3]
	vpor            %xmm14, %xmm7,  %xmm14 # s4: g[0:1] = g[0:1] | t[0:1]
	vpsrldq         $8,     %xmm14, %xmm7  # s5: t[0] = g[1], t[1] = 0 
	vpor            %xmm7,  %xmm14, %xmm14 # s6: g[0] = g[0] | t[0]
	vmovq           %xmm14, %rax           # s7: rax = g[0]
        #
        # return result
        #
	vzeroupper
        ret
        #

        #
        # kost_m_m
        #
        # The function has signature:
        #
        #   int64_t kost_mm (int64_t generator, int64_t propagator, int64_t blocker)
        #
        # This variant of the Kogge-Stone algorithm takes three bitboard as imput:
        #
        #  - rdi : generator  : move     : a square set having one or zero elements
        #  - rsi : propagator : opponent : the opponent square set (board[1])
        #  - rdx : blocker    : mover    : the mover (player) square set (board[0])
        #
        # and returns in the rax register the bitboard that contains the flipped squares (flips).
        #
        # The flipped square set carries two information:
        #
        #  - enables the computation of the new game position obtained by moving:
        #
        #        mover_new_square_set    = mover | flips
        #        opponent_new_square_set = opponent & ~flips
        #
        #  - verifies that the move is legal. When it is not the returned value is zero.
        #
        # Prerequisites:
        #
        #  - 'opponent & mover' must be equal to zero.
        #
        #  - 'popcnt move' must be in the range [0..1].
        #
kost_mm:
        vmovq           %rdi, %xmm15            # MOVD/MOVQ VEX.128.66.0F.W1 6E /r VMOVQ xmm1, r64/m64
        vpbroadcastq	%xmm15, %ymm0           # ymm0  - generator  - player
        vmovq           %rsi, %xmm15            #
        vpbroadcastq	%xmm15, %ymm2           # ymm2  - propagator - opponent
        vmovq           %rdx, %xmm15
        vpbroadcastq	%xmm15, %ymm4           # ymm4  - blocker    - empty
        #
        vmovdqa         .mask0(%rip), %ymm5     # ymm5 = mask0
        vmovdqa         .mask1(%rip), %ymm6     # ymm6 = mask1
        vmovdqa         .slide(%rip), %ymm1     # ymm1 = slide
        #
        # g: ymm10/ymm11
        # p: ymm12/ymm13
        # t: ymm14/ymm15 - tmp registers
        #
        # propagator_masked: ymm8/ymm9
        #
        vmovdqa         %ymm0,  %ymm10          # g = generator
        vmovdqa         %ymm0,  %ymm11          #
        vpand           %ymm2,  %ymm5,  %ymm8   # p = propagator & mask[i]
        vpand           %ymm2,  %ymm6,  %ymm9   #
        #
        # START of first forward slide.
        #
        # g |= p & ((g << slide_up_1[i]) >> slide_dw_1[i])
        #
        #   t = (g << slide_up_1[i]) >> slide_dw_1[i])
        #   t &= p
        #   g |= t
        #
        vpsllvq         %ymm1,  %ymm10, %ymm14  # ymm14 = ymm10 << ymm1
        vpsrlvq         %ymm1,  %ymm11, %ymm15  # ymm15 = ymm11 >> ymm1
        vpand           %ymm14, %ymm8,  %ymm14  # ymm14 &= ymm8
        vpand           %ymm15, %ymm9,  %ymm15  # ymm15 &= ymm9
        vpor            %ymm10, %ymm14, %ymm10  # ymm10 |= ymm14
        vpor            %ymm11, %ymm15, %ymm11  # ymm11 |= ymm15
        #
        # p &= ((p << slide_up_1[i]) >> slide_dw_1[i])
        #
        #   t = ((p << slide_up_1[i]) >> slide_dw_1[i])
        #   p &= t
        #
        vpsllvq         %ymm1,  %ymm8,  %ymm14  # ymm14 = ymm8 << ymm1
        vpsrlvq         %ymm1,  %ymm9,  %ymm15  # ymm15 = ymm9 >> ymm1
        vpand           %ymm14, %ymm8,  %ymm12  # ymm12 = ymm14 & ymm8
        vpand           %ymm15, %ymm9,  %ymm13  # ymm13 = ymm15 & ymm9
        #
        # END of first forward slide.
        #
        vpsllq          $1, %ymm1, %ymm7        # ymm7 = ymm1 << 1 - increment slide value: { 1, 7, 8, 9 } --> { 2, 14, 16, 18 }
        #
        # START of second foward slide.
        #
        # g |= p & ((g << slide_up_2[i]) >> slide_dw_2[i])
        #
        #   t = ((g << slide_up_2[i]) >> slide_dw_2[i])
        #   t &= p
        #   g |= t
        #
        vpsllvq         %ymm7,  %ymm10, %ymm14  # ymm14 = ymm10 << ymm7
        vpsrlvq         %ymm7,  %ymm11, %ymm15  # ymm15 = ymm11 >> ymm7
        vpand           %ymm14, %ymm12, %ymm14  # ymm14 &= ymm12
        vpand           %ymm15, %ymm13, %ymm15  # ymm15 &= ymm13
        vpor            %ymm10, %ymm14, %ymm10  # ymm10 |= ymm14
        vpor            %ymm11, %ymm15, %ymm11  # ymm11 |= ymm15
        #
        # p &= ((p << slide_up_2[i]) >> slide_dw_2[i])
        #
        #   t = ((p << slide_up_2[i]) >> slide_dw_2[i])
        #   p &= t
        #
        vpsllvq         %ymm7,  %ymm12, %ymm14  # ymm14 = ymm10 << ymm7
        vpsrlvq         %ymm7,  %ymm13, %ymm15  # ymm15 = ymm11 >> ymm7
        vpand           %ymm14, %ymm12, %ymm12  # ymm12 &= ymm14
        vpand           %ymm15, %ymm13, %ymm13  # ymm13 &= ymm15
        #
        # END of second forward slide.
        #
        vpsllq          $1, %ymm7, %ymm7        # ymm7 = ymm7 << 1 - increment slide value: { 2,  14, 16, 18 } --> { 4, 28, 32, 36 }
        #
        # START of third forward slide.
        #
        # g |= p & ((g << slide_up_4[i]) >> slide_dw_4[i])
        #
        #   t = ((g << slide_up_4[i]) >> slide_dw_4[i])
        #   t &= p
        #   g |= t
        #
        vpsllvq         %ymm7,  %ymm10, %ymm14  # ymm14 = ymm10 << ymm7
        vpsrlvq         %ymm7,  %ymm11, %ymm15  # ymm15 = ymm11 >> ymm7
        vpand           %ymm14, %ymm12, %ymm14  # ymm14 &= ymm12
        vpand           %ymm15, %ymm13, %ymm15  # ymm15 &= ymm13
        vpor            %ymm10, %ymm14, %ymm10  # ymm10 |= ymm14
        vpor            %ymm11, %ymm15, %ymm11  # ymm11 |= ymm15
        #
        # END of third forward slide.
        #
        # ymm10/11 collects flips + move when the direction is a true flip, garbage otherwise.
        #
        #
        # START of fourth forward slide.
        #
        # g = blocker & mask[i] & ((g << slide_up_1[i]) >> slide_dw_1[i])
        #
        #   t = ((g << slide_up_1[i]) >> slide_dw_1[i])
        #   t &= mask[i]
        #   t &= blocker
        #
        vpsllvq         %ymm1,  %ymm10, %ymm14  # ymm14 = ymm10 << ymm1
        vpsrlvq         %ymm1,  %ymm11, %ymm15  # ymm15 = ymm11 >> ymm1
        vpand           %ymm14, %ymm5,  %ymm14  # ymm14 &= ymm5
        vpand           %ymm15, %ymm6,  %ymm15  # ymm15 &= ymm6
        vpand           %ymm14, %ymm4,  %ymm6   # ymm6 = ymm14 & ymm4
        vpand           %ymm15, %ymm4,  %ymm7   # ymm7 = ymm15 & ymm4
        #
        # END of fourth forward slide.
        #
        # ymm6/7 collects the flip blockers for the eight directions.
        # When the direction flips at most one square is identified, and the qword is not equal zero.
        # When it doesn't the set is empty.
        #
        # This code block filter (mask) the eight values in ymm10/11 keeping the values when
        # the corresponding qword in ymm8/9 is not empty, when otherwise empty the value is zeroed.
        #
        vpxor           %ymm3,  %ymm3,  %ymm3   # ymm3 = { 0x0, 0x0, 0x0, 0x0 }
        vpcmpeqq        %ymm3,  %ymm6,  %ymm8   # ymm8 = (ymm3 == ymm6) ? 0xFFFFFFFFFFFFFFFF : 0x0000000000000000
        vpcmpeqq        %ymm3,  %ymm7,  %ymm9   # ymm9 = (ymm3 == ymm7) ? 0xFFFFFFFFFFFFFFFF : 0x0000000000000000
        vpandn          %ymm10, %ymm8,  %ymm10  # ymm10 &= ~ymm8
        vpandn          %ymm11, %ymm9,  %ymm11  # ymm11 &= ~ymm9
        #
        # accumulator |= g
        #
        # Accomplish the horizontal accumulation of the results computed in the eight directions.
        #
        # Registers ymm10/ymm11 collects the 8 sets of flips upon which we need to compute the union
        # operation (bitwise OR).
        #
        # ymm10 : g[0:3] --- ymm11 : g[4:7]
        #
        # accumulator = 0
        # for (int i = 0, i < 8, i++)
        #   accumulator |= g[i]
        #
        # accumulator &= ~move
        # return accumulator
        #
        # Step 1: reduce the eight values in ymm10 and ymm11 into four held by ymm10
        # Step 2: copy lower 128 bits of ymm10 into ymm7 (tmp)
        # Step 3: move upper 128 bits of ymm10 into lower 128 bits of ymm10
        # Step 4: reduce the values to two held in the lower 128 bits of ymm10
        # Step 5: shift xmm10 right by 8 bytes while shifting in 0s and save it to ymm7
        # Step 6: reduce the last two values into the first half of xmm10
        # Step 7: copy the first half of xmm10 into rax
        #
	vpor	        %ymm10, %ymm11, %ymm10  # s1: g[0:3] = g[0:3] | g[4:7]
        vpandn          %ymm10, %ymm0,  %ymm10  # removes the move (ymm0) from the computed union
	vmovdqa	        %xmm10, %xmm7           # s2: t[0:1] = g[0:1]
	vextracti128    $0x1,   %ymm10, %xmm10  # s3: g[0:1] = g[2:3]
	vpor            %xmm10, %xmm7,  %xmm10  # s4: g[0:1] = g[0:1] | t[0:1]
	vpsrldq         $8,     %xmm10, %xmm7   # s5: t[0] = g[1], t[1] = 0
	vpor            %xmm7,  %xmm10, %xmm10  # s6: g[0] = g[0] | t[0]
	vmovq           %xmm10, %rax            # s7: rax = g[0]
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
        # (gdb) p/z $ymm0.v4_int64
