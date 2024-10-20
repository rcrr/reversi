/**
 * @file
 *
 * @brief Kogge Stone module unit test suite.
 * @details Collects tests and helper methods for the kogge stone module.
 *
 * @par ut_kogge_stone.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2022, 2024 Roberto Corradini. All rights reserved.
 *
 * @par License
 * <tt>
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 * \n
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * \n
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 * or visit the site <http://www.gnu.org/licenses/>.
 * </tt>
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "unit_test.h"
#include "kogge_stone.h"
#include "board.h"



/* Function prototypes. */
typedef uint64_t (*lms_f) (uint64_t mover, uint64_t opponent, uint64_t empties);
typedef uint64_t (*mm_f) (uint64_t move, uint64_t opponent, uint64_t mover);

/* Legal Move Set */
const lms_f c_impl_kogge_stone_lms = kogge_stone_lms;
const lms_f asm_impl_kogge_stone_lms = kost_lms;
const lms_f lms_f_a[] = {c_impl_kogge_stone_lms, asm_impl_kogge_stone_lms};
const size_t lenght_lms_f_array = sizeof lms_f_a / sizeof lms_f_a[0];

/* Make Move */
const mm_f c_impl_kogge_stone_mm = kogge_stone_mm;
const mm_f asm_impl_kogge_stone_mm = kost_mm;
const mm_f mm_f_a[] = {c_impl_kogge_stone_mm, asm_impl_kogge_stone_mm};
const size_t lenght_mm_f_array = sizeof mm_f_a / sizeof mm_f_a[0];

/* Performance iterations. */
const int perf_iterations = 1000000000;


/*
 * Auxiliary functions.
 */



/*
 * Test functions.
 */

static void
lms_t (ut_test_t *const t)
{

  struct unit {
    uint64_t board[2];
    uint64_t lms;
  };

  struct unit data[] =
    {
     { { 0x0000000000000000 , 0x0000000000000000 }, 0x0000000000000000 }, // 000
     { { 0x0000000000000001 , 0x0000000000000000 }, 0x0000000000000000 }, // 001
     { { 0x0000000000000002 , 0x0000000000000000 }, 0x0000000000000000 }, // 002
     { { 0x0000000000000004 , 0x0000000000000000 }, 0x0000000000000000 }, // 003
     { { 0x0000000000000008 , 0x0000000000000000 }, 0x0000000000000000 }, // 004
     { { 0x0000000000000000 , 0x0000000000000001 }, 0x0000000000000000 }, // 005
     { { 0x0000000000000000 , 0x0000000000000002 }, 0x0000000000000000 }, // 006
     { { 0x0000000000000000 , 0x0000000000000004 }, 0x0000000000000000 }, // 007
     { { 0x0000000000000008 , 0x0000000000000000 }, 0x0000000000000000 }, // 008
     { { 0x0000000000000000 , 0x00000000000000ff }, 0x0000000000000000 }, // 009
     { { 0x0000000000000001 , 0x0000000000000002 }, 0x0000000000000004 }, // 010
     { { 0x0000000000000001 , 0x0000000000000006 }, 0x0000000000000008 }, // 011
     { { 0x0000000000000001 , 0x000000000000000e }, 0x0000000000000010 }, // 012
     { { 0x0000000000000001 , 0x000000000000001e }, 0x0000000000000020 }, // 013
     { { 0x0000000000000001 , 0x000000000000003e }, 0x0000000000000040 }, // 014
     { { 0x0000000000000001 , 0x000000000000007e }, 0x0000000000000080 }, // 015
     { { 0x0000000000000001 , 0x00000000000000fe }, 0x0000000000000000 }, // 016
     { { 0x0000000000000004 , 0x000000000000007a }, 0x0000000000000081 }, // 017
     { { 0x0000000000000081 , 0x000000000000007a }, 0x0000000000000004 }, // 018
     { { 0x0000000000000002 , 0x00000000000000fc }, 0x0000000000000000 }, // 019

     { { 0x0000000000000001 , 0x0000000000000100 }, 0x0000000000010000 }, // 020
     { { 0x0000000000000002 , 0x0000000000000200 }, 0x0000000000020000 }, // 021
     { { 0x0000000000000004 , 0x0000000000000400 }, 0x0000000000040000 }, // 022
     { { 0x0000000000000008 , 0x0000000000000800 }, 0x0000000000080000 }, // 023
     { { 0x0000000000000001 , 0x0000000000010100 }, 0x0000000001000000 }, // 024
     { { 0x0000000000000001 , 0x0001010101010100 }, 0x0100000000000000 }, // 025
     { { 0x0000000000000011 , 0x0011111111111100 }, 0x1100000000000000 }, // 026
     { { 0x0000000000000055 , 0x0055555555555500 }, 0x5500000000000000 }, // 027
     { { 0xaa00000000000000 , 0x00aaaaaaaaaaaa00 }, 0x00000000000000aa }, // 028
     { { 0x0055000000000000 , 0x0000555555550000 }, 0x0000000000005500 }, // 029

     { { 0x0000000000000001 , 0x0000000000000200 }, 0x0000000000040000 }, // 030
     { { 0x0000000000040000 , 0x0000000000000200 }, 0x0000000000000001 }, // 031
     { { 0x8000000000000000 , 0x0040000000000000 }, 0x0000200000000000 }, // 032
     { { 0x0000200000000000 , 0x0040000000000000 }, 0x8000000000000000 }, // 033
     { { 0x0000000000000001 , 0x0040201008040200 }, 0x8000000000000000 }, // 034
     { { 0x8000000000000000 , 0x0040201008040200 }, 0x0000000000000001 }, // 035
     { { 0x8000000000000001 , 0x0040200008040200 }, 0x0000001000000000 }, // 036
     { { 0x0000000000000020 , 0x0000000000004000 }, 0x0000000000800000 }, // 037
     { { 0x0000000000800000 , 0x0000000000004000 }, 0x0000000000000020 }, // 038
     { { 0x0000000000080000 , 0x0080002010000400 }, 0x0000400000000002 }, // 039

     { { 0x0000000000000080 , 0x0000000000004000 }, 0x0000000000200000 }, // 040
     { { 0x0000000000200000 , 0x0000000000004000 }, 0x0000000000000080 }, // 041
     { { 0x0000000000000080 , 0x0002040810204000 }, 0x0100000000000000 }, // 042
     { { 0x0100000000000000 , 0x0002040810204000 }, 0x0000000000000080 }, // 043
     { { 0x0000000010000000 , 0x0002040800204000 }, 0x0100000000000080 }, // 044
     { { 0x0000000000000004 , 0x0000000000000200 }, 0x0000000000010000 }, // 045
     { { 0x0000000000010000 , 0x0000000000000200 }, 0x0000000000000004 }, // 046
     { { 0x2000000000000000 , 0x0040000000000000 }, 0x0000800000000000 }, // 047
     { { 0x0000800000000000 , 0x0040000000000000 }, 0x2000000000000000 }, // 048
     { { 0x0000000001000408 , 0x0000000000000000 }, 0x0000000000000000 }, // 049

     { { 0x0000000810000000 , 0x0000001008000000 }, 0x0000102004080000 }, // 050 : woc18-FKvsAP-g1-60
     { { 0x0000000008000000 , 0x0000003810000000 }, 0x0000280020000000 }, // 051 : woc18-FKvsAP-g1-59
     { { 0x0000003010000000 , 0x0000080808000000 }, 0x0004040404040000 }, // 052 : woc18-FKvsAP-g1-58
     { { 0x0000080800000000 , 0x0000003018040000 }, 0x0000004020280000 }, // 053 : woc18-FKvsAP-g1-57
     { { 0x0000003010040000 , 0x0000080808080000 }, 0x0004040404100400 }, // 054 : woc18-FKvsAP-g1-56
     { { 0x0000080800080000 , 0x000000301c040000 }, 0x0000404220220000 }, // 055 : woc18-FKvsAP-g1-55
     { { 0x000000201c040000 , 0x0000081820080000 }, 0x0008340440301c00 }, // 056 : woc18-FKvsAP-g1-54
     { { 0x0000080820080000 , 0x000020301c040000 }, 0x0020404202220000 }, // 057 : woc18-FKvsAP-g1-53
     { { 0x000020300c040000 , 0x0000080830280000 }, 0x000c100440503c00 }, // 058 : woc18-FKvsAP-g1-52
     { { 0x0000080800280000 , 0x000020307c040000 }, 0x002040c200420000 }, // 059 : woc18-FKvsAP-g1-51
     { { 0x000020205c040000 , 0x0000081820680000 }, 0x0008140400107c00 }, // 060 : woc18-FKvsAP-g1-50
     { { 0x0000080020680000 , 0x000030385c040000 }, 0x002044c282020000 }, // 061 : woc18-FKvsAP-g1-49
     { { 0x000030385c000000 , 0x00000800206e0000 }, 0x000c04000000ff00 }, // 062 : woc18-FKvsAP-g1-48
     { { 0x00000800004e0000 , 0x000030387c202000 }, 0x0060404600100010 }, // 063 : woc18-FKvsAP-g1-47
     { { 0x000030387c002000 , 0x00000800007e0000 }, 0x000c04008000df00 }, // 064 : woc18-FKvsAP-g1-46
     { { 0x0000080000460000 , 0x000030387c383000 }, 0x0060404400000810 }, // 065 : woc18-FKvsAP-g1-45
     { { 0x0000303878383000 , 0x0000080404460000 }, 0x000c06028281c300 }, // 066 : woc18-FKvsAP-g1-44
     { { 0x0000000404460000 , 0x00003c3878383000 }, 0x0074004080000070 }, // 067 : woc18-FKvsAP-g1-43
     { { 0x0000343878383000 , 0x0010080404460000 }, 0x180c02028281c700 }, // 068 : woc18-FKvsAP-g1-42
     { { 0x0010000404460000 , 0x00043c3878383000 }, 0x0660004080000070 }, // 069 : woc18-FKvsAP-g1-41
     { { 0x00043c3800383000 , 0x00100004fc460000 }, 0x380002c202818700 }, // 070 : woc18-FKvsAP-g1-40
     { { 0x00100004dc460000 , 0x00043c7820383000 }, 0x066a408000004c78 }, // 071 : woc18-FKvsAP-g1-39
     { { 0x00043c0020383000 , 0x001000fcdc460000 }, 0x3800c20202818700 }, // 072 : woc18-FKvsAP-g1-38
     { { 0x001000dccc460000 , 0x00047c2030383000 }, 0x066a000000004878 }, // 073 : woc18-FKvsAP-g1-37
     { { 0x00047c2010203000 , 0x001000dcec5e0800 }, 0x380002020281c708 }, // 074 : woc18-FKvsAP-g1-36
     { { 0x001000d8e44e0800 , 0x00047e2418303000 }, 0x06ea000202004038 }, // 075 : woc18-FKvsAP-g1-35
     { { 0x0004660418303000 , 0x001818f8e44e0800 }, 0x382080020281c60c }, // 076 : woc18-FKvsAP-g1-34
     { { 0x001818f8e04e0800 , 0x000466041e303000 }, 0x06e2810301014038 }, // 077 : woc18-FKvsAP-g1-33
     { { 0x000466040e200000 , 0x001818f8f05e3810 }, 0x382000000081c768 }, // 078 : woc18-FKvsAP-g1-32
     { { 0x001018f8f05e3810 , 0x100c66040e200000 }, 0x0ee3810301014000 }, // 079 : woc18-FKvsAP-g1-31
     { { 0x100c660400200000 , 0x001018f8ff5e3810 }, 0x282000000081c668 }, // 080 : woc18-FKvsAP-g1-30
     { { 0x000018f8ff5e3810 , 0x181c660400200000 }, 0x26e3810200004000 }, // 081 : woc18-FKvsAP-g1-29
     { { 0x181c660000200000 , 0x000018feff5e3810 }, 0x000000000081c668 }, // 082 : woc18-FKvsAP-g1-28
     { { 0x000018daeb523810 , 0x181c6624142c0400 }, 0x2663810000004208 }, // 083 : woc18-FKvsAP-g1-27
     { { 0x180c6624142c0400 , 0x201018daeb523810 }, 0x402001010081c368 }, // 084 : woc18-FKvsAP-g1-26
     { { 0x001018daeb523810 , 0x780c6624142c0400 }, 0x0663810000004208 }, // 085 : woc18-FKvsAP-g1-25
     { { 0x780c0604142c0400 , 0x003078faeb523810 }, 0x00c081010081c368 }, // 086 : woc18-FKvsAP-g1-24
     { { 0x003078f8eb523810 , 0x780c0706142c0400 }, 0x060300010000420a }, // 087 : woc18-FKvsAP-g1-23
     { { 0x780c0700142c0400 , 0x003078ffeb523810 }, 0x00c080000081c368 }, // 088 : woc18-FKvsAP-g1-22
     { { 0x002070fae8503810 , 0x781c0f05172f0400 }, 0x0603000000004302 }, // 089 : woc18-FKvsAP-g1-21
     { { 0x78180705172f0400 , 0x022478fae8503810 }, 0x04c2800000804068 }, // 090 : woc18-FKvsAP-g1-20
     { { 0x020458eae0503810 , 0x787827151f2f0400 }, 0x0483000000004306 }, // 091 : woc18-FKvsAP-g1-19
     { { 0x78782511172f0400 , 0x02055aeee8503810 }, 0x058280000080c028 }, // 092 : woc18-FKvsAP-g1-18
     { { 0x02054acea8103810 , 0x7878353157ef0400 }, 0x808200000000c306 }, // 093 : woc18-FKvsAP-g1-17
     { { 0x7878351147e70000 , 0x02054aeeb8183c12 }, 0x050280000000002c }, // 094 : woc18-FKvsAP-g1-16
     { { 0x020542e6b0102012 , 0x78783d194fef1c08 }, 0x048200000000c324 }, // 095 : woc18-FKvsAP-g1-15
     { { 0x78783c184eee1c08 , 0x020543e7b1112112 }, 0x0482800000004060 }, // 096 : woc18-FKvsAP-g1-14
     { { 0x020543e7b1110112 , 0x78783c184eee7c08 }, 0x04820000000082e4 }, // 097 : woc18-FKvsAP-g1-13
     { { 0x78783c184cec7c08 , 0x020543e7b3130312 }, 0x0482800000000021 }, // 098 : woc18-FKvsAP-g1-12
     { { 0x020543e7b3130112 , 0x78783c184cec7e09 }, 0x04820000000080e4 }, // 099 : woc18-FKvsAP-g1-11
     { { 0x78783c1848e87001 , 0x020543e7b7170f1e }, 0x0582800000000020 }, // 100 : woc18-FKvsAP-g1-10
     { { 0x020503c7a7170f1e , 0x78f87c3858e87001 }, 0x8000800000008060 }, // 101 : woc18-FKvsAP-g1-09
     { { 0x78f87c3858c04001 , 0x020503c7a73f3f3e }, 0x0502800000000040 }, // 102 : woc18-FKvsAP-g1-08
     { { 0x020503c7a72f1f00 , 0x78f87c3858d0607f }, 0x8002800000008080 }, // 103 : woc18-FKvsAP-g1-07
     { { 0x78b85c2850d0607f , 0x824523d7af2f1f00 }, 0x0502800000000000 }, // 104 : woc18-FKvsAP-g1-06
     { { 0x824422d6ae2e1e00 , 0x79b95d2951d1617f }, 0x0402800000008080 }, // 105 : woc18-FKvsAP-g1-05
     { { 0x79391d2951d1617f , 0x82c4e2d6ae2e1e00 }, 0x0402000000000000 }, // 106 : woc18-FKvsAP-g1-04
     { { 0x82c0e0d4ac2c1c00 , 0x793f1f2b53d3637f }, 0x0400000000008080 }, // 107 : woc18-FKvsAP-g1-03
     { { 0xfec4e4d4ac2c1c00 , 0x013b1b2b53d3637f }, 0x0000000000008080 }, // 108 : woc18-FKvsAP-g1-02
     { { 0x013b1b2b53d3237f , 0xfec4e4d4ac2c5c80 }, 0x0000000000008000 }, // 109 : woc18-FKvsAP-g1-01
     { { 0xfec4e4d4ac2c1c80 , 0x013b1b2b53d3e37f }, 0x0000000000000000 }, // 110 : woc18-FKvsAP-g1-00

     { { 0x3e0059e9150b050e , 0x007c26162a341800 }, 0x4002800040406030 }, // 111 : ffo-30
     { { 0x0001073f0e060000 , 0x3e3cb84030393c3e }, 0x4040408040404040 }, // 112 : ffo-31
     { { 0xa4bc9c8c0a042c0c , 0x00406373751b1100 }, 0x0003000080e00210 }, // 113 : ffo-32
     { { 0x00a0cac0d8c804fe , 0x3c08303e26343800 }, 0x0050050101034200 }, // 114 : ffo-33
     { { 0x785c2e4620000000 , 0x84a0d0b9dffea000 }, 0x0200000000015f60 }, // 115 : ffo-34
     { { 0x00108e86bff38000 , 0x04687078400c3c1c }, 0xf884000000000262 }, // 116 : ffo-35
     { { 0x00783c3a66002020 , 0x000183c599ff9c08 }, 0x0002400000004316 }, // 117 : ffo-36
     { { 0x0c1c3c6c160e0000 , 0x2001031329713d3c }, 0x000200004080c042 }, // 118 : ffo-37
     { { 0x0014f9c0849a0000 , 0x0000063e7b643c3c }, 0x000300010001c242 }, // 119 : ffo-38
     { { 0x000002160e0e123d , 0x000d1d2931712d00 }, 0x1f306040c080c042 }, // 120 : ffo-39
    };

  const size_t test_data_lenght = sizeof data / sizeof data[0];

  for (int j = 0; j < lenght_lms_f_array; j++) {
    const lms_f f = lms_f_a[j];
    for (int i = 0; i < test_data_lenght; i++) {
      struct unit *u = &data[i];
      const uint64_t mover = u->board[0];
      const uint64_t opponent = u->board[1];
      const uint64_t empties = ~(mover | opponent);
      const uint64_t expected = u->lms;
      const uint64_t lms = f(mover, opponent, empties);

      if (expected != lms) {
        printf("\n");
        printf("j = %d [Index j identifies the function tested.]\n", j);
        printf("[%d]: board = { 0x%016lx , 0x%016lx }, expected = 0x%016lx, computed = 0x%016lx\n",
               i, data[i].board[0], data[i].board[1], data[i].lms, lms );
        ut_assert(t, false);
      }

    }
  }
}

static void
mm_t (ut_test_t *const t)
{
  struct unit {
    uint64_t board[2];
    uint64_t move;
    uint64_t flips;
  };

  struct unit data[] =
    {
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x0000000000000000 , 0x0000000000000000 }, // 000
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x0000000000000001 , 0x0000000000000000 }, // 001
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x0000000000000002 , 0x0000000000000000 }, // 002
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x0000000000000040 , 0x0000000000000000 }, // 003
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x0000000000000080 , 0x0000000000000000 }, // 004
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x0000000000000200 , 0x0000000000020000 }, // 005
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x0000000000008000 , 0x0000001020400000 }, // 006
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x0000010000000000 , 0x0000060204000000 }, // 007
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x0001000000000000 , 0x0000020400000000 }, // 008
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x0002000000000000 , 0x0000060200000000 }, // 009
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x0040000000000000 , 0x0038404040400000 }, // 010
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x0100000000000000 , 0x0000000000000000 }, // 011
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x0200000000000000 , 0x0000000000000000 }, // 012
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x0400000000000000 , 0x0000000000000000 }, // 013
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x0800000000000000 , 0x0018000000000000 }, // 014
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x2000000000000000 , 0x0030000000000000 }, // 015
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x4000000000000000 , 0x0020100000000000 }, // 016
     { { 0x000428080A2C7C3C , 0x10B8D6F7F5D30100 }, 0x8000000000000000 , 0x0000000000000000 }, // 017

     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0000000000000001 , 0x0000000000000000 }, // 018
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0000000000000002 , 0x0000000000080400 }, // 019
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0000000000000040 , 0x0000000000002020 }, // 020
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0000000000000080 , 0x0000000000000000 }, // 021
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0000000000000100 , 0x0000000000000000 }, // 022
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0000000000000200 , 0x0000000000043C00 }, // 023
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0000000000008000 , 0x0000000000000000 }, // 024
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0000000000010000 , 0x0000000002000000 }, // 025
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0000000000020000 , 0x00000002020C0400 }, // 026
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0000000000800000 , 0x0000000000000000 }, // 027
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0000000001000000 , 0x0000040200000000 }, // 028
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0000000004000000 , 0x0000000000040400 }, // 029
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0000000100000000 , 0x0000010202040800 }, // 030
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0000008000000000 , 0x0000004000000000 }, // 031
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0000400000000000 , 0x0000004000000000 }, // 032
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0010000000000000 , 0x0000080000000000 }, // 033
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0040000000000000 , 0x0000000000000000 }, // 034
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0080000000000000 , 0x0000000000000000 }, // 035
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x0100000000000000 , 0x0602040000000000 }, // 036
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x1000000000000000 , 0x0000000000000000 }, // 037
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x2000000000000000 , 0x0000000000000000 }, // 038
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x4000000000000000 , 0x0000000000000000 }, // 039
     { { 0x0829322CF850401C , 0x06068D52022C3C20 }, 0x8000000000000000 , 0x0000000000000000 }, // 040
     { { 0xFF0315CBE5713900 , 0x00FC6A341A0E067F }, 0x0000800000000000 , 0x00C0600000000000 }, // 041
    };

  const size_t test_data_lenght = sizeof data / sizeof data[0];

  for (int j = 0; j < lenght_mm_f_array; j++) {
    const mm_f f = mm_f_a[j];
    for (int i = 0; i < test_data_lenght; i++) {
      struct unit *u = &data[i];
      const uint64_t mover = u->board[0];
      const uint64_t opponent = u->board[1];
      const uint64_t move = u->move;
      const uint64_t expected_flips = u->flips;
      const uint64_t flips = f(move, opponent, mover);

      ut_assert(t, (mover & opponent) == 0ULL);
      ut_assert(t, ((mover | opponent) & move) == 0ULL);

      if (expected_flips != flips) {
        printf("\n");
        printf("j = %d [Index j identifies the function tested.]\n", j);
        printf("[unit test number %d]:\n", i);
        printf("  board           = { 0x%016lx , 0x%016lx }\n", mover, opponent);
        printf("  move            =   0x%016lx\n", move);
        printf("  expected_flips  =   0x%016lx\n", expected_flips);
        printf("  computed        =   0x%016lx\n", flips);
        ut_assert(t, false);
      }
    }
  }
}

static void
aux_lms_perf (ut_test_t *const t, lms_f f)
{
  /* game position: ffo-33 */
  const uint64_t mover        = 0x00a0cac0d8c804fe;
  const uint64_t opponent     = 0x3c08303e26343800;
  const uint64_t empties      = ~(mover | opponent);
  const uint64_t expected_lms = 0x0050050101034200;
  for (int i = 0; i < perf_iterations; i++) {
    const uint64_t lms = f(mover, opponent, empties);
    if (lms != expected_lms) ut_assert(t, false);
  }
}

static void
kogge_stone_lms_perf_t (ut_test_t *const t)
{
  aux_lms_perf(t, c_impl_kogge_stone_lms);
}

static void
kost_lms_perf_t (ut_test_t *const t)
{
  aux_lms_perf(t, asm_impl_kogge_stone_lms);
}

static void
aux_mm_perf (ut_test_t *const t, mm_f f)
{
  const uint64_t mover          = 0x0829322CF850401C;
  const uint64_t opponent       = 0x06068D52022C3C20;
  const uint64_t move           = 0x0000000100000000;
  const uint64_t expected_flips = 0x0000010202040800;
  for (int i = 0; i < perf_iterations; i++) {
    const uint64_t flips = f(move, opponent, mover);
    if (flips != expected_flips) ut_assert(t, false);
  }
}

static void
kogge_stone_mm_perf_t (ut_test_t *const t)
{
  aux_mm_perf(t, c_impl_kogge_stone_mm);
}

static void
kost_mm_perf_t (ut_test_t *const t)
{
  aux_mm_perf(t, asm_impl_kogge_stone_mm);
}


/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_prog_arg_config_t config;
  ut_init(&config, &argc, &argv);

  ut_suite_t *const s = ut_suite_new(&config, "kogge_stone");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lms", lms_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "mm", mm_t);

  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_10,   "kogge_stone_lms_perf", kogge_stone_lms_perf_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_10,   "kost_lms_perf", kost_lms_perf_t);

  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_10,   "kogge_stone_mm_perf", kogge_stone_mm_perf_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_10,   "kost_mm_perf", kost_mm_perf_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
