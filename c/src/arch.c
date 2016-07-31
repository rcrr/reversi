/**
 * @file
 *
 * @brief Architecture module implementation.
 *
 * @par arch.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2016 Roberto Corradini. All rights reserved.
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

#include <stdio.h>

#include "arch.h"


/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static void
cpuid (int info[4],
       int info_type);



/*
 * Internal variables.
 */

/* Misc. */
static bool ARCH_HW_MMX         = false;
static bool ARCH_HW_x64         = false;
static bool ARCH_HW_ABM         = false;    // Advanced Bit Manipulation
static bool ARCH_HW_RDRAND      = false;
static bool ARCH_HW_BMI1        = false;
static bool ARCH_HW_BMI2        = false;
static bool ARCH_HW_ADX         = false;
static bool ARCH_HW_PREFETCHWT1 = false;

/* SIMD: 128-bit */
static bool ARCH_HW_SSE         = false;
static bool ARCH_HW_SSE2        = false;
static bool ARCH_HW_SSE3        = false;
static bool ARCH_HW_SSSE3       = false;
static bool ARCH_HW_SSE41       = false;
static bool ARCH_HW_SSE42       = false;
static bool ARCH_HW_SSE4a       = false;
static bool ARCH_HW_AES         = false;
static bool ARCH_HW_SHA         = false;

/* SIMD: 256-bit */
static bool ARCH_HW_AVX         = false;
static bool ARCH_HW_XOP         = false;
static bool ARCH_HW_FMA3        = false;
static bool ARCH_HW_FMA4        = false;
static bool ARCH_HW_AVX2        = false;

/* SIMD: 512-bit */
static bool ARCH_HW_AVX512F     = false;   // AVX512 Foundation
static bool ARCH_HW_AVX512CD    = false;   // AVX512 Conflict Detection
static bool ARCH_HW_AVX512PF    = false;   // AVX512 Prefetch
static bool ARCH_HW_AVX512ER    = false;   // AVX512 Exponential + Reciprocal
static bool ARCH_HW_AVX512VL    = false;   // AVX512 Vector Length Extensions
static bool ARCH_HW_AVX512BW    = false;   // AVX512 Byte + Word
static bool ARCH_HW_AVX512DQ    = false;   // AVX512 Doubleword + Quadword
static bool ARCH_HW_AVX512IFMA  = false;   // AVX512 Integer 52-bit Fused Multiply-Add
static bool ARCH_HW_AVX512VBMI  = false;   // AVX512 Vector Byte Manipulation Instructions

/**
 * @endcond
 */



/**
 * @brief Returns true if the hardware and the OS support x86_64, POPCNT, AVX, AVX2 features.
 *
 * @return true if the platform is ok
 */
bool
arch_runtime_is_supported (void)
{

  int info[4];
  cpuid(info, 0);
  int n_ids = info[0];

  cpuid(info, 0x80000000);
  unsigned n_ex_ids = info[0];

  //  Detect Features
  if (n_ids >= 0x00000001) {
    cpuid(info, 0x00000001);
    ARCH_HW_MMX    = (info[3] & ((int)1 << 23)) != 0;
    ARCH_HW_SSE    = (info[3] & ((int)1 << 25)) != 0;
    ARCH_HW_SSE2   = (info[3] & ((int)1 << 26)) != 0;
    ARCH_HW_SSE3   = (info[2] & ((int)1 <<  0)) != 0;

    ARCH_HW_SSSE3  = (info[2] & ((int)1 <<  9)) != 0;
    ARCH_HW_SSE41  = (info[2] & ((int)1 << 19)) != 0;
    ARCH_HW_SSE42  = (info[2] & ((int)1 << 20)) != 0;
    ARCH_HW_AES    = (info[2] & ((int)1 << 25)) != 0;

    ARCH_HW_AVX    = (info[2] & ((int)1 << 28)) != 0;
    ARCH_HW_FMA3   = (info[2] & ((int)1 << 12)) != 0;

    ARCH_HW_RDRAND = (info[2] & ((int)1 << 30)) != 0;
  }

  if (n_ids >= 0x00000007) {
    cpuid(info, 0x00000007);
    ARCH_HW_AVX2   = (info[1] & ((int)1 <<  5)) != 0;

    ARCH_HW_BMI1        = (info[1] & ((int)1 <<  3)) != 0;
    ARCH_HW_BMI2        = (info[1] & ((int)1 <<  8)) != 0;
    ARCH_HW_ADX         = (info[1] & ((int)1 << 19)) != 0;
    ARCH_HW_SHA         = (info[1] & ((int)1 << 29)) != 0;
    ARCH_HW_PREFETCHWT1 = (info[2] & ((int)1 <<  0)) != 0;

    ARCH_HW_AVX512F     = (info[1] & ((int)1 << 16)) != 0;
    ARCH_HW_AVX512CD    = (info[1] & ((int)1 << 28)) != 0;
    ARCH_HW_AVX512PF    = (info[1] & ((int)1 << 26)) != 0;
    ARCH_HW_AVX512ER    = (info[1] & ((int)1 << 27)) != 0;
    ARCH_HW_AVX512VL    = (info[1] & ((int)1 << 31)) != 0;
    ARCH_HW_AVX512BW    = (info[1] & ((int)1 << 30)) != 0;
    ARCH_HW_AVX512DQ    = (info[1] & ((int)1 << 17)) != 0;
    ARCH_HW_AVX512IFMA  = (info[1] & ((int)1 << 21)) != 0;
    ARCH_HW_AVX512VBMI  = (info[2] & ((int)1 <<  1)) != 0;
  }
  if (n_ex_ids >= 0x80000001) {
    cpuid(info, 0x80000001);
    ARCH_HW_x64   = (info[3] & ((int)1 << 29)) != 0;
    ARCH_HW_ABM   = (info[2] & ((int)1 <<  5)) != 0;
    ARCH_HW_SSE4a = (info[2] & ((int)1 <<  6)) != 0;
    ARCH_HW_FMA4  = (info[2] & ((int)1 << 16)) != 0;
    ARCH_HW_XOP   = (info[2] & ((int)1 << 11)) != 0;
  }

  const int gnu_builtin_popcnt = __builtin_cpu_supports("popcnt");
  const int gnu_builtin_avx2   = __builtin_cpu_supports("avx2");

  /* Note that using callgrind the feature AVX2 is not detected. */

  return ARCH_HW_x64 && ARCH_HW_AVX && ARCH_HW_AVX2 && gnu_builtin_popcnt && gnu_builtin_avx2;
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

static void
cpuid (int info[4],
       int info_type) {
  __cpuid_count(info_type, 0, info[0], info[1], info[2], info[3]);
}

/**
 * @endcond
 */
