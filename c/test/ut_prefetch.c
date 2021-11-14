/**
 * @file
 *
 * @brief Unit test module for prefetch actions.
 * @details Collects tests and experiments on the prefetch concept.
 *
 * @par ut_prefetch.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2021 Roberto Corradini. All rights reserved.
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
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <time.h>

#include "time_utils.h"
#include "prng.h"
#include "unit_test.h"

/*
 * http://locklessinc.com/articles/gcc_asm/
 * https://www.ibiblio.org/gferg/ldp/GCC-Inline-Assembly-HOWTO.html
 * https://stackoverflow.com/questions/42681720/how-do-i-use-labels-in-gcc-inline-assembly
 *
 * https://stackoverflow.com/questions/56432259/how-can-i-indicate-that-the-memory-pointed-to-by-an-inline-asm-argument-may-be
 */

__attribute__ ((always_inline))
inline static
void
prefetcht0 (void *addr)
{
  __asm__ __volatile__ ("prefetcht0 %0;" : : "m" (addr));
}

__attribute__ ((always_inline))
inline static
void
prefetcht1 (void *addr)
{
  __asm__ __volatile__ ("prefetcht1 %0;" : : "m" (addr));
}

inline static
int64_t
aux_sum_plain (int64_t *arr,
               uint64_t len)
{
  int64_t res;
  res = 0;
  for (uint64_t i = 0; i < len; i++) {
    res += arr[i];
  }
  return res;
}

inline static
int64_t
aux_sum_asm (int64_t *arr,
             uint64_t len)
{
  int64_t res;
  uint64_t i;

  __asm__ __volatile__
    (
     "movq $0, %0\n\t"
     "cmpq $0, %3\n\t"
     "je .L%=end\n\t"
     "movq $0, %1\n\t"
     ".L%=loop:\n\t"
     "addq (%2, %1, 8), %0\n\t"
     "incq %1\n\t"
     "cmpq %3, %1\n\t"
     "jb .L%=loop\n\t"
     ".L%=end:\n\t"
     : "=r" (res) , "=&r" (i) , "+r" (arr) , "+r" (len)
     :
     : "cc" , "memory"
     );

  return res;
}

inline static
int64_t
aux_sum_indirect_asm (int64_t **ptr,
                      uint64_t len)
{
  int64_t res;
  uint64_t i;
  int64_t *p;

  __asm__ __volatile__
    (
     "movq $0, %0\n\t"
     "cmpq $0, %3\n\t"
     "je .L%=end\n\t"
     "movq $0, %1\n\t"
     ".L%=loop:\n\t"
     "movq (%2, %1, 8), %4\n\t"
     "addq (%4), %0\n\t"
     "incq %1\n\t"
     "cmpq %3, %1\n\t"
     "jb .L%=loop\n\t"
     ".L%=end:\n\t"
     : "=r" (res) , "=&r" (i) , "+r" (ptr) , "+r" (len) , "=&r" (p)
     :
     : "cc" , "memory"
     );

  return res;
}

__attribute__ ((noinline))
static
int64_t
aux_sum_indirect_asm_prefetch (int64_t **ptr,
                               uint64_t len)
{
  int64_t res;
  uint64_t g2, g1, g0;
  int64_t *p;
  uint64_t lg2, lg1, lg0;

  __asm__ __volatile__
    (
     "movq $80, %[g2] # prefetcht2 distance\n\t"
     "movq $24, %[g1] # prefetcht1 distance\n\t"
     "movq $8,  %[g0] # prefetcht0 distance\n\t"
     "#\n\t"
     "xorq %[res], %[res]\n\t"
     "cmpq $0, %[len]\n\t"
     "je .L%=LR2\n\t"
     "cmpq %[g2], %[len]\n\t"
     "jle .L%=LR1\n\t"
     "movq %[len], %[lg2]\n\t"
     "subq %[g2], %[lg2]\n\t"
     "movq %[len], %[lg1]\n\t"
     "subq %[g1], %[lg1]\n\t"
     "movq %[len], %[lg0]\n\t"
     "subq %[g0], %[lg0]\n\t"
     "#\n\t"
     ".L%=LR0:\n\t"
     "decq %[len]\n\t"
     "movq (%[ptr], %[len], 8), %[p]\n\t"
     "addq (%[p]), %[res]\n\t"
     "# --- \n\t"
     "decq %[lg2]\n\t"
     "movq (%[ptr], %[lg2], 8), %[p]\n\t"
     "prefetcht2 (%[p])\n\t"
     "decq %[lg1]\n\t"
     "movq (%[ptr], %[lg1], 8), %[p]\n\t"
     "prefetcht1 (%[p])\n\t"
     "decq %[lg0]\n\t"
     "movq (%[ptr], %[lg0], 8), %[p]\n\t"
     "prefetcht0 (%[p])\n\t"
     "# --- \n\t"
     "cmpq %[len], %[g2]\n\t"
     "jb .L%=LR0\n\t"
     "#\n\t"
     "cmpq $0, %[g2]\n\t"
     "je .L%=LR2\n\t"
     "movq $0, %[g2]\n\t"
     "#\n\t"
     ".L%=LR1:\n\t"
     "decq %[len]\n\t"
     "movq (%[ptr], %[len], 8), %[p]\n\t"
     "addq (%[p]), %[res]\n\t"
     "cmpq %[len], %[g2]\n\t"
     "jb .L%=LR1\n\t"
     "#\n\t"
     ".L%=LR2:\n\t"
     : [res] "=r" (res) , [p] "=r" (p) , [g2] "=&r" (g2) , [g1] "=&r" (g1) , [g0] "=&r" (g0) ,
       [ptr] "+r" (ptr) , [len] "+r" (len) , [lg2] "=r" (lg2) , [lg1] "=r" (lg1) , [lg0] "=r" (lg0)
     :
     : "cc" , "memory"
     );

  return res;
}

/*
 * Test functions.
 */

#define DATA_SIZE 1024*1024*16// 16M
#define PREFETCH0_GAP 4
#define PREFETCH1_GAP 32

static void
one_t (ut_test_t *const t)
{
  int64_t *data;
  int64_t **p;
  int64_t sum;
  int j;

  bool verbose = (ut_run_time_is_verbose(t)) ? true : false;

  /* Stopwatch variables. */
  timespec_t time_0, time_1, delta_cpu_time, start_time, end_time, delta_time;

  const uint64_t seed = 8701;

  prng_mt19937_t *rand = prng_mt19937_new();
  prng_mt19937_init_by_seed (rand, seed);

  data = malloc(DATA_SIZE * sizeof(int64_t));
  if (!data) ut_assert(t, false);

  if (verbose) printf("DATA_SIZE = %d\n", DATA_SIZE);

  for (int i = 0; i < DATA_SIZE; i++) {
    data[i] = ((i / 2) * 2) == i ? 1 : -1;
  }

  p = malloc(DATA_SIZE * sizeof(int64_t *));
  if (!p) ut_assert(t, false);
  for (int i = 0; i < DATA_SIZE; i++) {
    p[i] = &data[i];
  }
  prng_mt19937_shuffle_array_p(rand, p, DATA_SIZE);

  /* STEP 0 - no indiect refs. */

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  sum = 0;
  j = 0;
  for (; j < DATA_SIZE; j++) {
    sum += data[j];
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);
  clock_gettime(CLOCK_REALTIME, &end_time);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);
  if (verbose) {
    printf("          CPUTIME          REALTIME\n");
    printf("[%6lld.%9ld][%6lld.%9ld] - Step 0: no indiect refs.\n",
           (long long) timespec_get_sec(&delta_cpu_time), timespec_get_nsec(&delta_cpu_time),
           (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
  }

  ut_assert(t, sum == 0);



  /* STEP 1 : with indirect refs. */

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  sum = 0;
  j = 0;
  for (; j < DATA_SIZE; j++) {
    sum += *p[j];
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);
  clock_gettime(CLOCK_REALTIME, &end_time);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);
  if (verbose) {
    printf("[%6lld.%9ld][%6lld.%9ld] - Step 1: with indirect refs.\n",
           (long long) timespec_get_sec(&delta_cpu_time), timespec_get_nsec(&delta_cpu_time),
           (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
  }

  ut_assert(t, sum == 0);



  /* STEP 2 : with indirect refs, and prefetch. */

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  sum = 0;
  j = 0;
  for (; j < DATA_SIZE - PREFETCH1_GAP; j++) {
    //prefetcht1(p[j + PREFETCH1_GAP]);
    //prefetcht0(p[j + PREFETCH0_GAP]);
    __builtin_prefetch(p[j + PREFETCH1_GAP], 0, 2); // prefetch1
    __builtin_prefetch(p[j + PREFETCH0_GAP], 0, 3); // prefetch0
    sum += *p[j];
    /*
    printf("j = %d, p[j] - data = %ld, p[j] = %ld\n", j, p[j] - data, *p[j]);
    printf("data                 = %p\n", (void *) data);
    printf("p[j]                 = %p\n", (void *) p[j]);
    printf("p[j + PREFETCH0_GAP] = %p\n", (void *) p[j + PREFETCH0_GAP]);
    printf("p[j + PREFETCH1_GAP] = %p\n", (void *) p[j + PREFETCH1_GAP]);
    if (true) abort();
    */
  }

  for (; j < DATA_SIZE; j++) {
    sum += *p[j];
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);
  clock_gettime(CLOCK_REALTIME, &end_time);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);
  if (verbose) {
    printf("[%6lld.%9ld][%6lld.%9ld] - Step 2: with indirect refs, and prefetch. PREFETCH0_GAP = %d, PREFETCH1_GAP = %d\n",
           (long long) timespec_get_sec(&delta_cpu_time), timespec_get_nsec(&delta_cpu_time),
           (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time),
           PREFETCH0_GAP, PREFETCH1_GAP);
  }

  ut_assert(t, sum == 0);



  /* STEP 3 : ... */

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  sum = aux_sum_plain(data, DATA_SIZE);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);
  clock_gettime(CLOCK_REALTIME, &end_time);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);
  if (verbose) {
    printf("[%6lld.%9ld][%6lld.%9ld] - Step 3: function aux_sum_plain.\n",
           (long long) timespec_get_sec(&delta_cpu_time), timespec_get_nsec(&delta_cpu_time),
           (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
  }

  ut_assert(t, sum == 0);



  /* STEP 4 : ... */

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  sum = aux_sum_asm(data, DATA_SIZE);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);
  clock_gettime(CLOCK_REALTIME, &end_time);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);
  if (verbose) {
    printf("[%6lld.%9ld][%6lld.%9ld] - Step 4: function aux_sum_asm.\n",
           (long long) timespec_get_sec(&delta_cpu_time), timespec_get_nsec(&delta_cpu_time),
           (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
  }

  ut_assert(t, sum == 0);



  /* STEP 5 : ... */

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  sum = aux_sum_indirect_asm(p, DATA_SIZE);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);
  clock_gettime(CLOCK_REALTIME, &end_time);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);
  if (verbose) {
    printf("[%6lld.%9ld][%6lld.%9ld] - Step 5: function aux_sum_indirect_asm.\n",
           (long long) timespec_get_sec(&delta_cpu_time), timespec_get_nsec(&delta_cpu_time),
           (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
  }

  ut_assert(t, sum == 0);


  /* STEP 6 : ... */

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  sum = aux_sum_indirect_asm_prefetch(p, DATA_SIZE);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);
  clock_gettime(CLOCK_REALTIME, &end_time);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);
  if (verbose) {
    printf("[%6lld.%9ld][%6lld.%9ld] - Step 6: function aux_sum_indirect_asm_prefetch.\n",
           (long long) timespec_get_sec(&delta_cpu_time), timespec_get_nsec(&delta_cpu_time),
           (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
  }

  ut_assert(t, sum == 0);

  free(p);
  free(data);
  prng_mt19937_free(rand);
}

static void
zero_length_t (ut_test_t *const t)
{
  const int len = 0;
  int64_t *arr = NULL;
  int64_t res;

  res = aux_sum_plain(arr, len);
  ut_assert(t, res == 0);

  res = aux_sum_asm(arr, len);
  ut_assert(t, res == 0);

  res = aux_sum_indirect_asm(&arr, len);
  ut_assert(t, res == 0);

  res = aux_sum_indirect_asm_prefetch(&arr, len);
  ut_assert(t, res == 0);
}

static void
small_length_t (ut_test_t *const t)
{
  const int len = 5;
  int64_t arr[] = { -1, 1, -2, 0, 3 };
  const int64_t expected = 1;
  int64_t *ptr[5];
  int64_t res;

  ptr[0] = &arr[3];
  ptr[1] = &arr[0];
  ptr[2] = &arr[4];
  ptr[3] = &arr[2];
  ptr[4] = &arr[1];

  res = aux_sum_plain(arr, len);
  ut_assert(t, expected == res);

  res = aux_sum_asm(arr, len);
  ut_assert(t, expected == res);

  res = aux_sum_indirect_asm(ptr, len);
  ut_assert(t, expected == res);

  res = aux_sum_indirect_asm_prefetch(ptr, len);
  ut_assert(t, expected == res);
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

  ut_suite_t *const s = ut_suite_new(&config, "prefetch");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "zero_length", zero_length_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "small_length", small_length_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_10,   "one", one_t);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}
