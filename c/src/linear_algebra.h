/**
 * @file
 *
 * @brief Linear Algebra module definitions.
 *
 * @details This module defines functions that implements the Cholesky Decomposition,
 *          LU Factorization, and several other utilities for linear algebra operations.
 *
 *          The module is the result of an iterative self studying and progressive learning process.
 *          As so it is not complete, nor well documented, and neither consistent. But there is a
 *          lot of knowledge in it.
 *
 *          With the current hardware architectures it is not possible to achieve good performances
 *          without the blocking ( or tiling ) technique. Blocked algorithms enable cache-friendly
 *          amortized load-store operations, that are the bottleneck for this kind of algorithms on the current systems.
 *
 *          The module has a set of "naive" algorithms that are 25 times slower than state of the art
 *          tiled variants. This naive set of functions do not have dependencies. Tiled algorithms depend
 *          on BLAS level 3 functions that are then required. The module is built on top of
 *          the libflame and BLIS libraries, that on the side of their own API provide also a BLAS and LAPACK
 *          compatibility layer.
 *
 *          BLIS provides BLAS, and BLAS like, functions being single threaded, or fine grained multi-threaded.
 *          libflame provides LAPACK, and LAPACK like, functions either single threaded, or coarse grained multi-threaded.
 *          libflame provides also an high-level framework to develop linear algebra algorithms.
 *
 *          The Cholesky decomposition has been implemented in many different ways. With a naive approach, with a blocked
 *          implementation, and with an OpenMP, task based, version of the blocked design.
 *          The blocked version has performances in line with the libflame FLA_Chol implementation, whether the task based
 *          is equivalent to the multi-threaded FLASH_Chol design.
 *          A scalar and blocked variants have been realized using the libflame framework.
 *
 *          Many components and tools are missing, but with the decision of encapsulating the FLAME framework in the code
 *          of reversi function, I do not see a compelling reason to spend further relevant time on this endeavor.
 *
 * @par linear_algebra.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2019, 2020 Roberto Corradini. All rights reserved.
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

#ifndef LINEAR_ALGEBRA_H
#define LINEAR_ALGEBRA_H

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#define BLIS_DISABLE_BLAS_DEFS
#include "blis.h"
#pragma GCC diagnostic pop

#include "FLAME.h"

extern void
lial_dpotrf (const char *uplo,
             const int *n,
             double *a,
             const int *lda,
             int *info);

extern void
lial_dpotrs (const char *uplo,
             const int *n,
             const int *nrhs,
             double *a,
             const int *lda,
             double *b,
             const int *ldb,
             int *info);

extern void
lial_dpotrf_bp (const char *uplo,
                const int *n,
                double *a,
                const int *lda,
                int *info,
                const unsigned int block_size,
                const unsigned int thread_count);

extern void
lial_dpotrf_blis (const char *uplo,
                  const int *n,
                  double *a,
                  const int *lda,
                  int *info,
                  const unsigned int thread_count);

/* It is not multi-threaded yet! Be careful. */
extern void
lial_dpotrs_bp (const char *uplo,
                const int *n,
                const int *nrhs,
                double *a,
                const int *lda,
                double *b,
                const int *ldb,
                int *info,
                const unsigned int block_size,
                const unsigned int thread_count);

extern void
lial_dgemm (char *transa,
            char *transb,
            int *m,
            int *n,
            int *k,
            double *alpha,
            double *a,
            int *lda,
            double *b,
            int *ldb,
            double *beta,
            double *c,
            int *ldc);

extern void
lial_dsyrk (char *uplo,
            char *trans,
            int *n,
            int *k,
            double *alpha,
            double *a,
            int *lda,
            double *beta,
            double *c,
            int *ldc);

/**
 * @brief <B>D</B>ouble <B>TR</B>iangular <B>S</B>olve <B>M</B>atrix
 *
 * @details
 *
 *  Details
 * ---------
 *
 * DTRSM is part of the BLAS Level 3 API, it solves a triangular matrix equation when
 * arguments are defined as double precision.
 * The proposed implementation links directly to the underlining BLAS library.
 *
 *
 *  Purpose
 * ---------
 *
 * Solves one of the matrix equations:
 * @code
 * .
 * .    op( A )*X = alpha*B,   or   X*op( A ) = alpha*B
 * .
 * @endcode
 * where alpha is a scalar, X and B are m by n matrices, A is a unit, or
 * non-unit,  upper or lower triangular matrix  and  op( A )  is one  of:
 * @code
 * .
 * .    op( A ) = A   or   op( A ) = A**T
 * .
 * @endcode
 * The matrix X is overwritten on B.
 *
 *
 *  Arguments
 * -----------
 *
 * - _[in]_ ***side*** : `char *`
 * <br>
 * On entry, `side` specifies whether `op( A )` appears on the left
 * or right of `X` as follows:
 * @code
 * .
 * .    *side = 'L' or 'l'   op( A )*X = alpha*B.
 * .
 * .    *side = 'R' or 'r'   X*op( A ) = alpha*B.
 * .
 * @endcode
 * <p>
 * - _[in]_ ***uplo*** : `char *`
 * <br>
 * On entry, `uplo` specifies whether the matrix `A` is an upper or
 * lower triangular matrix as follows:
 * @code
 * .
 * .    *uplo = 'U' or 'u'   A is an upper triangular matrix.
 * .
 * .    *uplo = 'L' or 'l'   A is a lower triangular matrix.
 * .
 * @endcode
 * <p>
 * - _[in]_ ***transa*** : `char *`
 * <br>
 * On entry, `transa` specifies the form of `op( A )` to be used in
 * the matrix multiplication as follows:
 * @code
 * .
 * .    *transa = 'N' or 'n'   op( A ) = A.
 * .
 * .    *transa = 'T' or 't'   op( A ) = A**T.
 * .
 * .    *transa = 'C' or 'c'   op( A ) = A**T.
 * .
 * @endcode
 * <p>
 * - _[in]_ ***diag*** : `char *`
 * <br>
 * On entry, `diag` specifies whether or not `A` is unit triangular
 * as follows:
 * @code
 * .
 * .    *diag = 'U' or 'u'   A is assumed to be unit triangular.
 * .
 * .    *diag = 'N' or 'n'   A is not assumed to be unit triangular.
 * .
 * @endcode
 * <p>
 * - _[in]_ ***m*** : `int *`
 * <br>
 * On entry, `m` specifies the number of rows of `B`. `m` must be at
 * least zero.
 * <p>
 * - _[in]_ ***n*** : `int *`
 * <br>
 * On entry, `n` specifies the number of columns of `B`. `n` must be at
 * least zero.
 * <p>
 * - _[in]_ ***alpha*** : `double *`
 * <br>
 * On entry, `alpha` specifies the scalar `alpha`. When `alpha` is
 * zero then `A` is not referenced and `B` need not be set before entry.
 * <p>
 * - _[in]_ ***a*** : `double *` : `array` , `dimension ( *lda, k )`
 * where `k` is `*m` when ```*side = 'L' or 'l'```, and `k` is `*n` when ```*side = 'R' or 'r'```.
 * <br>
 * Before entry with ```*uplo = 'U' or 'u'```, the leading `k` by `k`
 * upper triangular part of the array `A` must contain the upper
 * triangular matrix  and the strictly lower triangular part of
 * `A` is not referenced.<br>
 * Before entry with ```*uplo = 'L' or 'l'```,  the  leading  `k` by `k`
 * lower triangular part of the array `A` must contain the lower
 * triangular matrix  and the strictly upper triangular part of
 * `A` is not referenced.<br>
 * Note that when ```*diag = 'U' or 'u'```,  the diagonal elements of
 * `A` are not referenced either, but are assumed to be unity.
 * <p>
 * - _[in]_ ***lda*** : `int *`
 * <br>
 * On entry, `lda` specifies the first dimension of `A` as declared in the
 * calling calling fanction.<br>
 * When ```*side = 'L' or 'l'``` then `*lda` must be
 * at least ```max( 1, *m )```, when ```*side = 'R' or 'r'``` then `*lda` must be
 * at least ```max( 1, *n )```.
 * <p>
 * - _[in]_ ***b*** : `double *` : `array` , `dimension ( *ldb, *n )`
 * <br>
 * Before entry, the leading `m` by `n` part of the array `B` must
 * contain the right-hand side matrix `B`, and on exit is
 * overwritten by the solution matrix `X`.
 * <p>
 * - _[in]_ ***ldb*** : `int *`
 * <br>
 * On entry, `ldb` specifies the first dimension of `B` as declared in the
 * calling calling fanction.<br>
 * `*ldb` must be at least ```max( 1, *m )```.
 * <p>
 *
 *
 *____________________________________________________________________
 *
 * @param [in]     side   relative position of A with respect to X
 * @param [in]     uplo   whether A is upper or lower triangular
 * @param [in]     transa whether A is transposed or not
 * @param [in]     diag   whether A is unit triangular or not
 * @param [in]     m      number of rows of B
 * @param [in]     n      number of columns of B
 * @param [in]     alpha  scalar parameter that scales B
 * @param [in]     a      A matrix
 * @param [in]     lda    leading dimension of A
 * @param [in,out] b      B on entry and X on exit matrix
 * @param [in]     ldb    leading dimension of B
 *
 */
extern void
lial_dtrsm (char *side,
            char *uplo,
            char *transa,
            char *diag,
            int *m,
            int *n,
            double *alpha,
            double *a,
            int *lda,
            double *b,
            int *ldb);

/**
 * @brief Do not use it, it is not completed, it doesn't work.
 */
extern void
lial_dtrsm_bp (char *side,
               char *uplo,
               char *transa,
               char *diag,
               int *m,
               int *n,
               double *alpha,
               double *a,
               int *lda,
               double *b,
               int *ldb,
               int block_size_m,
               int block_size_n,
               int thread_count);

/**
 * @brief Computes the magnitude of the `v` vector.
 *
 * @details Computes 2-norm of the `v` vector.
 *          If arguments `abs_min`, `abs_min_pos`, `abs_max`, and `abs_max_pos`
 *          are not `NULL` are populated with the smallest and largest modulus of
 *          the components of the vector, and by theyr respective positions.
 *
 * @invariant Pointer `v` cannot be `NULL`.
 *
 * @param [in]  v           pointer to vector
 * @param [in]  n           number of elements
 * @param [out] abs_min     absolute minimum value
 * @param [out] abs_min_pos position of the min
 * @param [out] abs_max     absolute maximum value
 * @param [out] abs_max_pos position of the max
 * @return                  the modulus of the vector
 */
extern double
lial_vector_magnitude (double *v,
                       size_t n,
                       double *abs_min,
                       size_t *abs_min_pos,
                       double *abs_max,
                       size_t *abs_max_pos);

/**
 * @brief Sets to zero all elements of the vector.
 *
 * @invariant Pointer `v` cannot be `NULL`.
 *
 * @param [in, out] v pointer to vector
 * @param [in]      n number of elements
 */
extern void
lial_zero_vector (double *v,
                  size_t n);

/**
 * @brief Allocates a vector of doubles having length equal to`n`.
 *
 * @param [in] n number of elements
 * @return       a pointer to the first element of the vector
 */
extern double *
lial_allocate_vector (size_t n);

/**
 * @brief Frees a vector of doubles, being allocate by a call to #lial_allocate_vector.
 *
 * @param [in, out] v the pointer to the first element of the vector
 */
extern void
lial_free_vector (double *v);

/**
 * @brief Allocates an `nr` x `nc` matrix of doubles.
 *
 * @details The procedures allocates the rectangular matrix, then
 *          allocates an array of pointers to
 *          doubles, and initializes them to pointers to rows of the matrix.
 *
 *          If memory allocations are unsuccessful or either parameters
 *          `nr` or `nc` are zero, `NULL` is returned.
 *
 * @param [in] nr number of rows
 * @param [in] nc number of columns
 * @return        a pointer to the array of pointers to rows
 */
extern double **
lial_allocate_matrix (size_t nr,
                      size_t nc);

/**
 * @brief Allocates a square `n` x `n` matrix of doubles.
 *
 * @details The procedures allocates the square matrix, then
 *          allocates an array of pointers to
 *          doubles, and initializes them to pointers to rows of the matrix.
 *
 *          If memory allocations are unsuccessful or parameter
 *          `n` is zero, `NULL` is returned.
 *
 * @param [in] n number of rows and columns
 * @return       a pointer to the array of pointers to rows
 */
extern double **
lial_allocate_square_matrix (size_t n);

/**
 * @brief Frees a matrix of doubles, being allocate by a call to #lial_allocate_matrix.
 *
 * @details The number of rows is required to be correct. The rows of the matrix could have been
 *          swapped and it is not sure that `m[0]` holds the address of the matrix.
 *          Swapping rows is permitted as long as the `m` pointers points to the set of all and only the
 *          pointers to rows.
 *
 * @param [in, out] m  the pointer to the array of row pointers of the matrix
 * @param [in]      nr number of rows
 */
extern void
lial_free_matrix (double **m,
                  size_t nr);

/**
 * @brief Factorizes a real matrix using the LU decomposition.
 *
 * @details Given a matrix `a[0..n-1][0..n-1]`, this function replaces it by the LU decomposition
 *          of a rowwise permutation of itself computed applying the Crout algorithm.
 *          Arguments `a` and `n` are input. `a` is also output, arranged having the diagonal elements of
 *          matrix `U` assumed to be ones.
 *          Argument `indx[0..n-1] is an output vector that records the row permutation effected by
 *          the partial pivoting.
 *          Argument `scale[0..n-1]` is an output vector that records the parameters used to scale the rows.
 *          This routine is used in combination with lial_lu_bsubst_naive() to solve linear equations or invert a matrix.
 *
 * @param [in, out] a      the square matrix to be decomposed
 * @param [in]      n      rank of the square matrix
 * @param [out]     indx   row permutations
 * @param [out]     scale  row parameters for scaling
 * @return
 */
extern int
lial_lu_decom_naive (double **a,
                     size_t n,
                     size_t *indx,
                     double *scale);

extern void
lial_lu_bsubst_naive (double **a,
                      size_t n,
                      size_t *indx,
                      double scale[],
                      double b[]);

/**
 * @brief Factorizes a real symmetric positive-definite matrix using the Cholesky decomposition.
 *
 * @details Given a positive-definite symmetric matrix `a[0..n-1][0..n-1]`, this function constructs
 *          its Cholewsky decomposition `A = Lower Lower_transposed`.
 *          On imput, only the upper triangle of `a` need to be given; it is not modified.
 *          The Cholewsky factor `Lower` is returned in the lower triangle of `a`, except for its
 *          diagonal elements which are returned in `p[0..n-1]`.
 *
 * @param [in, out] a a pointer returned by calling #lial_allocate_matrix
 * @param [in]      n rank of the square matrix
 * @param [out]     p the diagonal of the factorized `Lower` matrix
 */
extern void
lial_chol_fact_naive (double **a,
                      size_t n,
                      double p[]);

/**
 * @brief Parallel version of cholesky factorization.
 */
extern void
lial_chol_fact_omp (double **a,
                    size_t n,
                    double p[]);

/**
 * @brief Parallel version of cholesky factorization, with a given thread count.
 */
extern void
lial_chol_fact_omp_tc (double **a,
                       size_t n,
                       double p[],
                       size_t thread_count);

/**
 * @brief Solves a set of linear equations.
 *
 * @details Solves the set of `n` linear equations `A x = b`, where `a` is a positive-definite symmetrix matrix.
 *          `a[0..n-1][0..n-1]` and `p[0..n-1]` are input as the output of the function #lial_chol_fact_naive.
 *          Only the lower subdiagonal portion of `a` is accessed.
 *          `b[0..n-1]` is input as the right-hand side vector.
 *          The solution vector is returned in x[0..n-1].
 *          `a`, `n`, and `p` are not modified and can be left in place for successive calls
 *          with different right-hand sides `b`.
 *          `b` is not modified unless you identify `b` and `x` in the calling sequence, which is allowed.
 *
 * @param [in]  a the pointer to the matrix factorized by calling #lial_chol_fact_naive
 * @param [in]  n rank of the square matrix
 * @param [in]  p as the output obtained by calling #lial_chol_fact_naive
 * @param [in]  b the right-hand side vector
 * @param [out] x the solution vector
 */
extern void
lial_chol_solv_naive (double **a,
                      size_t n,
                      double p[],
                      double b[],
                      double x[]);

/**
 * @brief Inverts the `a` matrix.
 *
 * @details To be completed.
 *
 * @param [in, out] a the pointer to the symmetrix matrix
 * @param [in]      n rank of the square matrix
 * @param [out]     p the diagonal of the factorized `Lower` matrix
 * @param [out]     z the inverse of `a`
 */
extern void
lial_chol_inv_naive (double **a,
                     size_t n,
                     double p[],
                     double **z);

/**
 * @brief Dumps the `a` matrix into the `file_name` file.
 *
 * @details Opens a binary file using the name given by the `file_name` parameter.
 *          The matrix `a`, composed by `double` elements, having `nr` rows, and `nc`
 *          columns, is written into the binary file.
 *          If `ret_code` is not `NULL`, the result of the operation is returned into the
 *          value referenced by the argument.
 *          `0` is returned on success, a negative value means failure.
 *
 * @param [in]  a          the matrix
 * @param [in]  nr         number of rows
 * @param [in]  nc         number of columns
 * @param [in]  file_name  the file name
 * @param [out] ret_code   return code
 */
extern void
lial_dump_matrix (double **a,
                  size_t nr,
                  size_t nc,
                  const char *file_name,
                  int *ret_code);

/**
 * @brief Reads a matrix from a binary file.
 */
extern double **
lial_retrieve_matrix (const char *file_name,
                      size_t *nr,
                      size_t *nc,
                      int *ret_code);

/**
 * @brief Clones the matrix `a`.
 */
double **
lial_clone_matrix (double **a,
                   size_t nr,
                   size_t nc);

/**
 * @brief Dumps the vector `v` into the `file_name` file.
 */
extern void
lial_dump_vector (double *v,
                  size_t n,
                  char *file_name,
                  int *ret_code);

/**
 * @brief Reads a vector from a binary file.
 */
extern double *
lial_retrieve_vector (char *file_name,
                      size_t *n,
                      int *ret_code);

/**
 * @brief Clones the vector `v`.
 */
extern double *
lial_clone_vector (double *v,
                   size_t n,
                   int *ret_code);

/**
 * @brief Computes the dot product of vectors `a` and `b`.
 */
extern double
lial_dot_product (const double *a,
                  const double *b,
                  size_t n);

/**
 * @brief Computes the dot product of vectors `a` and `b`.
 */
extern double
lial_dot_product_avx (const double *a,
                      const double *b,
                      size_t n);

extern int
lial_lu_inv_naive (double **a,
                   size_t n,
                   size_t indx[],
                   double scale[],
                   double **z);

extern void
lial_transpose_square_matrix (double **a,
                              size_t n);

extern void
lial_chol_fact_lapack (double **a,
                       size_t nr,
                       int *ret);

extern void
lial_chol_solv_lapack (double **a,
                       size_t nr,
                       double *b,
                       int *ret);

extern void
lial_chol_inv_lapack (double **a,
                      size_t n,
                      double **z,
                      int *ret);

extern FLA_Error
lial_FLA_Chol_l_blk_var2 (FLA_Obj A,
                          int nb_alg);

extern FLA_Error
lial_FLA_Chol_u_unb_var3 (FLA_Obj A);

#endif /* LINEAR_ALGEBRA_H */
