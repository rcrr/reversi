/**
 * @file
 *
 * @brief Cholesky Decomposition module definitions.
 * @details This module defines ...
 *
 * @par cholesky_decomposition.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2019 Roberto Corradini. All rights reserved.
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

#ifndef CHOLESKY_DECOMPOSITION_H
#define CHOLESKY_DECOMPOSITION_H

/**
 * @brief Computes the magnitude of the `v` vector.
 *
 * @param [in]  v           pointer to vector
 * @param [in]  n           number of elements
 * @param [out] abs_min     minimum value
 * @param [out] abs_min_pos position of the min
 * @param [out] abs_max     maximum value
 * @param [out] abs_max_pos position of the max
 * @return                  the modulus of the vector
 */
double
chol_vector_magnitude (double *v,
                       size_t n,
                       double *abs_min,
                       size_t *abs_min_pos,
                       double *abs_max,
                       size_t *abs_max_pos);

/**
 * @brief Sets to zero all elements of the vector.
 *
 * @param [in, out] v pointer to vector
 * @param [in]      n number of elements
 */
extern void
chol_zero_vector (double *v,
                  size_t n);

/**
 * @brief Allocates a vector of doubles having length equal to`n`.
 *
 * @param [in] n number of elements
 * @return       a pointer to the first element of the vector
 */
extern double *
chol_allocate_vector (size_t n);

/**
 * @brief Frees a vector of doubles, being allocate by a call to #chol_allocate_vector.
 *
 * @param [in, out] v the pointer to the first element of the vector
 */
extern void
chol_free_vector (double *v);

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
chol_allocate_matrix (size_t nr,
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
chol_allocate_square_matrix (size_t n);

/**
 * @brief Frees a matrix of doubles, being allocate by a call to #chol_allocate_matrix.
 *
 * @param [in, out] m the pointer to the array of row pointers of the matrix
 */
extern void
chol_free_matrix (double **m);

/**
 * @brief Factorizes a real symmetric positive-definite matrix using the Cholesky decomposition.
 *
 * @details Given a positive-definite symmetric matrix `a[0..n-1][0..n-1]`, this function constructs
 *          its Cholewsky decomposition `A = Lower Lower_transposed`.
 *          On imput, only the upper triangle of `a` need to be given; it is not modified.
 *          The Cholewsky factor `Lower` is returned in the lower triangle of `a`, except for its
 *          diagonal elements which are returned in `p[0..n-1]`.
 *
 * @param [in, out] a a pointer returned by calling #chol_allocate_matrix
 * @param [in]      n rank of the square matrix
 * @param [out]     p the diagonal of the factorized `Lower` matrix
 */
extern void
chol_fact_naive (double **a,
                 size_t n,
                 double p[]);

/**
 * @brief Parallel version of cholesky factorization.
 */
extern void
chol_fact_omp (double **a,
               size_t n,
               double p[]);

/**
 * @brief Parallel version of cholesky factorization, with a given thread count.
 */
extern void
chol_fact_omp_tc (double **a,
                  size_t n,
                  double p[],
                  size_t thread_count);

/**
 * @brief Solves a set of linear equations.
 *
 * @details Solves the set of `n` linear equations `A x = b`, where `a` is a positive-definite symmetrix matrix.
 *          `a[0..n-1][0..n-1]` and `p[0..n-1]` are input as the output of the function #chol_fact_naive.
 *          Only the lower subdiagonal portion of `a` is accessed.
 *          `b[0..n-1]` is input as the right-hand side vector.
 *          The solution vector is returned in x[0..n-1].
 *          `a`, `n`, and `p` are not modified and can be left in place for successive calls
 *          with different right-hand sides `b`.
 *          `b` is not modified unless you identify `b` and `x` in the calling sequence, which is allowed.
 *
 * @param [in]  a the pointer to the matrix factorized by calling #chol_fact_naive
 * @param [in]  n rank of the square matrix
 * @param [in]  p as the output obtained by calling #chol_fact_naive
 * @param [in]  b the right-hand side vector
 * @param [out] x the solution vector
 */
extern void
chol_solv_naive (double **a,
                 size_t n,
                 double p[],
                 double b[],
                 double x[]);

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
chol_dump_matrix (double **a,
                  size_t nr,
                  size_t nc,
                  char *file_name,
                  int *ret_code);

/**
 * @brief Reads a matrix from a binary file.
 */
extern double **
chol_retrieve_matrix (char *file_name,
                      size_t *nr,
                      size_t *nc,
                      int *ret_code);

/**
 * @brief Clones the matrix `a`.
 */
double **
chol_clone_matrix (double **a,
                   size_t nr,
                   size_t nc,
                   int *ret_code);

/**
 * @brief Dumps the vector `v` into the `file_name` file.
 */
extern void
chol_dump_vector (double *v,
                  size_t n,
                  char *file_name,
                  int *ret_code);

/**
 * @brief Reads a vector from a binary file.
 */
extern double *
chol_retrieve_vector (char *file_name,
                      size_t *n,
                      int *ret_code);

/**
 * @brief Clones the vector `v`.
 */
extern double *
chol_clone_vector (double *v,
                   size_t n,
                   int *ret_code);

/**
 * @brief Computes the dot product of vectors `a` and `b`.
 */
extern double
chol_dot_product (double *a,
                  double *b,
                  size_t n);

/**
 * @brief Computes the dot product of vectors `a` and `b`.
 */
extern double
chol_dot_product_avx (double *a,
                      double *b,
                      size_t n);

#endif /* CHOLESKY_DECOMPOSITION_H */
