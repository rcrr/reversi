/**
 * @file
 *
 * @todo Program merge sort function.
 *
 * @brief Linked list module implementation.
 *
 * @details This module defines a linked list implementation.
 *
 * @par linked_list.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2015 Roberto Corradini. All rights reserved.
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
#include <stdlib.h>
#include <assert.h>

#include "linked_list.h"



/****************************************************/
/* Function implementations for the llist_t entity. */
/****************************************************/

/**
 * @brief Linked list structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * linked list structure is not `NULL`.
 *
 * @param cmp a compare function for the data type
 * @return    a pointer to a new linked list structure
 */
llist_t *
llist_new (llist_compare_f cmp)
{
  llist_t *l;
  static const size_t size_of_t = sizeof(llist_t);
  l = (llist_t *) malloc(size_of_t);
  assert(l);
  l->head = NULL;
  l->length = 0;
  l->cmp = cmp;
  return l;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #llist_new,
 *        and frees also all the linked elements by recursevely calling #llist_elm_free.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] l the pointer to be deallocated
 */
void
llist_free (l)
     llist_t *l;
{
  if (!l) return;
  llist_elm_t *e = l->head;
  while (e) {
    llist_elm_t *n = e->next;
    llist_elm_free(e);
    e = n;
  }
  free(l);
}

/**
 * @brief Adds a new element at the beginning of the linked list.
 *
 * @details A new element is created, having as data the parameter d, and
 *          as next the first element of the list, then it becomes the actual
 *          first element of the list.
 *
 * An assertion checks that the received pointer to the
 * linked list structure is not `NULL`.
 *
 * @param [in,out] l the linked list
 * @param [in]     d the data referenced by the added element
 */
void
llist_add (l, d)
     llist_t *const l;
     void *const d;
{
  assert(l);
  llist_elm_t *const e = llist_elm_new();
  e->data = d;
  e->next = l->head;
  l->head = e;
  l->length++;
}

/**
 * @brief Removes an element from the list.
 *
 * @details If two elements contain the same data, only the first is removed.
 *          If none of the elements contain the data, the list is unchanged.
 *
 * @param [in,out] l the linked list
 * @param [in]     d the data of the element to remove
 */
void
llist_remove (l, d)
     llist_t *const l;
     void *const d;
{
  assert(l);
  llist_elm_t **e = &(l->head);
  while (*e) {
    if ((*e)->data == d) {
      llist_elm_t *to_be_removed = *e;
      *e = (*e)->next;
      l->length--;
      llist_elm_free(to_be_removed);
      return;
    }
    e = &((*e)->next);
  }
}

/**
 * @brief Calls a function for each element of a list.
 *
 * @details The function applyed to each element receives a pointer
 *          to the data of the element and to the auxiliary data.
 *          The auxiliary data pointer is shared among all elements.
 *
 * @param [in,out] l        the linked list
 * @param [in]     fn       the function applyed on each element
 * @param [in,out] aux_data shared auxiliary data
 */
void
llist_foreach (l, fn, aux_data)
     llist_t *const l;
     void (* fn) (void *const elm_data,
                  void *const aux_data);
     void *const aux_data;
{
  assert(l);
  for (llist_elm_t *e = l->head; e; e = e->next) {
    if (0) printf("llist_foreach: e=%p, e->data=%p, e->next=%p\n", (void *)e, (void *)e->data, (void *)e->next);
    fn(e->data, aux_data);
  }
}

/**
 * @brief Gets the element at the given position in a linked list.
 *
 * @param [in,out] l the linked list
 * @param [in]     n the position of the element, counting from 0
 * @return           the element, or NULL when n is off the end of the list
 */
llist_elm_t *
llist_nth (l, n)
     llist_t *const l;
     const size_t n;
{
  assert(l);
  if (n > l->length) return NULL;
  llist_elm_t *e;
  size_t i;
  for (e = l->head, i = 0; e; e = e->next, i++) {
    if (i == n) return e;
  }
  abort(); /* Should never happens. */
}

/**
 * @brief Gets the data of the element at the given position.
 *
 * @param [in,out] l the linked list
 * @param [in]     n the position of the element, counting from 0
 * @return           the element's data, or NULL when n is off the end of the list
 */
void *
llist_nth_data (l, n)
     llist_t *const l;
     const size_t n;
{
  assert(l);
  if (n > l->length) return NULL;
  llist_elm_t *e;
  size_t i;
  for (e = l->head, i = 0; e; e = e->next, i++) {
    if (i == n) return e->data;
  }
  abort(); /* Should never happens. */
}

/**
 * @brief Finds the element in a list which contains the given data.
 *
 * @details The pointer data in each element of the list is compared with
 *          the `d` parameter. The first element matching is returned, `NULL`
 *          when none matches.
 *          When the search is successful, and the parameter `i` is not `NULL`,
 *          the index of the matching element, starting from `0`, is returned
 *          in the value pointed by `i`.
 *
 * @param [in,out] l the linked list
 * @param [in]     d the element data to find
 * @param [out]    i the element index if the search is successful
 * @return           the found element or NULL
 */
llist_elm_t *
llist_find (l, d, i)
     llist_t *const l;
     void *const d;
     size_t *i;
{
  assert(l);
  llist_elm_t *e;
  size_t k;
  for (e = l->head, k = 0; e; e = e->next, k++) {
    if (e->data == d) {
      if (i) *i = k;
      return e;
    }
  }
  return NULL;
}

/**
 * @brief Inserts a new element into the list at the given position.
 *
 * @details If the parameter `i` is larger than the number of elements in the list,
 *          the new element is added on to the end of the list.
 *
 * @param [in,out] l the linked list
 * @param [in]     d the element data to find
 * @param [in]     i the position to insert the element
 */
void
llist_insert_at_position (l, d, i)
     llist_t *const l;
     void *const d;
     const size_t i;
{
  assert(l);
  const size_t position = (i > l->length) ? l->length : i;
  llist_elm_t **e = &(l->head);
  for (size_t k = 0; k < position; k++, e = &((*e)->next)) ;
  llist_elm_t *const new = llist_elm_new();
  new->data = d;
  new->next = *e;
  *e = new;
  l->length++;
}

/**
 * @brief Inserts a new element after previous one.
 *
 * @details If the list doesn't contain element `p`, the new element is not
 *          created.
 *
 * @param [in,out] l the linked list
 * @param [in]     p the previous element
 * @param [in]     d the data to insert
 */
void
llist_insert_after_elm (l, p, d)
     llist_t *const l;
     llist_elm_t *const p;
     void *const d;
{
  assert(l);
  for (llist_elm_t *e = l->head; e; e = e->next) {
    if (e == p) {
      llist_elm_t *const new = llist_elm_new();
      new->data = d;
      new->next = e->next;
      e->next = new;
      l->length++;
      break;
    }
  }
}

/**
 * @brief Inserts a new element  before `n`, with the given data `d`.
 *
 * @details If the list doesn't contain element `n`, the new element is not
 *          created.
 *
 * @param [in,out] l the linked list
 * @param [in]     n the next element
 * @param [in]     d the data to insert
 */
void
llist_insert_before_elm (l, n, d)
     llist_t *const l;
     llist_elm_t *const n;
     void *const d;
{
  assert(l);
  for (llist_elm_t **e = &(l->head); *e; e = &((*e)->next)) {
    if (*e == n) {
      llist_elm_t *const new = llist_elm_new();
      new->data = d;
      new->next = *e;
      *e = new;
      l->length++;
      break;
    }
  }
}

/**
 * @brief Gets the last element in a list.
 *
 * @param [in,out] l the linked list
 * @return           the last element
 */
llist_elm_t *
llist_last_elm (l)
     llist_t *const l;
{
  llist_elm_t *e = l->head;
  if (!e) return NULL;
  while (e->next)
    e = e->next;
  return e;
}

/**
 * @brief Adds the second list onto the end of the first one.
 *
 * @details The second list is destroyed. Note that the elements of the
 *          second list are not copied, they are used directly.
 *          When the second list is `NULL` nothing happens.
 *
 * @param [in,out] la the first linked list
 * @param [in,out] lb the second linked list
 */
void
llist_concat (la, lb)
     llist_t *const la;
     llist_t *const lb;
{
  assert(la);
  if (!lb) return;
  llist_elm_t *last_elm_in_first_list = llist_last_elm(la);
  if (last_elm_in_first_list) {
    last_elm_in_first_list->next = lb->head;
  } else {
    la->head = lb->head;
  }
  la->length += lb->length;
  llist_free(lb);
}

/**
 * @brief Reverses a linked list.
 *
 * @param [in,out] l the linked list
 */
void
llist_reverse (l)
     llist_t *const l;
{
  assert(l);
  llist_elm_t *next;
  llist_elm_t *e = l->head;
  l->head = NULL;
  while (e) {
    next = e->next;
    e->next = l->head;
    l->head = e;
    e = next;
  }
}

/**
 * @brief Sorts a linked list.
 *
 * @details Applies the insertion sort algorithm to the list, ordering it in
 *          ascending order.
 *
 * @param [in,out] l the linked list
 */
void
llist_insertion_sort (l)
     llist_t *const l;
{
  assert(l);
  assert(l->cmp);

  if (l->length < 2) return;

  llist_elm_t *tail = l->head->next;
  l->head->next = NULL;
  llist_elm_t *tmp;
  for (llist_elm_t *t = tail; t;) {
    tmp = t->next;
    llist_elm_t **e;
    for (e = &(l->head); *e; e = &((*e)->next)) {
      if (l->cmp(t->data, (*e)->data) < 0) break;
    }
    t->next = *e;
    *e = t;
    t = tmp;
  }
}

/**
 * @brief Sorts a linked list.
 *
 * @details Applies the insertion sort algorithm to the list, ordering it in
 *          ascending order.
 *          This algorithm enhance insertion sort by merging already sorted
 *          sublists, reducing the required comparisons.
 *
 * @param [in,out] l the linked list
 */
void
llist_adv_insertion_sort (l)
     llist_t *const l;
{
  assert(l);
  assert(l->cmp);

  if (l->length < 2) return;

  llist_elm_t *t = l->head; /* t is the head of the tail. */
  l->head = NULL; /* The list is now empty. */

  while (t) {

    /*
     * Computes the run, a sequence of elements taken from the tail that are ordered.
     * r0 points to the first element in the run.
     * r1 points to the last element in the run.
     * r2 points to the next run.
     * The last element, next value is set to NULL.
     * The head of the tail is then moved to r2.
     */
    llist_elm_t *r0 = t;
    llist_elm_t *r1;
    llist_elm_t *r2;
    for (r1 = t, r2 = t->next; r2 && l->cmp(r1->data, r2->data) < 0; r1 = r2, r2 = r2->next) ;
    r1->next = NULL; /* Cut the run from the tail. */
    t = r2; /* Set the new tail head. */

    llist_elm_t **ep = &(l->head); /* A pointer to the next element pointer in the sorted list. */
    llist_elm_t **rp = &r0; /* A pointer to the next element pointer in the run. */
    while (*rp) {

      /* Searches for the insertion point in the list. */
      for (; *ep; ep = &((*ep)->next))
        if (l->cmp((*rp)->data, (*ep)->data) < 0) break; /* Insertion point found. */

      /* Searches the last element in the run, pointed by *xp, to insert at point. */
      llist_elm_t **xp = rp;
      for (; *xp && (!*ep || l->cmp((*xp)->data, (*ep)->data) < 0); xp = &((*xp))->next) ;

      /* Inserts a piece of the run starting at *rp, and ending at *xp ....*/
      llist_elm_t *tmp = *xp;
      *xp = *ep; /* Connects the last element in the run to the point. */
      *ep = *rp; /* Connects the run head with the point element. */

      *rp = tmp; /* Advances the run list pointer to next. */
    }
  }
}


static void
aux_print_lengths (lengths, stack_size, lengths_fill_p)
     size_t *const lengths;
     size_t stack_size;
     size_t *lengths_fill_p;
{
  printf("\n");
  printf("LENGTHS ARRAY: lengths_fill_p=%p, index=%02zu\n", (void *)lengths_fill_p, lengths_fill_p - &lengths[0]);
  for (size_t i = 0; i < stack_size; i++) {
    printf("               lengths[%02zu]=%02zu, address=%p\n", i, lengths[i], (void *)&lengths[i]);
  }
}


static void
aux_print_lists (lists, stack_size, lists_fill_p)
     llist_t *const lists;
     size_t stack_size;
     llist_t *lists_fill_p;
{
  printf("\n");
  printf("LISTS ARRAY: lists_fill_p=%p, index=%02zu\n", (void *)lists_fill_p, lists_fill_p - &lists[0]);
  for (size_t i = 0; i < stack_size; i++) {
    printf("             lists[%02zu]: head=%p, length=%02zu, cmp=%p, address=%p\n", i, (void *)(lists[i].head), lists[i].length, lists[i].cmp, (void *)&lists[i]);
    llist_elm_t *e = lists[i].head;
    size_t index = 0;
    while (e) {
      printf("             ... ... ...   e=%p, index=%02zu, e->data=%d\n", (void *)e, index, *(int *)(e->data));
      e = e->next;
      index++;
    }
  }
}


static void
aux_print_tail (head_of_tail)
     llist_elm_t *head_of_tail;
{
  printf("\n");
  printf("TAIL LIST: head_of_tail=%p\n", (void *)head_of_tail);
  llist_elm_t *e = head_of_tail;
  size_t index = 0;
  while (e) {
    printf("           ... ... .. e=%p, index=%02zu, e->data=%d\n", (void *)e, index, *(int *)(e->data));
    e = e->next;
    index++;
  }
}


/**
 * @brief Sorts a linked list.
 *
 * @details Applies the merge sort algorithm to the list, ordering it in
 *          ascending order.
 *
 * @param [in,out] l the linked list
 */
void
llist_merge_sort (l)
     llist_t *const l;
{
  assert(l);
  assert(l->cmp);

  if (l->length < 2) return;

  static const size_t min_merge_length = 4;

  size_t stack_size = 1;
  for (size_t i = min_merge_length; i < l->length; i <<= 1) stack_size++;

  llist_t *const lists = (llist_t *) malloc(stack_size * sizeof(llist_t));
  llist_t *lists_fill_p = &(lists[0]);
  size_t *const lengths = (size_t *) malloc(stack_size * sizeof(size_t));
  size_t *lengths_fill_p = &lengths[0];
  llist_elm_t *head_of_tail = l->head;

  aux_print_tail(head_of_tail);

  /* Prepares the lists array. */
  for (size_t i = 0; i < stack_size; i++) {
    lengths[i] = 0;
    lists[i].head = NULL;
    lists[i].length = 0;
    lists[i].cmp = l->cmp;
  }

  aux_print_lists(lists, stack_size, lists_fill_p);

  /* Computes the lists lengths. */
  lengths[0] = l->length;
 begin:
  while (*lengths_fill_p > min_merge_length) {
    *(lengths_fill_p + 1) = *lengths_fill_p / 2;
    *lengths_fill_p -= *(lengths_fill_p + 1);
    lengths_fill_p++;
  }

  aux_print_lengths(lengths, stack_size, lengths_fill_p);

  while (*lengths_fill_p > 0 && *lengths_fill_p <= min_merge_length) { /* When the last run is shorter than the min merge length a new list is added on the stack. */
    printf("\nCreate list: lengths_fill_p=%p, index=%02zu, length=%02zu, lists_fill_p=%p\n",
           (void *)lengths_fill_p, lengths_fill_p - &lengths[0], *lengths_fill_p, (void *)lists_fill_p);
    /* Prepares and sorts the work list, wl. */
    llist_t *wl = lists_fill_p; lists_fill_p++;
    wl->head = head_of_tail;
    wl->length = *lengths_fill_p;
    *lengths_fill_p = 0;
    if (lengths_fill_p > &lengths[0]) lengths_fill_p--;
    llist_elm_t **hp = &head_of_tail;
    for (size_t i = 0; i < wl->length; i++, hp = &((*hp)->next)) ;
    head_of_tail = *hp;
    *hp = NULL; /* Now wl is ready to be used. */
    llist_adv_insertion_sort(wl);
  }

  aux_print_tail(head_of_tail);
  aux_print_lengths(lengths, stack_size, lengths_fill_p);
  aux_print_lists (lists, stack_size, lists_fill_p);

 merge: // Has to be fully understood if it is required to iterate ...
  if (lists_fill_p > &lists[1]) { /* There are two or more lists on the stack. */
    llist_t *l1 = lists_fill_p - 1;
    llist_t *l2 = lists_fill_p - 2;
    size_t len_diff = (l1->length > l2->length) ? l1->length - l2->length : l2->length - l1->length;
    if (len_diff < 2) { /* Merge lists. */
      printf("\nMerge lists l1 + l2 -> l2: l1[address=%p, length=%02zu, head=%p], l2[address=%p, length=%02zu, head=%p]\n",
             (void *)l1, l1->length, (void *)(l1->head), (void *)l2, l2->length, (void *)(l2->head));
      lists_fill_p--;
      llist_elm_t *c1 = l1->head;
      llist_elm_t *c2 = l2->head;
      l2->head = NULL;
      l2->length += l1->length;
      l1->head = NULL; // Could be removed. It is here just to make the program more clear.
      l1->length = 0; // Could be removed. It is here just to make the program more clear.
      llist_elm_t **ep = &(l2->head);
      while (c2 && c1) {
        llist_elm_t **np = (l->cmp(c2->data, c1->data) <= 0) ? &c2 : &c1;
        *ep = *np; /* Connects the element selected among c1 and c2 to the tail of the composing list. */
        *np = (*np)->next; /* Either c1 or c2 is moved to next. */
        ep = &((*ep)->next); /* Moves the pointer to the tail of the forming list. */
      }
      llist_elm_t **np = c1 ? &c1 : &c2;
      *ep = *np;
      aux_print_lists (lists, stack_size, lists_fill_p);
      goto merge;
    }
  }
  if (lengths[0] > 0) goto begin;

  l->head = lists[0].head;

  free(lists);
  free(lengths);
}



/********************************************************/
/* Function implementations for the llist_elm_t entity. */
/********************************************************/

/**
 * @brief Element structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * element is not `NULL`.
 *
 * @return a pointer to a new element structure
 */
llist_elm_t *
llist_elm_new (void)
{
  llist_elm_t *e;
  static const size_t size_of_t = sizeof(llist_elm_t);
  e = (llist_elm_t *) malloc(size_of_t);
  assert(e);
  e->data = NULL;
  e->next = NULL;
  return e;
  ;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #llist_elm_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] e the pointer to be deallocated
 */
void
llist_elm_free (e)
     llist_elm_t *e;
{
  free(e);
}
