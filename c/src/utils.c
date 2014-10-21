#include <stdio.h>
#include <stdlib.h>

#define is_less(v1, v2) (v1 < v2)

#define swap(r,s)  do { double t = r; r = s; s = t; } while(0)

#define swap_p(r,s)  do { void *t = r; r = s; s = t; } while(0)

void
sift_down (double *const a, const int start, const int end);

void
sift_down_p (void **const a, const int start, const int end);

void
heapsort (double *const a,
          const int count)
{
  for (int start = (count - 2) / 2; start >= 0; start--) {
    sift_down(a, start, count);
  }
  for (int end = count - 1; end > 0; end--) {
    swap(a[end], a[0]);
    sift_down(a, 0, end);
  }
}

void
heapsort_p (void **const a,
            const int count)
{
  for (int start = (count - 2) / 2; start >= 0; start--) {
    sift_down_p(a, start, count);
  }
  for (int end = count - 1; end > 0; end--) {
    swap_p(a[end], a[0]);
    sift_down_p(a, 0, end);
  }
}

void
sift_down (double *const a,
           const int start,
           const int end)
{
  int root = start;
  while (root * 2 + 1 < end) {
    int child = 2 * root + 1;
    if ((child + 1 < end) && is_less(a[child], a[child + 1])) {
      child += 1;
    }
    if (is_less(a[root], a[child])) {
      swap(a[child], a[root]);
      root = child;
    }
    else
      return;
  }
}

void
sift_down_p (void **const a,
             const int start,
             const int end)
{
  int root = start;
  while (root * 2 + 1 < end) {
    int child = 2 * root + 1;
    if ((child + 1 < end) && is_less(a[child], a[child + 1])) {
      child += 1;
    }
    if (is_less(a[root], a[child])) {
      swap_p(a[child], a[root]);
      root = child;
    }
    else
      return;
  }
}


int
main()
{
  double vals_to_sort[] = {
    1.4, 50.2, 5.11, -1.55, 301.521, 0.3301, 40.17,
    -18.0, 88.1, 30.44, -37.2, 3012.0, 49.2};

  const int values_size = sizeof(vals_to_sort) / sizeof(vals_to_sort[0]);
  void *pointers_to_sort[values_size];

  for (int i = 0; i < values_size; i++) {
    pointers_to_sort[i] = vals_to_sort + i;
  }

  swap_p(pointers_to_sort[5], pointers_to_sort[3]);

  printf("index;value;address;point_to\n");
  for (int i = 0; i < values_size; i++) {
    printf("%2d;%10.3f;%p;%p\n", i, vals_to_sort[i], (void *) (vals_to_sort + i), pointers_to_sort[i]);
  }

  printf("pointers_to_sort[0] < pointers_to_sort[1] = %d\n", pointers_to_sort[0] < pointers_to_sort[1]);
  printf("pointers_to_sort[1] < pointers_to_sort[0] = %d\n", pointers_to_sort[1] < pointers_to_sort[0]);
  printf("is_less(pointers_to_sort[1], pointers_to_sort[0]) = %d\n", is_less(pointers_to_sort[1], pointers_to_sort[0]));

  heapsort(vals_to_sort, values_size);

  heapsort_p(pointers_to_sort, values_size);

  printf("{");
  for (int i = 0; i < values_size; i++) printf(" %.3f ", vals_to_sort[i]);
  printf("}\n");

  printf("index;value;address;point_to\n");
  for (int i = 0; i < values_size; i++) {
    printf("%2d;%10.3f;%p;%p\n", i, vals_to_sort[i], (void *) (vals_to_sort + i), pointers_to_sort[i]);
  }

  return 0;
}
