#include <stdio.h>
#include <stdlib.h>

#define ValType double
#define IS_LESS(v1, v2)  (v1 < v2)

void sift_down( ValType *a, int start, int count);

#define SWAP(r,s)  do{ValType t=r; r=s; s=t; } while(0)

void heapsort (ValType *a, int count)
{
  for (int start = (count - 2) / 2; start >= 0; start--) {
    sift_down(a, start, count);
  }
  for (int end = count - 1; end > 0; end--) {
    SWAP(a[end],a[0]);
    sift_down(a, 0, end);
  }
}

void sift_down (ValType *a, int start, int end)
{
  int root = start;
  while (root * 2 + 1 < end) {
    int child = 2 * root + 1;
    if ((child + 1 < end) && IS_LESS(a[child],a[child+1])) {
      child += 1;
    }
    if (IS_LESS(a[root], a[child])) {
      SWAP( a[child], a[root] );
      root = child;
    }
    else
      return;
  }
}


int main()
{
  double vals_to_sort[] = {
    1.4, 50.2, 5.11, -1.55, 301.521, 0.3301, 40.17,
    -18.0, 88.1, 30.44, -37.2, 3012.0, 49.2};

  const int values_size = sizeof(vals_to_sort) / sizeof(vals_to_sort[0]);
  void *pointers_to_sort[values_size];

  for (int i = 0; i < values_size; i++) {
    pointers_to_sort[i] = vals_to_sort + i;
  }

  printf("index;value;address\n");
  for (int i = 0; i < values_size; i++) {
    printf("%2d;%10.3f;%p\n", i, vals_to_sort[i], (void *) (vals_to_sort + i));
  }

  heapsort(vals_to_sort, values_size);

  printf("{");
  for (int i = 0; i < values_size; i++) printf(" %.3f ", vals_to_sort[i]);
  printf("}\n");

  return 0;
}
