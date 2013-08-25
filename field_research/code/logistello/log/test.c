// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2



inline void foo(int &isu, register int *seeds, register int *hash, register int *tab, register int i1, register int i2)
{
  if (seeds[i1] == 0) return;
  if (hash[seeds[i1]+i2] != i1) return;
  isu += tab[hash[seeds[i1]+i2+1]];
}

int v;

int *hash;
int *seeds;
int *tab;
int i1, i2, i3, i4;

int main() 
{
  register int isu asm("ebx");

  foo(isu, seeds, hash, tab, i1, i2);
  foo(isu, seeds, hash, tab, i3, i4);

  return isu;
}
  
