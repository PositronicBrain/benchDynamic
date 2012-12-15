#include <malloc.h>
#include <stdlib.h>


int fib (int n) {
  int i;
  int *fs;
  fs = (int*) malloc ((n+1) * sizeof(int));
  if (fs == NULL) {
    printf("malloc failed");
    exit(1);
  }
  fs[0] = 1;
  fs[1] = 1;
  for (i=2;i<=n;i++) {
    fs[i] = fs [i-1] + fs [i-2];
  }
  i = fs[n];
  free (fs);
  return(i);
}
