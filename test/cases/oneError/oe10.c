int test(int a, int b) {
  int x = 1 / a;
  int i, j;
  for (i = 0; i < b; i++) {
    j = j + x;
  }
  return j;
}
