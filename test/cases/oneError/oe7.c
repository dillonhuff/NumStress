int test(int a, int b) {
  int i;
  int x = 32;
  a = 0;
  for (i = 0; i < b; i++) {
    a += a / (x - i);
  }
  return a;
}

int test2(int* a, char** x) {
  if (x[*a][3] == 'x') {
    *a = 12;
  }
  *a = *a + 3;
  return *a;
}
