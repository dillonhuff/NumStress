int test() {
  int i, x, j;
  x = 12;
  for (i = 0; i < 15; i++) {
    j = j + x / (i - x);
  }
  return j;
}
