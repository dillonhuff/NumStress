int test() {
  int i, x, j;
  x = 12;
  for (i = 0; i < 0; i++) {
    j = j + x / (x - i);
  }
  return j;
}
