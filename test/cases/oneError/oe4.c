long int test(long int a, long int b) {
  long int x = a - b*b;
  long int y = x + 3*b;
  long int k = x / (y - 1);
  return k;
}
