#include<stdio.h>

enum week{Mon, Tue, Wed, Thur, Fri, Sat, Sun};

enum week foo;

int bar(int z)
{
  int x = z;
  foo = x;
  return foo;
}

int main() {
  return 1;
}
