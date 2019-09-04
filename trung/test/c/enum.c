#include<stdio.h>

enum week{Mon, Tue, Wed, Thur, Fri, Sat, Sun};

enum week foo;

int bar(enum week z)
{
    enum week day;
    day = z;
    foo = Fri;
    printf("%d",day);
    return 0;
}

int main() {
  return 1;
}
