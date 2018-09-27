#include<stdio.h>

int foo(int a, int b) {
    int z = 3;
    if (a > b)
        z = a;
    else
        z = b;
    int u = z;
    return u;
}
