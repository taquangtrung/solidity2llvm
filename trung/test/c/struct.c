struct Student {
    int age;
    int class;
};

struct Student an = {10, 3};

struct Student foo(struct Student a, int b) {
  a.age = 10;
  return a;
}

int bar() {
  return 1;
}

int main() {

    struct Student bn = {20, 5};
    bn.age = 1;
    bn.class = 10;
    struct Student c = foo(bn, 2);
    int a = c.class;
    int z = bar();
    return 1;
}
