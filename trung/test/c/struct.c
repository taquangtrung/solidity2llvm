struct Student {
    int age;
    int class;
};

struct Student an = {10, 3};

int main() {

    struct Student bn = {20, 5};
    bn.age = 1;
    bn.class = 10;
    return 1;
}
