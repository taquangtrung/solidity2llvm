struct foo {
    int value;
    int data;
};

int main() {
    int x = 10;
    if (x > 1)
        x = x + x;
    else
        x = x + x;
    x = x + 2;
    return 1;
}
