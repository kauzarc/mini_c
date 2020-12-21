#include "./std.mc"

int PARAM = 8;

int fact(int n)
{
    int result = 1;
    for (int i = 1; i < n + 1; i = i + 1)
    {
        result = result * i;
    }
    return result;
}

void main()
{
    print_int(fact(PARAM));
}