int power(int n, int p)
{
    int result = 1;
    while (p > 0)
    {
        if (p % 2 == 0)
        {
            p = p / 2;
            n = n * n;
        }
        else
        {
            p = p - 1;
            result = result * n;
            n = n * n;
        }
    }
    return result;
}

void print_int(int n)
{
    bool b = true;
    for (int i = 10000000000; i > 1; i = i / 10)
    {
        int d = (n - (n % i)) / i;
        n = n - (d * i);
        if (!(d == 0 && b))
        {
            putchar(d + 48);
        }
    }
    putchar(n + 48);
    putchar(10);
}

void print_bool(bool b)
{
    if (b)
    {
        putchar(116);
        putchar(114);
        putchar(117);
        putchar(101);
    }
    else
    {
        putchar(102);
        putchar(97);
        putchar(108);
        putchar(115);
        putchar(101);
    }
    putchar(10);
}

void print_hello_world()
{
    putchar(104);
    putchar(101);
    putchar(108);
    putchar(108);
    putchar(111);
    putchar(32);
    putchar(119);
    putchar(111);
    putchar(114);
    putchar(108);
    putchar(100);
    putchar(32);
    putchar(33);
}