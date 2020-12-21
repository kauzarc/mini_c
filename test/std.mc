int power(int n, int p)
{
    if (p == 0)
    {
        return 1;
    }

    if (p == 1)
    {
        return n;
    }

    int rec = power(n, p / 2);
    if (p % 2 == 1)
    {
        return rec * rec * n;
    }
    else
    {
        return rec * rec;
    }
}

void print_int(int n)
{
    bool b = true;
    for (int i = 10000000000; i > 1; i = i / 10)
    {
        int d = n - (n % i);
        n = n - d;
        if (!(d == 0 && b))
        {
            b = false;
            putchar(d / i + 48);
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