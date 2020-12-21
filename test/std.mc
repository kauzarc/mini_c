int power(int n, int p)
{
    int result = 1;
    while(p > 0)
    {
        if(p % 2 == 0)
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
    for(int i = 100000000; i > 1; i = i / 10)
    {
        int current_digit = (n - (n % i)) / i;
        n = n - i * current_digit;
        putchar(current_digit + 48);
    }
    putchar(10);
}