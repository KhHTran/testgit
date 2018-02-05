#include<stdio.h>
typedef int* TRIARR;

TRIARR triNew(int n)
{
    int b[n*n];
    int* p = b;
    return p;
}

int triStore(TRIARR as, int n, int row, int col, int val)
{
    if(row>col||row>n||col>n)
        return -1;
    *(as+(row-1)*n+col-1) = val;
    return 0;
}

int triFetch(TRIARR as, int n, int row, int col)
{
    int k =*(as+(row-1)*n+col-1);
    if(k==NULL)
        k = -1;
    return k;
}

int main()
{
    int n = 5;
    TRIARR as = triNew(n);
    triStore(as,n,2,3,4);
    int k = triFetch(as,n,2,4);
    printf("%d\n",k);
}
