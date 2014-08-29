#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int *initArrayT(int size, int init)
{
    int i;
    int *a = malloc(size*sizeof(int));
    for(i=0;i<size;i++) 
        a[i]=init;
    return a;
}

int *allocRecordT(int size)
{
    int i;
    int *p, *a;
    p = a = malloc(size);
    for(i = 0; i < size; i += sizeof(int)) 
        *p++ = 0;
    
    return a;
}

struct string {
    int length; 
    unsigned char chars[1];
};

struct string consts[256];
struct string empty={0,""};

int stringEqualT(struct string *s, struct string *t)
{
    int i;
    if (s==t) 
        return 1;
    if (s->length!=t->length) 
        return 0;
    for(i=0;i<s->length;i++) 
        if (s->chars[i]!=t->chars[i]) 
            return 0;
    return 1;
}

void printT(struct string *s)
{
    int i; 
    unsigned char *p=s->chars;
    for(i=0;i<s->length;i++,p++) 
        putchar(*p);
}

void flushT()
{
    fflush(stdout);
}


int main()
{
    int i;
    for(i=0;i<256;i++)
    {
        consts[i].length=1;
        consts[i].chars[0]=i;
    }
    return tigermain(0 /* static link!? */);
}

int ordT(struct string *s)
{
    if (s->length==0) 
        return -1;
    else 
        return s->chars[0];
}

struct string *chrT(int i)
{
    if (i<0 || i>=256) 
    {
        printf("chr(%d) out of range\n",i); 
        exit(1);
    }
    return consts+i;
}

int sizeT(struct string *s)
{ 
    return s->length;
}

struct string *substringT(struct string *s, int first, int n)
{
    if (first<0 || first+n>s->length)
    {
        printf("substring([%d],%d,%d) out of range\n",s->length,first,n);
        exit(1);
    }
    if (n==1) 
        return consts+s->chars[first];

    struct string *t = malloc(sizeof(int)+n);
    int i;
    t->length=n;
    for(i=0;i<n;i++) 
        t->chars[i]=s->chars[first+i];
    return t;
}

struct string *concatT(struct string *a, struct string *b)
{
    if (a->length==0) 
        return b;
    else if (b->length==0) 
        return a;
    else 
    {
        int i, n=a->length+b->length;
        struct string *t = malloc(sizeof(int)+n);
        t->length=n;
        for (i=0;i<a->length;i++)
            t->chars[i]=a->chars[i];
        for(i=0;i<b->length;i++)
            t->chars[i+a->length]=b->chars[i];
        return t;
    }
}

int notT(int i)
{
    return !i;
}


struct string *getcharT()
{
    int i=getc(stdin);
    if (i==EOF) 
        return &empty;
    else 
        return consts+i;
}

struct string* itoaT(int num)
{
    char *str = malloc(sizeof(char)*32);
    struct string *t;
    int len;
    int i;

    sprintf(str, "%d", num);

    len = strlen(str);

    t = malloc(len);
    t->length = len;
    for(i = 0; i < len; i++)
        t->chars[i] = str[i];

    return t;
}

struct string* getStringT()
{
    char str[80];
    struct string *t;
    int len;
    int i;

    fgets(str,80,stdin);
    len = strlen(str) - 1; /* remove new line */

    t = malloc(sizeof(int) + sizeof(char) * len);
    t->length = len;
    for(i = 0; i < t->length; i++)
        t->chars[i] = str[i];

    return t;
}


int strToIntT(struct string *str)
{
    int len = str->length;
    int i;
    int num = 0;

    for(i = 0; i < str->length; i++)
    {
        num *= 10;
        num += str->chars[i] - '0';
    }

    return num;

}


