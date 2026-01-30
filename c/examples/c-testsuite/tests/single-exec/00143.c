/* Disgusting, no?  But it compiles and runs just fine.  I feel a combination of
   pride and revulsion at this discovery.  If no one's thought of it before,
   I think I'll name it after myself.
   It amazes me that after 10 years of writing C there are still
   little corners that I haven't explored fully.
   - Tom Duff */

int main()
{
    int  count, n;
    short *from, *to;
    short a[39], b[39];

    for(n = 0; n < 39; n++) {
        a[n] = n;
        b[n] = 0;
    }
    from = a;
    to = b;
    count = 39;
    n = (count + 7) / 8;
    switch (count % 8) {
    case 0: do { *to++ = *from++;
    case 7:      *to++ = *from++;
    case 6:      *to++ = *from++;
    case 5:      *to++ = *from++;
    case 4:      *to++ = *from++;
    case 3:      *to++ = *from++;
    case 2:      *to++ = *from++;
    case 1:      *to++ = *from++;
            } while (--n > 0);
    }
    for(n = 0; n < 39; n++)
        if(a[n] != b[n])
            return 1;
    return 0;
}