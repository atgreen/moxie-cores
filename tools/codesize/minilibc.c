/* minilibc.c - the C library functions Embench needs, written once so
   that every target links the same code.  */
#include <stddef.h>
void *memcpy (void *d, const void *s, size_t n)
{ char *p = d; const char *q = s; while (n--) *p++ = *q++; return d; }
void *memmove (void *d, const void *s, size_t n)
{ char *p = d; const char *q = s;
  if (p < q) while (n--) *p++ = *q++; else while (n--) p[n] = q[n];
  return d; }
void *memset (void *d, int c, size_t n)
{ char *p = d; while (n--) *p++ = c; return d; }
int memcmp (const void *a, const void *b, size_t n)
{ const unsigned char *p = a, *q = b;
  for (; n; n--, p++, q++) if (*p != *q) return *p - *q;
  return 0; }
size_t strlen (const char *s) { const char *p = s; while (*p) p++; return p - s; }
char *strchr (const char *s, int c)
{ for (;; s++) { if (*s == (char) c) return (char *) s; if (!*s) return 0; } }
double sqrt (double x)
{ double r = x > 1 ? x : 1; if (x <= 0) return 0;
  for (int i = 0; i < 40; i++) r = (r + x / r) / 2; return r; }
/* newlib's ctype table: 1 + 256 entries.  */
#define U 01
#define L 02
#define N 04
#define S 010
#define P 020
#define C 040
#define X 0100
#define B 0200
const char _ctype_[1 + 256] = { 0,
  C,C,C,C,C,C,C,C, C,C|S,C|S,C|S,C|S,C|S,C,C, C,C,C,C,C,C,C,C, C,C,C,C,C,C,C,C,
  S|B,P,P,P,P,P,P,P, P,P,P,P,P,P,P,P, N,N,N,N,N,N,N,N, N,N,P,P,P,P,P,P,
  P,U|X,U|X,U|X,U|X,U|X,U|X,U, U,U,U,U,U,U,U,U, U,U,U,U,U,U,U,U, U,U,U,P,P,P,P,P,
  P,L|X,L|X,L|X,L|X,L|X,L|X,L, L,L,L,L,L,L,L,L, L,L,L,L,L,L,L,L, L,L,L,P,P,P,P,C };
