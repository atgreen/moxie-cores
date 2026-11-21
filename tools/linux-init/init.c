/* init.c - a first user program for Linux on mox125.

   There is no C library for Moxie Linux yet, so this talks to the
   kernel directly: the system call number goes in $r12, the arguments
   in $r0 to $r5, and "swi 0" enters the kernel.  The result comes back
   in $r0.  */

#define __NR_write	64
#define __NR_exit	93
#define __NR_waitid	95
#define __NR_kill	129
#define __NR_rt_sigaction	134
#define __NR_reboot	142
#define __NR_uname	160
#define __NR_getpid	172
#define __NR_clone	220

#define SIGUSR1		10
#define SIGCHLD		17
#define SA_SIGINFO	4
#define P_PID		1
#define WEXITED		4

static long
syscall5 (long nr, long a, long b, long c, long d, long e)
{
  register long r0 __asm__ ("$r0") = a;
  register long r1 __asm__ ("$r1") = b;
  register long r2 __asm__ ("$r2") = c;
  register long r3 __asm__ ("$r3") = d;
  register long r4 __asm__ ("$r4") = e;
  register long r12 __asm__ ("$r12") = nr;

  __asm__ volatile ("swi 0"
		    : "+r" (r0)
		    : "r" (r1), "r" (r2), "r" (r3), "r" (r4), "r" (r12)
		    : "memory");
  return r0;
}

static long
syscall3 (long nr, long a, long b, long c)
{
  return syscall5 (nr, a, b, c, 0, 0);
}

static unsigned long
length (const char *s)
{
  unsigned long n = 0;
  while (s[n])
    n++;
  return n;
}

static void
print (const char *s)
{
  syscall3 (__NR_write, 1, (long) s, length (s));
}

static void
print_number (unsigned long n)
{
  char buf[12];
  char *p = buf + sizeof buf;

  *--p = 0;
  do
    *--p = '0' + n % 10;
  while (n /= 10);
  print (p);
}

/* Enough of struct utsname: six fields of 65 bytes.  */
static char uts[6 * 65];

/* Something for the kernel to have to map on demand.  */
static char scratch[64 * 1024];

/* A siginfo: signal, errno, code, then pid, uid and status.  */
static int info[32];

/* What the kernel takes for a signal action.  */
struct kernel_sigaction
{
  void (*handler) (int, int *, void *);
  unsigned long flags;
  unsigned long mask[2];
};

static volatile int caught, caught_code;

static void
on_signal (int sig, int *siginfo, void *context)
{
  caught = sig;
  caught_code = siginfo[0];
  print ("Handler: running.\n");
}

int
main (void)
{
  struct kernel_sigaction act = { on_signal, SA_SIGINFO, { 0, 0 } };
  long pid, i;
  unsigned long sum = 0;

  print ("\nHello from user space on mox125!\n");

  if (syscall3 (__NR_uname, (long) uts, 0, 0) == 0)
    {
      print (uts);
      print (" ");
      print (uts + 2 * 65);
      print (" on ");
      print (uts + 4 * 65);
      print ("\n");
    }

  print ("I am process ");
  print_number (syscall3 (__NR_getpid, 0, 0, 0));
  print (".\n");

  /* Touch a page at a time, so each one faults in.  */
  for (i = 0; i < (long) sizeof scratch; i += 4096)
    scratch[i] = i >> 12;
  for (i = 0; i < (long) sizeof scratch; i += 4096)
    sum += scratch[i];
  print ("Touched 16 pages of bss, checksum ");
  print_number (sum);
  print (" (expecting 120).\n");

  syscall5 (__NR_rt_sigaction, SIGUSR1, (long) &act, 0, 8, 0);
  syscall3 (__NR_kill, syscall3 (__NR_getpid, 0, 0, 0), SIGUSR1, 0);
  print ("Caught signal ");
  print_number (caught);
  print (", siginfo says ");
  print_number (caught_code);
  print (" (expecting 10 both times).\n");

  pid = syscall3 (__NR_clone, SIGCHLD, 0, 0);
  if (pid == 0)
    {
      print ("Child: I am process ");
      print_number (syscall3 (__NR_getpid, 0, 0, 0));
      print (", exiting with status 42.\n");
      syscall3 (__NR_exit, 42, 0, 0);
    }
  else if (pid > 0)
    {
      print ("Parent: forked process ");
      print_number (pid);
      print (".\n");
      if (syscall5 (__NR_waitid, P_PID, pid, (long) info, WEXITED, 0) == 0)
	{
	  print ("Parent: child ");
	  print_number (info[3]);
	  print (" exited with status ");
	  print_number (info[5]);
	  print (".\n");
	}
      else
	print ("Parent: waitid failed.\n");
    }
  else
    print ("fork failed.\n");

  print ("Powering off.\n");
  /* LINUX_REBOOT_MAGIC1, MAGIC2, CMD_POWER_OFF */
  syscall5 (__NR_reboot, 0xfee1dead, 672274793, 0x4321fedc, 0, 0);
  for (;;)
    ;
}

/* The kernel starts us with $sp at the argument count.  The ABI wants
   every caller to leave room for six argument registers.  */
__asm__ (".text\n"
	 ".global _start\n"
	 "_start:\n"
	 "\txor\t$fp, $fp\n"
	 "\tdec\t$sp, 24\n"
	 "\tjsra\tmain\n"
	 "1:\tjmpa\t1b\n");
