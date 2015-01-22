#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <windows.h>

#define signal_mask(n)  (1 << (n))

static struct {
  HANDLE handle;
  LPVOID address;
  DWORD length ;
  char name[20] ;
} sharedMemory = {0,0,0x10000} ;



typedef struct {int signumber; char *name ;} sigNameStruct;
sigNameStruct sigNames[]=
{
#ifdef 	SIGHUP
{	SIGHUP, "HUP" },	/* Hangup (POSIX).  */
#endif
#ifdef 	SIGINT
{	SIGINT, "INT" },	/* Interrupt (ANSI).  */
#endif
#ifdef 	SIGQUIT
{	SIGQUIT, "QUIT" },	/* Quit (POSIX).  */
#endif
#ifdef 	SIGILL
{	SIGILL, "ILL" },	/* Illegal instruction (ANSI).  */
#endif
#ifdef 	SIGTRAP
{	SIGTRAP, "TRAP" },	/* Trace trap (POSIX).  */
#endif
#ifdef 	SIGABRT
{	SIGABRT, "ABRT" },	/* Abort (ANSI).  */
#endif
#ifdef 	SIGIOT
{	SIGIOT, "IOT" },	/* IOT trap (4.2 BSD).  */
#endif
#ifdef 	SIGBUS
{	SIGBUS, "BUS" },	/* BUS error (4.2 BSD).  */
#endif
#ifdef 	SIGFPE
{	SIGFPE, "FPE" },	/* Floating-point exception (ANSI).  */
#endif
#ifdef 	SIGKILL
{	SIGKILL, "KILL" },	/* Kill, unblockable (POSIX).  */
#endif
#ifdef 	SIGUSR1
{	SIGUSR1, "USR1" },	/* User-defined signal 1 (POSIX).  */
#endif
#ifdef 	SIGSEGV
{	SIGSEGV, "SEGV" },	/* Segmentation violation (ANSI).  */
#endif
#ifdef 	SIGUSR2
{	SIGUSR2, "USR2" },	/* User-defined signal 2 (POSIX).  */
#endif
#ifdef 	SIGPIPE
{	SIGPIPE, "PIPE" },	/* Broken pipe (POSIX).  */
#endif
#ifdef 	SIGALRM
{	SIGALRM, "ALRM" },	/* Alarm clock (POSIX).  */
#endif
#ifdef 	SIGTERM
{	SIGTERM, "TERM" },	/* Termination (ANSI).  */
#endif
#ifdef 	SIGSTKFLT
{	SIGSTKFLT, "STKFLT" },	/* Stack fault.  */
#endif
#ifdef 	SIGCLD
{	SIGCLD, "CLD" },	/* Same as SIGCHLD (System V).  */
#endif
#ifdef 	SIGCHLD
{	SIGCHLD, "CHLD" },	/* Child status has changed (POSIX).  */
#endif
#ifdef 	SIGCONT
{	SIGCONT, "CONT" },	/* Continue (POSIX).  */
#endif
#ifdef 	SIGSTOP
{	SIGSTOP, "STOP" },	/* Stop, unblockable (POSIX).  */
#endif
#ifdef 	SIGTSTP
{	SIGTSTP, "TSTP" },	/* Keyboard stop (POSIX).  */
#endif
#ifdef 	SIGTTIN
{	SIGTTIN, "TTIN" },	/* Background read from tty (POSIX).  */
#endif
#ifdef 	SIGTTOU
{	SIGTTOU, "TTOU" },	/* Background write to tty (POSIX).  */
#endif
#ifdef 	SIGURG
{	SIGURG, "URG" },	/* Urgent condition on socket (4.2 BSD).  */
#endif
#ifdef 	SIGXCPU
{	SIGXCPU, "XCPU" },	/* CPU limit exceeded (4.2 BSD).  */
#endif
#ifdef 	SIGXFSZ
{	SIGXFSZ, "XFSZ" },	/* File size limit exceeded (4.2 BSD).  */
#endif
#ifdef 	SIGVTALRM
{	SIGVTALRM, "VTALRM" },	/* Virtual alarm clock (4.2 BSD).  */
#endif
#ifdef 	SIGPROF
{	SIGPROF, "PROF" },	/* Profiling alarm clock (4.2 BSD).  */
#endif
#ifdef 	SIGWINCH
{	SIGWINCH, "WINCH" },	/* Window size change (4.3 BSD, Sun).  */
#endif
#ifdef 	SIGPOLL
{	SIGPOLL, "POLL" },	/* Pollable event occurred (System V).  */
#endif
#ifdef 	SIGIO
{	SIGIO, "IO" },          /* I/O now possible (4.2 BSD).  */
#endif
#ifdef 	SIGPWR
{	SIGPWR, "PWR" },	/* Power failure restart (System V).  */
#endif
#ifdef  SIGSYS
{ SIGSYS, "SYS" },
#endif
{ 0,0}
};

void close_shared_memory()
{
  if (sharedMemory.handle)
    CloseHandle(sharedMemory.handle);
  sharedMemory.handle = NULL;
  if (sharedMemory.address)
    UnmapViewOfFile(sharedMemory.address);
  sharedMemory.address = NULL;
}

int is_shared_memory_initialised = FALSE;

int check_shared_memory()
{
  return is_shared_memory_initialised;
}

void print_shared_memory_name()
{
  if (is_shared_memory_initialised)
    puts(sharedMemory.name);
}

void init_shared_memory (void)
{
  if ( ! is_shared_memory_initialised ) {
    sprintf ( sharedMemory.name, "maxima-%d", getpid());

    is_shared_memory_initialised = TRUE;

    sharedMemory.handle =
      CreateFileMapping ( (HANDLE)-1,
                          NULL,
                          PAGE_READWRITE,
                          0,
                          sharedMemory.length,
                          TEXT (sharedMemory.name) );

    if ( sharedMemory.handle == NULL ) {
      is_shared_memory_initialised = FALSE;
    }

    sharedMemory.address =
      MapViewOfFile(sharedMemory.handle, /* Handle to mapping object.  */
                    FILE_MAP_WRITE,      /* Read/write permission */
                    0,                   /* Max.  object size.  */
                    0,                   /* Size of hFile.  */
                    0);                  /* Map entire file.  */

    if ( sharedMemory.address == NULL ) {
      is_shared_memory_initialised = FALSE;
    }

    atexit ( close_shared_memory );
  }
}

int read_shared_memory(void)
{
  int *at;
  
  if (! is_shared_memory_initialised )
    return 0;
  
  at = (int *)(sharedMemory.address);
  
  if (*at & signal_mask(SIGINT))
    return 1;
  if (*at & signal_mask(SIGTERM))
    return 2;
  return 0;
}

int read_sm_sigterm(void)
{
  int *at;
  
  if (! is_shared_memory_initialised )
    return 0;
  
  at = (int *)(sharedMemory.address);
  
  if (*at & signal_mask(SIGTERM))
    return 1;
  
  return 0;
}

int read_sm_sigint(void)
{
  int *at;
  
  if (! is_shared_memory_initialised )
    return 0;
  
  at = (int *)(sharedMemory.address);
  
  if (*at & signal_mask(SIGINT))
    return 1;
  
  return 0;
}

void reset_shared_memory(void)
{
  int *at;
  
  if (! is_shared_memory_initialised )
    return ;
  
  at = (int *)(sharedMemory.address);
  
  *at = 0;
}

void reset_sm_sigint(void)
{
  int *at;
  
  if (! is_shared_memory_initialised )
    return ;
  
  at = (int *)(sharedMemory.address);
  
  *at &= ~signal_mask(SIGINT);
}

void reset_sm_sigterm(void)
{
  int *at;
  
  if (! is_shared_memory_initialised )
    return ;
  
  at = (int *)(sharedMemory.address);
  
  *at &= ~signal_mask(SIGTERM);
}
