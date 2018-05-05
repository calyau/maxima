//  Written by William Schelter.
//  Additional work from Davind Billinghurst and Andrej Vopodivec.
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+
//
//
//  Winkill is a program that tries to find the shared memory location maxima
//  offers on MS Windows in order to receive interrupt signals from a GUI.
// 

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

#define signal_mask(n)  (1 << (n))


/*
  Meant to resemble kill under unix.  Basic idea is that the
  process we want to kill, has a shared memory segment, and
  we write into it the flag of the signal we want to send.  That
  process has to frequently check that memory location.
  
  Sample USAGE:  winkill -SIGNAL ProcessID
  eg:
   C:> winkill -INT 243232  
 */


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
{	SIGIO, "IO" },	/* I/O now possible (4.2 BSD).  */
#endif
#ifdef 	SIGPWR
{	SIGPWR, "PWR" },	/* Power failure restart (System V).  */
#endif
#ifdef  SIGSYS
{ SIGSYS, "SYS" },
#endif
{ 0,0}
};

int ErrorHandler(char *s)
{
  fprintf(stderr,s);
  fflush(stderr);
  exit(1);
}

void close_shared_memory()
{
  if (sharedMemory.handle)
    CloseHandle(sharedMemory.handle);
  sharedMemory.handle = NULL;
  if (sharedMemory.address)
    UnmapViewOfFile(sharedMemory.address);
  sharedMemory.address = NULL;
}



int main(int argc, char *argv[])
{
  int sig=-1;
  int pid=-1;
  int value;
  int *at;
  char *in;  
  sigNameStruct *sigNamePtr = sigNames;
  
  if  (argc < 3 || argv[1][0] != '-') {
  USAGE:
    fprintf(stderr,"Sample usage: winkill -INT 232423, to interrupt the process 232423 ");
    {
      int i = 0;
      fprintf(stderr,"\nargv[1][0]=%c,%d",argv[1][0],argv[1][0]);
      fprintf(stderr,"\nCalled with: argc=%d <",argc);
      while (i < argc) fprintf(stderr, " %s",argv[i++]);
      fprintf(stderr,">\n");
    }
    exit(1);
  }

  /* Find which signal to send. */
  in = &(argv[1][1]);
  if (sscanf(&(argv[1][1]),"%d",&sig)==0) {
    while(sigNamePtr->name) {
      if (strcmp(sigNamePtr->name,in)==0) {
        sig = sigNamePtr->signumber;
        break;
      }
      sigNamePtr++;
    }
  }
  if (sig<0) {     
    fprintf(stderr,"winkill: Bad signal %s.", in);
    goto USAGE;
  }
  value = signal_mask(sig);

  /* Find the process pid. */
  if (sscanf(argv[2],"%d",&pid)!=1 ) {
    fprintf(stderr,"winkill: Bad pid %s.", argv[2]);
    goto USAGE;
  }

  /* First try to send the signal to gcl. */
  sprintf(sharedMemory.name,"gcl-%d", pid);
  sharedMemory.handle = OpenFileMapping(FILE_MAP_WRITE,     /*  Read/write permission.   */
                                        FALSE,              /*  Do not inherit the name  */
                                        sharedMemory.name); /*  of the mapping object.   */
 
 
  /* If gcl is not running, send to maxima. */
  if (sharedMemory.handle == NULL) {
    sprintf(sharedMemory.name,"maxima-%d", pid);
    sharedMemory.handle = OpenFileMapping(FILE_MAP_WRITE,     /*  Read/write permission.   */
                                          FALSE,              /*  Do not inherit the name  */
                                          sharedMemory.name); /*  of the mapping object.   */
  }

  if (sharedMemory.handle == NULL) { 
    printf("MEMORY: %s\n", sharedMemory.name);
    ErrorHandler("winkill: Could not open file-mapping object."); 
  } 
 
  sharedMemory.address = MapViewOfFile(sharedMemory.handle, /* Handle to mapping object.  */
                                       FILE_MAP_WRITE,      /* Read/write permission.  */
                                       0,                   /* Max.  object size.  */
                                       0,                   /* Size of hFile.  */
                                       0);                  /* Map entire file.  */
 
  if (sharedMemory.address == NULL) { 
    ErrorHandler("winkill: Could not map view of file."); 
  }
  
  at = (int *)(sharedMemory.address);
  *at |= value;
  close_shared_memory();

  return 0;
}
