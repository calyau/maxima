This is the windows README file.


Binary files included with this distribution
--------------------------------------------

The Windows version package of Maxima includes binary files 
from other Open Source projects also hosted on Sourceforge.

gcc: 

gcc.exe, cc1.exe and the files in lib/gcc-lib and include/ 
subdirectories are from the mingw version of gcc.  This is
available from http://prdownloads.sf.net/mingw/
     

binutils:

as.exe is from the mingw (http://www.mingw.org/) port of binutils
available from http://prdownloads.sf.net/mingw/


gnuplot:

The files wgnuplot.exe, wgnuplot.hlp and wgnuplot.mnu are from the
Windows distribution of gnuplot from http://gnuplot.sourceforge.net


wxMaxima:

The files in the wxMaxima subdirectory are from the Windows distribution
of wxMaxima available from https://wxmaxima-developers.github.io/wxmaxima/


Maxima GUI and firewall
-----------------------

Sometimes Maxima GUI (xmaxima or wxMaxima) can't launch Maxima
or issues timeout message or gets no response for Maxima commands.  
Quite probably the problem is caused by firewall and/or antivirus software.  
The Maxima GUI talks to the computational engine through a socket.  
Antivirus and/or firewall programs see that and may try to block it 
(because some malicious programs open sockets too).  

To resolve the problem:

1.  Try to find the control panel for the antivirus and/or firewall. 

2.  Find the Maxima GUI on the list of blocked programs and disable 
    blocking for it.  The GUI program might appear as "Tcl/Tk" 
    (the name of the GUI toolkit for xmaxima).


Data Execution Prevention (DEP)
-------------------------------
Sometimes not only Maxima GUI but also command line Maxima
doesn't work (maxima.bat starts and immediately quits).
Quite probably the problem is caused by Windows DEP.
Some Lisp implementations execute code in data areas of memory. 
Windows DEP sees that and blocks it (because some malicious programs 
execute code in data areas too).

Solution:  

  Include the full program path of the Maxima executable, for example: 
  C:\Program Files\Maxima-5.12.0\lib\maxima\5.12.0\binary-gcl\maxima.exe 
  in the list of DEP exceptions 
  (Control Panel -> System -> Advanced -> Performance -> DEP)

