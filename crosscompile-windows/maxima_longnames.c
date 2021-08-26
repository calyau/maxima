/*
SPDX-License-Identifier: GPL-2.0-or-later
    Clisp has problems with generated Windows 'short names' e.g.
    echo %TMP%
    C:\Users\TESTTE~1\AppData\Local\Temp
    Windows generates a short name for the temporary dir, when the
    username contains spaces and is longer than 8 chars.
    This program converts that path name to the 'long version', so that Clisp has
    no problems with long user names and plotting commands.
*/
#include <windows.h>
#include <tchar.h>
#include <stdio.h>

#define BUFSIZE 4096

int main(int argc, char *argv[])
{
    DWORD  retval=0;
    TCHAR  buffer[BUFSIZE];

   if( argc != 2 )
   {
      _tprintf(TEXT("Usage: maxima_longnames [filename]\n\n"));
      _tprintf(TEXT("This program converts Windows generated short filenames (ending with \"~\"\n"));
      _tprintf(TEXT("and a number (8.3 format for compatibility with old Windows/DOS versions))\n"));
      _tprintf(TEXT("back to the long filename (The short name cause problems with CLISP.)\n"));
      return 1;
   }

// Retrieve the long path name.  

    retval = GetLongPathName(argv[1],
              buffer,
              BUFSIZE);

    if (retval == 0) 
    {
        // Handle an error condition.
         printf ("GetLongPathName failed (%d)\n", GetLastError());
         return 1;
    }
    else printf("%s", buffer);
    return 0;
}


