; -*-mode: text; fill-column: 75; tab-width: 8; coding: iso-latin-1-dos -*-
;
; $Id: xmaxima.iss,v 1.2 2002-09-19 17:28:42 mikeclarkson Exp $
;

[Setup]
AppName=Maxima
AppVerName=Maxima 5.9.0cvs
AppPublisher=The Maxima Development Team
AppPublisherURL=http://maxima.sourceforge.net
AppSupportURL=http://maxima.sourceforge.net
AppUpdatesURL=http://maxima.sourceforge.net
DefaultDirName=C:\Programs\maxima-5.9
DisableDirPage=yes
DefaultGroupName=maxima-5.9
AllowNoIcons=yes
AlwaysCreateUninstallIcon=yes
LicenseFile=C:\Programs\maxima-5.9\COPYING.txt
InfoBeforeFile=C:\Programs\maxima-5.9\COPYING1.txt
Uninstallable=yes
UninstallFilesDir=C:\Programs\maxima-5.9\uninst
; uncomment the following line if you want your installation to run on NT 3.51 too.
; MinVersion=4,3.51

[Tasks]
Name: "desktopicon"; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"; MinVersion: 4,4

[Files]
Source: "C:\Programs\maxima-5.9\xmaxima\xmaxima.exe"; DestDir: "C:\Programs\maxima-5.9\xmaxima\"; CopyMode: alwaysoverwrite
Source: "C:\Programs\maxima-5.9\*.*"; DestDir: "C:\Programs\maxima-5.9\";  Flags: recursesubdirs

[Icons]
Name: "{group}\Maxima"; Filename: "C:\Programs\maxima-5.9\xmaxima\xmaxima.exe"
Name: "{group}\Maxima"; Filename: "C:\Programs\maxima-5.9\share\maxima\5.9.0rc1\doc\html\maxima_toc.html"
Name: "{userdesktop}\Maxima"; Filename: "C:\Programs\maxima-5.9\xmaxima\xmaxima.exe"; MinVersion: 4,4; Tasks: desktopicon

[Run]
Filename: "C:\Programs\maxima-5.9\xmaxima\xmaxima.exe"; Description: "Launch Maxima"; Flags: shellexec postinstall skipifsilent

