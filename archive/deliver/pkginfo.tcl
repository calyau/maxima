
# Create some images used in the installation program
catch {
image create photo img_logo -file ./src/maxicon.gif
image create photo img_license -file ./deliver/install1.gif
image create photo img_logo3 -file ./deliver/fdlogo3.gif
}

# Global variables
global pkgDesc
array set pkgDesc {maxima {All Maxima files will be put under the directory you choose, except for an optional button on the start/programs menu.  You should quit any programs,
during installation }}
global pkgBaseDir
array set pkgBaseDir {maxima .}
global pkgDefDir
array set pkgDefDir {maxima c:/maxima Documentation {} {freeDelivery program} {} {Build files} {}}
# array set pkgDefDir {maxima h:/wfs2xx/tmp/xmaxima7 Documentation {} {freeDelivery program} {} {Build files} {}}
global pkgAbsDir
array set pkgAbsDir {maxima {}}
global pkgUseDir
array set pkgUseDir {maxima DefDir}
global pkgDefault
array set pkgDefault {maxima 1 Documentation 0 {freeDelivery program} 0 {Build files} 0}
global pkgFiles
array set pkgFiles {maxima {./intro.html ./gcc.zip ./maxima.zip ./readme.xmaxima ./tclpip83.dll  ./unzip.exe}}
global pkgFileSpace
array set pkgFileSpace {maxima 4102762}
global settings_appName
set settings_appName {maxima}
global settings_appSlogan
set settings_appSlogan {}
global settings_appVersion
set settings_appVersion {5.5}
global settings_appAuthor
set settings_appAuthor {William Schelter}
global settings_CopyrightFile
set settings_CopyrightFile {./COPYING}
global settings_InfoFile
set settings_InfoFile {./readme.xmaxima}
global settings_runscript
set settings_runscript {./deliver/unpack.tcl}
global runscript
set runscript {1}
global settings_runbin
set settings_runbin {}
global runbin
set runbin {0}
global settings_appSetupFile
set settings_appSetupFile {setup.exe}
global settings_LogoFile
set settings_LogoFile {./src/maxicon.gif}
global pkg_test
set pkg_test 0
global viewReadme
set viewReadme 1
global abortNow
set abortNow 0
global replaceAll
set replaceAll 0
global replaceCur
set replaceCur 0

