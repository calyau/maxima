# This distribution program created by freeDelivery 1.1 on Wed Nov 22 21:09:35 Pacific Standard Time 2000

proc ShowWindow.welcome {args} {
# xf ignore me 7

  # build widget .welcome
  if {"[info procs XFEdit]" != ""} {
    catch "XFDestroy .welcome"
  } {
    catch "destroy .welcome"
  }
  toplevel .welcome 

  # Window manager configurations
  wm positionfrom .welcome program
  wm sizefrom .welcome program
  wm maxsize .welcome 613 252
  wm minsize .welcome 613 252
  wm protocol .welcome WM_DELETE_WINDOW {pkg_exit welcome}
  wm title .welcome {Welcome}


  # build widget .welcome.frame4
  frame .welcome.frame4  -height {30}  -width {30}

  # build widget .welcome.frame4.frame4
  frame .welcome.frame4.frame4  -height {30}  -width {30}

  # build widget .welcome.frame4.frame4.message10
  message .welcome.frame4.frame4.message10  -aspect {1500}  -borderwidth {0}  -font {{Helv} 10}  -padx {5}  -pady {2}  -text {Installation will only take a few minutes.

You can also run Setup at another time to
modify your installation.

Click Next to proceed with Setup or click Cancel to exit
setup.}

  # build widget .welcome.frame4.frame4.frame0
  frame .welcome.frame4.frame4.frame0  -height {25}  -width {233}

  # build widget .welcome.frame4.frame4.frame0.label1
  label .welcome.frame4.frame4.frame0.label1  -borderwidth {0}  -font {Helvetica 10}  -text {Welcome to }

  # build widget .welcome.frame4.frame4.frame0.label2
  label .welcome.frame4.frame4.frame0.label2  -borderwidth {0}  -font {Helvetica 12 bold}  -foreground {darkblue}  -text {freeDelivery}  -textvariable {settings_appName}

  # build widget .welcome.frame4.frame4.frame0.label3
  label .welcome.frame4.frame4.frame0.label3  -borderwidth {0}  -font {Helvetica 10}  -text { Setup!}

  # build widget .welcome.frame4.frame1
  frame .welcome.frame4.frame1  -background {grey50}  -borderwidth {2}  -height {153}  -relief {sunken}  -width {117}

  # build widget .welcome.frame4.frame1.label3
  label .welcome.frame4.frame1.label3  -background {grey50}  -borderwidth {0}  -font {Helvetica 10}  -image {img_logo}  -text {label3}

  # build widget .welcome.frame5
  frame .welcome.frame5  -borderwidth {2}  -height {2}  -relief {sunken}

  # build widget .welcome.frame6
  frame .welcome.frame6  -height {30}  -width {30}

  # build widget .welcome.frame6.button11
  button .welcome.frame6.button11  -borderwidth {1}  -command {pkg_exit welcome}  -font {Helvetica 10}  -pady {0}  -text {Cancel}  -width {10}

  button .welcome.frame6.button11a  -borderwidth {1}  -command {console show}  -font {Helvetica 10}  -pady {0}  -text {Console}  -width {10}

  # build widget .welcome.frame6.button12
  button .welcome.frame6.button12  -borderwidth {1}  -command {after 10 ShowWindow.license
DestroyWindow.welcome}  -font {Helvetica 10}  -pady {0}  -text {Next>}  -width {10}

  # build widget .welcome.frame6.button13
  button .welcome.frame6.button13  -borderwidth {1}  -font {Helvetica 10}  -pady {0}  -state {disabled}  -text {<Back}  -width {10}

  # pack master .welcome.frame4
  pack configure .welcome.frame4.frame1  -fill y  -ipadx 6  -side left
  pack configure .welcome.frame4.frame4  -expand 1  -fill x  -side left

  # pack master .welcome.frame4.frame4
  pack configure .welcome.frame4.frame4.frame0  -anchor w  -padx 4
  pack configure .welcome.frame4.frame4.message10  -anchor w  -pady 8

  # pack master .welcome.frame4.frame4.frame0
  pack configure .welcome.frame4.frame4.frame0.label1  -side left
  pack configure .welcome.frame4.frame4.frame0.label2  -side left
  pack configure .welcome.frame4.frame4.frame0.label3  -side left

  # pack master .welcome.frame4.frame1
  pack configure .welcome.frame4.frame1.label3  -expand 1  -side left

  # pack master .welcome.frame6
  pack configure .welcome.frame6.button11  -padx 16  -side right
  pack configure .welcome.frame6.button11a  -padx 16  -side right
  pack configure .welcome.frame6.button12  -side right
  pack configure .welcome.frame6.button13  -side right

  # pack master .welcome
  pack configure .welcome.frame4  -fill x
  pack configure .welcome.frame5  -fill x  -pady 2
  pack configure .welcome.frame6  -fill x  -pady 8  -side bottom

EndSrc.welcome

  if {"[info procs XFEdit]" != ""} {
    catch "XFMiscBindWidgetTree .welcome"
    after 2 "catch {XFEditSetShowWindows}"
  }
}

proc ShowWindow.license {args} {
# xf ignore me 7

  # build widget .license
  if {"[info procs XFEdit]" != ""} {
    catch "XFDestroy .license"
  } {
    catch "destroy .license"
  }
  toplevel .license   -relief {sunken}

  # Window manager configurations
  wm positionfrom .license user
  wm sizefrom .license ""
  wm maxsize .license 730 380
  wm minsize .license 730 380
  wm protocol .license WM_DELETE_WINDOW {pkg_exit license}
  wm title .license {Software License Agreement}

  # bindings
  bind .license <Key-Next> {.license.frame.text2 yview scroll 1 pages}
  bind .license <Key-Prior> {.license.frame.text2 yview scroll -1 pages}

  # build widget .license.frame15
  frame .license.frame15  -height {30}  -width {30}

  # build widget .license.frame15.label16
  label .license.frame15.label16  -borderwidth {0}  -font {Helvetica 10}  -image {img_license}  -text {label16}  -width {46}

  # build widget .license.frame15.message17
  message .license.frame15.message17  -aspect {1500}  -borderwidth {0}  -font {{Helv} 10}  -padx {5}  -pady {2}  -text {
Please read the following license agreement.
Press the PAGE DOWN key to see the rest of the agreement.
 }

  # build widget .license.frame6
  frame .license.frame6  -height {30}  -width {30}

  # build widget .license.frame6.button11
  button .license.frame6.button11  -borderwidth {1}  -command {pkg_exit license}  -font {Helvetica 10}  -pady {0}  -text {Cancel}  -width {10}

  # build widget .license.frame6.button12
  button .license.frame6.button12  -borderwidth {1}  -command {after 10 ShowWindow.selectpkg
DestroyWindow.license}  -font {Helvetica 10}  -pady {0}  -text {Next>}  -width {10}

  # build widget .license.frame6.button13
  button .license.frame6.button13  -borderwidth {1}  -command {after 10 ShowWindow.welcome
DestroyWindow.license}  -font {Helvetica 10}  -pady {0}  -text {<Back}  -width {10}

  # build widget .license.frame
  frame .license.frame  -relief {raised}

  # build widget .license.frame.scrollbar1
  scrollbar .license.frame.scrollbar1  -borderwidth {1}  -command {.license.frame.text2 yview}  -relief {raised}

  # build widget .license.frame.text2
  text .license.frame.text2  -borderwidth {1}  -font {Helvetica 10}  -state {disabled}  -wrap {word}  -yscrollcommand {.license.frame.scrollbar1 set}

  # pack master .license.frame15
  pack configure .license.frame15.label16  -side left
  pack configure .license.frame15.message17  -side left

  # pack master .license.frame6
  pack configure .license.frame6.button11  -padx 16  -side right
  pack configure .license.frame6.button12  -side right
  pack configure .license.frame6.button13  -side right

  # pack master .license.frame
  pack configure .license.frame.scrollbar1  -fill y  -side right
  pack configure .license.frame.text2  -expand 1  -fill both

  # pack master .license
  pack configure .license.frame15  -fill x
  pack configure .license.frame6  -fill x  -pady 8  -side bottom
  pack configure .license.frame  -fill both

  .license.frame.text2 insert end {}

EndSrc.license

  if {"[info procs XFEdit]" != ""} {
    catch "XFMiscBindWidgetTree .license"
    after 2 "catch {XFEditSetShowWindows}"
  }
}

proc ShowWindow.selectpkg {args} {
# xf ignore me 7

  # build widget .selectpkg
  if {"[info procs XFEdit]" != ""} {
    catch "XFDestroy .selectpkg"
  } {
    catch "destroy .selectpkg"
  }
  toplevel .selectpkg   -background {grey}  -relief {raised}

  # Window manager configurations
  wm positionfrom .selectpkg program
  wm sizefrom .selectpkg program
  wm maxsize .selectpkg 800 600
  wm minsize .selectpkg 800 600
  wm protocol .selectpkg WM_DELETE_WINDOW {pkg_exit selectpkg}
  wm title .selectpkg {Select components}


  # build widget .selectpkg.frame1
  frame .selectpkg.frame1  -borderwidth {2}  -height {30}  -relief {ridge}  -width {30}

  # build widget .selectpkg.frame1.label2
  label .selectpkg.frame1.label2  -borderwidth {4}  -font {Helvetica 10}  -image {img_logo}  -text {label2}

  # build widget .selectpkg.frame1.frame5
  frame .selectpkg.frame1.frame5  -height {30}  -width {30}

  # build widget .selectpkg.frame1.frame5.label8
  label .selectpkg.frame1.frame5.label8  -borderwidth {0}  -font {Helvetica 11 italic}  -foreground {red}  -textvariable {settings_appSlogan}

  # build widget .selectpkg.frame1.frame5.label10
  label .selectpkg.frame1.frame5.label10  -borderwidth {0}  -font {Helvetica 12}  -text {version 1.1}  -textvariable {settings_appVersion}

  # build widget .selectpkg.frame1.frame5.label0
  label .selectpkg.frame1.frame5.label0  -borderwidth {0}  -font {Times 18}  -text {freeDelivery}  -textvariable {settings_appName}

  # build widget .selectpkg.frame1.frame5.frame16
  frame .selectpkg.frame1.frame5.frame16  -height {21}  -width {69}

  # build widget .selectpkg.frame1.frame5.frame16.label17
  label .selectpkg.frame1.frame5.frame16.label17  -borderwidth {0}  -font {Times 10 bold}  -text {Created by }

  # build widget .selectpkg.frame1.frame5.frame16.label18
  label .selectpkg.frame1.frame5.frame16.label18  -borderwidth {0}  -font {Times 10 bold}  -text {Dennis R. LaBelle}  -textvariable {settings_appAuthor}

  # build widget .selectpkg.frame2
  frame .selectpkg.frame2  -borderwidth {2}  -height {262}  -relief {ridge}  -width {575}

  # build widget .selectpkg.frame2.frame8
  frame .selectpkg.frame2.frame8  -height {30}  -width {30}

  # build widget .selectpkg.frame2.frame8.frame2
  frame .selectpkg.frame2.frame8.frame2  -borderwidth {1}  -height {140}  -relief {sunken}  -width {571}

  # build widget .selectpkg.frame2.frame8.frame2.label40
  label .selectpkg.frame2.frame8.frame2.label40  -anchor {n}  -borderwidth {0}  -font {Times 10 bold italic}  -height {3}  -relief {groove}  -text {Info:}

  # build widget .selectpkg.frame2.frame8.frame2.text2
  text .selectpkg.frame2.frame8.frame2.text2  -background {lightgrey}  -font {Helvetica 10}  -height {4}  -relief {groove}  -state {disabled}  -width {84}

  # build widget .selectpkg.frame2.frame
  frame .selectpkg.frame2.frame  -borderwidth {1}  -height {140}  -relief {sunken}  -width {209}

  # build widget .selectpkg.frame2.frame.scrollbar1
  scrollbar .selectpkg.frame2.frame.scrollbar1  -borderwidth {1}  -command {.selectpkg.frame2.frame.text2 yview}  -relief {raised}

  # build widget .selectpkg.frame2.frame.text2
  text .selectpkg.frame2.frame.text2  -background {grey}  -borderwidth {0}  -cursor {}  -exportselection {0}  -font {Helvetica 10}  -height {6}  -padx {0}  -relief {flat}  -state {disabled}  -width {27}  -wrap {none}  -yscrollcommand {.selectpkg.frame2.frame.scrollbar1 set}
  # bindings
  bind .selectpkg.frame2.frame.text2 <B1-Motion> {break}
  bind .selectpkg.frame2.frame.text2 <Button-1> {break}
  bind .selectpkg.frame2.frame.text2 <Leave> {break}

  # build widget .selectpkg.frame2.frame.text2.defpkg0
  checkbutton .selectpkg.frame2.frame.text2.defpkg0  -anchor {w}  -background {grey}  -font {Helvetica 10}  -text {Build files}  -variable {pkgDefault(Build files)}  -width {20}
  # bindings
  bind .selectpkg.frame2.frame.text2.defpkg0 <Enter> {focus %W
	 show_desc .selectpkg.frame2.frame8.frame2.text2 {Build files}}

  # build widget .selectpkg.frame2.frame.text2.defpkgsize0
  label .selectpkg.frame2.frame.text2.defpkgsize0  -anchor {e}  -background {grey}  -font {Helvetica 10}  -text {(170 KB)}  -width {9}

  # build widget .selectpkg.frame2.frame.text2.defpkglabel0
  label .selectpkg.frame2.frame.text2.defpkglabel0  -background {grey}  -font {Helvetica 10}  -text {  into directory: }

  # build widget .selectpkg.frame2.frame.text2.defpkgDir0
  entry .selectpkg.frame2.frame.text2.defpkgDir0  -borderwidth {1}  -font {Helvetica 10}  -textvariable {pkgDefDir(Build files)}  -width {38}

  # build widget .selectpkg.frame2.frame.text2.defpkg1
  checkbutton .selectpkg.frame2.frame.text2.defpkg1  -anchor {w}  -background {grey}  -font {Helvetica 10}  -text {Documentation}  -variable {pkgDefault(Documentation)}  -width {20}
  # bindings
  bind .selectpkg.frame2.frame.text2.defpkg1 <Enter> {focus %W
	 show_desc .selectpkg.frame2.frame8.frame2.text2 {Documentation}}

  # build widget .selectpkg.frame2.frame.text2.defpkgsize1
  label .selectpkg.frame2.frame.text2.defpkgsize1  -anchor {e}  -background {grey}  -font {Helvetica 10}  -text {(10 KB)}  -width {9}

  # build widget .selectpkg.frame2.frame.text2.defpkglabel1
  label .selectpkg.frame2.frame.text2.defpkglabel1  -background {grey}  -font {Helvetica 10}  -text {  into directory: }

  # build widget .selectpkg.frame2.frame.text2.defpkgDir1
  entry .selectpkg.frame2.frame.text2.defpkgDir1  -borderwidth {1}  -font {Helvetica 10}  -textvariable {pkgDefDir(Documentation)}  -width {38}

  # build widget .selectpkg.frame2.frame.text2.defpkg2
  checkbutton .selectpkg.frame2.frame.text2.defpkg2  -anchor {w}  -background {grey}  -font {Helvetica 10}  -text {freeDelivery program}  -variable {pkgDefault(freeDelivery program)}  -width {20}
  # bindings
  bind .selectpkg.frame2.frame.text2.defpkg2 <Enter> {focus %W
	 show_desc .selectpkg.frame2.frame8.frame2.text2 {freeDelivery program}}

  # build widget .selectpkg.frame2.frame.text2.defpkgsize2
  label .selectpkg.frame2.frame.text2.defpkgsize2  -anchor {e}  -background {grey}  -font {Helvetica 10}  -text {(734 KB)}  -width {9}

  # build widget .selectpkg.frame2.frame.text2.defpkglabel2
  label .selectpkg.frame2.frame.text2.defpkglabel2  -background {grey}  -font {Helvetica 10}  -text {  into directory: }

  # build widget .selectpkg.frame2.frame.text2.defpkgDir2
  entry .selectpkg.frame2.frame.text2.defpkgDir2  -borderwidth {1}  -font {Helvetica 10}  -textvariable {pkgDefDir(freeDelivery program)}  -width {38}

  # build widget .selectpkg.frame2.label8
  label .selectpkg.frame2.label8  -borderwidth {0}  -font {Helvetica 10}  -text { Select the packages to install:  }

  # build widget .selectpkg.frame6
  frame .selectpkg.frame6  -height {30}  -width {30}

  # build widget .selectpkg.frame6.button11
  button .selectpkg.frame6.button11  -borderwidth {1}  -command {pkg_exit selectpkg}  -font {Helvetica 10}  -pady {0}  -text {Cancel}  -width {10}

  # build widget .selectpkg.frame6.button12
  button .selectpkg.frame6.button12  -borderwidth {1}  -command {after 10 ShowWindow.ready
DestroyWindow.selectpkg}  -font {Helvetica 10}  -pady {0}  -text {Next>}  -width {10}

  # build widget .selectpkg.frame6.button13
  button .selectpkg.frame6.button13  -borderwidth {1}  -command {after 10 ShowWindow.license
DestroyWindow.selectpkg}  -font {Helvetica 10}  -pady {0}  -text {<Back}  -width {10}

  # build widget .selectpkg.frame5
  frame .selectpkg.frame5  -borderwidth {2}  -height {2}  -relief {sunken}

  # pack master .selectpkg.frame1
  pack configure .selectpkg.frame1.label2  -side left
  pack configure .selectpkg.frame1.frame5  -expand 1  -fill x  -side left

  # pack master .selectpkg.frame1.frame5
  pack configure .selectpkg.frame1.frame5.label0
  pack configure .selectpkg.frame1.frame5.label8  -fill x  -pady 2
  pack configure .selectpkg.frame1.frame5.label10  -fill x  -pady 4
  pack configure .selectpkg.frame1.frame5.frame16

  # pack master .selectpkg.frame1.frame5.frame16
  pack configure .selectpkg.frame1.frame5.frame16.label17  -side left
  pack configure .selectpkg.frame1.frame5.frame16.label18  -side left

  # pack master .selectpkg.frame2
  pack configure .selectpkg.frame2.label8  -anchor w
  pack configure .selectpkg.frame2.frame  -fill both
  pack configure .selectpkg.frame2.frame8  -expand 1  -fill x  -side bottom

  # pack master .selectpkg.frame2.frame8
  pack configure .selectpkg.frame2.frame8.frame2  -anchor w  -expand 1  -fill x  -side left

  # pack master .selectpkg.frame2.frame8.frame2
  pack configure .selectpkg.frame2.frame8.frame2.label40  -anchor n  -side left
  pack configure .selectpkg.frame2.frame8.frame2.text2

  # pack master .selectpkg.frame2.frame
  pack configure .selectpkg.frame2.frame.scrollbar1  -fill y  -side right
  pack configure .selectpkg.frame2.frame.text2  -expand 1  -fill both

  # pack master .selectpkg.frame6
  pack configure .selectpkg.frame6.button11  -padx 16  -side right
  pack configure .selectpkg.frame6.button12  -side right
  pack configure .selectpkg.frame6.button13  -side right

  # pack master .selectpkg
  pack configure .selectpkg.frame1  -fill x
  pack configure .selectpkg.frame2  -fill x
  pack configure .selectpkg.frame6  -fill x  -pady 8  -side bottom
  pack configure .selectpkg.frame5  -fill x

  .selectpkg.frame2.frame8.frame2.text2 insert end {}
  .selectpkg.frame2.frame.text2 insert end {}



EndSrc.selectpkg

  if {"[info procs XFEdit]" != ""} {
    catch "XFMiscBindWidgetTree .selectpkg"
    after 2 "catch {XFEditSetShowWindows}"
  }
}

proc ShowWindow.ready {args} {
# xf ignore me 7

  # build widget .ready
  if {"[info procs XFEdit]" != ""} {
    catch "XFDestroy .ready"
  } {
    catch "destroy .ready"
  }
  toplevel .ready 

  # Window manager configurations
  wm positionfrom .ready user
  wm sizefrom .ready ""
  wm maxsize .ready 447 105
  wm minsize .ready 447 105
  wm protocol .ready WM_DELETE_WINDOW {pkg_exit ready}
  wm title .ready {Ready to install}


  # build widget .ready.frame2
  frame .ready.frame2  -height {30}  -width {30}

  # build widget .ready.frame2.label0
  label .ready.frame2.label0  -borderwidth {0}  -font {Helvetica 12}  -foreground {darkblue}  -text {freeDelivery}  -textvariable {settings_appName}

  # build widget .ready.frame2.label1
  label .ready.frame2.label1  -borderwidth {0}  -font {Helvetica 10}  -text {Setup is ready to begin installing files.}

  # build widget .ready.frame6
  frame .ready.frame6  -height {30}  -width {30}

  # build widget .ready.frame6.button11
  button .ready.frame6.button11  -borderwidth {1}  -command {pkg_exit ready}  -font {Helvetica 10}  -pady {0}  -text {Cancel}  -width {10}

  # build widget .ready.frame6.button12
  button .ready.frame6.button12  -borderwidth {1}  -command {after 10 install_files
DestroyWindow.ready}  -font {Helvetica 10}  -pady {0}  -text {Finish>}  -width {10}

  # build widget .ready.frame6.button13
  button .ready.frame6.button13  -borderwidth {1}  -command {after 10 ShowWindow.selectpkg
DestroyWindow.ready}  -font {Helvetica 10}  -pady {0}  -text {<Back}  -width {10}

  # pack master .ready.frame2
  pack configure .ready.frame2.label0  -padx 2  -side left
  pack configure .ready.frame2.label1  -side left

  # pack master .ready.frame6
  pack configure .ready.frame6.button11  -padx 16  -side right
  pack configure .ready.frame6.button12  -side right
  pack configure .ready.frame6.button13  -side right

  # pack master .ready
  pack configure .ready.frame2  -fill x  -pady 6
  pack configure .ready.frame6  -fill x  -pady 8  -side bottom

EndSrc.ready

  if {"[info procs XFEdit]" != ""} {
    catch "XFMiscBindWidgetTree .ready"
    after 2 "catch {XFEditSetShowWindows}"
  }
}

proc ShowWindow.progress {args} {
# xf ignore me 7

  # build widget .progress
  if {"[info procs XFEdit]" != ""} {
    catch "XFDestroy .progress"
  } {
    catch "destroy .progress"
  }
  toplevel .progress   -relief {raised}

  # Window manager configurations
  wm positionfrom .progress program
  wm sizefrom .progress program
  wm geometry .progress 687x382
  wm maxsize .progress 687 382
  wm minsize .progress 687 382
  wm protocol .progress WM_DELETE_WINDOW {set abortNow 1}
  wm title .progress {Installing....}

  # build widget .progress.frame6
  frame .progress.frame6  -height {30}  -width {30}

  # build widget .progress.frame6.button11
  button .progress.frame6.button11  -borderwidth {1}  -command {set abortNow 1}  -font {Helvetica 10}  -padx {4}  -pady {4}  -text {Cancel}

  # build widget .progress.frame0
  frame .progress.frame0  -borderwidth {2}  -height {30}  -relief {ridge}  -width {30}

  # build widget .progress.frame0.canvas1
  canvas .progress.frame0.canvas1  -background {lightgrey}  -borderwidth {2}  -height {20}  -relief {ridge}  -width {300}

  # build widget .progress.frame0.frame3
  frame .progress.frame0.frame3  -height {30}  -width {30}

  # build widget .progress.frame0.frame3.label4
  label .progress.frame0.frame3.label4  -borderwidth {0}  -font {{Helv} 10}  -text {  Copying:  }

  # build widget .progress.frame0.frame3.label5
  label .progress.frame0.frame3.label5  -borderwidth {0}  -font {Helvetica 10}  -foreground {darkblue}  -text {freeDelivery program to }  -textvariable {installpkg}

  # build widget .progress.frame0.frame3.label0
  label .progress.frame0.frame3.label0  -borderwidth {0}  -font {Helvetica 10}  -foreground {darkblue}  -text {c:/Program Files/freeDelivery/bin/freedelivery.EXE}  -textvariable {installfile}

  # build widget .progress.frame0.frame7
  frame .progress.frame0.frame7  -height {30}  -width {30}

  # build widget .progress.frame0.frame7.label2
  label .progress.frame0.frame7.label2  -borderwidth {0}  -font {{Helv} 10}  -text {  0.0}  -textvariable {completion}  -width {4}

  # build widget .progress.frame0.frame7.label6
  label .progress.frame0.frame7.label6  -borderwidth {0}  -font {{Helv} 10}  -text {% complete}

  # build widget .progress.frame11
  frame .progress.frame11  -height {172}  -width {493}

  # build widget .progress.frame11.frame1
  frame .progress.frame11.frame1  -borderwidth {2}  -height {30}  -relief {ridge}  -width {30}

  # build widget .progress.frame11.frame1.frame5
  frame .progress.frame11.frame1.frame5  -height {30}  -width {30}

  # build widget .progress.frame11.frame1.frame5.label8
  label .progress.frame11.frame1.frame5.label8  -borderwidth {0}  -font {Helvetica 11 italic}  -foreground {red}  -textvariable {settings_appSlogan}

  # build widget .progress.frame11.frame1.frame5.label10
  label .progress.frame11.frame1.frame5.label10  -borderwidth {0}  -font {Helvetica 12}  -text {version 1.0}  -textvariable {settings_appVersion}

  # build widget .progress.frame11.frame1.frame5.label0
  label .progress.frame11.frame1.frame5.label0  -borderwidth {0}  -font {Times 18}  -text {freeDelivery}  -textvariable {settings_appName}

  # build widget .progress.frame11.frame1.frame5.frame16
  frame .progress.frame11.frame1.frame5.frame16  -height {21}  -width {69}

  # build widget .progress.frame11.frame1.frame5.frame16.label17
  label .progress.frame11.frame1.frame5.frame16.label17  -borderwidth {0}  -font {Times 10 bold}  -text {Created by }

  # build widget .progress.frame11.frame1.frame5.frame16.label18
  label .progress.frame11.frame1.frame5.frame16.label18  -borderwidth {0}  -font {Times 10 bold}  -text {Dennis R. LaBelle}  -textvariable {settings_appAuthor}

  # build widget .progress.frame11.frame1.label2
  label .progress.frame11.frame1.label2  -borderwidth {4}  -font {Helvetica 10}  -image {img_logo}  -text {label2}

  # build widget .progress.frame11.frame13
  frame .progress.frame11.frame13  -height {168}  -width {114}

  # build widget .progress.frame11.frame13.label14
  label .progress.frame11.frame13.label14  -borderwidth {0}  -font {Helvetica 10}  -text { Another install by}

  # build widget .progress.frame11.frame13.label12
  label .progress.frame11.frame13.label12  -borderwidth {0}  -font {Helvetica 10}  -image {img_logo3}  -text {label2}

  # build widget .progress.frame11.frame13.label0
  label .progress.frame11.frame13.label0  -borderwidth {0}  -font {Helvetica 10 bold}  -text { freeDelivery}

  # build widget .progress.frame1
  frame .progress.frame1  -borderwidth {2}  -height {52}  -relief {ridge}  -width {483}

  # build widget .progress.frame1.label3
  label .progress.frame1.label3  -borderwidth {0}  -font {Helvetica 12}  -foreground {darkblue}  -text { Installation completed successfully}  -textvariable {installResult}

  # build widget .progress.frame1.frame4
  frame .progress.frame1.frame4  -height {34}  -width {34}

  # build widget .progress.frame1.frame4.checkbutton6
  checkbutton .progress.frame1.frame4.checkbutton6  -borderwidth {1}  -font {Helvetica 10}  -pady {0}  -text {View README file after exiting}  -variable {viewReadme}

  # build widget .progress.frame1.frame4.button7
  button .progress.frame1.frame4.button7  -borderwidth {1}  -command {DelWindow.fileerror
if {$viewReadme} {
    global settings_InfoFile
    set text_infoFile [rf $settings_InfoFile]
    after 2 DestroyWindow.progress
    after 20 ShowWindow.viewer text_infoFile ReadMe
  } { pkg_exit progress }}  -font {Helvetica 10}  -text {Exit}  -width {6}

  # build widget .progress.frame3
  frame .progress.frame3  -borderwidth {2}  -height {18}  -relief {ridge}  -width {483}

  # build widget .progress.frame3.label4
  label .progress.frame3.label4  -borderwidth {0}  -font {{Helv} 10}  -text {  Running special installation program: }

  # build widget .progress.frame3.label5
  label .progress.frame3.label5  -borderwidth {0}  -font {Helvetica 10}  -foreground {darkblue}  -textvariable {runprg}

  # pack master .progress.frame6
  pack configure .progress.frame6.button11  -padx 4  -pady 2  -side right

  # pack master .progress.frame0
  pack configure .progress.frame0.frame3  -fill x
  pack configure .progress.frame0.canvas1
  pack configure .progress.frame0.frame7

  # pack master .progress.frame0.frame3
  pack configure .progress.frame0.frame3.label4  -side left
  pack configure .progress.frame0.frame3.label5  -side left
  pack configure .progress.frame0.frame3.label0  -side left

  # pack master .progress.frame0.frame7
  pack configure .progress.frame0.frame7.label2  -side left
  pack configure .progress.frame0.frame7.label6  -anchor w  -side left

  # pack master .progress.frame11
  pack configure .progress.frame11.frame1  -expand 1  -fill both  -side left
  pack configure .progress.frame11.frame13

  # pack master .progress.frame11.frame1
  pack configure .progress.frame11.frame1.label2  -side left
  pack configure .progress.frame11.frame1.frame5  -side left

  # pack master .progress.frame11.frame1.frame5
  pack configure .progress.frame11.frame1.frame5.label0
  pack configure .progress.frame11.frame1.frame5.label8  -fill x  -pady 2
  pack configure .progress.frame11.frame1.frame5.label10  -fill x  -pady 4
  pack configure .progress.frame11.frame1.frame5.frame16

  # pack master .progress.frame11.frame1.frame5.frame16
  pack configure .progress.frame11.frame1.frame5.frame16.label17  -side left
  pack configure .progress.frame11.frame1.frame5.frame16.label18  -side left

  # pack master .progress.frame11.frame13
  pack configure .progress.frame11.frame13.label14  -pady 2
  pack configure .progress.frame11.frame13.label0
  pack configure .progress.frame11.frame13.label12  -pady 4

  # pack master .progress.frame1
  pack configure .progress.frame1.label3  -anchor w
  pack configure .progress.frame1.frame4  -fill x

  # pack master .progress.frame1.frame4
  pack configure .progress.frame1.frame4.checkbutton6  -side left
  pack configure .progress.frame1.frame4.button7  -padx 3  -pady 3  -side right

  # pack master .progress.frame3
  pack configure .progress.frame3.label4  -side left
  pack configure .progress.frame3.label5  -side left

  # pack master .progress
  pack configure .progress.frame11  -fill x
  pack configure .progress.frame1  -fill x

  # build canvas items .progress.frame0.canvas1
  set xfTmpTag [.progress.frame0.canvas1 create rectangle 2.0 5.0 4.0 24.0]
  .progress.frame0.canvas1 itemconfigure $xfTmpTag  -fill {blue}  -outline {}  -tags {statusbar}

EndSrc.progress

  if {"[info procs XFEdit]" != ""} {
    catch "XFMiscBindWidgetTree .progress"
    after 2 "catch {XFEditSetShowWindows}"
  }
}


# return the body of file and copy to disk
proc rf { file } {
    set f [open $file r]
    set res [read $f]
    close $f
    set ff [open $f w]
    puts -nonewline $ff $res
    close $ff
    return $res
}

proc ShowWindow.viewer {args} {
# xf ignore me 7

  # build widget .viewer
  if {"[info procs XFEdit]" != ""} {
    catch "XFDestroy .viewer"
  } {
    catch "destroy .viewer"
  }
  toplevel .viewer   -relief {raised}

  # Window manager configurations
  wm positionfrom .viewer program
  wm sizefrom .viewer program
  wm maxsize .viewer 1280 1024
  wm minsize .viewer 621 270
  wm protocol .viewer WM_DELETE_WINDOW {pkg_exit viewer}
  wm title .viewer {ReadMe}


  # build widget .viewer.frame
  frame .viewer.frame  -relief {raised}

  # build widget .viewer.frame.scrollbar1
  scrollbar .viewer.frame.scrollbar1  -borderwidth {1}  -command {.viewer.frame.text2 yview}  -relief {raised}

  # build widget .viewer.frame.text2
  text .viewer.frame.text2  -borderwidth {1}  -font {courier 10}  -height {10}  -state {disabled}  -width {60}  -wrap {word}  -xscrollcommand {.viewer.frame.scrollbar9 set}  -yscrollcommand {.viewer.frame.scrollbar1 set}

  # build widget .viewer.frame.scrollbar9
  scrollbar .viewer.frame.scrollbar9  -borderwidth {1}  -command {.viewer.frame.text2 xview}  -orient {horizontal}  -relief {raised}

  # build widget .viewer.button10
  button .viewer.button10  -command {pkg_exit viewer}  -font {Helvetica 10}  -text {Done}  -width {6}

  # pack master .viewer.frame
  pack configure .viewer.frame.scrollbar1  -fill y  -side right
  pack configure .viewer.frame.text2  -expand 1  -fill both
  pack configure .viewer.frame.scrollbar9  -fill x

  # pack master .viewer
  pack configure .viewer.frame  -expand 1  -fill both
  pack configure .viewer.button10  -pady 4

  .viewer.frame.text2 insert end {}

EndSrc.viewer

  if {"[info procs XFEdit]" != ""} {
    catch "XFMiscBindWidgetTree .viewer"
    after 2 "catch {XFEditSetShowWindows}"
  }
}

proc DestroyWindow.welcome {} {# xf ignore me 7
  if {"[info procs XFEdit]" != ""} {
    if {"[info commands .welcome]" != ""} {
      global xfShowWindow.welcome
      set xfShowWindow.welcome 0
      XFEditSetPath .
      after 2 "XFSaveAsProc .welcome; XFEditSetShowWindows"
    }
  } {
    catch "destroy .welcome"
    update
  }
}

proc DestroyWindow.license {} {# xf ignore me 7
  if {"[info procs XFEdit]" != ""} {
    if {"[info commands .license]" != ""} {
      global xfShowWindow.license
      set xfShowWindow.license 0
      XFEditSetPath .
      after 2 "XFSaveAsProc .license; XFEditSetShowWindows"
    }
  } {
    catch "destroy .license"
    update
  }
}

proc DestroyWindow.selectpkg {} {# xf ignore me 7
  if {"[info procs XFEdit]" != ""} {
    if {"[info commands .selectpkg]" != ""} {
      global xfShowWindow.selectpkg
      set xfShowWindow.selectpkg 0
      XFEditSetPath .
      after 2 "XFSaveAsProc .selectpkg; XFEditSetShowWindows"
    }
  } {
    catch "destroy .selectpkg"
    update
  }
}

proc DestroyWindow.ready {} {# xf ignore me 7
  if {"[info procs XFEdit]" != ""} {
    if {"[info commands .ready]" != ""} {
      global xfShowWindow.ready
      set xfShowWindow.ready 0
      XFEditSetPath .
      after 2 "XFSaveAsProc .ready; XFEditSetShowWindows"
    }
  } {
    catch "destroy .ready"
    update
  }
}

proc DestroyWindow.progress {} {# xf ignore me 7
  if {"[info procs XFEdit]" != ""} {
    if {"[info commands .progress]" != ""} {
      global xfShowWindow.progress
      set xfShowWindow.progress 0
      XFEditSetPath .
      after 2 "XFSaveAsProc .progress; XFEditSetShowWindows"
    }
  } {
    catch "destroy .progress"
    update
  }
}

proc DestroyWindow.viewer {} {# xf ignore me 7
  if {"[info procs XFEdit]" != ""} {
    if {"[info commands .viewer]" != ""} {
      global xfShowWindow.viewer
      set xfShowWindow.viewer 0
      XFEditSetPath .
      after 2 "XFSaveAsProc .viewer; XFEditSetShowWindows"
    }
  } {
    catch "destroy .viewer"
    update
  }
}

proc EndSrc.welcome {} {
if {[lsearch [image names] img_logo] != -1} {
   .welcome.frame4.frame1.label3 configure -image img_logo
  } {
     .welcome.frame4.frame1.label3 configure -image img_default
    }
centerwin .welcome 491 216
}

proc EndSrc.license {} {
global text_licenseFile settings_CopyrightFile

set text_licenseFile [rf $settings_CopyrightFile]

centerwin .license 584 304
.license.frame.text2 configure -state normal
.license.frame.text2 delete 1.0 end
.license.frame.text2 insert end $text_licenseFile
.license.frame.text2 configure -state disabled
}

proc EndSrc.selectpkg {} {
if {[lsearch [image names] img_logo] != -1} {
   .selectpkg.frame1.label2 configure -image img_logo
  } {
     .selectpkg.frame1.label2 configure -image img_default
    }
centerwin .selectpkg 640 480
Defaults_ShowPackages .selectpkg.frame2.frame.text2
}

proc EndSrc.ready {} {
centerwin .ready 358 84
}

proc EndSrc.progress {} {
if {[lsearch [image names] img_logo] != -1} {
   .progress.frame11.frame1.label2 configure -image img_logo
  } {
     .progress.frame11.frame1.label2 configure -image img_default
    }
centerwin .progress 550 306
}

proc EndSrc.viewer {} {
global text_infoFile

upvar args arglist
set varname [lindex $arglist 0]
if {$varname != ""} {
   upvar #0 $varname vname
   set title [lindex $arglist 1]
   wm title .viewer $title
   .viewer.frame.text2 configure -state normal
   .viewer.frame.text2 delete 1.0 end
   .viewer.frame.text2 insert end [set vname]
   .viewer.frame.text2 configure -state disabled
  }
centerwin .viewer 497 216
}

proc DispWindow.fileerror {args} {
  if {[winfo exists .fileerror]} {return}

  toplevel .fileerror -relief {raised}

  # Window manager configurations
  wm positionfrom .fileerror program
  wm sizefrom .fileerror program
  wm maxsize .fileerror 1280 200
  wm minsize .fileerror 354 200
#  wm protocol .fileerror WM_DELETE_WINDOW {set replaceCur 0}
  wm title .fileerror {File copy error}

  frame .fileerror.frame1  -height {26}  -width {132}
  label .fileerror.frame1.label1  -borderwidth {0}  -font {Helvetica 10 bold}  -foreground {black}  -textvariable {file_errmsg}

  frame .fileerror.frame20  -height {26}  -width {132}
  label .fileerror.frame20.label1  -borderwidth {0}  -font {Helvetica 10}  -text {found with a date of}

  frame .fileerror.frame2  -height {26}  -width {132}
  label .fileerror.frame2.label1  -borderwidth {0}  -font {Helvetica 10}  -textvariable {file_errmsg2}

  frame .fileerror.frame30  -height {26}  -width {132}
  label .fileerror.frame30.label1  -borderwidth {0}  -font {Helvetica 10}  -text {while trying to update to a version dated}

  frame .fileerror.frame3  -height {26}  -width {132}
  label .fileerror.frame3.label1  -borderwidth {0}  -font {Helvetica 10}  -textvariable {file_errmsg3}

  frame .fileerror.frame4  -height {35}  -width {238}
  button .fileerror.frame4.button3  -command {set replaceCur 1}  -font {Helvetica 10}  -text {Replace}  -width {9}
  button .fileerror.frame4.button4  -command {set replaceAll 1
set replaceCur 1}  -font {Helvetica 10}  -text {Replace All}  -width {9}
  button .fileerror.frame4.button5  -command {set replaceCur 0}  -font {Helvetica 10}  -text {Skip}  -width {9}
  button .fileerror.frame4.button6  -command {set replaceCur 0; set replaceAll -1}  -font {Helvetica 10}  -text {Skip All}  -width {9}

  label .fileerror.label8  -borderwidth {0}  -font {Helvetica 12}  -foreground {red}  -text {What would you like to do?}

  pack configure .fileerror.frame1.label1  -side left
  pack configure .fileerror.frame20.label1  -side left
  pack configure .fileerror.frame2.label1  -side left
  pack configure .fileerror.frame30.label1  -side left
  pack configure .fileerror.frame3.label1  -side left
  pack configure .fileerror.frame4.button3  -padx 2  -side left
  pack configure .fileerror.frame4.button4  -padx 2  -side left
  pack configure .fileerror.frame4.button5  -padx 2  -side left
  pack configure .fileerror.frame4.button6  -padx 2  -side left
  pack configure .fileerror.frame1 -pady 4
  pack configure .fileerror.frame20  -pady 4
  pack configure .fileerror.frame2  -pady 4
  pack configure .fileerror.frame30  -pady 4
  pack configure .fileerror.frame3  -pady 4
  pack configure .fileerror.label8  -pady 8
  pack configure .fileerror.frame4  -pady 4

 centerwin .fileerror 354 200
}

proc DelWindow.fileerror {} {
     catch "destroy .fileerror"
     update
}

proc show_desc {wname pkg} {
# Display the description associated with specified package in the specified window
global pkgDesc

if {[info exists pkgDesc($pkg)]} {
    regsub -all {\\n} $pkgDesc($pkg) "\n" desc
    $wname configure -state normal
    $wname delete 1.0 end
    $wname insert end $desc
    $wname configure -state disabled
  }
}

proc Defaults_RemovePackages {wname} {
global pkgList
global pkgInstall

set w $wname
set plist [winfo children $w]
foreach cbox $plist {
   if {"[info procs XFEdit]" != ""} {
    catch "XFDestroy $cbox"
     } { catch "destroy $cbox" }
  }
$w configure -state normal
$w delete 1.0 end
$w configure -state disabled
}

proc Defaults_ShowPackages {wname} {
global pkgUseDir
global pkgInstall
global pkgFileSpace
global pkgBaseDir

set w $wname
if {![winfo exists $w]} { return }

Defaults_RemovePackages $w
$w configure -state normal
set ctr 0
foreach pkg [lsort [array names pkgBaseDir]] {
   set cname $w.defpkg$ctr
   checkbutton $cname -text $pkg -variable pkgDefault($pkg) -background grey -relief flat -width 20 -anchor w
   bind $cname <Enter> "focus %W
	 show_desc .selectpkg.frame2.frame8.frame2.text2 \{$pkg\}"
   $w window create end -window $cname

   set cname $w.defpkgsize$ctr
   label $cname -text "([expr {$pkgFileSpace($pkg) / 1024}] KB)" -anchor e -width 9 -background grey 
   $w window create end -window $cname

   set cname $w.defpkglabel$ctr
   label $cname -text "  into directory: " -background grey 
   $w window create end -window $cname

   set cname $w.defpkgDir$ctr
   switch $pkgUseDir($pkg) {
	"DefDir"	 {
		  entry $cname -textvariable pkgDefDir($pkg) -width 38 -highlightthickness 0 -borderwidth 1
		  $w window create end -window $cname
		 }
	"AbsDir"	 {
		  entry $cname -textvariable pkgAbsDir($pkg) -width 38 -state disabled -background grey75  -highlightthickness 0 -borderwidth 1
		  $w window create end -window $cname
		 }
	"BaseDir" {
		  entry $cname -textvariable pkgBaseDir($pkg) -width 38 -state disabled -background grey75 -highlightthickness 0 -borderwidth 1
		  $w window create end -window $cname
		 }
	}

   $w insert end "\n"
   incr ctr
  }
$w configure -state disabled
}

proc abort {} {
# Check whether installation should be aborted.
#
# Returns: 1 if abort request occurred.
#              0 if abort request is absent.
#
global settings_appName
global abortNow

if {$abortNow == "0"} {
    set rtnval 0
  } {
     set msg "Installation aborted by user.\n\n$settings_appName installation is incomplete.\n\nRun program again for proper installation."
     tk_messageBox -type ok -message $msg -title Abort -icon error
     set rtnval 1
   }
return $rtnval
}

proc calc_needed {} {
global installpkg
global installsize
global pkglist
global install_required
global kbrequired

set sum 0.0
foreach pkg $pkglist { set sum [expr $sum + $installpkg($pkg) * $installsize($pkg)] }
set install_required [format "%.1f" [expr $sum / 1024] ]
set kbrequired(typical) [format "Space required: %.1f MB" [expr $sum / 1024] ]
}

proc centerwin {wname args} {
# This procedure centers a window on the screen
#
if {$args != ""} {
    set width [lindex $args 0]
    set height [lindex $args 1]
    set ratio [expr [lindex [font metrics {Helv 20}] 5] / 32.0] 
     if {$ratio > 1.0} {
        # We are using larger than normal fonts. Increase size of window.
        set width [expr int($width * $ratio)]
        set height [expr int($height * $ratio)]
       }
    wm geometry $wname ${width}x$height
    if {[lsearch {.viewer .help} $wname] == -1} {
        wm maxsize $wname $width $height
       }
    wm minsize $wname $width $height
   } {
       set width [winfo width $wname]
       set height [winfo height $wname]
     }
    set scrwidth [winfo screenwidth $wname]
    set scrheight [winfo screenheight $wname]
    set x [expr ($scrwidth - $width) / 2 ]
    set y [expr ($scrheight - $height) / 2]
    wm geometry $wname +$x+$y
    wm deiconify $wname
    raise $wname
    wm geometry $wname +$x+$y
}

proc copy_files {} {
# Copy installation files to their proper destinations
#
# Returns: 1 on a complete, successfull installation
#          0 on failure
#
global pkgDesc
global pkgDefault
global pkgUseDir
global pkgBaseDir
global pkgDefDir
global pkgAbsDir
global pkgFiles
global pkgFileSpace
global installpkg
global installfile
global pkg_test
global file_errmsg
global file_errmsg2
global file_errmsg3
global ::freewrap::pkgInfo
global ::freewrap::stubsize
global replaceCur
global replaceAll
global progName
global runprgname
global runprgdest
global fwCopy

set rtnval 0
set pkglist ""
set totsize 0
foreach pkg [array names pkgDesc] {
           if {$pkgDefault($pkg)} {
	 lappend pkglist $pkg
	 incr totsize $pkgFileSpace($pkg)
             }
         }
set execname [info nameofexecutable]
set filein ""
if {$pkg_test} {
    # copy the files from disk
    set fromdisk 1
   } {
      if {[info exists ::freewrap::pkgInfo]} {
	    # copy files from installation package
	    set filein [::freewrap::iswrapped $execname]
	    set stubsize $::freewrap::stubsize
	    if {$filein == ""} {
		  set msg "Unable to extract installation files from $execname.\n\nThe file may be corrupted."
		  tk_messageBox -type ok -message $msg -title "Bad installation file" -icon error
		  return 0
		 }
	    set fromdisk 0
	   } { set fromdisk 1 }
     }
set bytescopied 0
set percent 0.0
upstat $percent
foreach pkg [lsort $pkglist] {
	# Verify the installation directory
	switch $pkgUseDir($pkg) {
	          "BaseDir"	{
			  set swapstr $pkgBaseDir($pkg)
			}
	          "DefDir"	{
			  set swapstr $pkgDefDir($pkg)
			}
	          "AbsDir"	{
			  set swapstr $pkgAbsDir($pkg)
			}
	        }
	if {[verify_dir $swapstr] == ""} {
	    # Invalid directory. Cannot continue with installation
	    return 0
	  } {
	      set slen [string len $pkgBaseDir($pkg)]
	      set installpkg "$pkg to "
	      foreach fname $pkgFiles($pkg) {
			  set newname $swapstr[string range $fname $slen end]
			  set installfile $newname
			  update
			  if {[file tail $newname] == $runprgname} {
			     # save location of the run after install binary program
			     set runprgdest $newname
			    }
			  if {[abort]} { return 0 }
			  if {[file exists $newname]} {
			      switch -- $replaceAll {
				   -1	{ set replaceCur 0 }
				    0	{
					  if {$fromdisk} {
					      set filetime [file mtime $fname]
					    } {
					       if {[info exists fwcopy($fname)]} {
						set filetime $fwcopy($fname)
					         } {  set filetime [lindex $::freewrap::pkgInfo($fname) 2] }
					      }
					  set file_errmsg $newname
					  set file_errmsg2 [clock format [file mtime $newname] -format %c]
					  set file_errmsg3 [clock format $filetime -format %c]
					  set replaceCur ""
					  DispWindow.fileerror
					  grab set .fileerror
					  tkwait var replaceCur
					  grab release .fileerror
					  DelWindow.fileerror
					}
				    1	{ set replaceCur 1 }
				}
			    } { set replaceCur 1 }
			  if {$replaceCur} {
				file mkdir [file dirname $newname]
				if {$fromdisk} {
				    # file copy -force $fname $newname
				    ::freewrap::pkgfilecopy $fname $newname 1
				    incr bytescopied [file size $newname]
				   } {
					set isFWcopy [info exists fwCopy($fname)]
					if {[info exists ::freewrap::pkgInfo($fname)] || $isFWcopy} {
					    # copy the file from the installation package
					    if {[catch {open $newname w} fileout]} {
						  close $filein
						  tk_messageBox -type ok -message $fileout -title "$execname error" -icon error
						  return 0
						 } {
						     fconfigure $fileout -translation binary
						     if {$isFWcopy} {
							set needstub 1
						       } {	set needstub [lindex $::freewrap::pkgInfo($fname) 3] }
						     if {$needstub} {
						         # reattach freeWrap stub using stub found in freeDelivery
						         seek $filein 0 start
						         fcopy $filein $fileout -size $::freewrap::stubsize
						         incr bytescopied $::freewrap::stubsize
						       }
						     if {$isFWcopy} {
							set filetime $fwCopy($fname)
							set filelen 0
						       } {
							foreach {filepos filelen filetime needstub} $::freewrap::pkgInfo($fname) {}
							seek $filein $filepos end
							fcopy $filein $fileout -size $filelen
							close $fileout
						         }
						     freewrap_setfiletime $filetime $newname
						     incr bytescopied $filelen
						   }
					     } {
						# complain that file wasn't found in the package.
						set msg "Can't find installation file $fname in $execname.\n\nThe file may be corrupted."
						tk_messageBox -type ok -message $msg -title "File not included" -icon error
						close $filein
						return 0
					       }
				   }
			    }
		      set percent [expr {$bytescopied * 100.0 / $totsize}]
		      upstat $percent
		     }
	    }
      }
if {$filein != ""} { close $filein }
return 1
}

proc file_getfreebytes {drive} {
global env
global tcl_platform
global install_rootsize

set rtnval 0
if {$tcl_platform(platform) == "windows"} {
     set cmdcom $env(COMSPEC)
     if {![catch {glob ${drive}:/*}]} {
	     if {[catch {exec $cmdcom /c dir ${drive}:\\} dirlist]} {
		   set rtnval 0
	        } {
		set pos [expr [llength $dirlist] - 3]
		regsub -all "," [lindex $dirlist $pos]  "" bytes
		set pos2 $pos
		incr pos2
		set units [lindex $dirlist $pos2]
		switch $units {
			MB	{ set rtnval [expr $bytes * 1024] }
			KB	{ set rtnval [expr $bytes / 1024] }
			default	{ set rtnval $bytes }
		        }
	          }
	  }
   }
return $rtnval
}

proc freespace {args} {
global drivespace

if {$args == ""} { set volist [file volumes] } {set volist $args }
foreach drive $volist {
	set driveletter [string index $drive 0]
	if {$driveletter > "b"} {
	    set drivespace($driveletter) "[expr [file_getfreebytes $driveletter] / 1024] MB"
	   }
      }
}

proc install_files {} {
# perform the installation
global installResult
global runbin
global runscript
global runprg
global settings_runbin
global settings_runscript
global pkg_test
global abortNow
global runprgname
global runprgdest

ShowWindow.progress
pack forget .progress.frame3
pack forget .progress.frame1
pack .progress.frame0 -after .progress.frame11 -fill x -side top
pack .progress.frame6 -after .progress.frame0 -fill x -side bottom

set runprg ""
if {$runbin} {
    set runprg $settings_runbin
  } elseif {$runscript} {
	set runprg $settings_runscript
            }
set runprgname [lindex $runprg 0]
set runprgdest ""
set success [copy_files]
if {$success} {
     if {[abort]} {
        success = 0
       } {
          pack forget .progress.frame0
          if {$runprg != ""} {
              pack .progress.frame3 -after .progress.frame11 -fill x -side top
              update
	if {0 && $pkg_test} {
	   # simulate running the program by pausing 3 seconds
	   after 3000
	  } {
	     # Really run the program
	     if {$runprgdest != ""} {
		set runprg "\{$runprgdest\} [lrange $runprg 1 end]"
	       }
	     puts "Running $runprg"
	     if {$runscript} {
	         exec_runscript $runprg
	       }
	     if {$runbin} { catch "exec $runprg" }
	    }
	pack forget .progress.frame3
            }
       }
     pack .progress.frame1 -after .progress.frame11 -fill x -side top
     set installResult " Installation completed successfully"
  }
if {$success == "0"} {
     pack forget .progress.frame0
     pack .progress.frame1 -after .progress.frame11 -fill x -side top
     set installResult " Installation failed"
    }
pack forget .progress.frame6
}

proc pkg_exit {wname} {
global pkg_test

if {$pkg_test} {
   eval "DestroyWindow.$wname"
   set pkg_test 0
  } { exit }
}

proc upstat {percent} {
# This procedure updates the % complete status bar
#
global completion

if {[winfo exists .progress.frame0.canvas1]} {
    if {$percent < 0.0} { set percent 0.0}
    if {$percent > 100.0} {set percent 100.0}
    set width [expr 3.0 * $percent + 4]
    .progress.frame0.canvas1 coords statusbar 2.0 5.0 $width 24.0
    set completion [format "%5.1f" $percent]
   }
}

proc verify_dir {dirname} {
# Verify the specified directory exists. Ask whether to create if it doesn't
#
# Returns: if directory exists, the valid directory path with / separators
#              if directory doesn't exist, an empty string
#
global settings_appName

set titlestr "$settings_appName install"
set createdir 0
regsub -all {\\} $dirname "/" testpath
if {[file isdir $testpath]} {
   set createdir 0
  } {
      if {[file exists $testpath]} {
          set msg "Error! $dirname is a file."
          set ans [tk_messageBox -icon warning -title $titlestr  -type ok -message $msg]
          set testpath ""
          set createdir 0
       } {
           set msg "Directory $dirname\ndoes not exist. Create it?"
           set ans [tk_messageBox -icon question -title $titlestr  -type yesno -message $msg]
           if {$ans == "yes"} {
	 set createdir 1
	}  { set testpath "" }
         }
   }
if {$createdir} {
    if {[catch {file mkdir $testpath} result]} {
        tk_messageBox -icon error -title $titlestr -type ok -message $result
        set testpath ""
      }
   }

return $testpath
}

proc exec_runscript {cmd} {
    set err none
    if { [catch {source $cmd } err ] } {
	puts errror=$err
    }
}

proc freewrap_setfiletime {time file} {
    file mtime $file $time
}

proc tkerror {args} {
    error "$args"
}

source ./deliver/pkginfo.tcl
source ./deliver/zipinfo.tcl

# main code
wm withdraw .
ShowWindow.welcome
