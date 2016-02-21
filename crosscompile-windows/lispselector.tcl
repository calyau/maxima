# Simple GUI for selecting the default Lisp for Windows users.

proc selectclisp {} {
    set maximarc [file join $::env(USERPROFILE) maxima maximarc]
    file mkdir [file dirname $maximarc]
    set f [open $maximarc "w"]
    puts $f "MAXIMA_LISP=clisp"
    close $f
    tk_messageBox -type ok -message "CLISP was selected as default Lisp interpreter for Maxima." -icon info
}

proc selectsbcl {} {
    set maximarc [file join $::env(USERPROFILE) maxima maximarc]
    file mkdir [file dirname $maximarc]
    set f [open $maximarc "w"]
    puts $f "MAXIMA_LISP=sbcl"
    close $f
    tk_messageBox -type ok -message "SBCL was selected as default Lisp interpreter for Maxima." -icon info
}

set binpath [file dirname [file normalize [info script]]]

set documentation "One can use different LISP (the programming language, in which Maxima is (mostly) written) interpreters for running Maxima.
Currently this Windows installer supports CLISP (http://www.clisp.org) and SBCL (http://www.sbcl.org).

Which Lisp you select, may depend on your needs:

SBCL is usually faster, but there were issues with DEP (data execution prevention) reported. 
It might be necessary to disable DEP for $binpath/sbcl.exe. 
Due to memory problems, some packages (e.g. Lapack) will not work.

CLISP may be slower, but these problems do not occur there. In command line Maxima CLISP 
provides advanced editing features (a history of previous commands is accessible with the cursor keys).

If you click on 'Select CLISP' or 'Select SBCL', a configuration file 'maximarc'
will be created with your default Lisp selection. If the configuration file already exists, it will be overwritten."


# Buttons (clisp, sbcl, exit)
frame .toolbar
button .toolbar.clisp -text "Select CLISP" -command "selectclisp"
button .toolbar.sbcl -text "Select SBCL" -command "selectsbcl"
button .toolbar.exit -text "Exit" -command "exit"
pack .toolbar.clisp -side left
pack .toolbar.sbcl -side left
pack .toolbar.exit -side right

# Documentation area
frame .docu
label .docu.label -text $documentation
pack .docu.label -padx 50 -pady 50


wm title . "Select LISP for Maxima"
grid config .toolbar -column 0 -row 1 -sticky "snew"
grid config .docu    -column 0 -row 2

