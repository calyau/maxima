# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: xmaxima.tcl,v 1.20 2002-09-06 06:21:36 mikeclarkson Exp $
#

#mike The following files are prepended, and could be sourced instead.
# The only problem about sourcing them is that the way of finding
# the directory they're in may differ in a wrapped executable.
# Note that the order of required files may be important.

# Source Tkmaxima/Constants.tcl 	;# required - must not be autoloaded
# Source Tkmaxima/Readdata.tcl 		;# can be autoloaded
# Source Tkmaxima/Getdata1.tcl 		;# can be autoloaded
# Source Tkmaxima/Macros.tcl 		;# can be autoloaded
# Source Tkmaxima/Proxy.tcl 		;# can be autoloaded
# Source Tkmaxima/Send-some.tcl 	;# sets global variables
# Source Tkmaxima/Plotting.tcl 		;# sets global variables
# Source Tkmaxima/Private.tcl 		;# can be autoloaded
# Source Tkmaxima/Getopt.tcl 		;# can be autoloaded
# Source Tkmaxima/Parse.tcl 		;# sets global variables
# Source Tkmaxima/Textinsert.tcl 	;# can be autoloaded
# Source Tkmaxima/Printops.tcl 		;# can be autoloaded
# Source Tkmaxima/Push.tcl 		;# can be autoloaded
# Source Tkmaxima/Plotconf.tcl 		;# can be autoloaded
# Source Tkmaxima/Adams.tcl 		;# can be autoloaded
# Source Tkmaxima/Rk.tcl 		;# can be autoloaded
# Source Tkmaxima/Plotdf.tcl 		;# can be autoloaded
# Source Tkmaxima/Plot2d.tcl 		;# defined globals
# Source Tkmaxima/Matrix.tcl 		;# can be autoloaded
# Source Tkmaxima/Plot3d.tcl 		;# defined globals
# Source Tkmaxima/NPlot3d.tcl 		;# can be autoloaded
# Source Tkmaxima/EOctave.tcl 		;# can be autoloaded
# Source Tkmaxima/EOpenplot.tcl 	;# can be autoloaded
# Source Tkmaxima/EMaxima.tcl 		;# can be autoloaded
# Source Tkmaxima/EHref.tcl 		;# can be autoloaded
# Source Tkmaxima/Browser.tcl 		;# defines globals and bindings
# Source Tkmaxima/Wmenu.tcl 		;# can be autoloaded
# Source Tkmaxima/Tryftp2.tcl 		;# can be autoloaded
# Source Tkmaxima/Myhtml.tcl 		;# defines globals and tags
# Source Tkmaxima/Myhtml1.tcl 		;# can be autoloaded
# Source Tkmaxima/Base64.tcl 		;# can be autoloaded
# Source Tkmaxima/Bitmaps.tcl 		;# defines globals
# Source Tkmaxima/Tryembed.tcl 		;# defines globals?

## source preamble.tcl

###### preamble.tcl ######

# get the number of clicks per second on this machine..
after idle {after 1000 "set ws_openMath(clicks_per_second) \[expr 1.0 *( \[clock clicks\] - [clock clicks])\]" }

catch {
    # the following will be defined only in the plugin
    global embed_args
    array set embed_args [getattr browserArgs]
    proc wm { args } {}
}

## source Readdata.tcl

## source Getdata1.tcl

## source Macros.tcl

## source Proxy.tcl

# from Send-some.tcl
#mike - I hope these can be eliminated or encapsulated
global port magic interrupt_signal _waiting _debugSend
if { $argc == 0 } {
    set port 4444
    set magic "billyboy"
}
set interrupt_signal "<<interrupt fayve>>"

set _waiting 0

set _debugSend 0

## source Send-some.tcl

## source Plotting.tcl

## source Private.tcl

## source Getopt.tcl

## source Parse.tcl

## source Textinsert.tcl

## source Printops.tcl

## source Push.tcl

## source Plotconf.tcl

## source Rk.tcl

## source Adams.tcl

## source Plotdf.tcl

## source Plot2d.tcl

## source Plot3d.tcl

## source NPlot3d.tcl

# obsolete patchold.tcl

## source EOctave.tcl

## source EOpenplot.tcl

## source EMaxima.tcl

## source EHref.tcl

## source Browser.tcl

## source Wmenu.tcl

## source Tryftp2.tcl

## source Myhtml.tcl

## source Myhtml1.tcl

## source Base64.tcl

## source Bitmaps.tcl

## source Tryembed.tcl


global ws_openMath
set ws_openMath(fixedFont) Courier
# the linelength should be long enough to display formatted mathematical
# output from things like maxima, without adjustment, and to allow
# for a margin.
set ws_openMath(linelength) 90

proc genSample { x n } {
    set sample $x
    set m 1
    while { 1 } {
	if { $m >= $n } { return $sample }
    	if { [set tem [expr {2*$m}]] <= $n } {
	    append sample $sample
	    set m $tem
	} else {
	    return [append sample [genSample $x [expr {$n - $m}]]]
	}
    }
}

option add *Button.font [font create -family Courier -size $fontSize]

	
# font measuring is very slow so we cache the result of measuring a line
# of x's.
proc fontMeasure { font size } {
   global  ws_openMath
   set ll $ws_openMath(linelength) 
   if { ![catch {set answer [set $ws_openMath($font,$size,$ll)]} ] } { return $answer}
  set sample [genSample x $ll]
  set  ws_openMath($font,$size,$ll)  [font measure [list $font $size] $sample]
  return $ws_openMath($font,$size,$ll)
}

global fixedFont
set fixedFont Courier
proc getDefaultFontSize { width } {
    global fixedFont
    set answer "10 480"
    catch { 
    set wid1 [fontMeasure $fixedFont 10]
	set guess [expr {round($width/double($wid1) * 10.0)}]
    while { [fontMeasure $fixedFont $guess] < $width && $guess <= 14 } {
        incr guess }
    incr guess -1
    while { [fontMeasure $fixedFont $guess] > $width } { incr guess -1 }
    set answer   [list $guess [fontMeasure $fixedFont $guess]]
    }
    return $answer

}
    
proc getMaxDimensions { } {
  global embed_args
  set dims "800 600"
 if { [catch { set dims "$embed_args(width) $embed_args(height)" } ] } {
   set dims "[expr round(.85* [winfo screenwidth .])] [expr round(.9* [winfo screenheight .])]"} else {
       set dims "[getPercentDim [lindex $dims 0] width .] [getPercentDim [lindex $dims 1] height .]"
   }
  return $dims
}

proc getPercentDim { dim direction win } {
    if { [regexp {([0-9]+)%} $dim junk val] } {
	set dim [winfo $direction $win]
	catch { set dim [expr {round($val * $dim * .01)}] }
	return $dim
    }
    return $dim
}

proc computeTextWinDimensions { win width height } {
  # leave room for scroll bar
   global fixedFont ws_openMath
    # desetq "fsize wid" [getDefaultFontSize [expr {$width -15}]]
    set wid $width
   set fixedFont [xHMmapFont font:fixed:normal:r:3]
   set fsize [xHMfontPointSize $fixedFont]
   
   set lh [expr {$fsize +1}]
   catch {   set lh [font metrics $fixedFont -linespace] }
   oset $win fixedFont $fixedFont
   oset $win fontSize $fsize
   oset $win width $width
   oset $win width_chars $ws_openMath(linelength)
   set hei [expr {round($height/$lh)}]
   oset $win height_chars $hei
   oset $win height [expr {$hei * $lh}]
   oset $win lineheight $lh
   }



proc setFontOptions { fontSize }     {
    global buttonfont entryfont labelfont ws_openMath

    set fsize $fontSize
    if { $fontSize > 10 } { set fsize 12 }
    if { $fsize == 8 } { set entrysize 10 } else {set entrysize $fsize }
    #puts "fsize=$fsize"
    catch {
	set  buttonfont [font create -family Helvetica -size $fsize]
         set  labelfont [font create -family helvetica -size $fsize]
           set  fixedtextfont [font create -family courier -size $fsize]
           set  entryfont [font create -family courier -size $entrysize]
           option add *Button.font $buttonfont
           option add *Label.font $labelfont
           option add *Entry.font $entryfont
           option add  *Dialog.msg.wrapLength 500

    }

}
proc omPanel { w args } {
    global buttonfont entryfont labelfont ws_openMath
    set top [winfo toplevel $w]    
    linkLocal $top omPanel
    if { [info exists omPanel] } {return $omPanel }
    set top [winfo parent $w]
    # 
    if { "$top" == "." } { set top ""}	  
    set win $top.textcommands
    set omPanel $win
    makeLocal $w fontSize
    setFontOptions $fontSize

    global [oarray $top.textcommands]
    set menubar $top.textcommands
    if { [winfo exists $menubar] } {
	return $menubar
    }
    oset $win history ""
    oset $win historyIndex 0
    wmenubar $menubar
    pack $menubar -side top -expand 0 -fill x -anchor nw

    foreach v { back forward  file edit help  } {
	label $win.$v -text $v -font $buttonfont -relief raised
	$menubar add $win.$v
    }
    bind $win.back <Button-1>  "OpenMathMoveHistory $win -1"
    bind  $win.forward <Button-1> "OpenMathMoveHistory $win 1"
    setHelp $win.forward {Move forward in the history of documents visited.}
    setHelp $win.back {Move backward in the history of documents visited.}
 

    ####### begin edit button

    setHelp $win.edit {Bring down a menu with some edit options}
    set m [oget $win.edit menu]
    oset $win showEditBar "show edit bar"
    
   # $m add command -help "Toggle viewing of an editor bar which allows marking text with properties, eg marking to evaluate in maxima"  -textvariable [oloc $win showEditBar] -command "toggleEditBar $win"
    oset $win currentProgram maxima
    oset $win currentProgramInsert 1
    $m add command -underline 0 -help {_eval {Mark the currently selected region of the text window, so that clicking on the region will cause evaluation by a program.  The program specified below is [oget [omPanel %W] currentProgram].}} -label "mark for eval"  -command  "markForProgram \[oget $win textwin\]"

    global evalPrograms
 #   regsub -all eval_ [concat $evalPrograms [info proc eval_*]] "" programs
    set programs $evalPrograms
    foreach v $programs {
	$m add radio -help "Set `$v' to be the current type used by 'mark for eval' for making the selected region sensitive to double clicking." -label "$v" -value $v -variable [oloc $win currentProgram] -command "setTypeForEval $m $v"
    }
    frame $m.program
    $m invoke 1
    pack $m.program

   #  $m add check -help "Toggle whether Mark for Eval should expect the program to insert a result, or should just evaluate for effect." -label "Eval Insert" -onvalue 1 -offvalue 0 -variable [oloc $win currentProgramInsert]
    

    

  # ====begin Help button===    
    set m [oget $win.help menu]
    setHelp $win.help {Offer possible help options, including toggling \
	    whether to show balloon help messages}

    global show_balloons showHelpMessages
    set show_balloons 1 ; set showHelpMessages "Hide Balloon Help"
    $m add command -textvariable showHelpMessages -command {set show_balloons [expr {!$show_balloons}]; if { $show_balloons} {after 500 set showHelpMessages [list "Hide Balloon Help" ]} else {after 500 set showHelpMessages [list "Show Balloon Help" ]}}
    label $m.date -text "Version $ws_openMath(date)"
    $m add window -window $m.date
   
    
    

    
    
  # ====begin File button===
    set m [oget $win.file menu]
    setHelp $win.file {Menu of file options, for saving and preferences} 
    global [oarray $win]
    $m add command -label "Reload" -command {OpenMathOpenUrl [oget [omPanel %W] location] -reload 1 -commandpanel [omPanel %W]} -help {_eval {Reload the current URL displayed in the entry window: [oget [omPanel %W] location]}}
    $m add command -label "Interrupt" -command {omDoInterrupt [oget [omPanel %W] textwin]} -help {Try to interrupt the current remote computations.}
    $m add command -label "Abort" -command {omDoAbort [oget [omPanel %W] textwin]} -help {Try to abort the current remote computation.}
    $m add command -label "Stop" -command {omDoStop [oget [omPanel %W] textwin]} -help {Stop reading the current url or image.}
    $m add command -label "Forget" -command  "forgetCurrent $win"   -help {Move back one in the history, and remove the current one from the history, unless it was the first window.}
    $m add command -label "History" -command  "showHistory $win"   -help {Display the history list, so that one may be selected by clicking.}
    $m add command -label "Base Program" -command  {fileBaseprogram [oget [omPanel %W] textwin]  %W %x %y}   -help {Show and allow altering of the base program, which shows which is the default host for programs to run on.   May also be specified in <body baseprogram= ...> in the .html file.}        
    set new [$m add entry -label {Save file:} -help {_eval {Save to the file list below([oget [omPanel %W] savefilename]).  Not available when running inside Netscape}}]
    $new.entry configure  -textvariable [oloc $win savefilename]
    bind $new.entry  <Return> "saveToFile $win $new.label \[oget $win savefilename\] "
    bind $new.label  <Button-1> "saveToFile $win $new.label \[oget $win savefilename\] "
    $m add command -label "Exit" -command  "destroy ."   -help {Exit this program}        
#    $m add command -label "Print not yet!" -command "puts printing" -underline 0 -help {You may print individual graphs using their menu bars, but printing the whole file is not yet implemented.}
    $m add command -label Preferences -command "fontDialog .fontdialog" -help {set the default font sizes and types}
    if { "[info command console]" == "console" } {
	$m add command -underline 0 -label "Show Tcl Console" \
		-command "console show" -help \
		{This console is used mainly in debugging netmath}
    }



    global location
    button $win.loclabel -text " Url:" -command "OpenMathOpenUrl \[$win.location get\] -commandpanel  $win" -font $labelfont
    setHelp $win.loclabel {Fetch the URL or FILE indicated in the entry box. \
	    A local file is something like file:/home/wfs/foo.om, and a URL \
	    begins with http.}
    
    pack $win.loclabel -side left -fill x -expand 0
    entry $win.location -textvariable [oloc $win location] -width 40
    setHelp $win.location {Address of the current document.  You may modify it and type Enter, to fetch a new document.}
    bind $win.location <Key-Return> "OpenMathOpenUrl \[$win.location get\] -commandpanel  $win"
    pack $win.location  -side left -fill x -expand 1
    label $win.locspace -text " "
    pack $win.locspace -side left -fill x -expand 0

    oset $win history ""
    pack $win -side top -expand 1 -fill x

    ######## make status panel....
    set st $top.status
    frame $st
    oset $win status $st

    set ws_openMath(status_window) $st
    scale $st.scale -showvalue 0 -length 200 -orient horizontal
    label $st.rate -width 35 -font $labelfont -textvariable ws_openMath(load_rate)
    pack $st.rate $st.scale -side left 
    pack $st -side bottom
    return $win
}

proc forgetCurrent { win } {
    makeLocal $win history historyIndex
    set i 0
    if { [llength $history] > 1 } {
	set w [lindex $history $historyIndex]
	set history [lreplace $history $historyIndex $historyIndex]
	# might have caused two identical ones to be next to each other
	if { "[lindex $history $historyIndex]" == "[lindex $history [expr {$historyIndex -1 }]]" } {
	    set history [lreplace $history $historyIndex $historyIndex]
	    set i -1
	}
	if { [lsearch  $history $w] < 0 } {
	    after 2000 "destroy $w"
	}
	oset $win history $history
	OpenMathMoveHistory $win $i
    }
}

proc omDoStop { win } {
    global ws_openMath
    set st $ws_openMath(status_window)
    set var [$st.scale cget -variable]
    if { [regexp {sock[0-9]+} $var sock] } {
	oset $sock done -1
	if { ![catch { close $sock} ] } {
	    
	    append ws_openMath(load_rate) "--aborted"
	}
    }
}
    
    



#
 #-----------------------------------------------------------------
 #
 # setTypeForEval --  insert special editing of options, into MENU for PROGRAM
 #
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc setTypeForEval { menu program } {
    global ws_openMath
    #puts "$menu program"
    set slaves [pack slaves $menu.program ]
    set men $menu.program.$program
    if { [llength $slaves] > 0 } {eval pack forget $slaves}
    if { ![catch { set options $ws_openMath(options,$program) } ] } {
	if { ![winfo exists $menu.program.$program] } {
	    #puts "options=$options"
	   # puts "there"

	    ### set up to add menu items to a new frame
	    set key $menu.program

	    frame $men
	    rename $men $men-orig
	    set body "wmenuInternal $key \$option \$args"
	    oset $menu.program menu $men
	    oset $men items ""
	    oset $key parent $menu
	    proc $men {option args } $body
	    
	    ##### end
	    

	    foreach v $options {
		desetq "key dflt help" $v
		
		if { [catch { set ws_openMath(options,$program,$key)} ] } {
		    set ws_openMath(options,$program,$key) $dflt
		}
		switch [lindex $v 3] {
		    boolean {
			$men add check -label $key -variable ws_openMath(options,$program,$key) -help [concat $program option -$key: $help] -onvalue 1 -offvalue 0
		    }
		    default {
            		$men add entry -label "$key:" -entryvariable ws_openMath(options,$program,$key) -help [concat $program option -$key: $help]

		    }

		}

		
#		label $new.label -text $key:
#		entry $new.entry  -textvariable ws_openMath(options,$program,$key)
#		pack $new.label $new.entry -side top -anchor w -fill x
#		pack $new -fill x
#		setHelp $new [concat $program option -$v: $help]

	    }
	}
	
    }
    catch { pack $men}

}


#
 #-----------------------------------------------------------------
 #
 # getGlobalOptions --  Convert the current global options for program,
 # to an option list:  -key1 value1 -key2 value2 ..
 #
 #  Results: the option list
 #
 #  Side Effects: none
 #
 #----------------------------------------------------------------
#
proc getGlobalOptions { program } {
    global ws_openMath
    set ans ""
    if { ![catch { set options $ws_openMath(options,$program) } ] } {
	foreach v $options {
	    set key [lindex $v 0]
	    set dflt [lindex $v 1]
	    if { ![catch { set val $ws_openMath(options,$program,$key) }] } {
		if { "$val" != "$dflt" } {
		    lappend ans -$key $val
		}
	    }
	}
    }
    return $ans
}


#
 #-----------------------------------------------------------------
 #
 # setGlobalOptions --  set the current global values of the options for PROGRAM
 # according to the values specified in OPTIONLIST.   If a value is not specified
 # use the value supplied in the defaults: $ws_openMath(options,$program)
 #
 #  Results:  none
 #
 #  Side Effects: the entries ws_openMath(options,$program,$key) are changed
 #  for each $key which is an option for program.
 #
 #----------------------------------------------------------------
#
proc setGlobalOptions { program list } {
    global ws_openMath
    if { [catch { set options $ws_openMath(options,$program) } ] } {
	  foreach  v $options {
	      set key [lindex $v 0]
	      set dflt [lindex $v 1]
	      set $ws_openMath(options,$program,$key) \
		      [assoc -$key $list $dflt]
	  }
      }
  }

proc toggleEditBar  {win} {
    makeLocal $win showEditBar editbar
    if { [winfo viewable $editbar] }  {
	pack forget $editbar
	oset $win showEditBar "show edit bar"
    } else { pack $editbar -in $win -side bottom -expand 1 -fill x
		oset $win showEditBar "hide edit bar"
    }
}


proc getPrefixed { prefix  tags } {
    set i [lsearch $tags ${prefix}*]
    if { $i >= 0 } {
	return [string range [lindex $tags $i] [string length $prefix] end]
    } else {
	return ""
    }
}
    
proc programFromTags {tags} {
    if {[lsearch $tags Teval ] < 0 } {
	return ""
    }
    return [getPrefixed program: $tags]
}

proc saveToFile { commandPanel label file } {
    makeLocal $commandPanel textwin
    global ftpInfo
    $label configure -relief sunken
    set lab [$label cget -text]
   # set text [saveText $textwin 0.0 end]
    # save just as text
    set text [$textwin get 0.0 end]
    if { [catch { set fi [open $file w] } ] } {
	set ftpInfo(directory) [string trimleft [file dirname $file] /]
	if { "$ftpInfo(directory)" == "" } { set ftpInfo(directory) . }
	set ftpInfo(data) $text
	ftpDialog $textwin -filename [file tail $file]
	return 0
	# myerror "Could not open file $file"
    }
    puts $fi $text
    close $fi
    $label configure -relief raised -text "wrote $file"
    after 1200 [list $label configure -text $lab]
}

if { [catch { package require Safesock } ] } {
    catch { policy  home }
	# catch {  policy outside }
	
    }


proc mkOpenMath { win  } {
    global    ws_openMath
   
    set w $win
    catch {destroy $w}
    if { [catch { package require Safesock } ] } {
	   # policy network home
	catch {  policy  outside }
    }
    desetq "width height" [getMaxDimensions]
    computeTextWinDimensions $win $width $height 
    makeLocal $win fontSize width_chars height_chars fixedFont
    set font $fixedFont
    
    # puts "fontSize=$fontSize"
    frame $w
    set commandPanel [omPanel $w ]
    oset $w commandPanel $commandPanel
    set prevwindow ""
    catch { set prevwindow [oget $commandPanel textwin] }
    oset $commandPanel textwin $w.text
   # pack $commandPanel -in $w -side top -fill x -pady 2m
   # raise  $commandPanel
    set fontSize2  [expr {2+$fontSize}]
    if { $fontSize2 >= 14 } { set fontSize2 14}
    text $w.text -yscrollcommand "$w.scroll set" \
	 -width $width_chars  -height $height_chars -font $font -wrap word
    bind $w.text <Configure> "resizeSubPlotWindows $w.text %w %h"
    set ws_openMath(currentwin) $w.text
    set ws_openMath(point) end
    $w.text tag bind "currenteval" <Leave> "$w.text tag remove currenteval 0.0 end ; addTagSameRange %W Teval currenteval @%x,%y;"
    $w.text tag config "currenteval" -foreground red
    $w.text tag bind Teval <Double-Button-1> {doInvoke %W @%x,%y }
    $w.text tag bind Teval <Enter> {addTagSameRange %W Teval currenteval @%x,%y; textShowHelp %W Teval @%x,%y "Double clicking (with the left mouse button), in the marked region will cause evaluation. "}
    $w.text tag bind Teval <Leave> {deleteHelp %W}
    $w.text tag config hrule -font {Courier 1} -background black
    $w.text mark set insert 0.0
  # try "#d0d0d0" or "#ffffd0" or yellow
    $w.text tag configure Teval -foreground blue -font [font create -family Courier -size $fontSize]  -border 1 -lmargin1 20

    

    $w.text tag configure bold -font [xHMmapFont font:propor:bold:r:3] -lmargin1 15
    $w.text tag configure plain -font [xHMmapFont font:propor:bold:r:3] -lmargin1 10 
    $w.text tag configure Tresult -font [xHMmapFont font:fixed:bold:r:3] -lmargin1 10
    $w.text tag configure Tmodified -font [xHMmapFont font:fixed:normal:r:3] -background pink -relief sunken -border 1
    $w.text tag configure Thref -font [xHMmapFont font:fixed:normal:r:3]  -foreground blue  -relief flat 

    set lh [oget $win lineheight]
    $w.text tag configure sub -offset [expr {-round($lh*.6) }]
    $w.text tag configure sup -offset [expr {round($lh*.6) }]

    
    oset $w.text counter 0
    # allow some openmath text bindings to take precedence
    bindtags $w.text "OpenMathText [bindtags $w.text]"
    scrollbar $w.scroll -command "$w.text yview"
    pack $w.scroll -side right -fill y
    pack $w.text -expand 1  -fill both
    pack $w -expand 1 -fill both
    if {[winfo exists $prevwindow] } { pack forget [winfo parent $prevwindow] }
    return  $w.text
    
}

#source emaxima.tcl
#source egp.tcl

# Create bindings for tags.

# set ActiveTags {
#   gap-eval
#   gap-eval-insert
#   octave-eval
#   octave-eval-insert
#   face-jump-to-bkmark
#   xlsp-eval
#   xlsp-eval-insert
#   gcl-eval
#   gcl-eval-insert
#   emacs-lisp-eval
#   emacs-lisp-eval-insert
#   mma-eval
#   mma-eval-insert
#   Splus-eval
#   Splus-eval-insert
#   gp-eval
#   gp-eval-insert
#   maple-eval
#   maple-eval-insert
#   shell-eval-region
#   gnuplot-eval
#   xplot-eval
#   maxima-eval
#   maxima-eval-insert
#   dfplot-eval
#   book-shell-eval-insert
#   book-image-insert
#   book-postscript-insert
#   book-tex-math-mode
#   book-elisp-eval
#   book-shell-eval
#  }

global evalPrograms
set evalPrograms {  gp gap gb }
#set ws_openMath(options,maxima) {{doinsert 1 "Do an insertion" boolean}}
#set ws_openMath(options,gp) {{doinsert 1 "Do an insertion" boolean}}
# set ws_openMath(options,openplot) {{doinsert 0 "Do an insertion" boolean}}

# add in Toctave, Topenplot, Thref etc... ie ones with eval_* defined
foreach v [info proc insertResult_*] {
    lappend evalPrograms [string range $v 13 end]
}


#
 #-----------------------------------------------------------------
 #
 # defaultInsertMode --  each program can have a default insert mode.
 #  If the insert method is not noted specifically then it uses the default.
 #  maxima and gp have default to insert.
 #  Results: 0 or 1
 #
 #  Side Effects: none
 #
 #----------------------------------------------------------------
#
proc defaultInsertMode { program } {
    global ws_openMath
    if { [catch {  set dflt [getOptionDefault doinsert $ws_openMath(options,$program)]} ] } { return 1}
 
    if { "$dflt" == "" } {set dflt  1}
    return $dflt
}

proc doInsertp { tags } {
    set program [programFromTags $tags]
   # puts "program=$program," ; flush stdout
    return [getEvalArg -doinsert $tags [defaultInsertMode [programName $program]]]
}


#
 #-----------------------------------------------------------------
 #
 # doInvoke --  invoked when user clicks on WINDOW at INDEX
 # this will either call the program whose tag is in the list of
 # tags at this point, on the expression which is highlighted for this
 # or else call the special code in eval_$program if the latter exists.
 #  Results: none
 #
 #  Side Effects: The modified result of the insert field will be cleared,
 #  and the value there will be changed.
 #----------------------------------------------------------------
#
proc doInvoke { w index } {
    global evalPrograms MathServer
   set tags [$w tag names $index]
    
    $w tag delete sel

    set program [programFromTags $tags]
    if { "$program" == "" } {
	return
    }
   # puts "base=[oget $w baseprogram],w=$w"
   set res [resolveURL $program [oget $w baseprogram]]
   # puts "program=$program,baseprogram[oget $w baseprogram],res=$res"
   
   set MathServer "[assoc server $res [lindex $MathServer 0]] \
	   [assoc port $res [lindex $MathServer 1]]"
   set this [thisRange $w  program:$program $index]
   # puts "this=$this"

   set nextResult ""
   set doinsert [doInsertp $tags]
   # puts "doinsert=$doinsert"
   
   if { $doinsert} {
       set name [getPrefixed name: $tags]
       if { "$name" != "" } {
	   set nextResult [$w tag nextrange result:$name [lindex $this 1]]
	   if { 0 == [llength $nextResult] } {
	       error "No result field with name=$name"
	   }
       } else {
 	   set next [$w tag nextrange Teval [lindex $this 1]]
	   set nextResult [$w tag nextrange Tresult [lindex $this 1]]
	   if {
	       [llength $nextResult] == 0
	       ||    ([llength $next] !=0
	       &&  [$w  compare [lindex $nextResult 0] > [lindex $next 0]] )
	   } {
	    $w insert "[lindex $this 1]+1 char" " " "Tresult"
	     set nextResult [$w tag nextrange Tresult [lindex $this 1]]   
	   # error "no place to put result"
         }
      }
      if { "$nextResult" != "" } {
	  eval $w  tag add Tmodified $nextResult
      }
   } 
   set prog [programName $program]
   if { [info proc eval_$prog] != "" } {
       if {[eval_$prog $program $w $this $nextResult] != 0 }  {
	   error "Failed to eval region"
       }
   } else {
       global err
       if { [catch { sendOneInsertTextWin $program [eval $w get $this] $w $this $nextResult} err ] && [regexp "Can't connect" $err ]} {
	   global xHMpreferences
	   set now [encodeURL [oget $w baseprogram] ]
	   set tem [ldelete $now $xHMpreferences(defaultservers)]
	   if { [tk_dialog .jil 0 "$err: connect to one of $tem?" "" 0 change "keep $now"] == 0 } {
	       set xHMpreferences(defaultservers)  $tem
	       oset $w baseprogram [decodeURL [getBaseprogram]]
	       doInvoke $w $index
	       return
	   } else { return
	   }

       }
   }
	       
   
}

proc getEvalArg { key names {dflt ""} } {

    foreach v $names {
	if { "[string range $v 0 5]" == "Targs "} {
	    return [assoc $key [lrange $v 1 end] $dflt]
	}
    }
    return $dflt
}


#
 #-----------------------------------------------------------------
 #
 # setModifiedFlag --  add the Tmodified tag to the next Tresult field
 #  after the current expression.
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc setModifiedFlag { win index } {
  if { [lsearch [$win tag names $index] Teval] >= 0 } {
      set next [$win tag nextrange Tresult $index]
      if { "$next" != "" } {
	 eval $win  tag add Tmodified $next
      }
  }
}


#
 #-----------------------------------------------------------------
 #
 # insertResult --  replace RESULTRANGE of the text buffer by VALUE,
 #  and clear the Tmodified tag if there is one.
 #  most eval_$program programs will call this to insert their result.
 #  Results:  
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc insertResult { w resultRange value } {
       set tags [$w tag names [lindex $resultRange 0]]
       set value [xHMuntabify $value]
       # append a newline to a multiline result that has no newline after it.
       if { [regexp "\n.*\[^\n]\$" $value ] } {append value "\n"} 
       eval $w delete $resultRange
       # dont lose the whole thing!!
       if { "$value" == "" } { set value " "}
       $w insert [lindex $resultRange 0] $value  [ldelete Tmodified $tags]
   }




#
 #-----------------------------------------------------------------
 #
 # addPreloads --  Tack any preloads or preevals on to the 
 #  command.
 #  Results: the new COMMAND
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#   
proc addPreloads {command program win this } {   
   set preload [getTagsMatching $win ^pre(load|eval):* $this]
    if { "$preload" != "" &&  ![preeval $program $preload] } {
	if { [regexp \{pre(load|eval):(.*)\} $preload junk op url] ||
	[regexp pre(load|eval):(.*) $preload junk op url]} {
	    if { "$op" == "load" } {
		set res [HMgetURL $win $url type]
		append res $command
		set command $res
	    } else {
		append url $command
		set command $url
	    }

    }
   }
   return $command
}


#
 #-----------------------------------------------------------------
 #
 # sendOneInsertTextWin --  send PROGRAM the COMMAND for insertion
 # in the text window WIN at RANGE.   There may be a program specific
 # insertResult_maxima, .. in which case this does the job.   It  
 # is also passed the field of where the command came from.
 # We mark these fields with a tag, since they may get moved by typing
 # before the result comes back.   The com:* tags also provide omDoAbort
 # with the program names that are currently active, so that it can abort.
 #  Results:
 #
 #  Side Effects: until the evaluation succeeds the tags
 #  res:pdata($PROGRAM,result,$i) and a similar com: indicate the
 #  result field, and the command field. 
 #
 #----------------------------------------------------------------
#   
proc sendOneInsertTextWin { program command win this range} {
    set eval [getTagsMatching $win ^eval(sub|):* $this]
    if { "$eval" != "" } {
	if { [regexp \{eval(sub|):(.*)\}  $eval junk op val ]  } {
	    if { "$op" == "sub" } {
		regsub -all "\\&" $val $command val
	    }
	    set command $val
	}
    }
    set command [addPreloads $command $program $win $this ]
    
   # puts "preload=$preload,command:$command"
    set loc [sendOneDoCommand $program $command "sendOneInsertTextWin1 $win $program "]
    if { "$range" != "" } {
	$win tag add res:$loc [lindex $range 0] [lindex $range 1]
    }
    $win tag add com:$loc [lindex $this 0] [lindex $this 1]
}

proc sendOneInsertTextWin1 { win program location } {
    #puts "entering trace:sendOneInsertTextWin1 $win $location"
    #flush stdout
    message "received result"
    set resultRange [$win tag nextrange res:$location 0.0]
    set this [$win tag nextrange com:$location 0.0]
    $win tag delete res:$location com:$location    
#    if { "$resultRange" == ""} {
#	puts "somebody removed result place for $location"
#	return ""
#    }

   if { "[info command insertResult_[programName $program]]" != "" } {
       insertResult_[programName $program] $win $this $resultRange \
	       [uplevel #0 set $location]
   } else {
    insertResult $win $resultRange [uplevel #0 set $location]
   }
     uplevel #0 unset $location
}
   
    

proc xHMuntabify { s } {
    set lis [split $s \n]
    set ans [lindex $lis 0]
    foreach v [lrange $lis 1 end] {
	append ans \n[xHMuntabifyLine $v]
    }
    return $ans
}

proc xHMuntabifyLine { s } {
    set l [split $s \t]
    set ans [lindex $l 0]
    set rest [lrange $l 1 end]
    foreach w $rest {
	set n [expr {[string length $ans]%8}]
	append ans [string range "        " $n end]
	append ans $w
    }
    return $ans
}


#
 #-----------------------------------------------------------------
 #
 # textBbox --  Compute the bounding box of a range of characters
 # starting at IND1 and running to IND2.  
 #
 #  Results: return "x y width height" where x, y are the coordinates
 #  of the upper left corner.
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc textBbox { win ind1 ind2 } {
  
    foreach i { 1 2 } {
	set ind [eval $win index [set ind$i]]
	set ind$i $ind
	set line$i [lindex [split $ind .] 0]
	if { [catch {desetq "x$i y$i xdim$i ydim$i" [eval $win bbox $ind]}] } {
	    # not visible
	    return ""}
    }
    if { $line1 == $line2 } {
	return "$x1 $y1 [expr {$x2-$x1+$xdim2}] [expr {$y2-$y1+$ydim2}]"
    } else {
	set xrange "$x1 $x2+$xdim2"
	set yrange "$y1 $y2+$ydim2"
	
	for { set j $line1 } { $j < $line2 } { incr j } {
	    desetq "x y xdim ydim" [$win dlineinfo $j.0]
	    set xrange [minMax $xrange $x [expr {$x + $xdim}]]
	    set yrange [minMax $yrange $y [expr {$y + $ydim}]]
	}
	desetq "x y xdim ydim" [$win dlineinfo $line2.0]
	set xrange [minMax $xrange $x [expr {$x + $xdim}]]
	set yrange [minMax $yrange [expr {$y + $ydim}]]
	desetq "x1 x2 y1 y2" "$xrange $yrange"
	return "$x1 $y1 [expr {$x2 - $x1}] [expr {$y2 - $y1}]"

    }
}
	
proc textShowHelp { win tag index msg } {
    set this [thisRange $win $tag $index]
    if { "$this" == "" } { return }
    set tags [$win tag names $index]
    if { "$tag" == "Teval" } {
	set program [programFromTags $tags]
	if { "$program" != ""} {
	    set msg [string trimright $msg ". "]
	    append msg " by $program."
	}
	if { [doInsertp $tags] } {
	    append msg " The result will be inserted."
	}
	if { "[getPrefixed name: $tags]" != "" } {
	  append msg  "  The result field is named `[getPrefixed name: $tags]'."
	}
    }
    if { [catch { desetq "x y wid hei" [eval textBbox $win  $this] } ] } {
	# cant get position
	return ""
    }
    set top [winfo toplevel $win]
    
    set x [expr {$x + [winfo rootx $win] - [winfo rootx $top]}]
    set y [expr {$y + [winfo rooty $win] - [winfo rooty $top]}]
    

    
 
    #puts "showHelp $win $x $y $wid $hei"
    showHelp "$win $x $y $wid $hei" $msg
}

proc getTagsMatching { win regexp range } {
    foreach ind $range {
	foreach v [$win tag names $ind] {
	    if { [regexp $regexp $v] } {
		set there($v) 1
	    }
	}
    }
    set dump [eval $win dump -tag $range]
    set i 1
    set ll [llength $dump]
    while { $i < $ll } {
	set v [lindex $dump $i]
	if { [regexp $regexp $v] } {
	    set there($v) 1
	}
	incr i 3
    }
    return [array names there]
}

proc markForProgram { w args } {
    global evalTags
    set win [omPanel $w]
    set program [assoc -program $args [oget $win currentProgram]]
    set range [assoc -range $args [$w tag nextrange sel 0.0]]
    if { "$range" == ""} {
	return ""
    }
    set tags [assoc -tags $args ""]
    if { "$tags" == ""} {
	set tags [list Teval program:$program ]
	set opts [getGlobalOptions [programName $program]]
	if { "$opts" != ""} {  lappend tags [concat Targs $opts] }
    }
    # puts "tags=$tags"
    eval $w tag remove Teval $range
    foreach v [getTagsMatching $w "^Targs |^program:" $range] {
	eval $w tag remove [list $v] $range
    }
    foreach v $tags {eval $w tag add [list $v] $range}
    set insert [doInsertp $tags]
    if { $insert } {
	set nextResult [$w tag nextrange Tresult [lindex $range 1]]
	set next [$w tag nextrange Teval [lindex $range 1]]
	if { [llength $nextResult] == 0 ||
	([llength $next] !=0)
	&&  [$w compare [lindex $nextResult 0] > [lindex $next 0]] } {

	set templates [list " yields " " evaluates to "  \
	    " returns " " produces " " gives "]
        $w mark set tmp [lindex $range 1]	    

	    $w insert tmp [lindex $templates [expr {[clock clicks]%[llength $templates]}]] plain
        $w insert tmp RESULT {Tresult Tmodified}
        $w insert tmp " "  {plain}
    } else { apply $w tag add Tmodified $nextResult}
    
  }
}

global ws_openMath
set ws_openMath(counter) 0
	

## endsource preamble.tcl

## source nconsole.tcl

###### nconsole.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################


proc mkConsole { fr program } {
    catch { destroy $fr }
    global NCtextHelp
    frame $fr
    pack $fr -expand 1 -fill both
    set w $fr.text
    label [set msg $fr.label]  -height 1 -relief sunken \
	    -textvariable ws_openMath(load_rate)
    
    if { ![info exists NCtextHelp] } {
	set NCtextHelp "
	    Bindings:
	    <Return>   This sends the current expression (ie where the insert
	               cursor is)  for evaluation.
	    <Linefeed> (Control-j) This inserts a newline, and is useful
	               for entering multiline input.
	    <Control-k> Kills the current line and puts it in kill ring.
	                Successive control-k's append their output together.
	    <Control-y> Yank out the last kill, Meta-y cycles thru previous
	                kills.
	    <Control-c> Interrupt the current computation.
	    <Alt-p>   Previous input, or if repeated cycle through the previous
	               inputs.  If the current input is not empty, then
	                match only inputs which begin with the current input.
	    <Alt-n>   Like Previous input, but in opposite direction.
	"
    }

    clearLocal $w
     if { 1 || "[bind CNtext <Return>]" == "" } {
 	bind CNtext <Return> "CNeval %W  ; break"
 	bind CNtext <Control-c> "CNinterrupt %W "
 	bind CNtext <Control-u> "CNclearinput %W "
 	bind CNtext "\)"  "CNblinkMatchingParen %W %A"
 	bind CNtext "\]"  "CNblinkMatchingParen %W %A"
 	bind CNtext "\}"  "CNblinkMatchingParen %W %A"
 	bind CNtext <Control-j> "tkTextInsert %W %A ; openMathAnyKey %W %K  %A"
 	bind CNtext <Alt-p>  "CNpreviousInput $w -1"
 	bind CNtext <Alt-n>  "CNpreviousInput $w 1"
    }

    oset $w program $program
    oset $w prompt "% " 
    text $w
    bind $w <Configure> "resizeSubPlotWindows $w %w %h"

    $w tag configure input -foreground blue
    # -relief sunken -borderwidth 1
    bindtags $w [linsert [bindtags $w] 1 CNtext OpenMathText ]

    global ws_openMath

    if { ![regexp $ws_openMath(sticky) input] } {
	append ws_openMath(sticky) {|^input$}
    }

    CNinsertPrompt $w
    $w mark gravity lastStart left
    pack $w -side top -expand 1 -fill both
    pack $msg -side bottom -expand 0 -fill x
    raise $w
}

proc CNinterrupt { w } {
    sendInterrupt [oget $w program]
}

proc CNclearinput { w } {
    if { [$w compare lastStart < insert] } {
	pushl "[saveText $w lastStart insert ]" killRing
	$w delete lastStart insert
    }
}


proc CNinsertPrompt { w } {
    set prompt [oget $w prompt]
    $w mark set insert end
    if { [$w compare insert > "insert linestart"] } {
	$w insert insert \n
    }
    $w insert insert "$prompt" prompt
    $w mark set lastStart [$w index "end -1char"]
    $w see end
    #puts [$w dump -all "[$w index end] -2 lines" end]
}

proc CNeval { w } {
    linkLocal $w inputs
    set prev ""
    if { [$w compare insert < lastStart] } {
	set this [thisRange $w input insert]
	if { "$this" != "" } {
	    set code [eval $w get $this]
	    set prev [string trimright [$w get lastStart end] \n]

	    $w delete lastStart end
	    $w insert lastStart $code input
	}
   }
    set expr [string trimright [$w get lastStart end] \n]
    $w tag add input lastStart end
    lappend inputs $expr
    set tag ""
    #puts "sendind <$expr>"
    set res [sendOneWait [oget $w program] $expr]
    message "received result"

   if { "$res" != "" } {
    if { ![regexp \n $res] } {
	set tag center
	set res "\n $res"
    }

    if { [regexp "\{plot\[23\]d" $res] } {
	linkLocal $w counter
	#puts res=$res
	if { ![info exists counter] } {set counter 0} 
	set name $w.plot[oset $w counter [expr {1 + [oget $w counter]}]]
        eval plot2dData $name $res [getDimensions $w $name]
	set e [$w index end]
	set view [ShowPlotWindow $w $name  "$e $e" "$e $e"  ""]
	append view " -1 line"
    } else {
    $w insert end $res "$tag result"
    }
    
   }
    CNinsertPrompt $w
   if { [info exists view] } {$w yview $view }
    if { "$prev" != "" }  {$w insert insert $prev}
}

proc CNpreviousInput { w direction } {
    linkLocal $w  inputIndex matching
    makeLocal $w inputs
    if { [$w compare insert < lastStart ] } { return }
    set last [lindex [peekLastCommand $w] 1]
    if {  ("[lindex $last 2]" != "ALT_p" && "[lindex $last 2]" != "ALT_n")
    || ![info exists inputIndex] } {
	set inputIndex [expr {$direction < 0 ? [llength $inputs] : -1}]
	set matching [string trim [$w get lastStart end] " \n"]
    }
    lappend inputs $matching
    set n [llength $inputs]
    set j 0
    set matchRegexp "^[quoteForRegexp $matching]"
    while {[incr j] <= $n } {
	set inputIndex [expr {($inputIndex + $direction+ $n)%$n}]
	# [string match "$matching*" [lindex $inputs $inputIndex]]
    	if { [regexp $matchRegexp [lindex $inputs $inputIndex]] } {
	    $w delete lastStart end
	    $w insert insert [lindex $inputs $inputIndex] input
	    $w see end
	    break
	}
    }
}

proc CNblinkMatchingParen { win ch } {
    $win tag delete blink
    if { [string first $ch "\}\)\]"] >= 0 } {
	set tem [$win get "@0,0" "insert"]
	set ind [matchingParen $tem]
	if { "$ind" != "" } {
	    set ind [expr {[string length $tem] - $ind}]
	    catch { after cancel [oget $win blinkAfter] }
	    set i [$win index "insert -$ind chars"]
	    $win tag add blink "$i" "$i +1char"
	    $win tag configure blink -foreground red
	    oset $win blinkAfter [after 1000 $win tag delete blink]
	}
    }
}


#
 #-----------------------------------------------------------------
 #
 # matchingParen --  Return index of STRING which a close paren
 #   would match if added to end.
 #  Results: index
 #
 #  Side Effects: none
 #
 #----------------------------------------------------------------
#
proc matchingParen { s1  } {
    set s $s1
    set ind [string length $s]
    set count -1
    regsub -all "\[\01\02\]" $s x s
    regsub -all "\[\]\}\)\]" $s "\01c" s
    regsub -all "\[\[\{\(\]" $s "\01o" s
    set lis [split $s \01]
    set n [llength $lis]
    while { [incr n -1] > 0 } {
	set v [lindex $lis $n]
	incr ind -[string length $v]
	set c [string index $v 0]
	if { "$c" == "c" } {
	    incr count -1
	} else { incr count }
	if { $count == 0 } {
	    return $ind
	}
    }
}

## endsource nconsole.tcl
## source string.tcl

###### string.tcl ######



#
#-----------------------------------------------------------------
#
# trimSpace --  If a STRING contains no embedded newlines, then remove
#  the surrounding whitespace, otherwise just remove trailing whitespace.
#  
#  Results:
#
#  Side Effects: none
#
#----------------------------------------------------------------
#
proc trimSpace { ans } {
    	if { ![regexp "\n" $ans] } {
	    set ans [string trim $ans "\n \t"]
	} elseif { [regexp "^\[\n\t \](\[^\n\]+)\[\n\t \]\$" $ans junk item] } {
	    set ans [string trim $ans "\n \t"]
	} else {
	    # set ans [string range $ans 0 [expr {[string length $ans] - 2}]]
	    # try to make multiline things start with ans
	    set ans \n[string trimleft $ans \n]
	}
    return $ans
	
    if { [regexp "^\[\n\t \]*(\[^\n\]+)\[\n\t \]*\$" $ans junk item] } {
	set ans [string trim $ans "\n \t"]
    } elseif { [regexp "\n" $ans] } {
	set ans [string trim $ans "\n \t"]
	return "\n$ans"
    }    else { set ans [string trimright $ans "\n \t"] }
    return $ans
}


#
#-----------------------------------------------------------------
#
# genword --  make a string by copying STRING a total of COUNT times 
#
#  Results:string
#
#  Side Effects: none
#
#----------------------------------------------------------------
#
proc genword { string count } {
    set ans "" 
    while { [incr count -1] >= 0 } { append ans $string }
    return $ans
}

## endsource string.tcl
#source eopenplot.tcl


array set xmaximaPreferences {fontAdjust 0 }
catch { source ~/xmaxima.ini }


proc getkey {key lis } {
   set tem [ lsearch $lis $key ]
    if { $tem >= 0} {lindex $lis [expr 1 + $tem]}
}

proc usage {} {
    puts {usage: xmaxima [options] [url]}
    puts {           If given, [url] will be opened in the help browser instead}
    puts "           of the default starting page."
    puts "options:"
    puts "    --help: Display this usage message."
    puts "    -l <lisp>, --lisp=<lisp>: Use lisp implementation <lisp>."
    puts "    --use-version=<version>: Launch maxima version <version>."
}

proc doit { fr } {
    global NCtextHelp ws_openMath xmaximaPreferences argv argv0 env

    #mike Move this in from being at the global level
    setMaxDir

    if {[winfo exists $fr]} {catch { destroy $fr }}

    set ws_openMath(options,maxima) {{doinsert 0 "Do an insertion" boolean}}
    frame .browser
    set firstUrl file:/[file join $ws_openMath(maxima_xmaximadir) "intro.html"]

    set maxima_opts {}
    if { [lsearch $argv "--help"] > -1 } {
	usage
	exit 0
    }
    set lisp_pos [lsearch -exact $argv "--lisp=*"]
    if { $lisp_pos > -1 } {
	set arg [lindex $argv $lisp_pos]
	set prefix_end [expr [string length "--lisp="] - 1]
	set lisp [string replace $arg 0 $prefix_end]
	lappend maxima_opts -l $lisp
	set argv [lreplace $argv $lisp_pos $lisp_pos]
    }
    set lisp_pos [lsearch -exact $argv "-l"]
    if { $lisp_pos > -1 } {
	set lisp [lindex $argv [expr $lisp_pos + 1]]
	lappend maxima_opts -l $lisp
	set argv [lreplace $argv $lisp_pos [expr $lisp_pos + 1]]
    }
    set version_pos [lsearch -exact $argv "--use-version=*"]
    if { $version_pos > -1 } {
	set arg [lindex $argv $version_pos]
	set prefix_end [expr [string length "--use-version="] - 1]
	set version [string replace $arg 0 $prefix_end]
	lappend maxima_opts -u $version
	set argv [lreplace $argv $lisp_pos $version_pos]
    }
    if { [llength $argv] == 1 } {
	set firstURL [lindex $argv 0]
    } elseif { [llength $argv] > 1 } {
	puts "xmaxima: Error: arguments \"$argv\" not understood."
	exit 1
    }

    if { [auto_execok $ws_openMath(xmaxima_maxima)] != "" } {
	set ws_openMath(localMaximaServer) "$ws_openMath(xmaxima_maxima) $maxima_opts -p [file join $ws_openMath(maxima_xmaximadir) server.lisp] -r \":lisp (progn (user::setup PORT)(values))\" &"
    } else {
	if { [info exists env(XMAXIMA_MAXIMA)] } {
	    puts "xmaxima: Error. maxima executable XMAXIMA_MAXIMA=$env(XMAXIMA_MAXIMA) not found."
	    exit 1
	} else {
	    # A gruesome hack. Normally, we communicate to the maxima image
	    # through the maxima script, as above. If the maxima script is not
	    # available, as may happen on windows, directly talk to the GCL 
	    # saved image. jfa 04/28/2002
	    set env(MAXIMA_INT_LISP_PRELOAD) \
		"[file join $ws_openMath(maxima_xmaximadir) server.lisp]"
	    set env(MAXIMA_INT_INPUT_STRING) \
		":lisp (progn (user::setup PORT)(values));"
	    set ws_openMath(localMaximaServer) "[file join $ws_openMath(maxima_verpkglibdir) binary-gcl maxima] -eval \"(run)\" -f &"
	}
    }

    OpenMathOpenUrl $firstUrl -toplevel .browser
    frame $fr
    pack $fr -expand 1 -fill both -side top
    pack .browser -side bottom
    set m [oget .browser.textcommands.file menu]
    $m add command -underline 0 -label "Toggle Visibility of $fr" -command "if { \[catch {pack info $fr} \] } {packBoth $fr .browser} else { pack forget $fr}"
    packBoth $fr .browser
    
    set men [CMmenu $fr]
    set w $fr.text
    clearLocal $fr.text
    oset $w heightDesired 80%
    oset $men textwin $w
    set ws_openMath(maximaWindow) $w
    #text $fr.text 
    #label [set msg $fr.label]  -height 1 -relief sunken \
#	    -textvariable ws_openMath(load_rate)
    
    if { ![info exists NCtextHelp] } {
	set NCtextHelp "
	    Bindings:
	    <Return>   This sends the current expression (ie where the insert
	               cursor is)  for evaluation.
	    <Linefeed> (Control-j) This inserts a newline, and is useful
	               for entering multiline input.
	    <Control-k> Kills the current line and puts it in kill ring.
	                Successive control-k's append their output together.
	    <Control-y> Yank out the last kill, Meta-y cycles thru previous
	                kills.
	    <Control-c><Control-c> Interrupt the current computation.
	    <Alt-p>   Previous input, or if repeated cycle through the previous
	               inputs.  If the current input is not empty, then
	                match only inputs which begin with the current input.
	    <Alt-n>   Like Previous input, but in opposite direction.
	"
    }

    closeMaxima $w
    clearLocal $w
     if { 1 || "[bind CNtext <Return>]" == "" } {
 	bind CNtext <Return> "CMeval %W  ; break"
 	bind CNtext <Control-c><Control-c> "CMinterrupt %W "
 	bind CNtext <Control-u> "CNclearinput %W "
 	bind CNtext "\)"  "CNblinkMatchingParen %W %A"
 	bind CNtext "\]"  "CNblinkMatchingParen %W %A"
 	bind CNtext "\}"  "CNblinkMatchingParen %W %A"
 	bind CNtext <Control-j> "tkTextInsert %W %A ; openMathAnyKey %W %K  %A"
 	bind CNtext <Alt-p>  "CNpreviousInput $w -1"
        bind CNtext <Alt-n>  {sendMaxima %W ":n\n"}
        bind CNtext <Alt-s>  {sendMaxima %W ":s\n" }
	bind CNtext <Control-Key-c>  {tk_textCut %W ;break}
	bind CNtext <Control-Key-v>  {tk_textPaste %W ;break}
    }

    # oset $w program $program
    oset $w prompt "% " 
    catch { destroy $w } ;
    frame $fr.bottom -height 2
    $fr.bottom config -cursor double_arrow
    bind  $fr.bottom <B1-Motion> "changeSize $w %Y"
    pack $fr.bottom -side bottom -fill x
   
    text $w -background white -yscrollcommand "$fr.scroll set"
    set ws_openMath($w,inputTag) input
    resetMaximaFont $w
    scrollbar $fr.scroll -command "$w yview"
    pack $fr.scroll -side right -fill y
    bind $w <Destroy> "closeMaxima $w"
    
    $w mark set lastStart end
    $w mark gravity lastStart left
    bind $w <Configure> "resizeSubPlotWindows $w %w %h; resizeMaxima $w %w %h"

    $w tag configure input -foreground blue
    # -relief sunken -borderwidth 1
    bindtags $w [linsert [bindtags $w] 1 CNtext OpenMathText ]

    global ws_openMath

    if { ![regexp  input $ws_openMath(sticky)] } {
	append ws_openMath(sticky) {|^input$}
    }
    pack $fr.text -expand 1 -fill both -side left
    desetq "width height"  [getMaxDimensions]
    wm geometry . ${width}x${height}
    update
    
    if { [winfo height $fr] > .8 * [winfo height .]  } {
	$fr.text config -height 15
    }
    

    wm title . xmaxima

    #mike Defer the starting of maxima until the interface has been built
    runOneMaxima $w
  

}

proc textWindowWidth { w } {
    set font [$w cget -font]
    set w20 [font measure [$w cget -font] -displayof $w "01234567890123456789"]
    return [expr round(floor([winfo width $w]*20.0/$w20))]
}
    

proc resizeMaxima { win width height } {
    linkLocal $win pid
    if { [info exists pid] && $pid!=-1 } {
	set wid [expr [textWindowWidth $win]-6]
	sendMaxima $win ":lisp-quiet (setq linel $wid)\n"
    }
}

proc packBoth {fr browser} {
    pack forget $fr $browser
    pack $fr -expand 1 -fill both -side top
    pack $browser -side bottom -fill x
}

proc CMmenu { win } {
    global buttonfont ws_openMath
    set menubar $win.textcommands
    set win $menubar
    if { [winfo exists $menubar] } {
	return $menubar
    }
    wmenubar $menubar
    pack $menubar -side top -expand 0 -fill x -anchor nw
    foreach v { file edit help  } {
	label $win.$v -text $v -font $buttonfont -relief raised
	$menubar add $win.$v
    }


    ####### begin help button

    setHelp $win.help {Bring down a menu with some help options}
    set m [oget $win.help menu]
    #oset $win showHelpBar "show help bar"
    $m add command -underline 0 -label {Maxima Help} -help {Visit local maxima help file in html} -command {OpenMathOpenUrl file:/[file join $ws_openMath(maxima_verpkgdatadir) doc html maxima_toc.html]}
     $m add command -underline 0 -label {Netmath} -help {Visit netmath page} -command {OpenMathOpenUrl http://www.ma.utexas.edu/users/wfs/netmath/netmath.html}
     $m add command -underline 0 -label {Run Tests} -help {Run the test files in the doc/*.mac} -command "sendMaxima \[oget $win textwin\] {:lisp (progn (xchdir \"[file join $ws_openMath(maxima_verpkgdatadir) tests]\")(load \"tests.lisp\"))\n}"

 
 
    

    
    ####### begin file button

    setHelp $win.file {Bring down a menu with some file options}
    set m [oget $win.file menu]
    #oset $win showFileBar "show file bar"
    $m add command -underline 0 -label {Toggle Browser Visibility} -help {Toggle display of Browser} -command {if { [catch { pack info .browser }] } { pack .browser -side bottom } else { pack forget .browser }} 
    $m add command -underline 0 -label {Exit} -command "destroy ." -help  "End this session of Maxima"
     $m add command -underline 0 -label {Interrupt   C-c C-c} -command "CMinterrupt \[oget $win textwin\]" -help  "Interrupt the Maxima process and reset the filter"
    $m add command -underline 0 -label {Restart} -command "runOneMaxima \[oget $win textwin\]" -help  "Kill the Maxima process and reset the filter, and start a new one"
#     $m add command -underline 0 -label {Preferences} -command "xmaximaPreferences" -help  "Set Preferences for Xmaxima saved in ~/xmaxima.ini"
     $m add command -underline 0 -label {Preferences} -command "fontDialog .preferences" -help  "Set Preferences for Xmaxima and Netmath saved in ~/netmath.ini"
    if { "[info command console]" == "console" } {
	$m add command -underline 0 -label "Show Tcl Console" \
		-command "console show" -help \
		{This console is used mainly in debugging xmaxima}
    }



     ####### begin edit button

    setHelp $win.edit {Bring down a menu with some edit options}
    set m [oget $win.edit menu]
    #oset $win showEditBar "show edit bar"
    $m add command -underline 0 -label {Previous Input   Alt-p} -command "CNpreviousInput \[oget $win textwin\] -1" -help  "Insert previous inputs matching what is already typed"
    $m add command -underline 0 -label {Clear input   C-u} -command "CNclearinput \[oget $win textwin\]" -help  "Erase the current input"
    $m add command -underline 0 -label {Cut   C-c} -command "tk_textCut \[oget $win textwin\]" -help  "Cut the highlighted region, ready for pasting"
    $m add command -underline 0 -label {Paste   C-v} -command "tk_textPaste \[oget $win textwin\]" -help  "Paste the last cut region"
    pack $menubar -side top
    return $win
}

proc xmaximaPreferences { } {
    global xmaximaPreferences
    catch {destroy .prefs}
    toplevel .prefs
    proc mkentry { w var text } { set fr $w ; frame $fr
    uplevel 1 append topack [list " $fr"]
    label $fr.lab -text "$text"
    entry $fr.e -width 20 -textvariable $var
    pack $fr.lab $fr.e -side left -padx 3 }
    set i 0
    foreach v [array names xmaximaPreferences] {
	mkentry .prefs.it[incr i] xmaximaPreferences($v) "$v: "
    }
    eval pack $topack -side bottom
    button .prefs.save -text save -command {set f [open ~/xmaxima.ini w] ;
    puts $f "array set xmaximaPreferences {"
    foreach {v w} [array get xmaximaPreferences *] {puts $f [list $v $w]}
    puts $f "}"
    close $f
    }

    button .prefs.apply -text apply -command {
	if { [catch { resetMaximaFont .maxima.text } ] } {
	    error  "fontAdjust must be an integer not $xmaximaPreferences(fontAdjust)"
	}
	    

    }

    button .prefs.exit -text "exit preferences" -command {destroy .prefs}
    pack .prefs.save .prefs.apply .prefs.exit -side bottom -fill x


}



proc resetMaximaFont { w } {
    global xmaximaPreferences
    $w config -font [xHMmapFont font:fixed:normal:r:[expr $xmaximaPreferences(fontAdjust) + 3]]
}




proc CMeval { w } {
    linkLocal $w inputs
    set prev ""
    #puts "CMeval $w, [$w compare insert < lastStart]"
    if { [$w compare insert < lastStart] } {
	set this [thisRange $w input insert]
	if { "$this" != "" } {
	    set code [eval $w get $this]
	    set prev [string trimright [$w get lastStart end] \n]

	    $w delete lastStart end
	    $w insert lastStart $code input
	}
   }
#    puts "expr=<[$w get lastStart end]>"
 #  puts "tags=[$w tag names insert],insert=[$w index insert]"
   if { [lsearch [$w tag names insert] insert] >= 0 } {
       $w mark set lastStart [lindex [$w tag prevrange input] 0]
   }
    set expr [string trimright [$w get lastStart end] \n]
    if { ![regexp "^\[ \n\t]*:|\[;\$]\$" $expr] } {
	$w insert insert "\n"
	$w see insert
	if { [oget $w atMaximaPrompt] } {
	return
	}
    }
    
    $w tag add input lastStart "end -1char"
    $w mark set  lastStart "end -1char"
    lappend inputs $expr
    set tag ""
#    puts "sending <$expr>"
    # set res [sendMaxima $w $expr ]
     set res [sendMaxima $w $expr\n ]
    # set res [sendMaxima $w $expr ]
   # puts "[$w dump -all "lastStart linestart" end]"
    #message "send form"

}

proc acceptMaxima { win port filter } {
    set count 3 ;
    catch { close [oget $win server] }
    while {[incr count -1 ] > 0 } {
	if { ![catch {oset $win server [socket -server "runMaxima $win $filter" $port]} ] } {
	    # puts "server sock [oget $win server]"
	    return $port
    } else { incr port   }
 }
    return -1
}

proc openMaxima { win filter } {
    global ws_openMath env
    set port [acceptMaxima $win 4008 $filter]
    if { $port >= 0 } {
	set com "exec "
	append com    $ws_openMath(localMaximaServer)
	regsub PORT $com $port com
	if { [info exists env(MAXIMA_INT_INPUT_STRING)] } {
	    regsub PORT $env(MAXIMA_INT_INPUT_STRING) $port env(MAXIMA_INT_INPUT_STRING)
	    #puts env(MAXIMA_INT_LISP_PRELOAD)=$env(MAXIMA_INT_LISP_PRELOAD)
	    #puts env(MAXIMA_INT_INPUT_STRING)=$env(MAXIMA_INT_INPUT_STRING)
	}
	#puts com=$com
	if { [catch { eval $com } err ] } {
	    tk_messageBox -title "Error" -message "Can't execute $ws_openMath(localMaximaServer) : $err" }
    } else {error "could not open a socket " }
}

proc runMaxima { win  filter sock args } {
    linkLocal $win server
    oset $win maximaSocket $sock
    fconfigure $sock -blocking 0 -translation lf
    fileevent $sock readable "$filter $win $sock"
    if { [info exists server] } {
	# puts "closing server $server" 
    catch { 
    close $server
    unset server
    }
} else { # puts "server unset ??"
}
}

proc closeMaxima { win } {
    global pdata
    linkLocal $win maximaSocket pid 
    foreach v [array names pdata maxima*] { unset pdata($v) }

    if {[info exists pid] && $pid != "" && [string is int $pid]} {
	catch {
	    CMkill -TERM $pid
	    unset pid
	}
    }

    if {[info exists maximaSocket] && $maximaSocket != ""} {
	catch {
	    close $maximaSocket
	    unset maximaSocket
	}
    }
}




#
 #-----------------------------------------------------------------
 #
 # maximaFilter --  filter the output on SOCKET inserting in WINDOW
 # recognizing
 #     \032\032:file:line:charpos\n
 #               -->redisplay in other window
 # \032\031tcl: command \n
 #           --> eval tcl command o
 #       
 # 
 #  Results: none
 #
 #  Side Effects:  input is read from SOCK and WIN has items displayed.
 #
 #----------------------------------------------------------------
#
#todo fix sendMaximaWait win expr
proc maximaFilter { win sock } {
    linkLocal $win  plotPending
    global pdata
    if { [eof $sock] } {
	# puts "at end"
	close $sock
	return ""
    }
    set it [read $sock]
   # puts "read=<$it>"
    if { [string first "\032\032" $it] >= 0 &&
     [regexp  -indices "\032\032(\[^:]+):(\[0-9]+):\[^\n]*\n" $it junk file line] } {
	 
	 dblDisplayFrame [getMatch $it $file] [getMatch $it $line]
	 append res [string range $it 0 [expr { [lindex $junk 0] -1 } ]]
	 append res [string range $it [expr { 1+[lindex $junk 1]}] end]
	 set it $res
     }
     if { [string first "\032\031tcl:" $it] >= 0 &&
            [regexp  -indices "\032\031tcl:(\\[^\n]*)\n" $it junk com]
     } {
	 eval $com
	 append res [string range $it 0 [expr { [lindex $junk 0] -1 } ]]
	 append res [string range $it [expr { 1+[lindex $junk 1]}] end]
	 set it $res
     }
    # puts it=<$it>
    if { [regexp -indices "\{plot\[d23]\[fd]" $it inds] } {
	set plotPending [string range $it [lindex $inds 0] end]
	set it ""
	if { [regexp {\(C[0-9]+\) $} $it ff] } {
	    regexp "\{plot\[d23]\[df].*\}" $ff it
#	set it $ff
	}
    }
    if { [info exists plotPending] } {
	#puts "plotPending=<$plotPending>,it=<$it>"
	append plotPending $it
	set it ""
	if { [regexp -indices "\n\\(D\[0-9\]+\\)" $plotPending  inds] } {
	    set it [string range $plotPending [lindex $inds 0] end]
	    set plotPending [string range $plotPending 0 [lindex $inds 0]]
	    set data $plotPending
	    unset plotPending
	    #puts itplot=<$it>,$inds
	    #puts plotdata=<$data>
	    doShowPlot $win $data

	}
    }
    
    $win insert end $it "output"
    $win mark set  lastStart "end -1char"
    if { [regexp {\(C[0-9]+\) $|\(dbm:[0-9]+\) $|([A-Z]+>[>]*)$} $it junk lisp]  } {
  #puts "junk=$junk, lisp=$lisp,[expr { 0 == [string compare $lisp {}] }]"
	 #puts "it=<$it>,pdata={[array get pdata *]},[$win index end],[$win index insert]"

	if { [info exists pdata($sock,wait) ] && $pdata($sock,wait) > 0 } {
	    #puts "it=<$it>,begin=$pdata($sock,begin),end=[$win index {end linestart}]"
	    #puts dump=[$win dump -all "insert -3 lines" end]
	    setAct pdata($sock,result) [$win get $pdata($sock,begin) "end -1char linestart" ]
	    #puts result=$pdata($sock,result)
	    set pdata($sock,wait) 0
	 }
	$win mark set lastStart "end -1char"
	$win tag add  input "end -1char" end 
	oset $win atMaximaPrompt [expr { 0 == [string compare $lisp ""] }]
	
    }
    $win see end
    return
}

proc littleFilter {win sock } {
    global pdata
    set tem [gets $sock]
    append pdata(maximaInit,$sock) $tem
    debugsend "littlefilter got:<$tem>"
    if { [regexp {pid=([---0-9]+)} $tem junk pid] } {
	fileevent $sock readable ""
	oset $win pid $pid
	oset $win socket $sock
    }
}

if { ![info exists ws_openMath(timeout)] } {
    
    set ws_openMath(timeout) 20000
}

proc runOneMaxima { win } {
    global ws_openMath
    closeMaxima $win
    linkLocal $win pid
    set pid -1
    openMaxima $win littleFilter
    while { $pid == -1 } {  
	set af [after $ws_openMath(timeout) oset $win pid -1 ]
	# puts "waiting pid=$pid"
	vwait [oloc $win pid]
	after cancel $af
	if { $pid  == -1 } {
	    if { "[info  command console]" != "" } { console show }
	    if { [tk_dialog .jim ask {Starting maxima timed out.  Wait longer?} \
		    {} yes  no yes ] } {
		list
	    } else {
		closeMaxima $win
		set err   "runOneMaxima timed out"
		global pdata
		if { [info exists pdata(maximaInit,[oget $win socket])] } {
		    append err : $pdata(maximaInit,[oget $win socket])
		}
		error $err
	    }
	}
    }
    set res [list [oget $win pid] [oget $win socket] ]
    set sock [oget $win socket]
    global pdata
    set pdata(maxima,socket) $sock
    fileevent $sock readable  "maximaFilter $win $sock"
    return $res

}

proc sendMaxima { win form } {
    linkLocal $win maximaSocket
    if { ![regexp "\[\$;\]\[ \t\n\r\]*\$" $form ] } {
	# append form ";"
    }
    puts -nonewline $maximaSocket $form
    flush $maximaSocket
}


proc sendMaximaWait { win form {timeout 20000 }} {
    linkLocal $win maximaWait 

    set form [string trimright $form "\n \t\r"]
    
    if { ![regexp "\[\$;\]|^\[ \t]*:" $form ] } {
	 append form ";"
    }
    sendMaximaCall $win $form\n [list oset $win maximaWait 1]
    set maximaWait -1
    set af [after $timeout oset $win maximaWait -1]
    vwait [oloc $win maximaWait]
    after cancel $af
    if { $maximaWait > 0 } {
	global pdata 
	return [trim_maxima $pdata([oget $win maximaSocket],result)]
    } else {
	error "sendMaximaWait $form timed out"
    }
}
    


#
 #-----------------------------------------------------------------
 #
 # sendMaximaCall --  send FORM to maxima process in WIN
 # and when it gets the result have it execute CALL
 #
 #  Results: none
 #
 #  Side Effects: maxima executes form and then call may
 #  do something like insert it somewhere in a buffer.   
 #
 #  # todo: should probably make it so this guy looks at maxima c, d numbers
 #    and matches results ..
 #----------------------------------------------------------------
#
proc sendMaximaCall { win form call } {
    linkLocal $win maximaSocket
    global pdata
    set begin [$win index lastStart]
    if { [regexp {C([0-9]+)} [$win get "$begin linestart" $begin] junk \
	    counter ] } {
	#	set af [after 5000 set pdata($maximaSocket,wait) -1]
	set pdata($maximaSocket,wait) 1
	
	set pdata($maximaSocket,begin) $begin
    } else {
	catch { unset pdata($maximaSocket,wait) }
    }
    puts -nonewline $maximaSocket $form
    flush $maximaSocket
    if { [info exists counter] } {
	setAction pdata($maximaSocket,result) $call
    }
}

proc setAction { var action } {
    global _actions
    set _actions($var) $action
}

proc setAct { var val } {
    global _actions 
    uplevel #0 set $var [list $val]
    if { [info exists _actions($var)] } {
	uplevel #0 $_actions($var)
	unset _actions($var)
    }
}

proc CMresetFilter { win } {
    set sock [oget $win maximaSocket]
    fileevent $sock readable "maximaFilter $win $sock"
}

proc CMkill {  signal pid } {
    global ws_openMath
    if { $pid > 0 } {
	if { [info command "winkill"] == "winkill" } {
	    winkill -pid $pid -signal $signal
	} else {
	    exec $ws_openMath(kill) $signal $pid
	}
    }
}

proc CMinterrupt { win } {
    global ws_openMath
    CMkill   -INT [oget $win pid]  
    CMresetFilter $win
}


proc doShowPlot { w data } {
        global xHMpreferences
	#puts data=$data
    set name [plotWindowName $w]
    set command [lindex [lindex $data 0] 0]
    if { "$command" == "plotdf" } {
	set command [lindex $data 0]
    } else {
	lappend command -data [lindex $data 0]
    }
    lappend command -windowname $name
#	puts $command
	eval $command
#	return
	set e [$w index end]
	set view [ShowPlotWindow $w $name  "$e $e" "$e $e"  ""]
	if { "$view" == "" } { return }
	append view " -1 line"
	set tem [$w dump -window $view end]
	global billy
	set billy $tem
	if { [llength $tem] == 3 } {
	    after 80 $w see [lindex $tem 2]
	    #after 400 $w see [lindex $tem 2]
	    #puts "	    after 400 $w see [lindex $tem 2]"
	}
    }


proc dblDisplayFrame { location line } {
    OpenMathOpenUrl $location
    set panel [omPanel .]
    set w [oget $panel textwin]
    $w tag remove currentLine 0.0 end
    $w tag add currentLine "$line.0" "$line.0 lineend"
    $w tag config currentLine -foreground red
    set beg [lindex [split [$w index "@0,0"] .] 0]
    set end [lindex [split [$w index "@0,3000"] .] 0]
    # puts "line=$line,beg=$beg,end=$end"
    if { "$beg" != "" &&  ( $line < $beg + 3 || $line > $end - 3) } {
	$w yview [expr $line - 3]
    }
    $w see $line.0
}
	


#
 #-----------------------------------------------------------------
 # required:
 #
 # trim_maxima --  takes STRING and trims off the prompt
 # and trailing space if desired.   Usually single line results
 # have their white space completely trimmed, while multiline
 # results will be left so that they display properly from left margin
 #
 #  Results:  a string with white space trimmed off
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc trim_maxima { string } {
    debugsend "in trim_maxima input=<$string>"
    if { [string first \n $string] == 0 } {
	set string [string range $string 1 end]
    }
    if { [regexp -indices "(^|\n)(\\(D\[0-9\]+\\))" $string all junk inds] } {
        set len [expr {[lindex $inds 1]  - [lindex $inds 0] }]
        set repl [genword " " $len]
        set ans [string range $string 0 [expr {[lindex $inds 0 ] -1}]]
        append ans $repl
        append ans [string range $string  [expr {[lindex $inds 1 ] +1}] end ]
	debugsend "in trim_maxima ans=<$ans>"
	set string [trimSpace $ans]

    }
    return $string
}

proc dshow { args  } {
    foreach v $args { append ans $v=[uplevel 1 set $v], }
    puts $ans
}
proc maxima_insert { w this next val args } {
    catch { 
    set res [uplevel #0 set $val]
    }
    catch { 
    insertResult_maxima $w $this $next [trim_maxima $res]
    }
}

proc eval_maxima { prog win this nextResult } {
    global ws_openMath
    set w $ws_openMath(maximaWindow)
    linkLocal $w maximaSocket
    set form [string trimright [eval $win get $this] " \t\n;$"]
    set form [addPreloads $form maxima $win $this]
    if { "[lindex $nextResult 0]" != "" } {
	sendMaximaCall $w "$form;\n" [list maxima_insert $win $this  $nextResult pdata($maximaSocket,result)]
	
#         set res [sendMaximaWait $ws_openMath(maximaWindow) "$form;"]
#	insertResult_maxima $win $this  $nextResult $res
    } else {
	sendMaxima $ws_openMath(maximaWindow) "$form;\n"
    }
    return 0
}





proc changeSize { win  y } {
    set del 0
    set tem [expr { [winfo rooty $win] + [winfo height $win] } ]
    set del [expr {abs($y-$tem) <20 ? 0: $y-$tem < 0 ? -1 : 1 }]
    if { $del } {
	set h [$win cget -height]
	incr h $del
	if { $h >= 1 } {
	    $win config -height $h
    }   }
	
}
