# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: xmaxima.tcl,v 1.15 2002-09-06 01:23:27 mikeclarkson Exp $
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
# Source Tkmaxima/Private.tcl 		;# can be autoloaded
# Source Tkmaxima/Getopt.tcl 		;# can be autoloaded
# Source Tkmaxima/Parse.tcl 		;# sets global variables

## source preamble.tcl

###### preamble.tcl ######

# get the number of clicks per second on this machine..
after idle {after 1000 "set ws_openMath(clicks_per_second) \[expr 1.0 *( \[clock clicks\] - [clock clicks])\]" }

catch {
    # the following will be defined only in the plugin
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

# source Send-some.tcl

## source plotting.tcl

###### plotting.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################
## source plotconf.tcl

###### plotconf.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

## source private.tcl

## source getopt.tcl

## source parse.tcl

## source textinsert.tcl

###### textinsert.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

proc mkTextItem { c x y args  } {
    set font [assoc -font $args {Helvetica 14}]
    set tags [assoc -tags $args {}]
    set item [$c create text $x $y -text " " -width 440 -anchor n -font $font -justify left]
    append tags text
    foreach v $tags { $c addtag $v withtag $item}
    $c bind text <1> "textB1Press $c %x %y"
    $c bind text <B1-Motion> "textB1Move $c %x %y"
    $c bind text <Shift-1> "$c select adjust current @%x,%y"
    $c bind text <Shift-B1-Motion> "textB1Move $c %x %y"
    $c bind text <KeyPress> "textInsert $c %A"
    $c bind text <Return> "textInsert $c \\n"
    $c bind text <Control-h> "textBs $c"
    $c bind text <BackSpace> "textBs $c"
    $c bind text <Delete> "textDel $c"
    $c bind text <2> "textPaste $c @%x,%y" 
}


## endsource textinsert.tcl
## source printops.tcl

###### printops.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

### FIXME: fix a4 size !
global paperSizes printOptions
set paperSizes {{letter 8.5 11} { A4 8.5 11} {legal 8.5 13}} 

set printOptions { 
    { landscape  1 "Non zero means use landscape mode in printing" }
    { tofile 1 "Non zero means print to file" }
    { pagewidth "" "Figure width" }
    { pageheight "" "Figure height" }
    { papersize letter "letter, legal or A4"}
    { hoffset .5 "Left margin for printing"}
    { voffset .5 "Right margin for printing"}
    { xticks 20 "Rough number of ticks on x axis"}
    { yticks 20 "Rough number of ticks on y axis"}
    { domargin 1 "Print the frame and the margin ticks"}
    { printer "" "Printer to print to, eg lw8b " }
    { title "" "Title" }
    { psfilename "~/sdfplot.ps" "Postscript filename" }
    { gsview "gsview32" "postscript viewer, used for printing under Windows" }
    { centeronpage 1 ""} 
}

# proc getPageOffsets { widthbyheight} {
#     global printOption paperSizes
#     puts "wbh=$widthbyheight"
#     set pwid 8.5
#     set phei 11.0

#     foreach v $paperSizes {
# 	if { "[lindex $v 0]" == "$printOption(papersize)" } {
# 	    set pwid [lindex $v 1]
# 	    set phei [lindex $v 2]
# 	}
#     }
#     set wid [expr {$pwid - 2* $printOption(hoffset)}]
#     set hei [expr {$phei - 2* $printOption(voffset)}]
# #    if { $printOption(landscape) } {set widthbyheight [expr  {1.0 /$widthbyheight}]}
# #    set w $wid ; set hei $wid ; set wid $w

#     puts "pw=$wid,ph=$hei,w/h=$widthbyheight,hh=[expr {$hei * $widthbyheight}], ww=[expr {$wid / $widthbyheight}]"

#     set fac   $widthbyheight
#     puts "fac=$fac"
#     if { $fac * $hei < $wid } {
    # 	set iwid [expr {$fac *$hei}]
# 	set ihei $hei

#     } else {
    # 	set ihei [expr {$wid / $fac}]

# 	set iwid $wid

#     }

#     if { $printOption(landscape) } { set fac1 [expr {1/$fac}] }
#     if { $wid/$hei > $fac } {
# 	set ihei $hei
    # 	set iwid   [expr {$hei / $fac }]

#     } else {
# 	 set iwid $wid
    # 	 set ihei [expr {$wid * $fac }]
#     }

#     #-pagex = left margin (whether landscape or not)
#     #-pagey = right margin (whether landscape or not)
#     #-pagewidth becomes vertical height if landscape
#     #-pageheight becomes horiz width if landscape
    
#     set xoff [expr {($pwid-$iwid)/2.0}]
#     set yoff [expr  {($phei-$ihei)/2.0}]

#     if { $printOption(landscape) } {
# 	set h $ihei
# 	set ihei $iwid
# 	set iwid $h
#     }

#     puts "phei=$phei,ihei=$ihei,yoff=$yoff,voff=$printOption(voffset)"
#     set ans "-pagex [set xoff]i -pagey [set yoff]i \
# 	    -pagewidth [set iwid]i -pageheight [set ihei]i"
#     set ans "-pagex [set xoff]i -pagey [set yoff]i \
# 	    -pagewidth [set iwid]i -pageheight [set ihei]i"    
#     return $ans
# }

proc swap { a b } {
    set me [uplevel 1 set $b]
    uplevel 1 set $b \[set $a\]
    uplevel 1 set $a [list $me]
}

proc getPageOffsets { widthbyheight} {
    global printOption paperSizes
    #puts "wbh=$widthbyheight"
    set pwid 8.5
    set phei 11.0

    foreach v $paperSizes {
	if { "[lindex $v 0]" == "$printOption(papersize)" } {
	    set pwid [lindex $v 1]
	    set phei [lindex $v 2]
	}
    }
    set wid [expr {$pwid - 2* $printOption(hoffset)}]
    set hei [expr {$phei - 2* $printOption(voffset)}]
    if { $printOption(landscape) } {
	swap wid hei
#	swap pwid phei
    }
    if { $wid / $hei  < $widthbyheight  } {
	# width dominates
	set iwid $wid
	set ihei [expr {$wid / $widthbyheight }]
	append opts " -pagewidth [set wid]i"
    } else {
	set ihei $hei
	set iwid [expr {$hei * $widthbyheight }]
	append opts " -pageheight [set hei]i"
    }

    #-pagex = left margin (whether landscape or not)
    #-pagey = right margin (whether landscape or not)
    #-pagewidth becomes vertical height if landscape
    #-pageheight becomes horiz width if landscape
    
    append opts " -pagex [expr {$pwid / 2.0}]i -pagey [expr {$phei / 2.0}]i "

	if { $printOption(landscape) } {
	    append opts " -rotate $printOption(landscape)" 
	}
    return $opts
}

global printOption
set printOption(setupDone) 0

proc getEnv { name } {
  global env
 if { [catch { set tem $env($name) } ] } { return "" }
 return $tem
}
proc setPrintOptions { lis } {
    global browser_version
   global printOptions printOption printSetUpDone 
    if { !$printOption(setupDone) } {
	set printOption(setupDone) 1
	getOptions $printOptions $lis -allowOtherKeys 1 \
		-setdefaults [catch { source [getEnv HOME]/.printOptions }] -usearray printOption
        if { "$printOption(printer)" == "" } {set printOption(printer) [getEnv PRINTER] } else { set printOption(printer) lw8b }
	
    }
    if { [info exists browser_version] } { set printOption(tofile) 2 }
}

proc mkentryPr { w var text buttonFont }  {
    set fr $w ; frame $fr
    uplevel 1 append topack [list " $fr"]
    label $fr.lab -text "$text" -font $buttonFont
    entry $fr.e -width 20 -textvariable $var -font $buttonFont
    pack $fr.lab $fr.e -side left -expand 1 -padx 3 -fill x
}


proc mkPrintDialog { name args } {
    global printSet argv env printOptions printOption printSetUpDone paperSizes buttonfont

    set canv [assoc -canvas $args ]
    set buttonFont [assoc -buttonfont $args $buttonfont]
    catch { destroy $name }
    set dismiss "destroy $name"
    if { "$canv" == "" } {
     catch {destroy $name}
    toplevel $name
    wm geometry $name -0+20
   
    } else {
        $canv delete printoptions
        set name [winfo parent $canv].printoptions
	# set name $canv.fr1
        catch {destroy $name}
	frame $name -borderwidth 2 -relief raised
	
	set item [$canv create window [$canv canvasx 10] [$canv canvasy  10] -window $name -anchor nw -tags printoptions]
        $canv raise printoptions
	set dismiss "$canv delete $item; destroy $name "
    }
	
    frame $name.fr

    set w $name.fr
    label $w.msg  -wraplength 600 -justify left -text "Printer Setup"
    pack $w
    pack $w.msg
    set wb $w.buttons
    frame $wb 
    pack $wb -side left -fill x -pady 2m
    set topack ""
    catch { set printOption(psfilename) \
	    [file nativename $printOption(psfilename)]}
    button $wb.ok -text "ok" -font $buttonFont  -command "destroy $name ; $canv delete printoptions"
    radiobutton $wb.b0 -text "Save via ftp" -variable printOption(tofile) -relief flat -value 2 -command {set writefile "Save"} -font $buttonFont  -highlightthickness 0 
    radiobutton $wb.b1 -text "Save as Postscript File" -variable printOption(tofile) -relief flat -value 1 -command {set writefile "Save"} -font $buttonFont  -highlightthickness 0 
    radiobutton $wb.b2 -text "Print To Printer" -variable printOption(tofile) -relief flat -value 0 -command {set writefile "Print"} -font $buttonFont -highlightthickness 0 
    checkbutton $wb.b3 -text "Center on Page" -variable printOption(centeronpage) -relief flat -font $buttonFont -highlightthickness 0 
    checkbutton $wb.b4 -text "Landscape Mode" -variable printOption(landscape) -relief flat -font $buttonFont -highlightthickness 0 

    mkentryPr  $wb.pagewidth printOption(pagewidth) "Figure width" $buttonFont
    mkentryPr  $wb.pageheight printOption(pageheight) "Figure height" $buttonFont
    mkentryPr  $wb.hoffset printOption(hoffset) "Left margin for printing" $buttonFont
    mkentryPr  $wb.voffset printOption(voffset) "bottom margin for printing" $buttonFont
    mkentryPr  $wb.psfilename printOption(psfilename) "postscript filename" $buttonFont
    mkentryPr  $wb.printer printOption(printer) "Printer to print to" $buttonFont
    mkentryPr  $wb.gsview printOption(gsview) "postscript viewer, used for printing under Windows" $buttonFont
   mkentryPr  $wb.xticks printOption(xticks) "Rough number of xticks" $buttonFont
   mkentryPr  $wb.yticks printOption(yticks) "Rough number of yticks" $buttonFont
    eval pack $wb.ok $wb.b0 $wb.b1 $wb.b2 $wb.b3 $wb.b4
    eval pack $topack -expand 1

    foreach v  $paperSizes {
	set papersize [lindex $v 0]
        set lower [string tolower $papersize]
        radiobutton $wb.$lower -text [lindex $v 0] -variable printOption(papersize) \
	   -value [lindex $v 0] -font $buttonFont -highlightthickness 0 
    pack $wb.$lower -pady 2 -anchor w -fill x
    }
    checkbutton $wb.domargin -variable printOption(domargin) -text "do margin" 
    pack $wb.domargin -pady 2 -anchor w -fill x

    frame $w.grid
    pack $w.grid -expand yes -fill both -padx 1 -pady 1
    grid rowconfig    $w.grid 0 -weight 1 -minsize 0
    grid columnconfig $w.grid 0 -weight 1 -minsize 0
}

proc markToPrint { win tag title } {
    # puts "$win $tag"
   # bind $win <1> "bindBeginDrag $win %x %y $tag [list $title]"
    pushBind $win <1> "$win delete printrectangle ; popBind $win <1>"
    pushBind $win <1> "bindBeginDrag $win %x %y $tag [list $title]; popBind $win <1>"    
}

proc bindBeginDrag { win x y tag title } {
    $win delete $tag printrectangle
    set beginRect "[$win canvasx $x] [$win canvasy $y]"
    set it1 [eval $win create rectangle $beginRect $beginRect -tags $tag -width 3]
    set old [bind $win <B1-Motion>]
    set new "eval $win coords $it1 \
	    $beginRect \[$win canvasx %x\] \[$win canvasy %y\]; \
	    "
    if { "$old" == "$new" } {set old ""}
    bind $win <B1-Motion> $new
    bind $win <ButtonRelease-1> "bind $win <B1-Motion> [list $old];\
	    bind $win <ButtonRelease-1> {} ; unbindAdjustWidth $win $tag [list $title];"
}

proc unbindAdjustWidth { canv tag title } {
    set win [winfo parent $canv]
    global printOption

    set it [$canv find withtag $tag]
    set co1 [$canv coords $tag]
    set co [$canv coords $it]
   # if { "$co" != "$co1" } {puts differ,$co1,$co}
    desetq "x1 y1 x2 y2" $co
    set center [expr { ($x1+$x2 )/2}]
   set h [expr {$y2 - $y1}]
    set it [$canv find withtag $tag]
   set new [$canv create rectangle $x1 $y1 $x2 $y2 -outline white -width [expr {$h* .04}] -tags [concat $tag bigger] ]

    # puts "<marginTicks $canv $x1 $y1 $x2 $y2 printrectangle>"
    marginTicks $canv [storx$win $x1] [story$win $y2] [storx$win $x2] [story$win $y1] "printrectangle marginticks"
    desetq "a1 b1 a2 b2" [$canv bbox $new]
   set textit [$canv create text $center [expr {$y1 - $h *.03}] \
	    -font [font create -family Courier -size 14 -weight bold] -text "$title" \
	    -anchor s -tags [concat $tag bigger title]]

    set bb [$canv bbox $textit]
   $canv create rectangle $a1 [lindex $bb 1]  $a2 [expr {$y1 - 0.02 * $h}]  -tags $tag -fill white -outline {}
   $canv itemconfig $it -width [expr {$h *.002}]
    $canv raise $it
    $canv raise $textit
    $canv raise marginticks
    if { $printOption(domargin) == 0 } {
	$canv delete marginticks
    }

    $canv create text [expr {($a1 + $a2)/2.0}] [expr {$y2 + .01*$h  }] -anchor nw -text "For [getEnv USER] [clock format [clock seconds]]" -font [font create -family Courier -size 10 -weight normal] -tag $tag
    # puts h=$h

}
    

proc getPSBbox  { } {
  set fi [open /home/wfs/sdfplot.ps r]
  set me [read $fi 500]
  regexp {BoundingBox: (-*[0-9]+) (-*[0-9]+) (-*[0-9]+) (-*[0-9]+)} $me junk x1 y1 x2 y2
    set w [expr {72 * 8.5}]
    set h [expr {72 * 11}]
    # puts "hei=[expr {$y2-$y1}],tm=[expr {$h - $y2}],bm=$y1"
    # puts "wid=[expr {$x2-$x1}],lm=$x1,rm=[expr {$w - $x2}]"
    # puts "hei=[expr {($y2-$y1)/72.0}],tm=[expr {($h - $y2)/72.0}],bm=([expr {$y1/72.0}])"
    #puts "wid=[expr {($x2-$x1)/72.0}],lm=([expr {$x1/72.0}]),rm=[expr {($w - $x2)/72.0}]"    
  close $fi
}


## endsource printops.tcl
# set font {Courier 8}
global fontCourier8
set fontCourier8 "-*-Courier-Medium-R-Normal--*-120-*-*-*-*-*-*"

global axisGray
if { "[winfo screenvisual .]" == "staticgray" } { 
    set axisGray black
} else  { 
    set axisGray gray60
}

global writefile
set writefile  "Save"
# make printing be by ftp'ing a file..

if {[catch { set doExit }] } { set doExit ""}
set width_ [winfo screenwidth .]
if { $width_ >= 1280 } { set fontSize 12
  } elseif { $width_ <= 640} { set fontSize 8 } else {
    set fontSize 10}
unset width_    

proc makeFrame { w type } {
    global   writefile doExit fontSize buttonfont ws_openMath   
    set win $w
    if { "$w" == "." } {
        set w "" } else {
	    catch { destroy $w}
	    
	    frame $w
	    # toplevel $w
	    # set w $w.new
            # frame $w
           # puts "making $w"	
	    
    }

    set dismiss "destroy $win"
    catch { set  parent [winfo parent $win] 
    if { "$parent" == "." } {
	set dismiss "destroy ."
    }
    if { [string match .plot* [winfo toplevel $win]] } {
	set dismiss "destroy [winfo toplevel $win]"
    }
    }
    
    if { "$doExit" != "" } {set dismiss $doExit } 	
    oset $w type $type

    frame $w.grid
   #positionWindow $w
    set c $w.c
    oset $win c $c
    bboxToRadius $win
    
    if { [catch { set buttonfont} ] } {
	set buttonfont [font create -family Helvetica -size $fontSize]
    }
    set buttonFont $buttonfont    
    oset $win buttonFont $buttonfont

#    puts "children wb=[winfo children $w]"
    set wb $w.buttons
    frame $wb
    set dismiss [concat $dismiss "; clearLocal $win "]

    button $wb.dismiss -text Dismiss -command $dismiss -font $buttonFont
    setBalloonhelp $win $wb.dismiss {Close this plot window}
    button $wb.zoom -text "Zoom" -command "showZoom $w" -font $buttonFont
    setBalloonhelp $win $wb.zoom {Magnify the plot.  Causes clicking with the left mouse button on the plot, to magnify (zoom in) the plot where you click.  Also causes Shift+Click to  it to unmagnify (zoom out) at that point}
    oset $w position "" 
#    button $w.position -textvariable [oloc $w position] -font $buttonFont -width 10
    label $w.position  -textvariable [oloc $w position] -font $buttonFont -width 10
    setBalloonhelp $win $w.position {Position of the pointer in real x y coordinates.  For 3d it is the position of the nearest vertex of the polygon the pointer is over.}

    button $wb.help -text "Help" -command "doHelp$type $win" -font $buttonFont
    setBalloonhelp $win $wb.help {Give more help about this plot window}
    button $wb.postscript -textvariable writefile -command "writePostscript $w" -font $buttonFont
    setBalloonhelp $win $wb.postscript {Prints or Saves the plot in postscript format.  The region to be printed is marked using Mark.   Other print options can be obtained by using "Print Options" in the Config menu }
    
    button $wb.markrect -text "Mark" -command "markToPrint $c printrectangle \[eval \[oget $win maintitle\]\]" -font $buttonFont
    setBalloonhelp $win $wb.markrect {Mark the region to be printed.  Causes the left mouse button to allow marking of a rectangle by clicking at the upper left corner, and dragging the mouse to the lower right corner.  The title can be set under "Print Options" under Config}
    button $wb.replot -text "Replot" -command "replot$type $win" -font $buttonFont
    setBalloonhelp $win $wb.replot {Use the current settings and recompute the plot.  The settings may be altered in Config}
    
	
    
    button $wb.config -text "Config" -command "doConfig$type $win" -font $buttonFont
    setBalloonhelp $win $wb.config {Configure various options about the plot window.  After doing this one may do replot.  Hint: you may leave the config menu on the screen and certain actions take place immediately, such as rotating or computing a trajectory at a point.  To make room for the window you might slide the graph to the right, and possibly shrink it using the unzoom feature}    
    


    #mike FIXME: this is a wrong use of after cancel
    bind $win.position <Enter> "+place $win.buttons -in $win.position -x 0 -rely 1.0 ;  after cancel lower $win.position ; raise $win.buttons "
    bind $win.buttons <Leave> "deleteBalloon $c ; place forget $win.buttons"

    # pack $wb
    scrollbar $w.hscroll -orient horiz -command "$c xview"
    scrollbar $w.vscroll -command "$c yview"
    # -relief sunken
    canvas $c  -borderwidth 2 \
	    -scrollregion {-1200 -1200 1200 1200} \
	-xscrollcommand "$w.hscroll set" \
	-yscrollcommand "$w.vscroll set" -cursor arrow -background white
    # puts "$c config  -height [oget $win height] -width [oget $win width] "
    set buttonsLeft 1
    set wid [oget $win width]
    catch {$c config  -height [oget $win height] -width  $wid
           oset $win oldCheight [oget $win height]
           oset $win oldCwidth $wid
     }
    # puts "$c height =[$c cget   -height],$c width =[$c cget   -width]"
    # bind $c <2> "$c scan mark %x %y"
    bind $c <B3-Motion> "$c scan dragto %x %y"
    bind $c <3> "$c scan mark %x %y"
    bind $c <B3-Motion> "$c scan dragto %x %y"    
    bind $c <Motion> "showPosition $w %x %y"
    bind $c <Configure> "reConfigure $c %w %h"
    bind $c <Enter> "raise $win.position"
    bind $c <Leave> "after 200 lower $win.position"
    $w.position config -background [$c cget -background]
     
     
    pack  $wb.dismiss $wb.help $wb.zoom   \
	    $wb.postscript $wb.markrect $wb.replot $wb.config -side top -expand 1 -fill x
    if { 0 } {
	pack $w.hscroll -side bottom -expand 1 -fill x
	pack $w.vscroll -side right -expand 1 -fill y
    }
	pack $w.c -side right -expand 1 -fill both
    
    pack $w
    place $w.position -in $w -x 2 -y 2 -anchor nw
    oset $w position "Menu Here"
    if { ![info exists ws_openMath(showedplothelp)] ||
    [llength $ws_openMath(showedplothelp)] < 2 } {
	lappend ws_openMath(showedplothelp) 1
	
	after 100 balloonhelp $w $w.position [list \
		"Initial help: Moving the mouse over the position \
		window (top left corner), will bring up a menu.  Holding down \
		right mouse button and dragging will translate the plot"]
	after 2000 $w.c delete balloon
	

    }
    
    raise $w.position
    
    pack [winfo parent $wb]
   # update
#    set wid [ winfo width $win]
#    if { $wid > [      $c cget -width ] } {
#    $c config -width $wid
#	    oset $win width $wid
#    }

   addSliders $w 

   bind $w <Configure> "resizePlotWindow $w %w %h"
    return $w    
}

proc mkentry { newframe textvar text buttonFont } {
    frame $newframe
    set parent $newframe
    set found 0
    while { !$found } {
	set parent [winfo parent $parent]
	if { "$parent" == "" } { break }
	if { ![catch {  set type [oget $parent type] } ] } {
	    global plot[set type]Options
	    foreach v [set plot[set type]Options] {
		if { "[oloc $parent [lindex $v 0]]" == "$textvar" } {
		     setBalloonhelp $parent $newframe [lindex $v 2]
		    set found 1
		     break

		}
	    }
    }
    }
    label $newframe.lab1 
    label $newframe.lab -text "$text:" -font $buttonFont -width 0
    entry $newframe.e -width 20 -textvariable $textvar -font $buttonFont
    pack $newframe.lab1 -side left -expand 1 -fill x 
    pack $newframe.lab -side left
    pack $newframe.e -side right -padx 3 -fill x
   # pack $newframe.lab $newframe.e -side left -padx 3 -expand 1 -fill x
}
    

proc doHelp { win msg } {
    makeLocal $win c
    set atx [$c canvasx 0]
    set aty [$c canvasy 0]
    $c create rectangle [expr {$atx -1000}] [expr  {$aty -1000}] 10000 10000 -fill white -tag help

    $c create text [expr {$atx +10}] [expr {$aty + 10.0}] -tag help  -anchor nw  -width 400 -text $msg 

    pushBind $c <1> "$c delete help; popBind $c <1>"
}

## source push.tcl

###### push.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################



#
 #-----------------------------------------------------------------
 #
 # pushl --  push VALUE onto a stack stored under KEY
 #
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#

global __pushl_ar
proc pushl { val key  } {
    global __pushl_ar
  append __pushl_ar($key) " [list $val]"
}


#
 #-----------------------------------------------------------------
 #
 # peekl --  if a value has been pushl'd under KEY return the 
 # last value otherwise return DEFAULT.   If M is supplied, get the
 # M'th one pushed... M == 1 is the last one pushed.
 #  Results:  a previously pushed value or DEFAULT
 #
 #  Side Effects: none
 #
 #----------------------------------------------------------------
#
proc peekl {key default {m 1}} {
    global __pushl_ar
    if { [catch { set val [set __pushl_ar($key) ] } ] } {
	return $default } else {
	    set n [llength $val]
	    if { $m > 0 && $m <= $n } {
		return [lindex $val [incr n -$m]]
	    } else { return $default }
	}
    }
    
    

#
 #-----------------------------------------------------------------
 #
 # popl --  pop off  last value stored under KEY, or else return DFLT
 #
 #  Results: last VALUE stored or DEFAULT
 #
 #  Side Effects: List stored under KEY becomes one shorter
 #
 #----------------------------------------------------------------
#
proc popl { key  dflt} {
    global __pushl_ar
    
    if { [catch { set val [set __pushl_ar($key) ] } ] } {
	return $dflt } else {
	    set n [llength $val]
   	    set result [lindex $val [incr n -1]]

	    if { $n > 0 } {
		set __pushl_ar($key) [lrange $val 0 [expr {$n -1}]]
	    } else {unset __pushl_ar($key) }
	    return $result
	}
    }


#
 #-----------------------------------------------------------------
 #
 # clearl --  clear the list stored under KEY
 # 
 #  Result: none
 #
 #  Side Effects:  clear the list stored under KEY
 #
 #----------------------------------------------------------------
#
proc clearl { key } {
    global __pushl_ar
    catch { unset __pushl_ar($key) }
}
    


## endsource push.tcl
proc pushBind { win key action } {
    pushl [bind $win $key] [list $win $key ] 
    bind $win $key $action
}

proc popBind { win key  } {
    set binding [popl [list $win $key] {}]
   
    bind $win $key $binding
}

# exit if not part of openmath browser
proc maybeExit { n } {
    if { "[info proc OpenMathOpenUrl]" != "" } {
	uplevel 1 return
    } else { exit 0 }
}

proc showPosition { win x y } {
   # global position c
    makeLocal $win c
    # we catch so that in case have no functions or data..
    catch {
    oset $win position \
      "[format {(%.2f,%.2f)}  [storx$win [$c canvasx $x]] [story$win [$c canvasy $y]]]"
}   }

proc showZoom  { win } {
  #  global c position
    makeLocal $win c
    oset $win position "Click to Zoom\nShift+Click Unzoom"
     
    bind $c <1> "doZoom $win %x %y 1"
    bind $c  <Shift-1> "doZoom $win %x %y -1"
}

proc doZoom { win x y direction } {
    set zf [oget $win zoomfactor]
    if { $direction < 0 } {
	set zf 	"[expr {1/[lindex $zf 0]}] [expr {1/[lindex $zf 1]}]"
    }
    eval doZoomXY $win $x $y $zf
}
    


#
 #-----------------------------------------------------------------
 #
 # doZoomXY --  given screen coordinates (x,y) and factors (f1,f2)
 #  perform a scaling on the canvas, centered at (x,y) so that
 #  the distance in the x direction from this origin is multiplied by f1
 #  and similarly in the y direction
 #  Results:
 #
 #  Side Effects: scale the canvas, and set new transforms for translation
 #   from real to canvas coordinates.
 #----------------------------------------------------------------
#

proc doZoomXY { win x y facx facy } {
    if { [catch {
	makeLocal $win c transform
    } ] } {
	# not ready
	return
    }
    
    set x [$c canvasx $x]
    set y [$c canvasy $y]

    $c scale all $x $y $facx $facy

    set ntransform [composeTransform \
	    "$facx 0 0 $facy [expr {(1-$facx)* $x}] [expr {(1-$facy)* $y}]" \
	    $transform  ]
    oset $win transform $ntransform
    getXtransYtrans $ntransform rtosx$win rtosy$win
    getXtransYtrans [inverseTransform $ntransform] storx$win story$win
    axisTicks $win $c
}


#
 #-----------------------------------------------------------------
 #
 # scrollPointTo --  attempt to scroll the canvas so that point
 #  x,y on the canvas appears at screen (sx,sy)
 #
 #  Results: none
 #
 #  Side Effects: changes x and y view of canvas 
 #
 #----------------------------------------------------------------
#
proc scrollPointTo { c x y sx sy } {
    desetq "x0 y0 x1 y1" [$c cget -scrollregion]
    $c xview moveto [expr { 1.0*($x-$x0-$sx)/($x1-$x0)} ]
    $c yview moveto [expr { 1.0*($y-$y0-$sy)/($y1-$y0)} ]
}



#
 #-----------------------------------------------------------------
 #
 # reConfigure --  
 #
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#

proc reConfigure { c width height  } {
    set w [winfo parent $c]
    if { [catch { makeLocal $w oldCwidth oldCheight } ] } {
	oset $w oldCwidth $width
	oset $w oldCheight $height
	return
    }
    set oldx [$c canvasx [expr {$oldCwidth/2.0}]]
    set oldy [$c canvasy [expr {$oldCheight/2.0}]]
    doZoomXY $w [expr {$oldCwidth/2.0}] [expr {$oldCheight/2.0}] \
	    [expr {1.0*$width/$oldCwidth}] [expr {1.0*$height/$oldCheight}]
    
    scrollPointTo $c $oldx $oldy [expr {$width/2.0}] [expr {$height/2.0}]
   # update
    oset $w oldCwidth $width
    oset $w oldCheight $height
}

proc writePostscript { win } {
    global  printOption argv
    makeLocal $win c transform transform0 xmin ymin xmax ymax
    set rtosx rtosx$win ; set rtosy rtosy$win
    drawPointsForPrint $c
    if { "[$c find withtag printrectangle]" == "" } {
	# $c create rectangle [$rtosx $xmin] [$rtosy $ymin] [$rtosx $xmax] [$rtosy $ymax] -tags printrectangle -width .5
	$c create rectangle [$c canvasx 0] [$c canvasy 0] [$c canvasx [$c cget -width ]] [$c canvasy [$c cget -height ]]   -tags printrectangle -width .5	
	unbindAdjustWidth $c printrectangle [eval [oget $win maintitle]]
    }
    $c delete balloon
	
	
    set bbox [eval $c bbox [$c find withtag printrectangle]]
    desetq "x1 y1 x2 y2" $bbox
#     set title "unknown plot"
#     catch { set title [eval $printOption(maintitle)] }

#     $c create text [expr {($x1 + $x2)/2}]  [expr {$y1 + .04 * ($y2 - $y1)}] \
# 	    -anchor center -text $title -tag title

    update
set diag [vectorlength [expr {$y1-$x1}] [expr {$y2-$x2}]]
#  get rid of little arrows that creep onto the outside, ie let
#  the blank rectangle cover them.
set x1 [expr {$x1+.01 * $diag}]
set x2 [expr {$x2-.01 * $diag}]
set y1 [expr {$y1+.01 * $diag}]
set y2 [expr {$y2-.01 * $diag}]

    set com "$c postscript  \
      	    -x  $x1  -y $y1 \
	    -width [expr {($x2 - $x1)}] \
            -height [expr {($y2 - $y1)}] \
	    [getPageOffsets [expr {($x2 - $x1)/(1.0*($y2 - $y1))}] ] "

    #puts com=$com
    set output [eval $com]
    switch $printOption(tofile) {
	0 { global tcl_platform
	    set usegsview 0  
	    if { "$tcl_platform(platform)" == "windows" } {
		set usegsview 1
	    }
	    if { $usegsview } {
		set fi [open $printOption(psfilename) w]
		puts $fi $output
		close $fi
		exec "$printOption(gsview) /S $printOption(psfilename)"
	    } else {
	    set fi [open "|lpr -P[set printOption(printer)]" w]
	    puts $fi $output
	    close $fi
	    }
	}
	1 { set fi [open $printOption(psfilename) w]
	puts $fi $output
	close $fi }
	2 { global ftpInfo
	    set ftpInfo(data) $output
	    ftpDialog $win
	}
    }
#    if { $printOption(tofile) } {
#	set fi [open $printOption(psfilename) w]
#    } else { set fi [open "|lpr -P[set printOption(printer)]" w] }
 #   puts $fi $output
#    close $fi
}


#
 #-----------------------------------------------------------------
 #
 # ftpDialog --  open up a dialog to send ftpInfo(data) to a file
 # via http and ftp.   The http server can be specified.
 #
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#

global ftpInfo
set ftpInfo(host) genie1.ma.utexas.edu
set ftpInfo(viahost) genie1.ma.utexas.edu

proc ftpDialog { win args } {
    global ftpInfo buttonFont fontSize
    set fr ${win}plot
    set usefilename [assoc -filename $args  0]
    if { "$usefilename" != "0"} {
	set ftpInfo(filename) $usefilename
	set usefilename 1
    }
    catch { destroy $fr }
    set ftpInfo(percent) 0
    set buttonFont [font create -family Courier -size $fontSize]
    frame $fr -borderwidth 2 -relief raised
    if { [catch { set ftpInfo(directory) } ] } { set ftpInfo(directory) homework }
    label $fr.title -text "Ftp Dialog Box" -font [font create -family Helvetica -size [expr {2+ $fontSize}]]
    mkentry $fr.host ftpInfo(host) "host to write file on" $buttonFont
    mkentry $fr.viahost ftpInfo(viahost) "host to write to via" $buttonFont
    mkentry $fr.username ftpInfo(username) "Your User ID on host" $buttonFont
    mkentry $fr.password ftpInfo(password) "Your password on host" $buttonFont
    $fr.password.e config -show *
    mkentry $fr.directory ftpInfo(directory) "remote subdirectory for output" $buttonFont

    if { $usefilename } {
	mkentry $fr.filename ftpInfo(filename) "filename " $buttonFont
    } else {
    mkentry $fr.chapter ftpInfo(chapter) "chapter " $buttonFont
    mkentry $fr.section ftpInfo(section) "section" $buttonFont
    mkentry $fr.problemnumber ftpInfo(number) "Problem number" $buttonFont
    }
    scale   $fr.scale -orient horizontal -variable ftpInfo(percent) -length 100 
    button $fr.doit -text "Send it" -command "doFtpSend $fr" -font $buttonFont
    button $fr.cancel -text "Cancel" -command "destroy $fr" -font $buttonFont
    set ftpInfo(message) ""
    label $fr.message  -width 30 -height 3 -textvariable ftpInfo(message) -font $buttonFont
    eval pack  [winfo  children $fr] -side top 
    raise $fr
    place $fr -in $win -relx .5 -rely .5 -anchor center
   }

proc doFtpSend { fr } {
    global ftpInfo om_ftp

    set error ""
    if { [winfo exists $fr.filename] } {
	set filename $ftpInfo(filename)
	set check "host username directory filename"
    } else {
	set check "host username directory chapter section number"
    }
    foreach v $check {
	if { $ftpInfo($v) == "" } {
	    if  { "$error" == "" } { set error "Failed to specify $v " } else {
		append error ", $v"}
	}   
    }
    if { "$error" != "" } {
	set ftpInfo(message) $error
	return -1
    }
    if { [winfo exists $fr.chapter] } {
	set filename "$ftpInfo(chapter).$ftpInfo(section)-$ftpInfo(number).ps"
    }
    
    
    set res [submitFtp $ftpInfo(viahost) $ftpInfo(host) $ftpInfo(username) $ftpInfo(password) $ftpInfo(directory) $filename]
    if { "$res" == 1 }  {
	   after 1000 "destroy $fr"
    }
    return $res
    
#    set counter [ ftp $ftpInfo(host) $ftpInfo(username) $ftpInfo(password)]
#    if { $counter < 0 } {
#	set ftpInfo(message) [concat "Failed:" $om_ftp($counter,log)]
#	return -1
#    }

#     if { [ftpDoCd $counter $ftpInfo(directory)] < 0 &&
#          [ftpDoMkdir $counter $ftpInfo(directory)] > -10 &&
#        [ftpDoCd $counter $ftpInfo(directory)] < 0 } {
# 	set ftpInfo(message) [concat "Failed:" $om_ftp($counter,log)]
# 	return -1
#     }

    
#     set res [ftpDoStore $counter $ftpInfo(chapter).$ftpInfo(section)-$ftpInfo(number).ps $ftpInfo(data)]
#     if { $res < 0 } {
# 	set ftpInfo(message) "Failed: $om_ftp($counter,log)"
# 	return -1
#     } else {
# 	set ftpInfo(message) "Wrote $ftpInfo(directory)/$ftpInfo(chapter).$ftpInfo(section)-$ftpInfo(number).ps"
# 	after 1000 destroy $fr
#     }
#     ftpClose $counter
}

proc vectorlength { a b } {
    return [expr {sqrt($a*$a + $b * $b)} ]
}

proc setupCanvas { win } {
  makeLocal $win   xcenter xradius ycenter yradius

  oset $win xmin [expr {$xcenter - $xradius}]
  oset $win xmax [expr { $xcenter + $xradius}]
  oset $win ymin [expr { $ycenter - $yradius}]
  oset $win ymax [expr { $ycenter + $yradius} ]

}


#
 #-----------------------------------------------------------------
 #
 # compose --  A and B are transformations of the form "origin scalefac"
 # and composing them means applying first b then a, as in a.b.x
 #  "o s" . x ==> (x-o)*s + o
 #  Results: the "origin scalefac" which corresponds to the composition.
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc compose { a b } {
  return  "[expr {-[lindex $a 1]*[lindex $b 0]*[lindex $b 1] \
      +[lindex $a 1]*[lindex $b 0]-[lindex $a 0]*[lindex $a 1] \
      +[lindex $a 0]}] [expr {[lindex $a 1]*[lindex $b 1]}]"
}

# the following two have been replaced 
# proc sparseList { s } {
#     if  { [catch {
# 	set val [parseConvert "$s" -variables "x y t"] } err ] } {
# 	    error "Syntax error with `$s'\n $err"
# 	}
# 	return [lindex $val 0]
#     }
# 
# proc sparse { s } {
#     set val [sparseList $s]
#     set first $val
#     if { [llength $first] != 1 } {
# 	error "only one function wanted" }
# 	
# 	return [lindex $first 0]
#    }

proc sparseListWithParams { form variables paramlist } {
    set tem [parseConvert $form -doall 1]
    #puts tem=$tem
    set params [splitParams $paramlist]
    if { [catch {set res [substParams [lindex $tem 0] $variables $params] }\
	    err ] } {
	set vars [lindex $tem 1]
	set all $variables
	foreach { v val }  $params { lappend all $v}
	foreach v $vars { if { [lsearch $all [string range $v 1 end]] < 0 } {
	    error "The variable `[string range $v 1 end]' appeared in $form but was not in allowed variables:{$variables} or in parameters: {$paramlist}"
	}
    }
	error "The form $form may involve variables other than {$variables} or the parameters {$paramlist}, or the latter may have invalid expressions:\n $err"
    }
    return $res
}

proc sparseWithParams { form variables params } {
    set tem [sparseListWithParams $form $variables $params]
    if { [llength $tem ] > 1 } { error "only wanted one function: $form"}
    lindex $tem 0
}



#
 #-----------------------------------------------------------------
 #
 # myVarSubst --  into FORM substitute where
 # listVarsVals where each element of this list may mention
 # the previous values eg "k 7 ll sin(k+8)"
 # eg:
 #myVarSubst [lindex [parseConvert "k*x+l" -doall 1] 0] {x $x k 27+4 l 93+k^3}
 # ==> {((31 * $x) + 29884.0)}  
 #
 #  Results: FORM with the substitutions done
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc myVarSubst { form listVarsVals } {
    foreach {_u _v} $listVarsVals {
	if { "\$$_u" == "$_v" } {
	    set $_u $_v
	} else {
	    set _f1 [lindex [parseConvert  $_v -doall 1] 0]
	    set $_u [expr [lindex $_f1 0]]
	    # puts "$_u = [set $_u]"
    }
   }
   subst -nobackslashes -nocommands $form

}

proc splitParams { paramlist } {
    set params ""
    foreach v [split $paramlist ,] {
	set tem [split $v =]
	if { [llength $tem] == 2 } {
	    lappend params [lindex $tem 0] [lindex $tem 1]
	}
    }
    return $params
}

    

#
 #-----------------------------------------------------------------
 #
 # substParams --  substitute into FORM keeping VARIABLES as they are
 # and the PARAMLIST (of the form k=23, l=k+7,...) into FORM
 #
 #  Results: substituted FORM
 #
 #  Side Effects: none
 #
 #----------------------------------------------------------------
#
proc substParams { form variables params } {
   foreach v $variables { lappend params $v \$$v}
   set res [myVarSubst $form $params]
    return $res
}
	
    
	


#
 #-----------------------------------------------------------------
 #
 # setUpTransforms --  set up transformations for the canvas of WINDOW
 # so that the image is on FACTOR fractionof the window
 # these transforms are used for real to screen and vice versa.
 #  Results: 
 #
 #  Side Effects: transform functions rtosx$win rtosy$win storx$win story$win
 #  are defined.
 #
 #----------------------------------------------------------------
#    
proc setUpTransforms { win fac } {
    makeLocal $win xcenter ycenter xradius yradius c

    set delx [$c cget -width]
    set dely [$c cget -height]
    set f1 [expr {(1 - $fac)/2.0}]
    
    set x1 [expr {$f1 *$delx}]
    set y1 [expr {$f1 *$dely}]
    set x2 [expr {$x1 + $fac*$delx}]
    set y2 [expr {$x1 + $fac*$dely}]

    
    
    set xmin [expr {$xcenter - $xradius}]
    set xmax [expr {$xcenter + $xradius}]
    set ymin [expr {$ycenter - $yradius}]
    set ymax [expr {$ycenter + $yradius}]
    
    oset $win xmin $xmin
    oset $win xmax $xmax
    oset  $win ymin $ymin
    oset $win ymax $ymax
    
    oset $win transform [makeTransform "$xmin $ymin $x1 $y2" "$xmin $ymax $x1 $y1 " "$xmax $ymin $x2 $y2"]
    set transform [makeTransform "$xmin $ymin $x1 $y2" "$xmin $ymax $x1 $y1 " "$xmax $ymin $x2 $y2"]
    oset $win transform $transform
    oset $win transform0 $transform
    
    getXtransYtrans $transform rtosx$win rtosy$win
    getXtransYtrans [inverseTransform $transform] storx$win story$win 
    
}

proc inputParse { in } {
  if { [regexp -indices \
       {D\[([a-zA-Z][0-9a-zA-Z]*[ ]*),([a-zA-Z][0-9a-zA-Z]*[ ]*)\] *=} \
	  $in all1 i1 i2] } {
   set v1 [getOneMatch $in $i1]
   set v2 [getOneMatch $in $i2]
   set s1 [string range $in [lindex $all1 1] end]

     if { [regexp -indices {,[ \n]*D\[([a-zA-Z][0-9a-zA-Z]*[ ]*),([a-zA-Z][0-9a-zA-Z]*[ ]*)\] *=} \
	  $s1 all2 i1 i2] } {
   set v3  [getOneMatch $s1 $i1]
   set v4 [getOneMatch $s1 $i2]
   set end [string first \} $s1 ]
      set form2 [string range $s1 [expr {1 + [lindex $all2 1]}] [expr {$end -1}]]
    if { "$v4" != "$v2" } {error "different variable $v2 and $v4"}

    set form1 [string range $in [expr {1 + [lindex $all1 1]}] [expr {[lindex $all2 0] + -1 + [lindex $all1 1]}]]
    return [list  $v2 $v1 $v3 $form1 $form2]
    # puts "v1=$v1,form1=$form1,form2=$form2"  
  } 
 }
}

proc composeTransform { t1 t2  } {
    desetq "a11 a12 a21 a22 e1 e2" $t1
    desetq "b11 b12 b21 b22 f1 f2" $t2
   return  [list \
	   [expr {$a11*$b11+$a12*$b21}] \
	   [expr {$a11*$b12+$a12*$b22}] \
	   [expr {$a21*$b11+$a22*$b21}] \
	   [expr {$a22*$b22+$a21*$b12}] \
	   [expr {$a11*$f1+$a12*$f2+$e1}] \
	   [expr {$a21*$f1+$a22*$f2+$e2}] ]
}
    


#
 #-----------------------------------------------------------------
 #
 # makeTransform --  Given three points mapped to three other points
 # write down the affine transformation (A.X+B) which performs this.
 # the arguments are of the form "x1 y1 u1 v1" "x2 y2 u2 v2" "x3 y3 u3 v3"
 # where (x1,y1) --> (u1,v1)  etc.
 #  Results: an affine transformation "a b c d e f" which is
 #     [ a  b ]  [ x1 ] + [ e ]     
 #     [ c  d ]  [ y1 ]   [ f ]
 #  Side Effects: none
 #
 #----------------------------------------------------------------
#
proc makeTransform { P1 P2 P3 } {
    desetq  "X1 Y1 U1 V1" $P1
    desetq  "X2 Y2 U2 V2" $P2
    desetq  "X3 Y3 U3 V3" $P3
    set tem [expr {double((($X2-$X1)*$Y3+($X1-$X3)*$Y2+($X3-$X2)*$Y1))}]
    set A [expr {(($U2-$U1)*$Y3+($U1-$U3)*$Y2+($U3-$U2)*$Y1) \
	    /$tem}]
    set B [expr {-(($U2-$U1)*$X3+($U1-$U3)*$X2+($U3-$U2)*$X1) \
	    /$tem}]
    set E [expr {(($U1*$X2-$U2*$X1)*$Y3+($U3*$X1-$U1*$X3)*$Y2+($U2*$X3-$U3*$X2)*$Y1) \
	    /$tem}]
    set C [expr {(($V2-$V1)*$Y3+($V1-$V3)*$Y2+($V3-$V2)*$Y1) \
	    /$tem}]
    set D [expr {-(($V2-$V1)*$X3+($V1-$V3)*$X2+($V3-$V2)*$X1) \
	    /$tem}]
    set F [expr {(($V1*$X2-$V2*$X1)*$Y3+($V3*$X1-$V1*$X3)*$Y2+($V2*$X3-$V3*$X2)*$Y1) \
	    /$tem}]
    set xf ""
    set yf ""
    if { $B == 0  && $C == 0 } {
	set xf "$A*\$X+$E"
	set yf "$D*\$Y+$F"
    }
    return [list $A $B $C $D $E $F]
}


#
 #-----------------------------------------------------------------
 #
 # getXtransYtrans --   If the x coordinate transforms independently
 #  of the y and vice versa, give expressions suitable for building a
 # proc. 
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc getXtransYtrans { transform p1 p2 } {
    desetq "a b c d e f"  $transform
    if { $b == 0  && $c == 0 } {
	proc $p1 { x } "return \[expr {$a*\$x+$e}\]" 
	proc $p2 { y } "return \[expr {$d*\$y+$f} \]"
	return 1
    }
    return 0
}


#
 #-----------------------------------------------------------------
 #
 # inverseTransform --   Find the inverse of an affine transformation.
 #
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc inverseTransform { transform } {
    desetq "a b c d e f" $transform
    set det [expr {double($a*$d - $b*$c)}]
    return [list [expr {$d/$det}] [expr {- $b / $det }] [expr {- $c / $det}] [expr {$a / $det}]  [expr {($b*$f-$d*$e)/ $det }] [expr {-($a*$f-$c*$e)/ $det}]]

}


#
 #-----------------------------------------------------------------
 #
 # getTicks --  given an interval (a,b) subdivide it and 
 # calculate where to put the ticks and what to print there.
 # we want DESIRED number of ticks, but we also want the ticks
 # to be at points in the real coords of the form .2*10^i or .5*10^j
 #  Results: the ticks
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#

proc getTicks { a b n } {
    set len [expr {(($b - $a))}]
    if { $len < [expr {pow(10,-40)}] } { return ""}
    set best 0
    foreach v { .1 .2 .5 } {
	# want $len/(.1*10^i) == $n
	set val($v)  [expr {ceil(log10($len/(double($n)*$v)))}]
	set use [expr {$v*pow(10,$val($v))}]
	set fac [expr {1/$use}]
	set aa [expr {$a * $fac + .03}]
	set bb [expr {$b * $fac -.03}]
	set j [expr {round(ceil($aa)) }]
	set upto [expr {floor($bb) }]
	set ticks ""
	while { $j <= $upto } {
	    set tt [expr {$j / $fac}]
	    if { $j%5 == 0 } {
		append ticks " { $tt $tt }"
	    } else  {
		append ticks " $tt"
	    }
	    incr j   
	}
	set answer($v) $ticks
	set this [llength $ticks]
	if { $this  > $best } {
	    set best $this
	    set at $v
	}
	#puts "for $v [llength $ticks] ticks"
    }
    #puts "using $at [llength $answer($at)]"
     
    return $answer($at)
}
     
proc axisTicks { win c }  {
    $c delete axisTicks
    if { ![catch {oget $win noaxisticks}] } { return }
    set swid [$c cget -width]
    set shei [$c cget -height]
    set x1 [storx$win [$c canvasx 0]]
    set y1 [story$win [$c canvasy 0]]
    set x2 [storx$win [$c canvasx $swid]]
    set y2 [story$win [$c canvasy $shei]]
    #puts "x1=$x1,y1=$y1,y2=$y2,x2=$x2"
    if { $y1 > 0  &&  $y2 < 0 } {
	set ticks [getTicks $x1 $x2 [expr {$swid/50}] ]
	#puts "ticks=$ticks"
	set eps [expr {.005 * abs($y1 - $y2)}]
	set neps [expr {-.005 * abs($y1 - $y2)}]
	set donext 0
	foreach v $ticks {
	    set x [lindex $v 0]
	    set text [lindex $v 1]
	    if { $donext } {set text [lindex $v 0] ; set donext 0 }
	    if { [lindex $v 0] == 0 } { set text "" ; set donext 1 }
	    #puts " drawTick $c $x 0 0 $neps 0 $eps  $text axisTicks"
	    drawTick $c $x 0 0 $neps 0 $eps  $text axisTicks
	    }
	}
    if { 0 < $x2 && 0 > $x1 } {
	set ticks [getTicks $y2 $y1 [expr {$shei/50}]]
	set eps [expr {.005 * ($x2 - $x1)}]
	set neps [expr {-.005 * ($x2 - $x1)}]
	set donext 0
	foreach v $ticks {
	    set y [lindex $v 0]
	    set text [lindex $v 1]
	    if { $donext } {set text [lindex $v 0] ; set donext 0}
	    if { [lindex $v 0] == 0 } { set text "" ; set donext 1}

	    drawTick $c 0 $y $neps 0 $eps 0  $text axisTicks
	    }
	}

    }


#
 #-----------------------------------------------------------------
 #
 # marginTicks --  draw ticks around the border of window
 #  x1,y1  top left x2,y2 bottom right.
 #
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#    
proc marginTicks { c x1 y1 x2 y2 tag }  {
    global printOption
    set win [winfo parent $c]

    if { ![catch {oget $win noaxisticks}] } { return }
    $c delete marginTicks
    set ticks [getTicks $x1 $x2 $printOption(xticks)]
    # puts "x=$x1 $x2"
    set eps [expr {.008 * ($y1 - $y2)}]
    set neps [expr {-.008 * ($y1 - $y2)}]
    foreach v $ticks {
	set x [lindex $v 0]
	set text [lindex $v 1]
	drawTick $c $x $y1 0 0 0 $neps  $text $tag
	drawTick $c $x $y2 0 0 0 $eps  $text $tag
	
    }
    #puts "y=$y2,$y1"
    set ticks [getTicks $y1 $y2 $printOption(yticks)]
    set eps [expr {.005 * ($x2 - $x1)}]
    set neps [expr {-.005 * ($x2 - $x1)}]
    set donext 0
    foreach v $ticks {
	set y [lindex $v 0]
	set text [lindex $v 1]
	drawTick $c $x1 $y 0 0 $eps 0  $text $tag
	drawTick $c $x2 $y 0 0 $neps 0  $text $tag
	    }
	}

proc drawTick {c x y dx dy ex ey n tags} {
    global axisGray     fontCourier8
    set win [winfo parent $c]
    set rtosx rtosx$win ; set rtosy rtosy$win
    set it [$c create line [$rtosx [expr {$x +$dx}]] [$rtosy [expr {$y +$dy}]] [$rtosx [expr {$x +$ex}]] [$rtosy [expr {$y +$ey}]] -fill $axisGray -tags $tags]
    $c lower $it
    
    if { "$n" != "" } {
   if { $ey > 0 } { set anch s
    } elseif { $ex > 0 } {set anch w 
    } elseif { $ex < 0 } {set anch e
    } elseif { $ey < 0 } {set anch n}
    
    $c create text  [$rtosx [expr {$x +1.5*$ex}]] [$rtosy [expr {$y +1.5*$ey}]] \
		-text [format "%.8g" $n] -font $fontCourier8 -tags $tags \
		-anchor $anch
}   }

proc doConfig { win }  {
    makeLocal $win c buttonFont
    $c delete configoptions
    set canv $c
   # set w $c.config
     set w $win.config
    catch {destroy $w}
    frame $w -borderwidth 2 -relief raised

    label $w.msg  -wraplength 600 -justify left -text "Plot Setup" -font $buttonFont
    pack $w
    pack $w.msg -side top
    set wb1 $w.choose1
    frame $wb1
    set wb2 $w.choose2
    frame $wb2
    pack $wb1 $wb2 -side left -fill x -pady 2m
    set item [$canv create window [$canv canvasx 10] [$canv canvasy  10] -window $w -anchor nw -tags configoptions]
    button $wb1.dismiss -command  "$canv delete $item; destroy $w " -text "ok" -font $buttonFont
    button $wb1.printoptions -text "Print Options" -command "mkPrintDialog .dial -canvas $c -buttonfont $buttonFont " -font $buttonFont

    pack $wb1.dismiss  $wb1.printoptions -side top 
    return "$wb1 $wb2"
}
# mkentry { newframe textvar text } 

# turn off the horrible show_balloons by default.
global show_balloons
set show_balloons 0

proc balloonhelp { win subwin msg } {
    global show_balloons

    if { $show_balloons == 0 } {return}
    linkLocal  [oget $win c] helpPending
    if { [info exists helpPending] } {after cancel $helpPending}
    set helpPending [after 1000 [list balloonhelp1 $win $subwin $msg]]
}

proc balloonhelp1 { win subwin msg } {
    if { ![winfo exists $win] } { return }
    makeLocal $win c buttonFont
    set x0 [winfo rootx $win]
    set y0 [winfo rooty $win]
    
    
    set atx [expr {[winfo rootx $subwin] + [winfo width $subwin] - $x0} ]
    set aty [expr {[winfo rooty $subwin] + [winfo height $subwin] - $y0} ]

    set wid [$c cget -width]
    set wid2 [expr {round ($wid /2.0)}]
    set wid10 [expr {round ($wid /10.0)}]

    if { $aty <=1 } { set aty 30 } 
    incr aty 10
    incr atx 10
    set atx [$c canvasx $atx]
    set aty [$c canvasy $aty]
    #puts "$atx $aty"
    $c delete balloon
    $c create text $atx $aty -anchor nw -text $msg -font $buttonFont -width $wid2 -fill white -fill black -tags "balloon btext"
    desetq "x1 y1 x2 y2" [$c bbox btext]

    set x1 [expr {$x1 - .3*($x2-$x1)}]
    set x2 [expr {$x2 + .3*($x2-$x1)}]
    
    set y1 [expr {$y1 - .3*($y2-$y1)}]
    set y2 [expr {$y2 + .3*($y2-$y1)}]

    eval $c create polygon $x1 $y1  $x2 $y1 $x2 $y2 $x1 $y2  -fill beige -tags balloon -smooth 1
    $c raise btext
    
}

proc setBalloonhelp { win subwin msg } {
    makeLocal $win c
    bind $subwin <Enter> "balloonhelp $win $subwin [list $msg]"
    bind $subwin <Leave> "deleteBalloon $c"
}
    
proc deleteBalloon { c } {
    linkLocal $c helpPending
    if { [info exists helpPending] } {
	after cancel $helpPending
	unset helpPending
    }
    $c delete balloon
}


#
 #-----------------------------------------------------------------
 #
 # minMax --  Compute the max and min of the arguments, which may
 # be vectors or numbers
 #
 #  Results: list of MIN and MAX
 #
 #  Side Effects: none
 #
 #----------------------------------------------------------------
#
proc minMax { args } {
    set max [lindex [lindex $args 0] 0] ; set min $max ;
    foreach vec $args {
	foreach v $vec {
	    if { $v > $max } {set max $v }
	    if { $v < $min} {set min $v }
	}
    }
    return [list $min $max]
}

proc matrixMinMax { list } {
# compute the min max of the list    
    set min +10e300
    set max -10e300
    foreach mat $list {
	foreach row $mat {
	    foreach v [ldelete nam $row] {
		if { $v > $max } {catch  { set max [expr {$v + 0}] }}
		if { $v < $min} {catch  { set min [expr {$v + 0}] }}
		}
	    }
	}
    list $min $max
}
	
proc omPlotAny { data args } {
    # puts "data=<[lindex $data 0]>"
    set command [list [lindex [lindex $data 0] 0]  -data [lindex $data 0] ]
    if { "[lindex $command 0]" == "plot2d" } {
	lappend command -xfun {}
    }
    foreach v $args { [lappend command $v] }
    eval $command
    #eval [lindex [lindex $data 0] 0] -xfun [list {}] -data [list [lindex $data 0]] $args
}


proc resizeSubPlotWindows { win wid height } {
    set at [$win yview "@0,0"]
    foreach w [winfo children $win] {
	if { [string match plot* [lindex [split $w .] end]] } {
	    resizePlotWindow $w [winfo width $w] $height
    }
  }
  if { "$at" != "" } { $win yview $at} 
 }


	    
proc resizePlotWindow  { w width height } {
    if { [winfo width $w.c] <= 1 } {
	after 100 update ;
	return }
	if { ![catch { set tem [oget $w lastResize] } ] && [expr {[clock seconds] - $tem }] < 2 } { return
} else { oset $w lastResize [clock seconds ]
    }
    #puts "resizePlotWindow $w $width $height"
	
   # return
   set par [winfo parent $w]
   set facx 1.0
   set facy 1.0    
    set wid [winfo width $par]
    set hei [winfo height $par]
    
   if { "[winfo class $par]" == "Text" } {
   set dif 10

   set wid1 $wid ; set hei1 $hei
   #puts "now w=$w"
   #set wid1 [getPercentDim [oget $w widthDesired] width $par]        
   catch {set wid1 [getPercentDim [oget $w widthDesired] width $par] }
   catch {set hei1 [getPercentDim [oget $w heightDesired] height $par] }
   set wid [expr {($wid1 > $wid - 30 ? $wid - 30 : $wid1 )}]
   set hei [expr {($hei1 > $hei - 30 ? $hei - 30 : $hei1 )}]
   } else {
       set dif 10

   }
   

    #puts "width arg=$width,width $w=[winfo width $w],wid of $par=$wid,height=$height,hei=$hei,\[winfo width \$w.c\]=[winfo width $w.c]"
#     if { $width > $wid -20 || $wid > $width -20 }
    if { (abs($width-$wid) > $dif ||  abs($height-$hei) > $dif)
&&  [winfo width $w.c] > 1 } {
    set eps [expr {2 * [$w.c cget -insertborderwidth] + [$w.c cget -borderwidth] }]
    set epsx $eps
    set epsy $eps
	#puts "reconfiguring: w=$w,par=$par,dif=$dif,widths=$wid, \
    $width,[winfo width $par],[winfo width $w],[winfo width $w.c]\
	heights=$hei,$height,[winfo height $par],[winfo height $w],\
    [winfo height $w.c]"
    
    set extrawidth [expr {([winfo width $w] - [winfo width  $w.c]) +$epsx}]
    set extraheight [expr {([winfo height $w] - [winfo height  $w.c]) +$epsy}]
    set nwidth [expr {$wid - ($extrawidth > 0  ? $extrawidth : 0)}]
    set nheight [expr {$hei - ($extraheight > 0  ? $extraheight : 0)}]
    
    #puts "$w.c config -width $nwidth  -height $nheight, extraheight=$extraheight,epsy=$epsy"
    $w.c config -width $nwidth  -height $nheight

		}
    		
 }



proc bboxToRadius { win  } {
    makeLocal $win bbox
    if { "$bbox" != "" } {
	linkLocal $win       xradius yradius xcenter ycenter
	set i 0
	foreach v { x y z } {

	    set min [lindex $bbox $i]
	    set max [lindex $bbox [expr $i +2]]
	    if { "$min" != "" } {
		if { $min >= $max } {error "bad bbox $bbox since $min >= $max"}
	    set ${v}radius [expr { ($max - $min) /2.0}]
	    set ${v}center [expr { ($max + $min) /2.0}]
	    }
	}
    }
}

proc updateParameters { win var value} {
    linkLocal $win parameters
#    puts "$win $var $value"
    set ans ""
    set comma ""
    
    foreach {v val} [splitParams $parameters] {
        if { "$v" == "$var" } {
	    set val $value
	}
	append ans $comma $v=$val
	set comma ","
    }
#    puts "parameters=$ans"
    set parameters $ans
}

proc addSliders { win } {
    linkLocal $win sliders c width parameters
    set i 0
    if { "$sliders" == "" } { return }
    catch { destroy $c.sliders }
    set bg "#22aaee"
    set trough "#22ccff"
    frame $c.sliders -relief raised -highlightthickness 2 -highlightbackground $trough
    foreach v [split $sliders ,] {
	if { [regexp {([a-zA-Z0-9]+)[ ]*=?(([---0-9.]+):([---0-9.]+))?} $v  junk var junk x0 x1] } {
	    incr i
	    if { "$x0" == "" } { set x0 -5  ; set x1 5}

	    set fr $c.sliders.fr$i
	    frame $fr -background $bg 
	    label $fr.lab -text $var: -background $bg 
	    label $fr.labvalue -textvariable [oloc $win slidevalue$i]  -background $bg -relief sunken -justify left
	    scale $fr.scale -command "sliderUpdate $win $var" \
		    -from "$x0" -to $x1 -orient horizontal \
	    -resolution [expr ($x1 - $x0) < 1 ? ($x1-$x0)/100.0 : .01] \
	    -length [expr {$width/2}] -showvalue 0 -variable [oloc $win slidevalue$i] -background $bg -troughcolor "#22ccff" -highlightthickness 0
	    pack $fr.lab -side left -expand 1 -fill x
	    pack $fr.labvalue $fr.scale -side left
	    pack  $fr -side top -expand 1 -fill x
	    set found 0
	    set val  [assoc $var [splitParams $parameters] no]
	    if { "$val" == "no" } {
		set val  [expr ($x1 + $x0)/2.0]
		if { "$parameters" != "" }  { append parameters , } 
		append parameters $var=$val
	    }
	    $fr.scale set $val
	}
    }

    place  $c.sliders -in $c -x 4 -rely 1.0 -y -4 -anchor sw

    }
	

proc sliderUpdate { win var val } {
    linkLocal $win sliderCommand parameters
    set params $parameters
    updateParameters $win $var $val
    if { "$params" != "$parameters" &&
    [info exists sliderCommand] } {

    $sliderCommand $win $var $val
}   }
    



## endsource plotconf.tcl
## source plotdf.tcl

###### plotdf.tcl ######
#######################################################################
#######  Copyright William F. Schelter.  All rights reserved.  ########
#######################################################################

global plotdfOptions
set plotdfOptions {
    {dxdt "x-y^2+sin(x)*.3" {specifies dx/dt = dxdt.  eg -dxdt "x+y+sin(x)^2"} }
    {dydt "x+y" {specifies dy/dt = dydt.  eg -dydt "x-y^2+exp(x)"} }
    {dydx "" { may specify dy/dx = x^2+y,instead of dy/dt = x^2+y and dx/dt=1 }}
    {adamsMoulton red "Color to do adams moulton integration in. None means dont do" }
    {rungeKuttaA "" "Color to do Runge Kutta adaptive integration in. None means dont do" }
    
    {xradius 10 "Width in x direction of the x values" }
    {yradius 10 "Height in y direction of the y values"}
    {width 500 "Width of canvas in pixels"}
    {height 500 "Height of canvas in pixels" }
    {scrollregion {} "Area to show if canvas is larger" }
    {xcenter 0.0 {(xcenter,ycenter) is the origin of the window}}
    {ycenter 0.0 "see xcenter"}
    {bbox "" "xmin ymin xmax ymax .. overrides the -xcenter etc"}
    {tinitial 0.0 "The initial value of variable t"}
    {nsteps 100 "Number of steps to do in one pass"}
    {xfun "" "A semi colon separated list of functions to plot as well"}
    {tstep "" "t step size"}
    {direction "both" "May be both, forward or backward" }
    {versus_t 0 "Plot in a separate window x and y versus t, after each trajectory" }
    {windowname ".dfplot" "window name"}
    {parameters "" "List of parameters and values eg k=3,l=7+k"}
    {sliders "" "List of parameters ranges k=3:5,u"}
    {linecolors { green black  brown gray black} "colors to use for lines in data plots"}
    {doTrajectoryAt "" "Place to calculate trajectory"}
    {linewidth "1.0" "Width of integral lines" }
    {nolines 0 "If not 0, plot points and nolines"}
    {bargraph 0 "If not 0 this is the width of the bars on a bar graph" }
    {plotpoints 0 "if not 0 plot the points at pointsize" }
    {pointsize 2 "radius in pixels of points" }
    {autoscale "x y" "Set {x,y}center and {x,y}range depending on data and function. "}
    {zoomfactor "1.6 1.6" "Factor to zoom the x and y axis when zooming.  Zoom out will be reciprocal" }
    {errorbar 0 "If not 0 width in pixels of errorbar.  Two y values supplied for each x: {y1low y1high y2low y2high  .. }"}
     {data "" "List of data sets to be plotted.  Has form { {xversusy {x1 x2 ... xn} {y1 .. yn ... ym}} .. {againstIndex {y1 y2 .. yn}}  .. }"}
    {labelposition "10 35" "Position for the curve labels nw corner"}
}

if { "[info proc makeFrame]" == "" } { source "plotconf.tcl" }
 proc makeFrameDf { win } {
   set w [makeFrame $win df]
    makeLocal $win c dydx

    set top $win
   # puts "w=$w,win=$win"
    catch { set top [winfo parent $win]}
    catch {

    wm title $top "Direction Fields"
    wm iconname $top "DF plot"
#    wm geometry $top 750x700-0+20
   }
    set wb $w.buttons
   makeLocal $win buttonFont 
   label $w.msg  -wraplength 600 -justify left -text "A direction field plotter by William Schelter" -font $buttonFont
   
  button $wb.integrate -text "Integrate" -command "setForIntegrate $w" -font $buttonFont
   setBalloonhelp $win $wb.integrate {Causes clicking on the  plot with the left mouse button at a point, to draw a trajectory passing through that point.   Under Config there is an entry box which allows entering exact x,y coordinates, and which also records the place of the last trajectory computed.}

  button $wb.plotversust -text "Plot Versus t" -command "plotVersusT $w" -font $buttonFont
   setBalloonhelp $win $wb.plotversust {Plot the x and y values for the  last trajectory versus t.}   
   
   
  setForIntegrate $w
  pack $wb.integrate -side top -expand 1 -fill x
  pack $wb.plotversust -side top -expand 1 -fill x
 # pack $w.msg -side top
  pack $w
  return $win
}

proc swapChoose {win msg winchoose } {
   # global dydx dxdt dydt
    
    if { "$msg" == "dydt" } {
	pack $winchoose.dxdt -before $winchoose.dydt -side bottom
	oset $win dydx ""
	$winchoose.dydt.lab config -text "dy/dt"
    } else {
	pack forget $winchoose.dxdt
	oset $win dxdt 1
	oset $win dydx " "
	$winchoose.dydt.lab config -text "dy/dx"
    }
}
	

proc doHelpdf { win } {
    global Parser
 doHelp $win [join [list \
{
William Schelter's solver/plotter for ode systems.

To QUIT this HELP click here.

Clicking at a point computes the trajectory
(x(t),y(t)) starting at that point, and satisfying
the differential equation
	
  dx/dt = dxdt
  dy/dt = dydt

By clicking on Zoom, the mouse now allows you to zoom
in on a region of the plot.  Each click near a point
magnifies the plot, keeping the center at the point
you clicked.  Depressing the SHIFT key while clicking
zooms in the opposite direction.

To resume computing trajectories click on Integrate.

To change the differential equation, click on Config and
enter new values in the entry windows, and then click on
Replot in the main menu bar.

Holding the right mouse button down allows you to drag
(translate) the plot sideways or up and down.

Additional parameters such as the number of steps (nsteps),
the initial t value (tinitial), and the x and y centers
and radii, may be set under the  Config menu.

You may print to a postscript printer, or save the plot \
as a postscript file, by clicking on save.   To change \
between printing and saving see the Print Options under Config.
	
} $Parser(help)]]
}

proc setForIntegrate { win} {
    makeLocal $win c
    $c delete printrectangle
    bind $c  <1> "doIntegrateScreen $win %x %y "
}

## source rk.tcl

###### rk.tcl ######
#######################################################################
#######  Copyright William F. Schelter.  All rights reserved.  ########
#######################################################################

#proc try { } {
    #  proc ff { a b c } { return [expr {$b + $c}] }
    #  proc gg { a b c } { return [expr {$b - $c}] }
#  rungeKutta ff gg 0.2 0.2 0 .01 10
#}

proc rungeKutta { f g t0 x0 y0  h nsteps } {
  set n $nsteps
  set ans "$x0 $y0"
  set xn $x0
  set yn $y0
  set tn $t0
    set h2 [expr {$h / 2.0 }]
    set h6 [expr {$h / 6.0 }]
 catch {
  while { [incr nsteps -1] >= 0 } {
  

  set kn1 [$f $tn $xn $yn]
  set ln1 [$g $tn $xn $yn]

      set arg [list [expr {$tn + $h2}] [expr {$xn + $h2 * $kn1}] [expr {$yn + $h2*$ln1}]]
  set kn2 [eval $f $arg]
  set ln2 [eval $g $arg]

      set arg [list [expr {$tn + $h2}] [expr {$xn + $h2 * $kn2}] [expr {$yn +$h2*$ln2}]]
  set kn3 [eval $f $arg]
  set ln3 [eval $g $arg]

      set arg [list [expr {$tn + $h}] [expr {$xn + $h * $kn3}] [expr {$yn + $h*$ln3}]]
  set kn4 [eval $f $arg]
  set ln4 [eval $g $arg]

      set xn [expr {$xn + $h6 * ($kn1+2*$kn2+2*$kn3+$kn4)}]
      set yn [expr {$yn + $h6 * ($ln1+2*$ln2+2*$ln3+$ln4)}]
      set tn [expr {$tn+ $h}]

  lappend ans  $xn $yn
  }
 }

 return $ans 
}

proc pathLength { list } {
  set sum 0
  foreach { x y } $list {
      set sum [expr {$sum + sqrt($x*$x+$y*$y)}]
 }
  return $sum
}
proc rungeKuttaA { f g t0 x0 y0  h nsteps } {
  set ans [rungeKutta $f $g $t0 $x0 $y0 $h $nsteps]
  set count 0
  # puts "retrying([llength $ans]) .."
  while { [llength $ans] < $nsteps * .5  && $count < 7 } {
       incr count
       #set leng [pathLength $ans]
       #if { $leng == 0 } {set leng .001}
       set th [expr {$h / 3.0}]
       if { $th  < $h }  { set h $th }
       set ans  [rungeKutta $f $g $t0 $x0 $y0 $h $nsteps]
      # puts -nonewline "..(h=[format "%.5f" $h],pts=[llength $ans])"
       # flush stdout
  }
  return $ans
}

  

## endsource rk.tcl
## source adams.tcl

###### adams.tcl ######


proc adamsMoulton { f g t0 x0 y0  h nsteps } {
    set ans [rungeKutta $f $g $t0 $x0 $y0 $h 3]
    catch { 
    set i 0
    set h24 [expr {$h /24.0}]
    foreach { x y } $ans {
	lappend listXff [xff  [expr {$t0 + $i * $h} ] $x $y]
	lappend listYff [yff  [expr {$t0 + $i * $h} ] $x $y]
	incr i
	set xn $x
	set yn $y
    }

    set n [expr $nsteps -3]

    while { [incr n -1] >= 0 } {

    #puts "listXff = $listXff"
    #puts "listYff = $listYff"		
    # adams - bashford formula:
    set xp [expr {$xn + ($h24)*(55 *[lindex $listXff 3]-59*[lindex $listXff 2]+37*[lindex $listXff 1]-9*[lindex $listXff 0]) }]
    set yp [expr {$yn + ($h24)*(55 *[lindex $listYff 3]-59*[lindex $listYff 2]+37*[lindex $listYff 1]-9*[lindex $listYff 0]) }]
    #puts "i=$i,xp=$xp,yp=$yp"
    # adams-moulton corrector-predictor:
    # compute the yp = yn+1 value..
    set t [expr {$t0 + $i * $h}]
    incr i
    if { 1 } {
    set xap [expr { $xn+($h24)*(9*[xff $t $xp $yp]+19*[lindex $listXff 3]-5*[lindex $listXff 2]+[lindex $listXff 1]) }]
    set yap [expr { $yn+($h24)*(9*[yff $t $xp $yp]+19*[lindex $listYff 3]-5*[lindex $listYff 2]+[lindex $listYff 1]) }]

    set xn $xap
    set yn $yap
   # puts "after correct:i=[expr $i -1],xn=$xn,yn=$yn"	
    # could repeat it, or check against previous to see if changes too much.
	}
    set listXff [lrange $listXff 1 end]
    set listYff [lrange $listYff 1 end]

    lappend listXff [xff $t $xn $yn]
    lappend listYff [yff $t $xn $yn]

    lappend ans $xn $yn
   # puts "ans=$ans"	
    }
    #puts "adams:t=$t"
  }
    return $ans
}

## endsource adams.tcl

# sample procedures
# proc xff { t x y } { return [expr {$x + $y }] }
# proc yff { t x y } { return [expr {$x - $y }] }

proc doIntegrateScreen { win sx sy  } {
    makeLocal $win c
    doIntegrate $win [storx$win [$c canvasx $sx]] [story$win [$c canvasy $sy]]
}

proc doIntegrate { win x0 y0 } {
    # global xradius yradius c tstep  nsteps
#    puts "dointegrate $win $x0 $y0"
    makeLocal $win xradius yradius c tstep  nsteps direction linewidth tinitial versus_t linecolors
    linkLocal $win didLast trajectoryStarts
    set rtosx rtosx$win ; set rtosy rtosy$win      
    oset $win doTrajectoryAt [format "%.10g  %.10g" $x0 $y0]
    lappend trajectoryStarts [list $x0 $y0]
    
    set didLast {}
    # puts "doing at $doTrajectoryAt"
    set steps $nsteps
    if { "$tstep" == "" } {  
	set h [expr {[vectorlength $xradius $yradius] / 200.0}]
	set tstep $h
    } else {set h $tstep }
    
    # puts h=$h
    set todo $h
    switch $direction {
	forward { set todo "$h" }
	backward { set todo "[expr {- $h}]" }
	both { set todo "$h [expr {- $h}]" }
    }
    foreach method { adamsMoulton rungeKuttaA  } {
	set color [oget $win $method]
	if { "$color" != "" } {
	    lappend methods $method
	    lappend useColors $method $color
	}
    }
    set methodNo -1
    foreach method $methods {
	incr methodNo
#    puts method=$method
    foreach h $todo {
	set form [list $method xff yff $tinitial $x0 $y0 $h $steps]
	set ans [eval $form]
	lappend didLast $form

	#puts "doing: $form"
	set i -1
	set xn1 [$rtosx [lindex $ans [incr i]]]
	set yn1 [$rtosy [lindex $ans [incr i]]]
	set lim [expr {$steps * 2}]
	set mee [expr {pow(10.0,9)}]
	set ptColor [assoc $method $useColors ]
	set linecolor [lindex $linecolors $methodNo]
	#set im [getPoint 2 green]
	#set im1 [getPoint 2 purple]
	set im [getPoint 2 $ptColor]
	#set im1 [getPoint 2 purple]		
	catch  { 
	    while { $i <= $lim } {
		set xn2  [$rtosx [lindex $ans [incr i]]]
		set yn2  [$rtosy [lindex $ans [incr i]]]
		# puts "$xn1 $yn1"
		# xxxxxxxx following is for a bug in win95 version
		if { abs($xn1) + abs($yn1) +abs($xn2)+abs($yn2) < $mee    } {
		    $c create line $xn1 $yn1 $xn2 $yn2 -tags path -width $linewidth -fill $linecolor
		    
		}
		
                if { "$im" != "" } {
			#puts hi
			$c create image $xn1 $yn1 -image $im -anchor center \
				-tags "point"
			
		    } else {
		    $c create oval [expr $xn1 -2] [expr $yn1 -2] [expr $xn1 +2] [expr $yn1 +2] -fill $color

		}
	    
			
		    

		# puts "$xn1 $yn1"
		set xn1 $xn2
		set yn1 $yn2
	    }
    }   }
  }
  if { $versus_t } { plotVersusT $win}
}


proc plotVersusT {win } {
    linkLocal $win didLast dydt dxdt parameters xcenter xradius
    set nwin .versust.plot2d
    if { "$parameters" != ""  } { set pars ", $parameters"} else { set pars ""}
    oset $nwin themaintitle "dy/dt=$dydt, dx/dt=$dxdt $pars"
    lappend plotdata [list maintitle [list oget $nwin themaintitle]]


    foreach v $didLast {
	set ans [eval $v]
	desetq "tinitial x0 y0 h" [lrange $v 3 end]
	set this [lrange $v 0 5]
	if { [info exists doing($this) ] } { set tem $doing($this) } else {
	    set tem ""
	}
	set doing($this) ""
	set allx "" ; set ally "" ; set allt ""
	set ii 0 
	foreach {x y } $ans {
	    lappend allx $x
	    lappend ally $y
	    lappend allt [expr $tinitial + $h*$ii]
	    incr ii
	}
	
	foreach u $tem v [list $allx $ally $allt] {
		if { $h > 0 } { lappend doing($this) [concat $u $v]} else {
		    lappend doing($this) [concat [lreverse $v] $u]
	    }   }
	}

    foreach {na val } [array get doing] {
	lappend plotdata [list label "x versus t"] [list plotpoints 2]
	lappend plotdata [list xversusy [lindex $val 2] [lindex $val 0] ]
	lappend plotdata [list label "y versus t"]	
	lappend plotdata [list xversusy [lindex $val 2] [lindex $val 1] ]
    }
    if { ![winfo exists .versust] } {
	toplevel .versust
    }
    

    plot2d -data $plotdata -windowname $nwin -ycenter $xcenter -yradius $xradius
    wm title .versust "X and Y versus t"
}

proc lreverse { lis } {
    set ans ""
    set i [llength $lis]
    while { [incr i -1]>=0 } {
	lappend ans [lindex $lis $i]
    }
    return $ans
}


#
 #-----------------------------------------------------------------
 #
 # $rtosx,$rtosy --  convert Real coordinate to screen coordinate
 #
 #  Results: a window coordinate
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------


#
 #-----------------------------------------------------------------
 #
 # $storx,$story --  Convert a screen coordinate to a Real coordinate.
 #
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#

proc drawArrowScreen { c atx aty dfx dfy } {

    set x1 [expr {$atx + $dfx}]
    set y1 [expr {$aty + $dfy}]
    #   set x2 [expr {$atx + .8*$dfx +.1* $dfy}]
    #   set y2 [expr {$aty + .8*$dfy - .1* $dfx}]
    #   set x3 [expr {$atx + .8*$dfx -.1* $dfy}]
    #   set y3 [expr {$aty + .8*$dfy + .1* $dfx}]
  $c create line $atx $aty $x1 $y1 -tags arrow -fill blue -arrow last -arrowshape {3 5 2}
#  $c create line $x2 $y2  $x1 $y1 -tags arrow -fill red
#  $c create line $x3 $y3 $x1 $y1 -tags arrow -fill red
}

proc drawDF { win tinitial } {
    global  axisGray
    makeLocal  $win xmin xmax   xcenter ycenter c ymin ymax transform

    # flush stdout
    set rtosx rtosx$win ; set rtosy rtosy$win
    set storx   storx$win  ;   set story   story$win  
    set stepsize 30
    set min 100000000000.0
    set max 0.0
    set t0 $tinitial
    set xfactor [lindex $transform 0]
    set yfactor [lindex $transform 3]
    set extra $stepsize  
    set uptox [expr {[$rtosx $xmax] + $extra}]
    set uptoy [expr {[$rtosy $ymin] + $extra}]
    # draw the axes:
    #puts "draw [$rtosx $xmin] to $uptox"
    for { set x [expr {[$rtosx $xmin] - $extra}] } { $x < $uptox } { set x [expr {$x +$stepsize}] } {
	for { set y [expr {[$rtosy $ymax] - $extra}] } { $y < $uptoy } { set y [expr {$y + $stepsize}] } {
	    set args "$t0 [$storx $x] [$story $y]"
	    set dfx [expr {$xfactor * [eval xff $args]}]
	    # screen y is negative of other y
	    set dfy [expr  {$yfactor * [eval yff $args]}]
	    #     puts "$dfx $dfy"
	    set len  [vectorlength $dfx $dfy]
	    append all " $len $dfx $dfy "
	    if { $min > $len } { set min $len }
	    if { $max < $len } {set  max $len}
    }   }
    set fac [expr {($stepsize -5 -8)/($max - $min)}]
    set arrowmin 8
    set arrowrange [expr {$stepsize -4 - $arrowmin}]
    set s1 [expr {($arrowrange*$min+$arrowmin*$min-$arrowmin*$max)/($min-$max)}]
    set s2 [expr {$arrowrange/($max-$min) }]
    # we calculate fac for each length, so that
    # when we multiply the vector times fac, its length
    # will fall somewhere in [arrowmin,arrowmin+arrowrange].
    # vectors of length min and max resp. should get mapped
    # to the two end points.
    # To do this we set fac [expr {$s1/$len + $s2}]
    # puts "now to draw,s1=$s1 s2=$s2,max=$max,min=$min"
    # puts "xfactor=$xfactor,yfactor=$yfactor"

    
    set i -1
    for { set x [expr {[$rtosx $xmin] - $stepsize}] } { $x < $uptox } { set x [expr {$x +$stepsize}] } {
	for { set y [expr {[$rtosy $ymax] - $stepsize}] } { $y < $uptoy } { set y [expr {$y + $stepsize}] } {
	    
	    
	    set len [lindex $all [incr i]]
	    
	    set fac [expr {$s1/$len + $s2}]
	    set dfx [lindex $all [incr i]]
	    set dfy [lindex $all [incr i]]
	    #puts "[$storx $x] [$story $y] x=$x y=$y dfx=$dfx dfy=$dfy fac=$fac"
	    # puts "$len $dfx $dfy" 
	    drawArrowScreen $c $x $y [expr {$fac * $dfx}] [expr {$fac * $dfy}]
        }
    }
    
    $c create line [$rtosx 0 ] [$rtosy -1000] [$rtosx 0] [$rtosy 1000] \
	    -fill $axisGray
    $c create line [$rtosx -1000] [$rtosy 0] [$rtosx 1000] [$rtosy 0] \
	    -fill $axisGray
    axisTicks $win $c
}

proc parseOdeArg {  s } {
    set orig $s
    set w "\[ ]*"
    set exp "\[dD]$w\\($w\(\[xyz])$w,$w\(\[xyt])$w\\)$w=(\[^;]+)"
    while { [regexp $exp $s junk x t expr ] } {
	lappend ans  -d${x}d$t
	lappend ans $expr
	regexp -indices $exp $s junk x t expr
	set s [string range $s [lindex $junk 1] end]
    }
    if { ![info exists ans] || ([llength $ans] == 2 && "[lindex $ans 0]" != "-dydx") } {
	error "bad -ode argument: $orig\nwant d(y,x)=f(x,y) \n   OR d(x,t)=f(x,y) d(y,t)=g(x,y) "
    }
    return $ans
}



proc plotdf { args } {
    global plotdfOptions   printOption printOptions plot2dOptions
    # puts "args=$args"
    # to see options add: -debug 1
    set win [assoc -windowname $args]
    if { "$win" == "" } {set win [getOptionDefault windowname $plotdfOptions] }
    if { "[set ode [assoc "-ode" $args]]" != "" }  {
	set args [delassoc -ode $args]
	set args [concat [parseOdeArg $ode] $args]
    }
    global [oarray $win]
    getOptions $plotdfOptions $args -usearray [oarray $win]

    makeLocal $win dydx

    if { "$dydx" !="" } { oset $win dxdt 1 ; oset $win dydt $dydx }
    setPrintOptions $args
    foreach v {trajectoryStarts recompute} {
	    catch { unset [oloc $win $v]  }
	}
    
    makeFrameDf $win
    oset $win sliderCommand sliderCommandDf
    oset $win trajectoryStarts ""
	

    oset $win maintitle [concat "makeLocal $win  dxdt dydt dydx ;"  \
	    {if { "$dydx" == "" } { concat "dx/dt = $dxdt , dy/dt = $dydt"}  else {
	concat "dy/dx = $dydt" } } ]
	replotdf $win
    }

proc replotdf { win } {
    global plotdfOptions
    linkLocal $win xfundata data
    if { ![info exists data] } {
	set data ""
	
    }
    makeLocal $win c dxdt dydt tinitial nsteps xfun     doTrajectoryAt parameters 

    setUpTransforms $win 1.0
    setXffYff $dxdt $dydt $parameters
    $c delete all
    setForIntegrate $win
    oset $win curveNumber -1
    drawDF $win $tinitial
    if { "$doTrajectoryAt" != "" } {
         eval doIntegrate $win  $doTrajectoryAt
    }
    set xfundata ""
    foreach v [sparseListWithParams $xfun {x y t} $parameters ] {
	proc _xf {  x  } "return \[expr { $v } \]"
	regsub "\\$" $v "" label
	lappend xfundata [list label $label] \
	  [linsert [calculatePlot $win _xf $nsteps]  \
		0 xversusy]
    }
    redraw2dData $win -tags path
    
}    

proc setXffYff { dxdt dydt parameters } {
    
    proc xff { t x y } "expr { [sparseWithParams $dxdt { x y} $parameters] }"    
    proc yff { t x y } "expr { [sparseWithParams $dydt { x y} $parameters] } "
}

proc doConfigdf { win } {
    desetq "wb1 wb2" [doConfig $win]
    makeLocal $win buttonFont 
    frame $wb1.choose1
    set frdydx $wb1.choose1
    button $frdydx.dydxbut -command "swapChoose $win dydx $frdydx " \
	    -text "dy/dx" -font $buttonFont
    button $frdydx.dydtbut -command "swapChoose $win dydt $frdydx" \
	    -text "dy/dt,dx/dt" -font $buttonFont
    mkentry $frdydx.dxdt [oloc $win dxdt] "dx/dt" $buttonFont
    mkentry $frdydx.dydt [oloc $win dydt] "dy/dt" $buttonFont
    pack $frdydx.dxdt  $frdydx.dydt -side bottom  -fill x -expand 1
    pack $frdydx.dydxbut $frdydx.dydtbut -side left -fill x -expand 1
    
    foreach w {versus_t parameters linewidth xradius yradius xcenter ycenter tinitial nsteps tstep direction xfun linecolors rungeKuttaA adamsMoulton } {
	mkentry $wb1.$w [oloc $win $w] $w $buttonFont
	pack $wb1.$w -side bottom -expand 1 -fill x
    }
    mkentry $wb1.doTrajectoryAt [oloc $win doTrajectoryAt] \
	    "Trajectory at" $buttonFont
    bind $wb1.doTrajectoryAt.e <KeyPress-Return> \
	    "eval doIntegrate $win \[oget $win doTrajectoryAt\] "
    pack  $wb1.doTrajectoryAt   $frdydx    -side bottom -expand 1 -fill x
    if { "[oget $win dydx]" != "" } { swapChoose $win dydx $frdydx }
    setForIntegrate $win
 }



proc sliderCommandDf { win var val } {
    linkLocal $win recompute
    updateParameters $win $var $val
    set com "recomputeDF $win"
    # allow for fast move of slider...
    #mike FIXME: this is a wrong use of after cancel
    after cancel $com
    after 50 $com
}

proc recomputeDF { win } {
    linkLocal $win  recompute 
    if { [info exists recompute]  } {
	incr recompute
	return
    } else {
#	puts "set recompute 1"
	set recompute 1
    }
    linkLocal $win trajectoryStarts  c tinitial dxdt dydt parameters
    set redo 0
    set trajs ""

    catch {     set trajs $trajectoryStarts} 


    while { $redo != $recompute } {
#	puts "	setXffYff $dxdt $dydt $parameters"
	setXffYff $dxdt $dydt $parameters
#	$c delete path point arrow
	$c delete all
	catch { unset  trajectoryStarts }
	set redo $recompute
	foreach pt $trajs {
	    desetq "x0 y0" $pt
	    catch { doIntegrate $win $x0 $y0 }
	    update
	    if { $redo != $recompute } { break }
	}
	if  { $redo == $recompute } {
	    catch { drawDF $win $tinitial }
	}
    }
#    puts "    unset recompute"
    unset recompute
}
    

	
		
	
	
    

## endsource plotdf.tcl
## source plot2d.tcl

###### plot2d.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

global p
set p .plot
if {[winfo exists $p]} {catch { destroy $p }}

global plot2dOptions
set plot2dOptions { 
    {xradius 10 "Width in x direction of the x values" }
    {yradius 10 "Height in y direction of the y values"}
    {width 500 "Width of canvas in pixels"}
    {height 500 "Height of canvas in pixels" }
    {xcenter 0.0 {(xcenter,ycenter) is the origin of the window}}
    {xfun "" {function of x to plot eg: sin(x) or "sin(x);x^2+3" }}
    {parameters "" "List of parameters and values eg k=3,l=7+k"}
    {sliders "" "List of parameters ranges k=3:5,u"}
    {nsteps "100" "mininmum number of steps in x direction"}
    {ycenter 0.0 "see xcenter"}
    {bbox "" "xmin ymin xmax ymax .. overrides the -xcenter etc"}
    {screenwindow "20 20 700 700" "Part of canvas on screen"}

    {windowname ".plot2d" "window name"}
    {nolines 0 "If not 0, plot points and nolines"}
    {bargraph 0 "If not 0 this is the width of the bars on a bar graph" }
    {linewidth "0.6" "Width of plot lines" }
    {plotpoints 0 "if not 0 plot the points at pointsize" }
    {pointsize 2 "radius in pixels of points" }
    {linecolors {blue green red brown gray black} "colors to use for lines in data plots"}
    {labelposition "10 35" "Position for the curve labels nw corner"}
    {xaxislabel "" "Label for the x axis"}
    {yaxislabel "" "Label for the y axis"}
    {autoscale "y" "Set {x,y}center and {x,y}range depending on data and function. Value of y means autoscale in y direction, value of {x y} means scale in both.  Supplying data will automatically turn this on."}
    {zoomfactor "1.6 1.6" "Factor to zoom the x and y axis when zooming.  Zoom out will be reciprocal" }
    {errorbar 0 "If not 0 width in pixels of errorbar.  Two y values supplied for each x: {y1low y1high y2low y2high  .. }"} 
    {data "" "List of data sets to be plotted.  Has form { {xversusy {x1 x2 ... xn} {y1 .. yn ... ym}} .. {againstIndex {y1 y2 .. yn}}  .. }"}
}

proc argSuppliedp { x } {
  upvar 1 args a
  return [expr [set i [lsearch $a $x]] >= 0 && $i%2 == 0] 
}
    
proc mkPlot2d { args } {
    global plot2dOptions  printOption axisGray
    #puts "args=<$args>"
    # global  screenwindow c xmax xmin ymin ymax 
    # eval global [optionFirstItems $plot2dOptions]
    set win [assoc -windowname $args]
    if { "$win" == "" } {
	set win [getOptionDefault windowname $plot2dOptions] }
    global  [oarray $win]
    set data [assoc -data $args ]
    # puts ranges=[plot2dGetDataRange $data]

    getOptions $plot2dOptions $args -usearray [oarray $win]
    linkLocal $win autoscale 
    if { [argSuppliedp -data] && ![argSuppliedp -autoscale] &&
	![argSuppliedp -xradius] } {
	lappend autoscale x
	}
    if { ![argSuppliedp -autoscale] & [argSuppliedp -yradius] } {
	set autoscale [ldelete y $autoscale]
	}	
    
    oset $win curveNumber -1
    setPrintOptions $args
    oset $win maintitle ""	
    setupCanvas $win 
    catch { destroy $windowname }
    
    makeFrame2d $win
    oset $win sliderCommand sliderCommandPlot2d
    makeLocal $win c
    return $win
    
}

proc  makeFrame2d  { win } {
    set w [makeFrame $win 2d]
    set top $w
    catch { set top [winfo parent $w]}
    catch {
    wm title $top "Schelter's 2d Plot Window"
    wm iconname $top "2d plot"
   # wm geometry $top 750x700-0+20
   }
    pack $w
   return $w

}

proc doConfig2d { win } {
    desetq "wb1 wb2" [doConfig $win]
    makeLocal $win buttonFont 
    mkentry $wb1.nsteps [oloc $win nsteps]  "Number of mesh grids"  $buttonFont
    mkentry $wb1.xfun [oloc $win xfun]  "y=f(x)"  $buttonFont
    bind $wb1.xfun.e <Return> "replot2d $win"
    # button .jim.buttons.rot "rotate" -command "bindForRotation"
    # pack .jim.buttons.rot
    pack $wb1.xfun  $wb1.nsteps -expand 1 -fill x
    foreach w {xradius yradius xcenter ycenter linecolors autoscale linewidth parameters} {
	mkentry $wb1.$w [oloc $win $w] $w $buttonFont
	pack $wb1.$w -side bottom -expand 1 -fill x
    }
}

proc doHelp2d {win } {
 global Parser

doHelp $win [join [list \
{

William Schelter's plotter for two dimensional graphics.

to QUIT this HELP click here.

By clicking on Zoom, the mouse now allows you to zoom \
in on a region of the plot.  Each click near a point \
magnifies the plot, keeping the center at the point \
you clicked.  Depressing the SHIFT key while clicking \
zooms in the opposite direction. 

To change the functions plotted, click on Config and \
enter new values in the entry windows, and then click on \
Replot in the main menu bar.

Holding the right mouse button down allows you to drag
(translate) the plot sideways or up and down.

Additional parameters such as the number of steps (nsteps), \
and the x and y centers and radii, may be set under the \
Config menu.

You may print to a postscript printer, or save the plot \
as a postscript file, by clicking on save.   To change \
between printing and saving see the Print Options under Config.
	




} $Parser(help)]]
}

global plot
set   plot(numberPlots) 4
proc mkExtraInfo { name args } {
    # global plot 	
    catch { destroy $name }

    toplevel $name
    wm geometry $name -10+10
 # pack $name
    set canv [assoc -canvas $args ]
    set i 0
    set w $name
    frame $w.grid
    pack $w.grid -expand yes -fill both -padx 1 -pady 1
    grid $w.grid
    grid rowconfig    $w.grid 0 -weight 1 -minsize 0
    grid columnconfig $w.grid 0 -weight 2 -minsize 0
    
    set i 0
    label $w.title -text "Extra Plotting Information" -width 50
    grid $w.title -in $w.grid -columnspan 2 -row 0 -column 0
    incr i
    label $w.labppl -text "Plot Function f(x)"
    label $w.labcol -text "plot color"
    grid $w.labppl -padx 1 -in $w.grid  -pady 1 -row $i -column 0 -sticky news
    grid $w.labcol -padx 1 -in $w.grid  -pady 1 -row $i -column 1 -sticky news
    incr i
    set k 1
    proc mkPlotEntry { w k i } {
      entry $w.plot$k -textvariable plot(fun$k)
      entry $w.color$k -textvariable plot(col$k)
      grid $w.plot$k -padx 10 -in $w.grid  -pady 1 -row $i -column 0 -sticky news
      grid $w.color$k -padx 4 -in $w.grid  -pady 1 -row $i -column 1 -sticky news
    }
    while { $k <= $plot(numberPlots) } { mkPlotEntry $w $i $k ; incr i ; incr k}
   }

proc calculatePlot { win fun  nsteps } {
  #  global xmin xmax  ymax ymin
    makeLocal $win xmin xmax  ymax ymin
    set h0 [expr {($xmax - $xmin)/double($nsteps )}]
    set x0 $xmin
    set res ""
    set limit [expr {100 * (abs($ymax)> abs($ymin) ? abs($ymax) : abs($ymin))}]
    while { $x0 < $xmax } {
	set lastx0 $x0
	#puts xmax=$xmax
	append res " " [calculatePlot1 $win $x0 $h0 $fun $limit]
	#puts res:[lrange $res [expr [llength $res] -10] end]
	if { $x0 <= $lastx0 }	{
	    # puts "x0=$x0,($lastx0)"
	    set x0 [expr {$x0 + $h0/4}]
	    #error "how is this?"
	}
    }
    # puts "plength=[llength $res]"
    return $res
}


#
 #-----------------------------------------------------------------
 #
 # calculatePlot1 --   must advance x0 in its caller
 #
 #  Results: one connected line segment as "x0 y0 x1 y1 x2 y2 .."
 #
 #  Side Effects: must advance x0 in its caller
 #
 #----------------------------------------------------------------
#
proc  calculatePlot1 { win x0 h0 fun  limit } {
     #puts "calc:$win $x0 $h0 $limit $fun"
    makeLocal $win xmax
    set ansx ""
    set ansy ""
   while { [catch { set y0 [$fun $x0] } ] && $x0 <= $xmax }  {
       set x0 [expr {$x0 + $h0}] }
    if { $x0 > $xmax } {
	# puts "catching {$fun $x0}"
	uplevel 1 set x0 $x0
	return ""
    }
    set ans "$x0 $y0"
    set delta 0
    set littleLimit [expr {$limit/50.0 }]
    set veryLittleLimit [expr {$littleLimit * 10}]
    # now have one point..
    # this is really set below for subsequent iterations.
    set count 10
    set heps [expr {$h0/pow(2,6)}]
    set h2 [expr {$h0 *2 }]
    set ii 0
    set x1 [expr {$x0 + $h0}]
    while { $x1 <= $xmax  && $ii < 5000 } {
	    # puts $x1
	    incr ii
	if { [catch { set y1 [$fun $x1] } ] } {
	    	#puts "catching1 {$fun $x1}"
	    if { $count > 0 } {
		# try a shorter step.
		set x1 [expr {($x1 -$x0)/2 + $x0}]
		incr count -1
		continue
	    } else {
		uplevel 1 set x0 [expr {$x0 + $heps}]
		return [list $ansx $ansy]
	    }
	}
	# ok have x1,y1
	# do this on change in slope!! not change in limit..

	set nslope [expr {($y1-$y0)/($x1-$x0)}]
	catch { set delta [expr {($slope * $nslope < 0 ? abs($slope-$nslope) : .1*abs($slope-$nslope))}]} 
	# catch { set delta [expr {abs($slope - ($y1-$y0)/($x1-$x0))}] }
	
	if { $count > 0 && (abs($y1 - $y0) > $h2 || $delta > $h2)  && (0 || abs($y1) < $littleLimit)
	        } {
		    #puts "too  big $y1 [expr {abs($y1-$y0)}] at $x1"
		    set x1 [expr {($x1 -$x0)/2 + $x0}]
		incr count -1
	         continue 
             } elseif { abs($y1) > $limit || abs($y1-$y0) > $limit
		    || $delta > $littleLimit } {
		incr ii
		if { $count == 0 } {
		    uplevel 1 set x0 [expr {$x0 + $heps}]
		    return [list $ansx $ansy]
		} else {
		    
		    set x1 [expr {($x1 -$x0)/2 + $x0}]
		    incr count -1
		    continue
		}
	    } else {
    	 if {   abs($y1-$y0) > $limit/4} {
	     
	    # puts "x0=$x0,x1=$x1,y0=$y0,y1=$y1"
	    uplevel 1 set x0 $x1
	    return [list $ansx $ansy]
	}

	
		# hopefully common case!!
		# puts "got it: $x1,$y1,"
	        lappend ansx $x1
	        lappend ansy $y1
		#append ans " $x1 $y1"
              	set slope [expr {($y1-$y0)/($x1-$x0)} ]
		set x0 $x1
		set y0 $y1
	set x1 [expr {$x0 + $h0}]
		set count 4
	    }
	}
	uplevel 1 set x0 $x1
	return [list $ansx $ansy]
    }

    

	

#proc setup_xf { vars form } {
#    set s [sparse $form ] 
#    proc _xf  $vars  "return \[ expr { $s } \]"
#}


#
 #-----------------------------------------------------------------
 #
 # nextColor --  get next COLOR and advance the curveNumber
 #
 #  Results: a color
 #
 #  Side Effects: the local variable for WIN called curveNumber is incremented
 #
 #----------------------------------------------------------------
#
proc nextColor { win } {
    makeLocal $win linecolors 
    if { [catch { set i [oget $win curveNumber] } ] } { set i -1 }
    set color [lindex $linecolors [expr {[incr i]%[llength $linecolors]}]]
    oset $win curveNumber $i
    return $color
}
    

proc plot2d {args } {
    #puts "args=$args"
    set win [apply mkPlot2d $args]
    replot2d $win
    return $win
}

proc replot2d {win } {
    global printOption axisGray plot2dOptions
    linkLocal $win xfundata data
    foreach v $data {
	if { "[assq [lindex $v 0] $plot2dOptions notthere]" != "notthere" } {
	    oset $win [lindex $v 0] [lindex $v 1]
	}
    }
    linkLocal $win parameters 
    makeLocal $win xfun nsteps c linecolors xaxislabel yaxislabel  autoscale sliders
    if { "$sliders" != "" && ![winfo exists $c.sliders] } {
	addSliders $win
    }
    set xfundata ""
#   puts xfun=$xfun,parameters=$parameters,[oget $win xradius],[oget $win xmax]
    foreach v [sparseListWithParams $xfun x $parameters] {
#	puts v=$v
#	proc _xf {  x  } "return \[expr { $v } \]"
	proc _xf {  x  } "expr { $v }"	
	regsub "\\$" $v "" label
	lappend xfundata [list label $label] \
	  [linsert [calculatePlot $win _xf $nsteps]  \
		0 xversusy]
    }

    # in case only functions and no y autoscale dont bother.
    if { "$data" != "" || [lsearch $autoscale y]>=0  } {
	set ranges [plot2dGetDataRange [concat $data $xfundata]]
#	puts ranges=$ranges
	foreach {v k} [eval plot2dRangesToRadius $ranges] {
	    if { [lsearch $autoscale [string index $v 1] ] >= 0 } {
		oset $win [string range $v 1 end] $k
	    }
	}
    }
    
    setUpTransforms $win 1.0
    set rtosx rtosx$win ; set rtosy rtosy$win
    $c del axes
    $c create line [$rtosx 0 ] [$rtosy -1000] [$rtosx 0] [$rtosy 1000] -fill $axisGray -tags axes
    $c create line [$rtosx -1000] [$rtosy 0] [$rtosx 1000] [$rtosy 0] -fill $axisGray -tags axes
    axisTicks $win $c

    if { "$xfun" != "" } {
	 oset $win maintitle [concat list "Plot of y = \[oget $win xfun\]" ]
    }
    $c del path
    $c del label
    oset  $win curveNumber -1
    redraw2dData $win -tags path
    $c create text    [expr {[$rtosx 0] + 10}] [expr {[$rtosy [oget $win ymax]] +20}] -text [oget $win yaxislabel] -anchor nw
    $c create text     [expr {[$rtosx [oget $win xmax]] -20}] [expr {[$rtosy 0] - 10}] -text [oget $win xaxislabel] -anchor se

    
}



#
 #-----------------------------------------------------------------
 #  Should change name to plotData since works for 3d to now..
 # plot2dData --  create WIN and plot 2d OR 3d DATA which is a list of 
 #  data sets.  Each data set must begin with xversusy or againstIndex
 #  In the first case the data set looks like:
 #       { xversusy {x1 x2 ...xn} {y1 ... yn yn+1 ... ym} }
 #  and will be plotted as m/n curves : (x1,y1) (x2,y2) .. (xn,yn)
 #  and (x1,yn+1) (x2,yn+2) ..
 #  In the againstIndex case the x values are replace by the indices
 #  0,1,2,... [length $yvalues]-1 
 #  Results: none
 #
 #  Side Effects: curves draw
 #
 #----------------------------------------------------------------
#
proc plot2dData { win data args } {
   clearLocal $win
    #puts "data=$data, [regexp plot2d $data junk ]"
    if { [regexp plot2d $data junk] } {
  # eval plot2d $args -windowname $win  [plot2dGetRanges $data] -xfun [list {}] -data [list $data]
   eval plot2d $args -windowname $win   -xfun [list {}] -data [list $data]	
    } else {
	# puts data=$data
	set com [concat \
		plot3d $args -windowname $win -zfun {{}} -data [lrange $data 1 end]]
	# puts com=$com
	eval $com
    }
 }



proc plot2dGetDataRange { data } {
    set rangex ""
    set rangey ""
     #puts "data=$data"
    set extra ""
    foreach d $data {
	#puts first=[lindex $d 0]
      if { [catch { 	
	switch -exact -- [lindex $d 0] {
	   xversusy {
	       foreach { xx yy } [lrange $d 1 end] {
		  # puts "hi xx=[llength $xx],yy=[llength $yy]"
		   if { [llength $xx] > 0 } {
		       set rangex [minMax $xx $rangex]
		       set rangey [minMax $yy $rangey]
		   }
	       }
	       #puts "rangex=$rangex,rangey=$rangey"
	   }
	   againstIndex {
	       set rangex [minMax [list 0 [llength [lindex $d 1]]] $rangex]
	       set rangey [minMax [lindex $d 1] $rangey]
	   }
	   default {
	       set vv [lindex $d 0]
	       if { [lsearch {xrange yrange   } $vv] >= 0 } {
		   set radius [expr {([lindex $d 2] -[lindex $d 1])/2.0 }]
		   set center [expr {([lindex $d 2] +[lindex $d 1])/2.0 }]
		   set var [string range $vv 0 0]
		   lappend extra -${var}radius $radius -${var}center $center
	       }
	       if { [lsearch bargraph $vv] >= 0 } {
		       set rangey [minMax 0 $rangey]
		   }


	       if { [lsearch {xradius yradius xcenter ycenter } $vv] >= 0 } {
		   lappend extra -$vv [list [lindex $d 1]]
	       }

	    }
       }
   } errmsg ] } {
       set com [list error "bad data: [string range $d 0 200].." $errmsg]
       after 1 $com
   }
 }

   list $rangex $rangey $extra
}



proc plot2dRangesToRadius  { rangex rangey extra } {
   set ranges ""
  # puts "extra=$extra"
   foreach u { x y } {
       if { "[assoc -[set u]radius $extra]" == "" } {
	   desetq "min max" [set range$u]
	   if { "$min" == "$max" } {
	       set min [expr {$min - .5}]
	       set max [expr {$max + .5}]
	   }
	   #puts "$u has $min,$max"
	   # use 1.7 to get a bit bigger radius than really necessary.
	   if { "$max" != "" } {
	   
	       lappend extra -[set u]radius [expr {($max-$min)/1.7}] \
		       -[set u]center [expr {($max+$min)/2.0}]
	   }
   }
 }
 # puts "extra=$extra"
 return $extra
}
   

proc redraw2dData { win  args } {
   makeLocal $win c linecolors data xfundata errorbar linewidth
   set tags [assoc -tags $args {} ]
   set rtosx rtosx$win ; set rtosy rtosy$win  
   set i -1
   set label _default
   append data " " $xfundata
#    set linewidth 2.4
    
   #puts "data=$data"
   foreach d $data {
       set type [lindex $d 0]
       switch  $type {
	   xversusy {
	        #puts "starting .. [oget $win curveNumber]"
	       set curvenumber [oget $win curveNumber]
	       # the data can be multiple lists and each list
	       # will not be line connected to previous
	       foreach {xvalues yvalues} [lrange $d 1 end] {
		   # puts "xvalues=$xvalues"
		   #puts "here:$curvenumber,[oget $win curveNumber]"
		   oset $win curveNumber $curvenumber
		   set n [expr {[llength $xvalues] -1}]
		   while { [llength $yvalues] > 0 } {
		       set ans ""
		       set color [nextColor $win]
		       catch { set color [oget $win color] }
		       
		       if { [info exists didLabel([oget $win curveNumber])] } {
			   set label "" } else { set didLabel([oget $win curveNumber]) 1
		       }
		       set errorbar [oget $win errorbar]
		       # puts "errorbar=$errorbar"
		       if { $errorbar != 0 } {
			   set j 0
			  # puts "xvalues=$xvalues,yvalues=$yvalues"
     		       for { set i 0 } { $i <= $n } {incr i} {
			       set x [lindex $xvalues $i]
			       set y1 [lindex $yvalues [expr {$i * 2}]]
			       set y2 [lindex $yvalues [expr { $i * 2 +1}]]
			   if { 1 } {
			      # puts "x=$x,y1=$y1,y2=$y2"
			       set xx [$rtosx $x]
			       set y1 [$rtosy $y1]
			       set y2 [$rtosy $y2]
			       $c create line [expr {$xx - $errorbar}] $y1 [expr {$xx +$errorbar}] $y1 $xx $y1 $xx $y2 [expr {$xx -$errorbar}] $y2 [expr {$xx + $errorbar}] $y2  -tags [list [concat $tags line[oget $win curveNumber]]]  -fill $color
			   }
		       }
		       
			       
		       set yvalues [lrange $yvalues [llength $xvalues] end]
		       } else { 

		       foreach x $xvalues y [lrange $yvalues 0 $n] {
			   append ans "[$rtosx $x] [$rtosy $y] "
		       
		       }

		       drawPlot $win [list $ans] -tags [list [concat $tags line[oget $win curveNumber]]]  -fill $color -label $label
		       }
		       set label _default

		       set yvalues [lrange $yvalues [llength $xvalues] end]
		   }

	     } }
	   againstIndex {
               set color [nextColor $win]
	       set ind 0
	       set ans ""
	       foreach y [lindex $d 1] {
		   append ans "[$rtosx $ind] [$rtosy $y] "
		   incr ind
	       }
	       
	       drawPlot $win [list $ans] -tags \
		       [list [concat $tags line[oget $win curveNumber]]] \
		       -fill $color -width $linewidth -label $label
	       set label _default

#	       eval $c create line $ans -tags \
#		        [list [concat $tags line[oget $win curveNumber]]] \
#		       -fill $color -width .2
	   }
	   label {
	       set label [lindex $d 1]
	   }
	   default {

	       # puts "$type,[lindex $d 1]"
	       if { [lsearch { xfun color plotpoints linecolors pointsize nolines bargraph errorbar maintitle linewidth
	       labelposition
	       xaxislabel yaxislabel } $type] >= 0 } {
		   # puts "setting oset $win $type [lindex $d 1]"
		   oset $win $type [lindex $d 1]
	       } elseif { "$type" == "text" } {
		   desetq "x y text" [lrange $d 1 end]
		   $c create text [$rtosx $x] [$rtosy $y] -anchor nw -text $text -tags "text all" -font times-roman
	       }

	   }

       }
   }

}

proc plot2dDrawLabel { win label color } {
    makeLocal $win c labelposition
    #puts "$win $label $color"
    if { "$label" == ""} {return }
    set bb [$c bbox label]
    desetq "a0 b0" $labelposition
    if { "$bb" == "" } { set bb "$a0 $b0 $a0 $b0" }
    desetq "x0 y0 x1 y1" $bb
    set leng  15
    set last [$c create text [expr {$a0 +$leng +4}] \
	    [expr {2 + $y1}] \
	    -anchor nw       -text "$label" -tags label]
    desetq "ux0 uy0 ux1 uy1" [$c bbox $last]
    $c create line $a0 [expr {($uy0+$uy1) /2}] [expr {$a0 +$leng}] [expr {($uy0+$uy1) /2}]   -tags "label" -fill $color
}
	

proc RealtoScreen { win listPts } {
    set rtosx rtosx$win ; set rtosy rtosy$win  
    set ans ""
    if { [llength [lindex $listPts  0]] != 1 } {
	foreach v $listPts {
	    append ans " {"
	    append ans [RealtoScreen $win $v]
	    append ans "}"
	}
    }    else {
	foreach {x y } $listPts {
	    append ans " [$rtosx $x] [$rtosy $y]"
	}
    }
    return $ans
}

proc drawPlot {win listpts args } {
    makeLocal $win  c nolines plotpoints  pointsize bargraph linewidth
#    set linewidth 2.4
    # puts ll:[llength $listpts]
    set tags [assoc -tags $args ""]
    if { [lsearch $tags path] < 0 } {lappend tags path}
    set fill [assoc -fill $args black]
    set label [assoc -label $args ""]
    if { "$label" == "_default" } {
	set label line[oget $win curveNumber]
    }

    catch { set fill [oget $win color] }
    
    if { $nolines == 1 && $plotpoints == 0 && $bargraph == 0} {
	set plotpoints 1
    }

    catch { 
    foreach pts $listpts {
	if { $bargraph } {
	    set rtosy rtosy$win
	    set rtosx rtosx$win
	    set width [expr {abs([$rtosx $bargraph] - [$rtosx 0])}]
	    set w2 [expr {$width/2.0}]
	    # puts "width=$width,w2=$w2"
	    set ry0 [$rtosy 0]
	    foreach { x y } $pts {
		$c create rectangle [expr {$x-$w2}] $y  [expr {$x+$w2}] \
			$ry0 -tags $tags -fill $fill }
	    } else {
		if { $plotpoints } {
		    set im [getPoint $pointsize $fill]
		    
		    # there is no eval, so we need this.
		    if { "$im" != "" } {
			foreach { x y } $pts {
			$c create image $x $y -image $im -anchor center \
				-tags "$tags point"
			}
		    } else {
		    foreach { x y } $pts {
			$c create oval [expr {$x -$pointsize}] \
				[expr {$y -$pointsize}] [expr {$x +$pointsize}] \
				[expr {$y +$pointsize}] -tags $tags \
				-fill $fill -outline {}
			
		    }
		}
		}
		
		if { $nolines == 0 } {
		    set n [llength $pts]
		    set i 0
		    set res "$win create line "
		    #puts npts:[llength $pts]
		    if { $n >= 6 } {
			eval $c create line $pts  	-tags [list $tags] -width $linewidth -fill $fill
		    }
		}
	    }
	    
	}
    }
    plot2dDrawLabel $win $label $fill
}

    

proc drawPointsForPrint { c } {
    global ws_openMath
    foreach v [$c find withtag point] {
	set tags [ldelete point [$c gettags $v]]
	desetq "x y" [$c coords $v]
	
	
	desetq "pointsize fill" $ws_openMath(pointimage,[$c itemcget $v -image])
	catch { 
	    $c create oval [expr {$x -$pointsize}] \
		    [expr {$y -$pointsize}] [expr {$x +$pointsize}] \
		    [expr {$y +$pointsize}] -tags $tags \
				-fill $fill -outline {}
         $c delete $v			
	}


    }

}

array set ws_openMath { bitmap,disc4 {#define disc4_width 4
#define disc4_height 4
static unsigned char disc4_bits[] = {
    0x06, 0x0f, 0x0f, 0x06};}
    bitmap,disc6 {#define disc_width 6
#define disc_height 6
static unsigned char disc_bits[] = {
    0xde, 0xff, 0xff, 0xff, 0xff, 0xde};}
}
 

proc getPoint { size color } {
    global ws_openMath
    set im ""
    if { ![catch { set im $ws_openMath(pointimage,$size,$color) }] } {
	return $im
    }
    catch { set data $ws_openMath(bitmap,disc[expr {$size * 2}]) 
    set im [image create bitmap -data $data -foreground $color]
    set ws_openMath(pointimage,$size,$color) $im
    set ws_openMath(pointimage,$im) "$size $color"
   }
    return $im
}
    



proc sliderCommandPlot2d { win var val } {
    linkLocal $win recompute

    updateParameters $win $var $val
    set com "recomputePlot2d $win"
    # allow for fast move of slider...    
    #mike FIXME: this is a wrong use of after cancel
    after cancel $com
    after 10 $com
}

proc recomputePlot2d { win } {
       replot2d $win
}


## endsource plot2d.tcl
## source plot3d.tcl

###### plot3d.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

set plot3dOptions { 
    {xradius 1 "Width in x direction of the x values" }
    {yradius 1 "Height in y direction of the y values"}

    {width 500 "Width of canvas in pixels"}
    {height 500 "Height of canvas in pixels" }
    {xcenter 0.0 {(xcenter,ycenter) is the origin of the window}}
    {ycenter 0.0 "see xcenter"}
    {zcenter 0.0 "see xcenter"}
    {bbox "" "xmin ymin xmax ymax zmin zmax overrides the -xcenter etc"}
    {zradius auto " Height in z direction of the z values"}
    {az 60 "azimuth angle" }
    {el 30 "elevantion angle" }
    
    {thetax 10.0 "ignored is obsolete: use az and el"}
    {thetay 20.0 "ignored is obsolete: use az and el"}
    {thetaz 30.0 "ignored is obsolete: use az and el"}

    {flatten 0 "Flatten surface when zradius exceeded" }
    {zfun "" "a function of z to plot eg: x^2-y^2"}
    {parameters "" "List of parameters and values eg k=3,l=7"}
    {sliders "" "List of parameters ranges k=3:5,u"}
    {data  "" "a data set of type { variable_grid xvec yvec zmatrix}
    or {matrix_mesh xmat ymat zmat} or {grid {xmin xmax} {ymin ymax} zmatrix}"}
    {nsteps "10 10" "steps in x and y direction"}
    {rotationcenter "" "Origin about which rotation will be done"}
    {zoomfactor "1.6 1.6" "Factor to zoom the x and y axis when zooming.  Zoom out will be reciprocal" }
    {screenwindow "20 20 700 700" "Part of canvas on screen"}
    {windowname ".plot3d" "window name"}
}
    

## source matrix.tcl

###### matrix.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

# In this file a matrix is represented by a list of M*N entries together
# with an integer N giving the number of columns: {1 0 0 1} 2  would give
# the two by two identity

proc comment {args } { }
  set mee " } \] \[ expr { "  

proc mkMultLeftExpr { mat n prefix { constant "" } } {
#create a function body that does MAT (prefix1,prefix2,..) + constant 
    global mee
    set all ""
    
    set vars ""
    for { set i 0} { $i < $n} {incr i} { append vars " $prefix$i" }
    set j 0
    set k 0
    
    foreach v $mat {
	if { $j == 0 } {
	    set ro ""
	    # append ans ""
	    set op ""
	}
        append ro " $op $v*\$$prefix$j"
	set op "+"
	if { $j == [expr {$n -1}] } {
	     append ans " "
	    if { "[lindex $constant $k]" != "" } {
		append ro " + [lindex $constant $k] "
	    }
	    incr k
	    append ans [concat \[ expr [list $ro] \]]
	    set j -1
	}
	incr j
    }
    # puts [list $vars $ans]
     return [list $vars $ans]
}

proc mkMultLeftFun { mat n name { constant ""} } {
    set expr [mkMultLeftExpr $mat $n _a $constant]
    set bod1 [string trim [lindex $expr 1] " "]
#    set bod "return \"$bod1\""
    set bod [concat list [lindex $expr 1]]
    proc $name [lindex $expr 0] $bod
}

proc rotationMatrix { th ph } {
   return [list \
	   [expr {cos($ph)*cos($th)}] [expr {- cos($ph)*sin($th)}] [expr {sin($ph)}] \
	   [expr {sin($th)}] [expr {cos($th)}] 0.0 \
	   [expr {- sin($ph)*cos($th)}] [expr {sin($ph)*sin($th)}] [expr {cos($ph)}]]
}

# proc rotationMatrix { thx thy thz } {
#   return [list  \
#  [expr { cos($thy)*cos($thz)} ]  \
#  [expr { cos($thy)*sin($thz)} ]  \
#  [expr { sin($thy)} ]  \
#  [expr { sin($thx)*sin($thy)*cos($thz)-cos($thx)*sin($thz)} ]  \
#  [expr { sin($thx)*sin($thy)*sin($thz)+cos($thx)*cos($thz)} ]  \
#  [expr { -sin($thx)*cos($thy)} ]  \
#  [expr { -sin($thx)*sin($thz)-cos($thx)*sin($thy)*cos($thz)} ]  \
#  [expr { -cos($thx)*sin($thy)*sin($thz)+sin($thx)*cos($thz)} ]  \
#  [expr { cos($thx)*cos($thy)} ] ]
# }

proc rotationMatrix { thx thy thz } {
    return \
 [list  \
 [expr { cos($thy)*cos($thz) } ] \
 [expr { cos($thy)*sin($thz) } ] \
 [expr { sin($thy) } ] \
 [expr { sin($thx)*sin($thy)*cos($thz)-cos($thx)*sin($thz) } ] \
 [expr { sin($thx)*sin($thy)*sin($thz)+cos($thx)*cos($thz) } ] \
 [expr { -sin($thx)*cos($thy) } ] \
 [expr { -sin($thx)*sin($thz)-cos($thx)*sin($thy)*cos($thz) } ] \
 [expr { sin($thx)*cos($thz)-cos($thx)*sin($thy)*sin($thz) } ] \
 [expr { cos($thx)*cos($thy) } ] ]
}

# cross [a,b,c] [d,e,f] == [B*F-C*E,C*D-A*F,A*E-B*D]
# cross_product([a,b,c],[d,e,f]):=[B*F-C*E,C*D-A*F,A*E-B*D]
# cross_product(u,v):=sublis([a=u[1],b=u[2],c=u[3],d=v[1],e=v[2],f=v[3]],[B*F-C*E,C*D-A*F,A*E-B*D]);
# the rotation by azimuth th, and elevation ph
# MATRIX([COS(TH),SIN(TH),0],[-COS(PH)*SIN(TH),COS(PH)*COS(TH),SIN(PH)],
#	    [SIN(PH)*SIN(TH),-SIN(PH)*COS(TH),COS(PH)]);

proc rotationMatrix { th ph {ignore {} } } {
    return \
[list \
[	    expr {cos($th)   } ]\
[expr {sin($th)   } ]\
0 \
[expr {-cos($ph)*sin($th)   } ]\
[expr {cos($ph)*cos($th)   } ]\
[expr {sin($ph)   } ]\
[expr {sin($ph)*sin($th)   } ]\
[expr {-sin($ph)*cos($th)   } ]\
[expr {cos($ph)   } ]]
}

proc setMatFromList {name lis n} {
    set i 1
    set j 1
    foreach v $lis {
	uplevel 1 set [set name]($i,$j) $v
	if { $j == $n } {set j 1; incr i} else { incr j}
}   }

proc matRef { mat cols i j } { [lindex $mat [expr {$i*$cols + $j}]] }
proc matTranspose { mat cols } {
    set j 0
    set m [expr {[llength $mat ] / $cols}]
    while { $j < $cols} {
	set i 0
	while { $i < $m } {
	    append ans " [lindex $mat [expr {$i*$cols + $j}]]"
	    incr i
	}
	incr j
    }
    return $ans
}


proc matMul { mat1 cols1 mat2 cols2 } {
    mkMultLeftFun $mat1 $cols1 __tem
    set tr [matTranspose $mat2 $cols2]
    set rows1 [expr {[llength $mat1] / $cols1}]
    #puts "tr=$tr"
    set upto [llength $tr]
    set j 0
    set ans ""
    set i 0
    while { $j < $cols2  } {
	append ans " [eval __tem [lrange $tr $i [expr {$i+$cols1 -1}]]]"
	incr i $cols1
	incr j
    }
 #   return $ans
   # puts "matTranspose $ans $rows1"
    return [matTranspose $ans $rows1]
}



proc invMat3 { mat } {
    setMatFromList xx $mat 3
    set det [expr { double($xx(1,1))*($xx(2,2)*$xx(3,3)-$xx(2,3)*$xx(3,2))-$xx(1,2)* \
	    ($xx(2,1)*$xx(3,3)-$xx(2,3)*$xx(3,1))+$xx(1,3)*($xx(2,1)*$xx(3,2)\
	    -$xx(2,2)*$xx(3,1)) }]
    
    return [list   [expr { ($xx(2,2)*$xx(3,3)-$xx(2,3)*$xx(3,2))/$det}] \
	    [expr { ($xx(1,3)*$xx(3,2)-$xx(1,2)*$xx(3,3))/$det}] \
	    [expr { ($xx(1,2)*$xx(2,3)-$xx(1,3)*$xx(2,2))/$det}] \
	    \
	    [expr { ($xx(2,3)*$xx(3,1)-$xx(2,1)*$xx(3,3))/$det}] \
	    [expr { ($xx(1,1)*$xx(3,3)-$xx(1,3)*$xx(3,1))/$det}] \
	    [expr { ($xx(1,3)*$xx(2,1)-$xx(1,1)*$xx(2,3))/$det}] \
	    \
	    [expr { ($xx(2,1)*$xx(3,2)-$xx(2,2)*$xx(3,1))/$det}] \
	    [expr { ($xx(1,2)*$xx(3,1)-$xx(1,1)*$xx(3,2))/$det}] \
	    [expr { ($xx(1,1)*$xx(2,2)-$xx(1,2)*$xx(2,1))/$det}]]
}


proc vectorOp { a op b} {
    set i [llength $a]
    set k 0
    set ans [expr [list [lindex $a 0]  $op [lindex $b 0]]]
    while { [incr k] < $i } {
	lappend ans  [expr  [list [lindex $a $k] $op [lindex $b $k]]]
    }
    return $ans
}
## endsource matrix.tcl

proc transformPoints { pts fun } {
  set ans ""
  foreach { x y z } $pts {
      append ans " "
      append ans [$fun $x $y $z]
  }
  return $ans
}

proc calculatePlot3d {win fun  nx ny } {
    global plot3dMeshes$win
    set meshes  plot3dMeshes$win
    makeLocal $win xradius xmin yradius ymin zradius zcenter flatten
    
    set stepx [expr { 2*$xradius / double($nx)}]
    set stepy [expr { 2*$yradius / double($ny)} ]
    set i 0
    set j 0
    set zmax -1000000000
    set zmin 1000000000
    # check if zradius is a number
    set dotruncate [expr ![catch {expr {$zradius + 1} }]]
    if { $dotruncate } {
	if { $flatten } { set dotruncate 0 }
	set zzmax [expr {$zcenter + $zradius}]
	set zzmin [expr {$zcenter - $zradius}]
	#puts "zzmax=$zzmax,$zzmin"
   } else { set flatten 0 }

    catch { unset  $meshes }
    set k 0
    for {set i 0} { $i <= $nx } { incr i} {
	set x [expr { $xmin + $i * $stepx }]
	for {set j 0} { $j <= $ny } { incr j} {
	    set y [expr { $ymin + $j *$stepy }]
	   if { [catch {  set z [$fun $x $y] }] } {
	       set z nam
	   } elseif { $dotruncate  &&  ($z > $zzmax || $z < $zzmin) } {
	       set z nam

	    } else {
		if { $flatten } {
		    if { $z > $zzmax } { set z $zzmax } elseif {
			$z < $zzmin } { set z $zzmin }}
			
	       if { $z < $zmin }  { set zmin $z } elseif {
		$z > $zmax } { set zmax $z }
		if { $j != $ny && $i != $nx } {
		    set [set meshes]($k) \
                      "$k [expr { $k+3 }] [expr { $k+3+($ny+1)*3 }] \
		      [expr { $k+($ny+1)*3 }]"} else {
		  # set plot3dMeshes($k) ""
	      }
	  }
	      incr k 3
	    append ans " $x $y $z"
	}
    }
    oset $win zmin $zmin
    oset $win zmax $zmax
    oset $win points $ans
    oset $win nx $nx
    oset $win ny $ny
    oset $win colorfun plot3dcolorFun
    addAxes $win
    setupPlot3dColors $win
}

proc calculatePlot3data {win fun  nx ny } {
# calculate the 3d data from function:
    makeLocal $win xradius xmin xmax ymax yradius ymin zradius zcenter flatten
    
    set rowx [linspace $xmin $xmax $nx]
    set rowy [linspace $ymin $ymax $ny]
    foreach  y $rowy {
	set row ""
	foreach x $rowx {
	    if { [catch {  set z [$fun $x $y] }] } {
		set z nam
	    }
	    lappend row $z
	}
	lappend matrix $row
    }
    global silly
    set silly [list variable_grid $rowx $rowy $matrix ]
    return [list variable_grid $rowx $rowy $matrix ]
    
}

proc addAxes { win } {
    #global plot3dPoints plot3dMeshes xradius yradius xcenter ycenter
    global [oarray $win] plot3dMeshes$win
    linkLocal $win lmesh
    makeLocal $win   xradius yradius xcenter ycenter  points zmax zcenter zmin
    set meshes plot3dMeshes$win
    set ll [llength $points]

    # puts "oset $win  axisstart  $ll"
    oset $win  axisstart  $ll
    set nx2 5
    set ny2 5
    set xstep [expr { 1.2 * $xradius/double($nx2) }]
    set ystep [expr { 1.2 * $yradius/double($ny2) }]
    set nz2 $ny2

    set ans " "
    set x0 $xcenter
    set y0 $ycenter
    set z0 $zcenter

    set k $ll
    for { set i 0 } { $i < $nx2 } { incr i } {
	append ans "[expr {$x0 +$i * $xstep}] $y0 $z0 "
	lappend lmesh [list $k [incr k 3]]
	#set [set meshes]($k) "$k [incr k 3]"
    }
    append ans "[expr {$x0 +$nx2 * $xstep}] $y0 $z0 "
    incr k 3
    # set plot3dMeshes($k) ""

    for { set i 0 } { $i < $ny2 } { incr i } {
	append ans "$x0 [expr {$y0 +$i * $ystep}] $z0 "
	lappend lmesh [list $k [incr k 3]]
	#set [set meshes]($k) "$k [incr k 3]"
    }
    append ans "$x0 [expr {$y0 +$ny2 * $ystep}] $z0 "
    incr k 3
    # set $meshes($k) ""

    set zstep [expr {1.2 * $zmax/double($nz2)}]
    if { $zstep < $ystep } { set zstep $ystep }
    
    for { set i 0 } { $i < $ny2 } { incr i } {
	append ans "$x0 $y0 [expr {$z0 +$i * $zstep}] "
	# puts "set [set meshes]($k) \"$k [incr k 3]\""
	lappend lmesh [list $k [incr k 3]]
	# set [set meshes]($k) "$k [incr k 3]"
    }
    append ans "$x0 $y0 [expr {$z0 +$nz2 * $zstep}] "
    incr k 3
    # puts "ans=$ans"
    append [oloc $win points] $ans
    
   # set $meshes($k) ""

}

proc addBbox { win } {
    global plot3dMeshes$win
    makeLocal $win xmin xmax ymin ymax zmin zmax  cmap
    linkLocal $win points lmesh 
    set ll [llength $points]
     append points " $xmin $ymin $zmin \
	    $xmax $ymin $zmin \
            $xmin $ymax $zmin \
            $xmax $ymax $zmin \
            $xmin $ymin $zmax \
	    $xmax $ymin $zmax \
            $xmin $ymax $zmax \
            $xmax $ymax $zmax "
    foreach  { a b } { 0 1 0 2 2 3 3 1
                       4 5 4 6 6 7 7 5
    0 4 1 5 2 6 3 7  }  {
	set k [expr {$a*3 + $ll}]
	set l [expr {$b*3 + $ll}]
	# set plot3dMeshes${win}($k) [list $k $l]
	lappend lmesh [list $k $l]
    }
    lappend lmesh [list $ll]
    oset $win $cmap,[list $ll [expr {$ll + 3}]] red
    oset $win $cmap,[list $ll [expr {$ll + 6}]] blue
    oset $win $cmap,[list $ll [expr {$ll + 12}]] green
    
    oset $win special($ll) "drawOval [oget $win c] 3 -fill red -tags axis"
}

proc drawOval { c radius args } {
    set ll [llength $args]
    set x [lindex $args [expr {$ll -2}]]
    set y [lindex $args [expr {$ll -1}]]
    set rest [lrange $args 0 [expr {$ll -3}]]
    set com [concat $c create oval [expr {$x - $radius}]  [expr {$y - $radius}] [expr {$x + $radius}]  [expr {$y + $radius}] $rest]
    eval $com
}
    

proc plot3dcolorFun {win z } {
    makeLocal $win zmin zmax
    set ncolors 180
    set tem [expr {(180/$ncolors)*round(($z - $zmin)*$ncolors/($zmax - $zmin+.001))}]
    #puts "tem=$tem,z=[format %3g $z],[format "#%.2x%.2x%.2x" 50 50 $tem]"
    return [format "#%.2x%.2x%.2x" [expr {180 -$tem}] [expr {240 - $tem}] $tem]
}

proc setupPlot3dColors { win } {
    upvar #0 [oarray $win] wvar
    # the default prefix for cmap
    set wvar(cmap) c1
    set k 0
    makeLocal $win colorfun points
    foreach { x y z } $points {
	catch { set wvar(c1,$k) [$colorfun $win $z] }
	incr k 3 
    }
}

proc calculateRotated { win } {
    set pideg [expr {3.14159/180.0}]
    linkLocal $win scale
    makeLocal $win az el rotationcenter xradius zradius yradius
    set rotmatrix [rotationMatrix [expr {$az * $pideg }] \
	    [expr {$el * $pideg }] \
      ]

    # shrink by .2 on z axis
    # set fac [expr  {[vectorlength $xradius $yradius] / (sqrt(2) * $zradius)}]

    set rotmatrix [ matMul  $rotmatrix 3 $scale 3 ]
    set tem [matMul $scale 3 $rotationcenter 1]
    
    mkMultLeftFun  $rotmatrix 3 _rot$win
    set rot _rot$win
    set ans ""
    # puts "points=[oget $win points]"
    if { "$rotationcenter" != "" } {
	#puts "rotationcenter = $rotationcenter"
	set constant [vectorOp $tem - [eval $rot $rotationcenter]]
	mkMultLeftFun  $rotmatrix 3 _rot$win $constant
    }
    #puts "win $win"
    foreach { x y z } [oget $win points] {
	if { [catch { append ans " " [$rot $x $y $z] } ] } {
	    append ans "  nam nam nam " }
	}
    oset $win rotatefun $rot
    oset $win rotated $ans 
}

proc getOrderedMeshIndices { win } {
 #   global  plot3dMeshes$win
 #    set meshes plot3dMeshes$win
    linkLocal $win lmesh
    # puts "array names $meshes =[array names $meshes ]"
    # get the list offset by 2, so the lindex indices grab the Z coordinate.
    # without having to add 2.
    set pts2 [lrange [oget $win rotated] 2 end]
    set i 0
    foreach tem $lmesh {
        set k [llength $tem]
       if { [catch {
        if {  $k == 4 } {
	    set z [expr { ([lindex $pts2 [lindex $tem 0]] \
		    +[lindex $pts2 [lindex $tem 1]] \
		    + [lindex $pts2 [lindex $tem 2]] \
		    + [lindex $pts2 [lindex $tem 3]])/4.0 }]
	} elseif { $k == 2 } {
            set z [expr { ([lindex $pts2 [lindex $tem 0]] \
		    +[lindex $pts2 [lindex $tem 1]])/2.0 }]
	} else {
	    set z 0
            foreach w $tem {
		set z [expr {$z + [lindex $pts2 $w] }  ]
		
	    }	
	    set z [expr { $z/double($k)}]
        }
	lappend ans [list $z $i]
	# append pp($z) "$i "
	incr i
	
         } ]} {
	    set lmesh [lreplace $lmesh $i $i]
	 }
    }
    set ttem [lsort -real -index 0 $ans]
    set ans {}
    foreach v $ttem {
	lappend ans [lindex $v 1]
    }
    oset $win meshes $ans
    return
}


proc setUpTransforms3d { win } {
    global screenwindow
    #set scr $screenwindow
    # setUpTransforms $win .7
    # set screenwindow $scr
    linkLocal $win scale
        makeLocal $win xcenter ycenter xradius yradius c zmin zmax xmin xmax ymin ymax zradius
    #dshow xcenter ycenter xradius yradius c zmin zmax xmin xmax ymin ymax zradius
    set fac .5

    set delx [$c cget -width]
    set dely [$c cget -height]
    set f1 [expr {(1 - $fac)/2.0}]

    set scale [list [expr {1.5/($xradius)}] 0 0 0 [expr {1.5/($yradius)}] 0 0 0 [expr {1.5/($zradius)}] ]

    set x1 [expr {$f1 *$delx}]
    set y1 [expr {$f1 *$dely}]
    set x2 [expr {$x1 + $fac*$delx}]
    set y2 [expr {$y1 + $fac*$dely}]
    # set xmin [expr {($xcenter - $xradius) * 1.5/ ($xradius)}]
    # set ymin [expr {($ycenter - $yradius) * 1.5/ ($yradius)}]
    # set xmax [expr {($xcenter + $xradius) * 1.5/ ($xradius)}]
    # set ymax [expr {($ycenter + $yradius) * 1.5/ ($yradius)}]
    #puts "RANGES=$xmin,$xmax $ymin,$ymax $zmin,$zmax"
    desetq "xmin ymin" [matMul $scale 3 "$xmin $ymin 0" 1]
    desetq "xmax ymax" [matMul $scale 3 "$xmax $ymax 0" 1]
    #puts "RANGES=$xmin,$xmax $ymin,$ymax $zmin,$zmax"
    # set transform [makeTransform "$xmin $ymin $x1 $y2" "$xmin $ymax $x1 $y1 " "$xmax $ymin $x2 $y2"]
   # desetq "xmin xmax ymin ymax" "-2 2 -2 2"

    set transform [makeTransform "$xmin $ymin $x1 $y2" "$xmin $ymax $x1 $y1 " "$xmax $ymin $x2 $y2"]
    oset $win transform $transform
    oset $win transform0 $transform
    
    getXtransYtrans $transform rtosx$win rtosy$win
    getXtransYtrans [inverseTransform $transform] storx$win story$win 

}

# 

proc plot3d { args } {
    global  plot3dOptions
    set win [assoc -windowname $args]
    if { "$win" == "" } {
	set win [getOptionDefault windowname $plot3dOptions] }
    clearLocal $win
    apply mkPlot3d  $win $args
#    bind $win <Configure> {}	
    replot3d $win
}

proc replot3d { win } {
    global   printOption plot2dOptions
    makeLocal $win nsteps zfun data c
    linkLocal $win parameters sliders
    
    oset $win maintitle    "concat \"Plot of z = [oget $win zfun]\""
    if { [llength $nsteps] == 1 }    {
	oset $win nsteps \
		[set nsteps  [list [lindex $nsteps 0] [lindex $nsteps 0]]]
    }
    foreach v $data {
	if { "[assq [lindex $v 0] $plot2dOptions notthere]" != "notthere" } {
	    oset $win [lindex $v 0] [lindex $v 1]
	}
    }
    if { "$sliders" != "" && ![winfo exists $c.sliders] } {
	addSliders $win
    }

    if { "$zfun" != "" } {
	proc _xf {  x  y } "return \[expr { [sparseWithParams $zfun {x y} $parameters ] } \]"
	addOnePlot3d $win [calculatePlot3data $win _xf  [lindex $nsteps 0] [lindex $nsteps 1]]
	# calculatePlot3d $win _xf [lindex $nsteps 0] [lindex $nsteps 1]
    }

    if { "$data" != "" } {
	if { 0 } {
        puts "here"
	set ranges [ plot3dGetDataRange [list $data]]
	linkLocal $win zmin zmax
	desetq "zmin zmax" [lindex $ranges 2]
	puts "ranges=$ranges"
	set some [plot2dRangesToRadius [lindex $ranges 0] [lindex $ranges 1] ""]
	puts "and now"
	foreach {v k} $some {
	    puts "oset $win [string range $v 1 end] $k"
	    oset $win [string range $v 1 end] $k
	}
        }
	
	addOnePlot3d $win $data
    }


    setUpTransforms3d $win

    oset $win colorfun plot3dcolorFun
#    addAxes $win
    oset $win cmap c1
    setupPlot3dColors $win
    addBbox $win
    # grab the bbox just as itself
    global ws_openMath
    linkLocal $win lmesh
    if { [llength $lmesh] >   100 * $ws_openMath(speed)  } {
	# if we judge that rotation would be too slow, we make a secondary list
	# of meshes (random) including the bbox, and display those. 
	linkLocal $win  points lmeshBbox pointsBbox
	set n [llength $lmesh]
	set lmeshBbox [lrange $lmesh [expr {$n -13}] end]
	set i 0 ;
	while { [incr i ] < ( 35*$ws_openMath(speed)) } {
	    set j [expr {round(floor(rand()*($n-13))) }]
	    if { ![info exists temm($j)] } {
		lappend lmeshBbox [lindex $lmesh $j ]
		set temm(j) 1
	    }
	}
	resetPtsForLmesh $win
    }
    oset $win lastAnglesPlotted ""
    setView $win ignore
} 

proc setView { win ignore } {
    global timer
    foreach v [after info] {
	#puts "$v=<[after info $v]>"
	if {[lindex [after info $v] 0] == "setView1" } {
	    after cancel $v
	}
    }
    after 2 setView1 $win
}

proc setView1 { win  } {
    linkLocal $win lastAnglesPlotted points
    set new [list [oget  $win az] [oget  $win el] ]
    if { "$new" != "$lastAnglesPlotted" } {
   	makeLocal $win c
	calculateRotated $win
	getOrderedMeshIndices $win
	drawMeshes $win $c
	oset $win lastAnglesPlotted $new
    }
}

proc setQuick { win on } {
  linkLocal $win  lmesh  points savedData cmap 	lmeshBbox pointsBbox
    if { $on } {
	if { ![info exists savedData] && [info exists lmeshBbox] } {
	    set savedData [list $lmesh $points $cmap]
	    set lmesh $lmeshBbox
	    set points $pointsBbox
	    set cmap c2
	}
    } else {
	if { [info exists savedData] } {
	    desetq "lmesh points cmap" $savedData
	    unset savedData
	    oset $win lastAnglesPlotted ""
}   }   }


# reduce the set of pointsBbox to include only those needed by lmeshBbox
proc resetPtsForLmesh { win } {
    upvar 1 lmeshBbox lmeshBbox
    upvar 1 pointsBbox pointsBbox
    upvar 1 points points
    upvar #0 [oarray $win] wvar
    set k 0
    foreach v $lmeshBbox {
	if { [llength $v] == 1 } {
	    lappend nmesh $v
	} else {
	    set s ""
	    foreach w $v {
		if { [info exists tem($w)] } {
		    lappend s $tem($w)
		} else {
		    set tem($w) $k
		    lappend s $k
		    lappend pointsBbox \
			    [lindex $points $w] \
			    [lindex $points [expr {$w +1}]] \
			    [lindex $points [expr {$w +2}]]
		    catch {set wvar(c2,$k) $wvar(c1,$w)}
		    incr k 3
		
		}
			
	    }
	    lappend nmesh $s
	    if { [info exists wvar(c1,$v)] } {
		set wvar(c2,$s) $wvar(c1,$v)
	    }
	}
    }
    set lmeshBbox  $nmesh
}

proc drawMeshes {win canv} {
    # $canv delete poly
    # only delete afterwards, to avoid relinquishing the colors
    $canv addtag oldpoly withtag poly 
    $canv delete axis
    makeLocal $win lmesh rotated cmap
    upvar #0 [oarray $win] ar
    proc _xf { x} [info body rtosx$win]
    proc _yf { y} [info body rtosy$win]
    foreach { x y z} $rotated { lappend rotatedxy [_xf $x] [_yf $y] 0 }

    foreach k [oget $win meshes] {
	#puts "drawOneMesh $win $canv $k"
	#puts "drawOneMesh $win $canv $k"
	set mesh [lindex $lmesh $k]
	set col gray70
	catch { set col $ar($cmap,[lindex $mesh 0]) }
	drawOneMesh $win $canv $k $mesh $col
    }
    $canv delete oldpoly
}


#
 #-----------------------------------------------------------------
 # plot3dMeshes  --  given K an index in plot3dPoints(points) 
 # if this is the index of a lower grid corner, return the other points.
 # k takes values 0,3,6,9,... the values returned all have a 3 factor,  
 # and so are true lindex indices into the list of points.
 # returns {} if this is not a mesh point.
 #  Results:
 #
 #  Side Effects: none... NOTE we should maybe cash this in an array. 
 #
 #----------------------------------------------------------------
#

proc drawOneMesh { win  canv k mesh color } {
    #k=i*(ny+1)+j
    # k,k+1,k+1+nyp,k+nyp
    upvar 1 rotatedxy ptsxy
    set n [llength $mesh]

    foreach kk $mesh {
	lappend coords [lindex $ptsxy $kk] [lindex $ptsxy [expr {$kk + 1}]]
    }
    if { $n <= 2 } {
	#puts "drawing $k,n=$n $coords, points $mesh "
	#desetq "a b" $mesh
	#puts "<[lrange $points $a [expr {$a +2}]]> <[lrange $points $b [expr {$b +2}]]"
	if { $n == 2 } {
#	    set color gray70
#	    catch { set color [oget $win $cmap,$mesh]}
            
	    eval $canv create line $coords -tags [list [list axis mesh.$k]] \
		    -fill $color -width 5 
	} else {
	   # puts "doing special $mesh, $coords"
	    catch { set tem [oget $win special([lindex $mesh 0])]
	    eval [concat $tem $coords]
	}
	}
    } else {
	 eval $canv create polygon $coords -tags [list [list poly mesh.$k]] \
		-fill $color \
		-outline black
    }
}

proc doHelp3d { win } {
 global Parser
 doHelp $win [join [list \
{ 

William Schelter's plotter for three dimensional graphics.

To QUIT this HELP click here.

By clicking on Zoom, the mouse now allows you \
to zoom in on a region of the plot.  Each click \
near a point magnifies the plot, keeping the \
center at the point you clicked.  Depressing \
the SHIFT key while clicking zooms in the \
opposite direction.

Clicking on Rotate, makes the left mouse button  \
cause rotation of the image.   The current position \
can be determined by azimuth and elevation angles \
which are given under the Config menu.   They may also \
be specified on the command line.

To change the equations enter in the entry \
windows, and click on replot.

You may print to a postscript printer, or save the plot \
as a postscript file, by clicking on save.   To change \
between printing and saving see the Print Options under Config.
	
Clicking with the right mouse button and dragging may be used \
instead of the scroll bars to slide the plot \
around.


} $Parser(help)]]
}

proc     makeFrame3d { win } {
  global plot3dPoints
   set w [makeFrame $win 3d]
    set top $w
    catch { set top [winfo parent $w]}
    catch {

    wm title $top "Schelter's 3d Plot Window"
    wm iconname $top "DF plot"
 #   wm geometry $top 750x700-0+20
   }
  
    pack $w

}
    
proc mkPlot3d { win  args } {
    global plot3dOptions  printOption [oarray $win] axisGray

    getOptions $plot3dOptions $args -usearray [oarray $win]
    #puts "$win width=[oget $win width],args=$args"
    setPrintOptions $args
    set printOption(maintitle) ""
    set wb $win.buttons
    setupCanvas $win
   # catch { destroy $win }
    makeFrame3d $win
    oset $win sliderCommand sliderCommandPlot3d
   oset $win noaxisticks 1
   
   makeLocal $win buttonFont c
    bind $c <Motion> "showPosition3d $win %x %y"
    button $wb.rotate -text "Rotate" -command "setForRotate $win" -font $buttonFont
   setBalloonhelp $win $wb.rotate {Dragging the mouse with the left button depressed will cause the object to rotate.  The rotation keeps the z axis displayed in an upright position (ie parallel to the sides of the screen), but changes the viewpoint.   Moving right and left changes the azimuth (rotation about the z axis), and up and down changes the elevation (inclination of z axis).   The red,blue and green sides of the bounding box are parallel to the X, Y and Z axes, and are on the smaller side.} 

   $win.position config -width 15
    pack $wb.rotate -expand 1 -fill x
   setForRotate $win

    
}   

proc doConfig3d { win } {

    
    desetq "wb1 wb2" [doConfig $win]

    makeLocal $win buttonFont

    mkentry $wb1.zfun [oloc $win zfun]  "z=f(x,y)" $buttonFont 
    mkentry $wb1.nsteps [oloc $win nsteps]  "Number of mesh grids"  $buttonFont 
    # button .jim.buttons.rot "rotate" -command "bindForRotation"
    # pack .jim.buttons.rot
    pack $wb1.zfun  $wb1.nsteps
    pack	    $wb1.zfun  $wb1.nsteps 
   foreach w {xradius yradius xcenter ycenter zcenter zradius parameters } {
	mkentry $wb1.$w [oloc $win $w] $w $buttonFont
	pack $wb1.$w 
    }

    scale $wb1.rotxscale -label "azimuth"  \
	    -orient horizontal -length 150 -from -180 -to 180 -resolution 1 \
	    -command "setView $win" -variable [oloc $win az] -tickinterval 120 -font $buttonFont 

    scale $wb1.rotyscale -label "elevation"  \
	    -orient horizontal -length 150 -from -180 -to 180 -resolution 1 \
	    -command "setView $win" -variable [oloc $win el] -tickinterval 120 -font $buttonFont 


#    scale $wb1.rotzscale -label "thetaz"  \
#	    -orient horizontal -length 150 -from -180 -to 180 \
#	    -command "setView $win" -variable [oloc $win thetaz] -tickinterval 120 -font $buttonFont 

    pack   $wb1.rotxscale   $wb1.rotyscale   

}


proc showPosition3d { win x y } {
   # global position c
    makeLocal $win c
    set x [$c canvasx $x]
    set y [$c canvasy $y]
    set it [ $c find closest $x $y]
    set tags [$c gettags $it]
    if { [regexp {mesh[.]([0-9]+)} $tags junk k] } {
	set i 0
	set min 1000000
	set at 0
	# find closest.
	foreach {x1 y1} [$c coords $it] {
	    set d [expr {($x1 - $x)*($x1 - $x)+($y1 - $y)*($y1 - $y)}]
	    if { $d < $min} { set at $i ; set min $d }
	    incr i
	}
	set mesh [lindex [oget $win lmesh] $k]
	set ll [lindex $mesh $at]
	set pt [lrange [oget $win points] $ll [expr {$ll + 2}]]
	# puts pt=$pt
	catch { oset $win position [eval [concat "format {(%.2f %.2f %.2f)}" $pt]] }
    }
#    oset $win position [format {(%.1f %.1f)} $x $y]
#    oset $win position \
#      "[format {(%.2f,%.2f)}  [storx$win [$c canvasx $x]] [story$win [$c canvasy $y]]]"
}



#
 #-----------------------------------------------------------------
 #
 # rotateRelative --  do a rotation indicated by a movement
 # of dx,dy on the screen.  
 #
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#

proc rotateRelative { win x1 x2 y1 y2 } {
    makeLocal $win c az el rotatefun
    set x1 [$c canvasx $x1]
    set x2 [$c canvasx $x2]
    set y1 [$c canvasy $y1]
    set y2 [$c canvasy $y2]
    set xx [expr {$x2-$x1}]
    set yy [expr {($y2-$y1)}]
    set res [$rotatefun 0 0 1]
    set res1 [$rotatefun 0 0 0]
    set fac [expr {([lindex $res 1] > [lindex $res1 1] ? -1 : 1) }] ;
   # puts "fac=$fac,[lindex $res 1],[lindex $res1 1]"
    oset $win az [reduceMode360 [expr   {round($az + $fac *  $xx /2.0) }]]
    oset $win el [reduceMode360 [expr   {round($el -  $yy /2.0) }]]
    setView $win ignore


}

proc reduceMode360 { n } {
  return [  expr fmod(($n+180+5*360),360)-180]

}

proc setForRotate { win} {
    makeLocal $win c
    $c delete printrectangle
    bind $c  <Button-1> "setQuick $win 1 ; doRotateScreen $win %x %y "
    bind $c  <ButtonRelease-1> "setQuick $win 0 ; setView $win ignore"
}
proc doRotateScreen { win x y } {
    makeLocal $win c
    oset $win lastx $x
    oset $win lasty $y
    bind $c <B1-Motion> "doRotateScreenMotion $win %x %y"
    

}

proc doRotateScreenMotion {win x y } {
    makeLocal $win lastx lasty
    set dx [expr {$x - $lastx}]
    set dy [expr {$y - $lasty}]
    if { [vectorlength $dx $dy] < 4 } { return }
    rotateRelative $win $lastx $x $lasty $y
    oset $win lastx $x
    oset $win lasty $y
    
}

    
proc sliderCommandPlot3d { win var val } {
    linkLocal $win recompute

    updateParameters $win $var $val
    set com "recomputePlot3d $win"
    # allow for fast move of slider...    
    #mike FIXME: this is a wrong use of after cancel
    after cancel $com
    after 10 $com
}

proc recomputePlot3d { win } {
    linkLocal $win  recompute
    if { [info exists recompute]  } {
	incr recompute
	return
    } else {
		set recompute 1
    }
    set redo 0
    while { $redo != $recompute } {
	set redo $recompute
#	puts "replot3d $win,[oget $win parameters]"
	catch {replot3d $win }
	update
    }
    unset recompute
}
 

## endsource plot3d.tcl
## source nplot3d.tcl

###### nplot3d.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

# source plotting.tcl ; source nplot3d.tcl ; catch { destroy .plot3d} ;  plot3d -zfun "" -data $sample -xradius 10 -yradius 10
# newidea:
# { plot3d
#  { gridequal {minx maxx} {miny maxy}
#   {{z00 z01 z02 .. z0n } { z10 z11 z12 .. z1n} {..  } ...}
#  { grid {x0 x1  xm} {y0 y1 yn } miny maxy}
#   {{z00 z01 z02 .. z0n } { z10 z11 z12 .. z1n} {..  } ...}
#  { xyzgrid {{x00 y00 z00 x01 y01 z01 .. x0  }{x0 x1  xm} {y0 y1 yn } miny maxy}
#   {{z00 z01 z02 .. z0n } { z10 z11 z12 .. z1n} {..  } ...}
# tclMesh(2*[0,0,0,0,0;1,1,1,1,1]-1,2*[0,1,1,0,0;0,1,1,0,0]-1,2*[0,0,1,1,0;0,0,1,1,0]-1)
  
#     { gridequal { 

# z00 z01 .. all belong to x=minx and y = miny,.... up y=maxy in n+1 steps
#{ grid {minx maxx} {miny maxy}
#  {{z00 z01 z02 .. z0n } { z10 z11 z12 .. z1n} {..  } ...}
# }
# where a mesh(1) {z00 z01 z11 z10} above 



# { mesh {{{x00 y00 z00 } { x01 y01 z01} { x02 y02 z02}  ..}{{x10 y10 z10} {x11 y11 z11} ......} ..}}
# mesh(1) = P00 P01 P11 P10

set sample { variable_grid { 0 1 2 } { 3 4 5} { {21 111 2} {3 4 5 } {6 7 8 }}}
set sample { variable_grid { 0 1 2 } { 3 4 5} { {0 1 2} {3 4 5 } {6 7 8 }}}
set sample { matrix_mesh {{0 1} { 2 3 } {4 5 }}  {{0 1} { 2 3 } {4 5 }}  {{0 1} { 2 3 } {4 5 }} }
set sample { matrix_mesh {{0 1 2} {0 1 2 } {0 1 2 }} {{3 4 5} {3 4 5} {3 4 5}} { {0 1 2} {3 4 5 } {6 7 8 }}}
set sample1 { variable_grid  { 1 2 3 4 5 6 7 8 9 10 }
 { 1 2 3 }
 {  { 0 0 0 0 0 0 0 0 0 0 }
 { 0 0.68404 1.28558 1.73205 1.96962 1.96962 1.73205 1.28558 0.68404 2.44921e-16 }
 { 0 1.36808 2.57115 3.4641 3.93923 3.93923 3.4641 2.57115 1.36808 4.89843e-16 }
 }  }

set sample { matrix_mesh  {  { 0 0 0 0 0 }
 { 1 1 1 1 1 }
 }  {  { 0 1 1 0 0 }
 { 0 1 1 0 0 }
 }  {  { 0 0 1 1 0 }
 { 0 0 1 1 0 }
 }  } 

    
proc  fixupZ { } {
    uplevel 1 {
	if { [catch { expr $z + 0 } ] } {
	    set z nam
	}  elseif { $dotruncate  &&  ($z > $zzmax || $z < $zzmin) } {
	    set z nam
	    
	} else {
	    if { $flatten } {
		if { $z > $zzmax } { set z $zzmax } elseif {
		    $z < $zzmin } { set z $zzmin }}
		    
		    if { $z < $zmin }  { set zmin $z } elseif {
			$z > $zmax } { set zmax $z }
		    }
		}
}


proc vectorLength { v } {
    expr { sqrt(1.0 * [lindex $v 0]*[lindex $v 0] + [lindex $v 1]*[lindex $v 1] + [lindex $v 2]*[lindex $v 2]) }
}

proc normalizeToLengthOne { v } {
    set norm [expr { sqrt(1.0 * [lindex $v 0]*[lindex $v 0] + [lindex $v 1]*[lindex $v 1] + [lindex $v 2]*[lindex $v 2]) }]
    if { $norm != 0.0 } {
	return [list [expr { [lindex $v 0] / $norm  } ] \
		[expr { [lindex $v 1] / $norm  } ] \
		[expr { [lindex $v 2] / $norm  } ] ]
	 
    } else { return "1.0 0.0 0.0 " }
}
    
    

proc vectorCross { x1 x2 }  {
     list \
      [expr { [lindex $x1 1]*[lindex $x2 2]- [lindex $x2 1]*[lindex $x1 2]}] \
      [expr { [lindex $x1 2]*[lindex $x2 0]- [lindex $x2 2]*[lindex $x1 0] } ] \
      [expr { [lindex $x1 0]*[lindex $x2 1]- [lindex $x2 0]*[lindex $x1 1] }]
}
    
proc linspace { a b n } {
    if { $n < 2 } { error "from $a to $b requires at least 2 points" }
    set del [expr {($b - $a)*1.0/($n -1)  }]
    for { set i 0 } { $i < $n } { incr i } {
	lappend ans [expr {$a + $del * $i}]
    }
    return $ans
}


proc addOnePlot3d { win data } {
    upvar #0 plot3dMeshes$win meshes 
    #puts " adding meshes = plot3dMeshes$win"
    #puts "data=$data"
    linkLocal $win points zmax zmin zcenter zradius rotationcenter xradius yradius xmin xmax ymin ymax lmesh
    makeLocal $win flatten 
    catch { unset  meshes }
    set points ""


    set dotruncate [expr ![catch {expr {$zradius + 1} }]]
    set k [llength $points]
    set type [lindex $data 0]
    # in general the data should be a list of plots..
    if { [lsearch {grid mesh variable_grid matrix_mesh }  $type ]>=0 } {
	set alldata [list $data]
    } else {set alldata $data}
    foreach data $alldata {	
	set type [lindex $data 0]
    if { "$type" == "grid" } {
	desetq "xmin xmax" [lindex $data 1]
	desetq "ymin ymax" [lindex $data 2]
	set pts [lindex $data 3]
	
	set ncols [llength $pts]
	set nrows  [llength [lindex $pts 0]]
	set data [list variable_grid [linspace $xmin $xmax $ncols] \
		[linspace $ymin $ymax $nrows] \
		$pts ]
    }
    if { "$type" == "variable_grid" } {
	desetq "xrow yrow zmat" [lrange $data 1 end]
	# puts "xrow=$xrow,yrow=$yrow,zmat=$zmat"
	set nx [expr {[llength $xrow] -1}]
	set ny [expr {[llength $yrow] -1}]
	#puts "nx=$nx,ny=$ny"
#	set xmin [lindex $xrow 0]
#	set xmax [lindex $xrow $nx]
#	set ymin [lindex $yrow 0]
#	set ymax [lindex $yrow $ny]
	desetq "xmin xmax" [minMax $xrow ""]
	desetq "ymin ymax" [minMax $yrow ""]
	desetq "zmin zmax" [matrixMinMax [list $zmat]]
#	puts "and now"
#	dshow nx xmin xmax ymin ymax zmin zmax
	if { $dotruncate } {
	    if { $flatten } { set dotruncate 0 }

	    set zzmax [expr {$zcenter + $zradius}]
	    set zzmin [expr {$zcenter - $zradius}]
	    #puts "zzmax=$zzmax,$zzmin"
	} else { set flatten 0 }



	for {set j 0} { $j <= $ny } { incr j} {
	    set y [lindex $yrow $j]
	    set row [lindex $zmat $j]    
	for {set i 0} { $i <= $nx } { incr i} {
	    set x [lindex $xrow $i]
	    set z [lindex $row $i]
	    #puts "x=$x,y=$y,z=$z, at ($i,$j)"
	    fixupZ 
	    if { $j != $ny && $i != $nx } {
		lappend lmesh [list $k [expr { $k+3 }] \
			[expr { $k+3+($nx+1)*3 }] \
		      [expr { $k+($nx+1)*3 }]]
	    }
	      incr k 3
	  lappend points $x $y $z
	  }
	}
    } elseif { "$type" == "matrix_mesh" } {
	
	desetq "xmat ymat zmat" [lrange $data 1 end]
	foreach v {x y z} {
	    
	    
	    desetq "${v}min ${v}max" [matrixMinMax [list [set ${v}mat]]]
	    
	}
	#puts "zrange=$zmin,$zmax"
	set nj [expr {[llength [lindex $xmat 0]] -1 }]
	set ni [expr {[llength $xmat ] -1 }]
	set i -1
	set k [llength $points]
	foreach rowx $xmat rowy $ymat rowz $zmat {
	    set j -1
	    incr i
	    if { [llength $rowx] != [llength $rowy] } {
		error "mismatch rowx:$rowx,rowy:$rowy"
	    }
	    if { [llength $rowx] != [llength $rowz] } {
		error "mismatch rowx:$rowx,rowz:$rowz"
	    }
	    foreach x $rowx y $rowy z $rowz {
		incr j
		if { $j != $nj && $i != $ni } {
		#puts "tes=($i,$j) $x, $y, $z"
		    lappend lmesh [ list \
			    $k [expr { $k+3 } ] [expr { $k + 3  + ($nj+1)*3}] \
			    [expr { $k+($nj+1)*3 }] ]
		}
		incr k 3
		lappend points $x $y $z
	    }
	}
    } elseif { 0 && "$type" == "mesh" } {
  # walk thru compute the xmin, xmax, ymin , ymax...
  # and then go thru setting up the mesh array..
  # and maybe setting up the color map for these meshes..
  #
    # { mesh {{{x00 y00 z00 } { x01 y01 z01} { x02 y02 z02}  ..}{{x10 y10 z10} {x11 y11 z11} ......} ..}}
# mesh(1) = P00 P01 P11 P10
    set mdata [lindex $data 1]
    set nx [llength $mdata]
    set ny [llength [lindex $mdata 0]]
    
    for {set i 0} { $i <= $nx } { incr i} {
	set pts [lindex $mdata $i]
	set j 0
	foreach { x y z} $pts {
	    fixupZ $z
	    if { $j != $ny && $i != $nx } {
		lappend lmesh [list 
			$k [expr { $k+3 }] [expr { $k+3+($ny+1)*3 }] \
			[expr { $k+($ny+1)*3 }] ]
		}
	    }
	    incr k 3
	    lappend points $x $y $z
	    incr j
	}
    }
   }
    foreach v { x y z } {
	set a [set ${v}min]
	set b  [set ${v}max]
	if { $a == $b } {
	    set ${v}min [expr {$a -1}]
	    set ${v}max [expr {$a +1}]
	}
	set ${v}radius [expr {($b - $a)/2.0}]
	set ${v}center [expr {($b + $a)/2.0}]
    }
    if { "$rotationcenter" == "" } {
	set rotationcenter "[expr {.5*($xmax + $xmin)}] [expr {.5*($ymax + $ymin)}]   [expr {.5*($zmax + $zmin)}] "
    }
    
    #puts "meshes data=[array get meshes]"
    #global plot3dMeshes.plot3d
    #puts "array names plot3dMeshes.plot3d = [array names plot3dMeshes.plot3d]"
}

proc vectorDiff { x1 x2 } {
    list [expr { [lindex $x1 0] - [lindex $x2 0] }] \
	    [expr { [lindex $x1 1] - [lindex $x2 1] }] \
	    [expr { [lindex $x1 2] - [lindex $x2 2] }]
}


proc oneCircle { old2 old1 pt radius nsides { angle 0 } } {
    set dt  [expr {  3.14159265358979323*2.0/($nsides-1.0) + $angle }]
    for  { set i 0 } { $i < $nsides } { incr i } {
	set t [expr {$dt*$i }]
	lappend ans [expr { $radius*([lindex $old2 0]*cos($t) + [lindex $old1 0] * sin($t)) + [lindex $pt 0] } ] \
		[expr { $radius*([lindex $old2 1]*cos($t) + [lindex $old1 1] * sin($t)) + [lindex $pt 1] } ] \
		[expr { $radius*([lindex $old2 2]*cos($t) + [lindex $old1 2] * sin($t)) + [lindex $pt 2] } ]
    }
    return $ans
}

proc curve3d { xfun yfun zfun trange } {
    foreach u { x y z} {
	set res [parseConvert [set ${u}fun] -variables t]
	proc _${u}fun { t } [list expr [lindex [lindex $res 0] 0]]
}   }

proc tubeFromCurveData { pts nsides radius } {
    set n [llength $pts] ;
    set closed [ expr { [vectorLength [vectorDiff [lindex $pts 0] [lindex $pts end]]] < .02} ]
    if { $closed } {
	set f1 [expr {$n -2}]
	set f2 1
    } else { set f1 0
	 set f2 1
    }
    set delta [vectorDiff [lindex $pts $f2] [lindex $pts $f1]]
    if { [lindex $delta 0] == 0 && [lindex $delta 1] == 0 && [lindex $delta 2] == 0 } { set delta "0 0 1.0" }
    set old ".6543654 0.0765456443 0.2965433"
    set old1 [normalizeToLengthOne [vectorCross $delta $old]]
    set n1 $old1
    set n2 [normalizeToLengthOne [vectorCross $delta $old1]]
    set first1 $n1 ; set first2 $n2
    
    lappend ans [oneCircle $n2   old1 [lindex $pts 0]]
    for { set j 1 } { $j < $n -1 } { incr j } {
	set delta [vectorDiff [lindex $pts $j] [lindex $pts [expr {$j+1}]]]
	if { [lindex $delta 0] == 0 && [lindex $delta 1] == 0 && [lindex $delta 2] == 0 } { set delta $old
    }
    set old $delta
    set old1 [normalizeToLengthOne [vectorCross $delta $n1]]
    set old2 [normalizeToLengthOne [vectorCross $delta $n2]]
    set n2 $old1
    set n1 $old2
    lappend ans [oneCircle $n2 $n1 [lindex $pts $j] $radius $nsides]
}
    if { $closed } {
	set f2 1 ; set f1 [expr {$n -2}] ; set f3 0
    } else {
	set f1 [expr {$n -2}] ; set f2 [expr {$n-1}] ; set f3 $f2
    }

    set delta [vectorDiff [lindex $pts $f2] [lindex $pts $f1]]
    if { [lindex $delta 0] == 0 && [lindex $delta 1] == 0 && \
	    [lindex $delta 2] == 0 } { set delta $old }
    set old1 [normalizeToLengthOne [vectorCross delta $n1]]
    set old2 [normalizeToLengthOne [vectorCross $n2 $delta]]
    set n2 $old1 ; set n1 $old2
    if { $closed } {
	set angle [vangle $first1 $n1]
	set n1 $first1 ; st n2 $first2;
    }
    lappend ans [oneCircle $n2 $n1 [lindex $pts $f3] $radius $nsides $angle]
   return $ans
}


#
 #-----------------------------------------------------------------
 #
 # vangle --  angle between two unit vectors
 #
 #  Results: an angle
 #
 #  Side Effects: none.
 #
 #----------------------------------------------------------------
#
proc vangle { x1 x2 } {
    set dot [expr { [lindex $x1 0]*[lindex $x2 0] +\
	     [lindex $x1 1]*[lindex $x2 1] +\
	     [lindex $x1 2]*[lindex $x2 2]} ]
    if { $dot >= 1 } { return 0.0 }
    if { $dot <= -1.0 } { return 3.141592653589 }
    return [expr { acos($dot) } ]
}

## endsource nplot3d.tcl

# from shell 
# wish8.0 plotting.tcl -eval {plot2d -xfun  x^2+3}
# or in html
# <embed src=plotting.tcl eval="plot2d -xfun  x^2+3" >
#


## endsource plotting.tcl
## source patchold.tcl

###### patchold.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

# these are some compatibility patches for older versions
# eg tk4.1  Before tk4.1 it wont work.

proc  mproc { name argl body } {
    if { "[info command $name]" == "" } {
	proc $name $argl $body
    }
}
mproc event {args} {}
mproc font  {option args} {
    global  ws_openMath
    switch $option {
	create  {
		
	    # puts "args=$args"
	    
	    set family [assoc -family $args "courier"]
	    set family [string tolower $family]
	    set slant [assoc -slant $args r]
	    if { "$slant" == "italic" || "$slant" == "oblique" } {
		set slant o
	    } else { set slant r}
	    
	    set size [assoc -size $args 10]
	    set weight [assoc -weight $args normal]
	    if { [fontExistsp  $family $weight $slant $size ] } {
		return [.bfontexists cget -font]
	    }
	    if { [fontExistsp  $family * $slant $size ] ||
	    [fontExistsp  $family * $slant  $size ] ||
	    [fontExistsp  $family $weight *  $size ] ||
	    [fontExistsp  $family $weight $slant  [expr $size -1]] ||
	    [fontExistsp  $family $weight $slant  [expr $size +1]] ||
	    [fontExistsp  $family $weight $slant  *] ||
	    [fontExistsp  * $weight $slant  *] ||
	    [fontExistsp  * $weight *  *] ||
	    [fontExistsp  * * *  *] } {
		return [.bfontexists cget -font]
	    } else { return [lindex [.bfontexists config -font] 3]
	    }
	}
    default { error "cant measure"
}
}
}

mproc font {option args} {
    global bil ws_openMath
    switch $option {
	create  {
	    set bil $args
	    # puts "args=$args"
	    
	    set family [assoc -family $args "courier"]
	    set family [string tolower $family]
	    set slant [assoc -slant $args r]
	    if { "$slant" == "italic" || "$slant" == "oblique" } {
		set slant o
	    } else { set slant r}
	    
	    set size [assoc -size $args 10]
	    set weight [assoc -weight $args normal]
    	    if { [catch { set allfonts $ws_openMath(allfonts)} ] &&
	    [catch { set allfonts [exec xlsfonts] } ]
	} { 	        return [list $family $size $weight]
	}
	set ws_openMath(allfonts) $allfonts
	# puts " lsearch -glob \$allfonts *$family*-*$weight-$slant*-$size-*"
	# puts *$family-$weight-$slant*-$size-*
	if { [set ind [lsearch -glob $allfonts *$family-$weight-$slant*-$size-*]] < 0 } {
	   #  puts *$family-normal-$slant*-$size-*
	    if { [set ind [lsearch -glob $allfonts *$family-normal-$slant*-$size-*]] >= 0 } { return  [lindex $allfonts $ind]  }
	return [list $family $size $weight]
	} else { 	    return [lindex $allfonts $ind] }
    }  
    default { error "cant measure"
}
}
}

proc fontExistsp {   family weight slant size } {
    if { ![winfo exists .bfontexists ] }  { entry .bfontexists }
    return [expr ![catch { .bfontexists config -font *-$family-$weight-$slant-*--$size-*-*-*-*-*-*-* }]]
}
    
 


## endsource patchold.tcl
## source eoctave.tcl

###### eoctave.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################


#
 #-----------------------------------------------------------------
 #
 # insertResult_octave --  insert result RES, in text window W,
 # into RESULTRANGE.  The command which was sent to octave came
 # from THISRANGE.   For plots if a resultRANGE is missing,
 # we use a space just after the end of the line of THISRANGE.
 # checks if this is plotdata, and if so makes plot win for it.
 #
 #  Results: none
 #
 #  Side Effects:  inserts in text or graph in window W.
 #
 #----------------------------------------------------------------
#

proc insertResult_octave {  w thisRange resultRange res } {
    #puts "res=$res"
  if { [regexp "\{plot\[23\]d" $res] } {
      #puts "its a plot"
      set name [plotWindowName $w]
      set tem [setDesiredDims $w $name $thisRange ]
      eval plot2dData $name $res [getDimensions $w $name] 
      ShowPlotWindow $w $name  $thisRange $resultRange $tem
      return 0
   } elseif { "$resultRange" != "" } {
	insertResult $w $resultRange $res
   }
   return 0
}

global ws_openMath
set ws_openMath(options,octave) {{doinsert 1 "Do an insertion" boolean}}


## endsource eoctave.tcl
## source eopenplot.tcl

###### eopenplot.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################



#
 #-----------------------------------------------------------------
 #
 # eval_openplot --  invoke OPENPLOT on the substring of Window given
 # by thisRange, and substitute the result into resultRange, if the
 # latter is not the empty list.   If it is, then the window is placed
 # on the next line from this command.
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#

proc eval_openplot { program w thisRange resultRange } {

 
   set name  [plotWindowName $w]
    set desired [setDesiredDims $w $name $thisRange ]
   set tem [eval $w get $thisRange]
   lappend tem -windowname $name
   foreach v [getDimensions $w $name] { lappend tem $v }
   set allowed "plot2d plotdf plot3d"
   set f [lindex $tem 0]
   if { [lsearch $allowed $f] >= 0 } {
       apply $f [lrange $tem 1 end]
       ShowPlotWindow $w $name $thisRange $resultRange $desired
   } else { error "$f not allowed, only {$allowed}" }
   return 0
}


#
 #-----------------------------------------------------------------
 #
 # plotWindowName --  checks preferences to see if separate or multiple
 #  or nontoplevel windows are desired, and chooses a name accordingly.  
 #  in the first two cases it also assures that the toplevel window exists.
 #  
 #  Results: window name
 #
 #  Side Effects:  possibly make a new toplevel window.
 #
 #----------------------------------------------------------------
#
proc plotWindowName { w } {
    upvar #0 xHMpreferences(plotwindow) plot
    upvar #0 ws_openMath(plot,count) count
    set name ""
    
    if { ![info exists plot] || "$plot" == "embedded" } {
	linkLocal $w counter
	if { ![info exists counter] } {set counter 0}
	return $w.plot[incr counter]
    }
    set name ".plotfr"	
    if { "$plot" == "multiple" } {
	if { ![info exists count] } { set count 1} else {
	    incr count }
	append name $count
    }
    if { ![winfo exists $name ] } {
	toplevel $name
	set h [expr {round ([winfo screenheight $name]*.6) }]
	set wid [expr round ($h * 1.2) ]
	set r1 [expr {round(10+rand()*30)} ]
	set r2 [expr {round(10+rand()*30)} ]
	wm geometry $name ${wid}x${h}+${r1}+${r2}
	if { "[info proc setIcon]" != "" } {
	    after 1000 setIcon $name
	}

    }

    append name .plot
    return $name
}


proc whereToPutPlot { w thisRange resultRange } {
    if { "$resultRange" != "" } {
	eval $w  delete $resultRange
	set at [lindex $resultRange 0]
	$w insert $at " " { Tresult}
	set at [$w index "$at + 1char"]
    } else {
	set at "[lindex $thisRange 1] lineend + 1 chars"
    }
    return $at
}


proc setDesiredDims { w name range } {
    #puts "setDesiredDims  $w $name $range"
    foreach v [getTagsMatching $w "^(width|height):" $range] {
	set tem [split $v :]
	lappend ans [lindex $tem 0]Desired [lindex $tem 1]
    }
    if { [info exists ans] } {
	oarraySet $name $ans
	return $ans
    }
    return ""
}

proc getDimensions { w name } {
   # puts "getDimensions  $w $name"
   set parent [winfo parent $w] 
   set scrollwidth 15
   catch { set scrollwidth [ [winfo parent $parent].scroll cget -scrollwidth] }
   set width [winfo width $w]
   set height [winfo height $w]
   #set width [getPercentDim [oget $name widthDesired] width $w] 
   catch {set width [getPercentDim [oget $name widthDesired] width $w] }
   catch {set height [getPercentDim [oget $name heightDesired] height $w] }
   
   set width [expr {round ($width-4) }]
   set height [expr {round ($height-4)}]
   #puts "using width,height=$width,$height"
   
   if { $width <0 } {
       set width [expr {[oget $parent width] - 2*$scrollwidth}]
       set height [expr {round(.85*[oget $parent height])}]
   }
   return " -width $width -height $height"
}

global ws_openMath
set ws_openMath(options,openplot) {{doinsert 0 "Do an insertion" boolean}}

proc insertResult_openplot {w args } { puts "insert=[$w index insert]"  }

proc ShowPlotWindow { w name thisRange resultRange desired } {
   if { "[winfo toplevel $w]" != "[winfo toplevel $name]" } {
       $name config -relief sunken -borderwidth 2
       pack $name -expand 1 -fill both
       raise [winfo toplevel $name ]
       return
   }
   oarraySet $name $desired
   set at [whereToPutPlot $w $thisRange $resultRange]
   set col [lindex [split $at .] 1]
    
   if { $col > 0 } {
       $w insert $at "\n \n" "$name"
       set at [$w index "$at +1char"]
   }
   # compute where we will try to display.
   # try to leave top of window where it is, but if not
   # scroll lines up just the amount necessary to make the
   # window visible.
   
   set h1 [winfo height $w]
   set h2 [oget  $name height]
   set begin [$w index @0,0]
   set ind $at
   set dl [$w dlineinfo $ind]
   set y0 [lindex $dl 1]
   set prev ""
   if { "$y0" != "" } {
   while { [$w compare $begin <= $ind] } {
       set dl [$w dlineinfo $ind]

       if { "$dl" == "" } { break }
       if { $y0 - [lindex $dl 1] + $h2 +5 < $h1  } {
	   set prev $ind
	   set ind [$w index "$ind - 1 line" ]
       } else { break }
   }
  }

   bind $name <Destroy> "catch {$w yview [$w index @0,0] } ; eval $w delete \[$w tag  nextrange $name 0.0 \]"

   if { "$prev" != "" } { set ind $prev }
      $w insert $at " " "$name center"
      $w window create  $at+1char -window $name
      $w tag add "center $name" $at "$at+2char"
   update
   $w yview $ind
   # somehow the single button click gets run positioning the cursor
   # near where the 
   after 1 $w mark set insert [$w index insert]
   return $ind
}

## endsource eopenplot.tcl
## source emaxima.tcl

###### emaxima.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################


#
 #-----------------------------------------------------------------
 #
 # insertResult_maxima --  insert result RES, in text window W,
 # into RESULTRANGE.  The command which was sent to maxima came
 # from THISRANGE.   For plots if a resultRANGE is missing,
 # we use a space just after the end of the line of THISRANGE.
 # checks if this is plotdata, and if so makes plot win for it.
 #
 #  Results: none
 #
 #  Side Effects:  inserts in text or graph in window W.
 #
 #----------------------------------------------------------------
#

proc insertResult_maxima {  w thisRange resultRange res } {
    set program maxima
    
#    puts <lengthres=[llength $res],thisRange=$thisRange,resultRange=$resultRange>

    if { 0 == [string compare "$res" "cant connect"] } {
	bgerror "unable to call $program"
    }
  if { [regexp "\{plot\[23\]d" $res] } {
      #puts "its a plot"
      set name [plotWindowName $w]
      eval plot2dData $name $res [getDimensions $w $name]
      set desired [setDesiredDims $w $name $thisRange ]
      ShowPlotWindow $w $name  $thisRange $resultRange $desired
      return 0
    }   

    if { "$resultRange" != "" }   {
	set name $w.plot[oset $w counter [expr {1 + [oget $w counter]}]]
	insertResult $w $resultRange $res
	
    }
    return 0
}

## endsource emaxima.tcl
## source ehref.tcl

###### ehref.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################


#
 #-----------------------------------------------------------------
 #
 # eval_href --  Follow a link to another om document
 #
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#

proc obsoleteeval_href { program w this nextResult} {
    set arg ""
   foreach v [$w tag names [lindex $this 0]] {
       if { [string first "\{ThrefArg" $v] == 0 } {
	   set arg $v
	   break
       }
   }
   set arglist [getTargTags $w $this]
   if { [llength $arglist] != 1 } {
       return -code error -errorinfo  "[llength $arglist] args to href.  Wanted 1, got: $arglist"
   }
   puts "arglist=$arglist"
   
   set arg [lindex $arglist 0]
   puts "arg=$arg"
   set list [lrange $arg 1 end]
   set doc [assoc -src $list ""]

   set searchregexp [assoc -searchregexp $list ""]
   set search [assoc -search $list ""]

   puts "doc=$doc"
   
   if { "$doc" != "" } {
       puts "       OpenMathOpenUrl $doc -commandpanel [omPanel $w]"
       OpenMathOpenUrl $doc -commandpanel [omPanel $w]
   }
   makeLocal  [omPanel $w] textwin
   set ind ""
   if { "$searchregexp" != "" } {
       set ind [ $textwin search -regexp -- $searchregexp 1.0]
   } elseif { "$search" != "" } {
       set ind [ $textwin search -exact -- $search 1.0]
   }
   if { "$ind" != "" } {
       $textwin yview $ind
   }
   return 0
}

global ws_openMath
set ws_openMath(options,href) {
    {src "" "A URL (universal resource locator) such as http://www.ma.utexas.edu/foo.om"}
    {search "" "A string to search for, to get an initial position"}
    {searchregexp "" "A regexp to search for, to get an initial position"}
}


## endsource ehref.tcl
## source browser.tcl

###### browser.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

global MathServer
set MathServer "locahost 4443"
# help keysyms
# bind .jim <Key> "puts {%A %K}"
# to print them out

## source keyb.tcl

###### keyb.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

proc peekLastCommand {win} {
    global ws_openMath
    if { [info exists ws_openMath(lastcom,$win)] } {
	return $ws_openMath(lastcom,$win)
    }
}

proc pushCommand { win command arglist } {
    global ws_openMath
    set ws_openMath(lastcom,$win) [list $command $arglist]
}



global ws_openMath
set ws_openMath(sticky) "^Teval$|^program:"


#
 #-----------------------------------------------------------------
 #
 # tkTextInsert --  we add some things to the default tkTextInsert
 #  so that tags present before or after the insert, which are sticky
 #  are added to the inserted string.   As usual, ones on both sides
 #  are added.
 # 
 #  Results: 
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#

proc tkTextInsert { w s } {
    global ws_openMath
    set after [$w tag names insert]
    set before [$w tag names "insert-1char"]
    set both [intersect $after $before]
   # puts "after=$after"
   # puts "before=$before"
    
    foreach v [concat $after $before] {
	if { [regexp $ws_openMath(sticky) $v] } {
	    lappend both $v
	}
    }

    if { [info exists ws_openMath($w,inputTag) ] } {
	lappend both $ws_openMath($w,inputTag)
    }
    
    if {($s == "") || ([$w cget -state] == "disabled")} {
	return
    }
    catch {
	if {[$w compare sel.first <= insert]
		&& [$w compare sel.last >= insert]} {
	    $w delete sel.first sel.last
	}
    }
    $w insert insert $s $both
    $w see insert
    
}    
proc getRange { win a b }  {
    if { [$win compare $a < $b ] } { return "$a $b" } else { return "$b $a"}
}


#
 #-----------------------------------------------------------------
 #
 # binding --   push the current selection on the killRing, and
 # if there is no selection, push the region between the anchor and
 # the point.   
 #  Results:
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
bind OpenMathText <Control-Key-w> {
    pushCommand %W OpenMathTextCut ""
    # in the first case the <<Cut>> event on Text will delete the selection.
    if { [catch { pushl [saveText %W sel.first sel.last] killRing } ] } {
	catch {
	    set range [getRange %W anchor insert]
	    pushl [eval saveText %W $range] killRing
	    eval %W delete $range
	}
    }
}


if { [catch { set ws_openMath(bindings_added) } ] } {
    bind Text <Control-Key-k> "openMathControlK %W \n [bind Text <Control-Key-k>]"
    bind Text <B3-Motion> [bind Text <B2-Motion>]
    bind Text <Button-3> [bind Text <Button-2>]
    
  set ws_openMath(bindings_added) 1
}

global ws_openMath
set ws_openMath(doublek) 0

bind OpenMathText <Control-Key-k><Control-Key-k> {
    set ws_openMath(doublek) 1
}

proc openMathControlK { win } {
    global ws_openMath
    if { $ws_openMath(doublek) != 0 } {
	set now [popl killRing ""]
    } else { set now "" }
    set ws_openMath(doublek) 0
    if { [$win compare insert == "insert lineend" ]  } {
	if { [$win compare insert < end] } {
	    append now "\nTins {[ldelete sel [$win tag names insert]]} {\n}"
    } } else {
	append now "\n[saveText $win insert {insert lineend}]"
    }
    pushl $now killRing
}

bind OpenMathText <Control-Key-y> "OpenMathYank %W 0; break"
bind OpenMathText <Alt-Key-y> "OpenMathYank %W 1; break"
bind OpenMathText <Meta-Key-y> "OpenMathYank %W 1; break"

proc OpenMathYank {win level } {
    global ws_openMath
    #puts "doing OpenMathYank $win $level"
    if { $level == 0 } {
	set ws_openMath(currentwin) $win
	pushCommand $win OpenMathYank [list $win $level]
	set ws_openMath(point) insert
	$win mark set beforeyank insert
	$win mark gravity beforeyank left
	eval [peekl killRing "" ]
    } else {
	if { [catch {
	    set last $ws_openMath(lastcom,$win)
	    set m [lindex [lindex $last 1] 1]
	    incr m
	    if { "[lindex $last 0]" == "OpenMathYank" &&
	    "$ws_openMath(currentwin)" == "$win"
	    && "$ws_openMath(point)" == "insert" } {set doit 1}} ]
	    || $doit==0} {
		    pushCommand $win Error "" } else {
			
			set res [peekl killRing _none_ [expr {$m + 1}]]
		if { "$res" == "_none_" } {
		    # this will cause to cycle
		    set m 0
		} else {
		    $win delete beforeyank insert
		    eval $res
		}
                pushCommand $win OpenMathYank [list $win $m] 
	    }
    }
    catch { $win see insert}
}

# put the clipboard paste on Control-Shift-y
event add <<Paste>> <Control-Shift-y>

bind OpenMathText <Alt-Key-w> {
    pushCommand %W SaveSelection ""
    if { "[selection own -displayof %W]" == "%W"} {
    pushl [saveText %W sel.first sel.last] killRing
    selection clear -displayof %W
}   }

bind OpenMathText <Key> {openMathAnyKey %W %K %A}
bind OpenMathText <Alt-Key> {openMathAnyKey %W %K ALT_%A}

# stop the double button click word selection in openMathText..
bind OpenMathText <Double-Button-1> { break; }
bind OpenMathText <Control-c><Key-e> {doInvoke %W insert ; break; }

bind OpenMathText <Control-Key-space> {
    pushCommand %W SetAnchor ""
    %W mark set anchor insert }

    
proc openMathAnyKey { win keysym s  } {
   # puts "$win `$keysym' `$s'"
    if { "$s" != "" } {
	pushCommand $win openMathAnyKey [list $win  $keysym $s]
    }

    if { "$s" != "" && [doInsertp [$win tag names insert]]
      && ("$s" == "$keysym"  || [regexp  "\[\n\t \]" "$s" junk] )} {
	setModifiedFlag $win insert
    }
}

proc saveText { win args } {
    set tags [ldelete sel  [$win tag names]]
    set prev [lindex $args 0]
    set endregion [$win index [lindex $args 1 ]]
    if { "$prev" == "" } {set prev 0.0 }
    if { "$endregion" == "" } {set endregion end}
    set allar($prev) 1
    set allar($endregion) 1
    foreach v $tags {
	set ranges [tagRanges $win $v  $prev $endregion]
	foreach {begin end} $ranges {
	    lappend start($begin) $v
	    lappend stop($end) $v
	    set allar($begin) 1
	    set allar($end) 1
	    
	}
    }
   proc __comp { a b} " return  \[$win compare \$a > \$b \] "
   set all [lsort -command __comp [array names allar]]
   set result ""
   foreach v $all {
       append result "Tins [list [array names currentTags]] [quoteBraces [$win get $prev $v]]\n"
       set prev $v


       if { [info exists start($v)] } {

	   foreach u $start($v) { set currentTags($u) 1}
       }

       if { [info exists stop($v)] } {

	   foreach u $stop($v) { unset currentTags($u) }
       }

       

       #puts -nonewline "..deleting{$stop($v)} giving {$currentTags}"

    # puts ">>"

      }
      return $result
}



#
 #-----------------------------------------------------------------
 #
 # tagRanges --  find ranges on WINDOW for TAG from FROMINDEX below TOINDEX
 #
 #  Results: a list of ranges start1 stop1 start2 stop2 .. 
 # which are contained in [fromindex,toindex] such that TAG is on from
 # start1 to stop1 etc.
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc tagRanges { win tag begin end } {
    if {  [$win  compare $begin <= 1.0 ]  &&
    [$win  compare $end >= end ] } {
	return [$win tag ranges $tag ] } else {
	    set answer ""
	    set begin [$win index $begin]
	    set end [$win index $end]
	    if { [lsearch [$win tag names $begin] $tag ]>=0 } {
		set prev [$win tag prevrange $tag $begin+1chars]
		set to [lindex $prev 1]
		if { [$win compare $to > $end ] } {
		    set to $end
		}
		append answer "$begin $to "
		set begin $to
	    }
	    #puts "<$begin $end>"
	    while { [$win compare $begin < $end ] } {
		set next [$win tag nextrange $tag $begin]
		#puts "next=$next"
		if { "$next" == "" } { return $answer }
		if { [$win compare [lindex $next 1] <= $end]} {
		    append answer "$next "
		    set begin [lindex $next 1]
		} elseif {[$win compare [lindex $next 0] < $end ]} {
		    append answer "[lindex $next 0] $end"
		    return $answer
		} else { return $answer }
	    }
	    return $answer
	    
	}
}
	



#
 #-----------------------------------------------------------------
 #
 # quoteBraces --  given a STRING such that
 # puts $file "set new [quoteBraces $string]"
 # when re read by eval would make value of NEW identical to STRING
 #
 #  Results: a string
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc quoteBraces {string } {
    regsub -all {[{}]} $string {\\&} val
    return [list $val]
}

proc thisRange { win tag index } {
    set prev [$win tag prevrange $tag $index]
    if { "$prev" != "" && [$win compare [lindex $prev 1] >= $index] } {
	return $prev
    }
    set next [$win tag nextrange $tag $index]
    if { "$next" != ""  && [$win compare [lindex $next 0] <= $index] } {
	return $next
    }
    return ""
}




#
 #-----------------------------------------------------------------
 #
 # insertRichText --  insert rich text in TEXTWINDOW at INDEX according
 # to commands and data in LIST.   The latter must be of the form
 #  command1 arg1 ..argn command2 arg1 ..argn2 ..
 # for example if `Tins' takes two args 
 #  and the commands must be in 
 # since the rich text might come from a selection or some or an untrusted
 # file we want to be careful not to do any bad evals.
 #  Results: none
 #
 #  Side Effects:  the rich text commands are invoked to do insertions
 # on the window.  
 #
 #----------------------------------------------------------------
#
proc insertRichText {win index list } {
    global ws_openMath 
    set ws_openMath(currentwin) $win
    set ws_openMath(point) $index
    foreach v $ws_openMath(richTextCommands) {
	set ws_openMath($v,richTextCommand) [llength [info args $v]]
    }
    set i 0
    set ll [llength $list]
    while { $i < $ll } {
	set com [lindex $list $i]
	incr i
	if { [catch { set n $ws_openMath($com,richTextCommand)} ] } {
	    return -code error -errorinfo "illegal command in rich text:$com"
	}
	set form [concat $com [lrange $list $i [expr {$i +$n -1}]]]
	if { [catch {eval $form } ] } {
	    return -code error -errorinfo "unable to evaluate command:`$form' " }
	    
	incr i $n
    }
}


proc Tins { tags text } {
   global ws_openMath
   # foreach v $args { append text $v }
   $ws_openMath(currentwin) insert $ws_openMath(point) $text  $tags
}

proc TinsSlashEnd { tags text } {
   global ws_openMath
   # foreach v $args { append text $v }
   $ws_openMath(currentwin) insert $ws_openMath(point) "$text\\"  $tags
}

    

global ws_openMath
set ws_openMath(richTextCommands) {Tins TinsSlashEnd}

## endsource keyb.tcl

proc underTop {top win} {
    if { "$top" == "." } { return $win} else { return $top$win}}

proc showHistory { window } {
    set top [winfo toplevel $window]
    set win [omPanel $window]
    makeLocal $win history historyIndex
    set w [underTop $top .historylist]
    catch {destroy $w}
    frame $w -borderwidth 2 -relief raised
    label $w.title -text "History List" -relief raised
    setHelp $w.title {This window may be dragged elsewhere by grabbing this title bar with the mouse.   Double clicking on a history item, moves to that page.}
    button $w.dismiss -command "destroy $w" -text dimsiss
    setHelp $w.dismiss {Remove the history list} 
    pack $w.title $w.dismiss -side top -expand 1 -fill x
    scrollbar $w.scrolly -command "$w.list yview"
    scrollbar $w.scrollx -orient horizontal -command "$w.list xview"    
    pack $w.scrollx -side bottom -fill x -expand 1
    pack $w.scrolly -side right -fill y -expand 1
    listbox $w.list -yscroll "$w.scrolly set" \
	-width 35 -height 16 -setgrid 1 -xscroll "$w.scrollx set"
    $w.title configure -font [$w.list cget -font]
    set l $w.list

    pack $w.list  -side top -fill both -expand 1
    resetHistory $win $w.list junk history
    global [oarray $win]

    #puts "    trace variable [oloc $win history] w {resetHistory $win $w.list}"
    trace vdelete  [oloc $win history] w "resetHistory $win $w.list"
    trace variable [oloc $win history] w "resetHistory $win $w.list"
    trace vdelete [oloc $win historyIndex] w "resetHistory $win $w.list"
    trace variable [oloc $win historyIndex] w "resetHistory $win $w.list"
    bind $l <Double-1> {OpenMathMoveHistory [omPanel %W] [expr [%W index @%x,%y]-[oget [omPanel %W] historyIndex]]}
    bind  $w.title <B1-Motion> "dragPlacedWindow $w %W %X %Y"
    bind  $w.title <1> "startDragPlacedWindow $w %X %Y"
    place $w -relx .4 -rely .8 -in $top
    
}

proc deleteAllTraces {var} {
    foreach v [uplevel #0 trace vinfo $var] {
	uplevel #0 trace vdelete $var [lindex $v 0] [list [lindex $v 1]]
    }
}
proc resetHistory { win list args } {
    set action [lindex $args 1]
    if { [catch {
	if { "$action" == "history" } {
	    $list delete 0 end
	    if { [winfo exists $list] } { 
		foreach v [oget $win history] {
		    $list insert end [oget $v location]
		}
	    }
	}
	$list selection clear 0 end
	$list selection set [oget $win historyIndex]
	after 200 raise [winfo parent $list]

    } ] } {
	deleteAllTraces [oloc $win history]
	deleteAllTraces [oloc $win historyIndex]
    }
}


proc startDragPlacedWindow { win x y } {
    oset $win placeinfo [list $x $y [place info $win]]
}

proc dragPlacedWindow { win w1 x y } {
    global me recursive
    makeLocal $win placeinfo 
    catch { after cancel [oget $win after]}
    set me [oget $win placeinfo]
    #puts "have=[oget $win placeinfo]"
    desetq "px py pinfo" [oget $win placeinfo]
    set dx [expr {$x - $px}]
    set dy [expr {$y - $py}]
    set nx [expr {$dx + [assoc -x $pinfo]}]
    set ny [expr {$dy + [assoc -y $pinfo]}]
    set new "-x $nx -y $ny"
    eval place $win $new
    oset $win placeinfo [list $x $y $new]
}
    
proc OpenMathMoveHistory { win  n } {
    makeLocal $win history historyIndex
    incr historyIndex $n
    if { $historyIndex >= [llength $history] } {
	set historyIndex  [expr {[llength $history] -1}] }
    if { $historyIndex <0 } { set historyIndex 0}
    if { "[lindex $history $historyIndex]" != ""} {
	OpenMathGetWindow $win [lindex $history $historyIndex]
	oset $win historyIndex $historyIndex
    }
}

proc toLocalFilename { url } {
    set type [assoc type $url] 
    switch $type {
	http {
	    return [assoc filename $url]
	}
	file {
	    return [file join / [assoc dirname $url] [assoc filename $url] ]

	}
	default "unknown type: $type"
    }

}

proc OpenMathGetWindow { commandPanel win } {
    if { "[winfo parent [oget $commandPanel textwin]]" != "$win" } {
    catch { pack forget [winfo parent [oget $commandPanel textwin]] }
    pack $win -expand 1 -fill both
   # pack $win
    oset $commandPanel textwin $win.text
    oset $commandPanel location [oget $win location]
    set tem [toLocalFilename [decodeURL [oget $win location]]]
    oset $commandPanel savefilename  [file root $tem].txt
}   }
    

proc getw { s  } { eval pack forget [winfo children . ] ; pack $s}

proc try1 { file } {
   global ccc
   eval pack forget [winfo children . ] 
   mkOpenMath [set w .t[incr ccc]]
   uplevel #0 source $file
  }

proc filesplit { x } {
    set l [split $x /]
    set n [llength $l ]
    set dir [lrange $l 0 [expr {$n - 2}]]
    set file [lindex $l [expr {$n - 1}]]
    return [list [join $dir /] $file]
}
    
    

proc decodeURL { name } {
    set server ""
    if { [regexp  {([^#]*)#(.*)$} $name junk name anchor] } {
	lappend answer anchor $anchor
	   # puts "answer=$answer"
    }

    
    if { [regexp {^([a-z]+)[(]?([0-9]*)[)]?:/([^ ]+)$} $name all type port path ] } {
	lappend answer type $type
    } else { set path $name ; set type ""
    }
    
    set path [removeDotDot $path]
    #puts "path=$path"
    desetq "dirname filename" [filesplit $path]
    #puts "dirname=$dirname,path=$path,filename=$filename"
    set po [assoc $type {http 80 nmtp 4443} ]
    if { "$po" != "" } {
	if { "$port" == "" } {set port $po }

	if { [regexp {^/([^/:]*)(:([0-9]+))?(.*)$} $dirname all server \
		jun po dirname] } {
	   # puts "hi ther,server=$server"
	    if { "$po" != ""} {set port $po}
	    if { "$dirname" == "" } {set dirname / }
	} elseif { "$server" == "" } {
	    set server $filename
	    set dirname /
	    set filename {}
	}
	lappend answer port $port server $server 
    }
    lappend answer dirname $dirname filename $filename
    return $answer
}

proc removeDotDot { path } {
    while { [regsub  {/[^/]+/[.][.](/|$)} $path "\\1" path] } {list}
    return $path
}

proc appendSeparate { var before item separator } {
    if { "$item" != "" } {
	uplevel 1 append $var $before $item $separator
}   }

proc dirnamePlusFilename { lis } {
  return  [string trimright [assoc dirname $lis ""] /]/[assoc filename $lis ""]
}
proc encodeURL { lis } {
    set type [assoc type $lis ""]
    switch $type {
	nmtp {
	      if { [ set port [assoc port $lis 4443]] != 4443 } {
	       append type "($port)"
	   }
   	   appendSeparate ans "" $type ://[assoc server $lis ""] 
	   append ans [dirnamePlusFilename $lis]
	   appendSeparate ans "#" [assoc anchor $lis ""] ""
	}
	http  {
	   if { [ set port [assoc port $lis 80]] != 80 } {
	       append type "($port)"
	   }
	   appendSeparate ans "" $type ://[assoc server $lis ""] 
	   append ans [dirnamePlusFilename $lis]
	   #appendSeparate ans "" [assoc dirname $lis ""] 
	   #appendSeparate ans "/" [assoc filename $lis ""] ""
	   appendSeparate ans "#" [assoc anchor $lis ""] ""
       }
       file {
	   appendSeparate ans "" $type :/
	   append ans  [dirnamePlusFilename $lis] 
#	   appendSeparate ans "" [assoc dirname $lis ""] "/"
#	   appendSeparate ans "" [assoc filename $lis ""] ""
	   appendSeparate ans "#" [assoc anchor $lis ""] ""
       }
       default "error unsupported url type : $type"
   }
   return $ans
}

proc resolveURL { name current {post ""} } {
    set decode [decodeURL $name]
    #puts "name=$name,current=$current"
    set ans ""
    set relative 0
    if { "[assoc type $decode {} ]" == "" } {set relative 1}
    if { $relative == 0 } {
	set ans  $decode
    } else {
	foreach {x y } $current {
	    switch $x {
		dirname {
		    set ndir [assoc dirname $decode ""]
		    set cdir [assoc dirname $current ""]
		    if { [string match /* $ndir] } {
			set new $ndir
		    } elseif { "$ndir" != "" } {
			if { "$cdir" != ""  } {
			    set new [string trimright $cdir /]/$ndir
			} else { set new $ndir }
		    } else {
			set new $cdir
		    }
		    lappend ans dirname [removeDotDot $new]
		}
		filename {
		    
		    if { "[assoc filename $decode]" == "" && "[assoc anchor $decode]" != "" } {
			lappend ans $x $y
		    }
		}
		post { list }
		default {
		    lappend ans $x  [assoc $x $decode $y]
		}
	    }
	}
	foreach { key val } $decode {
	    if { "[assoc $key $ans --none--]" == "--none--" } {
		lappend ans $key $val
	    }
	}
	

    }
    if { "$post" != "" } {
	set ans [putassoc post $ans $post]
    }
    return $ans
}

global ws_openMath
set ws_openMath(urlHandlers) {
    text/html  netmath
    text/plain netmath
    image/gif  netmath
    application/postscript "ghostview -safer %s"
    application/pdf "acroread %s"
    application/x-dvi "xdvi %s"
}

proc getURLrequest { path server port types {post ""} {meth ""} } {
    global ws_openMath

    if { "$meth" != "" } {set method $meth } else {
	
	set method GET
	if { "$post" != "" } {set method POST}
    }
    
    #puts "getURLrequest $path $server $port [list $types]"
    foreach {v handler}  $ws_openMath(urlHandlers) {
	lappend types $v, 
    }

    set ans "$method $path HTTP/1.0\nConnection: Keep-Alive\nUser-agent: netmath\nHost: $server:$port\nAccept: $types\n"
    if { "$post" != "" } {
	# append ans "Content-length: [string length $post]\n\n$post"
	append ans "Content-type: application/x-www-form-urlencoded\nContent-length: [string length $post]\n\n$post"
    }
    
	return $ans
    
}

proc canonicalizeContentType { type } {
    regexp -nocase {([---a-zA-Z]+)/([---a-zA-Z]+)} $type type
    return [string tolower $type]
}

proc getURL { resolved type {mimeheader ""} {post ""} } {
    global ws_openMath
    set res $resolved
    
    set ans ""
    set method ""
    if { "$mimeheader" != ""} {
	uplevel 1 set $mimeheader \[list\]
    }
    uplevel 1 set $type "unknown"
    
    
    #puts "getting $resolved,post=<$post>"
    switch [assoc type $res] {
	http {
	   # puts $res
	   # puts "socket [assoc server $res] [assoc port $res 80]"
	    if { [info exists ws_openMath(proxy,http) ] } {
		set sock [eval socket $ws_openMath(proxy,http)]
#		puts "opening proxy request socket $ws_openMath(proxy,http)"
	    } else {
	    set sock [socket [assoc server $res] [assoc port $res 80]]
	    }
	    
	    fconfigure $sock -blocking 0
	    ##DO NOT DELETE THE FOLLOWING !!!!!puts!!!!!!!!
	    #puts request=[getURLrequest [dirnamePlusFilename $res] [assoc server $res] [assoc port $res] image/gif $post]
#	    set path [dirnamePlusFilename $res]
	    set path [encodeURL $res]
	    set server [assoc server $res]
	    set port  [assoc port $res]
	    puts $sock [getURLrequest $path $server $port image/gif $post]
	    if { "$post" == "" } {
		oset $sock cachename "http://$server:$port$path"
	    } else { oset $sock cachename "" }
	    flush $sock
	    if { [readAllData $sock -tovar ws_openMath(url_result) \
		    -translation binary -mimeheader ws_openMath(mimeheader)  \
		    -timeout 120000 -chunksize 2024] > 0 } {
		
	    #puts "length=[string length $ws_openMath(url_result)]"
	    #	flush stdout
		
		set contentType [canonicalizeContentType [assoc content-type $ws_openMath(mimeheader) text/plain]]
		uplevel 1 set $type [list $contentType]
		if { "$mimeheader" != "" } {
		    uplevel 1 set $mimeheader \[ uplevel #0 set ws_openMath(mimeheader) \]
		}
		set ans $ws_openMath(url_result)
		unset ws_openMath(url_result)
		return $ans
	    } else {return "had error"
	    }
	}
	file {
	    set name [toLocalFilename $res]
	    set fi [open $name r]
	    set answer [read $fi]
	    if { [regexp {[.]html?$} $name ] || [regexp -nocase "^(\[ \n\t\r\])*<html>" $answer] } {
		set contentType text/html
	    } elseif {  [regexp {[.]gif([^/]*)$} $name ] } {
		set contentType image/gif
	    } else { set contentType text/plain }
	    uplevel 1 set $type $contentType

	    close $fi
	    return $answer
	}
	default {
	    error "not supported [lindex $res 0]"
	}
    }
}




proc getImage { resolved width height} {
    global ws_openMath
    set res $resolved
    #puts [list getImage [list $resolved] $width $height]
    set ans ""
    catch {
	if { "" != "[image type $ws_openMath(image,$res,$width,$height)]" } {
	    set ans $ws_openMath(image,$res,$width,$height)
	}
    }
    if { "$ans" != "" } { return $ans }

    set image [image create photo -width $width -height $height]
    after 10 backgroundGetImage $image [list $resolved] $width $height
    set ws_openMath(image,$res,$width,$height) $image
    return $image
    }


global ws_openMath    
set ws_openMath(imagecounter) 0    

set ws_openMath(brokenimage,data) R0lGODlhHQAgAMIAAAAAAP9jMcbGxoSEhP///zExY/9jzgCEACH5BAEAAAIALAAAAAAdACAAAAPOOLrcLjDCQaq9+CoZaf7YIIicx50nNZYV6k4tCRPuYduSR8vmef+dy2rU4vyOM8uqJzkCBYCoNEqkGZ04SGHLBSiKTewhx/AyI+LxqWIGh5Eo9pdm8D3jhDa9/nrJTQaBfS5/LYGCgxyFe4cnAY+Qj1oFegKHjRKRkpMbgJeIEJqTBTyGnxybAlwbQYygKFusOaavo5SkJ5WYErELKAO6fBy4LxS6vFzEv4snpLIpIszIMiWKeXMWvS7RGXoVsX0g11NR1Bzk6F4jCn0ODgkAOwAA

proc backgroundGetImage  { image res width height }   {
    global ws_openMath
    #puts [list backgroundGetImage  $image $res $width $height ]
    if { [catch { backgroundGetImage1 $image $res $width $height } err ] } {
	if { ![info exists ws_openMath(brokenimage)] } {
	    set ws_openMath(brokenimage) [image create photo -data $ws_openMath(brokenimage,data)]
	}
	 #puts "got error $err, doing $image copy $ws_openMath(brokenimage)"
	set im $ws_openMath(brokenimage)
	$image config -width [image width $im] -height [image height $im]
	$image copy $im
    }
}

    
proc backgroundGetImage1  { image res width height }   {
    #puts  "resolved=$res"
    global ws_openMath
    #puts [list backgroundGetImage $image $res $width $height]
    switch [assoc type $res] {
	http {
	    set server [assoc server $res]
	    set port [assoc port $res 80]
	    if { [info exists ws_openMath(proxy,http) ] } {
		set s [eval socket $ws_openMath(proxy,http)]
#		puts "opening proxy request socket $ws_openMath(proxy,http)"
	    } else {
	    set s [socket [assoc server $res] [assoc port $res 80]]
	    }
	    fconfigure $s -blocking 0
	    ##DO NOT DELETE THE FOLLOWING !!!!!puts!!!!!!!!
	    puts $s [getURLrequest [encodeURL $res] \
		    $server $port {image/gif image/x-bitmap}]
	    flush $s

	    if { [regexp -nocase {[.]gif([^/]*)$} [assoc filename $res] ] } {
		fconfigure $s -translation binary
		set tmp xxtmp[incr ws_openMath(imagecounter)].gif

		if { [info exists ws_openMath(inbrowser)] ||  [catch {set out [open $tmp w] } ] } {
		    # if have binary..
		    if { "[info command binary]" != "binary" } {
			error "need version of tk with 'binary' command for images"}
			#puts "hi binary" ; flush stdout
			if {  [readAllData $s -tovar \
				ws_openMath($s,url_result) -mimeheader \
				ws_openMath($s,mimeheader) 
			] > 0  && [string match *gif [assoc content-type $ws_openMath($s,mimeheader)]] } {
			    set ans $image
			    $image configure -data [tobase64 $ws_openMath($s,url_result)]

			    unset ws_openMath($s,mimeheader)
			    unset ws_openMath($s,url_result)
			    
			} else  {
			    error "could not get image"
			}
		    } else {
			fconfigure $out -translation binary -blocking 0
			if { [readAllData $s -tochannel $out \
				-translation binary \
				-mimeheader \
				ws_openMath($s,mimeheader) -timeout 15000 -chunksize 2024 ] > 0 } {
			    set ans $image
			    $image config  -file \
				    $tmp 
			    unset ws_openMath($s,mimeheader)
			}

			
			
			# all the below just to try to remove the file..
			#  depending on versions and in environments..
			
		}   }
	    }
	    file {
		$image config -file [toLocalFilename $res]
		set ans $image
		# puts "$image config -file [toLocalFilename $res]"
		#set ans [image create photo -file [toLocalFilename $res]]
		
	    
	}
	    default { error "unknown type of image" }
	}
	## if we opened an out channel try hard to remove the tmp file.
	if { [info exists out] &&
	     [catch { file delete $tmp } ] && [catch { rm $tmp }]
	 && [catch { exec rm $tmp }] } {
			    puts "cant remove tmp file $tmp"
	 }
	 if { "$ans" == "" } {
	     error "Unable to open an image for [encodeURL $res]"
	 }

    }
    

#
#-----------------------------------------------------------------
#
# readData --  read data from S, storing the result
# in ws_openMath($s,url_result).   It times out after TIMEOUT without any data coming.
# it can be aborted by setting set ws_openMath($s,done)  -1 
#
# 
#  Results: -1 on failure and 1 on success.
#
#  Side Effects: it initially  empties ws_openMath($s,url_result) and then
#  adds data to it as read.   ws_openMath($s,done) is initialized to 0
#
#----------------------------------------------------------------
#
proc readData { s { timeout 10000 }} {
    global ws_openMath

    after $timeout "set ws_openMath($s,done) -1"
    fconfigure $s  -blocking 0
    set ws_openMath($s,done) 0
    set ws_openMath($s,url_result) ""

    #mike FIXME: this is a wrong use of after cancel
    fileevent $s readable \
	   "after cancel {set ws_openMath($s,done) -1} ; after $timeout {set ws_openMath($s,done) -1} ; set da \[read $s 8000] ; append ws_openMath($s,url_result) \$da; if { \[string length \$da] < 8000  && \[eof $s] } {after cancel {set ws_openMath($s,done) -1} ; set ws_openMath($s,done) 1; fileevent $s readable {} ;  }"
    myVwait ws_openMath($s,done)
    catch { close $s } 
    #mike FIXME: this is a wrong use of after cancel
    after cancel "set ws_openMath($s,done) -1"
    return $ws_openMath($s,done)
}

			

proc doRead { sock } {
    global ws_openMath
    #puts reading; flush stdout;
    set tem [read $sock]
    append ws_openMath(url_result)  $tem
    # puts read:<$tem>
    # flush stdout
    if { [eof $sock] } {
	set ws_openMath(done) 1
	close $sock}
}

proc tes {} {
    OpenMathOpenUrl http://www.ma.utexas.edu/users/wfs/foo/t1.om
}
proc tempName { name extension } {
    set count [pid]
    while { [file exists $name[incr count].$extension] } { list }
    return $name$count.$extension
}

proc ws_outputToTemp { string file ext encoding } {
    upvar 1 $string result
    set tmp [tempName $file $ext ]
    set open $tmp
    if { [lsearch {x-gzip x-compress}  $encoding] >= 0 } { lappend dogzip |gzip -dc > $open ; set open $dogzip}
    set fi [open $open w]
    fconfigure $fi -translation binary
    puts -nonewline $fi $result
    flush $fi
    close $fi
    return $tmp
}

global debugParse
if { ![info exists debugParse ] } {
set debugParse 0
}

proc OpenMathOpenUrl { name args} {
    global ws_openMath
    #puts "OpenMathOpenUrl  $name $args "
    set history "" ; set historyIndex -1 ;set currentUrl ""
    set prevwindow ""
    set commandPanel [assoc -commandpanel $args ]
    if { "$commandPanel" == "" } {
	linkLocal . omPanel
	if { [info exists omPanel] } {
	    set commandPanel $omPanel
    }   }
    set toplevel [assoc -toplevel $args ""]
    set reload [assoc -reload $args 0]
    set post [assoc -post $args ""]
    #puts "post=$post"
    if { [winfo exists $commandPanel ] }  {
	makeLocal $commandPanel history historyIndex textwin
	set toplevel [winfo paren $commandPanel]
	if { "$toplevel" == "." } {set toplevel ""}
	# eval pack forget [winfo parent $textwin ]
	set prevwin [winfo parent $textwin]
	set currentUrl [oget $textwin currentUrl]
	catch { set currentUrl [decodeURL [oget $textwin baseurl]] }

	if { $reload == 0} {
	    
	    set new [resolveURL $name $currentUrl $post]
	    if { [set anchor [assoc anchor $new]] != "" } {
		set new [delassoc anchor $new]
	    }
	    set ii -1
	    foreach v $history { incr ii
	    if { "[delassoc post $new]" == "[delassoc post [oget $v.text currentUrl]]" } {
		# puts "new=$new\nold=[oget $v.text currentUrl]"
	    }
	    if   { "$new" == "[delassoc anchor [oget $v.text currentUrl]]" } {
		OpenMathMoveHistory $commandPanel [expr {$ii - $historyIndex }]
		if { "$anchor" != "" } {
		    update
		    catch {  $v.text yview anchor:$anchor }
		}
		
		#    OpenMathGetWindow $commandPanel $v
		#    pushHistory $commandPanel $v
		return
	    }
	    
	}
    } else {
	# reload=1
	list
    }
   }
   set count 5
   while { [incr count -1] > 0 } {
       set new  [resolveURL $name $currentUrl $post]
       set result [getURL $new contentType mimeheader $post]
       if { [set tem [assoc location $mimeheader]] == "" } { break }
       set name $tem
   }
       
   #puts "contentType defined:[info exists contentType]"
   set handler [assoc $contentType $ws_openMath(urlHandlers)]
   if { "$handler" != "netmath" && "$handler" != "" } {
       set tmp [ws_outputToTemp result netmath ps "[assoc content-encoding $mimeheader]"]
       # to do fix this for windows #####
       exec sh -c "[format $handler $tmp] ; rm -f $tmp" &
       return
   }
  #puts contentType=$contentType

  #puts "got [string length $result] bytes"
  #puts ", result= [string range $result 0 70] .."

   if { [catch { set baseprogram [oget $textwin baseprogram] }] } {
       set baseprogram [decodeURL [getBaseprogram]]
   }
   # puts "using  $baseprogram"
   if { $reload } {   forgetCurrent $commandPanel }

   #puts "ws_openMath(counter)=$ws_openMath(counter)"

   set win [mkOpenMath [set w $toplevel.t[incr ws_openMath(counter)]] ]
   
   #puts "ws_openMath(counter)=$ws_openMath(counter)"


   makeLocal $w commandPanel
   #puts "resolveURL $name $currentUrl"


   if { [set anchor [assoc anchor $new]] != "" } {
       set new [delassoc anchor $new]
   } 
   if { "[assoc filename $new]" == "" } {
       set new [putassoc  filename $new index.html]
   }
   # puts "...> $new"
   oset $w.text currentUrl $new
   oset $commandPanel location [encodeURL $new]
   oset $commandPanel textwin $win
   oset $w location  [encodeURL $new]
   # puts "new=$new" 
   oset $commandPanel savefilename [file root [toLocalFilename $new]].txt
   set tem [assoc filename $new ""]
   #puts $contentType
   if { "$contentType" != "text/html" } {
       if { [string match "image/*" $contentType] } {
	   set im [image  create photo -data $result]
	   $win image create 0.0 -image $im
	   set err 0
       } else {
       set err [catch {   $win insert 0.0 $result } ]
       }
   } elseif { 1 }  {
    xHMinit_win $win
    xHMset_state $win url [encodeURL $new]
    oset $win baseprogram $baseprogram    
    # puts win=$win,lengres=[string length $result]
    set errmsg1 ""
       set err 0
       global debugParse
       if { $debugParse } {
	   xHMparse_html $result "xHMrender $win"
	   set err 0
       } else {
	   set err [catch { 
	       xHMparse_html $result "xHMrender $win"
	   } errmsg1 ]
	   }
	catch {
	    if { "$anchor" != "" } {
		update
		$win yview anchor:$anchor
	}   }
	
	#   foreach v {Tresult Teval} {  $win tag raise $v}	   


    }    else {
###Never get here.. must change to make be the rich text case..	
	# drop comment lines
	regsub -all "(^|\n)#\[^\n\]*\n" $result \n result ;
	#puts input=$result
	
	# note netscape would just truncate the history
	# at historyIndex, and start to grow it there,
	# losing the record of all files you have visited after..
	# maybe we should do this.
	#puts "history=$history"
	set err [catch { insertRichText $win insert $result }]
    }
    if { $err == 0 } {
	pushHistory $commandPanel $w
    }
    if { $err } {
	#puts "======begin======"
	#puts $result
	#puts "======end========"
	puts "$errmsg1"
	error "unable to evaluate [encodeURL $new]:$errmsg1"}
	
    }


proc pushHistory { commandPanel win } {
    global [oarray $commandPanel]
    makeLocal $commandPanel history historyIndex
    if { [llength $history] == 0 } {
	oset $commandPanel historyIndex -1
    }
    if { "[lindex $history $historyIndex ]" != "$win" } {
	oset $commandPanel history [linsert $history [incr [oloc $commandPanel historyIndex]] $win]
    }
}


#
 #-----------------------------------------------------------------
 #
 # omScrollPage --  scroll the page by N pages, keeping the insert
 # cursor visible.
 #
 #  Results: none
 #
 #  Side Effects: page scrolls 
 #
 #----------------------------------------------------------------
#
proc omScrollPage { win n } {
    tkTextScrollPages $win $n
    set bbox [$win bbox insert]
    if { "" == "$bbox" } {
	if { $n > 0 } {
	    $win mark set insert @0,0
	} else {$win mark set insert @0,[$win cget -height]}
    }
}
	    
#bind Text <Control-v> "omScrollPage %W 1"
#bind Text <Meta-v> "omScrollPage %W -1"
#bind Text <Alt-v> "omScrollPage %W -1"

proc addTagSameRange { win oldtag newtag index } {
    if { [lsearch [$win tag names $index] $oldtag ] >= 0 } {
	set this [$win tag prevrange $oldtag $index+1char]
	if { "$this" != "" && [$win compare $index < [lindex $this 1]] } {
	     $win tag remove $newtag 0.0 end
	     $win tag add $newtag [lindex $this 0] [lindex $this 1]
	     $win tag raise $newtag
	}
    }
}

global xHMpreferences
set xHMpreferences(defaultservers) { nmtp://genie1.ma.utexas.edu/ nmtp://linux51.ma.utexas.edu/ nmtp://linux52.ma.utexas.edu/ }

if { "[info var embed_args]" != "" } {
     set xHMpreferences(defaultservers) nmtp://genie1.ma.utexas.edu/
 }

proc getBaseprogram { } {
    global xHMpreferences
    lindex  $xHMpreferences(defaultservers) 0
}

proc fileBaseprogram { textwin parent x y } {
    set e $textwin.e
    catch { destroy $e }
    set x [expr {[winfo rootx $parent] + $x +30 - [winfo rootx $textwin]} ]
    set x 30
    set y [expr {[winfo rooty $parent] + $y - [winfo rooty $textwin]} ]
    global xHMpriv
    set xHMpriv(baseprogram) [encodeURL [oget $textwin baseprogram]]
    entry $e -width 40 -textvariable xHMpriv(baseprogram)
    place $e -in $textwin -x $x -y $y
    raise $e
    set com "destroy $e ; oset $textwin baseprogram \[decodeURL \$xHMpriv(baseprogram)] "
    bind $e <Leave> $com
    bind $e <Return> $com
    
}

######### font choosing utilities #########

if { "$tcl_platform(platform)" == "unix" } {
    array set isFixedp {
fixed 1 {fangsong ti} 1 {clearlyu alternate glyphs} 0 lucidatypewriter 1 charter 0 lucidabright 0 times 0 ming 1 {lucidux sans} 0 {open look glyph} 0 {song ti} 1 newspaper 0 helvetica 0 {open look cursor} 1 li 1 mincho 1 {clearlyu ligature} 0 {clearlyu pua} 0 {lucidux mono} 1 courier 1 clearlyu 0 utopia 0 lucida 0 nil 1 clean 1 terminal 1 kai 1 gothic 1 cursor 0 symbol 0 {clearlyu arabic extra} 0 {lucidux serif} 0 {new century schoolbook} 0 song 1
}
}

proc fontDialog { top } {
    global xHMpreferences 
    set font [xHMmapFont font:propor:normal:r:3]
    catch { destroy $top }
    toplevel $top
    wm iconify  $top
    set win $top.text
    text $win -font [list [font config $font -family] [font config $font -size]] -height 20
    wm deiconify $top
    foreach fam {propor fixed} {
	set lis ""
	set i 0
	while { $i <= 8 } {
	    lappend lis [expr {$i - 3}]
	    incr i
	}
	if { "$fam" == "fixed" } { set fixed 1 } else { set fixed 0}
	mkLabelListBoxChooser $win.size$fam "list $lis" xHMpreferences($fam,adjust)
	mkLabelListBoxChooser $win.family$fam "getFontFamilies $fixed " xHMpreferences($fam)
	set fo [xHMmapFont "font:$fam:normal:r:3"]
	catch { set xHMpreferences($fam) [assoc -family [font actual $fo]]}
    }
    $win insert insert "Font Settings\nThe proportional font is "
    $win window create insert -window $win.familypropor
    $win insert insert "with a size adjustment of "
    $win window create insert -window $win.sizepropor
    $win insert insert "\nThe proportional fixed font is "
    $win window create insert -window $win.familyfixed
    $win insert insert "with a size adjustment of "
    $win window create insert -window $win.sizefixed
    $win insert insert "\n"
    $win insert insert "Default nmtp servers  "
    global _servers
    set _servers $xHMpreferences(defaultservers)
    entry $win.entry -textvariable _servers -width 40
    $win window create insert -window $win.entry
    $win insert insert "\n\n"
    global ws_openMath
    $win insert insert "http Proxy host and port:"
    entry $win.entryproxy  -width 40
    catch { $win.entryproxy insert 0 $ws_openMath(proxy,http) }
    $win window create insert -window $win.entryproxy
    $win insert insert "\nIf you are behind a firewall enter the name of your http proxy host and port,\n eg: `foo.ma.utexas.edu 3128', otherwise leave this blank"
    global xHMpreferences
    set men [tk_optionMenu $win.plottype xHMpreferences(plotwindow) embedded separate multiple ]
    $win insert insert "\nShould plot windows be "
    $win window create insert -window $win.plottype
    $win insert insert "?"
    
    
    $win insert insert "\n\n\n"
    $win insert insert " Apply and Quit " "bye raised"
    $win insert insert "      "
    $win insert insert " Apply " "click raised"
    $win insert insert "      "
    $win insert insert " Cancel " "cancel raised"
    proc _FontDialogApply { win } {
	global xHMpreferences _servers ws_openMath
	set xHMpreferences(defaultservers) $_servers
	catch {xHMresetFonts .}
	if { [llength [$win.entryproxy get]] == 2 } {
	    set ws_openMath(proxy,http) [$win.entryproxy get]
	}
    }
    $win tag bind click <1> "_FontDialogApply $win"
    $win tag bind bye <1> "_FontDialogApply $win ; destroy $top"
    $win tag bind cancel <1> "destroy $top"
    $win tag configure raised -relief raised -borderwidth 2
    $win insert insert "      "
    $win insert insert "Save Preference" "save raised"
    $win tag bind save <1> "_FontDialogApply $win ; savePreferences"

    pack $win
#    place $win -in [oget [omPanel .] textwin] -x 10 -y 10
}
proc savePreferences {} {
    global xHMpreferences ws_openMath
    set fi [open  "~/netmath.ini" w]
    puts $fi "array set xHMpreferences {"
    foreach {k v} [array get xHMpreferences *] {
	lappend all [list $k $v]
    }
    set all [lsort $all]
    foreach v $all { puts $fi $v }
    puts $fi "}"
    if { [info exists ws_openMath(proxy,http)] && [llength $ws_openMath(proxy,http)] == 2   } {
	puts $fi [list array set ws_openMath [array get ws_openMath proxy,http]
	]
    }
    close $fi
}
    
    

    
    
    
proc getFontFamilies { fixed } {
    global isFixedp
    foreach font  [font families] {
	if { ![info exists isFixedp($font)] } {
	    set isFixedp($font) [font metrics [list $font] -fixed]
	}
	if { $isFixedp($font) == $fixed } {
	    lappend answer $font
	}
    }
    return [lsort $answer]
}
	


#
 #-----------------------------------------------------------------
 #
 # mkLabelListBoxChooser --  creates a button called WIN with textvariable
 #  $TEXTVAR.  When clicked on the WIN, brings down
 #  a list of items, and clicking on one of them selects that item. and
 #  resets $TEXTVAR
 #
 #  Results: none
 #
 #  Side Effects: the TEXTVAR value is changed, and so consequently the label.
 #
 #----------------------------------------------------------------
#
proc mkLabelListBoxChooser { win items  textvar} {
    button $win -textvariable $textvar -command "listBoxChoose $win [list $items] $textvar"
}
proc listBoxChoose { win  items textvar  } {
    global xHMpreferences
    set whei [winfo height $win]
    set items [eval $items]
    set hei [llength $items]
    set fr ${win}frame
    frame ${win}frame
    set list $fr.list
    set scroll $fr.scroll
    scrollbar $scroll -command "$list yview"
    listbox $list -yscroll "$scroll set" -setgrid 1 -height 8
    pack $scroll -side right -fill y
    pack $list -side left -expand 1 -fill both
    set wid 0
    foreach v $items {
	set xx [string length $v] ;
	set wid [expr {($xx > $wid ? $xx : $wid)}]
    }
    eval [concat $list insert 0 $items]
    catch { $list selection set [lsearch $items [set $textvar]] } 
    bind $list <1> "set $textvar \[$list get \[$list nearest %y\]\]; destroy $fr"
    place $fr -in $win -x 0  -y 0 -anchor n
}


proc quoteForRegexp { s } {
    regsub -all {[\]\[$+()\\.?*]} $s {\\\0}  ans
    return $ans
}

    
proc mkOpenMathEditButtons { win } {
    maxima
    octave
    pari
    bold
    italic
    setfont
    ..

    }


## endsource browser.tcl
## source wmenu.tcl

###### wmenu.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

# implement a menu bar without toplevel windows.
# wet

proc wmenubar { name  } {
    if { "[string index $name 0]" == "." } {
	frame $name
	# puts "rename $name $name-orig"
	rename $name $name-orig
	set top [winfo toplevel $name]
	oset $top helpwin "" 
	proc $name { option args } "wmenubarInternal $name \$option \$args"
	set parent [winfo parent $name]
	# maybe change this to do traversal toward side leaving on..
	oset $name items ""
    } else {error "needs a window name arg"
    }
}

    
proc eswitch { key lis } {
    foreach {k act} $lis { lappend allowd $k}
    lappend lis default "error $key must be  one of: $allowd"
    uplevel 1 switch $key  [list  $lis]
}

proc ogetr { win var dflt } {
    set w $win
    while { 1 } {
	if { 0 == [catch { set val [oget $w $var] }] } {
	    return $val
	}
	global [oarray $w]
	# puts w=$w,[array get [oarray $w]]
	set w [winfo parent $w]
	if { "$w" == "" } {return $dflt}
    }
}

proc deleteHelp { win } {

   linkLocal $win helpPending
   if { [info exists  helpPending] } {
	after cancel $helpPending
	unset helpPending
    }
    set top [winfo toplevel $win]
    set helpwin [oget $top helpwin]
   if { "$helpwin" != ""} {
       place forget $helpwin 
   }   
}
    
proc setHelp {win  help args } {
   # set c [ogetr $win c "cant"]
    if { "$help" == "" } {set help "This is a menu window $win"}
    set enter ""
    set exit ""
    if  { [catch { set current [$win cget -relief] } ] || "$current" \
	    != "flat" } {
	set enter ""
	set exit ""
    } else { set enter "$win configure -relief raised" ;
	set exit "$win configure -relief $current"
    }
    # puts "current=$current"

    bind $win <Enter> "$enter; showHelp $win  {$help} $args"
    bind $win <Leave> "$exit; deleteHelp $win"
}

global show_balloons
set show_balloons 1


#
 #-----------------------------------------------------------------
 #
 # showHelp --  for WINDOW show a HELP message using ANCHOR positions.
 #  WINDOW may be a window or a rectangle specifier: x,y,wid,height 
 #  ANCHOR positions may be either n,w,e,s,nw,ne,se,sw,center or
 #  one of these followed by two floating point numbers indicating
 #  the fraction of the width and height of the window one is away from
 #  the upper left x,y of the window.
 #  Results: none
 #
 #  Side Effects: display a window. 
 #
 #----------------------------------------------------------------
#
proc showHelp { win help args } {
    global show_balloons helpwin
    if { $show_balloons == 0 } {
	catch { place forget $helpwin }
	return
    }
    linkLocal [lindex $win 0] helpPending
    set helpPending [after 1000 [list showHelp1 $win $help $args]]
}

proc showHelp1 { win help args } {
    global  tk_version
    set top [winfo toplevel [lindex $win 0]]
#    set anchors $args
#    append anchors "  w  e s ne n sw nw"
#    set anchors " nw"
#    set anchors "w e n {nw .2 1.2} {ne .8 1.2} s se"
#     set anchors "w e n {nw .2 1.2} {ne .8 1.2} s se"
    set anchors "sw w e n {nw .2 1.2} {ne .8 1.2} s se"
    makeLocal $top helpwin
    if { "$helpwin" == "" } {
	set tt $top
	if { "$tt" == "." } {set tt ""}
	set helpwin $tt.balloonhelpwin
	if { ![winfo exists $helpwin] } {
	
	    label $helpwin -width 0 -height 0  -borderwidth 1 \
		    -background beige -padx 4 -pady 4 -justify left
	}
	if { $tk_version < 8.0 } {
	    $helpwin config -relief ridge -borderwidth 2
	} else { $helpwin config -relief solid }
	
	oset $top helpwin $helpwin
    }
    if { [string first _eval $help ] == 0 } {
	catch { set help [eval [concat list [lindex $help 1]]]}
    }
    
    $helpwin configure -text $help -wraplength [expr {round(.34 * [winfo width $top])}]
    global anchorPositions
    if { [llength $win] == 5 } {
	desetq "win wx wy wxdim wydim" $win
    }  else {
	set wx [expr {[winfo rootx $win ] - [winfo rootx $top]}]
	set wy [expr {[winfo rooty $win ] - [winfo rooty $top]}]
    set wxdim [winfo width $win]
    set wydim [winfo height $win]
    }
    set nxdim [winfo reqwidth $helpwin]
    set nydim [winfo reqheight $helpwin]
    set topxdim  [winfo width $top]
    set topydim  [winfo height $top]
    global anchorPositions
    foreach an $anchors {
	if {[llength $an] == 3} {
	    desetq "an rx ry" $an
	} else {
	desetq "rx ry" [lsublis { {0 1.1 } {1 -.1}} $anchorPositions($an)]
	}
	# puts "rx=$rx,ry=$ry"
	set yoff [expr { $ry > 1 ? 8 : $ry < 0 ? -8 : 0 } ]
	desetq "x y" [getPlaceCoords 0 $yoff $rx $ry $an $wx $wy $wxdim $wydim $nxdim $nydim]
	# puts "for $win $an rx=$rx,ry=$ry x=$x,y=$y :[expr {$x >5}],[expr {$y > 5}],[expr {$x+$nxdim < $topxdim}],[expr {$y +$nydim < $topydim}]"
	if { $x > 5 && $y > 5 && $x+$nxdim < $topxdim && \
		$y +$nydim < $topydim } {
	    place forget $helpwin

    	    place $helpwin -x $x -y $y -anchor nw
	    raise $helpwin
	    return
	}
    }
}

proc wmenubarInternal { win  option  lis } {
  # puts "{wmenubarInternal $win $option $lis}"
    set key [lindex $lis 0]
    set lis [lrange $lis 1 end]
    eswitch $option {
	add {
	    set parent [winfo parent $win]
	    if { "$parent" == "."} {set parent ""}
	    set men [assoc -menu $lis $parent.item[llength [oget $win items]]]
 	    bindAltForUnderline $key "wmenuPost $key"
	    frame $men -relief raised -borderwidth 2p
	    setHelp $key [assoc -help $lis] n nw ne
	    rename $men $men-orig
	    set body "wmenuInternal $key \$option \$args"
	    proc $men {option args } $body
	    pack $key -in $win -side left -expand 1 -fill both
	    global [oarray $win]
	    lappend [oloc $win items] $key
	    oset $key menu $men
	    oset $men items ""
	    oset $key parent $win
	    bind $key <Button-1>  {wmenuPost %W}
	    return $men
	}
	configure {
	    return [eval $win-orig configure $key $lis]

	}
	invoke {
	    set w [lindex [oget $win items] $key]
	    wmenuPost $w
	}
	cget {
	    return [eval $win cget $key $lis]
	}
    }
}

proc getSomeOpts { opts lis } {
    set answer ""
    foreach {ke val } $lis {
	if { [lsearch $opts $ke] >= 0  } {
	    lappend answer $ke $val
	}
    }
    return $answer
}

proc excludeSomeOpts { opts lis } {
    set answer ""
    foreach {ke val } $lis {
	if { [lsearch $opts $ke] < 0  } {
	    lappend answer $ke $val
	}
    }
    return $answer
}

proc lsublis { subs lis } {
    foreach v $subs {
	set key [lindex $v 0]
	while { [set i [lsearch $lis $key]] >= 0 } {
	    if { [llength $v] > 1 } {
		set lis [lreplace $lis $i $i [lindex $v 1]]
	    } else {
		set lis [lreplace $lis $i $i]
	    }
	}
    }
    return $lis
}

proc wmenuInternal {win option  olist } {
    set key [lindex $olist 0]
    set lis [lrange $olist 1 end]
    makeLocal $win menu parent 
    makeLocal $menu items
    eswitch $option {
	add {
	    if { [catch {set counter [oget $menu counter] }] }   {
		set counter 0
	    }
	    oset $menu counter [incr counter]
	    # set new to be the new menu item window
	    # set com to be the command for 'invoke' to invoke.
	    set opts [excludeSomeOpts "-textvariable -image -label -underline -help" $lis]
	    set labopts [lsublis {{-label -text}} \
		    [getSomeOpts "-image -label -textvariable -underline" $lis]]
	    append labopts " -justify left -anchor w"
	    eswitch $key {
	    radio {
		set new $menu.fr$counter
		frame $new -borderwidth 1
		# puts "new=$new"
		apply label $new.label $labopts
		pack $new.label -side left -fill x
        	set opts [lsublis {{-radiovariable -textvariable}} $opts]
		apply radiobutton $new.radio $opts
		pack $new.radio -side right -anchor e
		set com "$new.radio invoke"
	    }
	    check {
		set new $menu.fr$counter
		frame $new -borderwidth 1
		# puts "new=$new"
		apply label $new.label $labopts
		pack $new.label -side left
        	set opts [lsublis {{-checkvariable -textvariable}} $opts]
		apply checkbutton $new.check $opts
		pack $new.check -side right
		# puts "$var --> $val"
		set com "$new.check invoke"
	    }
	    command {
		set com [assoc -command $lis]
		set new $menu.fr$counter
		frame $new -borderwidth 1
		apply label $new.label $labopts
		pack $new.label -in $new -side left
		# puts "bind $new.label <Button-1> $com"
		bind $new.label <Button-1> $com
		bind $new <Button-1> $com
	    }
	    window {
		set new [assoc -window $lis]
		set com [assoc -command $lis list]
	    }
	    entry {
		set new $menu.fr$counter
		frame $new -borderwidth 1
		apply label $new.label $labopts
		set opts [lsublis {{-entryvariable -textvariable}} $opts]
		apply entry $new.entry $opts
		pack $new.label -side top -in $new -anchor w
		pack $new.entry  -side top -in $new
		set com "focus $new.entry"
	    }
	    separator {
		set new $menu.sep$counter
		frame $new -height 4
		propagate $new 0
		set com ""
	    }
	    
	}
	bindAltForUnderline $new.label "$menu invoke $new"
	pack $new -in $menu -side top -fill x -expand 1
	oset $menu items [lappend items $new]
	oset $menu command$new $com
	setHelp $new [assoc -help $lis] w e
	return $new
    }
    configure {
	return [eval $win configure $key $lis]
    }
    invoke {
	makeLocal $menu items
	if { ![winfo exists $key] }  {
	    # it is an index
	    set key [lindex $items $key]
	}
	eval [oget $menu command$key]
	return
    }
    post {
	
        place $menu -anchor nw -relx 0 -rely 0 -bordermode outside -in $win
	bind $menu <Leave> "place forget $menu"
	focus $menu
	#bind $menu <FocusIn> "puts focus in"
	#bind $menu <FocusOut> "puts {leave for focus  menu}"
	raise $menu
    }
  }
}

proc wmenuPost { win } {
    makeLocal $win parent menu
    bind $menu <Leave> "place forget $menu"
    place $menu -anchor nw -relx 0 -rely 1.0 -bordermode outside -in $win
    raise $menu
}

proc bindAltForUnderline { item command } {
    set ind -1
    catch { set ind [$item cget -underline] }
    if { $ind >= 0 } {
	set letter [string index [$item cget -text] $ind]
	set to [winfo toplevel $item]
	bind $to <Alt-Key-$letter> $command
    }
}
	
proc showSomeEvents { win } {
    foreach v { Enter FocusIn FocusOut Visibility Leave} {  bind $win <$v> "puts {$win $v %x %y}"}
}

array set anchorPositions {
    n {.5 0} nw { 0 0 } se {1 1} e {1 .5} center {.5 .5}
    s { .5 1} sw { 0 1} w { 0 .5} ne { 0 1} }
    
proc getPlaceCoords { x y relx rely anchor xIn yIn xdimIn ydimIn xdim ydim } {
    global anchorPositions
    # puts "xIn=$xIn,yIn=$yIn,xdimIn=$xdimIn,ydimIn=$ydimIn,xdim=$xdim,ydim=$ydim"
    set x1 [expr {$x + $xIn+$relx * $xdimIn}]
    set y1 [expr {$y + $yIn+$rely * $ydimIn}]
    desetq "fx1 fy1" $anchorPositions($anchor)
    set atx [expr {$x1 - $fx1*$xdim}]
    set aty [expr {$y1 - $fy1*$ydim}]
    return [list $atx $aty]
}

## endsource wmenu.tcl
# source tryftp1.tcl
## source tryftp2.tcl

###### tryftp2.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################
if { "[info commands vwait]" == "vwait" && "[info commands myVwait]" == "" } {
  proc myVwait { x  } {uplevel 1  vwait $x }
}

proc submitFtp { viahost host name password directory filename} {
    global ftpInfo 

    if  { [catch { set sock [socket $viahost 80] } ] } {
	set sock [socket $viahost 4080]
    }
    set ftpInfo($sock,done) 0
    set len [string length $ftpInfo(data)]
    set ftpInfo($sock,data) $ftpInfo(data)
 
    # set sock [open /tmp/jim w+]
    fconfigure $sock -blocking 0 -translation {lf lf}
    # global billy ;lappend billy [list [fconfigure $sock]]
    puts $sock "POST /cgi-pub/wfs/submitftp HTTP/1.0"
    puts $sock "MIME-Version: 1.0"
    puts $sock "Accept: text/html"
    puts $sock "Accept: text/plain"
    puts $sock "Content-type: text/plain"
    puts $sock "Content-length: $len"
    puts $sock "Username: $name"
    puts $sock "Password: $password"
    puts $sock "Remote-host: $host"
    puts $sock "Remote-directory: $directory"
    puts $sock "Remote-filename: $filename"
    puts $sock ""
    flush $sock
    # puts $sock $ftpInfo(data) ; flush $sock
    # puts sock=$sock
    set ftpInfo(message) ""

    set after_id [after 10000 "set ftpInfo($sock,done) -1"]

    set ftpInfo($sock,datalength) $len
    set ftpInfo($sock,datanext) 0
    set ftpInfo($sock,log) "none.."
   # puts $sock $ftpInfo(data) ; flush $sock
    fileevent $sock writable "ftp2SendData $sock"
    fileevent $sock readable "ftp2WatchReturn $sock"
    myVwait ftpInfo($sock,done)
    set res $ftpInfo($sock,done)
    set ftpInfo(message) $ftpInfo($sock,log)

    #mike FIXME: this is a wrong use of after cancel
    after cancel $after_id

    # puts $ftpInfo($sock,return)
    ftp2Close $sock
    return $res
}

proc ftp2Close { sock } {
    global ftpInfo
    close $sock
    foreach v  [array names ftpInfo $sock,*]  {
	unset ftpInfo($v)
    }
}

proc ftp2WatchReturn { sock } {
    global ftpInfo 

    append ftpInfo($sock,return) " watching ..."
    set new [read $sock ]
    #global billy ; lappend billy [list return $new]
    if { [eof $sock] } {fileevent $sock readable {}}
    # puts "watching..new=$new" ; flush stdout
    append ftpInfo($sock,return) $new
    if { [regexp "Succeeded: (\[^\n]*)\n" $ftpInfo($sock,return) junk msg]} {
	set ftpInfo($sock,done) 1
	set ftpInfo($sock,log) $msg
    } elseif { [regexp "Failed: (\[^\n]*)\n" $ftpInfo($sock,return) junk msg] } {
	set ftpInfo($sock,done) -1
	set ftpInfo($sock,log) $msg
    }
    #mike FIXME: this is a wrong use of after cancel
    after cancel "set ftpInfo($sock,done) -1"
    after 3000 "set ftpInfo($sock,done) -1"
}
# set billy {}
proc ftp2SendData { sock } {
    global ftpInfo 
    
    set dn $ftpInfo($sock,datanext)
    set dl $ftpInfo($sock,datalength)
    #global billy ; lappend billy [list $dn $dl]
    set ftpInfo(percent) [expr {($dn >= $dl ? 100.0 : 100.0 * $dn/$dl)}]
    # puts "storing data to $sock $percent %"
    if { $ftpInfo($sock,datanext) >= $ftpInfo($sock,datalength) } {
	#mike FIXME: this is a wrong use of after cancel
	after cancel "set ftpInfo($sock,done) -1"
	after 10000 "set ftpInfo($sock,done) -1"
	fileevent $sock writable ""
	# puts $sock "abcdefghijklmno"
	# flush $sock
	return
    }
    set amtToSend 4000
    puts -nonewline $sock [string range $ftpInfo($sock,data) $ftpInfo($sock,datanext) [expr {$ftpInfo($sock,datanext) + $amtToSend -1}]]
    # puts  $sock $tosend
    flush $sock

    #mike FIXME: this is a wrong use of after cancel
    set ftpInfo($sock,datanext) [expr {$ftpInfo($sock,datanext) + $amtToSend}]
	after cancel "set ftpInfo($sock,done) -1"
    after 10000 "set ftpInfo($sock,done) -1"
}
    


## endsource tryftp2.tcl
## source myhtml.tcl

###### myhtml.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

# parsing routines for html
# try to be compatible from calling level with the package by stephen uhler.
# to use:
#  set html [exec cat /home/wfs/tclet/server/sample.html] ; xHMinit_win .t ; xHMset_state .t url sample.html ; xHMparse_html $html "xHMrender .t" ;     array set wvar $args
# source myhtml.tcl ; catch {destroy .t } ; text .t ;  set html [exec cat /home/wfs/tclet/server/sample.html] ; xHMinit_win .t ; xHMset_state .t url sample.html ; xHMparse_html $html "xHMrender .t" 

proc testit { file } {
    global xHMpriv
    source myhtml.tcl
    catch {destroy .t }
    foreach {k val} [array get xHMpriv geom*] {unset xHMpriv($k) }
    frame .t
    text .t.text
    set t .t.text
    set html [exec cat $file]
    xHMinit_win $t
    xHMset_state $t url $file
    xHMparse_html $html "xHMrender $t"
    pack .t
    pack $t
    raise .
}

#     
#     xHMparse_html $html "xHMrender .t"
# you can change the state of the parse engine by using
#    xHMset_state .t key1 val1 key2 val2...

#########

#  the HTML tags:

# becomes

# idea: some tags like font,indent,link have only one per but the tag
# varies..  others have a constant tag... eg 'strike' 'underline' ...
# or fill.  You cant have    
# and are either on or off...  
# have pushConstantTag win tag
# have popConstantTag win tag
# have pushNamedTag win name tag
# have popNamedTag win name tag   :sets current to be this one and pushes previous..
# and these maintain things so that
# [array names xHMtaglist$win] should provide the taglist to do

proc xHMpushConstantTag { win tag } {
    upvar #0 xHMtaglist$win taglist
    if { [catch {incr taglist($tag) } ] } {
	set taglist($tag) 1 }
}

proc xHMpopConstantTag {win tag} {
    upvar #0 xHMtaglist$win taglist
    catch {
	set i [incr  taglist($tag) -1]
	if { $i <= 0 } {unset taglist($tag) }
    }
}

proc xHMpushNamedTag {win name tag} {
     upvar #0 xHMvar$win wvar
    #puts "push $win <$name> <$tag>"
    if { [catch { set now [lindex [set wvar($name)] end] }] } {
	set now "" }
    lappend wvar($name) $tag
}

proc xHMpopNamedTag {win name} {
    upvar #0 xHMvar$win wvar
    set v [set wvar($name)]
    set now [lindex $v end]
    catch { set v [lreplace $v end end] }
    set wvar($name) $v
    return $now
}

proc xHMgetNamedTag {win tag } {
    upvar #0 xHMvar$win wvar
    set res ""
    catch  { set res [lindex $win($tag) end] }
    return $res
}
    
proc xHMpushAindent { win i } {
    upvar #0 xHMvar$win wvar
    upvar #0 xHMtaglist$win taglist
    set n [incr wvar(indent) $i]
    # puts "taglist:[array names taglist ]"
    unset taglist(indent:[expr {$n - $i}])
    set taglist(indent:$n) 1
}

proc xHMpopAindent { win i } {
    upvar #0 xHMtaglist$win taglist
    upvar #0 xHMvar$win wvar
    set n 0
    set n [set wvar(indent)]
    
    unset taglist(indent:$n)
    set n [expr {$n - $i}]
    if { $n < 0 } { set n 0 }
    set wvar(indent) $n
    set taglist(indent:$n) 1
    
}

# font and indent wil


#
 #-----------------------------------------------------------------
 #
 # defTag --  creates an executable scripts to invoke when the TAG
 #  or /TAG are encountered.
 #     -alter  takes a list of key1 val1 key2 val2
 #         generally these are pushed onto stacks for TAG and popped for /TAG
 #         the value of xHMtaglist$win  should get altered
 #     -before  set the prefix for text inserted for TAG
 #     -after   set the prefix for text inserted for /TAG
 #     -body   additional body to use for TAG
 #     -sbody   additional body to use for the /TAG
 #  The variables { tag  params text }  are bound when
 #  the BODY is evaluated.   Thus for example $text would get the
 #  text following the tag, and
 # 	set paramList [xHMsplitParams $params]
 #  could be used to decode the params. 
 # 
 #  Results: none
 #
 #  Side Effects: saves the script in xHMtag array under TAG and /TAG
 #
 #----------------------------------------------------------------
#
proc defTag { htag args } {
    global xHMtag
    foreach {key val } $args { set $key $val }
    if { [info exists -alter] } {
	foreach { key tag } ${-alter} {
	    if { [string match A* $key] } {
		append body "\nxHMpush$key \$win $tag"
		append sbody "\nxHMpop$key \$win $tag"
	    } elseif { [string match C* $key] } {
		append body "\nxHMpushConstantTag \$win $tag"
		append sbody "\nxHMpopConstantTag \$win $tag"
	    } else {
		append body "\nxHMpushNamedTag \$win $key $tag"
		append sbody "\nxHMpopNamedTag \$win $key"
	    }
	}
	array set toalter ${-alter}
	foreach prop { family size weight style} {
	    if { [info exists toalter($prop)] } { append fontprops " $prop"}
	}
	catch {
	    append body "\nxHMalterFont \$win $fontprops"
	    append sbody "\nxHMalterFont \$win $fontprops"
	}
    }
    catch { append body \n${-body} }
    catch { append sbody \n${-sbody} }
    catch { append body "\nset prefix \"[slashNewline ${-before}]\"" }
    catch {append sbody "\nset prefix \"[slashNewline ${-after}]\""  }
    catch { set xHMtag($htag) $body }
    catch { set xHMtag(/$htag) $sbody }
}
proc slashNewline { s } {
    regsub -all "\n" $s "\\n" s
    return $s
}
# netscape uses fonts in the following progression.
# we will have the font labels looking like:
#  font:propor:normal:r:4   to indicate size 4
# In an application if the user sets the default
# nfont:nfamily:nweight:nstyle:nsize
# where nfamily is in {propor,fixed}
# where nweight is in {normal,bold}
# where nstyle  is in {i,r}
# where nsize   is in {1,2,3,4,5,6,7}
# then we map the label to a particular font....
# propor-->times
# fixed->courier

# set the font to be what it would map to for X.
proc xHMsetFont { win fonttag  } {
    upvar #0 xHMvar$win wvar
    set fo [xHMmapFont $fonttag]
    set wvar($fonttag) 1
    $win tag config $fonttag -font $fo
}


    
#
#  #### We have legacy code from before the font command existed..
#  
    
if { "[info command font]" != "font" } {

 #convert a fonttag into an actual font specifier, using preferences.
 # mapping propor,fixed to font families, and dobing size adjusting based
 # on font type.
 proc xHMmapFont {  fonttag } {
    # font:family:weight:style:size
    global xHMpreferences
    set s [split $fonttag :]
    set fam [lindex $s 1]
    #puts "fam=$fam,fonttag=$fonttag"
    if { "$fam" == "" } {
	set fam propor
    }
    set si [expr {$xHMpreferences($fam,adjust) + [lindex $s 4]}]
    set si [expr {($si < 1 ? 1 : ($si > 8 ? 8 : $si))}]
#    set family $xHMpreferences([lindex $s 1])
#    set weight [lindex $s 2]
#    set style [lindex $s 3]
    return "-*-$xHMpreferences($fam)-[lindex $s 2]-[lindex $s 3]-normal-*-*-$xHMpreferences($fam,$si)0-*-*-*-*-*-*"
 }
 # reset fonts for a window taking into account current preferences.
 proc xHMresetFonts { win } {
    upvar #0 xHMvar$win wvar
    foreach fonttag [array names wvar font:* ] {
	xHMsetFont $win $fonttag }
    }


 proc xHMfontPointSize { string } {
#    expr round ([lindex [split [xHMmapFont font:fixed:normal:r:3] -] 8] / 10.0)
    set tem [lindex $string 1]
    if { [catch { expr { $tem +1} }] } {
	error "bad font $string"
    }
    return $tem
#    expr round ([lindex [split $string -] 8] / 10.0)
 }
	
 } else {    
    
#convert a fonttag into an actual font specifier, using preferences.
# mapping propor,fixed to font families, and dobing size adjusting based
# on font type.
 proc xHMmapFont {  fonttag } {
    # font:family:weight:style:size
    global xHMpreferences xHMfonts
    if { [info exists xHMfonts($fonttag) ] } {
	return $xHMfonts($fonttag)
    } else {
	set xHMfonts($fonttag) [set fo [font create]]
	xHMconfigFont $fonttag
	return $fo
	
    }
 }

 proc xHMconfigFont {  fonttag } {
    # font:family:weight:style:size
    global xHMpreferences xHMfonts
    set font $xHMfonts($fonttag)
    set s [split $fonttag :]
    set fam [lindex $s 1]
    #puts "fam=$fam,fonttag=$fonttag,s=$s"
    if { "$fam" == "" } {
	set fam propor
    }
    set si [expr {$xHMpreferences($fam,adjust) + [lindex $s 4]}]
    #set si [lindex $s 4]
    set si [expr {($si < 1 ? 1 : ($si > 8 ? 8 : $si))}]
    set family $xHMpreferences([lindex $s 1])
    set weight [lindex $s 2]
    set slant [lindex $s 3]
    if { "$slant" == "i" } { set slant italic} else {set slant roman}
    #puts "font config $font -family $family -size $xHMpreferences($fam,$si) -slant $slant -weight $weight"
    global tcl_platform
    if { "$tcl_platform(platform)" == "unix" } {
	set usePixel "-"
    } else { set usePixel "" }
    font config $font -family $family -size $usePixel$xHMpreferences($fam,$si) -slant $slant -weight $weight
    return
 }

 ### the following resets all the fonts
 ### for any windows now that font objects are interned

 proc xHMresetFonts { win } {
     global xHMfonts
     foreach v [array names xHMfonts] {
	 xHMconfigFont $v
     }
 }

proc xHMfontPointSize { string } {
    #mike FIXME: hard coded font name and $string is ignored
    set si [font config font2 -size]
    return [expr { $si < 0 ? - $si : $si }]
}
}



proc xHMalterFont {win args } {
    upvar #0 xHMvar$win wvar
    upvar #0 xHMtaglist$win taglist

#    puts "font:$args,[array get wvar *]"
    foreach v {family weight style size adjust}  {
	set $v [lindex $wvar($v) end]
    }

    set si $size
    if { [catch { set si [expr {$si + $adjust}] }] } {
	# puts "too many pops"
	return
    }
    set font font:$family:$weight:$style:$si
    if { ![catch { set fo $wvar(font) }] } {
	catch { unset taglist($fo) } }
#    puts "font=$font, wvar=[array get wvar fon*]"
    set  wvar(font) $font
    if { ![info exists wvar($font)] } {
	xHMsetFont $win $font }
    set taglist($font) 1
	
   # return "-*-$family-$weight-$style-normal-*-*-${size}0-*-*-*-*-*-*"
}

proc xHMsplitParams { param } {
    if { "$param" == "" } { return ""}
   set reg "(\[^= \t\n\]+)\[ \t\n]*((=\[ \t\n]*((\"(\[^\"\]*)\")|('(\[^'\]*)')|(\[^ \t\n\]*)))|(\[ \t\n\])|\$)"

   # set sub "{1=\\1,2=\\2,3=\\3,4=\\4,5=\\5,6=\\6,7==\\7,8=\\8,9=\\9}"
   # regsub -all $reg $param $sub  joe
   # puts joe=$joe
    
    set sub "\\1\\6\\8\\9"
    regsub -all $reg $param $sub  joe
    foreach { dummy key val } [lreplace [split $joe ] end end]  { lappend new [string tolower $key] $val}
    return $new
}

proc xHMextract_param {paramList  key args} {
    foreach { k val } $paramList {
	if { "$k" == "$key" } {
	    uplevel 1 set $key [list $val]
	return 1}}
	if { "$args" != "" } {
	    uplevel 1 set $key  [list [lindex $args 0] ]
	}
	return 0
    }

global xHMtag
catch {unset xHMtag}

defTag a -alter {Cdoaref doaref} -body xHMdo_a  -sbody xHMdo_/a 
defTag b -alter {weight bold }
defTag -body xHMdo_body 
defTag br -before "\n"
defTag center -alter {Ccenter center}
defTag cite -alter {style i} 
defTag code -alter {family fixed}
defTag dd -before "\n" -after "\n"
defTag dfn -alter {style i}
defTag dt -before "\n"
defTag em -alter {style i}
defTag h1 -alter {size 7 weight bold} -body {xHMassureNewlines 1} -after "\n"
defTag h2 -alter {size 6} -body {xHMassureNewlines 1} -after "\n"
defTag h3 -alter {size 6} -body {xHMassureNewlines 1} -after "\n"
defTag h4 -alter {size 5} -body {xHMassureNewlines 1} -after "\n"
defTag h5 -alter {size 4} -before "\n" -after "\n"
defTag h6 -alter {size 3 style i} -before "\n" -after "\n"
defTag i -alter {style i}
defTag img -body xHMdo_img

defTag kbd -alter {family fixed weight bold}
defTag li -body xHMdo_li 

defTag dl  -body xHMlistEnter -sbody xHMlistExit  
defTag dir  -body xHMlistEnter -sbody xHMlistExit 
defTag menu -body xHMlistEnter -sbody xHMlistExit 
defTag ol  -body {
    xHMlistEnter
    set wvar(listindex$wvar(indent)) 0} -sbody {
	xHMlistExit }	

defTag title  -body {wm title [winfo toplevel $win] $text ; set text ""} -sbody {list }
defTag ul -alter {Aindent 1} -body { xHMlistEnter
  set paramList [xHMsplitParams $params]
  set _iii -1
  if { [xHMextract_param $paramList type ""] } {
      set _iii [lsearch {disc circle square} $type]
  }
  if { $_iii < 0 } {
      set _iii [expr {($wvar(indent)/2 > 3 ? 3 : $wvar(indent)/2) -1 }]
     if { $_iii < 0 } { set _iii 0}
  }
  # push an index which will say disc, circle or square.
  xHMpushNamedTag $win ultype $_iii
}  -sbody { xHMlistExit ; catch { xHMpopNamedTag $win ultype }}
   

#defTag p -before "\n\n" -sbody {}
#defTag p -before "\n\n" -sbody {}
defTag p -body { xHMassureNewlines 1 } -sbody {}
defTag blockquote -before "\n\n" -after "\n"
defTag pre -alter {family fixed Cnowrap nowrap} -before "\n" /pre "\n"
defTag samp -alter {family fixed}
defTag strike -alter {Cstrike strike}
defTag strong -alter {weight bold}
defTag sup -alter {Csup sup}
defTag sub -alter {Csub sub}

defTag tt -alter {family fixed}
defTag u -alter {Cunderline underline}

defTag hrx  -body { $win insert $wvar(W_insert) "\n" ;
     $win insert $wvar(W_insert) "\n" hrule
    } -sbody {} 
defTag hr -before \n  -body { 
     $win insert $wvar(W_insert) "                  " underline
    } -sbody {} 

defTag var -alter {style i}

defTag hmstart -alter {	family propor   weight normal   style r   size 3
	list list 
        adjust 0 } -body { set wvar(counter) 0 }

defTag font -body {
    set paramList [xHMsplitParams $params]
    xHMpushNamedTag $win adjust [assoc size $paramList 0]
    xHMalterFont $win adjust
    }  -sbody {
	xHMpopNamedTag $win adjust
	xHMalterFont $win adjust
    }


proc notyet { args } 	{puts "not yet $args" }
defTag isindex -body xHMdo_isindex -sbody {}
defTag meta -body list -sbody list
defTag form  -before "\n" -after "\n"  -body {
    global xHMpriv
    set xHMpriv(form) [gensym form]
    upvar #0 $xHMpriv(form) form
    set paramList [xHMsplitParams $params]
    #puts "paramList=$paramList"
    if { [xHMextract_param $paramList action ""] } {
	set form(action) $action
    }
    xHMextract_param $paramList method "get"
    set form(method) $method

  } -sbody { global xHMpriv ;
    if { [info exists xHMpriv(form) ] } {
	upvar #0 $xHMpriv(form) form
	#puts form=$xHMpriv(form)
	#puts "form values=[array get form]"

	if { ![info exists form(f_has_submit)] } {
	    set params ""
	    xHMtextInsert $win "\n"
	    xHMdo_input submit
	}
	unset xHMpriv(form)
     }
    }
defTag input -body xHMdo_input
defTag select -body "xHMdo_input select" -sbody {
#    puts wvar=[array get wvar f_in_select]
    #catch {
    global xHMpriv
    upvar #0 $xHMpriv(form) form
    puts "\[array get wvar f_in_select*]=[array get wvar f_in_select*]"
    set na [lindex $wvar(f_in_select) 0]
     	
    set w $form(f_select,$na)
    foreach v [lrange $$wvar(f_in_select) 1 end] {
	$w.list insert end $v
    }
    xHMresetListbox $w $wvar(f_selected,$na)
    append form(f_reset) " ; xHMresetListbox $w [list $wvar(f_selected,$na)]"
    #puts $w
    if { [winfo exists ${w}label] } {
	#puts "have label $w and ${w}label"
	bind  ${w}label <1> "place $w -anchor center -relx 0 -rely 1.0 -bordermode outside -in ${w}label ; raise $w"
	bind  $w <Leave> "xHMresetListbox $w \[$w.list curselection\] ; place forget $w"
    }
    if { [$w.list cget -height] > 0  && [llength $wvar(f_select_values)] > [$w.list cget -height] } {
	scrollbar $w.scroll -orient v -command "$w.list yview" -takefocus 0
	$w.list configure -yscrollcommand "$w.scroll set"
	pack $w.scroll -side right -fill y
    }

    set form(f_select_list,$na) $wvar(f_select_values)
    if { [catch { unset wvar(f_selected,$na) }] } { puts "failed= unset wvar(f_selected,$na)"}
    if { [catch  { unset wvar(f_select_values) }] } { puts "failed=unset wvar(f_select_values)"} 
    #}
}

proc   xHMresetListbox  { w selected } {
    $w.list selection clear 0 end
    foreach v $selected { $w.list selection set $v}
    set i 0
    if { [llength $selected] > 0 } {
	set i [lindex $selected 0]
    }
    if { [winfo exists ${w}label] } {
	${w}label configure -text [$w.list get $i]
    }
}

defTag textarea -body "xHMdo_input textarea"
proc configColor { args } {
    set color [lindex $args end]
    if { [catch { eval $args } ] } {
	set color [lindex $args end]
	set args [lreplace $args end end "#$color"]
	catch { eval $args }
    }
}


defTag html -body "list " -sbody "list "
defTag head -body "list " -sbody "list "
defTag body -body {
    #puts "<body $params> $text"
     set paramList [xHMsplitParams $params]
    if { [xHMextract_param $paramList bgcolor ""] } {
	configColor $win config -background $bgcolor
	configColor $win tag  config hrule -font {courier 2} -background $bgcolor
    }
    if { [xHMextract_param $paramList baseprogram ] } {
        oset $win baseprogram [resolveURL $baseprogram [oget $win baseprogram]]
	oset $win baseprogram [decodeURL $baseprogram]
    }


    set _text $text
    if { [xHMextract_param $paramList text ""] } {
	 configColor $win config -foreground $text 
    }
    set text ${_text}
    foreach {ll tag} {evalrelief Teval resultrelief  Tresult aevalrelief currenteval resultmodifiedrelief Tmodified }  {
	if { [xHMextract_param $paramList $ll ""] } {
	    $win tag configure $tag -relief [set $ll]
	}
    }

    foreach {ll tag} {bgeval Teval bgresult Tresult bgresultmodified Tmodified bgaeval currenteval}  {
	if { [xHMextract_param $paramList $ll ""] } {
	      configColor $win tag configure $tag -background [set $ll] 
	}
    }
    foreach {ll tag} {link href alink currenthrefforeground eval Teval result Tresult resultmodified Tmodified aeval currenteval}  {
	if { [xHMextract_param $paramList $ll ""] } {
	configColor $win tag configure $tag -foreground [set $ll]
	}
    }
   } -sbody "list "
    
defTag base -body {       set paramList [xHMsplitParams $params]
   if { [xHMextract_param $paramList href ""] } {
       set wvar(baseurl) $href
      #xHMset_state $win baseurl $href
       oset $win baseurl $href
   }
  }
  
    

defTag option -body { set text [string trimright $text]
       set paramList [xHMsplitParams $params]
       xHMextract_param $paramList value $text
       lappend wvar(f_select_values) $value
       lappend wvar(f_in_select) $text
       if { [xHMextract_param $paramList selected] } {
	   #puts "hi==wvar(f_selected,[lindex $wvar(f_in_select) 0])"
	   lappend wvar(f_selected,[lindex $wvar(f_in_select) 0]) [expr {[llength $wvar(f_in_select)] -2}]
       }
       set text ""
}

global xHMpriv
set xHMpriv(counter) 0


#
 #-----------------------------------------------------------------
 #
 # ldelete --  remove all copies of ITEM from LIST
 #
 #  Results: new list without item
 #
 #  Side Effects: 
 #
 #----------------------------------------------------------------
#
proc ldelete { item list } {
    while { [set i [lsearch $list $item]] >= 0} {
	set list [lreplace $list $i $i]
    }
    return $list
}
if { ![info exists _gensymCounter] } {set _gensymCounter  0}
proc gensym { name } {
    global _gensymCounter
    incr _gensymCounter
    set var ${name}_${_gensymCounter} 
    catch { uplevel #0  unset $var}
    return $var
}

proc xHMdo_input {{type ""}} {
    global xHMpriv
    if { ![info exists xHMpriv(form)] } {
	set xHMpriv(form) [gensym form]
    }
    upvar 1 win win
    upvar #0 $xHMpriv(form) form
    upvar #0 xHMvar$win wvar
    upvar 1 params params
    set form(url) $wvar(url)

    set paramList [xHMsplitParams $params]

    set w $win.input[incr wvar(counter)]
#    bindtags $w [ldelete maxlength [bindtags $w]]
    xHMextract_param $paramList name ""
   if { "$type" == "" } {
    xHMextract_param $paramList type text
   }
    xHMextract_param $paramList value ""
    set value  [xHMconvert_ampersand $value]
    switch -regexp -- $type {
	{text$|password|int$|string} {
	    xHMextract_param $paramList size 20
	    entry $w -width $size
	    if { "$type" == "password" } { $w config -show * }
	    if { [xHMextract_param $paramList maxlength] } {
		bindtags $w [concat [bindtags $w] maxlength]
		bind maxlength <KeyPress> "xHMdeleteTooLong $win %W"
		
		set wvar($w,maxlength) $maxlength
	    }

	    $w insert end $value

	    append form(f_reset) " ; $w delete 0 end ; $w insert end [list $value] "
	    set form(f_submit,$name) "$w get"
	}
	select {
	    xHMextract_param $paramList size 1
	    xHMextract_param $paramList mode single
	    set lis $w
	    if { $size == 1 } {
		set w ${w}label
		label $w -relief raised
	    }
	    frame $lis
	    listbox $lis.list  -selectmode $mode -width 0 -exportselection 0 -height [expr {$size > 1 ? $size : 0}]
	    pack $lis.list -side left

	    # will contain list "window value1 value2 value3 .."
	    # added to by <option>
	    set wvar(f_selected,$name) ""
	    set form(f_select,$name) $lis
	    set wvar(f_in_select) $name
	    set wvar(f_select_values) $name
	    # throw away any text after select
	    set text ""  
	    
	}
	textarea {
	    upvar 1 text text
	    xHMextract_param $paramList cols 30
	    xHMextract_param $paramList rows 5
	    catch { 
	      frame $w
	      puts "w=$w"
	    scrollbar $w.yscroll -command "$w.text yview" -orient v
	    text $w.text -height $rows -width $cols -wrap none \
		    -yscrollcommand "$w.yscroll set"  -padx 2 -pady 2
	     $w.text insert 0.0 $text
	
	    set text ""
	    pack $w.text
	    set form(f_submit,$name) "$w.text get 0.0 end"
	    append form(f_reset) " ; $w.text delete 0.0 end ; $w.text insert end [list $text]"
	} errm ;
	    puts errm=$errm;
	    
	}
	image {

	    xHMextract_param $paramList width 0
	    xHMextract_param $paramList height 0
	    xHMextract_param $paramList src "broken.ppm"
	    set form(f_has_submit) 1
	    catch { set base $wvar(url) ; set base $wvar(baseurl) }
	    label $w -image [xHMgetImage $win $src $base $width $height] \
		    -background [$win cget -background]
	    bind $w <ButtonRelease-1>   "xHMdoSubmit $w $xHMpriv(form) {$name.x %x $name.y %y}"
	    bind $w <Return> "xHMdoSubmit $w $xHMpriv(form) {$name.x 0 $name.y 0}"
	    bind $w <Leave> "$w configure -relief raised"
	    
	    }
	radio {

	    if { [catch { set var $form(radio,$name) } ] } {
		set var [set form(radio,$name) [gensym radio_value]]
	    }
	    radiobutton $w -variable $var -value $value -text " "
	    if { [xHMextract_param $paramList checked] } {
		append form(f_reset) "; $w select"
		$w select
		
	    } else {
		append form(f_reset) "; $w deselect"
		$w deselect
		
	    }

	    set form(f_submit,$name) "uplevel #0 set $var"

	}
	checkbox {
	    ######### to do fix this..failed: http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Forms/example-4.html 
	    if { [catch { set var $form(checkbox,$name) } ] } {
		set var [set form(checkbox,$name) [gensym checkbox_value]]
	    }
	    xHMextract_param $paramList value on
	    checkbutton $w -on $value -variable $var -off _dontsubmit_ \
		    -text " "

	    set form(f_submit,$name) "uplevel #0 set $var"
	    
	    if { [xHMextract_param $paramList checked] } {
		append form(f_reset) " ; $w select"
		$w select;
	    } else {
		$w deselect
		append form(f_reset) " ; $w deselect"
	    }

	}
	hidden {
	    set form(f_submit,$name) "list  [list $value]"
	    set w ""
	}
	reset {
	    if { "$value" == "" } {set value "Reset"}
	    button $w -text $value -command "xHMdoReset $xHMpriv(form)"

	}
	submit {
	    set form(f_has_submit) 1
	    if { "$value" == "" } { set value "Submit Query" }
	    if { "$name" != "" } {
		button $w -text $value -command [list xHMdoSubmit $w $xHMpriv(form) [list $name $value]]
	    } else {
		button $w -text $value -command "xHMdoSubmit $w $xHMpriv(form) [list {}]"
	    }
	
	}
    }
#    if { [info exists form(f_submit,$name)] } {
#	lappend form(f_tosubmit) $name
#    }
    #dputs "type=$type,w=$w"
    #dputs "form(reset)=$form(f_reset)"
    if { "$w" != "" } {
	#catch { puts "class=[winfo class $w]" }
	if { [catch {   $win window create $wvar(W_insert) -window $w -align bottom -padx 1 -pady 1 } ] } {
	    puts "$w bad window ?"
	}
	
	### todo handle focus of forms.. with tabbing.
	
    }
    
}

proc xHMsetSubmitPosition { formvar name x y } {
    upvar #0 $formvar form
    set form(f_submit,$name.x) "list $x"
    set form(f_submit,$name.y) "list $y"
}
    
    

proc xHMdoReset { formVar } {
    upvar #0 $formVar form
    eval $form(f_reset)
}
proc xHMdoSubmit { w formVar nameVals } {
    upvar #0 $formVar form
    set ans ""
    set win [omPanel $w]
    foreach { name value } $nameVals {
	puts "value=$value--><[xHMencode_get $value]>"
	if { "$name" != "" } { append ans "&$name=[xHMencode_get $value]"}
    }

#    foreach name $form(f_tosubmit) {
#	set val [eval $form(f_submit,$name)]
#	if { "$val" != "_dontsubmit_" } {
#	    append ans "&$name=[xHMencode_get $val]"
#	}
#    }
    set n [string length f_submit,]
    foreach {name value}  [array get form f_submit,* ] {
	 puts "form submit:[array get form f_submit,*]"
	set val [eval $value]
	puts "name=$name,val=$val-->[xHMencode_get $val]"
	if { "$val" != "_dontsubmit_" } {
	append ans "&[string range $name $n end]=[xHMencode_get $val]"
	}
    }
    # do the select listboxes:
    
    foreach { name w } [array get form f_select,*] {
	set name [string range $name [string length f_select,] end]
	
	set values [lrange $form(f_select_list,$name) 1 end]
	set ans1 ""

	foreach v [$w.list curselection] {
	    lappend ans1 [lindex $values $v]
	}
	puts w=$w.list,name=$name,ans1=$ans1,
	set ans1 [join $ans1 " "]
	append ans "&$name=[xHMencode_get $ans1]"
    }
    #puts ans=$ans
    #puts form=[array get form]
    set action $form(action)
    if { "[string tolower $form(method)]" == "get" } {
	xHMfindUrl $win $form(method) $form(action)?[string range $ans 1 end]
    } else {
	xHMfindUrl $win $form(method) $form(action) [string range $ans 1 end]
}   }

proc xHMfindUrl { win method  url { body "" }} {
    #puts "$win,$method,$url,$body"
    set method "[string tolower $method]"
    if { "$method" == "get" } {
	OpenMathOpenUrl $url -commandpanel $win
    } elseif { "$method" == "post" } {
	if { "$body" == "" } {set body " "}
	OpenMathOpenUrl $url -commandpanel $win -post $body
    }
}

proc xHMdeleteTooLong { win w } {
    upvar #0 xHMvar$win wvar
    catch { $w delete $wvar($w,maxlength) end }
    #puts $wvar($w,maxlength)
}

proc xHMconvert_ampersand { text } {
    if {![regexp & $text]} {return $text}
    regsub  -all {([[\\])|(&((#([0-9][0-9]?[0-9]?))|([a-zA-Z]+));?)} $text {[xHM_do1 \\\1  \5 : \6]} tmp
    return [subst -novariables $tmp]
}

proc xHM_do1 { a b {c xx} } {
    global isoLatin1
   if { "$a" == " " } {
      if { "$b" == ":" } {
	  #set result ?
	  if { [catch { set result $isoLatin1($c) }] } {
	     return "&$c"
	  }
	  return $result
      }    else {
      return [format %c $b] }
   } else { return [string index $a 0] }
}

proc xHMdo_li {} {
    uplevel 1 {
	set i $wvar(indent)
	set taglist(listindex) 1
	set text [string trimleft $text]
	if { ![catch { incr wvar(listindex$i) }] } {
	    xHMpopAindent $win 1
	    xHMtextInsert $win "\n\t$wvar(listindex$i).\t"
	    xHMpushAindent $win 1
	} else {
	    set ii 0
	    catch { set ii [lindex $wvar(ultype) end] }
	    xHMpopAindent $win 1
	    xHMtextInsert $win "\n\t"
	    xHMinsertBullet $win $ii 
	    xHMtextInsert $win "\t"
	    xHMpushAindent $win 1
	}
    unset  taglist(listindex)
 }
}

proc xHMinsertBullet { win i } {
    global xHMulBMPdata xHMpriv
    upvar #0 xHMvar$win wvar
    set fg [$win cget -foreground]
    set image ""
    if {[catch { set image $xHMpriv(ul,$fg,$i) }] } {
	catch { set image [set xHMpriv(ul,$fg,$i) [image create bitmap -data [lindex $xHMulBMPdata $i] -foreground $fg]] }
    }
    # if we cant get the image, or cant insert it fall back to
    # inserting a simple character
    if { "$image" == "" || [catch { $win  image create $wvar(W_insert) -image $image } ] } {
	if { $i > 2 } { set i 2}
	$win tag configure listindex -foreground red		
	xHMtextInsert $win [string range "oo*" $i $i]
    }
}

defTag th -body list
defTag td -body list
defTag tr -body list




    
proc xHMdo_a  {} {
   uplevel 1  {
       set paramList [xHMsplitParams $params]
       if { [xHMextract_param $paramList href] } {
	   # in case they forget </a>
	   foreach v [array names taglist h:*] {
	       unset taglist($v)
	   }
	   $win tag bind h:$href <Enter> "HMdoaref enter $win %x %y"
	   $win tag bind h:$href <Leave> "HMdoaref leave $win %x %y"
	   $win tag bind h:$href <1> "HMdoaref click $win %x %y"
	   set taglist(h:$href) 1
	   set taglist(href) 1
	   
       }
       if { [xHMextract_param $paramList name] } {
	   $win mark set anchor:$name "$wvar(W_insert) -1 chars"
	   $win mark gravity anchor:$name left
	   }
       }
}

proc xHMdo_/a  {} {
    uplevel 1 {
	foreach v [array names taglist h:*] { unset taglist($v) }
	catch {unset taglist(href)}
    }
}
    
proc xHMdo_body { win } {
    global xHMOptions
    upvar 1 params params
    upvar #0 xHMvar$win wvar
    set paramList [xHMsplitParams $params]
    foreach {key val } $paramList {
	catch { $win config -$key $val }
	set wvar(option,$key) $val
    }
}

proc xHMdo_img {} {
    upvar 1 params params
    upvar 1 wvar wvar
    upvar 1 taglist taglist
    upvar 1 win win
    set paramList [xHMsplitParams $params]

    xHMextract_param $paramList align bottom
    xHMextract_param $paramList border 1
    xHMextract_param $paramList width 0
    xHMextract_param $paramList height 0
    xHMextract_param $paramList src ""
#    xHMextract_param $paramList alt <image:[file tail $src]>
    xHMextract_param $paramList alt <image:$src>
    #puts "img:$src,$alt,$width,$height"
    if { [lsearch {bottom top center} $align ] < 0 } { set align bottom}
	set w $win.fr[incr wvar(counter)]
    set base ""
    set bg [$win cget -background]

    catch { set base $wvar(url) ; set base $wvar(baseurl) }
    if { [catch { set im [xHMgetImage $win $src $base $width $height] }] } {
	error "dont get here now"
	frame $w -width $width -height $height -background $bg
	label $w.label -text $alt -background $bg
	if { $width && $height } { pack  propagate  $w 0 }
	pack $w.label -fill both -expand 1
    } else {
	if { $wvar(measure) >= 0 } {
	    incr wvar(measure) [image width $image]
	}
	label $w -image $im -background $bg
	bind $w <Enter> [list set ws_openMath(load_rate) "$alt" ]
	bind $w <Leave> [list set ws_openMath(load_rate) ""  ]

    }
    catch { $w configure -border $border}
    set href [lindex [array names taglist h:*] 0]
    if { "$href" != "" }  {
	bind $w <1> "OpenMathOpenUrl [string range $href 2 end] \
			-commandpanel [omPanel $win]"
    }
    foreach v [array names taglist] { $win tag add $v $wvar(W_insert)}
    $win window create $wvar(W_insert) -window $w -align $align -padx 1 -pady 1
    
    
## to do add links for call backs
}
    
# return an image object..
proc xHMgetImage {win src baseurl width height } {
#     puts "$win,$src,$baseurl,$width,$height"
#     puts "getImage [resolveURL $src [decodeURL $baseurl]] $width $height"
    return [getImage [resolveURL $src [decodeURL $baseurl]] $width $height]
}

proc xHMget { url } {
}

proc xHMlistEnter {} 	{
    uplevel 1 {
	xHMassureNewlines [expr {($wvar(indent) < 2 ?  1 : 0)}]
	set _ii [expr {(($wvar(indent) <= 0  ) ? 2 : 1)}]
	xHMpushAindent $win $_ii
	catch { unset wvar(listindex$wvar(indent))}
    }
}

proc xHMlistExit {} 	{
    uplevel 1 {
	set _ii [expr {($wvar(indent) <= 2) ? 2 : 1}]
	xHMpopAindent $win $_ii
	xHMassureNewlines [expr {($wvar(indent) < 2 ?  1 : 0)}]
	
    }
}

proc dupString { s n } {
    set ans ""
    while { [incr n -1] >= 0 } { append ans $s }
    return $ans
}

### to do fix this to see how many blank lines there are at our insert
### point and to insert ones to make up.
proc xHMassureNewlines { n } {
    
    uplevel 1 set _n $n
    uplevel 1 {
	set _have 0
	foreach _v [lrange [split [$win get "$wvar(W_insert)-4char" $wvar(W_insert)] \n] 1 end] {
	    if { [string trim "$_v"  " "] == "" } {
		incr _have
	    } else { set _have 0}
	}
#    set _have  [$win  compare $wvar(W_insert) == "$wvar(W_insert) linestart"]
	xHMtextInsert $win [dupString "\n" [expr {$_n - $_have}]]
    }
}

global xHMpreferences
set xHMpreferences(adjust) 0
catch {
    set width_ [expr {.9 * [winfo screenwidth .]}]
if { [winfo width .] >= 500 } {  set width_ [winfo width .] }
set xHMpreferences(adjust) [expr {
    $width_<= 640 ? -1 :
    $width_<= 800 ? 0 :
    1 } ]
   unset width_    
}
proc xHMsetDefaultPreferences {} {
    global xHMpreferences 
    foreach fam {propor fixed} {
	foreach {n si} {  1 8
    2 10
    3 12
    4 14
    5 18
    6 24
    7 24
    8 34	
    } { set xHMpreferences($fam,$n) $si}}
    set xHMpreferences(propor,adjust) [expr {$xHMpreferences(adjust) + 0}]
    set xHMpreferences(fixed,adjust) [expr {$xHMpreferences(adjust)  + 0}]
    array set xHMpreferences { propor arial fixed courier  indentwidth .7 }
}

xHMsetDefaultPreferences
catch { source ~/netmath.ini }

proc dputs {x}  { puts $x ; flush stdout}
proc xHMinit_state { win args } {
    upvar #0 xHMvar$win wvar
    upvar #0 xHMtaglist$win taglist
    global xHMpreferences
    array set saveme [array get wvar W_*]
    catch { unset wvar}
        catch { unset taglist}
    array set wvar {
	family propor   weight normal   style r   size 3
	list list 
	indent 0
	adjust 0
	measure -1
	W_insert insert
	W_update 15
    }
    array set wvar [array get saveme]
    array set taglist {indent:0 1}
    
}

proc xHMrender { win tag  params text } {
    global xHMtag
    upvar #0 xHMtaglist$win taglist
    upvar #0 xHMvar$win wvar
    set prefix ""

    set tag [string tolower $tag]
    # the following will go in a catch after debugging:
    #dputs "doing <$tag>"
    #dputs text=<<$text>> 
    # puts "xHMtag($tag)=[set xHMtag($tag)]"


   # eval [set xHMtag($tag)]
    if { [info exists xHMtag($tag)] } {
	# if { [catch { eval [set xHMtag($tag)] }] } { puts "error evaling tag:$tag" }
	eval [set xHMtag($tag)]
    } else {
	if { [string match "!--*" $tag] } { list} else {
	#puts "undefined $tag: puts comment:$text"
    }   }
    
 
    if { [regexp & $text] }  {
       set text [xHMconvert_ampersand $text]
    }

    #dputs "nowrap=[info exists taglist(nowrap)]"
    if { ![info exists taglist(nowrap)] } {
	regsub -all "\[ \t\r\n\]+" $text " " text
	if { "$prefix" != "" } { set text [string trimleft $text] }
    }
    xHMtextInsert $win $prefix$text
}

# make a copy of it.
proc xHMrender_orig [info args xHMrender] [info body xHMrender]

 
proc xHMtextInsert { win text } {
    global xHMtaglist$win
    upvar #0 xHMvar$win wvar
    # dputs "$win insert $wvar(W_insert) [list $text] [list [array names xHMtaglist$win ]]"
    # we calculate the longest unbroken line...
    if { 0 && $wvar(measure) >= 0 } {
	# puts "hi"
	set fo [xHMmapFont  $wvar(font)]
	set lis [split $text \n]
	set ll [font measure $fo [lindex $lis 0]]
	incr wvar(measure) $ll
	foreach vv [lrange $lis 1 end] {
	    maxIn wvar(maxwidth) $wvar(measure)
	    set wvar(measure)   [font measure $fo $vv]
	}
	maxIn wvar(maxwidth) $wvar(measure)
    }
    $win insert $wvar(W_insert) $text [array names xHMtaglist$win ]
}

proc xHMset_state { win args } {
    upvar #0 xHMvar$win wvar

    array set wvar $args

}

proc toPixelWidth { dim win } {
    if { [regexp {([.0-9]+)c} $dim junk d] } {
	return [expr {round($d*[winfo screenwidth $win] /(.1*[winfo screenmmwidth $win]))}] } else { return $dim}
    }
	

proc xHMinit_win { win } {
    upvar #0 xHMvar$win wvar
    global xHMpreferences
    # global xHMvar$win
   # catch { unset xHMvar$win }
    xHMinit_state $win
    $win config -font [xHMmapFont font:fixed:normal:r:3]
    catch { eval destroy [winfo children $win] }
    set iwidth [toPixelWidth  [set xHMpreferences(indentwidth)]c $win]
    # puts iwidth=$iwidth
    for { set i 0 } { $i < 12 } { incr i } {
	set half [expr {$iwidth/2.0 }]
	set w [expr {$i * $iwidth}]
	$win tag configure indent:$i -lmargin1 ${w} -lmargin2 ${w} -tabs \
		"[expr {$w + $half}] [expr {$w + 2*$half}]"
    }
   # $win tag bind doaref <Enter> "HMdoaref enter $win %x %y"
   # $win tag bind doaref <Leave> "HMdoaref leave $win %x %y"
   # $win tag bind doaref <1> "HMdoaref click $win %x %y"

    $win tag configure indent:0 -lmargin1 ${half} -lmargin2 ${half} -tabs "${half} [expr {2 * $half}]"
    $win tag configure href -borderwidth 2 -foreground blue -underline 1

    $win tag configure nowrap -wrap none
    $win tag configure rindent -rmargin $iwidth
    $win tag configure strike -overstrike 1

    $win tag configure underline -underline 1
    $win tag configure center -justify center
    $win configure -wrap word
}

global HMdefaultOptions
set HMdefaultOptions {
    {atagforeground blue "foreground for <a href=...>  tags"}
    {currenthrefforeground red "foreground of current <a href=..> tags"} 
    {foreground black "foreground"}
    {background white "background "}
    {atagbackground blue "background for <a href=...>  tags" }
}

foreach v $HMdefaultOptions {set HMOption([lindex $v 0]) [lindex $v 1] }

proc xHMwget { win key dflt } {
    upvar #0 xHMvar$win wvar
    if { [info exists wvar($key)] } {return $wvar($key) } else {
	return $dflt
}   }
    
proc HMdoaref { action win x y } {
    global HMOption
    set tags [$win tag names  @$x,$y ]
    set i [lsearch $tags h:*]
    set tag [lindex $tags $i]
    set reference [string range [lindex $tags $i] 2 end]
    # puts "$action $x $y"do_a
    switch $action {
	enter {
	    if { $i >= 0  }  {
		set ranges [$win tag ranges $tag]
		eval $win tag add currenthref $ranges
		textShowHelp $win currenthref @$x,$y "Click to follow link to $reference"

		$win tag bind $tag <Leave> "deleteHelp $win ;$win tag remove currenthref $ranges"
		$win tag  config currenthref -foreground [xHMwget $win option,atagforeground $HMOption(currenthrefforeground)] }
	    }
	click {
	    if { $i>= 0 } {
		global [oarray $win]
		if { [info exists [oloc $win dontopen]] } {
		    unset [oloc $win dontopen]
		} else {
		    oset $win dontopen 1
		    OpenMathOpenUrl $reference \
			    -commandpanel [omPanel $win]
		    catch {  unset [oloc $win dontopen] }
		}
		    return
	    }

	}
	    leave {
		
		$win tag delete currenthref
	    }
	}
    }   

proc xHMdo_isindex {} {
    uplevel 1 {
	set paramList [xHMsplitParams $params]
	xHMextract_param $paramList prompt " Enter search keywords: "
	xHMtextInsert $win $prompt
	set w $win.entry[incr wvar(counter)]
	entry $w
	# puts "wvar=[array get wvar]"
        $win window create $wvar(W_insert) -window $w  -padx 1 -pady 1
	bind $w <Return> "xHMget $wvar(url)?\[xHMencode_get \[$w get\]\]"
    }
}

# encode a string where
#  " " --> "+"
#  "\n" --> "%0d%0a"
#  [a-zA-Z0-9] --> self
#   c --> [format %.2x $c]

# make a list of all characters, to get char code from char.
global xHMallchars
set xHMallchars ""
for { set i 1} { $i <256 } {incr i } { append xHMallchars [format %c $i] }

proc xHMhexChar { c } {
    global xHMallchars
    set i [string first $c $xHMallchars]
    return %[format %.2x [expr {$i + 1}]]
}

# "ISO 8879-1986//ENTITIES Added Latin 1 substitutions
array set isoLatin1 {
    	AElig \xc6 	Aacute \xc1 	Acirc \xc2 	Agrave \xc0 
	Aring \xc5 	Atilde \xc3 	Auml \xc4 	Ccedil \xc7 
	ETH \xd0 	Eacute \xc9 	Ecirc \xca 	Egrave \xc8 
	Euml \xcb 	Iacute \xcd 	Icirc \xce 	Igrave \xcc 
	Iuml \xcf 	Ntilde \xd1 	Oacute \xd3 	Ocirc \xd4 
	Ograve \xd2 	Oslash \xd8 	Otilde \xd5 	Ouml \xd6 
	THORN \xde 	Uacute \xda 	Ucirc \xdb 	Ugrave \xd9 
	Uuml \xdc 	Yacute \xdd 	aacute \xe1 	acirc \xe2 
	acute \xb4 	aelig \xe6 	agrave \xe0 	amp \x26 
	aring \xe5 	atilde \xe3 	auml \xe4 	brvbar \xa6 
	cb \x7d 	ccedil \xe7 	cedil \xb8 	cent \xa2 
	copy \xa9 	curren \xa4 	deg \xb0 	divide \xf7 
	eacute \xe9 	ecirc \xea 	egrave \xe8 	eth \xf0 
	euml \xeb 	frac12 \xbd 	frac14 \xbc 	frac34 \xbe 
	gt \x3e 	hibar \xaf 	iacute \xed 	icirc \xee 
	iexcl \xa1 	igrave \xec 	iquest \xbf 	iuml \xef 
	laquo \xab 	lt \x3c 	micro \xb5 	middot \xb7 
	nbsp \xa0 	not \xac 	ntilde \xf1 	oacute \xf3 
	ob \x7b 	ocirc \xf4 	ograve \xf2 	ordf \xaa 
	ordm \xba 	oslash \xf8 	otilde \xf5 	ouml \xf6 
	para \xb6 	plusmn \xb1 	pound \xa3 	quot \x22 
	raquo \xbb 	reg \xae 	sect \xa7 	shy \xad 
	sup1 \xb9 	sup2 \xb2 	sup3 \xb3 	szlig \xdf 
	thorn \xfe 	times \xd7 	uacute \xfa 	ucirc \xfb 
	ugrave \xf9 	uml \xa8 	uuml \xfc 	yacute \xfd 
	yen \xa5 	yuml \xff 
}

proc xHMencode_get { str } {
    regsub -all "\[^a-zA-Z0-9\]" $str "\[xHMencode_get1 {x&x}]" str
    regsub -all "{x(\[{}\])x}" $str \{\\\\\\1x\} str
    return [subst  -novariables -nobackslashes $str ]
}

proc xHMencode_get1 { s } {
    set c [string index $s 1]
    switch -- $c {
	\n  { return %0d%0a }
	" " { return + }
	default { return [xHMhexChar $c ]}
    }
}


proc HexDecode { me }  {
    regsub -all {\+} $me " "  me
  if { [regexp % $me] } {
     regsub -all {\[} $me {[dec1 5b]} me
    regsub -all {%([0-9A-Fa-f][0-9A-Fa-f])} $me {[dec1 \1]}  me
    subst -nobackslashes -novariables $me
 } else { return $me }
}
proc dec1 { s } {
    if { [scan  $s %x d] } {
	format %c $d
    } else { error "cant decode hex $s" }
}




#
 #-----------------------------------------------------------------
 #
 # xHMparse_html --  takes HTML containing valid html code, and
 #  converts it into a sequence of calls to CMD.   These
 #  CMD should take 4 arguments:
 #     tagname slash tagArguments followingText
 #  where slash is {} or {/} depending on whether the TAGNAME was
 #  prefixed with a '/'.   The tagAguments are not parsed: eg
 #  <foo bil=good joe> hi there <next> this is
 #  would turn into
 #  $CMD {foo} {} {bil=good joe} {hi there}
 #  $CMD {next} {} {}   {this is..}
 #  We have tried to stay call compatible with a similar command
 #  written by Stephen Uhler.   Our handling of all the tags is different
 #  however.
 #
 #  Results: none
 #
 #  Side Effects: the sequence of $CMD is evald.
 #
 #----------------------------------------------------------------
#
proc xHMparse_html {html {cmd HMtest_parse} {firstTag hmstart}} {
    #dputs "beginning parse"
    
     global meee ; set meee $html;
	regsub -all \} <$firstTag>\n$html\n</$firstTag> {\&cb;} html
        #dputs "beginning parse1"
	regsub -all \{ $html {\&ob;} html
        # prevent getting \} \{ or \\n in a braces expression.
    	regsub -all "\\\\(\[\n<>])" $html "\\&#92;\\1" html
	#regsub -all "<(/?)(\[^ \t\n\r>]+)\[ \t\n\r\]*(\[^>]*)>" $html \
		"\}\n$cmd {\\2} {\\1} {\\3} \{" html
    	regsub -all "<(\[^ \t\n\r>]+)\[ \t\n\r\]*(\[^>]*)>" $html \
		"\}\n$cmd {\\1}  {\\2} \{" html
        # puts "<html=$html>"
        #dputs "beginning end splitparse1"
        
        #dputs "list {$html}"
	eval "list {$html}"
 
}

proc myPost { win menu } {
    bind $menu <Leave> "place forget $menu"
    place $menu -anchor center -relx 0 -rely 1.0 -bordermode outside -in $win
    raise $menu
}

## source "myhtml1.tcl"

###### myhtml1.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

defTag eval -alter {family fixed Cnowrap nowrap adjust 0} \
	-body {
    set paramList [xHMsplitParams $params]
    if { [xHMextract_param $paramList program ""] } {
	set wvar(evalPushed) "Teval"
	xHMpushConstantTag $win Teval
	foreach { k val } $paramList {
	    if { "$k" == "doinsert" } {
		set doinsert $val
		if { "$doinsert" != "[defaultInsertMode $program]" } {
		    lappend wvar(evalPushed) [list Targs -doinsert $doinsert]
		    xHMpushConstantTag $win [list Targs -doinsert $doinsert]
		}
	    } else {
		set tem "$k:$val"
		xHMpushConstantTag $win $tem
		lappend wvar(evalPushed) $tem
	    }
	}
    }
   }  -sbody {
       catch {foreach v $wvar(evalPushed) { xHMpopConstantTag $win $v } }
   }
   

defTag result  -alter {family fixed  weight bold  adjust 0} -body {
    set paramList [xHMsplitParams $params]
    set wvar(resultPushed) Tresult
    set taglist(Tresult) 1
    if { [xHMextract_param $paramList modified ""] } {
	lappend wvar(resultPushed) Tmodified
	set taglist(Tmodified) 1
    }
    if { [xHMextract_param $paramList name ""] } {
	lappend wvar(resultPushed) result:$name
	set taglist(result:$name) 1
    }}   -sbody {
       catch {foreach v $wvar(resultPushed) { xHMpopConstantTag $win $v } }
}

defTag netmath -body {
    set paramList [xHMsplitParams $params]
    catch {
    if { [xHMextract_param $paramList version ""] } {
	global ws_openMath
	if { [clock scan $version] > [clock scan $ws_openMath(date)] } {

	    xHMextract_param $paramList oldversion ""
	    append oldversion $text
	    set text $oldversion
	}
    }
#  swallow the following text if the browser is netmath enabled...
    if { [xHMextract_param $paramList swallow] } {
	set text ""
    }
    
  }
}

defTag math -body {
    set paramList [xHMsplitParams $params]
    upvar #0 xHMtaglist$win taglist
    global xHMpriv 	 xHMpreferences
    set pre {$}
    if { [xHMextract_param $paramList display] } {
	set pre {$\displaystyle}
	xHMassureNewlines 1
    }
	
    set wc $win.c[incr xHMpriv(counter)]
    canvas $wc 		-background [$win cget -background] -highlightthickness 0 -borderwidth 0
    
    set si [expr {[lindex $wvar(size) end] + [lindex $wvar(adjust) end]}]
    if { [xHMextract_param $paramList size] } {
	catch { incr si $size }
    }
    set si [expr {($si < 1 ? 1 : ($si > 7 ? 7 : $si))}]
    
    set ptsize $xHMpreferences([lindex $wvar(family) end],$si)
    if { [regexp & $text] }  {
	set text [xHMconvert_ampersand $text]
    }
    if { [catch { set it [ $wc create stext 0 0 \
	    -anchor nw -stext "$pre $text \$" -pointsize $ptsize \
	    ] } ]  } {
	xHMpushConstantTag $win "center"
	xHMtextInsert $win $text
	xHMpopConstantTag $win "center"
	set text ""
	destroy $wc
    } else {
	set text ""
	set dims [$wc bbox $it]
	$wc config -width [lindex $dims 2] -height [lindex $dims 3]
	xHMpushConstantTag $win "center"
	xHMtextInsert $win " "
	$win window create $wvar(W_insert) -window $wc  -padx 1 -pady 1
	xHMpopConstantTag $win "center"
    }
} -sbody {list }

proc getDim { dim  max } {
    if { [regexp {([0-9.]+)%$} $dim junk amt] } {
	return [expr {round($amt * .01 * $max) }]
    } elseif { $dim  < 0 } {
	return $max
    } else { return $dim }
}

defTag embed  -body {
    set paramList [xHMsplitParams $params]
    xHMextract_param $paramList width -1
    # allow for things like 50%
    set width [getDim [set width] [expr {.95 * [winfo width $win]}]]
    xHMextract_param $paramList height -1
    set height [getDim [set height] [expr {.95 * [winfo height $win]}]]
    set ewin [makeEmbedWin $win $width $height]
    $win window create $wvar(W_insert) -window $ewin  -padx 1 -pady 1
    set slave [oget $ewin slave]
    if { [    xHMextract_param $paramList src] } {
	set data [HMgetURL $win $src text]
	interp eval $slave $data
    }
} -sbody {}


proc HMgetURL {  textwin url type } {
    set currentUrl [oget $textwin currentUrl]
    catch { set currentUrl [decodeURL [oget $textwin baseurl]] }
    set new [resolveURL $url $currentUrl ]
    return [uplevel 1 getURL [list $new] type]
}

    
    
    
    
    

## endsource "myhtml1.tcl"
# source "mytable1.tcl"



## endsource myhtml.tcl
## source base64.tcl

###### base64.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################
# aaaaaabb bbbbcccc ccdddddd

proc tobase64 { binary } {
    set ll [string length $binary ]
    set n [binary scan  $binary "c*" ans]
    lappend ans 0 0 0
    foreach { x y z } $ans {
#	puts "$x $y $z $n"
	catch { 
	append new  \
		[char64 [expr {(($x & 255)>>2) }]][char64 \
		[expr {((($x & 3)<<4) | (($y >> 4) & 15))}]][char64 \
		[expr {((($y & 15)<<2) | (($z &255) >> 6))}]][char64 \
		[expr {($z & 63) }]]
	    
	}
    }
    return $new
}

proc char64 { x } {
    string range "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" $x $x
}
    
## endsource base64.tcl
## source bitmaps.tcl

###### bitmaps.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

global xHMulBMPdata
set xHMulBMPdata ""
lappend xHMulBMPdata "#define disc_width 6\n#define disc_height 6
static unsigned char disc_bits[] = {
   0xde, 0xff, 0xff, 0xff, 0xff, 0xde};"
    
lappend xHMulBMPdata  "#define circ_width 8\n#define circ_height 8
static unsigned char circ_bits[] = {
   0x3c, 0x42, 0x81, 0x81, 0x81, 0x81, 0x42, 0x3c};"

lappend xHMulBMPdata "#define rect_width 11\n#define rect_height 11
static unsigned char rect_bits[] = {
   0xff, 0x07, 0x01, 0x04, 0x01, 0x04, 0x01, 0x04, 0x01, 0x04, 0x01, 0x04,
   0x01, 0x04, 0x01, 0x04, 0x01, 0x04, 0x01, 0x04, 0xff, 0x07};"




    

## endsource bitmaps.tcl
## source tryembed.tcl

###### tryembed.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

## the following worked to have an entry box that spoke... 
# %     safe::interpCreate jack
# jack
# % set slave jack
# jack
# %     safe::interpInit $slave
# jack
# %     interp eval $slave set env(DISPLAY) $env(DISPLAY)
# :0.0
# %     load {} Tk $slave
# % interp eval jack {entry .ja ; pack .ja}
# %     interp eval $slave { proc policy {args } {} }
# %     Safesock_PolicyInit $slave
# %     setupUnknown $slave
# %     setupPrintVariables $slave
# % interp eval jack plot2d -xfun {sin(x)}




proc makeEmbedWin { parent width height } {
    global ws_openMath env auto_index
    set win $parent.embed[incr ws_openMath(counter)]
    set fr [frame $win -width $width -height $height -container 1]
    set slave tclet$ws_openMath(counter)
    safe::interpCreate $slave
    # make it exist somehow the autoload stuff doesnt make it a command
    if { [info exists auto_index(::safe::allowTk) ]  } {
	::safe::allowTk $slave [list -use [winfo id $fr]]
	::safe::TkInit $slave
	::safe::tkInterpInit $slave [list -use [winfo id $fr]]
	interp eval $slave [list set argv [list -use [winfo id $fr]]]	
    } else {
	safe::interpInit $slave
	interp eval $slave [list set argv [list -use [winfo id $fr]]]	
    }

    if { [info exists env(DISPLAY)] } {
    interp eval $slave set env(DISPLAY) $env(DISPLAY)
    }
    interp eval $slave { proc policy {args } {} }
#    $slave alias bgerror bgerror
    load {} Tk $slave
    Safesock_PolicyInit $slave
    setupUnknown $slave
    setupPrintVariables $slave
    oset $fr slave $slave
    return $fr
}


proc setupUnknown { slave } {
    interp eval $slave {rename auto_load auto_load-orig}
    interp alias $slave auto_load1 {} auto_load1 $slave
    interp eval $slave { proc auto_load {args} {
	if { [eval auto_load1 $args] } { return 1 }
	uplevel 1 auto_load-orig $args
    }
 }
}



proc auto_load1 { slave name {namespace ""} } {
   if { "[info proc $name ]" != "" } {
       set arglist [info args $name]
       set theargs {}
       foreach v $arglist {
	   if { [info default $name $v theDefault] } {
	       lappend theargs [list $v $theDefault]
	   } else { lappend theargs $v
	   }
       }
       interp eval $slave [list proc $name $theargs [info body $name]]
       return 1
   }
   return 0
}

proc setupPrintVariables { slave } {
    global printOption fontSize show_balloons getOp parse_table Parser     axisGray plot2dOptions plot3dOptions paperSizes  printOptions writefile     doExit  fontCourier8   plotdfOptions ftpInfo  ws_openMath
    foreach v {printOption fontSize show_balloons getOp parse_table Parser
    axisGray plot2dOptions plot3dOptions paperSizes  printOptions writefile
    doExit  fontCourier8   plotdfOptions ftpInfo ws_openMath} {
	if { [array exists  $v] } {
	    interp eval $slave [list array set $v [array get $v *] ]
	} else {  interp eval $slave [list set $v [set $v ]]
	}
    }

}
# proc tryit { {win .}  } {
#  global  ws_openMath
#     if { ![info exists ws_openMath(counter)] } {
# 	set ws_openMath(counter) 0
#     }
#     set width [winfo width $win]
#     set height [winfo height $win]
#     if { $width <=1 } {set width 200}
#     if { $height <=1 } {set height 200}
#     set ff [makeEmbedWin $win $width $height]
    
#     return [list $ff [oget $ff slave]]
# }

if { "[info command policy]" != "policy" } {
    proc policy { args } { }
}

proc browser_log { args } {
    # puts "$args"
}

## source nsafesock.tcl

###### nsafesock.tcl ######
# The Safesock Security Policy.
# -----------------------------
#
# Author: Jacob Levy & Brent Welch, 3/10/97
#
# This policy allows a safe slave to connect to remote sockets under the
# control of a master. The URL from which the applet is classified as
# either "inside" or "outside" and the host is added to the set of "inside"
# and "outside" hosts that this Tclet is allowed to connect to. Then, on
# the first request to connect to a host, if the host is classified as
# "inside" then subsequently the Tclet is allowed to connect only to hosts
# that are classified as "inside" (the same for if the first attempt is to
# connect to a host classified as "outside").
#
# The arrays used to drive this policy are defined in safesock.data.

# Remember the location of the data file for the Safesock policy, so that
# it can be reloaded each time the policy is used, to reflect changes.

global safesockDataFile
set safesockDataFile [file join [file dirname [info script]] safesock.data]

proc Safesock_PolicyInit {slave {version 1.0}} {
    global browser_state		;# Browser state
    global safesock_inside safesock_outside

    interp alias $slave socket {} SafesockSocketAlias $slave
    interp alias $slave fconfigure {} SafesockFconfigureAlias $slave

    uplevel #0 {source $safesockDataFile}


    # Attempt to get the URL and extract the server and port portions:

    set server "" ; set port "" ; set url ""
    catch {set url $browser_state($slave,url)}
    if {[regexp -nocase {http://([^:/]+)(:([0-9]+))?/} $url \
		x server y port]} {
	if {[string length $port] == 0} {
	    set port 80
	}
	set server [string tolower $server]
    } elseif {[string match "file:*" $url]} {
	set server localhost
	set port 80
    }

    # At this time it is unknown whether the slave will use inside
    # or outside connections:

    set browser_state($slave,safesock,permissions) unknown

    # Save the homebase for this Tclet:

    set browser_state($slave,safesock,homebase) $server
    set browser_state($slave,safesock,homeport) [list $port 1025- ftp ping]

    # Tell the slave about itself:

    interp eval $slave [list set env(SERVER) $server] 
    interp eval $slave [list set env(PORT) $port]
    interp eval $slave [list set env(URL) $url]

    browser_log $slave security installed policy Safesock
}

proc SafesockDecideInsideOrOutside {slave server} {
    global safesock_insideExclude safesock_outsideExclude
    global safesock_inside safesock_outside

    set status unknown

    # If the server matches anything outside and nothing in the outside
    # exclusion list, then it's outside:

    foreach i [array names safesock_outside] {
	if {[string match $i $server]} {
	    set status outside
	    break
	}
    }

    if {"$status" == "outside"} {
	foreach i [array names safesock_outsideExclude] {
	    if {[string match $i $server]} {
		set status unknown
		break
	    }
	}
    }

    # If the status is unknown, check whether it might be inside. It is
    # inside if the server matches anything inside and nothing in the
    # inside exclusion list:

    if {"$status" == "unknown"} {
	foreach i [array names safesock_inside] {
	    if {[string match $i $server]} {
		set status inside
		break
	    }
	}

	if {"$status" == "inside"} {
	    foreach i [array names safesock_insideExclude] {
		if {[string match $i $server]} {
		    set status unknown
		    break
		}
	    }
	}
    }

    # If the status is unknown at this point, raise an error

    if {"$status" == "unknown"} {
	error "unknown host: $server"
    }

    return $status
}

# This procedure is invoked when the slave is destroyed to clean up
# any associated state. It frees up the array of hosts and ports that
# the slave is allowed to connect to:

proc Safesock_PolicyCleanup {slave} {
    global browser_state

    foreach i [array names browser_state $slave,safesock,*] {
	unset browser_state($i)
    }
}


#
 #-----------------------------------------------------------------
 #
 # SafesockServerAnswer --  will replace COMMAND in a `socket -server command'
 #  request.   Checks if the incoming connection is allowed and if so
 #  invokes the original command.   Allowed is based on the same criteria
 #  as the outgoing connection.   
 #
 #  Results: none
 #
 #  Side Effects: if connect is allowed, transfer the socket to the slave
 #  and eval the original command there. 
 #
 #----------------------------------------------------------------
#
proc SafesockServerAnswer { slave command sock host port } {
    set peer [fconfigure $sock -peername]
    set host [lindex $peer 1]
    set host [string tolower $host]
    if { [SafesockAllow $slave $host [lindex $peer 2]] > 0 } {
	interp transfer {} $sock $slave
	interp eval $slave $command $sock $host $port
    } else { interp eval $slave [list error "connection from $host and $port disallowed"]
   }
}



#
 #-----------------------------------------------------------------
 #
 # SafesockAllow --  check if connection by SLAVE to HOST at PORT is allowed,
 #  based on the inside/outside history of slave and data in safesock.data
 #
 #  Results: 1 if succeeds and 0 if it fails to allow
 #
 #  Side Effects:  set GOOD to ok port in the caller 
 #
 #----------------------------------------------------------------
#
proc SafesockAllow { slave host port} {
    global browser_state
    global safesock_insideExclude safesock_outsideExclude
    global safesock_inside safesock_outside
    upvar 1 good good
    set host [string tolower $host]
    if {"$browser_state($slave,safesock,permissions)" == "unknown"} {
	if {[catch {set this [SafesockDecideInsideOrOutside $slave $host]}]} {
	    if {"$host" == "$browser_state($slave,safesock,homebase)"} {
	        set this homebase
	    } else {
	        error "unknown host: $host"
	    }
	}
	set browser_state($slave,safesock,permissions) $this
	browser_log $slave security $slave classified as $this
    }

    set portset -
    if {"$browser_state($slave,safesock,permissions)" == "homebase"} {
	if {"$host" == "$browser_state($slave,safesock,homebase)"} {
	    set portset $browser_state($slave,safesock,homeport)
	}
    } elseif {"$browser_state($slave,safesock,permissions)" == "inside"} {
	foreach hostpat [array names safesock_inside] {
	    if {[string match $hostpat $host]} {
		set portset $safesock_inside($hostpat)
		break
	    }
	}
	if {"$portset" != "-"} {
	    foreach hostpat [array names safesock_insideExclude] {
		if {[string match $hostpat $host]} {
		    set portset -
		    break
		}
	    }
	}
    } else {
	foreach hostpat [array names safesock_outside] {
	    if {[string match $hostpat $host]} {
		set portset $safesock_outside($hostpat)
		break
	    }
	}
	if {"$portset" != "-"} {
	    foreach hostpat [array names safesock_outsideExclude] {
		if {[string match $hostpat $host]} {
		    set portset -
		    break
		}
	    }
	}
    }

    if {"$portset" == "-"} {
	error "unknown host: $host"
    }

    if { [safesockPortMatches $port $portset] } {
	set good $port
	return 1
    }
    return 0
}

proc safesockPortMatches { port portset } {
    foreach portspec $portset {
	set low [set high ""]
	if {[regexp {^([0-9]+)-([0-9]*)$} $portspec x low high]} {
	    if {($low <= $port && $high == "") ||
			($low <= $port && $high >= $port)} {
                return 1
		break
	    }
	} elseif {$port == $portspec} {
	    return 1
	}
    }
    return 0
}

# the following should be set in safesock.data
if { ![info exists safesockAllowedServerPorts ] } {
  set safesockAllowedServerPorts { 1025-3000 }
}

proc SafesockSocketAlias {slave host port args} {
    global safesockAllowedServerPorts
    set option {}
    if { "$host" == "-server" } {
	set command $port
	set port [lindex $args 0]
	if { ![safesockPortMatches $port $safesockAllowedServerPorts] } {
	     error "bad port: $port"
	}
	set sock [socket -server \
		"SafesockServerAnswer $slave [list $command]" $port]
	interp transfer {} $sock $slave
	browser_log $slave normal socket -server $port
	return $sock
    } elseif { "$host" == "-async" } {
	set option $host
	set host $port
	set port [lindex $args 0]
    } else {
	if { [llength $args ] != 0 } {
	    error "wrong args: socket host port OR socket -server command port"
	}
	set serverCommand ""
    }
    SafesockAllow $slave $host $port
    if [info exists good] {
	if { "$option" != "" } { 
	    set sock [interp invokehidden $slave socket $option $host $good]
	} else { set sock [interp invokehidden $slave socket $host $good]}
	browser_log $slave normal socket $host $port
	return $sock
    }
    error "bad port: $port"
}

# This procedure handles the "fconfigure" alias from the slave:

proc SafesockFconfigureAlias {slave sock args} {
    global jack
    if {[llength $args] == 0} {
	return [interp invokehidden $slave fconfigure $sock]
    } elseif {[llength $args] == 1} {
	set flag [lindex $args 0]
	return [interp invokehidden $slave fconfigure $sock $flag]
    } else {
	browser_log $slave normal fconfigure $sock $args

	array set config [interp invokehidden $slave fconfigure $sock]
	foreach {flag value} $args {
	    switch -- $flag {
		-peername -
		-peerport {
		    error "Cannot change $flag configuration"
		}
		-blocking -
		-buffering -
		-buffersize -
		-eofchar -
		-translation {
		    set config($flag) $value
		}
		default {
		    error "unknown option $flag"
		}
	    }
	}
	lappend jack [list interp invokehidden $slave fconfigure $sock \
	    -blocking $config(-blocking) \
	    -buffering $config(-buffering) \
	    -buffersize $config(-buffersize) \
	    -eofchar $config(-eofchar) \
	    -translation $config(-translation)]
	return [interp invokehidden $slave fconfigure $sock \
	    -blocking $config(-blocking) \
	    -buffering $config(-buffering) \
	    -buffersize $config(-buffersize) \
	    -eofchar $config(-eofchar) \
	    -translation $config(-translation)]
    }
}

## endsource nsafesock.tcl
# source /home/wfs/openmath/nsafesock.tcl





## endsource tryembed.tcl

if { ![info exists ws_openMath(date)] } {
    set ws_openMath(date) [clock  format [clock seconds] -format {%m/%d/%Y} ]
}

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
#source macros.tcl
#source private.tcl
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
