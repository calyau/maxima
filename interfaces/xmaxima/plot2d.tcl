# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: plot2d.tcl,v 1.1 2002-05-24 17:35:54 amundson Exp $
#
###### plot2d.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

set p .plot
catch { destroy $p }

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
    after cancel $com
    after 10 $com
}

proc recomputePlot2d { win } {
       replot2d $win
}


## endsource plot2d.tcl
