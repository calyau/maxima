############################################################
# Plotdf.tcl                                               #
# Copyright (C) 1998 William F. Schelter                   #
# For distribution under GNU public License.  See COPYING. #
#                                                          #
#     Modified by Jaime E. Villate                         #
#     Time-stamp: "2024-03-28 14:51:52 villate"            #
############################################################

global plotdfOptions
set plotdfOptions {
    {dxdt "x-y^2+sin(x)*.3" {specifies dx/dt = dxdt.  eg -dxdt "x+y+sin(x)^2"} }
    {dydt "x+y" {specifies dy/dt = dydt.  eg -dydt "x-y^2+exp(x)"} }
    {dydx "" { may specify dy/dx = x^2+y,instead of dy/dt = x^2+y and dx/dt=1 }}
    {vectors blue "Color for the vectors"}
    {fieldlines red "Color for the fieldlines"}
    {curves "" "Color for the orthogonal curves"}
    {xradius 10 "Width in x direction of the x values" }
    {yradius 10 "Height in y direction of the y values"}
    {width 700 "Width of canvas in pixels"}
    {height 600 "Height of canvas in pixels" }
    {scrollregion {} "Area to show if canvas is larger" }
    {xcenter 0.0 {(xcenter,ycenter) is the origin of the window}}
    {ycenter 0.0 "see xcenter"}
    {bbox "" "xmin ymin xmax ymax .. overrides the -xcenter etc"}
    {tinitial 0.0 "The initial value of variable t"}
    {nsteps 300 "Number of steps to do in one pass"}
    {xfun "" "A semi colon separated list of functions to plot as well"}
    {tstep "" "t step size"}
    {direction "both" "May be both, forward or backward" }
    {versus_t 0 "Plot in a separate window x and y versus t, after each trajectory" }
    {windowname ".plotdf" "window name"}
    {windowtitle "Plotdf" "window title"}
    {parameters "" "List of parameters and values eg k=3,l=7+k"}
    {linecolors { green black  brown gray black} "colors for functions plots"}
    {sliders "" "List of parameters ranges k=3:5,u"}
    {trajectory_at "" "Place to calculate trajectory"}
    {linewidth "1.5" "Width of integral lines" }
    {nolines 0 "If not 0, plot points and nolines"}
    {plotpoints 0 "if not 0 plot the points at pointsize" }
    {pointsize 2 "radius in pixels of points" }
    {autoscale "x y" "Set {x,y}center and {x,y}range depending on data and function. "}
    {errorbar 0 "If not 0 width in pixels of errorbar.  Two y values supplied for each x: {y1low y1high y2low y2high  .. }"}
    {data "" "List of data sets to be plotted.  Has form { {xversusy {x1 x2 ... xn} {y1 .. yn ... ym}} .. {againstIndex {y1 y2 .. yn}}  .. }"}
    {labelposition "10 15" "Position for the curve labels nw corner"}
    {xaxislabel "" "Label for the x axis"}
    {yaxislabel "" "Label for the y axis"}
    {psfile "" "A filename where the graph will be saved in PostScript."}
    {nobox 0 "if not zero, do not draw the box around the plot."}
    {axes "xy" "if zero, no axes are drawn. x, y or xy to draw the axes."}
    {narrows "15" "Minimum number of arrows to draw in x and y directions"}
    {nolegend 0 "if not zero, do not write down the legend."}
    {bargraph 0 "If not 0 this is the width of the bars on a bar graph" }
}

proc makeFrameDf { win } {
    set w [makeFrame $win df]
    if { $w eq "." } { set w "" }
    makeLocal $win c dydx buttonFont type

    set top $win
    catch { set top [winfo parent $win]}
    catch {
	wm title $top {Xmaxima: plotdf}
	wm iconname $top "plotdf"
    }
    set mb $w.menubar
    ttk::button $mb.versust -text [mc "Time Plot"] -command "plotVersusT $win"
    ttk::button $mb.replot -text [mc "Replot"] -command "replot$type $win"
    pack $mb.replot $mb.versust -side left
    setForIntegrate $w
    return $win
}

proc swapChoose {win msg winchoose } {
    # global dydx dxdt dydt
    if { "$msg" == "dydt" } {
	pack $winchoose.dxdt -before $winchoose.dydt -side bottom
	oset $win dydx ""
	$winchoose.dydt.lab config -text "dy/dt:"
    } else {
	pack forget $winchoose.dxdt
	oset $win dxdt 1
	oset $win dydx " "
	$winchoose.dydt.lab config -text "dy/dx:"}}

proc setForIntegrate { win} {
    makeLocal $win c
#    $c delete printrectangle
    bind $c  <1> "doIntegrateScreen $win %x %y "
}

# sample procedures
# proc xff { t x y } { return [expr {$x + $y }] }
# proc yff { t x y } { return [expr {$x - $y }] }

proc doIntegrateScreen { win sx sy  } {
    makeLocal $win c
    doIntegrate $win [storx$win [$c canvasx $sx]] [story$win [$c canvasy $sy]]
}

proc doIntegrate { win x0 y0 } {
    makeLocal $win xradius yradius c dxdt dydt tinitial tstep nsteps \
        direction linewidth tinitial versus_t xmin xmax ymin ymax parameters \
        width height
    linkLocal $win didLast trajectoryStarts
    set linewidth [expr {$linewidth*[vectorlength $width $height]/1000.}]
    set arrowshape [scalarTimesVector $linewidth {3 5 2}]

    # method can be rungeKutta, rungeKuttaA or adamsMoulton
    set method {adamsMoulton}
    oset $win trajectory_at [format "%.10g  %.10g" $x0 $y0]
    lappend trajectoryStarts [list $x0 $y0]
    set didLast {}
    # puts "doing at $trajectory_at"
    # A reasonabel value of tstep has already been set up in drawDF by
    # using the maximum length of the field vectors. This is just in case.
    if {$tstep eq {}} {set tstep 0.1}

    set todo {1}
    switch -- $direction {
	forward { set todo {1}}
	backward { set todo {-1}}
	both { set todo {-1 1}}}
    set tasks {}
    foreach task {fieldlines curves} {
        set color [oget $win $task]
        if {($color ne {}) && ($color ne {blank})} {
            lappend tasks $task
            lappend useColors $task $color}}
    set taskNo -1
    foreach task $tasks {
        incr taskNo
        set linecolor [assoc $task $useColors]
        set signs $todo
        set coords {}
        if {$task eq {curves}} {
            setCurvesFunctions $dxdt $dydt $parameters
            set signs {-1 1}
        } else {
             setFieldFunctions $dxdt $dydt $parameters}                 
        foreach sgn $signs {
            set arrow {none}
            if {$task eq {fieldlines}} {
                if { $sgn < 0 } {
                    set arrow {first}
                    set coords {}
                } else {
                    if {$direction eq {forward}} {
                        set arrow {last}
                        set coords {}}}}
            set h [expr {$sgn*$tstep}]
            set form [list $method xff yff $tinitial $x0 $y0 $h $nsteps]

            # puts "doing: $form"
            # pts will be a list with values of t, x and y, at the initial
            # point and at each of the steps
            set pts [eval $form]
            lappend didLast $form
            set first 1
            # puts "clipping box: ($x1,$y1), ($x2,$y2)"
            foreach {t xr yr} $pts {
                if {$first} {
                        set p1 [list $xr $yr]
                        set c1 [PointCode $p1 $xmin $ymin $xmax $ymax]
                        # puts "point $p1 with code $c1"
                        if {!$c1} {
                            set coords [list [rtosx$win $xr] [rtosy$win $yr]]
                        } else {set coords {}}
                        set first 0
                    } else {
                        set p2 [list $xr $yr]
                        set c2 [PointCode $p2 $xmin $ymin $xmax $ymax]
                        # puts "point $p2 with code $c2"
                        if {$c1|$c2} {
                            set clip \
                                [ClipLine $p1 $p2 $c1 $c2 $xmin $ymin $xmax $ymax]
                            if {[llength $clip]} {
                                foreach p $clip {
                                    lappend coords [rtosx$win [lindex $p 0]]
                                    lappend coords [rtosy$win [lindex $p 1]]}}
                            if {$c2 && ([llength $coords] >= 4)} {
                                $c create line $coords -tags path -width \
                                    $linewidth -fill $linecolor -arrow $arrow \
                                    -arrowshape $arrowshape
                                set coords {}}
                        } else {
                            lappend coords [rtosx$win [lindex $p2 0]]
                            lappend coords [rtosy$win [lindex $p2 1]]}
                        set p1 $p2
                        set c1 $c2}}
            if {[llength $coords] >= 4} {
                $c create line $coords -tags path -width $linewidth \
                    -fill $linecolor -arrow $arrow -arrowshape $arrowshape}}}
    if { $versus_t } { plotVersusT $win}
}

proc plotVersusT { win } {
    linkLocal $win didLast dydt dxdt parameters xcenter xradius ycenter yradius
    if { $didLast == {} } { return }
    set w [winfo parent $win]
    if {$w eq {.}} { set w {}}
    set xwin .versust.plotx
    set ywin .versust.ploty
    set xdata {}
    set ydata {}

    foreach v $didLast {
	set ans [eval $v]
	desetq "tinitial x0 y0 h steps" [lrange $v 3 end]
	set this [lrange $v 0 4]
	if { [info exists doing($this) ] } { set tem $doing($this) } else {
	    set tem ""
	}
	set doing($this) ""
	set allx ""; set ally ""; set allt ""
	foreach {t x y } $ans {
            if {($x>=$xcenter-1.1*$xradius) && ($x<=$xcenter+1.1*$xradius)
                && ($y>=$ycenter-1.1*$yradius) && ($y<=$ycenter+1.1*$yradius)} {
                lappend allx $x
                lappend ally $y
                lappend allt $t}}
	foreach u $tem v [list $allx $ally $allt] {
	    if { $h > 0 } { lappend doing($this) [concat $u $v]} else {
		lappend doing($this) [concat [lreverse $v] $u]}}}

    foreach {na val } [array get doing] {
	lappend xdata [list xaxislabel "t"]
	lappend xdata [list yaxislabel [oget $win xaxislabel]]
        lappend xdata [list plotpoints 0] [list nolegend 1]
	lappend xdata [list xversusy [lindex $val 2] [lindex $val 0] ]
	lappend ydata [list xaxislabel "t"]
	lappend ydata [list yaxislabel [oget $win yaxislabel]]
        lappend ydata [list plotpoints 0] [list nolegend 1]
	lappend ydata [list xversusy [lindex $val 2] [lindex $val 1] ]}
    if { ![winfo exists .versust] } {toplevel .versust}
    # puts "plotdata: $plotdata"
    plot2d -data $xdata -windowname $xwin -ycenter $xcenter -yradius $xradius
    wm title $xwin [concat [oget $win xaxislabel] [mc " versus t"]]
    plot2d -data $ydata -windowname $ywin -ycenter $ycenter -yradius $yradius
    wm title $ywin [concat [oget $win yaxislabel] [mc " versus t"]]
}

proc lreverse { lis } {
    set ans ""
    set i [llength $lis]
    while { [incr i -1]>=0 } {
	lappend ans [lindex $lis $i]
    }
    return $ans
}

proc drawArrowScreen { c atx aty dfx dfy color } {
    set win [winfo parent $c]
    makeLocal $win width height
    set linewidth [expr {[vectorlength $width $height]/1000.}]
    set arrowshape [scalarTimesVector $linewidth {3 5 2}]
    set x1 [expr {$atx + $dfx}]
    set y1 [expr {$aty + $dfy}]
    $c create line $atx $aty $x1 $y1 -tags arrow -fill $color -arrow last \
        -arrowshape $arrowshape -width $linewidth }

proc drawDF { win tinitial } {
    global axisGray
    makeLocal  $win xmin xmax xcenter ycenter c ymin ymax transform vectors \
        xaxislabel yaxislabel nobox axes width height narrows tstep
    set rtosx rtosx$win
    set rtosy rtosy$win
    set storx storx$win
    set story story$win
    set stepsize [expr {$width/($narrows+2.0)}]
    set stepy [expr {$height/($narrows+2.0)}]
    if { $stepy < $stepsize } {set stepsize $stepy}
    set margin [expr {0.7*$stepsize}]
    set min 1.0E42
    set max 0.0
    set t0 $tinitial
    set xfactor [lindex $transform 0]
    set yfactor [lindex $transform 3]
    set uptox [expr {[$rtosx $xmax] - $margin}]
    set uptoy [expr {[$rtosy $ymin] - $margin}]
    # draw the axes:
    #puts "draw [$rtosx $xmin] to $uptox"
    if {($vectors ne "") && ($vectors ne "blank") } {
	for {set x [expr {[$rtosx $xmin] + $margin}]} {$x < $uptox} \
            {set x [expr {$x + $stepsize}]} {
                for {set y [expr {[$rtosy $ymax] + $margin}]} {$y < $uptoy} \
                    {set y [expr {$y + $stepsize}]} {
		set args "$t0 [$storx $x] [$story $y]"
		set dfx [expr {$xfactor * [eval xff $args]}]
		# screen y is negative of other y
		set dfy [expr  {$yfactor * [eval yff $args]}]
		# puts "$dfx $dfy"
		set len [vectorlength $dfx $dfy]
		append all " $len $dfx $dfy "
		if { $min > $len } {set min $len}
                        if { $max < $len } {set max $len}}}
        if {$tstep eq {}} {oset $win tstep [expr {$stepsize/(10.0*$max)}]}
	set arrowmin [expr {0.25*$stepsize}]
	set arrowrange [expr {0.85*$stepsize - $arrowmin}]
	set s1 [expr {($arrowrange*$min+$arrowmin*$min-$arrowmin*$max)/($min-$max)}]
	set s2 [expr {$arrowrange/($max-$min)}]
	# we calculate fac for each length, so that
	# when we multiply the vector times fac, its length
	# will fall somewhere in [arrowmin,arrowmin+arrowrange].
	# Vectors of length min and max resp. should get mapped
	# to the two end points.
	# To do this we set fac [expr {$s1/$len + $s2}]
        set i -1
	for {set x [expr {[$rtosx $xmin] + $margin}]} {$x < $uptox} \
            {set x [expr {$x + $stepsize}]} {
                for {set y [expr {[$rtosy $ymax] + $margin}]} {$y < $uptoy} \
                    {set y [expr {$y + $stepsize}]} {
                set len [lindex $all [incr i]]
		set dfx [lindex $all [incr i]]
		set dfy [lindex $all [incr i]]
		#puts "[$storx $x] [$story $y] x=$x y=$y dfx=$dfx dfy=$dfy
		# puts "$len $dfx $dfy"
                if {$len != 0.0} {
                    set fac [expr {$s1/$len + $s2}]
                    drawArrowScreen $c $x $y [expr {$fac * $dfx}] \
                        [expr {$fac * $dfy} ] $vectors}}}}
    set x1 [rtosx$win $xmin]
    set y1 [rtosy$win $ymax]
    set x2 [rtosx$win $xmax]
    set y2 [rtosy$win $ymin]
    
    # Draw the two axes
    $c del axes
    if { $xmin*$xmax < 0 && ($axes == {y} || $axes == {xy}) } {
	if { $nobox == 0 } {
	    $c create line [$rtosx 0] $y1 [$rtosx 0] $y2 -fill $axisGray \
		-tags axes
	} else {
	    $c create line [$rtosx 0] $y1 [$rtosx 0] $y2 -width 2 \
		-arrow "first" -tags axes}}
    if { $ymin*$ymax < 0  && ($axes == {x} || $axes == {xy}) } {
	if { $nobox == 0 } {
	    $c create line $x1 [$rtosy 0] $x2 [$rtosy 0] -fill $axisGray \
		-tags axes
	} else {
	    $c create line $x1 [$rtosy 0] $x2 [$rtosy 0] -width 2 \
		-arrow "last" -tags axes}}
    # Draw the plot box
    if { "[$c find withtag printrectangle]" == "" && $nobox == 0 } {
	$c create rectangle $x1 $y1 $x2 $y2 -tags printrectangle -width 2
	marginTicks $c [storx$win $x1] [story$win $y2] [storx$win $x2] \
	    [story$win $y1] "printrectangle marginticks"}
    # Write down the axes labels
    $c del axislabel
    set width [oget $win width]
    set height [oget $win height]
    if {$nobox != 0  && $xmin*$xmax < 0  && ($axes == {y} || $axes == {xy})} {
	set xbound [expr { [$rtosx 0] - 0.08*$width}]
    } else {
	set xbound [expr {$x1-0.08*$width}]
    }
    $c create text $xbound [expr {($y1+$y2)/2.0}] -anchor center -angle 90 \
       -text [oget $win yaxislabel] -font {helvetica 16 normal} -tags axislabel
    if {$nobox != 0  && $ymin*$ymax < 0  && ($axes == {x} || $axes == {xy})} {
	$c create text [expr {$x2-0.01*$width}] \
            [expr { [$rtosy 0]+0.02*$height}] -anchor ne -tags axislabel \
            -text [oget $win xaxislabel] -font {helvetica 16 normal}
    } else {
	$c create text [expr {($x1 + $x2)/2}] [expr {$y2 + 0.08*$height}] \
	    -anchor center -text [oget $win xaxislabel] \
	    -font {helvetica 16 normal} -tags axislabel
    }
}

proc parseOdeArg {  s } {
    set orig $s
    set w "\[ ]*"
    set exp "\[dD]$w\\($w\(\[xyz])$w,$w\(\[xyt])$w\\)$w=(\[^;]+)"
    while { [regexp -- $exp $s junk x t expr ] } {
	lappend ans  -d${x}d$t
	lappend ans $expr
	regexp -indices $exp $s junk x t expr
	set s [string range $s [lindex $junk 1] end]
    }
    if { ![info exists ans] || ([llength $ans] == 2 && "[lindex $ans 0]" != "-dydx") } {
	error [mc "bad -ode argument:\n$orig\nShould be d(y,x)=f(x,y)
       OR d(x,t)=f(x,y) d(y,t)=g(x,y)"]
    }
    return $ans
}

proc plotdf { args } {
    global plotdfOptions printOption printOptions plot2dOptions
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
    oset $win didLast {}
    # Makes extra vertical space for sliders
    linkLocal $win sliders height
    if {[string length $sliders] > 0} {
        oset $win height [expr {$height + 40*[llength [split $sliders ,]]}]}

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
    global printOption plotdfOptions
    linkLocal $win xfundata data psfile
    if { ![info exists data] } {set data ""}
    makeLocal $win c dxdt dydt tinitial nsteps xfun trajectory_at parameters
    set_xy_region $win 0.8
    set_xy_transforms $win
    setFieldFunctions $dxdt $dydt $parameters
    $c delete all
    setForIntegrate $win
    oset $win curveNumber -1
    drawDF $win $tinitial
    if { $trajectory_at ne "" } {eval doIntegrate $win  $trajectory_at}
    set xfundata ""
    foreach v [sparseListWithParams $xfun {x y t} $parameters ] {
	proc _xf {  x  } "return \[expr { $v } \]"
	lappend xfundata [list nolegend 1] \
	    [linsert [calculatePlot $win _xf $nsteps] 0 xversusy] }
    redraw2dData $win -tags path

    # Create a PostScript file, if requested
    if { $psfile ne "" } {
	set printOption(psfilename) $psfile
	writePostscript $win
	$c delete printoptions
	eval [$win.menubar.close cget -command] }}

proc setFieldFunctions { dxdt dydt parameters } {
    proc xff {t x y} "expr {[sparseWithParams $dxdt {x y} $parameters]}"
    proc yff {t x y} "expr {[sparseWithParams $dydt {x y} $parameters] } "
}
proc setCurvesFunctions { dxdt dydt parameters } {
    proc xff {t x y} "expr {[sparseWithParams $dydt {x y} $parameters]}"
    proc yff {t x y} "expr {[sparseWithParams -($dxdt) {x y} $parameters]}"
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

    foreach w {narrows parameters xfun linewidth xradius yradius xcenter ycenter tinitial versus_t nsteps direction curves vectors fieldlines } {
	mkentry $wb1.$w [oloc $win $w] $w $buttonFont
	pack $wb1.$w -side bottom -expand 1 -fill x
    }
    mkentry $wb1.trajectory_at [oloc $win trajectory_at] \
	"Trajectory at" $buttonFont
    bind $wb1.trajectory_at.e <KeyPress-Return> \
	"eval doIntegrate $win \[oget $win trajectory_at\] "
    pack  $wb1.trajectory_at   $frdydx    -side bottom -expand 1 -fill x
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
    linkLocal $win trajectoryStarts c tinitial dxdt dydt parameters
    set redo 0
    set trajs ""

    catch {set trajs $trajectoryStarts}

    while { $redo != $recompute } {
	#	puts "	setFieldFunctions $dxdt $dydt $parameters"
	setFieldFunctions $dxdt $dydt $parameters
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
