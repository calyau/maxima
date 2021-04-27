############################################################
# Plot3d.tcl                                               #
# Copyright (C) 1998 William F. Schelter                   #
# For distribution under GNU public License.  See COPYING. #
#                                                          #
#     Modified by Jaime E. Villate                         #
#     Time-stamp: "2021-04-27 19:10:28 villate"            #
############################################################

global plot3dOptions
set plot3dOptions {
    {xradius 1 "Width in x direction of the x values" }
    {yradius 1 "Height in y direction of the y values"}

    {width 700 "Width of canvas in pixels"}
    {height 500 "Height of canvas in pixels" }
    {xcenter 0.0 {(xcenter,ycenter) is the origin of the window}}
    {ycenter 0.0 "see xcenter"}
    {zcenter 0.0 "see xcenter"}
    {bbox "" "xmin ymin xmax ymax zmin zmax overrides the -xcenter etc"}
    {zradius auto " Height in z direction of the z values"}
    {az 30 "azimuth angle" }
    {el 60 "elevation angle" }

    {thetax 10.0 "ignored is obsolete: use az and el"}
    {thetay 20.0 "ignored is obsolete: use az and el"}
    {thetaz 30.0 "ignored is obsolete: use az and el"}

    {flatten 1 "Flatten surface when zradius exceeded" }
    {zfun "" "a function of z to plot eg: x^2-y^2"}
    {parameters "" "List of parameters and values eg k=3,l=7"}
    {sliders "" "List of parameters ranges k=3:5,u"}
    {data  "" "a data set of type { variable_grid xvec yvec zmatrix}
    or {matrix_mesh xmat ymat zmat} or {grid {xmin xmax} {ymin ymax} zmatrix}"}
    {nsteps "10 10" "steps in x and y direction"}
    {rotationcenter "" "Origin about which rotation will be done"}
    {windowname ".plot3d" "window name"}
    {psfile "" "A filename where the graph will be saved in PostScript."}
    {nobox 0 "if not zero, do not draw the box around the plot."}
    {hue 0.25 "Default hue value."}
    {saturation 0.7 "Default saturation value."}
    {value 0.8 "Default brightness value."}
    {colorrange 0.5 "Range of colors used."}
    {gradlist {{0 "#00ff00"} {1 "#ff00ff"}} "Color gradient: List of values and colors."}
    {ncolors 180 "Number of colors used."}
    {colorscheme "hue" "Coloring Scheme (hue, saturation, value, gray, gradient or 0)."}
    {mesh_lines "black" "Color for the meshes outline, or 0 for no outline."}
}


## source Matrix.tcl

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
    } else {
	set flatten 0
    }

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
    makeLocal $win xmin xmax ymin ymax zmin zmax cmap
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
    eval $com}

proc plot3dcolorFun {win z } {
    makeLocal $win zmin zmax ncolors hue saturation value colorrange colorscheme gradlist
    if { $z < $zmin || $z > $zmax } {return "none"}
    set h [expr { 360*$hue }]
    if { ($value > 1) || ($value < 0) } {
        set value [expr { $value - floor($value) }]}
    set tem [expr {(double($colorrange)/$ncolors)*round(($z - $zmin)*$ncolors/($zmax - $zmin+.001))}]
    switch -exact $colorscheme {
	"hue" { return [hsv2rgb [expr { 360*$tem+$h }] $saturation $value] }
	"saturation" { return [hsv2rgb $h [expr { $tem+$saturation }] $value] }
	"value" { return [hsv2rgb $h $saturation [expr {$tem+$value}]] }
	"gray"  { set g [expr { round( ($tem+$value)*255 ) } ]
	    return  [format "\#%02x%02x%02x" $g $g $g] }
        "gradient" {
            for {set i 0} {$i < [llength $gradlist]} {incr i} {
                if {$tem < [lindex $gradlist $i 0]} break}
            if {$i == 0} {
                return [lindex $gradlist 0 1]
            }
            set down [lindex $gradlist [expr $i-1] 0]
            set up [lindex $gradlist $i 0]
            return [interpolatecolor [lindex $gradlist [expr $i-1] 1] [lindex $gradlist $i 1] [expr {($tem-$down)/($up-$down)}]]}
	"0" { return "#ffffff" }}}

proc setupPlot3dColors { win first_mesh} {
    upvar #0 [oarray $win] wvar
    # the default prefix for cmap
    set wvar(cmap) c1
    makeLocal $win colorfun points lmesh
    foreach tem [lrange $lmesh $first_mesh end] {
        set k [llength $tem]
	if { $k == 4 } {
	    set z [expr { ([lindex $points [expr { [lindex $tem 0] + 2 } ]] +
			   [lindex $points [expr { [lindex $tem 1] + 2 } ]] +
			   [lindex $points [expr { [lindex $tem 2] + 2 } ]] +
			   [lindex $points [expr { [lindex $tem 3] + 2 } ]])/
			  4.0 } ]
	    catch { set wvar(c1,[lindex $tem 0]) [$colorfun $win $z] }
	}
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

#
#-----------------------------------------------------------------
#
# set_xy_region_3d --  set up the bounds of the x and y coordinates
# of the projection of the surface on the xy plane and the part of the
# window that will be filled by that projection (fac, a number between
# 0 and 1).
#
#----------------------------------------------------------------
#
proc set_xy_region_3d { win fac } {
    linkLocal $win scale
    makeLocal $win xcenter ycenter xradius yradius xmin xmax ymin ymax zradius
    set scale [list [expr {1.5/($xradius)}] 0 0 0 [expr {1.5/($yradius)}] \
                   0 0 0 [expr {1.5/($zradius)}] ]
    desetq "xmin ymin" [matMul $scale 3 "$xmin $ymin 0" 1]
    desetq "xmax ymax" [matMul $scale 3 "$xmax $ymax 0" 1]
    oset $win fac $fac
    oset $win xmin $xmin
    oset $win xmax $xmax
    oset $win ymin $ymin
    oset $win ymax $ymax
}

#

proc plot3d { args } {
    global  plot3dOptions
    set win [assoc -windowname $args]
    if { "$win" == "" } {
	set win [getOptionDefault windowname $plot3dOptions] }
    clearLocal $win
    mxapply mkPlot3d  $win $args
    #    bind $win <Configure> {}	
    replot3d $win
}

proc replot3d { win } {
    global printOption
    makeLocal $win nsteps zfun data c
    linkLocal $win parameters sliders psfile nobox

    oset $win maintitle    "concat \"Plot of z = [oget $win zfun]\""
    if { [llength $nsteps] == 1 }    {
	oset $win nsteps \
	    [set nsteps  [list [lindex $nsteps 0] [lindex $nsteps 0]]]
    }

    set sliders [string trim $sliders]
    if { "$sliders" != "" && ![winfo exists $c.sliders] } {
	addSliders $win
    }

    set zfun [string trim $zfun]
    if { "$zfun" != "" } {
	proc _xf {  x  y } "return \[expr { [sparseWithParams $zfun {x y} $parameters ] } \]"
	addOnePlot3d $win [calculatePlot3data $win _xf  [lindex $nsteps 0] [lindex $nsteps 1]]
	# calculatePlot3d $win _xf [lindex $nsteps 0] [lindex $nsteps 1]
    }

    set data [string trim $data]
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

    if { $nobox == 0 } {
	addBbox $win
    }

    set_xy_region_3d $win 0.5
    set_xy_transforms $win
    # grab the bbox just as itself
    global maxima_priv
    linkLocal $win lmesh
    if { [llength $lmesh] > 100 * $maxima_priv(speed)  } {
	# if we judge that rotation would be too slow, we make a secondary list
	# of meshes (random) including the bbox, and display those.
	linkLocal $win  points lmeshBbox pointsBbox
	set n [llength $lmesh]
	set lmeshBbox [lrange $lmesh [expr {$n -13}] end]
	set i 0 ;
	while { [incr i ] < ( 35*$maxima_priv(speed)) } {
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

    # Create a PostScript file, if requested
    if { $psfile != "" } {
	set printOption(psfilename) $psfile
	writePostscript $win
	$c delete printoptions
	eval [$win.menubar.close cget -command]
    }

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
	}
    }
}


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
	set col black
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
    makeLocal $win mesh_lines
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
	    #	    set color black
	    #	    catch { set color [oget $win $cmap,$mesh]}

	    eval $canv create line $coords -tags [list [list axis mesh.$k]] \
		-fill $color -width 2
	} else {
	    # puts "doing special $mesh, $coords"
	    catch { set tem [oget $win special([lindex $mesh 0])]
		eval [concat $tem $coords]
	    }
	}
    } elseif { [string length $color] < 8 && $color != "none"} {
	if { $mesh_lines != 0 } {
	    set outline "-outline $mesh_lines"
	} else {
	    set outline ""
	}
	eval $canv create polygon $coords -tags [list [list poly mesh.$k]] \
	    -fill $color $outline
    }
}


proc makeFrame3d { win } {
    global plot3dPoints
    set w [makeFrame $win 3d]
    set top $w
    catch { set top [winfo parent $w]}
    catch {
	wm title $top {Xmaxima: plot3d}
	wm iconname $top "plot3d"
    }
    #pack $w
}

proc mkPlot3d { win  args } {
    global plot3dOptions  printOption [oarray $win] axisGray
    getOptions $plot3dOptions $args -usearray [oarray $win]
    setPrintOptions $args
    set printOption(maintitle) ""
    set wb $win.menubar
    setupCanvas $win
    # catch { destroy $win }
    makeFrame3d $win
    oset $win sliderCommand sliderCommandPlot3d
    oset $win noaxisticks 1

    makeLocal $win buttonFont c
    [winfo parent $c].position config -text {}
    bind $c <Motion> ""

    setForRotate $win
}

proc doConfig3d { win } {
    desetq "wb1 wb2" [doConfig $win]
    makeLocal $win buttonFont

    mkentry $wb1.zfun [oloc $win zfun]  "z=f(x,y)" $buttonFont
    mkentry $wb1.nsteps [oloc $win nsteps]  [mc "Number of mesh grids"]  $buttonFont

    pack $wb1.zfun  $wb1.nsteps
    pack	    $wb1.zfun  $wb1.nsteps
    foreach w {xradius yradius xcenter ycenter zcenter zradius parameters } {
	mkentry $wb1.$w [oloc $win $w] $w $buttonFont
	pack $wb1.$w
    }

    scale $wb1.rotxscale -label [mc "azimuth"]  \
	-orient horizontal -length 150 -from -180 -to 180 -resolution 1 \
	-command "setView $win" -variable [oloc $win az] -tickinterval 120 -font $buttonFont

    scale $wb1.rotyscale -label [mc "elevation"]  \
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
	catch { $win.position config -text [eval [concat "format {(%.2f %.2f %.2f)}" $pt]] }	
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
    # show values of azimuth and elevation angles
    set az [oget $win az]
    set el [oget $win el]
    catch { $win.position config -text [eval [concat "format {Azimuth: %.2f, Elevation: %.2f}" $az $el]] }
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
