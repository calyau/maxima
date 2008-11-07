# -*-mode: tcl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
# Copyright (C) 2008 Jaime E. Villate <villate@fe.up.pt>
#
# Fourth order Tunge-Kutta method, with fixed-lenght steps in phase space.
# Based on Rk.tcl by William F. Schelter
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifht Floor, Boston,
# MA 02110-1301, USA

proc trajectory { f g t0 x0 y0 sx sy nsteps dir} {
    set n $nsteps
    set ans "$x0 $y0"
    set xn $x0
    set yn $y0
    set tn $t0
    catch {
	while { [incr nsteps -1] >= 0 } {
	    set kn1 [$f $tn $xn $yn]
	    set ln1 [$g $tn $xn $yn]

	    set h [expr {$dir/[vectorlength [expr {$kn1/$sx}] [expr {$ln1/$sy}]]}]
	    set h2 [expr {$h / 2.0 }]
	    set h6 [expr {$h / 6.0 }]

	    set arg [list [expr {$tn + $h2}] [expr {$xn + $h2 * $kn1}] [expr {$yn + $h2*$ln1}]]
	    set kn2 [eval $f $arg]
	    set ln2 [eval $g $arg]

	    set arg [list [expr {$tn + $h2}] [expr {$xn + $h2 * $kn2}] [expr {$yn +$h2*$ln2}]]
	    set kn3 [eval $f $arg]
	    set ln3 [eval $g $arg]

	    set arg [list [expr {$tn + $h}] [expr {$xn + $h * $kn3}] [expr {$yn + $h*$ln3}]]
	    set kn4 [eval $f $arg]
	    set ln4 [eval $g $arg]

	    set dx [expr {$h6 * ($kn1+2*$kn2+2*$kn3+$kn4)}]
	    set dy [expr {$h6 * ($ln1+2*$ln2+2*$ln3+$ln4)}]

	    if { [vectorlength $dx $dy] > 5*[vectorlength $sx $sy] } { return $ans }
	    set xn [expr {$xn + $dx}]
	    set yn [expr {$yn + $dy}]
	    set tn [expr {$tn + $h}]

	    lappend ans  $xn $yn
	}
    }
    return $ans
}

proc orthogonal { f g t0 x0 y0 sx sy nsteps dir} {
    set n $nsteps
    set ans "$x0 $y0"
    set xn $x0
    set yn $y0
    set tn $t0
    catch {
	while { [incr nsteps -1] >= 0 } {
	    set kn1 [expr -1*[$g $tn $xn $yn] ]
	    set ln1 [$f $tn $xn $yn]

	    set h [expr {$dir/[vectorlength [expr {$kn1/$sx}] [expr {$ln1/$sy}]]}]
	    set h2 [expr {$h / 2.0 }]
	    set h6 [expr {$h / 6.0 }]

	    set arg [list [expr {$tn + $h2}] [expr {$xn + $h2 * $kn1}] [expr {$yn + $h2*$ln1}]]
	    set kn2 [expr -1*[eval $g $arg] ]
	    set ln2 [eval $f $arg]

	    set arg [list [expr {$tn + $h2}] [expr {$xn + $h2 * $kn2}] [expr {$yn +$h2*$ln2}]]
	    set kn3 [expr -1*[eval $g $arg] ]
	    set ln3 [eval $f $arg]

	    set arg [list [expr {$tn + $h}] [expr {$xn + $h * $kn3}] [expr {$yn + $h*$ln3}]]
	    set kn4 [expr -1*[eval $g $arg] ]
	    set ln4 [eval $f $arg]

	    set dx [expr {$h6 * ($kn1+2*$kn2+2*$kn3+$kn4)}]
	    set dy [expr {$h6 * ($ln1+2*$ln2+2*$ln3+$ln4)}]

	    if { [vectorlength $dx $dy] > 5*[vectorlength $sx $sy] } { return $ans }
	    set xn [expr {$xn + $dx}]
	    set yn [expr {$yn + $dy}]
	    set tn [expr {$tn + $h}]

	    lappend ans  $xn $yn
	}
    }
    return $ans
}
