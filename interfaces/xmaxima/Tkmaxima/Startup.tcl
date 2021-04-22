############################################################
# Startup.tcl                                              #
# Copyright (C) 1998 William F. Schelter                   #
# For distribution under GNU public License.  See COPYING. #
#                                                          #
#     Time-stamp: "2021-04-04 12:02:54 villate"            #
############################################################
#
# Old startup code - unused.

set dontstart 1
if { ![info exists dontstart] } {
    ## source "startup.tcl"

    # try to fix up so that we dont write to stdout in the browser..
    
    # source preamble.tcl
    
    if { "[info command wm]" != "wm" } {
        proc wm { args } {}
    }
    if { [info exists embed_args(width)] || \
             [info exists embed_args(embed_mode)]} {
        set ws_openMath(inbrowser) 1
        # rebind puts
        rename puts _joel
        proc puts {args} {
            global badout ;
            switch [llength $args] {
                2 {_joel [lindex $args 0] [lindex $args 1]}
                1 {append badout $args}
                3 {_joel [lindex $args 0] [lindex $args 1] [lindex $args 2]}
            }
        }
        
        #  rename flush _flush1
        #  proc flush { x } {
        #       if { "$x" != "stdout" } {
        # 	  _flush1 $x
        #       }
        #   }
        
    }
    
    set toeval "OpenMathOpenUrl http://maxima.sourceforge.net/index.html"
    
    catch {
        if { "[lindex $argv 0]" != "" } {
            wm minsize . 500 400
            after 7000  wm minsize . 0 0
            set toeval "OpenMathOpenUrl [lindex $argv 0]"
        }
    }
    catch { set toeval [assoc eval [getattr browserArgs]] }
    
    if { [catch { eval $toeval } err ] }  {
        if { [regexp "unreachable|couldn't open" $err]
             &&  (1 || [catch { socket www.yahoo.com 80 } ])
             && (![info exists ws_openMath(proxy,http)] ||
                 [catch { eval socket $ws_openMath(proxy,http) } ]) } {
            if { 0 == [tk_dialog .jil 0 "Connection Problem:$err" "" 0 \
                           {Configure proxy server?} {Connect Later}] } {
                fontDialog .top
            }
        } else {error "$toeval:\n   error: $err" }
    }
}
