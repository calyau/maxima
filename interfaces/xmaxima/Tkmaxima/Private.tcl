###### private.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

# a private way of storing variables on a window by window
# basis

proc makeLocal { win args } {
  foreach v $args {
     uplevel 1 set  $v \[oget $win $v\]
 }
}

proc linkLocal { win args } {
  foreach v $args {
      uplevel 1 upvar #0 _WinInfo${win}\($v) $v
 }
}

proc clearLocal { win } {
    global _WinInfo$win
       # puts "clearing info for $win in [info level 1]"

    catch { unset _WinInfo$win }
}
 

proc oset { win var val } {
  global _WinInfo$win
  set _WinInfo[set win]($var) $val
}

proc oarraySet { win vals } {
  global _WinInfo$win
  array set  _WinInfo$win $vals
}

proc oloc { win var } {
  return _WinInfo[set win]($var)
}

proc oarray { win  } {
  return _WinInfo[set win]
}

proc oget { win var } {
  global _WinInfo$win
  return [set _WinInfo[set win]($var)]
}


## endsource private.tcl
