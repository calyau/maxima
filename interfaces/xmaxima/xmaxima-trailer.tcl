if { "$tcl_platform(platform)" == "windows" } {
    global ws_openMath
     set ws_openMath(kill) [file join $ws_openMath(maxima_prefix) \
          bin winkill.exe]
    catch { load  [file join $ws_openMath(maxima_prefix) \
          bin tclwinkill.dll]  }
#    proc setIcon { w } {
#     global ws_openMath
#     winico set [winfo toplevel $w] [winico createfrom [file join \
#             $ws_openMath(maxima_prefix) src max.ico ]]
#    }
#    after 2000 { setIcon . }
#    after 6000 { setIcon . }
>
  } else {
    set ws_openMath(kill) kill
}

doit .maxima
