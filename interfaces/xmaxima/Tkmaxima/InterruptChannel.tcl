############################################################
# InterruptChannel.tcl                                     #
# For distribution under GNU public License.  See COPYING. #
#                                                          #
############################################################
#
# Interrupting Maxima without signals.
#
# On MS Windows there is no kill(SIGINT). Without this channel xmaxima runs
# winkill.exe instead, which sets a bit in a shared-memory segment that
# Maxima's Lisp watches -- but only if maxima.bat loaded win_signals.lisp and
# winkill_lib.dll, and virus scanners tend to object to a small console
# program writing into another process's memory.
#
# So xmaxima passes Maxima a random token in the environment variable
# MAXIMA_INTERRUPT_TOKEN and keeps listening on the port Maxima connects to.
# A Maxima whose Lisp has threads then opens a second connection, sends the
# token as its first line, and interrupts its computation each time it reads
# the line "interrupt" there (see "The interrupt channel" in
# src/server.lisp). A Lisp without threads never opens the channel, and
# xmaxima falls back to sending a signal.
#
# These procedures need neither Tk nor the rest of xmaxima, so that the test
# suite can drive them with a plain tclsh. All state lives in the array
# ::icState, indexed by a key the caller chooses (xmaxima uses the console's
# text widget).

# icNewToken --
#
#   Returns a fresh token that is hard to guess.
#
#   Read from /dev/urandom where there is one. Tcl itself has no
#   cryptographic random source, so elsewhere (MS Windows) the token is
#   mixed from the clock, the process id and Tcl's rand(). Guessing it would
#   gain an attacker on the same machine little: the channel only ever
#   receives interrupt requests, it never sends Maxima any input.
#
proc icNewToken {} {
    set token ""
    if {![catch {open /dev/urandom rb} f]} {
        catch {
            binary scan [read $f 16] H* token
        }
        close $f
    }
    if {[string length $token] < 32} {
        expr {srand(([clock microseconds] ^ ([pid] << 16)) & 0x7fffffff)}
        set token ""
        for {set i 0} {$i < 4} {incr i} {
            set word [expr {int(rand() * 0x7fffffff) ^ [clock clicks]}]
            append token [format %08x [expr {$word & 0xffffffff}]]
        }
    }
    return $token
}

# icExpect --
#
#   Starts waiting for the channel of the Maxima belonging to KEY, which
#   will authenticate itself with TOKEN. Forgets any channel KEY had before.
#   ONACCEPT, if given, is evaluated once the channel is open.
#
proc icExpect { key token {onAccept ""} } {
    icClose $key
    set ::icState($key,token) $token
    set ::icState($key,onAccept) $onAccept
}

# icCandidate --
#
#   Hands SOCK, a connection that arrived after Maxima's main connection, to
#   the channel's handshake. It becomes the channel if its first line is the
#   expected token; otherwise it is closed.
#
proc icCandidate { key sock } {
    if {![info exists ::icState($key,token)] || [icIsOpen $key]} {
        catch {close $sock}
        return
    }
    fconfigure $sock -blocking 0 -translation lf -encoding utf-8
    fileevent $sock readable [list icHandshake $key $sock]
}

proc icHandshake { key sock } {
    if {[catch {gets $sock line} len] || $len < 0} {
        if {[catch {eof $sock} atEof] || $atEof} {
            catch {close $sock}
        }
        # Otherwise only part of the line has arrived yet.
        return
    }
    if {![info exists ::icState($key,token)] || [icIsOpen $key] || \
            $line ne $::icState($key,token)} {
        catch {close $sock}
        return
    }
    set ::icState($key,socket) $sock
    # From now on nothing is read; only notice when Maxima closes it.
    fileevent $sock readable [list icWatch $key $sock]
    if {$::icState($key,onAccept) ne ""} {
        uplevel #0 $::icState($key,onAccept)
    }
}

proc icWatch { key sock } {
    if {[catch {read $sock}] || [catch {eof $sock} atEof] || $atEof} {
        icClose $key
    }
}

# icIsOpen --
#
#   True if KEY's Maxima has an open interrupt channel.
#
proc icIsOpen { key } {
    return [info exists ::icState($key,socket)]
}

# icSendInterrupt --
#
#   Asks KEY's Maxima to interrupt its computation. Returns 1 if the request
#   went out, 0 if there is no channel and the caller has to fall back to a
#   signal.
#
proc icSendInterrupt { key } {
    if {![icIsOpen $key]} {
        return 0
    }
    set sock $::icState($key,socket)
    if {[catch {
        puts $sock interrupt
        flush $sock
    }]} {
        icClose $key
        return 0
    }
    return 1
}

# icClose --
#
#   Closes KEY's channel, if any, and stops expecting one.
#
proc icClose { key } {
    if {[info exists ::icState($key,socket)]} {
        catch {close $::icState($key,socket)}
    }
    array unset ::icState $key,*
}
