# interruptcheck.tcl -- interrupt Maxima through its interrupt channel.
#
# Plays xmaxima's part with xmaxima's own InterruptChannel.tcl: listens on a
# port, starts Maxima with -s <port> and a token in MAXIMA_INTERRUPT_TOKEN,
# accepts the main connection and then the channel. Starts a computation
# that would run for minutes, interrupts it through the channel, and checks
# that Maxima reports the interrupt and that the same session then still
# computes 1+41.
#
# Usage: tclsh interruptcheck.tcl <maxima-local> <lisp> <InterruptChannel.tcl>
#
# Prints "interruptcheck: PASS", "interruptcheck: SKIP" (exit 77, the Lisp
# has no threads, so Maxima can't offer a channel) or "interruptcheck: FAIL"
# with a reason (exit 1).

lassign $argv maxima lisp channelCode
source $channelCode

set key test
set output ""
set mainSocket ""
set deadline [expr {[clock seconds] + 120}]

proc finish { status message } {
    global mainSocket maximaPid
    puts "interruptcheck: $status"
    if {$message ne ""} {
        puts $message
    }
    puts "--- Maxima's output ---"
    puts $::output
    icClose $::key
    if {$mainSocket ne ""} {
        catch {puts $mainSocket "quit();"; flush $mainSocket}
        catch {close $mainSocket}
    }
    if {[info exists maximaPid]} {
        catch {exec kill $maximaPid}
    }
    exit [dict get {PASS 0 SKIP 77 FAIL 1} $status]
}

proc accept { sock host port } {
    global mainSocket
    if {$mainSocket ne ""} {
        icCandidate $::key $sock
        return
    }
    set mainSocket $sock
    fconfigure $sock -blocking 0 -translation lf -encoding utf-8
    fileevent $sock readable [list readMaxima $sock]
}

proc readMaxima { sock } {
    append ::output [read $sock]
    if {[eof $sock]} {
        finish FAIL "Maxima closed the connection."
    }
}

# Waits until Maxima's output matches PATTERN (counting from offset FROM),
# or fails after the deadline.
proc waitFor { pattern from what } {
    while {![regexp -start $from -- $pattern $::output]} {
        if {[clock seconds] > $::deadline} {
            finish FAIL "Timed out waiting for $what."
        }
        after 100 {set ::tick 1}
        vwait ::tick
    }
}

proc send { text } {
    puts $::mainSocket $text
    flush $::mainSocket
}

set server [socket -server accept -myaddr 127.0.0.1 0]
set port [lindex [fconfigure $server -sockname] 2]

set token [icNewToken]
if {[string length $token] < 32} {
    finish FAIL "icNewToken returned the short token '$token'."
}
icExpect $key $token {set ::channelOpen 1}
set env(MAXIMA_INTERRUPT_TOKEN) $token
set maximaPid [exec $maxima --no-init -q --lisp=$lisp -s $port \
                   >@ stdout 2>@ stderr &]

waitFor {\(%i1\)} 0 "Maxima's first prompt"

# Does this Lisp offer a channel at all?
set from [string length $output]
send {:lisp (format t "channel-available=~a~%" (maxima::interrupt-channel-available-p))}
waitFor {channel-available=(T|NIL)} $from "the answer whether threads exist"
if {[regexp -start $from {channel-available=NIL} $output]} {
    finish SKIP "This Lisp has no threads, so Maxima offers no interrupt channel."
}

# The main connection is answered by now, so the channel's connection was
# made long ago; give the event loop a moment to finish the handshake.
set channelDeadline [expr {[clock milliseconds] + 10000}]
while {![icIsOpen $key] && [clock milliseconds] < $channelDeadline} {
    after 50 {set ::tick 1}
    vwait ::tick
}
if {![icIsOpen $key]} {
    finish FAIL "Maxima didn't open its interrupt channel."
}

# Something that runs for minutes. Interrupt it once it has started.
set from [string length $output]
send {for i:1 thru 10^12 do i;}
after 1500 {set ::tick 1}
vwait ::tick
if {![icSendInterrupt $key]} {
    finish FAIL "Could not write to the interrupt channel."
}
waitFor {User interrupt} $from "Maxima to report the interrupt"
waitFor {\(%i\d+\)} $from "the prompt after the interrupt"

# The session survived it.
set from [string length $output]
send {1+41;}
waitFor {42} $from "the result of 1+41"

# A second interrupt works, too: the channel thread survives the first.
set from [string length $output]
send {for i:1 thru 10^12 do i;}
after 1000 {set ::tick 1}
vwait ::tick
icSendInterrupt $key
waitFor {User interrupt} $from "Maxima to report the second interrupt"

finish PASS ""
