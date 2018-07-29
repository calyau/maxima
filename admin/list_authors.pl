#!/usr/bin/perl
# Create Maxima's "AUTHORS" file from the Git log.
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.


use strict;
use warnings;
use List::MoreUtils qw(uniq);


my $headtext=<<'ENDHEAD';
Maxima's development history spans three distinct periods: The
research project at MIT, the stewardship of William Schelter and the
current Maxima project.

MACSYMA (Project MAC's SYmbolic MAnipulation System) was developed by
the Mathlab group of the MIT Laboratory for Computer Science
(originally known as Project MAC), during the years 1969-1972.  Their
work was supported by grants NSG 1323 of the National Aeronautics and
Space Administration, N00014-77-C-0641 of the Office of Naval
Research, ET-78-C-02-4687 of the U.S. Department of Energy, and
F49620-79-C-020 of the U.S. Air Force.  MACSYMA was further modified
for use under the UNIX operating system (for use on DEC VAX computers
and Sun workstations), by Richard Fateman and colleagues at the
University of California at Berkeley; this version of MACSYMA is known
as VAXIMA.

William Schelter developed and maintained this version, Maxima, from
the project's inception until his untimely death in 2001. We are
eternally grateful for his enormous contribution.

William Schelter thanked the following people for having tested
the code under various common lisp implementations, and for helpful
comments:

hagiya%kurims.kurims.kyoto-u.junet%utokyo-relay.csnet@RELAY.CS.NET
  		(in kcl aosv)
steve@spock.ncsa.uiuc.edu  (Steve on sun/franz common lisp)
spar!malcolm@decwrl.dec.com (malcolm) sun/lucid
raible@orville.nas.nasa.gov (Eric Raible on iris(kcl))
fateman@peoplesparc.Berkeley.EDU (Richard Fateman)


In November, 2001, the Maxima project moved to the Sourceforge
project hosting site. The following people contributed to the Maxima
sourcecode (according to Git logs):

ENDHEAD


my $foottext=<<'ENDFOOT';

Maxima also includes contributions from:

Michel van den Bergh
Karl Berry
Salvador Bosch Pérez
Fedor Bezrukov
Fabrizio Caruso
Ari Constancio
Gosei Furuya
Juan Pablo Hierro Álvarez
Vadim Konovalov
Tim Moore
Guenther Nowak
Kostas Oikonomou
Edmond Orignac
Valerij Pipin
Jose Ramirez Labrador
Ole Rohne
Thomas A. Russ
Marek Rychlik

Starting with version 5.9.0, Maxima uses mk::defsystem and (slightly
modified) run-lisp from the Common Lisp Open Code Collection,
<http://clocc.sourceforge.net>.

The nregex code was written by Lawrence E. Freil.

ENDFOOT

# get current year
my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();
$year = $year+1900;

print $headtext;

# the first year is 2000 (git log | tail)
for (my $y=$year; $y >= 2000; $y--) {
    print "Contributors in $y\n====================\n";
    my $since = $y . '-01-01';
    my $until = $y . '-12-31';
    my @authors=`git log --since=$since --until=$until --format='format:%an'`;

    foreach my $i (0 .. $#authors) {
        chomp $authors[$i];
        # correct author names:
        # This should be finished, before the "AUTHORS" file is generated
        # But I do not know every Git log name <=> Realname
        # completed (as good as possible) using https://sourceforge.net/p/maxima/_members/
        $authors[$i] =~ s/^amundson$/James Amundson/;
        $authors[$i] =~ s/^andrejv$/Andrej Vodopivec/;
        $authors[$i] =~ s/^are_muc$/Andreas Eder/;
        $authors[$i] =~ s/^Andreas Eder \(are_muc\)$/Andreas Eder/;
        $authors[$i] =~ s/^belanger$/Jay Belanger/;
        $authors[$i] =~ s/^beshenov$/Alexey Beshenov/;
        $authors[$i] =~ s/^billingd$/David Billinghurst/;
        $authors[$i] =~ s/^crategus$/Dieter Kaiser/;
        $authors[$i] =~ s/^dgildea$/Dan Gildea/;
        $authors[$i] =~ s/^dstang$/Dan Stanger/;
        $authors[$i] =~ s/^dtc$/Douglas Crosher/;
        $authors[$i] =~ s/^fabriziocaruso$/Fabrizio Caruso/;
        $authors[$i] =~ s/^FabrizioCaruso$/Fabrizio Caruso/;
        $authors[$i] =~ s/^Fabrizio CARUSO$/Fabrizio Caruso/;
        $authors[$i] =~ s/^ficmatinfmag$/Jorge Barros de Abreu/;
        # unknown: Author: grozin <grozin>
        $authors[$i] =~ s/^hgeyer$/Harald Geyer/;
        $authors[$i] =~ s/^jpromerobx-max$/Juan Pablo Romero Bernal/;
        $authors[$i] =~ s/^l_butler$/Leo Butler/;
        $authors[$i] =~ s/^macrakis$/Stavros Macrakis/;
        $authors[$i] =~ s/^mhw2$/Mark H. Weaver/;
        $authors[$i] =~ s/^Mark H Weaver$/Mark H. Weaver/; # same spelling as above.
        $authors[$i] =~ s/^mikeclarkson$/Mike Clarkson/;
        $authors[$i] =~ s/^riotorto$/Mario Rodriguez Riotorto/;
        $authors[$i] =~ s/^Mario Rodriguez$/Mario Rodriguez Riotorto/; # same spelling as above
        $authors[$i] =~ s/^robert_dodier$/Robert Dodier/;
        $authors[$i] =~ s/^Robert$/Robert Dodier/;  # unknown, git log entry was: Robert <robert@robert-laptop.(none)> - perhaps Robert Dodier?
        $authors[$i] =~ s/^rtoy$/Raymond Toy/;
        $authors[$i] =~ s/^starseeker$/Cliff Yapp/;
        $authors[$i] =~ s/^van_nek$/Volker van Nek/;
        $authors[$i] =~ s/^villate$/Jaime E. Villate/;
        $authors[$i] =~ s/^Jaime Villate$/Jaime E. Villate/; # same spelling as above
        $authors[$i] =~ s/^vttoth$/Viktor T. Toth/;
        $authors[$i] =~ s/^vvzhy$/Vadim V. Zhytnikov/;
        $authors[$i] =~ s/^wfs$/William F. Schelter/; # unknown (Author: wfs <wfs>), I assume, that is William F. Schelter?
        $authors[$i] =~ s/^willisbl$/Barton Willis/;
        $authors[$i] =~ s/^wjenkner$/Wolfgang Jenkner/;
        # unknown: Author: xefe <xefe@xefe.(none)>
        $authors[$i] =~ s/^yasu-honda$/Yasuaki Honda/;
        $authors[$i] =~ s/^yycamm$/Camm Maguire/;
    }
    @authors=sort(@authors);
    @authors=uniq(@authors);
    
    foreach my $i (0 .. $#authors) {
        print $authors[$i] . "\n";
    }
    print "\n";
}
print $foottext;


