# -*-mode: perl; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#	$Id: maxima.perl,v 1.1 2002-09-24 00:32:47 mikeclarkson Exp $
#

&ignore_commands( <<_IGNORED_CMDS_);
CMMCmd # {} # {}
_IGNORED_CMDS_

sub do_cmd_Maxima {
    join('',"<TT>Maxima</TT>",@_[0]);
}

1;
 
