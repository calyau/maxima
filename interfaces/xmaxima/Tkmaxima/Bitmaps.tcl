###### Bitmaps.tcl ######
############################################################
# Netmath       Copyright (C) 1998 William F. Schelter     #
# For distribution under GNU public License.  See COPYING. # 
############################################################

global xHMulBMPdata
set xHMulBMPdata ""
lappend xHMulBMPdata "#define disc_width 6\n#define disc_height 6
static unsigned char disc_bits[] = {
   0xde, 0xff, 0xff, 0xff, 0xff, 0xde};"
    
lappend xHMulBMPdata  "#define circ_width 8\n#define circ_height 8
static unsigned char circ_bits[] = {
   0x3c, 0x42, 0x81, 0x81, 0x81, 0x81, 0x42, 0x3c};"

lappend xHMulBMPdata "#define rect_width 11\n#define rect_height 11
static unsigned char rect_bits[] = {
   0xff, 0x07, 0x01, 0x04, 0x01, 0x04, 0x01, 0x04, 0x01, 0x04, 0x01, 0x04,
   0x01, 0x04, 0x01, 0x04, 0x01, 0x04, 0x01, 0x04, 0xff, 0x07};"




    

## endsource bitmaps.tcl
