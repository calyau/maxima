pdffiles =  draw_scene.pdf draw_equiv.pdf draw_axis3d.pdf draw_axis_bottom.pdf
giffiles =  draw_scene.gif draw_equiv.gif draw_axis3d.gif draw_axis_bottom.gif
pdffiles += draw_border.pdf
giffiles += draw_border.gif
all: $(pdffiles)
all: $(giffiles)

draw_scene.pdf: draw_scene.mac docdraw.mac
draw_scene.gif: draw_scene.mac docdraw.mac
draw_equiv.pdf: draw_equiv.mac docdraw.mac
draw_equiv.gif: draw_equiv.mac docdraw.mac
draw_axis3d.pdf: draw_axis3d.mac docdraw.mac
draw_axis3d.gif: draw_axis3d.mac docdraw.mac
draw_axis_bottom.pdf: draw_axis_bottom.mac docdraw.mac
draw_axis_bottom.gif: draw_axis_bottom.mac docdraw.mac
draw_border.pdf: draw_border.mac docdraw.mac
draw_border.gif: draw_border.mac docdraw.mac


.mac.pdf:
	maxima < $<

.mac.gif:
	maxima < $<
