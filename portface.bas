dim pt[4,4]
t = 0
pfxn = 0
pfyn = 0
pfzn = 0
pfzint = 0

stop = 0

while (stop = 0)
	for i = 0 to 3
		if (t = 0)
			if (i > 0) inc_move("x", 5)
			msg = "position the probe to measure port face point " + str(i + 1) + " of 4"
			display(msg)
		else
			xm = pt[0, i]
			ym = pt[1, i]
			zm = pt[2, i]			
			move("x", xm + 2)
			move("yz", ym, zm)
		endif

		touch(-1, 0, 0, 0)
		read_position(pt[i])		
	next i

	lsq_plane(pt, 4, pfxn, pfyn, pfzn, pfzint)

	for i = 0 to 3
		if (i = 0)
			ymax = pt[1, i]
			ymin = ymax
			zmax = pt[2, i]
			zmin = zmax
		else
			if (pt[1, i] > ymax) ymax = pt[1, i]
			if (pt[1, i] < ymin) ymin = pt[1, i]
			if (pt[2, i] > zmax) zmax = pt[2, i]
			if (pt[2, i] < zmin) zmin = pt[2, i]
		endif
	next i

	yerr = pfyn * (ymax - ymin)
	zerr = pfzn * (zmax - zmin)

	msg = "y error " + str(yerr) + chr(13) + "z error " + str(zerr) + chr(13) + "ok to repeat?"
	stop = display(msg)	
	t = t + 1
wend

end
	