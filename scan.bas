    dim mdir[4] 
    
	scan_acc(2)
    scan_dec(2)

    filtering("none", 3)
    nom_srate = 400

	scan_speed(1)		
    scan_start(1)
    scan_increment(5)

mdir[0] = 1
mdir[1] = 1
mdir[2] = 1
mdir[3] = 1
	inc_scan(mdir, "scramble", "xyzc", 5.0, 0.0, 0, 0)