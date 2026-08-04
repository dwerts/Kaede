z_clear = 50.0

msg = "program starting"

display_status(msg)


sphere_diam = 19.05
touch_pts = 7

xc = 0.0
yc = 0.0
zc = 0.0
radius = 0.0


'for MEASURE_SPHERE_SUB

Dim sphere_norms[4, touch_pts]

sphere_norms[0, 0] = 0.0 'z- @top
sphere_norms[1, 0] = 0.0
sphere_norms[2, 0] = 1.0
sphere_norms[3, 0] = 0.0

sphere_norms[0, 1] = 0.0 'y+ @ front
sphere_norms[1, 1] = -1.0
sphere_norms[2, 1] = 0.0
sphere_norms[3, 1] = 0.0

sphere_norms[0, 2] = sin(RADIANS(45.0)) 'x-, y+ along 45
sphere_norms[1, 2] = sin(RADIANS(-45.0))
sphere_norms[2, 2] = 0.0
sphere_norms[3, 2] = 0.0

sphere_norms[0, 3] = 1.0 'X- @ right
sphere_norms[1, 3] = 0.0
sphere_norms[2, 3] = 0.0
sphere_norms[3, 3] = 0.0

sphere_norms[0, 4] = sin(RADIANS(45.0)) 'x-, y- along 45
sphere_norms[1, 4] = sin(RADIANS(45.0))
sphere_norms[2, 4] = 0.0
sphere_norms[3, 4] = 0.0

sphere_norms[0, 5] = 0.0 'y+ @ back
sphere_norms[1, 5] = 1.0
sphere_norms[2, 5] = 0.0
sphere_norms[3, 5] = 0.0

sphere_norms[0, 6] = 0.0 'z- @top
sphere_norms[1, 6] = 0.0
sphere_norms[2, 6] = 1.0
sphere_norms[3, 6] = 0.0


'for DSP_ALL_SUB
dim probe[3] dim dspall[7] dim axis[4]


' to set axis move speeds, etc
move_factor = 3			'1, 2, 3, 4, 5
min_slew = 10 * move_factor	'10, 20, 30, 40, 50
max_slew = 20 * move_factor	'20, 40, 60, 80, 100
oversamp = 30 * move_factor
slew_accel = 40 + 60 * move_factor
slew_decel = slew_accel
scan_accel = 40 + 30 * (move_factor - 1)
scan_decel = scan_accel

slew_speed(max_slew)
slew_acc(slew_accel)
slew_dec(slew_decel)
scan_acc(scan_accel)
scan_dec(scan_decel)



display("jog probe above sphere and select OK to start measurements")

gosub SETUP_FILE_SUB
fprint #1, "Loc, Xc, Yc, Zc, Rs+p";chr(13);chr(10)

num_locs = 2
loc_start = 270.0
loc_inc = 180.0

dim sphere_locs[num_locs, 2]


gosub DSP_ALL_SUB
z_appr = last_z
z_clear = z_appr + 50

msg = "Measuring sphere at start point"
gosub MEASURE_SPHERE_SUB

initial_c_angle = DEGREES(atn2(yc, xc))

gosub DSP_ALL_SUB
initial_c_pos = last_c

z_appr = zc + radius + 2
z_clear = z_appr + 40

c_offset = initial_c_pos - initial_c_angle

next_c = fmod(270 + c_offset, 360)


radius_of_rotation = sqr(axis[0]^2 + axis[1]^2)

arc_to_rad = radius_of_rotation
arc_to_y = -radius_of_rotation
arc_to_x = 0
arc_to_c = next_c
arc_to_z = z_clear
arc_move(arc_to_rad, arc_to_y, arc_to_z, arc_to_c)

msg = "Measuring sphere at 270 degrees..."
gosub MEASURE_SPHERE_SUB
xc1 = xc
yc1 = yc

gosub DSP_ALL_SUB
c_offset = last_c - 270

move("cy", 271 + c_offset, -radius_of_rotation + 0.05)

arc_to_x = 0
arc_to_y = radius_of_rotation
arc_to_c = 90 + c_offset
arc_move(arc_to_rad, arc_to_y, arc_to_z, arc_to_c)

gosub MEASURE_SPHERE_SUB
xc2 = xc
yc2 = yc

xc_avg = (xc1 + xc2) / 2

circle_radius = (abs(yc1) + abs(yc2)) / 2

x_adj = xc_avg - xc2

c_adj = DEGREES(atn2(x_adj, circle_radius))

c_offset = c_offset - c_adj

move("cy", 89 + c_offset, radius_of_rotation -0.05)

arc_to_x = 0
arc_to_y = -radius_of_rotation
arc_to_c = 270 + c_offset
arc_move(arc_to_rad, arc_to_y, arc_to_z, arc_to_c)

num_locs = 13
loc_start = 270.0
loc_inc = 15.0

dim sphere_locs[num_locs, 2]	'nominal angle at index 0, actual angle at index 1

for i = 0 to num_locs - 1
	sphere_locs[i, 0] = fmod(360.0 + loc_start + loc_inc * i, 360.0)
next i

for i = 0 to num_locs - 1
	arc_to_x = cos(RADIANS(sphere_locs[i, 0])) * radius_of_rotation
	arc_to_y = sin(RADIANS(sphere_locs[i, 0])) * radius_of_rotation
	arc_to_z = z_clear
	arc_to_c = sphere_locs[i, 0] + c_offset
	arc_move(arc_to_rad, arc_to_y, arc_to_z, arc_to_c)


	msg = "INDEX " + str(i) + "Measuring sphere at " + str(sphere_locs[i, 0]) + " degrees"

	gosub MEASURE_SPHERE_SUB

	fprint #1, sphere_locs[i, 0]; ","; xc; ","; yc; ","; zc; ","; radius; chr(13); chr(10)

	sphere_locs[i, 1] = DEGREES(atn2(yc, xc))
next i

display_status("Measurements Complete")
display("Measurements complete")
display_status("Test completed sucessfully")
if(file_err = 0) close#1
display_status("file saved")

end

MEASURE_SPHERE_SUB
	gosub MOVE_Z_APPROACH
	display_status(msg)
	locate_sphere(sphere_diam, sphere_norms, touch_pts, xc, yc, zc, radius)
	gosub MOVE_Z_CLEAR
	return

SETUP_FILE_SUB
	filename = "c:\penta\data\sphere_map_data-"
	filename = filename + date + "-"
	tstr = time
	filename = filename + left(tstr, 2) + mid(tstr, 4, 2) + ".csv"
	kill(filename)
	file_err = 1
	file_err = fopen(filename, writing, sequential, 1)

	if(file_err <> 0)
		display("CANNOT OPEN " + filename)
		end
	endif
	return


DSP_ALL_SUB
	read_dsp(dspall, oversamp)
	probe[0] = dspall[0]
	probe[1] = dspall[1]
	probe[2] = dspall[2]
	axis[0] = dspall[3]
	axis[1] = dspall[4]
	axis[2] = dspall[5]
	axis[3] = dspall[6]
	last_x = axis[0]
	last_y = axis[1]
	last_z = axis[2]
	last_c = axis[3]
	return

MOVE_Z_APPROACH
	display_status("Moving to Z approach height")
	move("z", z_appr)
	return

MOVE_Z_CLEAR
'	dispay_status("Moving to Z clear height")
	move("z", z_clear)
	return

DEFFN RADIANS(deg) = deg * pi / 180.0
DEFFN DEGREES(rad) = rad * 180.0 / pi
DEFFN CLEN(ce, cs) = (ce - cs) - 360.0 * sgn(ce - cs) * (abs(ce - cs) > 180.0)




