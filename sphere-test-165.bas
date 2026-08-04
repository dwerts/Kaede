' set the diameter of the sphere being used
sphere_diam = 8.0

sleep

' set the max angle from 3 o'clock to test (normally 90 degrees)
max_angle = 70.0

' set the table increment in degrees
table_increment = 5.0

' z height gap
z_gap = 10.0


msg = "program starting"

display_status(msg)

' get probe reference values so we can back figure where we are in the travel.
dim ref[3]
get_probe_ref(ref)

touch_pts = 7

xc = 0.0
yc = 0.0
zc = 0.0
radius = 0.0

gosub SPHERE_NORMS_SUB

'for DSP_ALL_SUB
dim probe[3] dim dspall[7] dim axis[4]


' to set axis move speeds, etc
move_factor = 5			'1, 2, 3, 4, 5
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


'
'
'	START THE PROGRAM HERE...
'
'
'
display("jog probe above sphere and select OK to start measurements")


' get the file open, and record the labels as the first row.
gosub SETUP_FILE_SUB
fprint #1, "Sphere Test";chr(13);chr(10)
fprint #1, "Sphere Diameter,"; sphere_diam; chr(13);chr(10)
fprint #1, "Max Angle,"; max_angle; chr(13);chr(10)
fprint #1, "Table Increment,"; table_increment; chr(13);chr(10)

' locations for setting up - 2 locations, front and back
' start angle - go ccw 'max_angle' from 360 (3 o'clock)
start_angle = 360 - max_angle
fprint #1, "Start Angle,";start_angle;chr(13);chr(10)
' end angle is max_angle from zero (3 0'clock)
end_angle = max_angle
fprint #1, "End Angle,"; end_angle; chr(13); chr(10)

num_locs = 2
loc_start = start_angle
loc_end = end_angle


dim sphere_locs[num_locs, 2]

' read the current z position and record as the approach height for now.
gosub DSP_ALL_SUB
' make starting height the approach height
z_appr = last_z

' create a clearance height by adding the specified gap
z_clear = z_appr + z_gap

' measure the sphere right where we are - need to be over the sphere when starting.
msg = "Measuring sphere at start point"
gosub MEASURE_SPHERE_SUB

' initial angle of the sphere where we started.
initial_c_angle = DEGREES(atn2(yc, xc))

' read the table position
gosub DSP_ALL_SUB
initial_c_pos = last_c

' now adjust the approach and clear heights for the center location we just found
z_appr = zc + radius + 2
z_clear = z_appr + z_gap

' calculate the table offset (distance between our measured angle and the table position)
c_offset = initial_c_pos - initial_c_angle

' set up to move to the start location (y front)
next_c = fmod(loc_start + c_offset, 360)

radius_of_rotation = sqr(axis[0]^2 + axis[1]^2)
fprint #1, "Radius of Rotation,"; radius_of_rotation; chr(13);chr(10)
' store the angle of the start and end locations
sphere_locs[0, 0] = fmod(360.0 + loc_start, 360.0)
sphere_locs[1, 0] = fmod(360.0 + loc_end, 360.0)

' calculate the arc move to go to the start angle
arc_to_rad = radius_of_rotation
arc_to_x = cos(RADIANS(sphere_locs[0, 0])) * radius_of_rotation
arc_to_y = sin(RADIANS(sphere_locs[0, 0])) * radius_of_rotation
arc_to_c = next_c
arc_to_z = z_clear

' make the move to the start angle
arc_move(arc_to_rad, arc_to_y, arc_to_z, arc_to_c)

' measure the sphere at the start angle
msg = "Measuring sphere at loc_start degrees..."
gosub MEASURE_SPHERE_SUB
xc1 = xc
yc1 = yc

' store a new c offset
gosub DSP_ALL_SUB
c_offset = last_c - loc_start

' move a degree to get off 270 when we are starting at 270...
move("cy", loc_start + 1 + c_offset, yc + 0.05)

arc_to_x = cos(RADIANS(sphere_locs[1, 0])) * radius_of_rotation
arc_to_y = sin(RADIANS(sphere_locs[1, 0])) * radius_of_rotation
' go to y plus end (270 - 180 = 90, or 240 - 140 = 100, e.g.)
arc_to_c = loc_end + c_offset
arc_move(arc_to_rad, arc_to_y, arc_to_z, arc_to_c)

gosub MEASURE_SPHERE_SUB
xc2 = xc
yc2 = yc

xc_avg = (xc1 + xc2) / 2

delta_y = (abs(yc1) + abs(yc2)) / 2

x_adj = xc_avg - xc2

c_adj = DEGREES(atn2(x_adj, delta_y))

c_offset = c_offset - c_adj

' 89, or 79 e.g.
move("cy", loc_end -1 + c_offset, yc -0.05)

arc_to_x = cos(RADIANS(sphere_locs[0, 0])) * radius_of_rotation
arc_to_y = sin(RADIANS(sphere_locs[0, 0])) * radius_of_rotation
arc_to_c = loc_start + c_offset
arc_move(arc_to_rad, arc_to_y, arc_to_z, arc_to_c)

' 290 to 70
' e.g. 70 degrees angle, 5 degrees increment
' 70 * 2 / 5 + 1 =
' 140 / 5 + 1 = 
' 28 + 1 =
' 29
num_locs = max_angle * 2 / table_increment + 1
loc_start = start_angle
loc_inc = table_increment

dim sphere_locs[num_locs, 2]	'nominal angle at index 0, actual angle at index 1

for i = 0 to num_locs - 1
	sphere_locs[i, 0] = fmod(360.0 + loc_start + loc_inc * i, 360.0)
next i

fprint #1, "Loc, Xc, Yc, Zc, Rs+p, , X, Y, Z";chr(13);chr(10)

for i = 0 to num_locs - 1
	arc_to_x = cos(RADIANS(sphere_locs[i, 0])) * radius_of_rotation
	arc_to_y = sin(RADIANS(sphere_locs[i, 0])) * radius_of_rotation
	arc_to_z = z_clear
	arc_to_c = sphere_locs[i, 0] + c_offset
	arc_move(arc_to_rad, arc_to_y, arc_to_z, arc_to_c)


	msg = "INDEX " + str(i) + "Measuring sphere at " + str(sphere_locs[i, 0]) + " degrees"

	gosub MEASURE_SPHERE_SUB

	fprint #1, sphere_locs[i, 0]; ","; xc; ","; yc; ","; zc; ","; radius
	fprint #1, ",,"; xc + ref[0]; ","; yc + ref[1]; ","; zc + ref[2]; chr(13); chr(10)

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

SPHERE_NORMS_SUB
' set up the normals that we will touch the sphere with
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
	return



DEFFN RADIANS(deg) = deg * pi / 180.0
DEFFN DEGREES(rad) = rad * 180.0 / pi
DEFFN CLEN(ce, cs) = (ce - cs) - 360.0 * sgn(ce - cs) * (abs(ce - cs) > 180.0)




