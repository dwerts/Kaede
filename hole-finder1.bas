'"simple" hole finder. Made by Dan.
'This serves as a prototype for measuring the long bar for machine alignment testing. 

block_len_mm = 4 * 25.4 'this should be modifiable at will to allow for a baseline of block length. 

msg = "program starting"
display_status(msg)

gosub SETUP_FILE_SUB
fprint #1,"Hole, Hole_edge, Y1,Y2,";chr(13);chr(10)
num_locs = 2
loc_start = 0
loc_inc = 300

'for DSP_ALL_SUB
dim probe[3] dim dspall[7] dim axis[4]

'to set axis move speeds, etc
move_factor = 3	'1, 2, 3, 4, 5
min_slew = 10 * move_factor	'10, 20, 30, 40, 50
max_slew = 20 * move_factor	'20, 40, 60, 80, 100
oversamp = 30 * move_factor
slew_accel = 40 + 60 * move_factor
slew_decel = slew_accel
scan_accel = 40 + 30 * (move_factor - 1)
scan_decel = scan_accel  'Not sure if this section is even needed. Run a beta with this removed...

'update: It was VERY much needed...

slew_speed(max_slew)
slew_acc(slew_accel)
slew_dec(slew_decel)
scan_acc(scan_accel)
scan_dec(scan_decel)

gosub SETUP_FILE_SUB
dim ref[3] dim axis[4]
dim start_pt[4]

get_probe_ref(ref) 'rem get the probe offsets for the current probe
target_defl = 0.250
gear_probe_tip = ?
cur_probe_tip = gear_probe_tip
'this will update probe dia and probe_rad
gosub PROBE_CHANGE_SUB

gosub DSP_ALL_SUB
start_pt[0] = last_x
start_pt[1] = last_y
start_pt[2] = last_z
gosub DSP_ALL_SUB
fprint #1," ,",last_x, last_y, last_z

'Todo: put in every hole measure section...

xref = ref[0]
yref = ref[1]
zref = ref[2]
x_pos_lim = get_axis_param(0, 0) - 1.0
x_neg_lim = get_axis_param(0, 1) + 1.0
y_neg_lim = get_axis_param(1, 0) - 1.0
y_pos_lim = get_axis_param(1, 1) + 1.0
z_neg_lim = get_axis_param(2, 0) - 1.0
z_pos_lim = get_axis_param(2, 1) + 1.0

display("jog probe in front of block")

msg = "Proceeding with alignment phase"
display_status(msg)

'touch the block near the front edge
touch(-1,0,0,0) '-1 indicates pressing into the block'
gosub DSP_ALL_SUB
'store the second x location

sec_x = xc
sec_y = yc
gosub DSP_ALL_SUB
fprint #1," ", xc, yc
inc_move("x", 5.0) 'This should be the first move backwards which is in millimeters


'move over x mm and touch again
inc_move("y", 76.0)
touch(-1,0,0,0)
gosub DSP_ALL_SUB
inc_move("x", 5.0) 'changed what I think is the length backwards'
'store the first position
first_x = xc
first_y = yc

gosub DSP_ALL_SUB
fprint #1, xc, yc

'calculate the angular error and adjust the block square with y
msg = "Calculating and correcting angular error..."
display_status(msg)

delta_x = first_x - sec_x
delta_y = first_y - sec_y
ang_err = DEGREES(atn2(delta_x, delta_y))

msg = "No serious error..."
display_status(msg)

inc_move("c", ang_err)
display("Adjustment complete")

'move back over to the start point, then touch again

msg = "Top alignment test underway..."
display_status(msg)
inc_move("y", -76.0)
touch(-1,0,0,0)
gosub DSP_ALL_SUB
surf_loc_x = xc - probe_rad
meas_loc_x = surf_loc_x - probe_diameter

'increase the deflection to the target value
inc_move("x", probe[0] - target_defl)


'find the front edge
find(0,-50, 0, 0)
gosub DSP_ALL_SUB

'store the approximate y edge location
left_edge_y = last_y - probe_diameter
move("y", left_edge_y)
move("x", meas_loc_x)

'touch the left edge of the block and store the location
touch(0, 1, 0, 0)
gosub DSP_ALL_SUB
left_edge_blk = yc + probe_rad
left_edge_z_meas = zc
left_edge_yc = yc

'set deflection
inc_move("y", probe[1] + target_defl)
find(0, 0, 20, 0)
gosub DSP_ALL_SUB
top_edge_block = zc + probe_diameter
fprint #1, top_edge_block = zc +probe_diameter

move("z", top_edge_block)
move("y", left_edge_blk + 5.0)
touch(0, 0, -1, 0)
gosub DSP_ALL_SUB

top_edge_block = zc - probe_rad
move("z", top_edge_block + probe_diameter)
inc_move("y", block_len_mm - 10.0)
touch(0, 0, -1, 0)

gosub DSP_ALL_SUB
top_edge_block_rt = zc - probe_rad
move("z", top_edge_block + probe_diameter)
move("y", left_edge_blk + block_len_mm + probe_diameter + 1)
move("z", left_edge_z_meas)
touch(0, -1, 0, 0)

gosub DSP_ALL_SUB
right_edge_blk = yc - probe_rad
right_edge_yc = yc
fprint #1, right_edge_bk
fprint #1, right_edge_yc 
inc_move("y", 1.0)
move("x", surf_loc_x + 1.0 + probe_rad)
inc_move ("x", 4)
inc_move("y", -100)

'touch back to start location.

msg = "Proceeding with hole identification"

'touch the block near the front edge - dev note. start of hole ID for loop purposes. 
touch(-1,0,0,0)
gosub DSP_ALL_SUB

'find the hole 

msg = "Finding hole"
display_status(msg)
find(0,50, 0, 0)
gosub DSP_ALL_SUB

'store the approximate y hole location
left_hole_y = last_y - probe_diameter
right_hole_y = first_y - probe_diameter 

fprint #1," ,",left_hole_y, right_hole_y
inc_move("y", 5.0)
inc_move("x", -8.0)

'end of finding phase. Should be able to consolidate this into 
'a loop and replace other hole finding routines in program....

msg = "Measuring hole 1"
display_status(msg)

'find the edge of the hole. should be able to loop this as well. 

find(0,-10,0,0)
touch(0,-1,0,0)

gosub DSP_ALL_SUB
fprint #1," ,", P1_S1

' find other edge of hole... 

find(0,10,0,0)
touch(0,1,0,0)
gosub DSP_ALL_SUB
fprint #1, ", ",P1_S2
inc_move("x", 10.0)

'Move to center of next face. End of this routine. 
'Should be able to consolidate for other sections...
'TODO: working on it...

inc_move("y", 6.0)
touch(-1,0,0,0)
gosub DSP_ALL_SUB
find(0,-50,0,0)

gosub DSP_ALL_SUB
fprint #1," ,",axis[1] 'might be edge of surface...

inc_move("x", 1)
inc_move("y", 1)
touch(-1,0,0,0)
find(0,50,0,0)

gosub DSP_ALL_SUB
fprint #1," ,",axis[1] 
'Finding the edge of the 2nd hole here. 
'Hole 2

msg = "Examining hole 2"

'this section should be the trial for the subroutines...
display_status(msg)

gosub HOLE_MEASURE_SUB
gosub DSP_ALL_SUB
fprint #1," ,",P2_S1

Gosub hole_opposite_direction
gosub DSP_ALL_SUB
fprint #1," ,",P2_S2

gosub face_measure_sub
msg = "Hole 3 found"
msg = "Examining hole 3"

display_status(msg)
display_status(msg)

gosub HOLE_MEASURE_SUB
gosub DSP_ALL_SUB
fprint #1," ,",P3_S1 'should be saving the hole location here...

Gosub hole_opposite_direction
gosub DSP_ALL_SUB
fprint #1," ,",P3_S2

gosub face_measure_sub

'Hole 4 found
msg = "Examining hole 4"
display_status(msg)

'marked for consolidation...
gosub HOLE_MEASURE_SUB
gosub DSP_ALL_SUB
fprint #1," ,",P4_S1
find(0,10,0,0)
touch(0,1,0,0)

gosub DSP_ALL_SUB
fprint #1," ,",P4_S2
fprint #1," ,",P4_S2-P4_S1
gosub face_measure_sub

'Hole 5 measurement phase

msg = "Examining hole 5"
display_status(msg)
gosub HOLE_MEASURE_SUB
gosub DSP_ALL_SUB
fprint #1," ,",P5_S1

find(0,10,0,0)
touch(0,1,0,0)
gosub DSP_ALL_SUB
fprint #1," ,",P5_S2
fprint #1," ,",P5_S2-P5_S1
inc_move("x", 5)
inc_move("y", 3)

'Find the back edge of the block

msg = "Finding back edge of block"
display_status(msg)
touch(-1,0,0,0)
find(0,-50, 0, 0)

'This finds the edge of the hole. 

inc_move("x", 5)
inc_move("y", 1)
touch(-1,0,0,0)
find(0,50,0,0)

gosub DSP_ALL_SUB

inc_move("x", 4)
inc_move("y", -97)

'following block is for doing measurements for the block length and error. 

cos_top_edge = cos(atn2(top_edge_block - top_edge_block_rt, block_len_mm - 10))
ctr_to_ctr = right_edge_yc - left_edge_yc
block_len = ctr_to_ctr * cos_top_edge - probe_diameter
block_len_inch = block_len / 25.4
block_len_error = block_len - block_len_mm
fprint #1, str(block_len_inch)

print("block measures: " + str(block_len_inch) + ", error = " + str(block_len_error * 1000) + " microns")

msg = "Measurement of block complete"
display_status(msg)
display("End of test")
end

DSP_ALL_SUB
	read_dsp(dspall, oversamp)
	probe[0] = dspall[0] 
	probe[1] = dspall[1] 
	probe[2] = dspall[2] 
	axis[0] = dspall[3] 'x
	axis[1] = dspall[4] 'y
	axis[2] = dspall[5] 'z
	axis[3] = dspall[6] 'w
	last_x = axis[0]
	last_y = axis[1]
	last_z = axis[2]
	last_c = axis[3]
	xc = probe[0] + axis[0]
	yc = probe[1] + axis[1]
	zc = probe[2] + axis[2]
	P1_S1 = probe[1] + axis[1]
	P1_S2 = probe[1] + axis[1]
	P2_S1 = probe[1] + axis[1]
	P2_S2 = probe[1] + axis[1]
	P2_S1 = probe[1] + axis[1]
	P2_S2 = probe[1] + axis[1]
	P3_S1 = probe[1] + axis[1]
	P3_S2 = probe[1] + axis[1]
	P4_S1 = probe[1] + axis[1]
	P4_S2 = probe[1] + axis[1]
	P5_S1 = probe[1] + axis[1]
	P5_S2 = probe[1] + axis[1]
	display_status(msg)
	return

SETUP_FILE_SUB
	filename = "c:\penta\data\Hole_Data-"
	filename = filename + date + "-"
	tstr = time
	filename = filename + left(tstr, 2) + mid(tstr, 4, 2) + ".rtf"
	kill(filename)
	file_err = 1
	file_err = fopen(filename, writing, sequential, 1)
	'if(file_err <> -1)
		'display("CANNOT OPEN " + filename)
		'end
	'endif
	return
	
PROBE_CHANGE_SUB
    probe_diameter = change_rprobe_id(cur_probe_tip)
    probe_rad = .5 * probe_diameter
    probe_id = get_probe_name()
    return ' PROBE_CHANGE_SUB
'potentially able to remove probe change sub...
	
HOLE_MEASURE_SUB
	inc_move("x", 5)
	inc_move("y", 5)
	inc_move("x", -8)
	find(0,-10,0,0)
	touch(0,-1,0,0)
	return

hole_opposite_direction
	find(0,10,0,0)
	touch(0,1,0,0)
	return

DEFFN RADIANS(deg) = deg * pi / 180.0
DEFFN DEGREES(rad) = rad * 180.0 / pi
DEFFN CLEN(ce, cs) = (ce - cs) - 360.0 * sgn(ce - cs) * (abs(ce - cs) > 180.0)

face_measure_sub
	inc_move("x", 10)
	inc_move("y", 6.0)
	touch(-1,0,0,0)
	gosub DSP_ALL_SUB
	find(0,-50,0,0)
	inc_move("x", 1)
	inc_move("y", 1)
	touch(-1,0,0,0)
	find(0,50,0,0)
	return
	
'What is needed is a way to turn a long series of codes into short references to loops. 
'Then to do a call to them for each stage. This should allow for the pgm to be shortened.
