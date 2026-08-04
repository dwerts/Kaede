' program to measure the length of a jo block in y axis

'This will be a variation of the standard program I've been modifying...

block_len_mm = 6.399730 * 25.4

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


dim ref[3] dim axis[4]
dim start_pt[4]

get_probe_ref(ref)		rem get the probe offsets for the current probe

target_defl = 0.250

gear_probe_tip = ?
cur_probe_tip = gear_probe_tip
' this will update probe dia and probe_rad
gosub PROBE_CHANGE_SUB

gosub DSP_ALL_SUB
start_pt[0] = last_x
start_pt[1] = last_y
start_pt[2] = last_z


'print "x ref = " + str(ref[0]) + ", y ref = " + str(ref[1]) + ", z ref = " + str(ref[2])
xref = ref[0]
yref = ref[1]
zref = ref[2]
x_pos_lim = get_axis_param(0, 0) - 1.0
x_neg_lim = get_axis_param(0, 1) + 1.0
y_neg_lim = get_axis_param(1, 0) - 1.0
y_pos_lim = get_axis_param(1, 1) + 1.0
z_neg_lim = get_axis_param(2, 0) - 1.0
z_pos_lim = get_axis_param(2, 1) + 1.0






display("jog to position probe in front of block")

' touch the block near the front edge
touch(-1,0,0,0) '-1 indicates pressing into the block'
gosub DSP_ALL_SUB

' store the first x location
sec_x = xc
sec_y = yc
inc_move("x", 5.0) 'This should be the first move backwards'

' move over 150 mm and touch again
inc_move("y", 150.0)
touch(-1,0,0,0)
gosub DSP_ALL_SUB

inc_move("x", 5.0) 'changed what I think is the length backwards'

' store the second position
first_x = xc
first_y = yc

' calculate the angular error and adjust the block square with y
delta_x = first_x - sec_x
delta_y = first_y - sec_y
ang_err = DEGREES(atn2(delta_x, delta_y))
inc_move("c", ang_err)

display("This should be the end of the adjustment phase")

' move back over to the start point, then touch again
inc_move("y", -150)
touch(-1,0,0,0)
gosub DSP_ALL_SUB
surf_loc_x = xc - probe_rad
meas_loc_x = surf_loc_x - probe_diameter

' increase the deflection to the target value
inc_move("x", probe[0] - target_defl)

' find the front edge
find(0,-50, 0, 0)
gosub DSP_ALL_SUB

' store the approximate y edge location
left_edge_y = last_y - probe_diameter

move("y", left_edge_y)
move("x", meas_loc_x)

' touch the left edge of the block and store the location
touch(0, 1, 0, 0)
gosub DSP_ALL_SUB
left_edge_blk = yc + probe_rad
left_edge_z_meas = zc
left_edge_yc = yc

' set deflection
inc_move("y", probe[1] + target_defl)
find(0, 0, 20, 0)
gosub DSP_ALL_SUB
top_edge_block = zc + probe_diameter

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

inc_move("y", 1.0)
move("x", surf_loc_x + 1.0 + probe_rad)

'removed spurious code that had no purpose. 

move("xyz", start_pt[0], start_pt[1], start_pt[2])


cos_top_edge = cos(atn2(top_edge_block - top_edge_block_rt, block_len_mm - 10))

ctr_to_ctr = right_edge_yc - left_edge_yc
block_len = ctr_to_ctr * cos_top_edge - probe_diameter

block_len_inch = block_len / 25.4


block_len_error = block_len - block_len_mm

print("block measures: " + str(block_len_inch) + ", error = " + str(block_len_error * 1000) + " microns.")




end


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
	xc = probe[0] + axis[0]
	yc = probe[1] + axis[1]
	zc = probe[2] + axis[2]
	return


PROBE_CHANGE_SUB
    probe_diameter = change_rprobe_id(cur_probe_tip)
    probe_rad = .5 * probe_diameter
    probe_id = get_probe_name()
    return ' PROBE_CHANGE_SUB


DEFFN RADIANS(deg) = deg * pi / 180.0
DEFFN DEGREES(rad) = rad * 180.0 / pi
DEFFN CLEN(ce, cs) = (ce - cs) - 360.0 * sgn(ce - cs) * (abs(ce - cs) > 180.0)
the rfffk  ffflkf fkf flfffef 
lfjleft edgeske
first_x thefjf'
n