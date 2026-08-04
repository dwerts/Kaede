'Starting a new version to work on individual portions. 
'Baseline is 4 inch block program. 
'The program overall is a stepping stone to the actual bar measuring script. A rather steep
'End product from 0 programming experience. 


block_len_mm = 320 'Note: Find out how to change this from prompt window

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

get_probe_ref(ref)		'Gets the offset for the current probe.

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
x_pos_lim = val(cmd_control("MG_FLX")) / 10000 - 1.0
x_neg_lim = val(cmd_control("MG_BLX")) / 10000 + 1.0
y_neg_lim = val(cmd_control("MG_BLY")) / 10000 + 1.0
y_pos_lim = val(cmd_control("MG_FLY")) / 10000 - 1.0
z_neg_lim = val(cmd_control("MG_BLZ")) / 10000 + 1.0
z_pos_lim = val(cmd_control("MG_FLZ")) / 10000 - 1.0


display("jog to position probe in front of bar")

' touch the block near the front edge
touch(-1,0,0,0)
gosub DSP_ALL_SUB

' store the first x location
sec_x = xc
sec_y = yc
inc_move("x", 2.0)

' move over 10 mm and touch again
inc_move("y", 10.0)
touch(-1,0,0,0)
gosub DSP_ALL_SUB

inc_move("x", 2.0)

' store the second position
first_x = xc
first_y = yc

' calculate the angular error and adjust the block square with y
delta_x = first_x - sec_x
delta_y = first_y - sec_y
ang_err = DEGREES(atn2(delta_x, delta_y))
inc_move("c", ang_err)

' move back over to the start point, then touch again
inc_move("y", -10)
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

inc_move("x", 2.0 + probe_rad)

move("y", left_edge_y + block_len_mm - 5.0)

touch(-1,0,0,0)
inc_move("x", probe[0] - target_defl)

find(0, 75, 0, 0)

gosub DSP_ALL_SUB

right_edge_y = last_y + probe_diameter

move("y", right_edge_y)
move("x", meas_loc_x)
touch(0, -1, 0, 0)
gosub DSP_ALL_SUB

right_edge_blk = yc - probe_rad

inc_move("y", 1.0)
move("x", surf_loc_x + 1.0 + probe_rad)

move("y", left_edge_y)

move("x", meas_loc_x)

touch(0, 1, 0, 0)
gosub DSP_ALL_SUB
left_edge_blk = yc + probe_rad

inc_move("y", -1.0)
move("x", surf_loc_x + 1.0 + probe_rad)
move("xyz", start_pt[0], start_pt[1], start_pt[2])

block_len = right_edge_blk - left_edge_blk
block_len_inch = block_len / 25.4


block_len_error = block_len - block_len_mm

print("Bar length error = " + str(block_len_error * 1000) + " microns")
'may revise this to just showing the mm measurement. Don't need to do it in inches. 

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
    probe_id = get_probe_id()
    return ' PROBE_CHANGE_SUB


DEFFN RADIANS(deg) = deg * pi / 180.0
DEFFN DEGREES(rad) = rad * 180.0 / pi
DEFFN CLEN(ce, cs) = (ce - cs) - 360.0 * sgn(ce - cs) * (abs(ce - cs) > 180.0)