rem Program to run axis used to measure linear distance errors with laser

dim ref[3] dim axis[4]
get_probe_ref(ref)

test_axis = 0 rem 0: X, 1: Y, 2: Z

switch(test_axis)
	case 0:
		axname = "x"
		break
	case 1:
		axname = "y"
		break
	case 2:
		axname = "z"
endswitch

start_loc = val(cmd_control("MG_FL" + axname)) / 10000
end_loc = val(cmd_control("MG_BL" + axname)) / 10000

if (test_axis = 1)
	start_loc = end_loc + 430.0
endif

inc = -5

rem nsamps = abs(int((end_loc - .2 * inc - start_loc) / inc)) rem adjust for extra 20% overshoot
nsamps = int((abs(end_loc - start_loc) - .4 * abs(inc)) / abs(inc)) rem adjust for extra 20% overshoot
extra = abs((end_loc - start_loc) - nsamps * inc) * .5

start_loc = start_loc - extra
end_loc = end_loc + extra

print "test axis: " + axname + ", dist: " + str(abs(end_loc - start_loc));"start: " + str(start_loc) + ", end: " + str(end_loc);"inc: " + str(inc) + ", points: " + str(nsamps + 1)

display("MOUNT THE OPTICS ONTO THE AXIS")
display("MAKE SURE THE AXIS HAS BEEN HOMED")
display("JOG THE AXIS CLEAR TO MOVE TO START POSITION")

display_status("MOVING TO AXIS START")

move(axname, start_loc - ref[test_axis])

display("CLICK OK WHEN READY TO BEGIN")

gosub SETUP_FILE_SUB

for i = 0 to nsamps 
	gosub MOVE_AND_WAIT_SUB
next i

inc_move(axname, .2 * inc)

wait(8.0)

for i = nsamps downto 0
	gosub MOVE_AND_WAIT_SUB
next i

if (file_err = 0) close #1
display("END OF TEST")
end

MOVE_AND_WAIT_SUB
	display_status("MOVING TO POS", i * inc)
	move(axname, (start_loc + inc * i) - ref[test_axis])
	display_status("WAITING ON POS", i * inc)
	wait(10.0)
	read_axis(axis)
	fprint #1, axis[0] + ref[0];",";axis[1] + ref[1];",";axis[2] + ref[2];",";axis[3];chr(13);chr(10)
	return

SETUP_FILE_SUB
	filename = "c:\pecos\data\" + axname + "linlocs-"
	filename = filename + date + "-"
	tstr = time
	filename = filename + left(tstr, 2) + mid(tstr, 4, 2) + ".dat"
	kill(filename)
	file_err = 1
	file_err = fopen(filename, writing, sequential, 1)
	
	if (file_err <> 0)
		display("CANNOT OPEN " + filename)
		end
	endif
	return