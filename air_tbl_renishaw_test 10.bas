print "c axis test"

rem incs = # incremental moves
incs = 36
rem dly = delay between inc moves in seconds
dly = 3.0
rem cmove = incremental move amount in degrees
cmove = -10
rem cmove = 10
slew_acc(300)
slew_dec(300)
slew_speed(400)
inc_move("c",5.0)
wait(2.0)
inc_move("c",-5.0)
wait(2.0)
for i = 1 to incs
	inc_move("c",cmove)
	wait(dly)
next i

inc_move("c",-5.0)
wait(2.0)
inc_move("c",5.0)
wait(2.0)
for i = 1 to incs
	inc_move("c",-cmove)
	wait(dly)
next i
print "end of program"
end
