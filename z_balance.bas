print "z axis balance test"
zmove = 200
slew_speed(20)
slew_acc(100)
slew_dec(100)
tce11 = 0
tce12 = 0
tce13 = 0
rem thermal_comp(tce11,tce12,tce13) rem no thermal comp
for i=1 to 50
  inc_move("z",zmove)
  wait(1)
  inc_move("z",-zmove)
  wait(1)
next
print "end of program"
end
