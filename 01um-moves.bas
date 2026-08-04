file = "c:\pecos\status\38-01um-moves-xyzc.txt"
num_repeats = 1
backoff_dist = 0
c_backoff_dist = 0


rem logmsg(file, "Starting Small Move Test")
logmsg(file, "  pass  , x defl  ,  y defl  ,  z defl , x prb ctr,  y prb ctr,  z prb ctr,   x pos  ,   y pos   ,   z pos   ,  c pos  , tot defl")

dim start[4]		rem start position
dim prb_ctr_tch[4]	rem center position at first touch
dim axs_pos_tch[4]	rem axis positions at first touch
dim position[4]
dim dspall[7]
dim probe[3]
dim axis[4]

dim backoff[4]

i = 0
oversamp = 5

gosub dsp_pos_sub
start[0]=axis[0]
start[1]=axis[1]
start[2]=axis[2]
start[3]=axis[3]

c_start = axis[3]


slew_speed(300)		rem VECTOR_SPEED
slew_acc(100)		rem VECTOR_ACC
slew_dec(100)		rem VECTOR_DEC



display_status("Seeking part in x axis")
touch(-0.577, -0.577, -0.577, 0)
setdefl(0.250, -0.577, -0.577, -0.577)
setdefl(0.250, -0.577, -0.577, -0.577)
setdefl(0.250, -0.577, -0.577, -0.577)
display_status("waiting 2 seconds to settle axis")
wait(2.0)
gosub dsp_pos_sub	rem read probe center positions
prb_ctr_tch[0] = position[0]
prb_ctr_tch[1] = position[1]
prb_ctr_tch[2] = position[2]
prb_ctr_tch[3] = position[3]
axs_pos_tch[0] = axis[0]
axs_pos_tch[1] = axis[1]
axs_pos_tch[2] = axis[2]
axs_pos_tch[3] = axis[3]

backoff[0] = axis[0] + backoff_dist
backoff[1] = axis[1] + backoff_dist
backoff[2] = axis[2] + backoff_dist
backoff[3] = axis[3] - c_backoff_dist


c_backoff = axis[3] + 2.0
c_touch = axis[3]

move("xyzc", backoff[0], backoff[1], backoff[2], backoff[3])


for i = 1 to 10
    display_status("pre-sample #" + str(i))
    gosub log_sub
    wait(1.0)
next i


display_status("testing x plus")
for i = 1 to 30
    display_status("x plus sample " + str(i))
    inc_move("x", 0.0001)
    gosub log_sub
next i
display_status("testing x minus")
for i = 1 to 30
    display_status("x minus sample " + str(i))
    inc_move("x", -0.0001)
    gosub log_sub
next i
display_status("testing y plus")
for i = 1 to 30
    display_status("y plus sample " + str(i))
    inc_move("y", 0.0001)
    gosub log_sub
next i
display_status("testing y minus")
for i = 1 to 30
    display_status("y minus sample " + str(i))
    inc_move("y", -0.0001)
    gosub log_sub
next i
display_status("testing z plus")
for i = 1 to 30
    display_status("z plus sample " + str(i))
    inc_move("z", 0.0001)
    gosub log_sub
next i
display_status("testing z minus")
for i = 1 to 30
    display_status("z minus sample " + str(i))
    inc_move("z", -0.0001)
    gosub log_sub
next i
display_status("testing c plus")
for i = 1 to 30
    display_status("c plus sample " + str(i))
    inc_move("c", 0.0001)
    gosub log_sub
next i
display_status("testing c minus")
for i = 1 to 30
    display_status("c minus sample " + str(i))
    inc_move("c", -0.0001)
    gosub log_sub
next i

for i = 1 to 10
    display_status("post-sample #" + str(i))
    gosub log_sub
    wait(1.0)
next i

display_status("returning to start point")

move("xyzc", start[0],start[1],start[2],start[3])

display_status("test completed - results are in " + file)

end

LOG_SUB
    gosub DSP_POS_SUB
    def = sqr(probe[0]^2 + probe[1]^2 + probe[2]^2)
    logmsg(file, str(i) + ", " + str(probe[0]) + ", " + str(probe[1]) + ", " + str(probe[2]) + ", " + str(position[0]) + ", " + str(position[1]) + ", " + str(position[2]) + ", " + str(axis[0]) + ", " + str(axis[1]) + ", " + str(axis[2]) + ", " + str(axis[3]) + ", " + str(def))
    wait(1.0)
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
    return

DSP_POS_SUB
    gosub DSP_ALL_SUB
    position[0] = probe[0] + axis[0]
    position[1] = probe[1] + axis[1]
    position[2] = probe[2] + axis[2]
    position[3] = axis[3]
return
