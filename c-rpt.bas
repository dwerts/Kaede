logmsg("c:\pecos\status\c_rpt.log", "Starting c axis repeatability test")
logmsg("c:\pecos\status\c_rpt.log", "    pass, x prb ctr,  y prb ctr,    y pos,    y defl,  c pos")

dim start[4]		rem start position
dim prb_ctr_tch[4]	rem center position at first touch
dim axs_pos_tch[4]	rem axis positions at first touch
dim position[4]
dim dspall[7]
dim probe[3]
dim axis[4]
i = 0
oversamp = 5

gosub dsp_pos_sub
c_start = axis[3]

display_status("Seeking part in x axis")
touch(0, 0, 0, -1)	rem touch in x
display_status("waiting 10 seconds to settle axis")
wait(10.0)
gosub dsp_pos_sub	rem read probe center positions
prb_ctr_tch[0] = position[0]
prb_ctr_tch[1] = position[1]
prb_ctr_tch[2] = position[2]
prb_ctr_tch[3] = position[3]
axs_pos_tch[0] = axis[0]
axs_pos_tch[1] = axis[1]
axs_pos_tch[2] = axis[2]
axs_pos_tch[3] = axis[3]

c_backoff = axis[3] + 2.0
c_touch = axis[3]

move("c", c_backoff)

for i = 1 to 25000
    display_status("moving in")
    move("c", c_touch)
    display_status("recording pass " + str(i) + ", sample 1")
    gosub log_sub
rem    display_status("recording pass " + str(i) + ", sample 2")
rem    gosub log_sub
rem    display_status("recording pass " + str(i) + ", sample 3")
rem    gosub log_sub
rem    display_status("recording pass " + str(i) + ", sample 4")
rem    gosub log_sub
rem    display_status("recording pass " + str(i) + ", sample 5")
rem    gosub log_sub
rem    display_status("recording pass " + str(i) + ", sample 6")
rem    gosub log_sub
rem    display_status("recording pass " + str(i) + ", sample 7")
rem    gosub log_sub
rem    display_status("recording pass " + str(i) + ", sample 8")
rem    gosub log_sub
rem    display_status("recording pass " + str(i) + ", sample 9")
rem    gosub log_sub
rem    display_status("recording pass " + str(i) + ", sample 10")
rem    gosub log_sub
    display_status("backing off")
    move("c", c_backoff)
next i

display_status("returning to start point")

move("c", c_start)

display_status("test completed - results are in c:\pecos\status\x_rpt.log")

end

LOG_SUB
    gosub DSP_POS_SUB
    logmsg("c:\pecos\status\c_rpt.log", str(i) + ", " + str(position[0]) + ", " + str(position[1]) + ", " + str(axis[1]) + ", " + str(probe[1]) + ", " + str(axis[3]))
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
