logmsg("c:\pecos\status\x_rpt.log", "Starting x axis repeatability test")

dim start[4]		rem start position
dim prb_ctr_tch[4]	center position at first touch
dim axs_pos_tch[4]	axis positions at first touch
dim position[4]
dim dspall[7]
dim probe[3]
dim axis[4]
i = 0

display_status("seeking part in x axis")

touch(1, 0, 0, 0)	rem touch in x
wait(10.0)
gosub dsp_pos_sub	read probe center positions
prb_ctr_tch[0] = position[0]
prb_ctr_tch[1] = position[1]
prb_ctr_tch[2] = position[2]
prb_ctr_tch[3] = position[3]
axs_pos_tch[0] = axis[0]
axs_pos_tch[1] = axis[1]
axs_pos_tch[2] = axis[2]
axs_pos_tch[3] = axis[3]

x_backoff = axis[0] + 20.0
x_touch = axis[0]

move("x", x_backoff)

for(i = 1 to 100)
    move("x", x_touch)
    display_status("logging pass " + str(i) + "sample 1")
    gosub log_sub
    display_status("logging pass " + str(i) + "sample 2")
    gosub log_sub
    display_status("logging pass " + str(i) + "sample 3")
    gosub log_sub
    display_status("logging pass " + str(i) + "sample 4")
    gosub log_sub
    display_status("logging pass " + str(i) + "sample 5")
    gosub log_sub
    move("x", x_backoff)
next i

move("x", x_start)

end

LOG_SUB
    gosub DSP_POS_SUB
    logmsg("c:\pecos\status\x_rpt.log", str(i) + ", " + str(position[0] + ", " + str(axis[0]) + ", " + str(probe[0]))
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
