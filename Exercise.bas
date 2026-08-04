dim ref[3]
dim axis[4]
get_probe_ref(ref)
offset = 5

slew_speed(75)
slew_acc(300)
slew_dec(300)

x_neg = val(cmd_control("MG_BLX")) / 10000 - ref[0] + offset
x_pos = val(cmd_control("MG_FLX")) / 10000 - ref[0] - offset

y_neg = val(cmd_control("MG_BLY")) / 10000 - ref[1] + offset
y_pos = val(cmd_control("MG_FLY")) / 10000 - ref[1] - offset

z_neg = val(cmd_control("MG_BLZ")) / 10000 - ref[2] + offset
z_pos = val(cmd_control("MG_FLZ")) / 10000 - ref[2] - offset

display("select ok to start the exercise script. Press ESC to terminate this script.")

cnt = 0
while (1)
    cnt = cnt + 1
    msg = "exercise interation # " + str(cnt)
    display_status(msg)
    read_axis(axis)
    qbegin(1)
    move("x", x_pos)
    qdecel()
    inc_move("yzc", y_pos - axis[1], z_pos - axis[2], 360.0)
    qdecel()
    inc_move("yzc", y_neg - y_pos, z_neg - z_pos, -360.0)
    qdecel()
    move("x", x_neg)
    qgo()
wend
end

