'program to move Z axis in defined lengths of 5mm upwards'

msg = "program starting"
display_status(msg)

display("press ok to start")

msg = "moving now..."
display_status(msg)

x_pos_lim = get_axis_param(0, 0) - 1.0
x_neg_lim = get_axis_param(0, 1) + 1.0
y_neg_lim = get_axis_param(1, 0) - 1.0
y_pos_lim = get_axis_param(1, 1) + 1.0
z_neg_lim = get_axis_param(2, 0) - 1.0
z_pos_lim = get_axis_param(2, 1) + 1.0

for test_loop=0 to 80

inc_move("z",10)
wait(5)

next test_loop

for test_loop=0 to 80

inc_move("z",-10)
wait(5)

next test_loop

msg = "movements complete"
display_status(msg)

display("movements have finished")

end

