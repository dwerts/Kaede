rem Program to exercise the probe prox sensor.

prbprox(1)

dim ref[3] dim axis[4]
get_probe_ref(ref)		rem get the probe offsets for the current probe


print "x ref = " + str(ref[0]) + ", y ref = " + str(ref[1]) + ", z ref = " + str(ref[2])
xref = ref[0]
yref = ref[1]
zref = ref[2]
x_pos_lim = val(cmd_control("MG_FLX")) / 10000 - 1.0
x_neg_lim = val(cmd_control("MG_BLX")) / 10000 + 1.0
y_neg_lim = val(cmd_control("MG_BLY")) / 10000 + 1.0
y_pos_lim = val(cmd_control("MG_FLY")) / 10000 - 1.0
z_neg_lim = val(cmd_control("MG_BLZ")) / 10000 + 1.0
z_pos_lim = val(cmd_control("MG_FLZ")) / 10000 - 1.0