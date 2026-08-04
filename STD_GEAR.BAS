REM std_gear.bas start

st = stime()
'>>>>> START OF PROGRAM

gosub INITIALIZE_SUB                            ' Section 1     initialize variables and arrays
'                                                               locate the part to be inspected
gosub ESTABLISH_JOURNALS_SUB                    ' Section 2     perform journal reference and axial reference height
gosub ESTABLISH_ZREF_HEIGHT_SUB                 ' Section 3     determine z position at the bottom of the facewidth
gosub TOOTH_SETUP_SUB                           ' Section 4     find flanks to be inspected
'                                                               perform inspections
if (index_first = 1) gosub INSPECT_INDEX_SUB    ' Section 5     inspect index
gosub INSPECT_HELIX_PROFILE_SUB                 ' Section 6     inspect helix & profile
if (index_first = 0) gosub INSPECT_INDEX_SUB    ' Section 5     inspect index
gosub INITIALIZE_CUTTER_VALUES_SUB              '               initialize variables for cutting tool specific inspections
gosub INSPECT_FACE_OD_INDEX_SUB                 ' Section 7     inspect top face and/or outside diameter runout
gosub INSPECT_FACE_ANGLE_SUB                    ' Section 8     inspect top face angle, step angle, and/or outside clearance angle
gosub ROOT_SCAN_SUB                             ' Section 9     inspect root profile
gosub RELATED_FEATURES_SUB                      ' Section 10    inspect related features
gosub SYNCHRONIZER_TEETH_SUB                    ' Section 11    inspect synchronizer teeth
gosub SURFACE_FINISH_SUB                        ' Section 12    inspect surface finish
'                                                               clean up and retract
gosub FINISHED_CLEANUP_SUB                      ' Section 13    done with inspections, return to start or retraction diameter
end

'<<<<< END OF PROGRAM / START OF SUBROUTINES >>>>>
'#region @INITIALIZE_SUB
INITIALIZE_SUB
'   *******************************************************************************************************************************************
'   *******************************************************************************************************************************************
    m~ = str(stime() - st) + ",S1 ***************************************************** Section 1 - Initialize *****************************************************"
'   *******************************************************************************************************************************************
'   *******************************************************************************************************************************************
    coord rotate coord mcs

' DIMENSIONING OF ARRAYS
    dim bcr[2] dim bha[2] dim lead[2]
    dim sap_ang[2] dim eap_ang[2] dim tpai[2] dim y_off[2]
    dim mdir[4] 
    dim shap_edge_c[2]                          ' used to store the c location of the shaper/broach cutting edges
    dim shap_edge_z[2]                          ' used to store the z height at the shaper/broach cutting edges
    dim sideclearangle[2]
    dim xn[2] dim yn[2] dim zn[2]               ' probe normals 0 = left, 1 = right
    dim xb[2] dim yb[2] dim zb[2]               ' backoff point for index touches
    dim xp[2] dim yp[2] dim zp[2] dim cp[2]     ' endpoint for index touches
    dim yps[2] dim zps[2]                       ' surface points used when changing probe tips
    dim xpbt[2] dim ypbt[2]                     ' base tangent versions of xp and yp
    dim setup_pos[2]                            ' this is ONLY to report the setup angles with "PROCESS LOCATION" messages
    dim setup_pos_adj[2]                        ' 912.10 adjusted setup_pos to send with first index point of each location
    dim setup_defl[2]                           ' 1509.25 all the data recorded during flank setup, used for debugging size issues
    dim setup_xp[2] dim setup_yp[2]             ' 903.23 jjm Corrects issue at Zellamehliss
    dim cluster_pos[4] dim shap_fot[2]
    dim flk_lbl[2]
    dim probe[3] dim dspall[7]
    dim axis[4] dim target[4] dim position[4]
    dim setmsg[2]
    dim sum[4]
    dim tooth1[2]
    dim ref[3]
    dim pos_err[4]

    pos_err[0] = 0.0
    pos_err[1] = 0.0
    pos_err[2] = 0.0
    pos_err[3] = 0.0

' DEFAULT ARGUMENTS
    r_con = 180 / pi
    tooth_direction = 1                 ' 1 = count teeth counter-clockwise (standard/old direction), -1 = count teeth clockwise (new option)
    do_indx_log = 1                     ' debugging information about index data
    surf_meas = 0                       ' bke 1206.15 init here so it can be tested in helix and profile subroutines
    remove_surf_probe = 0
    bt_cl = 0                           ' bke 1206.12 flag for current mode of x and y coords and normals

    follow = 1                          ' bke 904.15
    corr_pos_err = 1                    ' determines if the index deflection should be corrected for axis positioning error
    skip_table_touch = 0                ' 904.09
    touch_mode = 0
    all_data_columns = 0                ' bke 811.07
    tooth1_rpt_limit = .005
    ps_spline_off = 0.0                 ' bke 802.22 leave this 0.0 until we need to develop Triumph order
    cp0_rev = 0                         ' galil table counts var to evaluate and REMove odd table rotation before rel feat measure
    tip_fall_off = 0
    do_disp = 1                         ' set to 0 to turn off messages to display.exe
    pcorr = 1
    use_q = 1
    cp[0] = 0.0                         ' mpw/bke 1206.27 initialize cp[] so XFORM_BT_2_CL and XFORM_CL_2_BT subroutines don't fail with invalid index
    cp[1] = 0.0
    plr_delay = 0.2
    xy_invo = 0
    thread_lead = 0.0
    tlead = 0.0
    cl_ref_type = 0                     ' 0: bcd ref, 1: insp d ref
    do_setup_cl = 0
    do_tests_cl = 0
    z_move = 0.0
    c_move = 0.0
    pitch_pct = .75                     ' pct amount of pitch to move clear to rf
    surf_defl = .02
    zinc_tooth = 0
    npitch = 0
    edge_ht = 0
    feedrate = 0.0
    prev_feedrate = 0
    scallop_scan_dir = 1
    scallop_pts_per_mm = 50
    scallop_defl = .05
    z_scallop_padding = 0.0
    normal_path = 0                     ' normal_path: 1 = normal, -1 = inverted normal (scallop), 0 = transverse
    scal_act_helix = 0
    plane_z_shift = 0

' PARAMETERS READ FOR THE MACHINE
    index_first = ?

' PARAMETERS READ FROM THE GEAR

    flank_name_type = ?                             ' bke 907.13
        switch (flank_name_type)                    ' bke 907.13
        case 0:
            flk_lbl[0] = "LEFT"
            flk_lbl[1] = "RIGHT"
            break
        case 1:
            flk_lbl[0] = "RIGHT"
            flk_lbl[1] = "LEFT"
            break
        endswitch
    move_factor = ?                                 ' 1, 2, 3, 4, 5
        min_slew = 10 * move_factor                 ' 5,10,15,20,25
        max_slew = 20 * move_factor     			' 20,40,60,80,100 bke 811.07 was 25-125
        oversamp = 30 - 5 * move_factor			    ' 25,20,15,10,5
        slew_accel = 40 + 60 * (move_factor - 1)	' 40,100,160,220,280 bke 811.07 was 50-350
        scan_accel = 40 + 30 * (move_factor - 1)    ' 40,70,100,130,160 bke 1003.17
        set_inpos_ct(5 - move_factor)               ' bke 1003.17 this is supported in TOUCHPROX22.DMC, harmless for other versions

' adjust the acc/dec for medium and heavy inertia parts
    heavy_part = ?
    scurve(0.05)
    if (heavy_part = 1)
        slew_accel = slew_accel * 0.75
        scan_accel = scan_accel * 0.75
        scurve(0.04)
    endif
    if (heavy_part = 2)
        slew_accel = slew_accel * 0.5
        scan_accel = scan_accel * 0.5
'0.03 was the minimum I could get to without the table banging during index.
        scurve(0.03)
    endif
    slew_decel = slew_accel
    scan_decel = scan_accel

    straight_sided = ?                              ' now is 0:involute, 1:ss, 2:ps bke 909.08
    part_type = ?                                   ' 0:gear, 1:ss spline, 2:ps spline, 3:shaver, 4:def prof, 5:shaper, 6:broach
        is_gear = (part_type = 0)
        is_ss_spline = (part_type = 1)
        is_ps_spline = (part_type = 2)
        is_shaver = (part_type = 3)
        is_def_prof = (part_type = 4)
        is_shaper = (part_type = 5)
        is_broach = (part_type = 6)
        is_tapered_sector = (part_type = 7)         ' bke 1102.08
        if (is_shaper || is_broach)
            if (straight_sided = 1) is_ss_spline = 1
            if (straight_sided = 2) is_ps_spline = 1
        endif
        if (is_ss_spline || is_ps_spline) follow = 0
    insp_idx = ?
    insp_lead = ?
    insp_prof = ?
    insp_relfeat = ?
    insp_face_index = ?
    insp_od_index = ?
    insp_face_angle = ?
    insp_step_angle = ?
    insp_clear_angle = ?
    insp_surf = ?
    insp_dogteeth = ?
    faceangle = ?                                   ' shaper or broach rake angle
    odclearangle = ?                                ' shaper od clear
    stepangle = ?                                   ' shaper step or sharpening angle, broach gullet lead angle
    sideclearangle[0] = ?                           ' shaper or broach lf side clear angle
    sideclearangle[1] = ?                           ' shaper or broach rf side clear angle
    nteeth = ?
    facewidth = ?
    z_off = ?                                       ' inspection height
    base_height = ?                                 ' always 0
    num_teeth = ?
        dim ioff[2, num_teeth + 1]	                ' declare and initialize index angle offsets for follow teeth
        for i = 0 to num_teeth
            ioff[0, i] = 0.0
            ioff[1, i] = 0.0
        next i
    numstarts = ?                                   ' moved up for inspection_thread initialization
    pitch = ?
    inspection_diameter = ?
        insp_rad = .5 * inspection_diameter
        if ((inspection_diameter / num_teeth) < 2.0) pitch_pct = .6
    tip_diameter = ?
    root_diameter = ?
    cluster_height = ?
    clear_diam = ?
        if (clear_diam <> 0)
            get_probe_ref(ref)
            x_pos_lim = get_axis_param(0, 0) - 10.0 - ref[0]
            if ((0.5 * clear_diam)  > x_pos_lim) clear_diam = x_pos_lim * 2
        endif
    retract_diam = ?
        if (retract_diam <> 0)
            get_probe_ref(ref)
            x_pos_lim = get_axis_param(0, 0) - 10.0 - ref[0]
            if ((0.5 * retract_diam) > x_pos_lim) retract_diam = x_pos_lim * 2
        endif
    is_cluster = ?
    cluster_first = ?
    cluster_last = ?
    helix_angle = ?                                 ' use sign of this to determine helix hand of shaper
    pitch_diameter = ?                              ' used to calc lead from helix_angle which may differ from lead[] if shaper
        if (helix_angle <> 0.0) tlead = pi * pitch_diameter / tan(helix_angle / r_con)
        if (is_shaver)
            shv_gash_wid = ?
            shv_gash_wid = shv_gash_wid * cos(helix_angle / r_con)              ' abs(yn[0])
            shv_land_wid = ?
            shv_land_wid = shv_land_wid * cos(helix_angle / r_con)              ' abs(yn[0])
            npitch = shv_gash_wid + shv_land_wid
        else
            shv_gash_wid = 0
        endif
    inspect_pl_runout = ?
    find_gap = ?
    gear_probe_tip = ?
    find_bottom = ?
    find_top = ?
    app_height = ?
    zref_type = ?                                   ' 0:top/bottom edge find, 1:top/bottom face find, 2:fixed z ht, 3: offset from ax. ref. bke 909.18
    zref_val = ?
    auto_z_move = ?
    inspect_space_tooth = ?
    z_setup = ?
    find_scal = ?                                   ' 804.09 find hob scallops 1 = on, 0 = off
        if (find_scal)
            feedrate = ?
            scal_act_helix = ?
        endif
    ie_type = ?
        if (ie_type <> 0)
            neg_if_int = 1.0		                ' external
        else
            neg_if_int = -1.0		                ' internal
        endif
    if (is_shaver = 0) do_setup_cl = ?          ' bke 805.14 was 0
    if (((is_gear || is_tapered_sector) && (inspect_pl_runout = 0)) || is_shaver) ' bke 1102.08
        do_tests_cl = ?
    endif
    for i = 0 to 1
        bcr[i] = ?
            if (bcr[i] > (.5 * tip_diameter))
                eap_ang[i] = 0.0
            else
                eap_ang[i] = sqr(((.5 * tip_diameter) + tip_fall_off)^2 - bcr[i]^2) * r_con / bcr[i]
            endif
        lead[i] = ?
        sap_ang[i] = ?
        tpai[i] = ?                                 ' acos(bcr[i] / insp_rad) * r_con
            y_off[i] = insp_rad * sin(tpai[i] / r_con)
        xn[i] = ?
        yn[i] = ?
        zn[i] = ?
            bha[i] = asin(zn[i]) * r_con            ' bha sign will follow zn sign which is needed for find_edge()
    next i
        if (is_broach)
            thread_lead = -pi * pitch_diameter * tan(stepangle / r_con) * sgn(tlead)        ' assume 0 spline lead would have 0 gullet lead

            if (numstarts > 0)
                npitch = abs(thread_lead) / numstarts * (1 - thread_lead / (thread_lead - tlead))
            else                                        ' bke 711.16
                apitch = ?
                npitch = apitch
            endif

            if (0)                                      ' bke 909.08 these vars are no longer used and may not be valid for ss broach
                npa = bcr[0] / (.5 * pitch_diameter)

                ha_insp = atn(pi * inspection_diameter / abs(tlead)) * r_con
                thread_la = atn(thread_lead / (pi * pitch_diameter)) * r_con
            endif

            if (0)                                      ' this is only needed when starting on gullet with side relief
                for i = -1 to 1 step 2                  ' use -1 for lf vars
                    lead[i > 0] = pi * pitch_diameter / tan((helix_angle + sideclearangle[i > 0] * i) / r_con)
                    bha[i > 0] = atn(2.0 * pi * bcr[i > 0] / lead[i > 0]) * r_con
                    yn[i > 0] = -cos(bha[i > 0] / r_con) * i                        ' lf+, rf-
                    zn[i > 0] = sin(bha[i > 0] / r_con) * sgn(lead[i > 0]) * i      ' lflh+, rflh-, lfrh-, rfrh+
                next i
            endif
        endif
        zinc_tooth = npitch * numstarts / num_teeth     ' 712.12 removed abs( ) around numstarts tlt - for shaver
    skip_prompts = ?
    flank = ?
        if (do_disp = 0) flank = 3
        if (flank = 0) flank = 3
        if (insp_relfeat) flank = 3
    index_on_center = ?
    target_defl = ?
        if ((target_defl < .05) || (target_defl > .6))
            if ((inspection_diameter / num_teeth) < .85)
                target_defl = .1
            else
                target_defl = .25
            endif
        endif
        backoff_defl = target_defl * 1.2
    intTraces = ?
    intThreads = ?
        if (is_broach = 0) intThreads = intTraces   ' step angle inspection changed to use thread list
        dim inspection_tooth[intTraces + 1]
        for i = 0 to (intTraces - 1)
            inspection_tooth[i] = ?	
        next i
        dim inspection_thread[intThreads + 1]
        if (intThreads = 0)
            inspection_thread[0] = 1
        else
            for i = 0 to (intThreads - 1)
                if (is_broach)
                    inspection_thread[i] = ?
' convert thread number to equivalent starting tooth, this way compatibility is maintained and the inspection code works for shapers and broaches
                    inspection_thread[i] = INT(1 + (inspection_thread[i] - 1) / numstarts * num_teeth)
                else
                    inspection_thread[i] = inspection_tooth[i]
                endif
            next i
        endif
    num_indexlocs = ?
        dim index_loc[num_indexlocs]
        if num_indexlocs = 1
            index_loc[0] = z_off
        else
            for i = 0 to (num_indexlocs - 1)
                index_loc[i] = ?
            next i
        endif
    num_leadlocs = ?
        if (num_leadlocs < 0)
            num_leadlocs = abs(num_leadlocs)
            leadlocs_allteeth = 0
        else
            leadlocs_allteeth = 1
        endif
        dim lead_loc[num_leadlocs]
        for i = 0 to (num_leadlocs - 1)
            lead_loc[i] = ?
        next i
    num_proflocs = ?
        if (num_proflocs < 0)
            num_proflocs = abs(num_proflocs)
            proflocs_allteeth = 0
        else
            proflocs_allteeth = 1
        endif
        dim prof_loc[num_proflocs]
        for i = 0 to (num_proflocs - 1)
            prof_loc[i] = ?
        next i
    num_of_journals = ?
    is_scan_root = ?
    use_journals = ?
    helix_speed = ?
    inv_speed = ?
    helix_inc = ?
    inv_inc = ?
    safety = ?                                      ' bke 909.29 safety = 1.0, was 2.0
    arbor_jump = ?

    if (do_indx_log)
        index_log("CMD, NEW_FILE")
        index_log("BCR, " + str(bcr[0]) + ", " + str(bcr[1]))
        index_log("XN, " + str(xn[0]) + ", " + str(xn[1]))
        index_log("YN, " + str(yn[0]) + ", " + str(yn[1]))
        index_log("ZN, " + str(zn[0]) + ", " + str(zn[1]))
        index_log("BHA, " + str(bha[0]) + ", " + str(bha[1]))
        index_log("EXT, " + str(ie_type))
        index_log("NTEETH, " + str(num_teeth))
    endif

' VARIABLES ARE SET, PREPARE FOR INSPECTION
    slew_speed(max_slew)		                    REM VECTOR_SPEED
    slew_acc(slew_accel)		                    REM VECTOR_ACC
    slew_dec(slew_decel)		                    REM VECTOR_DEC
    scan_acc(scan_accel)
    scan_dec(scan_decel)

    filtering("none", 3)
    nom_srate = 400
    act_srate = probe_sample_rate(nom_srate)	    REM PROBE_SAMPLE_RATE
    jog_on("xyzct")			                        REM JOG_ENABLE ALL

    msg = "BUILD GEAR" + " " + str(xn[0]) + " " + str(yn[0]) + " " + str(zn[0]) + " " + str(xn[1]) + " " + str(yn[1]) + " " + str(zn[1])
    if (do_disp) chart(msg)
    return ' end of INITIALIZE_SUB
'#endregion @INITIALIZE_SUB
'#region @ESTABLISH_JOURNALS_SUB
ESTABLISH_JOURNALS_SUB
'   *******************************************************************************************************************************************
'   *******************************************************************************************************************************************
    m~ = str(stime() - st) + ",S2 ************************************************* Section 2 - Establish Journals *************************************************"
'   *******************************************************************************************************************************************
'   *******************************************************************************************************************************************
    if (num_of_journals < 1) use_journals = 0
    if (zref_type = 3)	'offset from axial reference
        use_journals = 1
        if (num_of_journals >= 0)
            num_of_journals = -(num_of_journals * use_journals + 1)         REM result = -1 (z ht only), -2 (1 journal & z ht) or -3 (2 journals & z ht)
        else
            num_of_journals = -4                                            REM result = -4 (1 or 2 journals not being inspected but z ht is inspected)
        endif
    endif
    if (use_journals)
        measure_journals(num_of_journals, do_disp, skip_prompts, app_height, tooth_direction)
        if (zref_type = 3) zref_val = ?                                     REM update zref_val after axial ref. measured
    endif

    cur_probe_tip = gear_probe_tip
    gosub PROBE_CHANGE_SUB

    msg = "PROBE DIAM " + str(probe_diameter)
    if (do_disp) chart(msg)
    if (do_indx_log) index_log("DIA, " + str(probe_diameter))

REM tip falloff adjustment for shavers
    if (is_shaver) tip_fall_off = 0.75* probe_rad + .25 REM 810.24 tlt

    if (is_def_prof)
        for i = 0 to 1
            xp[i] = ?
            yp[i] = ?
REM         xp[i] = xp[i] + target_defl * xn[i]
REM         yp[i] = yp[i] + target_defl * yn[i]
            xb[i] = xp[i] + (target_defl - backoff_defl) * xn[i]
            yb[i] = yp[i] + (target_defl - backoff_defl) * yn[i]
            bcr[i] = xp[i]
            y_off[i] = 0
        next i
        tpai_x2 = 0	
    else
        xp[0] = bcr[0]
        xp[1] = bcr[1]
        yp[0] = -y_off[0] * neg_if_int - probe_rad * yn[0] + ps_spline_off
        yp[1] = y_off[1] * neg_if_int - probe_rad * yn[1] + ps_spline_off	
        tpai_x2 = tpai[0] + tpai[1]	
    endif

' store base tangent versions of xp and yp
    for i = 0 to 1
        xpbt[i] = xp[i]
        ypbt[i] = yp[i]
    next i

' rotate xp, yp, xn, yn
    if (do_setup_cl)
        gosub XFORM_BT_2_CL
    endif
REM bke 909.21 moved these initializations above tooth setup code so when they are needed for tooth setup in related feat they will not have defl
REM setup vars are in setup corrd (cl or bt) w/o defl
    setup_xp[0]=xp[0]    REM 903.23 jjm corrected Zella Mehliss internal related feature (Diameter runout) 
    setup_xp[1]=xp[1]
    setup_yp[0]=yp[0]
    setup_yp[1]=yp[1]

    x_clear = .5 * tip_diameter + (probe_rad + safety) * neg_if_int
    return ' end of ESTABLISH_JOURNALS_SUB
'#endregion @ESTABLISH_JOURNALS_SUB
'#region @ESTABLISH_ZREF_HEIGHT_SUB
ESTABLISH_ZREF_HEIGHT_SUB
'   *******************************************************************************************************************************************
'   *******************************************************************************************************************************************
    m~ = str(stime() - st) + ",S3 *********************************************** Section 3 - Establish Z Ref Height ***********************************************"
'   *******************************************************************************************************************************************
'   *******************************************************************************************************************************************
    if (is_cluster = 1)
        if (((cluster_height > -999.0) || (clear_diam > 0.0) || (retract_diam > 0.0)) && (use_journals = 0) && (cluster_first = 1))
            prompt = "JOG THE PROBE CLEAR TO APPROACH THE PART"
            gosub PROMPT_SUB
        endif

        if (cluster_height > -999.0) REM move("z", cluster_height)
            gosub DSP_ALL_SUB
	        inc_move("zc", cluster_height - axis[2], (cluster_height -axis[2]) * ANG_PERZ(lead[flank = 2]))
        endif

        if (app_height > -999.0) move("z", app_height)
        if (clear_diam > 0.0)
            move("xy", 0.5 * clear_diam, 0.0)
        else
            if (retract_diam > 0.0) move("xy", 0.5 * retract_diam, 0.0)
        endif
    else
        if (app_height > -999.0)
            if (use_journals = 0)
                prompt = "JOG THE PROBE CLEAR TO APPROACH THE PART"
                gosub PROMPT_SUB
            endif
            if (clear_diam > 0) move("xy", 0.5 * clear_diam, 0.0)
            move("z", app_height)
            if (clear_diam < -999.0)
                if (straight_sided)
                    move("y", ps_spline_off)
                else
                    move("y", yp[flank = 2])
                endif
    	        move("x", tip_clear(x_clear))
            endif
        else
            if (clear_diam > 0) move("xy", 0.5 * clear_diam, 0.0)
        endif
    endif

    c_offset = 0.0
    zb[0] = 0.0
    zb[1] = 0.0
    z_land_hgt = 0

    if (inspect_pl_runout = 1)
        flank = 3
        base_height = zref_val
        fall_off_top = base_height + facewidth
        yp[0] = 0.0
        if (clear_diam > 0)
            move("xy", 0.5 * clear_diam, 0.0)
        else
            move("x", x_clear)
        endif
    else
        x_pos = xp[flank = 2]
        y_pos = yp[flank = 2]
        xnorm = xn[flank = 2]
        ynorm = yn[flank = 2]
        znorm = zn[flank = 2]

        if (straight_sided)  REM extra setup for straight sided splines
            if (ps_spline_off = 0.0)
                x_pos = insp_rad
            else
                x_pos = sqr(insp_rad^2 - ps_spline_off^2)
            endif
            y_pos = ps_spline_off
        endif
        gosub ZREF_SETUP_SUB
    endif
' fall_off_top, facewidth, base_height are now set based upon the zref configuration.
    zland_adj = 0
    if ((npitch <> 0.0) && (abs(z_off) > npitch) && (is_shaver = 0)) zland_adj = fmod(z_off, npitch) REM shaver will get adjusted at inspection_height

    if (z_off <= 0.0) REM chg < to <= for broach on cutting edge
        inspection_height = fall_off_top + z_off - zland_adj
    else
        inspection_height = base_height + z_off - zland_adj
    endif

' put shaver insp ht at the land center nearest to the programmed insp ht.
    if(is_shaver) inspection_height = inspection_height - fmod_min(inspection_height - z_land_hgt, npitch)

' initialize zp's - they may change during tooth setup for scallops, lands, cutting edges, etc.
    zp[0] = inspection_height - probe_rad * zn[0]
    zp[1] = inspection_height - probe_rad * zn[1]

'   bke 1111.28 this was a temp solution at magna
'   if (num_indexlocs > 1)
'       if (index_loc[0] <= 0.0) REM chg < to <= for broach on cutting edge
'           temp_insp_ht = fall_off_top + index_loc[0] - zland_adj
'       else
'           temp_insp_ht = base_height + index_loc[0] - zland_adj
'       endif

'       zp[0] = temp_insp_ht - probe_rad * zn[0]
'       zp[1] = temp_insp_ht - probe_rad * zn[1]
'   endif

    c2_adj = 0 ' adjustment of c2 position for transverse z height difference between cp[0] and cp[1]
    return ' end of ESTABLISH_ZREF_HEIGHT_SUB
'#endregion @ESTABLISH_ZREF_HEIGHT_SUB
'#region @TOOTH_SETUP_SUB
TOOTH_SETUP_SUB
'   *******************************************************************************************************************************************
'   *******************************************************************************************************************************************
    m~ = str(stime() - st) + ",S4 ***************************************************** Section 4 - Tooth Setup ****************************************************"
'   *******************************************************************************************************************************************
'   *******************************************************************************************************************************************

	GOSUB SETUP_FILE_SUB

    read_dsp(axis, oversamp)

    x_pos = xp[flank = 2]   REM  base circle radius
    y_pos = yp[flank = 2]   REM  base tan position offset by probe radius * yn
    z_pos = zp[flank = 2]   REM  insp ht offset by probe rad * zn

	' not finding top or bottom edge.
    if (zref_type > 1) REM move to z ht option bke 909.18
        z_start = zp[0]

		' fixed reference height
        if (((is_cluster = 0) || (cluster_first = 1)) && (use_journals = 0) && (zref_type = 2)) REM bke 911.05 bke 910.08
            if (ie_type = 0)	REM internal
                prompt = "JOG AXIS CLEAR TO MOVE PROBE DOWN INTO PART"
            else				REM external
                prompt = "JOG AXIS CLEAR TO MOVE PROBE NEAR OD AT INSPECTION HEIGHT"
            endif

            gosub PROMPT_SUB
        endif
        read_dsp(axis, oversamp)
    endif

	' cluster start point
    if (is_cluster = 1)
        if (cluster_first = 1)
            cluster_pos[0] = 0.5 * retract_diam
            cluster_pos[1] = y_pos
            cluster_pos[2] = z_start
            cluster_pos[3] = axis[3] + (z_start - axis[2]) * ANG_PERZ(lead[flank = 2])
            set_cluster_start(cluster_pos)
        else
            get_cluster_start(cluster_pos)
            if ((0.5 * retract_diam) > cluster_pos[0])				REM always find the max clear for cluster with internal and external gears
                cluster_pos[0] = 0.5 * retract_diam
                set_cluster_start(cluster_pos)
            endif
        endif
    endif

    if use_q qbegin(1)
	
	' move x to an appropriate clearance position based upon the conditions
	' if we have a clearance diameter set, and we aren't finding an edge,
	' and we are internal and outside of the clearance diameter,
	' or we are external and inside of the clearance diameter, we need to move
    if (((clear_diam > 0.0) && (zref_type > 1)) && ((ie_type = 0) && (axis[0] > 0.5 * clear_diam)) || ((ie_type = 1) && (axis[0] < 0.5 * clear_diam)))
        if (((sgn(y_pos) < 0) && (axis[1] > y_pos)) || ((sgn(y_pos) > 0) && (axis[1] < y_pos)))
            if (sgn(y_pos) = sgn(axis[1]))
                move("x", tip_clear(0.5 * clear_diam))
            else
                move("x", 0.5 * clear_diam)
            endif
        else
            move("x", tip_clear(0.5 * clear_diam, y_pos))
        endif
    else
	' no clearance diameter set, move to x_clear which is based upon the tip diameter and a safety factor
        if ((ie_type = 0) && (axis[0] > x_clear)) || ((ie_type = 1) && (axis[0] < x_clear))
            if (((sgn(y_pos) < 0) && (axis[1] > y_pos)) || ((sgn(y_pos) > 0) && (axis[1] < y_pos)))
                if (sgn(y_pos) = sgn(axis[1]))
                    move("x", tip_clear(x_clear))
                else
                    move("x", x_clear)
                endif
            else
	            move("x", tip_clear(x_clear, y_pos))
            endif
	        if use_q qdecel()
        endif
    endif

	' move z
    z_move = z_pos - axis[2]
    c_inc = z_move * ANG_PERZ(lead[flank = 2])
    move("zc", z_pos, axis[3] + c_inc - c_offset) REM bke 902.17 abs move better for high helix moves > 360deg
	
	
	' bke 911.17 this cond allows moving to clear location after moving to setup ht
    if ((ie_type = 0) && (axis[0] < x_clear)) || ((ie_type = 1) && (axis[0] > x_clear))
	    if use_q qdecel()
        if (((sgn(y_pos) < 0) && (axis[1] > y_pos)) || ((sgn(y_pos) > 0) && (axis[1] < y_pos)))
            if (sgn(y_pos) = sgn(axis[1]))
                move("x", tip_clear(x_clear))
            else
                move("x", x_clear)
            endif
        else
	        move("x", tip_clear(x_clear, y_pos))
        endif
    endif
    if use_q qgo()

    if (straight_sided && (zref_type > 1))  REM extra setup for straight sided splines bke 909.18
        y_pos = 0.0
        x_pos = insp_rad
    endif

	' we are now at the setup height in z, and at a clearance location in x.
	' call the setup sub to find the flanks.
    if (is_def_prof)
        gosub DEFINED_PROFILE_SETUP_SUB
    else
        gosub STANDARD_SETUP_SUB
    endif

REM the next block must be after setup and before tests
    if ((is_broach) && (z_off <= 0.0)) REM this code will move fall_off_top from top of 1st gullet to top of part and adj z_off
        zadj = facewidth - npitch - fmod(facewidth - npitch, npitch)
        fall_off_top = fall_off_top + zadj REM zadj must be added here
        z_off = z_off - zadj REM zadj must be subtracted here
        if (num_indexlocs = 1) index_loc[0] = z_off
    endif

'   1404.29 mpw
    if (find_scal)
        m~ = str(stime() - st) + ",SET_IPTH = 20"
        set_inpos_thresh(0.002)                        ' was 10
    else
        m~ = str(stime() - st) + ",SET_IPTH = 100"
        set_inpos_thresh(0.010)
    endif
' loosens the "in position threshhold" to speed up inspection
' index corrects for this tolerance and it should not affect helix or profile in significantly
' turn this back OFF after helix & profile in case this adversely affects related features or synchronizer teeth or something else (???)
    return ' end of TOOTH_SETUP_SUB
'#endregion @TOOTH_SETUP_SUB
'#region @INSPECT_INDEX_SUB
INSPECT_INDEX_SUB
'   *******************************************************************************************************************************************
'   *******************************************************************************************************************************************
    m~ = str(stime() - st) + ",S5 **************************************************** Section 5 - Inspect Index ***************************************************"
'   *******************************************************************************************************************************************
'   *******************************************************************************************************************************************
    c_1 = cp[0] REM cp[] are in setup coords (cl or bt) c_x are always bt
    c_2 = cp[1]

    if (do_setup_cl)
        switch (cl_ref_type)
        case 0: 'ref to bcd
            c_1 = fmod(360.0 + c_1 - tan(tpai[0] / r_con) * r_con * neg_if_int, 360.0)
            c_2 = fmod(360.0 + c_2 + tan(tpai[1] / r_con) * r_con * neg_if_int, 360.0)
            break
        case 1: 'ref to insp dia
            c_1 = fmod(360.0 + c_1 - tpai[0] * neg_if_int, 360.0)
            c_2 = fmod(360.0 + c_2 + tpai[1] * neg_if_int, 360.0)
            break
        endswitch
    endif
'c_1 and c_2 are now base tangent versions of cp[0] and cp[1](which might be either)

    if (do_tests_cl)
        xy_invo = 1
        index_on_center = 1
REM make coords cl if tests are cl but not setup
        if (do_setup_cl = 0)
            gosub XFORM_BT_2_CL
            xb[0] = xp[0] - backoff_defl * xn[0]
            yb[0] = yp[0] - backoff_defl * yn[0]
            xb[1] = xp[1] - backoff_defl * xn[1]
            yb[1] = yp[1] - backoff_defl * yn[1]
        endif
    endif

    slew_speed(max_slew)

    if (insp_idx)
        m~ = str(stime() - st) + ",INSPECTING INDEX"
        dim init_zp[2] dim init_cp[2] dim init_cpbt[2]

        init_cpbt[0] = c_1 REM make sure to use c_1, c_2 beacuse they are always base tangent relative
        init_cpbt[1] = c_2

        for flk = 0 to 1
            init_zp[flk] = zp[flk]
            init_cp[flk] = cp[flk] REM use cp[] here for broach & shaper; was setup_pos[i_cnt]
        next flk

        for lc = 0 to (num_indexlocs - 1)        
            for flk = 0 to 1
                zloc_adj = index_loc[lc] - z_off

                if (sgn(z_off) <> sgn(index_loc[lc]))
                    zloc_adj = zloc_adj + (fall_off_top - base_height) * sgn(z_off)
                endif

                if (npitch <> 0) zloc_adj = zloc_adj - fmodx(zloc_adj, npitch) REM 712.13

                zp[flk] = init_zp[flk] + zloc_adj
                zb[flk] = zp[flk] - backoff_defl * zn[flk]
                cp[flk] = fmod(init_cpbt[flk] + 360.0 + zloc_adj * ANG_PERZ(lead[flk]), 360.0)
'912.10 tlt - always use setup_pos to derive the location to process - cp's are not necessarily transverse, but setup_pos's are
                setup_pos_adj[flk] = fmod(setup_pos[flk] + 360.0 + zloc_adj * ANG_PERZ(lead[flk]), 360.0)
                msg = "PROCESS LOCATION " + str(setup_pos_adj[flk])
                if (do_indx_log) index_log(msg)
                if (do_disp) chart(msg)
            next flk

            slew_speed(min_slew)
            if ((index_on_center = 0) || (is_def_prof))
                gosub STD_INDEX_SUB
            else
                gosub CL_INDEX_SUB
            endif
            slew_speed(max_slew)
        next lc

        for flk = 0 to 1
            zp[flk] = init_zp[flk]
            zb[flk] = zp[flk] - backoff_defl * zn[flk]
            cp[flk] = init_cp[flk]
        next flk

        c_1 = init_cpbt[0]
        c_2 = init_cpbt[1]
    endif
	
	if(file_err = 0) close #2
	
    return ' end of INSPECT_INDEX_SUB
'#endregion @INSPECT_INDEX_SUB
'#region @INSPECT_HELIX_PROFILE_SUB
INSPECT_HELIX_PROFILE_SUB
'   *******************************************************************************************************************************************
'   *******************************************************************************************************************************************
    m~ = str(stime() - st) + ",S6 ********************************************** Section 6 - Inspect Helix & Profiles **********************************************"
'   *******************************************************************************************************************************************
'   *******************************************************************************************************************************************
    z_move = base_height - inspection_height
    temp_lead_avg = lead[0] temp_lead_current = lead[0]

    slew_speed(max_slew)
    for flk = (flank = 2) to (flank <> 1)    
        for i = 0 to nteeth - 1
            scan_tooth = inspection_tooth[i]
            if (scan_tooth > 0)
                if (insp_lead)
                    nlocs = 1
                    if ((i = 0) || (leadlocs_allteeth > 0)) nlocs = num_leadlocs
                    scan_spd = helix_speed
                    scan_st = .002
                    scan_inc = helix_inc

                    yloc_adj = 0
                    z_move = base_height - (zp[flk] + (probe_rad - target_defl) * zn[flk]) - scan_st REM 712.13 to fix lead stagger
                    z_scan = facewidth + 2 * scan_st

' added this to make sure we get a point below base_height and above the top.
                    if(is_shaver)
                        z_scan = z_scan + 0.5   'was + scan_inc
                        z_move = z_move - scan_st
                    endif

                    scan_label = "HELIX"
                    gosub HELIX_SCAN_SUB
                endif

                if (insp_prof)                
                    zloc_adj = 0
                    nlocs = 1
                    if ((i = 0) || (proflocs_allteeth > 0)) nlocs = num_proflocs
                    scan_spd = inv_speed
                    scan_st = .002
                    scan_inc = inv_inc

                    normal_path = find_scal * -1
                    if (find_scal && (lead[flk] <> 0.0) && (temp_lead_current <> 0.0) && scal_act_helix)
                        helix_y_per_z = pi * pitch_diameter * (1.0 / lead[flk] - 1.0 / temp_lead_current)
                    else
                        helix_y_per_z = 0.0
                    endif
                    c_scan = eap_ang[flk] - sap_ang[flk] + 2 * scan_st
                    if (straight_sided)
                        c_sap = 0.0
                    else
                        c_sap = sap_ang[flk] - scan_st
                    endif

                    if (straight_sided)
                        scan_label = "PROFILE"                    
                    else
                        scan_label = "INVOLUTE"                    
                    endif
                    gosub PROFILE_SCAN_SUB
                endif
            endif
        next i
    next flk

    if (insp_lead) temp_lead_avg = get_lead_average()

    slew_speed(max_slew)
    return ' end of INSPECT_HELIX_PROFILE_SUB
'#endregion @INSPECT_HELIX_PROFILE_SUB
'#region @INITIALIZE_CUTTER_VALUES_SUB
INITIALIZE_CUTTER_VALUES_SUB
    if (is_shaper || is_broach)
' bke 1012.10 changed cond back to zref_type because edge_ta is not measured if zref_type > 1
'    if (odclearangle = 0.0) REM bke 911.05 better fix for trw, et al
        if (zref_type > 1) REM bke 910.20 for trw
'           edge_ta = tpai_x2 - fmod(cp[1] - cp[0] + 360.0, 360.0)
            edge_ta = tpai_x2 - fmod(setup_pos[1] - setup_pos[0] + 360.0, 360.0) 'tlt these are transverse
            edge_rad = insp_rad
        endif

        f_xn = sin(faceangle / r_con)
REM f_xn = sin(faceangle / r_con) * cos(stepangle / r_con)
        f_yn = cos(faceangle / r_con) * sin(stepangle / r_con) * sgn(thread_lead)
        f_zn = -cos(faceangle / r_con) * cos(stepangle / r_con)

        shap_clr_defl = (edge_ta / r_con * insp_rad * tan(stepangle / r_con) + (edge_rad - .5 * root_diameter) * tan(faceangle / r_con)) / abs(f_zn) + backoff_defl
    endif

    o_xn = -cos(odclearangle / r_con)
    o_yn = 0.0
    o_zn = sin(odclearangle / r_con)
    return ' end of INITIALIZE_CUTTER_VALUES_SUB
'#endregion @INITIALIZE_CUTTER_VALUES_SUB
'#region @INSPECT_FACE_OD_INDEX_SUB
INSPECT_FACE_OD_INDEX_SUB
    if (insp_face_index || insp_od_index)
'       *******************************************************************************************************************************************
'       *******************************************************************************************************************************************
        m~ = str(stime() - st) + ",S7 ************************************************ Section 7 - Inspect Face/OD Index ***********************************************"
'       *******************************************************************************************************************************************
'       *******************************************************************************************************************************************
        dim index_type[2]
        index_type[0] = "FACE INDEX"
        index_type[1] = "OD INDEX"
        index_idx = 1 - insp_face_index

        display_status("MOVING TO INSPECT " + index_type[index_idx])

        fi_xp = insp_rad - (probe_rad - target_defl) * f_xn
        fi_yp = -(probe_rad - target_defl) * f_yn
        fi_zp = edge_ht + (insp_rad - edge_rad) * tan(faceangle / r_con) - (probe_rad - target_defl) * f_zn	

        fi_xb = fi_xp - shap_clr_defl * f_xn
        fi_yb = fi_yp - shap_clr_defl * f_yn
        fi_zb = fi_zp - shap_clr_defl * f_zn

        oi_xp = edge_rad - (probe_rad - target_defl) * o_xn
        oi_yp = -(probe_rad - target_defl) * o_yn
        oi_zp = edge_ht - (probe_rad - target_defl) * o_zn

        oi_xb = oi_xp - shap_clr_defl * o_xn
        oi_yb = oi_yp - shap_clr_defl * o_yn
        oi_zb = oi_zp - shap_clr_defl * o_zn

        for i = 0 to num_teeth
            if (i = int(num_teeth)) i = num_teeth REM trw 10.8 teeth
            if (insp_face_index)			
                m~ = str(stime() - st) + ",INSPECTING TOOTH " + SINT(i + 1) + " FACE INDEX"
'               if use_q qbegin(1)
                move("z", fi_zb)
'               if use_q qdecel()
                c_inc = edge_c + (insp_rad - edge_rad) * tan(faceangle / r_con) * ANG_PERZ(tlead) - pitch * i * tooth_direction
                move("xyc", fi_xb, fi_yb, c_inc)
'               if use_q qgo()
                slew_speed(min_slew)
                move("xyz", fi_xp, fi_yp, fi_zp)
                display_status("INSPECTING " + index_type[0] + " TOOTH " + SINT(i + 1))
                gosub DSP_ALL_SUB
                msg = "PROCESS POINT "+ str(probe[1]) +" " + str(probe[2]) + " " + str(probe[0]) + " " + str(axis[0]) + " " + str(axis[1])
                if (do_disp) chart(msg)
                slew_speed(max_slew)
                move("xyz", fi_xb, fi_yb, fi_zb)
            endif

            if (insp_od_index)
                m~ = str(stime() - st) + ",INSPECTING TOOTH " + SINT(i + 1) + " OD INDEX"
'               if use_q qbegin(1)
                move("x", oi_xb)
'               if use_q qdecel()
                c_inc = edge_c - pitch * i * tooth_direction
                move("yzc", oi_yb, oi_zb, c_inc)
'               if use_q qgo()
                slew_speed(min_slew)
                move("xyz", oi_xp, oi_yp, oi_zp)
                display_status("INSPECTING " + index_type[1] + " TOOTH " + SINT(i + 1))
                gosub DSP_ALL_SUB
                msg = "PROCESS POINT "+ str(probe[1]) +" " + str(probe[2]) + " " + str(probe[0]) + " " + str(axis[0]) + " " + str(axis[1])
                if (do_disp) chart(msg)
                slew_speed(max_slew)
                move("xyz", oi_xb, oi_yb, oi_zb)
            endif
        next i
    endif
    return ' end of INSPECT_FACE_OD_INDEX_SUB
'#endregion @INSPECT_FACE_OD_INDEX_SUB
'#region @INSPECT_FACE_ANGLE_SUB
INSPECT_FACE_ANGLE_SUB
    if (insp_face_angle || insp_step_angle || insp_clear_angle)
'       *******************************************************************************************************************************************
'       *******************************************************************************************************************************************
        m~ = str(stime() - st) + ",S8 ******************************************** Section 8 - Inspect face/step/clear Angle *******************************************"
'       *******************************************************************************************************************************************
'       *******************************************************************************************************************************************
        scan_speed(6)	

        zstart_adj = 0.0    REM z offset to accomodate gullet face angle
        if (is_broach)
            zrake_adj = get_broach_z_adj(root_diameter, 2.0 * edge_rad, thread_lead)
        else
            zrake_adj = (0.5 * root_diameter - edge_rad) * tan(faceangle / r_con)
        endif
        crake_adj = zrake_adj * ANG_PERZ(tlead)

        fa_xp = .5 * root_diameter - (probe_rad - target_defl) * f_xn
        fa_yp = -(probe_rad - target_defl) * f_yn
        fa_zp = edge_ht + zrake_adj - (probe_rad - target_defl) * f_zn

        fa_xb = fa_xp - shap_clr_defl * f_xn
        fa_yb = fa_yp - shap_clr_defl * f_yn
        fa_zb = fa_zp - shap_clr_defl * f_zn

        num_rake_pts = 100.0
        fa_inc = (edge_rad - .5 * root_diameter) / num_rake_pts * get_axis_counts(0) REM bke 1207.26

        sa_start = .5 * edge_ta / r_con * insp_rad * tan(stepangle / r_con)

        sa_xp = insp_rad - (probe_rad - target_defl) * f_xn
        sa_yp = -(probe_rad - target_defl) * f_yn
        sa_zp = edge_ht + (insp_rad - edge_rad) * tan(faceangle / r_con)  - sa_start - (probe_rad - target_defl) * f_zn	

        sa_xb = sa_xp - shap_clr_defl * f_xn
        sa_yb = sa_yp - shap_clr_defl * f_yn
        sa_zb = sa_zp - shap_clr_defl * f_zn

        if (is_broach)
            use_z = 1 use_c = 0
            sa_zlen = facewidth - (edge_ht - base_height)
            sa_clen = sa_zlen * ANG_PERZ(thread_lead)
            sa_start = sa_start * get_axis_counts(2)   REM convert to counts REM bke 1207.26
            sa_inc = zinc_tooth * get_axis_counts(2) REM bke 1207.26
        else
            use_z = 0 use_c = 1
            sa_zlen = insp_rad * edge_ta / r_con * tan(stepangle / r_con)
            sa_clen = edge_ta * (1 - 2 * (tlead <= 0))
            sa_start = 0.0 REM start right at the edge
            sa_inc = edge_ta / 100.0 * get_axis_counts(3) / 360.0  REM c-axis REM bke 1207.26
        endif

        ca_xp = edge_rad + (base_height - edge_ht) * tan(odclearangle / r_con) - (probe_rad - target_defl) * o_xn
        ca_yp = -(probe_rad - target_defl) * o_yn
        ca_zp = base_height - (probe_rad - target_defl) * o_zn

        ca_xb = ca_xp - shap_clr_defl * o_xn
        ca_yb = ca_yp - shap_clr_defl * o_yn
        ca_zb = ca_zp - shap_clr_defl * o_zn

        ca_inc = (edge_ht - base_height) / 100.0 * get_axis_counts(3) / 360.0 REM bke 1207.26

        if (is_broach)
            step_label = "GULLET LEAD"
        else
            step_label = "STEP"
        endif

        move("x", x_clear)

        for i = 0 to nteeth - 1		
            if (inspection_tooth[i] > 0)
                if (insp_face_angle)
                    if ((is_broach) && (npitch <> 0))                       REM adjusts for thread lead
                        zstart_adj = fmodx(zinc_tooth * (inspection_tooth[i] - 1), npitch)
                        if (tlead > 0.0) zstart_adj = fmod(npitch - zstart_adj, npitch)
                    endif

                    display_status("MOVING TO INSPECT RAKE")
                    c_inc = edge_c + crake_adj - pitch * (inspection_tooth[i] - 1) * tooth_direction REM 802.12
'                   if use_q qbegin(1)
                    move("yzc", fa_yb, fa_zb + zstart_adj, c_inc + zstart_adj * ANG_PERZ(tlead))
'                   if use_q qdecel()
                    move("x", fa_xb)
'                   if use_q qgo()
                    slew_speed(min_slew)
                    move("xyz", fa_xp, fa_yp, fa_zp + zstart_adj)
                    scan_increment(fa_inc)
                    display_status("INSPECTING RAKE TOOTH " + SINT(inspection_tooth[i]))
                    if (is_broach)
                        m~ = str(stime() - st) + ",measure_broach_rake(" + str(root_diameter) + ", " + str(tip_diameter) + ", " + SINT(num_rake_pts) + ", " + str(probe_rad) + ", " + str(target_defl) + ")"
                        measure_broach_rake(root_diameter, tip_diameter, num_rake_pts, probe_rad, target_defl)
                    else
                        mdir[0] = 1 mdir[1] = 0 mdir[2] = 0 mdir[3] = 0
                        x_inc = edge_rad - .5 * root_diameter
                        inc_scan(mdir, "scramble", "xzc", x_inc, x_inc * tan(faceangle / r_con), x_inc * tan(faceangle / r_con) * ANG_PERZ(tlead)) 
                    endif
                    if (do_disp) chart("PROCESS FILE")
                    slew_speed(max_slew)
                    inc_move("xyz", fa_xb - fa_xp, fa_yb - fa_yp, fa_zb - fa_zp)				
                endif

REM             if (is_broach && insp_step_angle)
                if (is_shaper && insp_step_angle) REM bke 1201.03
                    display_status("MOVING TO INSPECT STEP")
                    gosub STEPANG_SUB
                endif

                if (insp_clear_angle)
                    strt_ht = 0.0
                    scan_len = edge_ht - base_height
                    ca_xp_sub = ca_xp
                    ca_yp_sub = ca_yp
                    ca_zp_sub = ca_zp
                    ca_xb_sub = ca_xb
                    ca_yb_sub = ca_yb
                    ca_zb_sub = ca_zb
                    scan_tooth = inspection_tooth[i]
                    gosub TOOTH_TIP_SCAN_SUB
                endif			

                if (tlead <> 0.0) move("x", x_clear)
            endif
        next i

        if ((is_broach) && insp_step_angle)
            display_status("MOVING TO INSPECT GULLET LEAD")
            for i = 0 to intThreads - 1
                if (inspection_thread[i] > 0)
                    if (npitch <> 0)
                        zstart_adj = fmodx(zinc_tooth * (inspection_thread[i] - 1), npitch)
                        if (tlead > 0.0) zstart_adj = fmod(npitch - zstart_adj, npitch)
                    endif
                    gosub STEPANG_SUB
                endif
            next i
        endif
    endif
    return ' end of INSPECT_FACE_ANGLE_SUB
'#endregion @INSPECT_FACE_ANGLE_SUB
'#region @ROOT_SCAN_SUB
ROOT_SCAN_SUB
' if we verify that this is not necessary later, remove this
    m~ = str(stime() - st) + ",SET_IPTH = DEFAULT"    ' 1404.29 mpw   reset to default "in position threshhold"
    set_inpos_thresh()          ' 1404.29 mpw

    if (is_scan_root = 1)
'       *******************************************************************************************************************************************
'       *******************************************************************************************************************************************
        m~ = str(stime() - st) + ",S9 ****************************************************** Section 9 - Root Scan *****************************************************"
'       *******************************************************************************************************************************************
'       *******************************************************************************************************************************************
        if (is_def_prof)
            angle_to_root = 0
        else
            angle_to_root = ?
        endif

        root_probe_tip = ?
        num_of_roots = ?
        root_start_dir = ?
        root_start_rad = ?

        dim root_tooth[num_of_roots]
        for i = 0 to (num_of_roots - 1)
            root_tooth[i] = ?		
        next i

        if (root_probe_tip <> "OPERATOR SELECT") && (root_probe_tip <> gear_probe_tip) && (app_height > -999.0)
'           if use_q qbegin(1)
            move("x", x_clear)
'           if use_q qdecel()
            move("z", app_height)
'           if use_q qgo()
        endif

        cur_probe_tip = root_probe_tip
        gosub PROBE_CHANGE_SUB

REM bke 711.28
'       if use_q qbegin(1)
        if (root_probe_tip <> "OPERATOR SELECT") && (root_probe_tip <> gear_probe_tip)
            if (app_height > -999.0)
                move("z", app_height)
'               if use_q qdecel()
                move("xy", x_clear, 0.0)
'               if use_q qdecel()
            endif
            move("z", zp[0])
'           if use_q qdecel()
            move("xy", x_clear, 0.0)
'           if use_q qdecel()
        endif

        c_lf_sap = c_1 + angle_to_root
        first_root_tooth = 1
        if ((num_of_roots < 5) || (num_of_roots = 8)) first_root_tooth = root_tooth[0]
        y_pos = 0.0
        display_status("MOVING TO SCAN THE ROOT")
        move("x", x_clear)
'       if use_q qdecel()
        if (is_def_prof)
            move("zc", zp[0], c_lf_sap - (pitch * (first_root_tooth - 1) * tooth_direction))
        else
            move("yzc", y_pos, zp[0], c_lf_sap - (pitch * (first_root_tooth - 1) * tooth_direction))
        endif
'       if use_q qgo()

        if ((root_start_dir <> 0) && (is_def_prof = 0))
            display("JOG THE Y-AXIS READY TO TOUCH OD ON LEFT FLANK IN THE RADIAL-X DIRECTION")
            read_dsp(axis, oversamp)
            y_pos = axis[1]
        endif

        for i = 0 to num_of_roots - 1
            if (num_of_roots < 5)
                display_status("SCANNING ROOT " + SINT(root_tooth[i]))
            else
                display_status("SCANNING ROOT " + SINT(i + 1))
            endif

            if ((root_start_dir <> 1) && (is_def_prof = 0)) move("x", root_start_rad)

            scan_root()

'           if use_q qbegin(1)
            move("xz", x_clear, zp[0])
            if (i < (num_of_roots - 1))
'               if use_q qdecel()
                if ((num_of_roots < 5) || (num_of_roots = 8))
                    move("c", c_lf_sap - (pitch * (root_tooth[i + 1] - 1) * tooth_direction))
                else
                    move("c", c_lf_sap - pitch * (i + 1) * tooth_direction)
                endif
            endif
'           if use_q qgo()

            msg = "PROCESS FILE"
            if (do_disp) chart(msg)
        next i
    endif
    return ' end of ROOT_SCAN_SUB
'#endregion @ROOT_SCAN_SUB
'#region @RELATED_FEATURES_SUB
RELATED_FEATURES_SUB
    no_z_return = 0

    if (insp_relfeat)
'       *******************************************************************************************************************************************
'       *******************************************************************************************************************************************
        m~ = str(stime() - st) + ",S10 ************************************************* Section 10 - Related Features *************************************************"
'       *******************************************************************************************************************************************
'       *******************************************************************************************************************************************
        rfeat_reftooth = ?
        num_of_rfeats = ?
        rfeat_reftype = ?		REM 0:CL of tooth, 1:CL of space, 2:LEFT flank, 3:RIGHT flank

        coord rfeat_coord

        REM rotate by X pitches, where X is the reference tooth number (base 1)
        r_1 = cp[0] - pitch * (rfeat_reftooth - 1) * tooth_direction
        r_2 = cp[1] - pitch * (rfeat_reftooth - 1) * tooth_direction

        REM rotate to get left and right flank contact points on centerline (y = 0.0)
        switch (straight_sided)
        case 0: 'involute
            if (do_setup_cl = 1)
                REM since we were "on center", rotate by the involute of the transverse pressure angle
                r_1 = r_1 - INVO(tpai[0]) * neg_if_int
                r_2 = r_2 + INVO(tpai[1]) * neg_if_int
            else
                REM since we were base tangent, rotate by the transverse pressure angle
                r_1 = r_1 + tpai[0] * neg_if_int
                r_2 = r_2 - tpai[1] * neg_if_int
            endif
            break
        case 1: 'straight sided
            r_1 = r_1 + tpai[0] * neg_if_int
            r_2 = r_2 - tpai[1] * neg_if_int
            break
        case 2: 'parallel sided
            r_1 = r_1 + tpai[0] * neg_if_int
            r_2 = r_2 - tpai[1] * neg_if_int
            break
        endswitch

        REM make sure the r_1 and r_2 values are properly in the range of 0.0 <= r_val <= 360.0
        r_1 = fmod(720.0 + r_1, 360.0)
        r_2 = fmod(720.0 + r_2, 360.0)

        setup_ang = 0.0
        switch (rfeat_reftype)
        case 0: REM center line of tooth
            REM subtract 1 pitch from right flank if we setup on spaces
            if (inspect_space_tooth = 0) r_2 = fmod(360.0 + r_2 - pitch * tooth_direction, 360.0)
            setup_ang = CLEN(r_2, r_1)
            ref_ang = r_1 + setup_ang * 0.5
            break
        case 1: REM center line of space
            REM add 1 pitch to right flank if we setup on teeth
            if (inspect_space_tooth = 1) r_2 = fmod(360.0 + r_2 + pitch * tooth_direction, 360.0)
            setup_ang = CLEN(r_2, r_1)
            ref_ang = r_1 + setup_ang * 0.5
            break
        case 2: REM (Penta) left flank
            ref_ang = r_1
            break
        case 3: REM (Penta) right flank
            ref_ang = r_2
            break
        endswitch
		
        if use_q qbegin(1)
        move("x", tip_clear(x_clear)) REM bke 1212.11
        if use_q qdecel()
        move("c", ref_ang)
        if use_q qgo()

'       cp0_rev = cp0_rev + setup_ang * .5 / 360.0 REM adjust revolution by reference offset	REM disabled mpw 1809.14
        cur_rev = get_axis_pos(3, 1) / 360.0
        diff_rev = cp0_rev - cur_rev
'       if (abs(fmod(abs(diff_rev), 2.0) - 1) < .5)
'           inc_move("c", 360.0 * sgn(diff_rev))
'       endif

        max_clearht = -999.9
        use_journals = 1 REM use to disable prompts/automate moves in ZREF_SETUP_SUB

        yps[0] = yp[0] + (probe_rad - target_defl) * yn[0]
        yps[1] = yp[1] + (probe_rad - target_defl) * yn[1]
        zps[0] = zp[0] + (probe_rad - target_defl) * zn[0]
        zps[1] = zp[1] + (probe_rad - target_defl) * zn[1]

        for rf_idx = 1 to num_of_rfeats
            rfeat_name = load_rfeat_values(rf_idx)
            rfeat_meas = ?

            if (rfeat_meas = 1)
                rfeat_type = ?
                rfeat_probe_tip = ?

                if ((rfeat_probe_tip <> "OPERATOR SELECT") && (rfeat_probe_tip <> gear_probe_tip))
                    no_z_return = 1
                    if (app_height > -999.0)
                        move("z", app_height)
                    endif
                endif

                cur_probe_tip = rfeat_probe_tip
                gosub PROBE_CHANGE_SUB

                display_status("PREPARING TO INSPECT RELATED FEATURE #" + SINT(rf_idx))

                switch (rfeat_type)
                case 0: ' internal timing
                case 1: ' external timing
                    gosub RFEAT_TIMING_SUB
                    break
                case 2: ' internal xy circle
                case 3: ' external xy circle
                    gosub RFEAT_TRUE_POS_XY_CIRCLE_SUB
                    break
                case 4: ' perpendicularity & flatness
                    gosub RFEAT_PERP_FLAT_SUB
                    break
                case 5: ' diameter runout
                    gosub RFEAT_DIAM_RUNOUT_SUB
                    break
                case 6: ' conicity
                    gosub RFEAT_CONICITY_SUB
                    break
                case 7: ' profile
                    gosub RFEAT_PROFILE_SUB
                    break
                case 8: 'True Position of a Rectangle
                    gosub RFEAT_TRUE_POS_RECTANGLE_SUB
                    break
                case 9: 'Chamfer Tolerance
                    gosub RFEAT_CHAMFER_SUB
                    break
                case 10: ' Single Edge Timing
                    gosub RFEAT_EDGE_TIMING_SUB
                    break
                endswitch
            endif
        next rf_idx

        if (max_clearht > -999.9) move("z", max_clearht)
    endif
    return ' end of RELATED_FEATURES_SUB
'#endregion @RELATED_FEATURES_SUB
'#region @RFEAT_TIMING_SUB
RFEAT_TIMING_SUB
    rfeat_val1 = ?          REM timing type 0 = involute, 1 = straight, 2 = parallel
    rfeat_nomang = ?
    rfeat_idod = ?
    rfeat_pitchang = ?
    rfeat_bcd = ?
    rfeat_lead = ?
    rfeat_inspdia = ?
    rfeat_inspht = ?
    rfeat_clearht = ?
    rfeat_clearrad = ?      REM actually clearance RADIUS jjm 906.08 bke 1302.15
    rfeat_val2 = ?          REM tooth profile(positive)/include(negative) angle or parallel sided width
    rfeat_val3 = ?          REM 0:CL of tooth, 1:CL of space, 2:LEFT flank, 3:RIGHT flank

    rfeat_nomang = -rfeat_nomang

REM bke 1302.15 rfeat_clearrad = rfeat_cleardia	REM this should cut down on confusion

    neg_if_int = 1.0
    if (rfeat_idod = 0.0) neg_if_int = -1.0

    rfeat_ang_adjust = 0.0

    if (rfeat_clearht < max_clearht) rfeat_clearht = max_clearht
    if ((rfeat_idod = 0) && (rfeat_clearrad > (0.5 * tip_diameter))) rfeat_clearrad = (0.5 * tip_diameter) REM bke 910.12
    if ((rfeat_idod = 1) && (rfeat_clearrad < (0.5 * tip_diameter))) rfeat_clearrad = (0.5 * tip_diameter) REM bke 910.12

    rfeat_inspht = inspection_height + rfeat_inspht			REM height of main part plus offset to timed part

    if (rfeat_val1 = 0)				REM involute form
        dim was_bha[2] dim was_tpai[2] dim was_bcr[2]

        was_straight_sided = straight_sided
        was_bha[0] = bha[0] was_bha[1] = bha[1] was_tpai[0] = tpai[0] was_tpai[1] = tpai[1] was_bcr[0] = bcr[0] was_bcr[1] = bcr[1]

        straight_sided = 0
        bha[0] = 0.0 bha[1] = 0.0
        if (rfeat_lead <> 0.0)
            bha[0] = atn(PI * rfeat_bcd / rfeat_lead) * r_con
            bha[1] = bha[0]
        endif
        tpai[0] = acos(rfeat_bcd / rfeat_inspdia) * r_con
        tpai[1] = tpai[0] REM in case we ever need dual geom					
        bcr[0] = .5 * rfeat_bcd
        bcr[1] = bcr[0]

        REM nominal angle needs to be converted to be relative to centerline of tooth (external) or space (internal)
        switch (rfeat_val3)
        case 0: 'already on centerline
            if (rfeat_idod = 0) rfeat_ang_adjust = 0.5 * rfeat_pitchang * neg_if_int
            break
        case 1: 'wrong centerline, rotate half of a pitch to get to centerline
            if (rfeat_idod = 1) rfeat_ang_adjust = -0.5 * rfeat_pitchang * neg_if_int
            break
        case 2: 'left flank, rotate one quarter of a pitch to get to centerline
            rfeat_ang_adjust = -0.25 * rfeat_pitchang * neg_if_int
            break
        case 3: 'right flank, rotate one quarter of a pitch to get to centerline
            rfeat_ang_adjust = 0.25 * rfeat_pitchang * neg_if_int
            break
        endswitch
        rfeat_nomang = fmod(360.0 + rfeat_nomang + rfeat_ang_adjust, 360.0)

        if (rfeat_idod = 1) REM tooth bke 1202.29 restored from 910.08
            x_clear = rfeat_clearrad + probe_diameter
        else
            x_clear = rfeat_clearrad * cos (tpai[0] / r_con) - probe_diameter
        endif

        display_status("INSPECTING RELATED FEATURE #" + SINT(rf_idx))

        xnorm = 0.0
        ynorm = abs(cos(bha[0] / r_con))
        znorm = -sin(bha[0] / r_con)

        x_pos = rfeat_bcd / 2.0
        y_pos = -x_pos * tan(tpai[0] / r_con) * neg_if_int - probe_rad * ynorm
        if (rfeat_val3 = 3) y_pos = x_pos * tan(tpai[1] / r_con) * neg_if_int - probe_rad * -ynorm

'       if use_q qbegin(1)
        if (rfeat_clearht <> 0.0) && (rfeat_clearht > -999.9)
            move("z", rfeat_clearht) REM bke 910.20
'           if use_q qdecel()
        endif
        move("xy", x_clear, y_pos)

' measure the left flank -- not used in right flank only timing (rfeat_val3 == 4)
        if (rfeat_val3 <> 3)
            z_pos = rfeat_inspht - probe_rad * znorm

            if (rfeat_idod = 1) REM tooth bke 1202.29 restored from 910.08
                c_pos = fmod(360.0 + ref_ang + rfeat_nomang - tpai[0] * neg_if_int + 0.4 * rfeat_pitchang, 360.0)
            else
                pitch_adjust = 0.0
                if (rfeat_val3 = 0) pitch_adjust = rfeat_pitchang
                c_pos = fmod(360.0 + ref_ang + rfeat_nomang - pitch_adjust - tpai[0] * neg_if_int - 0.1 * rfeat_pitchang, 360.0)
            endif

'           if use_q qdecel()
            move("yzc", y_pos, z_pos, c_pos)
'           if use_q qdecel()
            move("x", x_pos)
'           if use_q qgo()

            gosub UNLOAD_PROBE_SUB
            gosub FLANK_SETUP_SUB
            read_dsp(axis, oversamp)
            f_1 = axis[3]

            REM rotate this value by transverse pressure angle to put this point on centerline
            f_1 = fmod(360.0 + f_1 + tpai[0] * neg_if_int, 360.0)

'           if use_q qbegin(1)
            move("x", x_clear)
        endif

' measure the right flank -- not used in left flank only timing (rfeat_val3 == 3)
        if (rfeat_val3 <> 2)
            xnorm = 0.0
            ynorm = -ynorm
            znorm = -znorm
			
            y_pos = x_pos * tan(tpai[1] / r_con) * neg_if_int - probe_rad * ynorm
            z_pos = rfeat_inspht - probe_rad * znorm

            if (rfeat_idod = 1) REM tooth bke 1202.29 restored from 910.08
                pitch_adjust = 0.0
                if (rfeat_val3 = 1) pitch_adjust = rfeat_pitchang
                c_pos = fmod(360.0 + ref_ang + rfeat_nomang + pitch_adjust + tpai[1] * neg_if_int - 0.4 * rfeat_pitchang, 360.0)
            else
                c_pos = fmod(360.0 + ref_ang + rfeat_nomang + tpai[1] * neg_if_int + 0.1 * rfeat_pitchang, 360.0)
            endif

'           if use_q qdecel()
            move("yzc", y_pos, z_pos, c_pos)
'           if use_q qdecel()
            move("x", x_pos)
'           if use_q qgo()

            gosub UNLOAD_PROBE_SUB
            gosub FLANK_SETUP_SUB
            read_dsp(axis, oversamp)
            f_2 = axis[3]

            REM rotate this value by transverse pressure angle to put this point on centerline
            f_2 = fmod(360.0 + f_2 - tpai[1] * neg_if_int, 360.0)

'           if use_q qbegin(1)
            move("x", x_clear)
        endif
		
        if ((rfeat_clearht <> 0.0) && (rfeat_clearht > -999.9))
'           if use_q qdecel()
            move("z", rfeat_clearht) REM bke 910.20
        endif
'       if use_q qgo()

        switch (rfeat_val3)
        case 0: 'centerline of tooth
        case 1: 'centerline of space
            setup_ang = CLEN(f_2, f_1)
            rfeat_ang = CLEN(f_1 + setup_ang * 0.5, ref_ang)
            break
        case 2: 'left flank only
            rfeat_ang = CLEN(f_1, ref_ang)
            break
        case 3: 'right flank only
            rfeat_ang = CLEN(f_2, ref_ang)
            break
        endswitch

        rfeat_ang = -rfeat_ang

        msg = "PROCESS POINT "+str(rfeat_ang)
        bha[0] = was_bha[0] bha[1] = was_bha[1] tpai[0] = was_tpai[0] tpai[1] = was_tpai[1] bcr[0] = was_bcr[0] bcr[1] = was_bcr[1]
        straight_sided = was_straight_sided
    else
        dir = 1
        if (rfeat_idod = 1) dir = -1

        if (rfeat_val1 = 1)			REM straight sided
            if (rfeat_type = 0) REM bke 910.20
                x_clear = rfeat_clearrad
                x_pos = .5 * rfeat_inspdia
                y_pos = 0.0
                z_pos= rfeat_inspht
                target_rad1 = x_pos
'               if use_q qbegin(1)
                if ((rfeat_clearht <> 0.0) && (rfeat_clearht > -999.9))
                    move("z", rfeat_clearht) REM bke 910.20
'                   if use_q qdecel()
                endif
                move("x", x_clear)
'               if use_q qdecel()
                move("yzc", y_pos, z_pos, fmod(360.0 + ref_ang + rfeat_nomang, 360.0)) REM bke 910.20
'               if use_q qdecel()
                move("x", x_pos)
'               if use_q qgo()
                gosub DSP_POS_SUB
                target_rad2 = sqr(position[0]^2 + position[1]^2)
                if (abs(target_rad2 - target_rad1) > .050)
                    display("the probe is not reaching the target radius.  Do you want to continue?")
                endif

                display_status("INSPECTING RELATED FEATURE #" + SINT(rf_idx))
                touch(0, 0, 0, dir)
                read_dsp(axis, oversamp)
                f_1 = axis[3]

                y_pos = -y_pos
                ynorm = -ynorm

                move("c", fmod(360.0 + ref_ang + rfeat_nomang, 360.0)) REM bke 910.20
                touch(0, 0, 0, -dir)
                read_dsp(axis, oversamp)
                f_2 = axis[3]

'               if use_q qbegin(1)
                move("xc", x_clear, fmod(360.0 + ref_ang + rfeat_nomang, 360.0))
                if ((rfeat_clearht <> 0.0) && (rfeat_clearht > -999.9))
'                   if use_q qdecel()
                    move("z", rfeat_clearht) REM bke 910.20
                endif
'               if use_q qgo()
            else
                if (rfeat_val2 < 0.0)
                    half_included_angle = abs(rfeat_val2) / 2.0               REM started with included angle
                else
                    half_included_angle = (rfeat_val2 - rfeat_pitchang / 2.0) REM convert tooth profile angle to included angle 
                endif

                x_clear = rfeat_clearrad * cos (half_included_angle / r_con) - probe_diameter * dir  REM Fix SSS parts jjm 906.08
                x_pos = .5 * rfeat_inspdia * cos(half_included_angle / r_con)  REM Fix SSS parts jjm 906.08
                y_pos = (.5 * rfeat_inspdia * sin(half_included_angle / r_con) - .5 * probe_diameter) * dir REM Fix SSS parts jjm 906.08
                z_pos= rfeat_inspht
                target_rad1 = sqr(x_pos^2 + y_pos^2)
'               if use_q qbegin(1)
                if ((rfeat_clearht <> 0.0) && (rfeat_clearht > -999.9))
                    move("z", rfeat_clearht) REM bke 910.20
'                   if use_q qdecel()
                endif
                move("x", x_clear)
'               if use_q qdecel()
                move("yzc", y_pos, z_pos, fmod(360.0 + ref_ang + rfeat_nomang + half_included_angle * dir, 360.0)) REM bke 910.20
'               if use_q qdecel()
                move("x", x_pos)
'               if use_q qgo()
                gosub DSP_POS_SUB
                target_rad2 = sqr(position[0]^2 + position[1]^2)
                if (abs(target_rad2 - target_rad1) > .050)
                    display("the probe is not reaching the target radius.  Do you want to continue?")
                endif

                display_status("INSPECTING RELATED FEATURE #" + SINT(rf_idx))
                xnorm = 0.0
                ynorm = dir
                znorm = 0.0

                gosub UNLOAD_PROBE_SUB
                gosub FLANK_SETUP_SUB
                read_dsp(axis, oversamp)
                f_1 = axis[3]

                y_pos = -y_pos
                ynorm = -ynorm

'               if use_q qbegin(1)
                move("x", x_clear)
'               if use_q qdecel()
                move("yc", y_pos, fmod(360.0 + ref_ang + rfeat_nomang - half_included_angle * dir, 360.0)) REM bke 910.20
'               if use_q qdecel()
                move("x", x_pos)
'               if use_q qgo()

                gosub UNLOAD_PROBE_SUB
                gosub FLANK_SETUP_SUB
                read_dsp(axis, oversamp)
                f_2 = axis[3]

'               if use_q qbegin(1)
                move("x", x_clear)
'               if use_q qdecel()
                move("y", 0.0)
                if ((rfeat_clearht <> 0.0) && (rfeat_clearht > -999.9))
'                   if use_q qdecel()
                    move("z", rfeat_clearht) REM bke 910.20
                endif
'               if use_q qgo()
            endif
            setup_ang = CLEN(f_2, f_1)
            rfeat_ang = CLEN(f_1 + setup_ang * 0.5, ref_ang)

            rfeat_ang = -rfeat_ang

            msg = "PROCESS POINT "+str(rfeat_ang)
        else						REM parallel sided
            x_clear = rfeat_clearrad - probe_diameter * dir

            if (rfeat_val2 < 0.0) REM ps space bke 909.28
                y_off[0] = 0.25 * (rfeat_val2 + probe_diameter)
            else
                y_off[0] = 0.6 * (rfeat_val2 + probe_diameter)
            endif

'           if use_q qbegin(1)
            if (rfeat_clearht > -990.0)
                move("z", rfeat_clearht)
'               if use_q qdecel()
            endif
            move("x", x_clear)
'           if use_q qdecel()
            move("yzc", y_off[0], rfeat_inspht, (ref_ang + rfeat_nomang))
'           if use_q qdecel()
            move("x", (rfeat_inspdia / 2.0))
'           if use_q qgo()

            display_status("INSPECTING RELATED FEATURE #" + SINT(rf_idx))

            touch(0, dir, 0, 0)
            gosub DSP_POS_SUB
            y_pos_1 = position[1]

'           if use_q qbegin(1)
            if (rfeat_val2 > 0.0)
                move("x", x_clear)
'               if use_q qdecel()
            endif
            move("y", -y_off[0])
            if (rfeat_type = 1)
'               if use_q qdecel()
                move("x", (rfeat_inspdia / 2.0))
            endif
'           if use_q qgo()

            touch(0, -dir, 0, 0)
            gosub DSP_POS_SUB
            y_pos_2 = position[1]

'           if use_q qbegin(1)
            move("x", x_clear)
'           if use_q qdecel()
            move("y", 0.0)
            if (rfeat_clearht > -990.0)
'               if use_q qdecel()
                move("z", rfeat_clearht)
            endif
'           if use_q qgo()

            rfeat_ang = atn2(.5 * (y_pos_1 + y_pos_2), 0.5 * rfeat_inspdia) * r_con REM bke 909.28

            if (y_pos_2 > y_pos_1)
                rfeat_width = abs(y_pos_2 - y_pos_1)
            else
                rfeat_width = abs(y_pos_1 - y_pos_2)
            endif
            if (rfeat_val2 > 0.0) REM ps tooth
                rfeat_width = rfeat_width - probe_diameter
            else REM ps space
                rfeat_width = rfeat_width + probe_diameter
            endif
            rfeat_width = rfeat_width * cos(rfeat_ang / r_con)

            rfeat_ang = rfeat_nomang - rfeat_ang
REM         rfeat_ang = CLEN(rfeat_ang, rfeat_nomang)
            if (rfeat_ang < -180.0) rfeat_ang = (rfeat_ang + 360.0)
            if (rfeat_ang > 180.0) rfeat_ang = (rfeat_ang - 360.0)

            rfeat_ang = -rfeat_ang

            msg = "PROCESS POINT " + str(rfeat_ang) + ", " + str(rfeat_width)
        endif
    endif
    if (do_disp) chart(msg)
    return ' end of RFEAT_TIMING_SUB
'#endregion @RFEAT_TIMING_SUB
'#region @RFEAT_TRUE_POS_XY_CIRCLE_SUB
RFEAT_TRUE_POS_XY_CIRCLE_SUB
    rfeat_inspdia = ?		REM diameter
    rfeat_val1 = ?			REM x_location
    rfeat_val2 = ?			REM y_location
    rfeat_zrefval = ?		REM z approach
    rfeat_inspht = ?		REM z inspecation
    rfeat_val3 = ?			REM start angle
    rfeat_val4 = ?			REM angle length
    rfeat_val5 = ?			REM number of points to touch

    dir = 1
    if (rfeat_type = 2) dir = -1

    cinc = rfeat_val4 / rfeat_val5
    if (rfeat_val4 < 360.0) rfeat_val5 = rfeat_val5 + 1		REM inc rfeat_val5 to get point on end

REM bke 911.09      rfeat_insprad = ((0.5+(0.05*dir))*rfeat_inspdia)+((probe_rad+1.0)*dir)REM radius of feature (size)
    rfeat_insprad = (0.5 * rfeat_inspdia) + ((probe_rad + 1.0) * dir) REM bke 911.09
    rfeat_radius = sqr(rfeat_val1^2 + rfeat_val2^2)						REM radius of feature center (position)
    rfeat_ang_setup = atn2(rfeat_val2, rfeat_val1) * r_con				REM angle from setup tooth/space to rotate for feature
    rfeat_ang = fmod(ref_ang - rfeat_ang_setup, 360.0)					REM using negative agle to move to correct location
    if (rfeat_insprad < 0.0) rfeat_insprad = 0.0

    rfeat_tmp_ang = fmod(rfeat_val3 + 360, 360)
    rfeat_tmp_x = rfeat_radius + rfeat_insprad * cos(rfeat_tmp_ang / r_con)
    rfeat_tmp_y = rfeat_insprad * sin(rfeat_tmp_ang / r_con)

'   if use_q qbegin(1)
    move("z", rfeat_zrefval)
'   if use_q qdecel()
    move("xyc", rfeat_tmp_x, rfeat_tmp_y, rfeat_ang)
'   if use_q qdecel()
    move("z", rfeat_inspht)
'   if use_q qgo()

    setbase(rfeat_coord, "PCS")
    translate(rfeat_coord, rfeat_radius, 0.0, rfeat_inspht)

    display_status("INSPECTING RELATED FEATURE #" + SINT(rf_idx))

    dim points[4, rfeat_val5]

    j = 0
    while (j < rfeat_val5)
        if (j <> 0) rfeat_tmp_ang = rfeat_tmp_ang + cinc
        rfeat_tmp_x = rfeat_insprad * cos(rfeat_tmp_ang / r_con) + rfeat_radius
        rfeat_tmp_y = rfeat_insprad * sin(rfeat_tmp_ang / r_con)
        if (j = 0)
            move("xy", rfeat_tmp_x, rfeat_tmp_y)
        else
            xy_arc_scan(rfeat_coord, cinc / r_con)
        endif

        touch(-cos(rfeat_tmp_ang / r_con) * dir, -sin(rfeat_tmp_ang / r_con) * dir, 0, 0)
        wait(.1)
        read_position(points[j])
        points[0, j] = points[0, j] - probe_rad * cos(rfeat_tmp_ang / r_con) * dir
        points[1, j] = points[1, j] - probe_rad * sin(rfeat_tmp_ang / r_con) * dir

        move("xy", rfeat_tmp_x, rfeat_tmp_y)

        j = j + 1
	wend
REM bke 911.09  xy_arc_scan(rfeat_coord, (-1.0 * fmod(rfeat_val3 + rfeat_val4, 360)) / r_con)   REM was missing rfeat_coord
REM mpw 1404.23 xy_arc_scan(rfeat_coord, -1.0 * rfeat_val4 / r_con) REM bke 911.09
    if (rfeat_val4 = 360.0)
        xy_arc_scan(rfeat_coord, cinc / r_con)
    else
        xy_arc_scan(rfeat_coord, (-cinc * (rfeat_val5 - 1)) / r_con)
    endif

    rfeat_tmp_ang = fmod(rfeat_val3 + 360, 360)
    rfeat_tmp_x = rfeat_radius + rfeat_insprad * cos(rfeat_tmp_ang / r_con)
    rfeat_tmp_y = rfeat_insprad * sin(rfeat_tmp_ang / r_con)

    move("xy", rfeat_tmp_x, rfeat_tmp_y)

    move("z", rfeat_zrefval)

    mx = 0
    my = 0
    mr = 0

    lsq_circle(points, rfeat_val5, mx, my, mr)

    for j = 0 to (rfeat_val5 - 1)
        rfeat_tmp_rad = sqr((points[0, j] - mx)^2 + (points[1, j] - my)^2)
        if (j = 0)
            rfeat_min_rad = rfeat_tmp_rad
            rfeat_max_rad = rfeat_tmp_rad
        else
            if (rfeat_tmp_rad < rfeat_min_rad) rfeat_min_rad = rfeat_tmp_rad
            if (rfeat_tmp_rad > rfeat_max_rad) rfeat_max_rad = rfeat_tmp_rad
        endif
    next j

    rfeat_tmp_x = mx
    rfeat_tmp_y = my
    mx = rfeat_tmp_x*cos(rfeat_ang_setup / r_con) - rfeat_tmp_y*sin(rfeat_ang_setup / r_con)
    my = rfeat_tmp_y*cos(rfeat_ang_setup / r_con) + rfeat_tmp_x*sin(rfeat_ang_setup / r_con)

    msg = "PROCESS POINT "+str(mx)+" "+str(my)+" "+str(mr * 2.0)+" "+str(rfeat_min_rad * 2.0)+" "+str(rfeat_max_rad * 2.0)
    if (do_disp) chart(msg)
    return ' end of RFEAT_TRUE_POS_XY_CIRCLE_SUB
'#endregion @RFEAT_TRUE_POS_XY_CIRCLE_SUB
'#region @RFEAT_PERP_FLAT_SUB
RFEAT_PERP_FLAT_SUB
    rfeat_idod = ?
    rfeat_zrefval = ?
    rfeat_inspht = ?
    rfeat_clearrad = ? REM bke 1302.15
    rfeat_inspdia = ?
    rfeat_val1 = ?		REM End Diameter
    rfeat_val2 = ?		REM Points to Touch
    rfeat_val3 = ?		REM Number of Revolutions

    dir = 1.0
    if (rfeat_idod = 0) dir = -1.0
    scan_speed(15)

    if (rfeat_val3 = 3)
        scan_increment(get_axis_counts("c") * 1080.0 / 999.0)
    else
        scan_increment(get_axis_counts("c"))
    endif

    plane_z_shift = probe_rad * dir - base_height

'   if use_q qbegin(1)
    move("zc", rfeat_zrefval, ref_ang)
'   if use_q qdecel()
    move("xy", rfeat_clearrad, 0.0) REM bke 1302.15
'   if use_q qdecel()
    move("z", rfeat_inspht)
'   if use_q qdecel()
    move("x", (rfeat_inspdia / 2.0))
'   if use_q qgo()
    if (rfeat_val2 > 0)
        rfeat_pts = (rfeat_val2 * rfeat_val3)
        dim points[4,rfeat_pts]
        for j = 0 to (rfeat_pts - 1)
            display_status("INSPECTING RELATED FEATURE #" + SINT(rf_idx) + " (" + SINT(j+1) + " / " + SINT(rfeat_pts) + ")")
            touch(0.0, 0.0, dir, 0.0)
            read_dsp(dspall, oversamp)

            rad = sqr(((dspall[3] + dspall[0]) ^ 2) + (dspall[1] ^ 2))
            points[0,j] = rad * cos(dspall[6] / R_CON)
            points[1,j] = rad * sin(dspall[6] / R_CON)
            points[2,j] = dspall[5] + dspall[2] + plane_z_shift
            points[3,j] = dspall[6]

'           if use_q qbegin(1)
            inc_move("z", -dir)	
'           if use_q qdecel()
            inc_move("xc", ((rfeat_val1 - rfeat_inspdia) / (2.0 * rfeat_pts)), (-360.0 / rfeat_val2))
'           if use_q qgo()
        next j
    else
        touch(0.0, 0.0, dir, 0.0)
'       read_dsp(probe, oversamp)
'       inc_move("z", probe[2] + (target_defl * dir))
        setdefl(target_defl, 0.0, 0.0, dir)     REM mpw 1404.23
        display_status("INSPECTING RELATED FEATURE #" + SINT(rf_idx))
        rfeat_pts = inc_scan("scramble", "cxz", (rfeat_val3 * -360.0), ((rfeat_val1 - rfeat_inspdia) / 2.0), 0.0)
        inc_move("z", -1.0 * dir)
    endif
'   if use_q qbegin(1)
    move("x", rfeat_clearrad) REM bke 1302.15
'   if use_q qdecel()
    move("z", rfeat_zrefval)
'   if use_q qgo()
    gosub PLANE_ANALYSIS
    return ' end of RFEAT_PERP_FLAT_SUB
'#endregion @RFEAT_PERP_FLAT_SUB
'#region @RFEAT_DIAM_RUNOUT_SUB
RFEAT_DIAM_RUNOUT_SUB
    rfeat_idod = ?		REM internal = 0, external = 1
    rfeat_clearht = ?	REM clearance height
    rfeat_inspht = ?	REM inspection height
    rfeat_clearrad = ?	REM clearance radius bke 1302.15
    rfeat_inspdia = ?	REM inspection diameter
    rfeat_nomang = ?	REM angle from tooth / space 1 to first diameter runout location
    rfeat_val1 = ?		REM	number of locations for runout test
    rfeat_val2 = ?		REM	backoff distance between touches (clearance)
    rfeat_val3 = ?		REM relative height = 1, absolute height = 0

    dim points[4, rfeat_val1] dim rfeat_touchlist[rfeat_val1 + 1]

    for i = 0 to ([rfeat_val1] - 1)
        rfeat_touchlist[i] = ?	REM list of "teeth" to touch
    next i
    rfeat_touchlist[i] = 0.0

    dir = 1.0
    if (rfeat_idod = 0) dir = -1.0

    if (rfeat_val3 = 1) rfeat_inspht = rfeat_inspht + inspection_height

    if ((rfeat_idod = ie_type) && (rfeat_inspht >= base_height) && (rfeat_inspht <= (base_height + facewidth)))
        rfeat_nomang = rfeat_nomang + (rfeat_inspht - inspection_height) * ANG_PERZ(lead[0])
    endif

    c_pos = ref_ang + rfeat_nomang

    if (rfeat_clearht <> 0.0) REM bke 812.01
        move("z", rfeat_clearht)
    endif

    if (use_q) REM bke 1302.15
        gosub DSP_ALL_SUB
        arc_to_rad = rfeat_clearrad + probe_rad * dir
        arc_to_y = 0.0
        arc_to_z = axis[2]
        arc_to_c = axis[3]
        gosub ARC_TO_SUB
    else
        move("x", rfeat_clearrad + probe_rad * dir)
        move("y", 0.0)
    endif

    if use_q qbegin(1)
    move("z", rfeat_inspht)
    if use_q qdecel()
    move("xc", ((rfeat_inspdia / 2.0) + rfeat_val2 * dir + probe_rad * dir), c_pos + rfeat_touchlist[0])
    if use_q qdecel()

    display_status("INSPECTING RELATED FEATURE #" + SINT(rf_idx))

    for j = 0 to (rfeat_val1 - 1)
        inc_move("x", rfeat_val2 * dir * -0.9) REM mpw 910.07 move 90% of the way back into the part to speed this up
        if use_q qgo()

REM bke 911.17 test for defl and backoff to avoid vector slow down hang-up
        read_dsp(probe)
        if (abs(probe[0]) > .015)
            inc_move("x", 1.25 * probe[0])
        endif

        touch(-1.0 * dir, 0.0, 0.0, 0.0)
        wait(.1)
        read_position(points[j])
        r = sqr(points[0, j]^2 + points[1, j]^2) REM bke 812.15
        ang = points[3,j] - c_pos
        if (ang < 0) ang = ang + 360.0
        if (ang > 360) ang = ang - 360.0
        points[0,j] = r * cos(ang / r_con)
        points[1,j] = r * sin(ang / r_con)
        points[2,j] = 0.0
        points[3,j] = 0.0                                

        if use_q qbegin(1)
        inc_move("x", rfeat_val2 * dir)
        if use_q qdecel()
        inc_move("c", rfeat_touchlist[j+1] * tooth_direction)
        if use_q qdecel()
    next j
    move("xc", rfeat_clearrad + probe_rad * dir, c_pos) REM bke 1302.15
    if (rfeat_clearht <> 0.0) REM bke 812.01
        if use_q qdecel()
        move("z", rfeat_clearht)
    endif
    if use_q qgo()

    mx = 0
    my = 0
    mr = 0
    tir = 0
    if (rfeat_val1 < 3) REM since there are only 1 or 2 points, don't calculate X or Y center point or runout
        mr = sqr(points[0, 0]^2 + points[1, 0]^2)
        mind = mr
        maxd = mr
        if (rfeat_val1 > 1)
            mr = sqr(points[0, 1]^2 + points[1, 1]^2)
            if (mr < mind)
                mind = mr               REM since 2nd is less than 1st, set mind and add 1st (maxd)
            else
                maxd = mr               REM since 2nd is greater than (or equal to) 1st, set maxd and add 1st (mind)
            endif
            mr = 0.5 * (mind + maxd)    REM half of the sum is the average
            tir = maxd - mind
            mind = 2.0 * (mind - probe_rad * dir)
            maxd = 2.0 * (maxd - probe_rad * dir)
        endif
    else
        lsq_circle(points, rfeat_val1, mx, my, mr, tir)

        for j = 0 to (rfeat_val1 - 1) REM bke 812.01
            mdia = 2.0 * (sqr((points[0,j] - mx) ^ 2 + (points[1,j] - my) ^2) - probe_rad * dir)
            if (j = 0)
                maxd = mdia
                mind = maxd
            else
                maxd = maxf(maxd, mdia)
                mind = minf(mind, mdia)
            endif
        next j
    endif
    msg = "PROCESS POINT "+str(mx)+" "+str(my)+" "+str((2.0 * mr) - (probe_diameter * dir))+" "+str(tir)+" "+str(maxd)+" "+str(mind) REM bke 812.01
    if (do_disp) chart(msg)
    return ' end of RFEAT_DIAM_RUNOUT_SUB
'#endregion @RFEAT_DIAM_RUNOUT_SUB
'#region @RFEAT_CONICITY_SUB
RFEAT_CONICITY_SUB
    rfeat_idod = ?		REM internal = 0, external = 1
    rfeat_zrefval = ?	REM clearance height
    rfeat_inspht = ?	REM inspection height
    rfeat_clearrad = ?	REM clearance radius REM bke 1302.15
    rfeat_inspdia = ?	REM inspection diameter
    rfeat_nomang = ?	REM angle from tooth / space 1 to inspection
    rfeat_val1 = ?		REM	length of scan

    dir = 1.0
    if (rfeat_idod = 0) dir = -1.0
    scan_speed(20)
    scan_increment(rfeat_val1 * get_axis_counts("z") / 600.0)	REM 600 samples

    c_pos = ref_ang - rfeat_nomang

    read_dsp(axis, oversamp)

'   if use_q qbegin(1)
    if ((axis(0) > rfeat_clearrad) && (rfeat_idod = 1)) || ((axis(0) < rfeat_clearrad) && (rfeat_idod = 0))     REM bke 1302.15
        move("z", rfeat_zrefval)
'       if use_q qdecel()
        move("xy", rfeat_clearrad, 0.0)
    else
        move("xy", rfeat_clearrad, 0.0)
'       if use_q qdecel()
        move("z", rfeat_zrefval)
    endif
'   if use_q qdecel()
    move("zc", rfeat_inspht, c_pos)
'   if use_q qdecel()
    move("x", (rfeat_inspdia / 2.0) + 1.0)
'   if use_q qgo()

    touch((-1.0 * dir), 0.0, 0.0, 0.0)
    read_dsp(probe, oversamp)
    inc_move("x", probe[0] - (target_defl * dir))
    display_status("INSPECTING RELATED FEATURE #" + SINT(rf_idx))
    rfeat_pts = inc_scan("scramble", "z", rfeat_val1)
'   if use_q qbegin(1)
    inc_move("x", dir)
'   if use_q qdecel()
    move("z", rfeat_inspht)
'   if use_q qdecel()
    move("x", rfeat_clearrad) REM bke 1302.15
'   if use_q qdecel()
    move("z", rfeat_zrefval)
'   if use_q qgo()
    gosub LINE_ANALYSIS
    return ' end of RFEAT_CONICITY_SUB
'#endregion RFEAT_CONICITY_SUB
'#region @RFEAT_PROFILE_SUB
RFEAT_PROFILE_SUB
    rfeat_inspdia = ?		REM diameter (only for round profiles)
    rfeat_val1 = ?			REM x_location
    rfeat_val2 = ?			REM y_location
    rfeat_zrefval = ?		REM z approach
    rfeat_inspht = ?		REM z inspecation
    rfeat_val3 = ?			REM start angle
    rfeat_val4 = ?			REM angle length
    rfeat_val5 = ?			REM number of points
    rfeat_idod = ?			REM round / flat , 10 * internal / external

    is_flat = fmod(rfeat_idod, 10)
    rfeat_idod = rfeat_idod - is_flat
    is_external = rfeat_idod / 10

    dir = 1
    if (is_external = 0) dir = -1

    cinc = rfeat_val4 / rfeat_val5

    if (is_flat = 0)
        rfeat_insprad_clr = (0.5 * rfeat_inspdia) + ((probe_rad + 1.0) * dir)   REM radius just clear of contact point
        rfeat_insprad_prb = (0.5 * rfeat_inspdia) + probe_rad * dir             REM radius of inspection (includes probe radius offset)
        rfeat_insprad = (0.5 * rfeat_inspdia)                                   REM radius of feature (actual)
        rfeat_radius = sqr(rfeat_val1^2 + rfeat_val2^2)						    REM radius of feature center (position)
        rfeat_ang_setup = atn2(rfeat_val2, rfeat_val1) * r_con				    REM angle from setup tooth/space to rotate for feature
        if (rfeat_insprad_clr < 0.0) rfeat_insprad_clr = 0.0
        if (rfeat_insprad_prb < 0.0) rfeat_insprad_prb = 0.0
        if (rfeat_insprad < 0.0) rfeat_insprad = 0.0
    else
        rfeat_radius = 0.0
        rfeat_ang_setup = rfeat_val2
    endif
    rfeat_ang = fmod(ref_ang - rfeat_ang_setup, 360.0)					REM using negative agle to move to correct location

    if (is_flat = 0)
        rfeat_tmp_ang = fmod(rfeat_val3 + 360, 360)
        rfeat_tmp_x = rfeat_insprad_clr * cos(rfeat_tmp_ang / r_con) + rfeat_radius
        rfeat_tmp_y = rfeat_insprad_clr * sin(rfeat_tmp_ang / r_con)
        inc = rfeat_inspdia * pi * rfeat_val4 / 360.0 / rfeat_val5 * get_axis_counts(0) REM bke 1207.26
    else
        rfeat_tmp_ang = rfeat_ang
        rfeat_tmp_x = rfeat_val3
        rfeat_tmp_y = rfeat_val1 + (probe_rad + 1.0) * dir
        inc = rfeat_val4 / rfeat_val5 * get_axis_counts(0) REM bke 1207.26
    endif

'   if use_q qbegin(1)
    move("z", rfeat_zrefval)
'   if use_q qdecel()
    move("xyc", rfeat_tmp_x, rfeat_tmp_y, rfeat_ang)
'   if use_q qdecel()
    move("z", rfeat_inspht)
'   if use_q qgo()

    setbase(rfeat_coord, "PCS")
    translate(rfeat_coord, rfeat_radius, 0.0, rfeat_inspht)

    display_status("INSPECTING RELATED FEATURE #" + SINT(rf_idx))

    for j = 0 to rfeat_val5
        if (j = 0)
            if (is_flat = 0)
                touch(-cos(rfeat_tmp_ang / r_con) * dir, -sin(rfeat_tmp_ang / r_con) * dir, 0, 0)
            else
                touch(0, -1.0 * dir, 0, 0)
                read_dsp(axis, oversamp)
                rfeat_tmp_y = axis[1]
            endif

            qbegin(1)
            scan_start(10)
            scan_increment(inc)
            slew_speed(40)
            scan_speed(5)
        else
            if (is_flat = 0)
                rfeat_tmp_ang = rfeat_tmp_ang + rfeat_val4 / rfeat_val5
                rfeat_tmp_x = rfeat_insprad_prb * cos(rfeat_tmp_ang / r_con) + rfeat_radius
                rfeat_tmp_y = rfeat_insprad_prb * sin(rfeat_tmp_ang / r_con)
            else
                rfeat_tmp_x = rfeat_tmp_x + rfeat_val4 / rfeat_val5
            endif
            move("xy", rfeat_tmp_x, rfeat_tmp_y)
        endif
    next j

    qgo("scramble", "xy")

    if (is_flat = 0)
        rfeat_tmp_x = rfeat_insprad_clr * cos(rfeat_tmp_ang / r_con) + rfeat_radius
        rfeat_tmp_y = rfeat_insprad_clr * sin(rfeat_tmp_ang / r_con)
        move("xy", rfeat_tmp_x, rfeat_tmp_y)
        xy_arc_scan(rfeat_coord, -1.0 * rfeat_val4 / r_con)
    else
        rfeat_tmp_x = rfeat_tmp_x - rfeat_val4
        rfeat_tmp_y = rfeat_val1 + (probe_rad + 1.0) * dir
        move("y", rfeat_tmp_y)
        move("x", rfeat_tmp_x)
    endif
    move("z", rfeat_zrefval)

    file = get_last_scan_file()
    if (fopen(file, reading, sequential, 1))
        display("error opening " + file)
    else
        j = 0
        px = 0
        py = 0
        pz = 0
        ax = 0
        ay = 0
        min_material = 0
        max_material = 0

        flen = lof(1)
        while (loc(1) < flen)
            fscanf(1, "n,n,n,n,n", py, pz, px, ax, ay)
            cur_x = px + ax
            cur_y = py + ay
            if (is_flat = 0)
                rfeat_tmp_rad = sqr((rfeat_radius - cur_x)^2 + cur_y^2)
                cur_material = (rfeat_tmp_rad - probe_rad * dir - rfeat_insprad) * dir
            else
                cur_material = ((cur_y - probe_rad * dir) - rfeat_val1) * dir
            endif
            if ((j = 0) || (cur_material < min_material)) min_material = cur_material
            if ((j = 0) || (cur_material > max_material)) max_material = cur_material
            j = j + 1
        wend
        close #1
    endif

    msg = "PROCESS POINT "+str(min_material)+" "+str(max_material)+" "+str(max_material - min_material)
    if (do_disp) chart(msg)
    return ' end of RFEAT_PROFILE_SUB
'#endregion RFEAT_PROFILE_SUB
'#region @RFEAT_TRUE_POS_RECTANGLE_SUB
RFEAT_TRUE_POS_RECTANGLE_SUB
    rfeat_nominal_width = ?         REM nominal width
    rfeat_rec_offset = ?            REM rectangle offset (probably won't be used for a while)
    rfeat_clearht = ?               REM clearance height
	rfeat_inspht = ?			    REM inspection height
    rfeat_inspdia = ?               REM inspection diameter
    rfeat_inspdia = rfeat_inspdia *.5
    rfeat_clearrad = ?              REM clearance radius
    rfeat_perp_ang = ?              REM perpendicularity nominal
    rfeat_key_keyway = ?            REM key or keyway
	rfeat_current_angle	= rfeat_perp_ang + ref_ang
	perp_sgn = 1
	dim points[4, 4]

	if (rfeat_key_keyway) 
		dir = 1
		y_adj = rfeat_nominal_width * .4 - probe_rad
	else
		dir = -1
		y_adj = rfeat_nominal_width * .6 + probe_rad
	endif

'   if use_q qbegin(1)
	move("z", rfeat_clearht)
'   if use_q qdecel()
	move("xyc", rfeat_clearrad, 0.0, rfeat_current_angle)
'   if use_q qdecel()

	for j = 0 to 3
		move("yz", y_adj, rfeat_inspht) 
'       if use_q qdecel()
		move("x", sqr(rfeat_inspdia^2 - y_adj^2))
'       if use_q qgo()

		touch( 0, dir, 0, 0)
		read_position(points[j])
		points[1, j] = points[1, j] + probe_rad * dir

'       if use_q qbegin(1)
		if (rfeat_key_keyway = 0)
            move("xy", rfeat_clearrad, y_adj)
'           if use_q qdecel()
        endif
		if (j = 1)
			rfeat_current_angle = rfeat_current_angle + 180
			move("xy", rfeat_clearrad, y_adj)
'           if use_q qdecel()
			move("c", rfeat_current_angle)
'           if use_q qdecel()
		endif
		y_adj = y_adj * -1
		dir = dir * -1
	next j
	move("xy", rfeat_clearrad, y_adj)
'   if use_q qgo()

	cntr_y_1 = (points[ 1, 0] + points[ 1, 1]) / 2
	cntr_y_2 = -(points[ 1, 2] + points[ 1, 3]) / 2
	avg_thick = abs((points[ 1, 0] - points[ 1, 1]) + (points[ 1, 2] - points[ 1, 3])) / 2

	x_dist_1 = sqr(rfeat_inspdia^2 - cntr_y_1^2)
	x_dist_2 = sqr(rfeat_inspdia^2 - cntr_y_2^2)
	x_dist_tot = x_dist_1 + x_dist_2
	y_dist_tot = cntr_y_2 - cntr_y_1

	rough_ang = atn(abs(y_dist_tot) / x_dist_tot)

	if (cntr_y_1 > cntr_y_2) 
		true_perp = rfeat_perpang - rough_ang 
		true_offset = cntr_y_2 - tan(rough_ang)* x_dist_2
	else
		true_perp = rfeat_perp_ang + rough_ang
		true_offset = cntr_y_2 + tan(rough_ang)* x_dist_2
	endif

	msg = "PROCESS POINT "+str(true_perp)+", "+str(true_offset)+", "+str(avg_thick)
	if (do_disp) chart(msg)
    return ' end of RFEAT_TRUE_POS_RECTANGLE_SUB
'#endregion @RFEAT_TRUE_POS_RECTANGLE_SUB
'#region @RFEAT_CHAMFER_SUB
RFEAT_CHAMFER_SUB
    rfeat_val1 = ? rem start of test
    rfeat_val4 = ? rem end of test
    rfeat_num_teeth = ? rem the number of teeth to check
    rfeat_val2 = ? rem start of chamfer
    rfeat_val3 = ? rem end of chamfer
    rfeat_cham_test_type = ? rem type of test
    rfeat_flank = ? rem the flank
    flk = rfeat_flank
    rfeat_szPnts = " "
    low = -1
    high = -1
    runningTotal = 0
    rfeat_szPnts = " "
    old_disp = do_disp
    do_disp = 0
    num_ok = 0
    old_t_d = target_defl
    target_defl = 0.15
    dim yp_old[2] dim zp_old[2] dim yb_old[2] dim zb_old[2]

    yp_old[0] = yp[0]
    yp_old[1] = yp[1]
    zp_old[0] = zp[0]
    zp_old[1] = zp[1]

    yb_old[0] = yb[0]
    yb_old[1] = yb[1]
    zb_old[0] = zb[0]
    zb_old[1] = zb[1]

    yp[0] = yps[0] - (probe_rad - target_defl) * yn[0]
    yp[1] = yps[1] - (probe_rad - target_defl) * yn[1]
    zp[0] = zps[0] - (probe_rad - target_defl) * zn[0]
    zp[1] = zps[1] - (probe_rad - target_defl) * zn[1]

    yb[0] = yp[0] - backoff_defl * yn[0]
    yb[1] = yp[1] - backoff_defl * yn[1]
    zb[0] = zp[0] - backoff_defl * zn[0]
    zb[1] = zp[1] - backoff_defl * zn[1]

    dim rfeat_chamferlist[rfeat_num_teeth]
    for i = 0 to rfeat_num_teeth - 1
        rfeat_chamferlist[i] = ? rem this will populate the tooth list
    next i

    slew_speed(max_slew) 
        for i = 0 to rfeat_num_teeth - 1
            scan_st = .01
            num_samp = 600
REM ref_var = 0
            scan_tooth = rfeat_chamferlist[i]
                if (rfeat_cham_test_type = 1 || rfeat_cham_test_type = 2)
' HELIX SCANS
                    yloc_adj = 0
                    z_move = base_height + rfeat_val1 - (zp[flk] + (probe_rad - target_defl) * zn[flk]) rem 712.13 to fix lead stagger
                    z_scan = rfeat_val4 - rfeat_val1 rem facewidth
                    scan_label = "HELIX"
                    nlocs = 1
                    scan_spd = helix_speed
                    scan_dir = 1
                    if (rfeat_cham_test_type = 2) scan_dir = -1
REM z_scan = z_scan + probe_rad * scan_dir rem must remove after test
                    scan_inc = abs(z_scan)/num_samp
                    z_scan = z_scan * 1.1
                    gosub HELIX_SCAN_SUB
                    ref_var = base_height
                endif
                if (rfeat_cham_test_type = 3 || rfeat_cham_test_type = 4)
' TIP DIAM VERTICAL SCANS
                    strt_ht = rfeat_val1
                    scan_len = rfeat_val4 - rfeat_val1
                    scan_speed(helix_speed)
REM ca_inc = abs(scan_len) / 300.0 * get_axis_counts(2)
                    ca_inc = abs(scan_len) / num_samp * get_axis_counts(2)
                    force_edge_ht = (edge_ht = 0)
                    if (force_edge_ht)
                        edge_ht = fall_off_top
                        edge_c = fmod(cp[0] + 0.5 * CLEN(cp[1], cp[0]) + 360.0, 360.0) + (fall_off_top - inspection_height) * ANG_PERZ(lead[0])
                        ca_xp_sub = 0.5 * tip_diameter + (base_height - edge_ht + strt_ht) * tan(odclearangle / r_con) - (probe_rad - target_defl) * o_xn
                    else
                        ca_xp_sub = ca_xp + strt_ht * tan(odclearangle / r_con)
                    endif
                    ca_yp_sub = -(probe_rad - target_defl) * o_yn
                    ca_zp_sub = base_height + rfeat_val1 - (probe_rad - target_defl) * o_zn

                    ca_xb_sub = ca_xp_sub - probe_rad * o_xn
                    ca_yb_sub = ca_yp_sub - probe_rad * o_yn
                    ca_zb_sub = ca_zp_sub - probe_rad * o_zn

                    scan_len = scan_len * 1.1
                    gosub TOOTH_TIP_SCAN_SUB
                    if (force_edge_ht) edge_ht = 0
                    ref_var = base_height
                endif

                if (rfeat_cham_test_type = 0)
' PROFILE SCANS
                    if (straight_sided)
                        scan_label = "PROFILE"
                        c_sap = 0.0 
                        x_pro_st  = sqr(rfeat_val1^2 - y_off[flk]^2) 
                        x_pro_len = (rfeat_val4 - rfeat_val1) * .5
                    else
                        scan_label = "INVOLUTE"  
                        c_sap = rfeat_val1 rem sap_ang[rfeat_flank]                  
                    endif
                    normal_path = 0
                    helix_y_per_z = 0.0
                    c_scan = rfeat_val4 - rfeat_val1 rem eap_ang[rfeat_flank] - sap_ang[rfeat_flank]
                    zloc_adj = 0
                    nlocs = 1
                    scan_spd = inv_speed
                    scan_inc = abs(c_scan)/num_samp
                    c_scan = c_scan * 1.1
                    gosub PROFILE_SCAN_SUB
                    ref_var = rfeat_val1
                endif
            move("x", tip_clear(x_clear))
            currentNum = find_chamfer(rfeat_val2, rfeat_val3, rfeat_cham_test_type, rfeat_flank, ref_var)
            if (currentNum <> 0)
                rfeat_szPnts = rfeat_szPnts + ", " + str(currentNum)
                if (high < currentNum || high = -1) high = currentNum
                if (low > currentNum || low = -1) low = currentNum
                runningTotal = runningTotal + currentNum
                num_ok = num_ok + 1
            endif
        next i
    if(num_ok = 0) num_ok = 1
    avg = runningTotal / num_ok
    variation = high - low

    do_disp = old_disp       

    yp[0] = yp_old[0] 
    yp[1] = yp_old[1] 
    zp[0] = zp_old[0] 
    zp[1] = zp_old[1] 

    yb[0] = yb_old[0] 
    yb[1] = yb_old[1] 
    zb[0] = zb_old[0] 
    zb[1] = zb_old[1] 

    target_defl = old_t_d 

    msg = "PROCESS POINT " + str(avg) + ", " + str(variation) + ", " + str(high) + rfeat_szPnts 
	if (do_disp) chart(msg)
    return ' end of RFEAT_CHAMFER_SUB
'#endregion @RFEAT_CHAMFER_SUB
'#region @RFEAT_EDGE_TIMING_SUB
RFEAT_EDGE_TIMING_SUB
    rfeat_inspht = ?
    rfeat_clearht = ?
    rfeat_clearrad = ?
    rfeat_nomang = ?	    REM angle from tooth / space 1 to inspection
    rfeat_val1 = ?			REM x start
    rfeat_val2 = ?			REM x end
    rfeat_val3 = ?          REM touch direction (-1 or +1, this is the actual direction to move the Y axis)
    rfeat_val4 = ?          REM y offset

'    rfeat_inspht = inspection_height + rfeat_inspht			REM height of main part plus offset to timed part

    if ((2.0 * rfeat_clearrad) > rfeat_val1)
        x_clear = rfeat_clearrad + probe_diameter
    else
        x_clear = rfeat_clearrad - probe_diameter
    endif

    y_off[0] = rfeat_val4 - (probe_rad + 1.0) * rfeat_val3

'   if use_q qbegin(1)
    if (rfeat_clearht > -990.0)
        move("z", rfeat_clearht)
'       if use_q qdecel()
    endif
    move("x", x_clear)
'   if use_q qdecel()
    move("yzc", y_off[0], rfeat_inspht, (ref_ang - rfeat_nomang))
'   if use_q qdecel()
    move("x", rfeat_val1)
'   if use_q qgo()

    display_status("INSPECTING RELATED FEATURE #" + SINT(rf_idx))

    touch(0, rfeat_val3, 0, 0)
    gosub DSP_POS_SUB
    y_pos_1 = position[1]

    move("xy", rfeat_val2, y_off[0])

    touch(0, rfeat_val3, 0, 0)
    gosub DSP_POS_SUB
    y_pos_2 = position[1]

'   if use_q qbegin(1)
    move("y", y_off[0])
'   if use_q qdecel()
    move("x", x_clear)
    if (rfeat_clearht > -990.0)
'       if use_q qdecel()
        move("z", rfeat_clearht)
    endif
'   if use_q qgo()

    if (rfeat_val2 > rfeat_val1)
        rfeat_ang = atn2((y_pos_2 - y_pos_1), (rfeat_val2 - rfeat_val1)) * r_con
    else
        rfeat_ang = atn2((y_pos_1 - y_pos_2), (rfeat_val1 - rfeat_val2)) * r_con
    endif

    y_off[1] = (0.5 * (y_pos_1 + y_pos_2)) + ((0.5 * (rfeat_val2 + rfeat_val1)) * sin(rfeat_ang / r_con)) + (probe_rad * rfeat_val3)

    rfeat_ang = rfeat_nomang + rfeat_ang
'REM rfeat_ang = CLEN(rfeat_ang, rfeat_nomang)
    if (rfeat_ang < -180.0) rfeat_ang = (rfeat_ang + 360.0)
    if (rfeat_ang > 180.0) rfeat_ang = (rfeat_ang - 360.0)			

    msg = "PROCESS POINT " + str(rfeat_ang) + ", " + str(y_off[1])
    if (do_disp) chart(msg)
    return ' end of RFEAT_EDGE_TIMING_SUB
'#endregion @RFEAT_EDGE_TIMING_SUB
'#region @SYNCHRONIZER_TEETH_SUB
SYNCHRONIZER_TEETH_SUB
    if (insp_dogteeth) REM bke 1207.16 REM bke 1207.26
'       *******************************************************************************************************************************************
'       *******************************************************************************************************************************************
        m~ = str(stime() - st) + ",S11 ******************************************** Section 11 - Inspect Synchronizer Teeth ********************************************"
'       *******************************************************************************************************************************************
'       *******************************************************************************************************************************************
        num_dogteeth = ?
        dim inspection_dogtooth[num_dogteeth]
        for i = 0 to num_dogteeth - 1
            inspection_dogtooth[i] = ?
        next i
        roof_radius = ?
        dim roof_angle[2]
        roof_angle[0] = ?
        roof_angle[1] = ?
        roof_relief_angle = ?
        dogtooth_probe_tip = ?

        cur_probe_tip = dogtooth_probe_tip
        gosub PROBE_CHANGE_SUB

        tpai_sum = (tpai[0] + tpai[1]) * neg_if_int
        ta_insp_ht = CLEN(setup_pos[0], setup_pos[1]) + tpai_sum
        if (inspect_space_tooth = 0) ta_insp_ht = ta_insp_ht + pitch
        z_to_top = fall_off_top - inspection_height
        ta_top = ta_insp_ht + z_to_top * ANG_PERZ(lead[0]) + z_to_top * ANG_PERZ(lead[1])

        c_dt_cl = fmod(360.0 + (setup_pos[0] + tpai[0]) * neg_if_int - .5 * ta_insp_ht, 360.0)

        dim roofang_xn[2] dim roofang_yn[2] dim roofang_zn[2]
        dim x_roof[2] dim y_roof[2] dim z_roof[2]
        dim roof_center_z[num_dogteeth] dim roof_center_c[num_dogteeth]

        roofang_xn[0] = ?
        roofang_xn[1] = ?
        roofang_yn[0] = ?
        roofang_yn[1] = ?
        roofang_zn[0] = ?
        roofang_zn[1] = ?

        x_roof[0] = insp_rad * cos(-.25 * ta_top / r_con) - probe_rad * roofang_xn[0]
        x_roof[1] = insp_rad * cos(.25 * ta_top / r_con) - probe_rad * roofang_xn[1]
        y_roof[0] = insp_rad * sin(-.25 * ta_top / r_con) - probe_rad * roofang_yn[0]
        y_roof[1] = insp_rad * sin(.25 * ta_top / r_con) - probe_rad * roofang_yn[1]
        z_roof[0] = fall_off_top + insp_rad * .25 * ta_top / r_con / tan(roof_angle[0] / r_con) - probe_rad * roofang_zn[0]
        z_roof[1] = fall_off_top + insp_rad * .25 * ta_top / r_con / tan(roof_angle[1] / r_con) - probe_rad * roofang_zn[1]

        move("x", tip_clear(x_clear))
        z_roof_clear = fall_off_top + insp_rad * .5 * ta_top / r_con / tan(minf(roof_angle[0], roof_angle[1]) / r_con) + probe_rad + 2.0

        for i = 0 to 1
'           if use_q qbegin(1)
            move("zc", z_roof_clear, c_dt_cl)
'           if use_q qdecel()
            display_status("setting up " + flk_lbl[i] + " roof angle...")
            move("xy", x_roof[i], y_roof[i])
'           if use_q qgo()
            touch(0.0, 0.0, -1.0, 0.0)
            gosub DSP_POS_SUB
            inc_move("xyz", probe[0] * 2.0, probe[1] * 2.0, probe[2] * 2.0)
            touch(roofang_xn[i], roofang_yn[i], roofang_zn[i], 0.0)
            gosub DSP_POS_SUB
            inc_move("xyz", x_roof[i] - position[0] - backoff_defl * roofang_xn[i], y_roof[i] - position[1] - backoff_defl * roofang_yn[i], -backoff_defl * roofang_zn[i])
            touch(roofang_xn[i], roofang_yn[i], roofang_zn[i], 0.0)
            setdefl(target_defl, roofang_xn[i], roofang_yn[i], roofang_zn[i])
            gosub DSP_POS_SUB
            x_roof[i] = probe[0] + axis[0] + probe_rad * roofang_xn[i] REM put coords on the surface for convenience later if used for surface finish
            y_roof[i] = probe[1] + axis[1] + probe_rad * roofang_yn[i]
            z_roof[i] = probe[2] + axis[2] + probe_rad * roofang_zn[i]
        next i

        nsamps = 101

        for i = 0 to num_dogteeth - 1
'           if use_q qbegin(1)
            move("z", z_roof_clear)
'           if use_q qdecel()
            move("xc", tip_clear(x_clear), c_dt_cl - pitch * (inspection_dogtooth[i] - 1) * tooth_direction)
'           if use_q qgo()

            for j = 0 to 1
                display_status("inspecting " + flk_lbl[j] + " roof angle, tooth " + SINT(inspection_dogtooth[i]) + "...")
                meas_roof_angle(i, j, target_defl, ta_top, insp_rad, x_roof[j], y_roof[j], z_roof[j], nsamps)
                if (do_disp) chart("PROCESS FILE")
                move("z", z_roof_clear)
            next j

            roof_center_z[i] = ?
            roof_center_c[i] = ?

            relief_xn = -sin(roof_relief_angle / r_con)
            relief_zn = -cos(roof_relief_angle / r_con)

            sap_radius = bcr[0] / cos(atn(sap_ang[0] / r_con))

            x_relief = sap_radius - (probe_rad - target_defl) * relief_xn
            y_relief = 0.0
            z_relief = roof_center_z[i] - (sap_radius - insp_rad) * tan(roof_relief_angle / r_con) - (probe_rad - target_defl) * relief_zn
            c_relief = roof_center_c[i]

'           if use_q qbegin(1)
            move("z", z_roof_clear)
'           if use_q qdecel()
            move("xyc", x_relief - backoff_defl * relief_xn, 0.0, c_relief)
'           if use_q qdecel()
            move("z", z_relief - backoff_defl * relief_zn)
'           if use_q qdecel()
            move("xz", x_relief, z_relief)
'           if use_q qgo()

            scan_increment((.5 * tip_diameter - sap_radius) / (nsamps - 1) * get_axis_counts(0))
            mdir[0] = 1 mdir[1] = 0 mdir[2] = 0 mdir[3] = 0
            scan_speed(1.0)
            display_status("inspecting relief angle, tooth " + SINT(inspection_dogtooth[i]) + "...")
            inc_scan(mdir, "scramble", "xyzc", .5 * tip_diameter - sap_radius, 0.0, -(.5 * tip_diameter - sap_radius) * tan(roof_relief_angle / r_con), 0.0)
            if (do_disp) chart("PROCESS FILE")
        next i

        display_status(" ")

'       if use_q qbegin(1)
        move("z", z_roof_clear)
'       if use_q qdecel()
        move("x", tip_clear(x_clear))
'       if use_q qgo()
    endif
    return ' end of SYNCHRONIZER_TEETH_SUB
'#endregion @SYNCHRONIZER_TEETH_SUB
'#region @SURFACE_FINISH_SUB
SURFACE_FINISH_SUB
    if (insp_surf) REM bke 809.19
'       *******************************************************************************************************************************************
'       *******************************************************************************************************************************************
        m~ = str(stime() - st) + ",S12 ********************************************** Section 12 - Inspect Surface Finish **********************************************"
'       *******************************************************************************************************************************************
'       *******************************************************************************************************************************************
' bke 1206.12 need to revert to base tangent coords before performing surface finish
        was_xy_invo = xy_invo
        was_do_tests_cl = do_tests_cl
        if (do_tests_cl = 1)
            do_tests_cl = 0
            xy_invo = 0
            gosub XFORM_CL_2_BT
        endif

        yps[0] = yp[0] + (probe_rad - target_defl) * yn[0]
        yps[1] = yp[1] + (probe_rad - target_defl) * yn[1]
        zps[0] = zp[0] + (probe_rad - target_defl) * zn[0]
        zps[1] = zp[1] + (probe_rad - target_defl) * zn[1]

        prev_defl = target_defl     REM bke 903.25
        target_defl = surf_defl     REM bke 903.25
        prev_probe_id = probe_id
        prev_x_clear = x_clear
        x_clear = x_clear + 2.0 * neg_if_int    REM this helps the surf probe clear the tip

        num_of_surfs = ?
        dim surf_probe_tip[num_of_surfs]
        surf_idx = 0

        scan_acc(.01)   REM this gets rid of spikes between moves during the scans
        scan_dec(.01)

        while (surf_idx < num_of_surfs)
            surf_meas = ?
            if (surf_meas)
                surf_flk = ?
                surf_inc = ?
                surf_length = ?
                surf_location = ?
                surf_normal = ? REM 1 if profile normal plane
                normal_path = surf_normal
                surf_probe_tip[surf_idx] = ?
                surf_start = ?
                surf_tooth = ?
                scan_tooth = surf_tooth
                surf_type = ? REM 0: lead, 1: profile, 2: dog up, 3: dog out (this will inc index of feature)

                move("x", tip_clear(x_clear))

                if (app_height > -999.0) REM bke 1206.15
                    move("z", app_height)
                    read_dsp(dspall, oversamp)
                endif

                cur_probe_tip = surf_probe_tip[surf_idx]
                gosub PROBE_CHANGE_SUB

                if (app_height > -999.0) REM bke 1206.15 - adjust for height difference between probes (if changed)
                    move("z", app_height) REM app_height should be the same as dspall[5]
                    if (ie_type = 0) move("xy", dspall[3], dspall[4]) REM x = tip_clear(x_clear) [at target y], y = yb[flk] + yloc_adj
                endif

                yp[0] = yps[0] - (probe_rad - surf_defl) * yn[0]
                yp[1] = yps[1] - (probe_rad - surf_defl) * yn[1]
                zp[0] = zps[0] - (probe_rad - surf_defl) * zn[0]
                zp[1] = zps[1] - (probe_rad - surf_defl) * zn[1]

                yb[0] = yp[0] - backoff_defl * yn[0]
                yb[1] = yp[1] - backoff_defl * yn[1]
                zb[0] = zp[0] - backoff_defl * zn[0]
                zb[1] = zp[1] - backoff_defl * zn[1]

                flk = surf_flk
                long_scan(1)
                nlocs = 1
                all_data_columns = 1
                touch_mode = 1

                switch (surf_type)
                case 0: REM helix surface scan
                    scan_st = surf_inc
                    scan_inc = surf_inc
                    scan_spd = minf(surf_inc * act_srate, .5)
                    scan_label = "HELIX SURFACE"
REM was roll        yloc_adj = (surf_location * pi * bcr[flk] / 180.0 - y_off[flk]) * sgn(yp[flk])
                    yloc_adj = (sqr((.5 * surf_location)^2 - bcr[flk]^2) - y_off[flk]) * sgn(yp[flk]) REM now diam
REM back to dia     yloc_adj = (surf_location - y_off[flk]) * sgn(yp[flk])
                    z_move = base_height + surf_start - (zp[flk] + (probe_rad - target_defl) * zn[flk]) REM 712.13 to fix lead stagger
                    z_scan = surf_length
                    scan_ratio = abs(ANG_PERZ(lead[flk]))
                    if (scan_ratio > 1)
                        scan_spd = scan_spd * scan_ratio
                    endif
                    gosub HELIX_SCAN_SUB
                    break
                case 1: REM profile surface scan
                    scan_st = surf_inc * 180.0 / (PI * bcr[flk])
                    scan_inc = scan_st
                    scan_spd = minf(scan_inc * act_srate, .5)
                    scan_label = "PROFILE SURFACE"

                    if (surf_location <= 0.0) REM bke 904.15
                        zloc_adj = surf_location + fall_off_top - (zp[flk] + (probe_rad - target_defl) * zn[flk])
                    else
                        zloc_adj = surf_location + base_height - (zp[flk] + (probe_rad - target_defl) * zn[flk])
                    endif

                    c_sap = surf_start * 180.0 / (PI * bcr[flk])
                    c_scan = surf_length * 180.0 / (PI * bcr[flk])
                    helix_y_per_z = 0.0
                    gosub PROFILE_SCAN_SUB
                    break
                case 2: REM syncro up scan
                    x_st = x_roof[flk] - (probe_rad + backoff_defl) * roofang_xn[flk]

                    if (flk = 0)
                        y_st = y_roof[flk] + surf_start * sin(roof_angle[flk] / r_con) - (probe_rad + backoff_defl) * roofang_yn[flk]
                    else
                        y_st = y_roof[flk] - surf_start * sin(roof_angle[flk] / r_con) - (probe_rad + backoff_defl) * roofang_yn[flk]
                    endif

                    z_st = z_roof[flk] + surf_start * cos(roof_angle[flk] / r_con) - (probe_rad + backoff_defl) * roofang_zn[flk]
                    move("zc", z_roof_clear, c_dt_cl - pitch * (surf_tooth - 1) * tooth_direction)
                    move("xyz", x_st, y_st, z_st)
                    touch(roofang_xn[flk], roofang_yn[flk], roofang_zn[flk], 0.0)
                    setdefl(target_defl, roofang_xn[flk], roofang_yn[flk], roofang_zn[flk])

                    scan_speed(minf(surf_inc * act_srate, .5))		
                    scan_start(surf_inc * get_axis_counts("y"))
                    scan_increment(surf_inc * get_axis_counts("y"))

                    mdir[0] = 0.0 mdir[1] = 1.0 mdir[2] = 1.0 mdir[3] = 0.0 
                    if (flk = 0)
                        inc_scan(mdir, "scramble", "xyzc", 0.0, surf_length * sin(roof_angle[flk] / r_con), surf_length * cos(roof_angle[flk] / r_con), 0.0)
                    else
                        inc_scan(mdir, "scramble", "xyzc", 0.0, -surf_length * sin(roof_angle[flk] / r_con), surf_length * cos(roof_angle[flk] / r_con), 0.0)
                    endif
                    if (do_disp) chart("PROCESS FILE")
                    move("z", z_roof_clear)
                    move("x", tip_clear(x_clear))
                    break
                case 3: REM syncro out scan
                    x_st = x_roof[flk] + surf_start * cos(roof_relief_angle / r_con) - (probe_rad + backoff_defl) * roofang_xn[flk]
                    y_st = y_roof[flk] - (probe_rad + backoff_defl) * roofang_yn[flk]
                    z_st = z_roof[flk] - surf_start * sin(roof_relief_angle / r_con) - (probe_rad + backoff_defl) * roofang_zn[flk]
                    move("zc", z_roof_clear, c_dt_cl - pitch * (surf_tooth - 1) * tooth_direction)
                    move("xyz", x_st, y_st, z_st)
                    touch(roofang_xn[flk], roofang_yn[flk], roofang_zn[flk], 0.0)
                    setdefl(target_defl, roofang_xn[flk], roofang_yn[flk], roofang_zn[flk])

                    scan_speed(minf(surf_inc * act_srate, .5))		
                    scan_start(surf_inc * get_axis_counts("y"))
                    scan_increment(surf_inc * get_axis_counts("y"))

                    mdir[0] = 1.0 mdir[1] = 0.0 mdir[2] = 1.0 mdir[3] = 0.0 
                    inc_scan("scramble", "xyzc", surf_length * cos(roof_relief_angle / r_con), 0.0, -surf_length * sin(roof_relief_angle / r_con), 0.0)
                    if (do_disp) chart("PROCESS FILE")
                    move("z", z_roof_clear)
                    move("x", tip_clear(x_clear))
                    break
                endswitch
            endif
            surf_idx = ?
        wend

        scan_acc(scan_accel)
        scan_dec(scan_decel)

        target_defl = prev_defl REM bke 903.25
        move("x", tip_clear(x_clear))
        x_clear = prev_x_clear
        cur_probe_tip = prev_probe_id
'       gosub PROBE_CHANGE_SUB          DO NOT call this, it is handled in FINISHED_CLEANUP_SUB if necessary
        remove_surf_probe = 1

        yp[0] = yps[0] - (probe_rad - target_defl) * yn[0]
        yp[1] = yps[1] - (probe_rad - target_defl) * yn[1]
        zp[0] = zps[0] - (probe_rad - target_defl) * zn[0]
        zp[1] = zps[1] - (probe_rad - target_defl) * zn[1]

        yb[0] = yp[0] - backoff_defl * yn[0]
        yb[1] = yp[1] - backoff_defl * yn[1]
        zb[0] = zp[0] - backoff_defl * zn[0]
        zb[1] = zp[1] - backoff_defl * zn[1]

        xy_invo = was_xy_invo
        do_tests_cl = was_do_tests_cl
        if ((do_setup_cl = 1) && (do_tests_cl = 1)) ' bke 1206.12 conditionally revert back to CL if surface finish block changed to BT
            gosub XFORM_BT_2_CL
        endif
    endif
    return ' end of SURFACE_FINISH_SUB
'#endregion @SURFACE_FINISH_SUB
'#region @FINISHED_CLEANUP_SUB
FINISHED_CLEANUP_SUB
'   *******************************************************************************************************************************************
'   *******************************************************************************************************************************************
    m~ = str(stime() - st) + ",S13 *************************************************  Section 13 - Return to Start *************************************************"
'   *******************************************************************************************************************************************
'   *******************************************************************************************************************************************
    if (no_z_return <> 1)
        gosub RETURN_TO_START_SUB
    else
        if (clear_diam > 0.0) move("x", tip_clear(0.5 * clear_diam))
        if (app_height > -999.0) move("z", app_height) REM externals now move up to app_height as well
'       if ((app_height > -999.0) && (ie_type = 0)) move("z", app_height)
    endif

    if (is_cluster = 1)
        move("xy", .5 * retract_diam, 0.0)

        if (cluster_height > -999.0)        REM move("z", cluster_height)	REM internal
	        gosub DSP_ALL_SUB
	        inc_move("zc", cluster_height - axis[2], (cluster_height - axis[2]) * ANG_PERZ(lead[flank = 2]))
        endif

'       if use_q qbegin(1)
        move("xy", cluster_pos[0], 0.0)
        if (cluster_last = 1)
'           if use_q qdecel()
            move("zc", cluster_pos[2], cluster_pos[3])
        endif
'       if use_q qgo()
    else
        if (retract_diam > 0.0)
            get_probe_ref(ref) REM in case probe changed since start of program
            x_pos_lim = get_axis_param(0,0) - 10.0 - ref[0]
            if((.5 * retract_diam) > x_pos_lim) retract_diam = x_pos_lim * 2.0
            if (use_q)
                gosub DSP_ALL_SUB
                arc_to_z = axis[2]
                if (clear_diam > 0)
                    arc_to_rad = 0.5 * clear_diam
                else
                    arc_to_rad = x_clear
                endif
                arc_to_y = 0.0
                arc_to_c = axis[3]
                gosub ARC_TO_SUB
                move("x", .5 * retract_diam)
            else
                move("xy", .5 * retract_diam, 0.0)
            endif
        endif
    endif

    gosub END_TEST_SUB

    if (remove_surf_probe = 1)
        gosub PROBE_CHANGE_SUB
    endif
    return ' end of FINISHED_CLEANUP_SUB
'#endregion @FINISHED_CLEANUP_SUB
'#region @RETURN_TO_START_SUB
RETURN_TO_START_SUB
    display_status("MOVING BACK TO START POINT")

    if (clear_diam > 0)
        x_clear = .5 * clear_diam
    else
        x_clear = .5 * tip_diameter + (probe_rad + safety) * neg_if_int
    endif

    if (straight_sided)
        yp[0] = 0.0
        yp[1] = 0.0
        if ((is_ps_spline) && (inspect_space_tooth = 1)) REM bke 906.16 (inspect_space_tooth <> ie_type))
            cp[0] = space_center + pitch / 2
            cp[1] = space_center + pitch / 2
        else
            cp[0] = space_center
            cp[1] = space_center
        endif
        pitch = 0.0
    endif

    c_offset = pitch / 12.0 * (1 - 2 * (flank = 2))
    if (is_def_prof) c_offset = 0.0
REM calculate c_offset before adjusting z_start so we don't mis-rotate based on clearance height on helical parts
    c_offset = c_offset + (z_start - zp[flank = 2]) * ANG_PERZ(lead[flank = 2]) REM bke 805.19 apply this z change to c_offset but not the z change in the line below

    if (app_height > -999.0) z_start = app_height REM bke 1304.02 set z_start before computing c_offset below to maintain z/c helix sync

    if ((do_setup_cl = 0) && (do_tests_cl = 1))
        gosub XFORM_CL_2_BT
        xb[0] = xp[0] - backoff_defl * xn[0]
        yb[0] = yp[0] - backoff_defl * yn[0]
        xb[1] = xp[1] - backoff_defl * xn[1]
        yb[1] = yp[1] - backoff_defl * yn[1]
    endif

    if (use_q)
        if (arbor_jump)
'           qbegin(1)
            move("x", tip_clear(x_clear))
'           qdecel()
            move("z", fall_off_top)
'           qgo()
		    arc_to_z = fall_off_top
        else
		    arc_to_z = z_start
        endif
        if (clear_diam > 0)
            gosub DSP_ALL_SUB
            arc_to_rad = 0.5 * clear_diam
            arc_to_z = axis[2]
        else
            arc_to_rad = x_clear
        endif
        arc_to_y = setup_yp[flank = 2]
        arc_to_c = cp[flank = 2] + c_offset
        gosub ARC_TO_SUB
        if (arbor_jump || (clear_diam > 0)) move("z",z_start)
    else
        move("x", tip_clear(x_clear))
        if (arbor_jump)
            move("z",  fall_off_top)
            move("yc", setup_yp[flank = 2], cp[flank = 2] + c_offset)
            if (clear_diam > 0) move("x", tip_clear(0.5 * clear_diam))
            move("z",  z_start)
        else
            if (clear_diam > 0)
                move("xyc", tip_clear(0.5 * clear_diam, setup_yp[flank = 2]), setup_yp[flank = 2], cp[flank = 2] + c_offset)
                move("z", tip_clear(z_start)
            else
                move("yzc", setup_yp[flank = 2], z_start, cp[flank = 2] + c_offset)
            endif
        endif
    endif
    return ' end of RETURN_TO_START_SUB
'#endregion @RETURN_TO_START_SUB
'#region @DEFINED_PROFILE_SETUP_SUB
DEFINED_PROFILE_SETUP_SUB
    if (zref_type > 1) gosub INITIAL_SETUP_SUB REM bke 909.18

    display_status("MEASURING ACTUAL SIZE")

    for i = 0 to 1
        zb[i] = zp[i] - backoff_defl * zn[i]
'        if use_q qbegin(1)
        move("yz", yb[i], zb[i])
'        if use_q qdecel()
        move("x", xb[i])
'        if use_q qgo()
        touch(xn[i], yn[i], zn[i], 0)
        gosub DSP_POS_SUB
        xb[i] = xp[i] + xp[i] - position[0] - backoff_defl * xn[i]
        yb[i] = yp[i] + yp[i] - position[1] - backoff_defl * yn[i]
        zb[i] = zp[i] + zp[i] - position[2] - backoff_defl * zn[i]
        move("xyz", xb[i], yb[i], zb[i])
        touch(xn[i], yn[i], zn[i], 0)
        setdefl(target_defl, xn[i], yn[i], zn[i])

        gosub DSP_POS_SUB

        xp[i] = position[0] + probe_rad * xn[i]
        yp[i] = position[1] + probe_rad * yn[i]

        if (i = 0)
            zloc = position[2] + probe_rad * zn[i]
        else
            angle = (zloc - (position[2] + probe_rad * zn[i])) * ANG_PERZ(tlead)
            equal(rotate, mcs)
            rotatez(rotate, angle / r_con)
            transform(rotate, xp[i], yp[i])
        endif

        angle = atn2(yp[i], xp[i]) * r_con + getprofang(inspection_diameter, i) - getprofang(2.0 * sqr(xp[i]^2 + yp[i]^2), i)

        if (angle < 0) angle = angle + 360.0
        setup_pos[i] = angle

        msg = "PROCESS LOCATION " + str(angle)
        if (do_disp) chart(msg)		

        setmsg[i] = str(axis[0])+" "+str(axis[1])+" "+str(axis[2])

        if (insp_idx = 0)
            msg = "PROCESS POINT "+str(probe[1])+" "+str(probe[2])+" "+str(probe[0])+" "+str(axis[0])+" "+str(axis[1])+" "+setmsg[i]+" "+str(setup_pos[i])
            if (do_disp) chart(msg)
            if (do_indx_log) index_log(msg)
        endif

        xp[i] = axis[0]
        yp[i] = axis[1]
        zp[i] = axis[2]
        cp[i] = axis[3] REM these coords may be cl or bt
        xb[i] = xp[i] - backoff_defl * xn[i]
        yb[i] = yp[i] - backoff_defl * yn[i]
        zb[i] = zp[i] - backoff_defl * zn[i]

'        if use_q qbegin(1)
        move("yz", yb[i], zb[i])
'        if use_q qdecel()
        move("x", x_clear)
'        if use_q qgo()
    next i
    move("yz", yb[0], zb[0])
    return ' end of DEFINED_PROFILE_SETUP_SUB
'#endregion @DEFINED_PROFILE_SETUP_SUB
'#region @STANDARD_SETUP_SUB
STANDARD_SETUP_SUB
    move("y", y_pos)
    if (inspect_pl_runout = 1)
        gosub PLR_SUB
    else REM not plr
	
        if (zref_type > 1) ' not finding an edge for setup height
            if (find_gap = 0)
                slew_speed(min_slew)
                jog_off("yz")
                prompt = "JOG THE PROBE CLEAR TO MOVE INTO THE TOOTH GAP"

                if (ie_type = 0) REM internal
					bmp = "c:\penta\img\index_int.bmp"
				else
					bmp = "c:\penta\img\index_ext.bmp"
                endif
                gosub BMP_PROMPT_SUB
            else
                display_status("AUTOMATICALLY FINDING THE TOOTH GAP")
            endif
			
            find_tooth_gap(x_pos, num_teeth) REM bke 812.02

            if (straight_sided)
                move("x", x_pos)
                gosub INITIAL_SETUP_SUB
            endif
        endif

        if (straight_sided) REM block is here to support when zref_type <> 2
            temp_z_dist = inspection_height - z_at_init
            space_center = fmod(360.0 + space_center + temp_z_dist * ANG_PERZ(lead[flank = 2]), 360.0)
        endif

        jog_on("xyzct")
        slew_speed(min_slew)

		' flank loop
        for flk = 0 to 1
            this_flank_only = ((flk = 0) && (flank = 1)) || ((flk = 1) && (flank = 2))
            if (flank & 2^flk) REM  if flk is being checked
                xnorm = xn[flk]
                ynorm = yn[flk]
                znorm = zn[flk]
                x_pos = xp[flk] REM  bcd
                y_pos = yp[flk] REM  tpa offset by probe rad * yn
                z_pos = zp[flk] REM  insp ht offset by probe rad * zn

                move("x", x_pos)

                if (straight_sided = 0) gosub UNLOAD_PROBE_SUB					

                slew_speed(max_slew)

				' adjust z_pos for shaver lands
                if (is_shaver)
					' if fixed ref height or right flank, need to find the land center on this flank
                    if ((zref_type > 1) || (flk = 1)) REM bke 909.18
						' we are in position, but possibly not on a flank
						' the next line will do the land find and leave us at the center of a flank,
						' undeflected (probe radius already in z)
                        z_pos = land_center(shv_gash_wid, shv_land_wid, 1)
                        gosub UNLOAD_PROBE_SUB
                        if ((flk = 0) || this_flank_only)
                            npitch = shv_gash_wid + shv_land_wid
                            zinc_tooth = npitch * numstarts / num_teeth
                        endif
                    endif
                endif


                display_status("FINDING THE FIRST TOOTH")

                gosub FLANK_SETUP_SUB
                gosub DSP_POS_SUB

                snapshot_msg = "TOOTH SETUP, FLANK, " + str(flk)

                if (find_scal)
                    idx_scan_xn = xnorm
                    idx_scan_yn = ynorm
                    idx_scan_zn = znorm
                    idx_scan_xb = axis[0] - backoff_defl * xnorm
                    idx_scan_yb = axis[1] - backoff_defl * ynorm
                    idx_scan_zb = axis[2] - backoff_defl * znorm
                    idx_scan_xp = axis[0] + (scallop_defl - target_defl) * idx_scan_xn
                    idx_scan_yp = axis[1] + (scallop_defl - target_defl) * idx_scan_yn
                    idx_scan_zp = axis[2] + (scallop_defl - target_defl) * idx_scan_zn
                    idx_scan_c = axis[3]

                    if use_q qbegin(1)
                    move("x", tip_clear(x_clear))
                    if use_q qdecel()

                    gosub INDEX_SCAN_SUB

' the code below should improve feed scallop tooth thickness accuracy/repeatability
                    if (straight_sided = 0)
                        y_trans = (yp[flk] - (axis[1] + probe[1])) / cos(bha[flk] / r_con)^2
                        c_adj = (y_trans * r_con) / bcr[flk]
                    else
                        y_trans = (yp[flk] - (axis[1] + probe[1])) / ynorm^2
                        axis[1] = yp[flk] - y_trans
                        c_adj = 0
                    endif

                    axis[0] = axis[0] + (target_defl - scallop_defl) * idx_scan_xn
                    axis[1] = axis[1] + (target_defl - scallop_defl) * idx_scan_yn
                    axis[2] = axis[2] + (target_defl - scallop_defl) * idx_scan_zn
                    axis[3] = axis[3] + c_adj 

                    if (feedrate = 0.0)
                        feedrate = meas_feedrate
                        msg = "Please enter the feed rate"
                        feedrate = timed_val(msg, feedrate, 5000)
                    endif

					' record the feedrate in the file, this is especially necessary
					' in case the operator overwrote the value
                    if ((flk = 1) || (flank = 1))
						' this is the last measured flank:
						' either:   flk = 1, which means we are are "on the right flank"
						' or:       flank = 1 means inspection was "left flank only"                        
                        msg = "PROCESS SIZE " + str(feedrate) REM "feedrate" is not really size but it is safe to use this message anyway
                        if (do_disp) chart(msg)
                        if (do_indx_log) index_log(msg)
                    endif
                endif

				' we are at the setup location, deflected - store the position
                xp[flk] = axis[0]
                yp[flk] = axis[1]
                zp[flk] = axis[2]
                cp[flk] = axis[3] REM these coords may be cl or bt
				' todo: is the next line correct when setting up on center line?????
                if (insp_relfeat && (flk = 0)) cp0_rev = get_axis_pos(3,1) / 360.0
				' temporarily use the old code until we decide on a proper solution
				' if (insp_relfeat && (flk = 0)) cp0_rev = val(cmd_control("MG_PAW")) / (get_axis_counts(3)) REM get galil table revolution to evaluate and REMove odd table rotation for related feat measuREMent
                xb[flk] = xp[flk] - xnorm * backoff_defl
                yb[flk] = yp[flk] - ynorm * backoff_defl
                zb[flk] = zp[flk] - znorm * backoff_defl
				
				' xp, yp, zp, cp positions are now deflected axis positions
                snapshot("After FLANK_SETUP_SUB, flk " + str(flk)) ' record this in the log file

				
				' this adjustment will calculate a correction for our current C axis position that would
				' put the table in the correct position to make the touch perfect at this location.
				' read the positions
				GOSUB DSP_POS_SUB
				' calculate the adjustment
				c_correct_adjust = correctpt(flk, position[0], position[1], position[2], position[3], inspection_height, probe_rad)
				' add the correction to the cp[flk] to incorporate it into the setup
				'cp[flk] = cp[flk] + c_correct_adjust

				
				GOSUB PRINT_POS_SUB
				
				' correct locations to base tangent if setting up on centerline
                if (do_setup_cl) REM bke 809.29 this fixes int cl index bug that ka-wood found
				
				
					'cp[flk] is rotated to base tan by c_correct_adjust above, when we are doing cl setup.
					setup_pos[flk] = fmod(360.0 + cp[flk] + c_correct_adjust, 360.0)
				
                    switch (cl_ref_type)
                    case 0:
                        rot = -tan(tpai[flk] / r_con) * sgn(ynorm) * neg_if_int REM conv tpai to rolli and leave in radians
                        break
                    case 1:
                        rot = -tpai[flk] / r_con * sgn(ynorm) * neg_if_int REM conv tpai to rolli and leave in radians
                        break
                    endswitch

                    equal(rotate, mcs) REM reset to mcs	
                    rotatez(rotate, -rot)
					
					'rotate x, y
                    transform(rotate, axis[0], axis[1])
					
					'rotate setup_pos[flk] to get it base tangent for tt calc.
                '    setup_pos[flk] = fmod(360.0 + cp[flk] + rot * r_con, 360.0)
					
					'rotate probe x, probe y
                    transform(rotate, probe[0], probe[1])

					'rotate position[0] and [1] (x&y probe ctr pos
                    transform(rotate, position[0], position[1])

					transform(rotate, xnorm, ynorm)

                    switch (cl_ref_type)
                    case 0:
                        tpai_x2 = -(INVO(tpai[0]) + INVO(tpai[1]))
                        break
                    case 1:
                        tpai_x2 = 0
                        break
                    endswitch
                else
				' not setup on centerline
					cp[flk] = fmod(360.0 + cp[flk] + c_correct_adjust, 360.0)
                    setup_pos[flk] = cp[flk]
                endif

				
                setup_defl[flk] = "PROCESS DATA SETUP "+str(flk)+" "+str(probe[0])+" "+str(probe[1])+" "+str(probe[2])+" "+str(axis[0])+" "+str(axis[1])+" "+str(axis[2])+" "+str(axis[3])+" "+str(setup_pos[flk])
                if (do_disp) chart(setup_defl[flk])

' fixes inspecting at specified diameter instead of pitch diameter
' in all cases, setup_pos is the current c axis position, cp[] (could be base tan or centerline here)
'                setup_pos[flk] = cp[flk]

				' shaver fixed z height - set z_off (inspection height offset) for the land center we are now touching
                contact_z = 0
                if((is_shaver) && (zref_type > 1)) REM bke 909.18
                    contact_z = zp[flk] + (probe_rad - target_defl) * zn[flk] ' back out defl and probe to get contact z
                    if(flk = 0)
						'shift zp[1] to match the new zp[0] - needs to be undeflected here
                        zp[1] = contact_z + probe_rad * zn[0]
                    endif
                    if((flk = 0) || this_flank_only)    REM  left flank, or only flank
                        if (z_off <= 0.0)
                            z_off = contact_z + zland_adj - fall_off_top
                        else
                            z_off = contact_z + zland_adj - base_height
                        endif
                    endif
                endif

				'ps spline - this needs re-tested
                if (is_ps_spline) setup_pos[flk] = probe[1] + axis[1] + probe_rad * ynorm

				'shaper or broach find right flank top edge, not fixed z height
                if ((is_shaper || is_broach) && (flk = 1) && (flank = 3) && (zref_type < 2))
                    gosub SHAP_RIGHT_EDGE_SUB
                    gosub SHAP_OD_MEASURE_SUB
                endif

				' shavers - eap adjustment to make sure we see tip falloff
                if ((is_shaver) && ((flk = 1) || (flank = 1)))
                    for i = 0 to 1
                        if ((flank & (i + 1)) <> 0)
                            eap_ang[i] = sqr(((.5 * tip_diameter) + tip_fall_off)^2 - bcr[i]^2) * r_con / bcr[i]
                        endif
                    next i
                endif

				'PROCESS LOCATION, 1st flank - (will be left, unless testing right flank only)
                if (flk = 0 || (flk = 1 && flank = 2))
                    setmsg[flank = 2] = str(xp[flank = 2]) + " " + str(yp[flank = 2]) + " " + str(zp[flank = 2])

                    if (is_shaper)
                        setup_pos[flank = 2] = shap_edge_c[ynorm < 0]
                    endif

                    msg = "PROCESS LOCATION " + str(setup_pos[flank = 2])
                    if (do_disp) chart(msg)
                    if (do_indx_log) index_log(msg)
                endif

				'PROCESS LOCATION, 2nd flank - (will always be right flank, testing both)
                if (flk = 1 && flank = 3)
                    setmsg[1] = str(xp[1]) + " " + str(yp[1]) + " " + str(zp[1])
                    contact_z_left = zp[0] + (probe_rad - target_defl) * zn[0]
                    contact_z_right = zp[1] + (probe_rad - target_defl) * zn[1]
REM bke 1207.11     setup_z_diff = contact_z_right - contact_z_left
                    setup_z_diff = contact_z_left - contact_z_right REM bke 1207.11

' this is now taken care of by the c_correct_adjust calculation.
'                    if (is_shaper = 0)
'                        setup_pos[1] = setup_pos[1] + setup_z_diff * ANG_PERZ(lead[1])
'                    endif

                    msg = "PROCESS LOCATION " + str(setup_pos[1])
                    if (do_disp) chart(msg)
                    if (do_indx_log) index_log(msg)
                endif

				'Process point when we aren't checking index
                if (insp_idx = 0)     '     probe y              probe z          probe x           x axis          y axis       x, y, z base tan        c setup pos
                    msg = "PROCESS POINT "+str(probe[1])+" "+str(probe[2])+" "+str(probe[0])+" "+str(axis[0])+" "+str(axis[1])+" "+setmsg[flk]+" "+str(setup_pos[flk])
                    if (do_disp) chart(msg)
                    if (do_indx_log) index_log(msg)
                endif

				' move to right flank
                if ((flk = 0) && (flank = 3))
                    move("x", tip_clear(x_clear))
		            if (straight_sided)
                        if (is_ss_spline) REM  straight
                            c_inc = space_center + tpai[0] * neg_if_int
                            if (inspect_space_tooth = 1) c_inc = c_inc - pitch
                        else REM parallel
                            c_inc = space_center
                        endif
                        arc_to_y = yp[1] - yn[1] * backoff_defl
			            arc_to_z = zp[1] - zn[1] * backoff_defl
                  	    arc_to_c = c_inc  
                    else
                        if (inspect_space_tooth = 1)
                            c_adj = -pitch_pct * pitch * neg_if_int
                        else
                            c_adj = (1 - pitch_pct) * pitch * neg_if_int
                        endif					
                        c_inc = tpai_x2 + c_adj
                        if ((is_broach) && (thread_lead <> 0.0)) zp[1] = zp[1] + c_adj / ANG_PERZ(thread_lead)
			            arc_to_y = yp[1]
			            arc_to_z = zp[1]
                  	    arc_to_c = cp[0] + (c_inc * neg_if_int)  
                    endif
		            if (use_q)
                        arc_to_rad = x_clear
                        gosub ARC_TO_SUB
		            else
			            move("xyzc", x_clear, arc_to_y, arc_to_z, arc_to_c)
		            endif
                endif
            else    REM  this flank not being checked, set stuff to 0
                yb[flk] = 0.0
                zb[flk] = 0.0
                cp[flk] = 0.0
                setup_pos[flk] = 0.0
                setup_defl[flk] = "PROCESS DATA SETUP "+str(flk)+" NOT INSPECTED"
            endif
        next flk
    endif					REM end_not_plr
    return ' end of STANDARD_SETUP_SUB
'#endregion @STANDARD_SETUP_SUB
'#region @END_TEST_SUB	
END_TEST_SUB
    msg = "TEST COMPLETED!"
    display_status(msg)
    if (do_disp) chart(msg)
    if (do_indx_log) index_log(msg)
    jog_on("xyzct")
    qbegin(0)
    return ' end of END_TEST_SUB
'#endregion @END_TEST_SUB
'#region @PROMPT_SUB
PROMPT_SUB
    if (skip_prompts = 0) display(prompt)
    return ' end of PROMPT_SUB
'#endregion @PROMPT_SUB
'#region @BMP_PROMPT_SUB
BMP_PROMPT_SUB
    if (skip_prompts = 0) display_bmp(prompt, bmp)
    return ' end of BMP_PROMPT_SUB
'#endregion @BMP_PROMPT_SUB
'#region @FLANK_SETUP_SUB
' new version of FLANK_SETUP_SUB, uses "canned" c++ version in MOTION
REM  enter this sub ready to touch flank at x_pos, y_pos, z_pos, leave against flank at target deflection
FLANK_SETUP_SUB
    m~ = str(stime() - st) + ",FLANK_SETUP_SUB"
    c_temp = 0
    y_temp = y_pos
    flank_setup(x_pos, y_temp, z_pos, c_temp, xnorm, ynorm, znorm, bha[ynorm < 0], bcr[ynorm < 0], target_defl, backoff_defl, min_slew, straight_sided, skip_table_touch, oversamp)
REM in cases were we actually do need c_pos or y_pos to be set (or updated), this will record the new value in the basic log
    c_pos = c_temp  REM updated for all part types except straight sided spline
    y_pos = y_temp  REM updated for striaght sided spline
    return ' end of FLANK_SETUP_SUB
'#endregion @FLANK_SETUP_SUB
'#region @ZREF_SETUP_SUB
ZREF_SETUP_SUB
    m~ = str(stime() - st) + ",ZREF_SETUP_SUB"
' - purpose - to set fall_off_top, and base_height
    if (zref_type > 1) REM fixed z ht bke 909.18
        base_height = zref_val
        fall_off_top = base_height + facewidth
    else
        if ((find_top = 0) && (find_bottom = 0))
            display("WHEN FIND EDGES SELECTED, YOU MUST SELECT AT LEAST BOTTOM OR TOP")
            gosub END_TEST_SUB
            end
        endif

        neg_if_rflank = 1
        if (flank = 2) neg_if_rflank = -1
        y_setup = y_pos

        slew_speed(min_slew)

        if (auto_z_move <> 0)
            display_status("AUTOMATICALLY MOVING TO Z SETUP HEIGHT")

            if ((use_journals = 0) && (is_cluster = 0) && (app_height < -999.0))
                prompt = "JOG THE PROBE CLEAR TO MOVE TO Z SETUP HEIGHT"
                gosub PROMPT_SUB
            endif 

'            if use_q qbegin(1)
            if (clear_diam > 0)
                move("xy", tip_clear(0.5 * clear_diam, y_setup), y_setup)
            else
                move("xy", tip_clear(x_clear, y_setup), y_setup)
            endif
'            if use_q qdecel()
            move("z", z_setup)
'            if use_q qgo()
        else
            display_status("MANUALLY SETTING Z SETUP HEIGHT")

            if (find_bottom = 1)
                prompt = "JOG THE PROBE IN Z NEAR THE BOTTOM EDGE OF THE TOOTH"        
            else
                prompt = "JOG THE PROBE IN Z NEAR THE TOP EDGE OF THE TOOTH"
            endif

            gosub PROMPT_SUB
            read_dsp(axis, oversamp)
            z_setup = axis[2]
        endif

        z_start = z_setup
        move("xy", tip_clear(x_clear, y_setup), y_setup)
        slew_speed(max_slew)

        if (is_def_prof = 0)
		
            if (find_gap = 0)
                jog_off("yz")
                prompt = "JOG THE PROBE CLEAR TO MOVE INTO THE TOOTH"
                if (ie_type = 0) REM internal
                    bmp = "c:\penta\img\index_int.bmp"
                else
                    bmp = "c:\penta\img\index_ext.bmp"
                endif
                gosub BMP_PROMPT_SUB
                jog_on("xyzct")
            else
                display_status("AUTOMATICALLY FINDING THE TOOTH GAP")
            endif
			
            find_tooth_gap(x_pos, num_teeth) REM bke 812.02

            gosub UNLOAD_PROBE_SUB

            if (is_shaver)
                z_setup = land_center(shv_gash_wid, shv_land_wid, flank = 2)
                npitch = shv_gash_wid + shv_land_wid
                zinc_tooth = npitch * numstarts / num_teeth     REM 801.09 update zinc because npitch probably changed
                inc_move("yz", -ynorm * backoff_defl, -znorm * backoff_defl)
            endif

            y_pos = y_setup
            z_pos = z_setup
        endif

        gosub INITIAL_SETUP_SUB REM this only calls FLANK_SETUP_SUB and sets c_offset if involute gear

        read_dsp(axis, oversamp)
        x_setup = axis[0]
        y_setup = axis[1]
        z_setup = axis[2]

        if (is_def_prof && (find_bottom || find_top))
            x_setup = xp[0] REM bke 711.28
            y_setup = yp[0]
            xboff = x_setup - backoff_defl * xnorm
            yboff = y_setup - backoff_defl * ynorm
            zboff = z_setup - backoff_defl * znorm
'            if use_q qbegin(1)
            move("yz", yboff, zboff)
'            if use_q qdecel()
            move("x", xboff)
'            if use_q qgo()
            touch(xnorm, ynorm, znorm, 0)
            setdefl(target_defl, xnorm, ynorm, znorm)
        endif

        if (find_bottom = 1)
            display_status("FINDING BOTTOM EDGE")

            if (is_shaver = 0)
                fall_off_bot = find_edge("down", lead[ynorm < 0], facewidth, bha[ynorm < 0])
'                fall_off_bot = fall_off_bot + probe_rad * znorm REM bke/jjm 810.01
                if (zref_type = 1) REM touch on end face option
                    inc_move("zc", -probe_diameter, -probe_diameter * ANG_PERZ(lead[ynorm < 0]) * neg_if_rflank)
                    read_dsp(axis, oversamp)
                    prev_z = axis[2]
                    prev_c = axis[3]
                    if (is_def_prof = 0)
                        inc_move("c", -pitch * .25 * neg_if_rflank)
                    else
                        inc_move("xy", probe_diameter * xnorm, probe_diameter * ynorm)
                    endif
                    touch(0, 0, 1, 0)
                    gosub DSP_POS_SUB
                    fall_off_bot = position[2] + probe_rad
                    base_height = fall_off_bot
'                    if use_q qbegin(1)
                    move("zc", prev_z, prev_c)
'                    if use_q qdecel()
                    if (is_def_prof = 0)
                        inc_move("zc", probe_diameter, probe_diameter * ANG_PERZ(lead[ynorm < 0]) * neg_if_rflank)
                    else
                        inc_move("xy", -probe_diameter * xnorm, -probe_diameter * ynorm)
                    endif
'                   if use_q qgo()
                else
                    base_height = fall_off_bot
                endif
                fall_off_top = base_height + facewidth
            else REM shaver gashes
                while (abs(probe[1]) > .025)
                    inc_move("zc", -npitch, -npitch * ANG_PERZ(lead[ynorm < 0]))
                    gosub DSP_ALL_SUB
                wend
                gosub DSP_ALL_SUB
                z_land_hgt = axis[2] + npitch
                y_land_pos = axis[1]
                c_land_pos = axis[3] + (npitch * ANG_PERZ(lead[ynorm < 0]))
                x_land_pos = axis[0]
' scan up to find the edge of the last land
                fall_off_bot = find_edge("up", lead[ynorm < 0], facewidth, bha[ynorm < 0])
                fall_off_bot = fall_off_bot + probe_rad * znorm REM bke/jjm 810.01
                base_height = fall_off_bot
                fall_off_top = fall_off_bot + facewidth  REM - fmod(facewidth, npitch)
                move("xyzc", x_land_pos, y_land_pos, z_land_hgt, c_land_pos)    REM move back up to the last full pitch
                z_land_hgt = z_land_hgt + probe_rad * zn[ynorm < 0] 'project back to surface
            endif
        endif

        was_facewidth = facewidth
        if (find_top = 1)
            display_status("FINDING TOP EDGE")

            if (find_bottom = 1)
                zland_adj = 0
                if (is_shaver)
                    zland_adj = fmod(facewidth - safety, npitch)
                endif

                z_move = facewidth - safety - zland_adj
                if (is_shaver) z_move = z_move - 2 * npitch REM 905.22 tlt shortened this by a couple of pitches for shavers
                if (is_broach) z_move = z_setup - fall_off_bot
                c_inc = z_move * ANG_PERZ(lead[ynorm < 0])

'                if use_q qbegin(1)
                inc_move("yz", -ynorm * backoff_defl, -znorm * backoff_defl)
'                if use_q qdecel()
                inc_move("zc", z_move, c_inc)
'                if use_q qdecel()
                inc_move("yz", ynorm * backoff_defl, znorm * backoff_defl)
'                if use_q qgo()

                read_dsp(probe, oversamp)

                if (abs(probe[1]) < (.5 * target_defl * abs(ynorm)))
                    display("ENTERED FACE WIDTH MAY BE TOO LARGE, CORRECT AND TRY AGAIN")
                    gosub END_TEST_SUB
                end
                endif
            endif

            if (is_shaver)
                while (abs(probe[1]) > .025)
                    inc_move("zc", npitch, npitch * ANG_PERZ(lead[ynorm < 0]))
                    gosub DSP_ALL_SUB
                wend
                if(find_bottom = 0)
                    gosub DSP_ALL_SUB
                    z_land_hgt = axis[2] - npitch + probe_rad * zn[ynorm < 0] 'project back to surface
                    y_land_pos = axis[1]
                    c_land_pos = axis[3] - (npitch * ANG_PERZ(lead[ynorm < 0]))
                    x_land_pos = axis[0]
                endif

                fall_off_top = find_edge("down", lead[ynorm < 0], facewidth, bha[ynorm < 0]) + probe_rad * znorm REM bke/jjm 810.01
                if (find_bottom)
                    facewidth = fall_off_top - fall_off_bot 					
                else
                    base_height = fall_off_top - facewidth REM (facewidth - fmod(facewidth, npitch))
                endif
            else REM  all but shavers
                fall_off_top = find_edge("up", lead[ynorm < 0], facewidth, bha[ynorm < 0])

'SHAPER/BROACH
' this could be either left or right flank, although checking one flank only has other problems
                if ((is_shaper) || (is_broach))
                    shap_edge_z[ynorm < 0] = fall_off_top
                    gosub SHAP_TOP_EDGE_FIND
                    fall_off_top = shap_edge_z[ynorm < 0]
                endif

                if (zref_type = 1) REM touch on end face option
                    inc_move("zc", probe_diameter, -probe_diameter * ANG_PERZ(lead[ynorm < 0]) * neg_if_rflank)
                    read_dsp(axis, oversamp)
                    prev_z = axis[2]
                    prev_c = axis[3]
                    if (is_def_prof = 0)
                        inc_move("c", -pitch * .25 * neg_if_rflank)
                    else
                        inc_move("xy", probe_diameter * xnorm, probe_diameter * ynorm)
                    endif
                    touch(0, 0, -1, 0)
                    gosub DSP_POS_SUB
                    fall_off_top = position[2] - probe_rad
                    if (find_bottom)
                        facewidth = fall_off_top - fall_off_bot 
                    else
                        base_height = fall_off_top - facewidth 
                    endif
'                    if use_q qbegin(1)
                    move("zc", prev_z, prev_c)
'                    if use_q qdecel()
                    if (is_def_prof = 0)
                        inc_move("zc", -probe_diameter, probe_diameter * ANG_PERZ(lead[ynorm < 0]) * neg_if_rflank)
                    else
                        inc_move("xy", -probe_diameter * xnorm, -probe_diameter * ynorm)
                    endif
'                    if use_q qgo()
                else
                    if (is_broach = 0)
                        if (find_bottom)
                            facewidth = fall_off_top - fall_off_bot
                        else
                            base_height = fall_off_top - facewidth
                        endif
                    endif
                endif
            endif
        endif REM  find top edge

' code to force insp ht at measured center
' z_off is the offset from the reference edge to the inspection height
        if (facewidth <> was_facewidth)
            if (abs(abs(z_off / was_facewidth) - .5) < .01)
                z_off = .5 * facewidth * sgn(z_off)
                if (num_indexlocs = 1) index_loc[0] = z_off
            endif
        endif
    endif
    return ' end of ZREF_SETUP_SUB
'#endregion @ZREF_SETUP_SUB
'#region @SHAP_TOP_EDGE_FIND
'****************************************************************************************
' to find the top edge of a shaper/broach
'
' call right after find_edge, and setting shap_edge_z[]
' call this with x_pos, y_pos, and the normals set properly for the flank
'
' this will find the top edge of the flank and set shap_edge_z[] and shap_edge_c[]
' it will return deflected against the flank at the cutting edge
' shap_edge_z will be set to the transverse z height of the edge
' shap_edge_c will be set to the c location of the edge
'****************************************************************************************
SHAP_TOP_EDGE_FIND
    gosub DSP_ALL_SUB
    xboff = x_pos - backoff_defl * xnorm
    yboff = y_pos - backoff_defl * ynorm
    z_pos = shap_edge_z[ynorm < 0] - abs(probe_rad * ynorm)
    zboff = z_pos - backoff_defl * znorm
'    if use_q qbegin(1)
    move("xy", xboff, yboff)
'    if use_q qdecel()
    inc_move("zc", zboff - axis[2], (zboff - axis[2]) * ANG_PERZ(lead(ynorm < 0))) '912.12
'    if use_q qgo()
    skip_table_touch = 1
    gosub FLANK_SETUP_SUB
' fall off the top again - this time we should get a closer reading (esp. broaches)
    shap_edge_z[ynorm < 0] = find_edge("up", lead[ynorm < 0], facewidth, bha[ynorm < 0])
    z_pos = shap_edge_z[ynorm < 0] - probe_rad * znorm    '912.12 get probe radius back in z_pos
    zboff = z_pos - backoff_defl * znorm
'    if use_q qbegin(1)
    move("xy", xboff, yboff)
'    if use_q qdecel()
    move("z", zboff)
'    if use_q qgo()
    skip_table_touch = 1
    gosub FLANK_SETUP_SUB
' todo: could add y error correction to this c_pos
    shap_edge_c[ynorm < 0] = c_pos    'flank setup sub sets c_pos to the right value
    return ' end of SHAP_TOP_EDGE_FIND
'#endregion @SHAP_TOP_EDGE_FIND
'#region @INITIAL_SETUP_SUB
INITIAL_SETUP_SUB
    m~ = str(stime() - st) + ",INITIAL SETUP SUB"
    switch (part_type)
    case 1: REM ss spline
    case 2: REM ps spline
        gosub SS_PS_SETUP
        break
    case 4: REM defined profile
        gosub DEF_PROFILE_SETUP
        break
    default:
REM bke 909.08
        if ((is_broach || is_shaper) && straight_sided)
            gosub SS_PS_SETUP
        else
            gosub FLANK_SETUP_SUB
            read_dsp(probe, oversamp)
		    c_offset = (probe[1] * r_con) / bcr[ynorm < 0]
        endif
        break
    endswitch
    return ' end of INITIAL_SETUP_SUB
'#endregion @INITIAL_SETUP_SUB
'#region @DSP_ALL_SUB
DSP_ALL_SUB
    read_dsp(dspall, oversamp)
    probe[0] = dspall[0]
    probe[1] = dspall[1]
    probe[2] = dspall[2]
    axis[0] = dspall[3]
    axis[1] = dspall[4]
    axis[2] = dspall[5]
    axis[3] = dspall[6]
    return ' end of DSP_ALL_SUB
'#endregion @DSP_ALL_SUB
'#region @DSP_POS_SUB
DSP_POS_SUB
    gosub DSP_ALL_SUB
    position[0] = probe[0] + axis[0]
    position[1] = probe[1] + axis[1]
    position[2] = probe[2] + axis[2]
    position[3] = axis[3]
    return ' end of DSP_POS_SUB
'#endregion @DSP_POS_SUB
'#region @PLANE_ANALYSIS
PLANE_ANALYSIS
    max = -999.9
    min = 999.9
    result = 0

    if (rfeat_val2 = 0)
        fname = get_last_scan_file()
        result = fopen(fname, reading, sequential, 3)
        if ((result = 0) && (rfeat_pts > 0))
            dim points[4,rfeat_pts]
            rad = 0
            x = 0
            y = 0
            z = 0
            c = 0
            ht = 0
            rad = 0
            for cur_idx = 0 to (rfeat_pts - 1)
REM read as "probe_y, probe_z, probe_x, x_axis, z_axis, c_axis"
                fscanf(3, "n,n,n,n,n,n", y, z, x, rad, ht, c)
                r = sqr((rad + x)^2 + y^2)
                points[0,cur_idx] = r*cos(c / R_CON)
                points[1,cur_idx] = r*sin(c / R_CON)
                points[2,cur_idx] = ht + z + plane_z_shift
                points[3,cur_idx] = c
                if (points[2,cur_idx] < min) min = points[2,cur_idx]
                if (points[2,cur_idx] > max) max = points[2,cur_idx]
            next cur_idx
            close #3
            plane_z_shift = 0
        endif
    else
        for cur_idx = 0 to (rfeat_pts - 1)
            if (points[2,cur_idx] < min) min = points[2,cur_idx]
            if (points[2,cur_idx] > max) max = points[2,cur_idx]
        next cur_idx
    endif

    if ((max <> -999.9) && (min <> 999.9))
        x_n = 0
        y_n = 0
        z_n = 0
        z_ht = 0
        newmin = 999.9
        newmax = -999.9
        lsq_plane(points, rfeat_pts, x_n, y_n, z_n, z_ht)
        if (z_n < 0.0) ' make sure the normal vector is always pointing UP (z positive)
            x_n = x_n * -1.0
            y_n = y_n * -1.0
            z_n = z_n * -1.0
        endif
        plane_denom = sqr(x_n^2 + y_n^2 + z_n^2)
        for cur_idx = 0 to (rfeat_pts - 1)
            dist_from_plane = (((x_n * points[0,cur_idx]) + (y_n * points[1,cur_idx]) + (z_n * points[2,cur_idx]) - z_ht) / plane_denom)
            if (dist_from_plane < newmin) newmin = dist_from_plane
            if (dist_from_plane > newmax) newmax = dist_from_plane
        next cur_idx

        rfeat_relto = ?

        if (rfeat_relto <> -1)
            rfeat_plane_x = 0 rfeat_plane_y = 0 rfeat_plane_z = 0 rfeat_plane_off = 0
            result = getrfeatplane(rfeat_relto, rfeat_plane_x, rfeat_plane_y, rfeat_plane_z, rfeat_plane_off)
        endif
        if ((rfeat_relto <> -1) && (result <> 1))
            plane_denom = sqr(rfeat_plane_x^2 + rfeat_plane_y^2 + rfeat_plane_z^2)
            plane_min = 999.9
            plane_max = -999.9
            for cur_idx = 0 to (rfeat_pts - 1)
                dist_from_plane = (((rfeat_plane_x * points[0,cur_idx]) + (rfeat_plane_y * points[1,cur_idx]) + (rfeat_plane_z * points[2,cur_idx]) - rfeat_plane_off) / plane_denom)
                if (dist_from_plane < plane_min) plane_min = dist_from_plane
                if (dist_from_plane > plane_max) plane_max = dist_from_plane
            next cur_idx
            msg = "PROCESS POINT "+str(x_n)+" "+str(y_n)+" "+str(z_n)+" "+str(z_ht)+" "+str(max-min)+" "+str(newmax-newmin)+" "+str(plane_max - plane_min)
        else
            msg = "PROCESS POINT "+str(x_n)+" "+str(y_n)+" "+str(z_n)+" "+str(z_ht)+" "+str(max-min)+" "+str(newmax-newmin)
        endif
        if (do_disp) chart(msg)
    endif
    return ' end of PLANE_ANALYSIS
'#endregion @PLANE_ANALYSIS
'#region @LINE_ANALYSIS
LINE_ANALYSIS
    fname = get_last_scan_file()
    result = fopen(fname, reading, sequential, 3)
    if ((result = 0) && (rfeat_pts > 0))
        dim x_val[rfeat_pts] dim y_val[rfeat_pts]
        slp_err = 0
        x = 0
        y = 0
        z = 0
        ht = 0
        max = -999.9
        min = 999.9
        for cur_idx = 0 to (rfeat_pts - 1)
REM read as "probe_y, probe_z, probe_x, z_axis"
            fscanf(3, "n,n,n,n", y, z, x, ht)
            y_val[cur_idx] = x		REM deviation
            x_val[cur_idx] = ht		REM travel
            if (y_val[cur_idx] < min) min = y_val[cur_idx]
            if (y_val[cur_idx] > max) max = y_val[cur_idx]
        next cur_idx
        close #3
        dim linecoef[2]
        fitpoly(linecoef, y_val, x_val)
        slp_err = evalpoly(linecoef, x_val[rfeat_pts - 1]) - evalpoly(linecoef, x_val[0])
        msg = "PROCESS POINT "+str(slp_err)+" "+str(x_val[rfeat_pts - 1] - x_val[0])+" "+str(min)+" "+str(max)
        if (do_disp) chart(msg)
    endif
    return ' end of LINE_ANALYSIS
'#endregion @LINE_ANALYSIS
'#region @PLR_SUB
PLR_SUB
    slew_speed(min_slew)
    move("y", 0.0)
    jog_off("y")
    display_status("READY TO INSPECT PL RUNOUT")
    prompt = "ROTATE THE PART SO PROBE IS CLEAR TO MOVE INTO TOOTH GAP"
    gosub PROMPT_SUB
    display_status("FINDING FIRST TOOTH GAP")

    touch(-1.0 * neg_if_int, 0.0, 0.0, 0.0)

    gosub UNLOAD_PROBE_SUB
    read_dsp(axis, oversamp)
    c_1 = axis[3]

    touch (0.0, 0.0, 0.0, -1.0)
    read_dsp(axis, oversamp)
    cp[0] = axis[3]

    move("c", c_1)

    touch (0.0, 0.0, 0.0, 1.0)
    read_dsp(axis, oversamp)
    cp[1] = axis[3]

    if ((cp[1] - cp[0]) < 0.0) cp[1] = cp[1] + 360.0
    c_1 = (cp[0] + cp[1]) / 2.0
    if (c_1 > 360.0) c_1 = c_1 - 360.0

'    if use_q qbegin(1)
    move("c", c_1)
'    if use_q qdecel()
    move("x", x_clear)
'    if use_q qgo()
    touch(-1.0 * neg_if_int, 0.0, 0.0, 0.0)

    gosub DSP_ALL_SUB
    x_pos = axis[0]
    cp[0] = axis[3]
    cp[1] = axis[3]

    msg = "PROCESS LOCATION "+str(axis[3])

    if (do_disp) chart(msg)
    setup_pos[0] = axis[3]
    setmsg[0] = str(axis[0])+" "+str(axis[1])+" "+str(axis[2])

    c_1 = axis[3]
    c_2 = c_1
    yp[0] = axis[1]
    zp[0] = axis[2]

    move("x", x_clear)

    display_status("MEASURING PITCH LINE RUNOUT")

    gosub DSP_ALL_SUB

    msg = "PROCESS POINT "+str(probe[1])+" "+str(probe[2])+" "+str(probe[0])+" "+str(axis[0])+" "+str(axis[1])+" "+str(axis[0])+" "+str(axis[1])+" "+str(axis[2])+" "+str(axis[3])
    if (do_disp) chart(msg)

    for i = 0 to num_teeth
        if (i = int(num_teeth)) i = num_teeth REM trw 10.8 teeth
        display_status("MEASURING PL RUNOUT TOOTH " + SINT(i + 1))

        move("c", c_1 - (pitch * i) * tooth_direction)
        move("x", x_pos)

        wait(plr_delay)

        gosub DSP_ALL_SUB
        msg = "PROCESS POINT "+str(probe[1])+" "+str(probe[2])+" "+str(probe[0])+" "+str(axis[0])+" "+str(axis[1])
        if (i = 0)
            msg = msg+" "+setmsg[0]+" "+str(setup_pos[0])
        else
            msg = msg+" "+str(axis[2])
        endif
        if (do_disp) chart(msg)

        move("x", x_clear)
    next i
    return ' end of PLR_SUB
'#endregion @PLR_SUB
'#region @STD_INDEX_SUB
STD_INDEX_SUB
    for flk = (flank = 2) to (flank <> 1)
        zadj = 0
        cadj = 0
        xoff = 0

        t_err = 0.0

        nom_y = y_off[flk] * sgn(yp[flk])
        nom_angle = atn(nom_y / xp[flk]) * r_con

        for i = 0 to num_teeth
            if (i = int(num_teeth)) i = num_teeth REM trw 10.8 teeth
            m~ = str(stime() - st) + ",INSPECTING TOOTH " + SINT(i + 1) + " INDEX"
            if (i > 0 && ie_type = 1 && part_type < 1) xoff = .5 * tip_diameter * (1.0 - cos(tpai[flk] / r_con))
            display_status("MEASURING " + flk_lbl[flk] + " FLANK INDEX TOOTH " + SINT(fmod(i, num_teeth) + 1))

            if (use_q && (i = 0))
                arc_to_rad = x_clear
                arc_to_y = yb[flk]
                arc_to_z = zb[flk]
                arc_to_c = cp[flk]
                gosub ARC_TO_SUB
                qbegin(1)
            else
                if use_q qbegin(1)
                move("x", tip_clear(x_clear))
            endif

            ioff[flk, i] = 0.0

            if (ismissing(i + 1, flk + 1) = 1)
                if use_q qgo()
                t_err = 0.0
                gosub DSP_ALL_SUB
            else
                if (use_q && (i > 0)) qdecel()
REM bke 911.17 add .01 threshold to prevent follow corr calc from a missing tooth
                if ((follow = 1) && (i < num_teeth) && (abs(t_err) > .01)) ' no need to check for (i > 0) because "t_err" cannot be set until i >= 1
                    ioff[flk, i] = asin((nom_y + t_err) / insp_rad) * r_con - nom_angle
                endif
                if (npitch <> 0)
                    zadj = fmod(zinc_tooth * i, npitch)
                    if ((is_broach) && (tlead > 0)) zadj = fmod(npitch - zadj, npitch)
                    if (abs(zadj) > (0.5 * abs(npitch)))
                        if (npitch > 0.0)
                            zadj = zadj - npitch
                        else
                            zadj = zadj + npitch
                        endif
                    endif
                    cadj = zadj * ANG_PERZ(lead[flk])
                endif

                snapshot_msg = "INDEX, TOOTH, " + str(i + 1) + ", FLANK, " + str(flk)

                nominal_tooth_position = cp[flk] - (pitch * i * tooth_direction) + cadj - ioff[flk, i]      REM used to reduce confusion where this is used below

                if (find_scal)
                    idx_scan_xn = xn[flk]
                    idx_scan_yn = yn[flk]
                    idx_scan_zn = zn[flk]
                    idx_scan_xb = xb[flk]
                    idx_scan_yb = yb[flk]
                    idx_scan_zb = zb[flk] + zadj
                    idx_scan_xp = xp[flk] + (scallop_defl - target_defl) * idx_scan_xn
                    idx_scan_yp = yp[flk] + (scallop_defl - target_defl) * idx_scan_yn
                    idx_scan_zp = zp[flk] + zadj + (scallop_defl - target_defl) * idx_scan_zn
                    idx_scan_c = nominal_tooth_position

                    gosub INDEX_SCAN_SUB
                else
                    move("yzc", yb[flk], zb[flk] + zadj, nominal_tooth_position)				
                    if use_q qdecel()
                    move("x", xb[flk]) REM xb and xp are same except when is_def_prof
                    if use_q qdecel()                                                       ' this was commented out before 1404.23
                    move("xyz", xp[flk], yp[flk], zp[flk] + zadj)
                    if use_q qgo()

                    gosub DSP_ALL_SUB
                endif

                if (corr_pos_err)
                    pos_err[0] = (xp[flk] - axis[0])
                    pos_err[1] = (yp[flk] - axis[1])
                    pos_err[2] = (zp[flk] + zadj - axis[2])
                    pos_err[3] = CLEN(nominal_tooth_position, axis[3])

                    probe[0] = probe[0] - pos_err[0]
                    probe[1] = probe[1] - pos_err[1]
                    probe[2] = probe[2] - pos_err[2]
                endif

                snapshot(snapshot_msg) ' record this in the log file
				GOSUB PRINT_POS_SUB
				
                t_err = 0.0
REM use defl dot surf unit norm to calc defl along normal
                n_err = probe[0] * -xn[flk] + probe[1] * -yn[flk] + probe[2] * -zn[flk]
                if (abs(n_err) > .01)   REM bke 911.17 add .01 threshold to prevent follow corr applied to missing tooth
REM if there was at least 10 microns deflection, normalize probe deflections and convert "ioff" back into the probe deflection
                    n_err = n_err + (insp_rad * sin((nom_angle + ioff[flk, i] + pos_err[3]) / r_con) - nom_y) * -yn[flk]
                    t_err = (n_err - target_defl) / -yn[flk] REM transverse plane error

                    probe[0] = n_err * -xn[flk]
                    probe[1] = n_err * -yn[flk]
                    probe[2] = n_err * -zn[flk]
                endif
            endif

            if (i = num_teeth)
                if (abs(sqr(probe[0]^2 + probe[1]^2 + probe[2]^2) - tooth1[flk]) > tooth1_rpt_limit)
                    msg = "the part may have slipped because the first tooth repeatability exceeded " + left(str(tooth1_rpt_limit), 5) + " mm. select ok to continue or cancel to abort this test"
                    display(msg)
                endif
            endif

            msg = "PROCESS POINT "+str(probe[1])+" "+str(probe[2])+" "+str(probe[0])+" "+str(axis[0])+" "+str(axis[1])

            if (i = 0) 
                msg = msg+" "+setmsg[flk]+" "+str(setup_pos_adj[flk])   '912.10
                tooth1[flk] = sqr(probe[0]^2 + probe[1]^2 + probe[2]^2)
            else
                msg = msg+" "+str(axis[2])+" "+str(axis[3])
            endif

            if (do_disp) chart(msg)
            if (do_indx_log) index_log(msg)
        next i		
    next flk
    return ' end of STD_INDEX_SUB
'#endregion @STD_INDEX_SUB
'#region @CL_INDEX_SUB
CL_INDEX_SUB
    dim xn_ioc[2] dim yn_ioc[2] dim zn_ioc[2]
    dim x_ioc[2] dim y_ioc[2] dim z_ioc[2]
    dim nom_y_ioc[2] dim nom_ang_ioc[2]
    dim t_err[2]

    display_status("MOVING TO TEST INDEX ON CENTERLINE")
    switch (flank)
    case 3:
        angle_to_root = ?
        ref_angle = angle_to_root - tpai[0] * neg_if_int
        c_inc = cp[0] + angle_to_root
        break
    case 1:
        ref_angle = INVO(tpai[0])
        c_inc = cp[0] + (tpai[0] + ref_angle) * neg_if_int
        break
    case 2:
        ref_angle = INVO(tpai[1])
        c_inc = cp[1] - (tpai[1] + ref_angle) * neg_if_int
        break
    endswitch
REM bke 1111.23 todo: it would probably be cleaner to just transform (rotate) xp, yp, xn, yn by angle_to_root
    xn_ioc[0] = -sin((tpai[0] + ref_angle * neg_if_int) / r_con) * cos(bha[0] / r_con) * neg_if_int REM bke 1111.23 added '* neg_if_int' to ref_angle
    yn_ioc[0] = cos((tpai[0] + ref_angle * neg_if_int) / r_con) * cos(bha[0] / r_con) REM bke 1111.23 added '* neg_if_int' to ref_angle
    zn_ioc[0] = zn[0]

    xn_ioc[1] = -sin((tpai[1] + ref_angle * neg_if_int) / r_con) * cos(bha[1] / r_con) * neg_if_int REM bke 1111.23 added '* neg_if_int' to ref_angle
    yn_ioc[1] = -cos((tpai[1] + ref_angle * neg_if_int) / r_con) * cos(bha[1] / r_con) REM bke 1111.23 added '* neg_if_int' to ref_angle
    zn_ioc[1] = zn[1]

    x_ioc[0] = insp_rad * cos(ref_angle / r_con) - (probe_rad - target_defl) * xn_ioc[0]
    y_ioc[0] = insp_rad * sin(ref_angle / r_con) - (probe_rad - target_defl) * yn_ioc[0]
    z_ioc[0] = zp[0]

    x_ioc[1] = insp_rad * cos(-ref_angle / r_con) - (probe_rad - target_defl) * xn_ioc[1]
    y_ioc[1] = insp_rad * sin(-ref_angle / r_con) - (probe_rad - target_defl) * yn_ioc[1]
    z_ioc[1] = zp[1]

    nom_x = insp_rad * cos(ref_angle / r_con)
    x_clear_ioc = x_clear

    nom_y_ioc[0] = insp_rad * sin(ref_angle / r_con)
    nom_ang_ioc[0] = atn(nom_y_ioc[0] / nom_x) * r_con

    nom_y_ioc[1] = insp_rad * sin(-ref_angle / r_con)
    nom_ang_ioc[1] = atn(nom_y_ioc[1] / nom_x) * r_con

    switch (flank)
    case 1:
        xn_ioc[1] = xn_ioc[0]
        yn_ioc[1] = yn_ioc[0]
        zn_ioc[1] = zn_ioc[0]
        x_ioc[1] = x_ioc[0]
        y_ioc[1] = y_ioc[0]
        z_ioc[1] = z_ioc[0]
        break
    case 2:
        xn_ioc[0] = xn_ioc[1]
        yn_ioc[0] = yn_ioc[1]
        zn_ioc[0] = zn_ioc[1]
        x_ioc[0] = x_ioc[1]
        y_ioc[0] = y_ioc[1]
        z_ioc[0] = z_ioc[1]
        break
    endswitch

    zadj = 0.0
    cadj = 0.0
    t_err[0] = 0.0
    t_err[1] = 0.0

    prev_was_missing = 0 REM bke 812.02

    debug_file = 0
    if debug_file REM if enabled, this is currently setup to store actual defl w/o follow tooth angle added back in
        follow_file = "c:\penta\data\follow_data.dat"
        kill(follow_file)
        fopen(follow_file, writing, sequential, 1)
    endif

    for i = 0 to num_teeth + (inspect_space_tooth = 1) * (flank = 3)        
        if (i = int(num_teeth)) i = num_teeth REM trw 10.8 teeth
        m~ = str(stime() - st) + ",INSPECTING TOOTH " + SINT(i + 1) + " INDEX"
        if (i > 0) x_clear_ioc = .5 * tip_diameter + (probe_rad + safety) * neg_if_int

        if (use_q)
            if (i = 0)
                flk_opp = (flank <> 2)
                arc_to_rad = x_clear
                arc_to_y = y_ioc[flk_opp] - backoff_defl * yn_ioc[flk_opp]
                arc_to_z = z_ioc[flk_opp] - backoff_defl * zn_ioc[flk_opp]
                arc_to_c = c_inc
                gosub ARC_TO_SUB
            endif
            qbegin(1)
        endif
        if ((i > 0) || (use_q <> 1)) move("x", tip_clear(x_clear))

        for flk = (flank = 2) to (flank <> 1)
            flk_opp = (flk <> 1)
            if (flk = 0)
                if (i = (num_teeth + 1))
                    idx_tooth = -1
                else
                    idx_tooth = int(i)
                    if (npitch <> 0) zadj = fmod(zinc_tooth * i, npitch)
                endif
            else REM (flk = 1)
                idx_tooth = (int(i) - (inspect_space_tooth = 1) * (flank = 3))
                if (npitch <> 0) zadj = fmod(zinc_tooth * fmod(i + (num_teeth - 1) * (inspect_space_tooth = 1) * (flank = 3), num_teeth), npitch)
            endif

            if (idx_tooth > -1)
                if (npitch <> 0)
                    if ((is_broach) && (tlead > 0)) zadj = fmod(npitch - zadj, npitch)
                    if (abs(zadj) > (0.5 * abs(npitch)))
                        if (npitch > 0.0)
                            zadj = zadj - npitch
                        else
                            zadj = zadj + npitch
                        endif
                    endif
                    cadj = zadj * ANG_PERZ(lead[0])
                endif

                ioff[flk, idx_tooth] = 0.0

                display_status("MEASURING " + flk_lbl[flk] + " FLANK INDEX TOOTH " + SINT(fmod(idx_tooth, num_teeth) + 1))
                if (ismissing(idx_tooth + 1, flk + 1) = 1)
                    if (use_q && (flk = 0)) qgo()
                    t_err[flk] = 0.0
                    probe[0] = 0.0 probe[1] = 0.0 probe[2] = 0.0
                    prev_was_missing = 1
                else
                    if ((follow = 1) && (idx_tooth < num_teeth) && (abs(t_err[flk]) > .01)) REM 1005.17 ' no need to check for (idx_tooth > 0) because "t_err[flk]" cannot be set until idx_tooth >= 1
                        ioff[flk, idx_tooth] = asin((nom_y_ioc[flk] + t_err[flk]) / insp_rad) * r_con - nom_ang_ioc[flk]
                    endif

                    if (use_q && (flk = 0) && (i > 0)) qdecel()
                    if (use_q && (flk = 1) && (flank <> 2))
                        if ((idx_tooth = int(num_teeth)) && (inspect_space_tooth = 1))
                            qdecel()
                        else
                            qbegin(1)
                        endif
                    endif

                    nominal_tooth_position = c_inc - (pitch * i * tooth_direction) + cadj - ioff[flk, minf(idx_tooth, num_teeth)]     REM used to reduce confusion where this is used below

                    if (find_scal = 0)
                        if ((flk = 0) || (flank = 2) || ((flk = 1) && (idx_tooth = int(num_teeth))) || prev_was_missing)
							move("yzc", y_ioc[flk_opp] - backoff_defl * yn_ioc[flk_opp], z_ioc[flk_opp] - backoff_defl * zn_ioc[flk_opp] + zadj, nominal_tooth_position)
							if use_q qdecel()
						endif
                    endif

                    if (prev_was_missing)
						if(find_scal = 0)
'							if use_q qgo()
'							if use_q qbegin(1)
'							move("x", x_ioc[flk_opp] - backoff_defl * xn_ioc[flk_opp])
'							if use_q qdecel()
						endif
                        prev_was_missing = 0
                    else
						if(find_scal = 0)
							if (use_q && ((flk = 0) || (flank = 2) || (idx_tooth = int(num_teeth)))) qdecel()
						endif
                    endif

                    snapshot_msg = "INDEX, " + flk_lbl[flk] + ", TOOTH, " + SINT(fmod(idx_tooth, num_teeth) + 1)

                    if (find_scal)
                        idx_scan_xn = xn_ioc[flk]
                        idx_scan_yn = yn_ioc[flk]
                        idx_scan_zn = zn_ioc[flk]
                        idx_scan_xb = x_ioc[flk] - backoff_defl * xn_ioc[flk]
                        idx_scan_yb = y_ioc[flk] - backoff_defl * yn_ioc[flk]
                        idx_scan_zb = z_ioc[flk] + zadj - backoff_defl * zn_ioc[flk]
                        idx_scan_xp = x_ioc[flk] + (scallop_defl - target_defl) * idx_scan_xn
                        idx_scan_yp = y_ioc[flk] + (scallop_defl - target_defl) * idx_scan_yn
                        idx_scan_zp = z_ioc[flk] + zadj + (scallop_defl - target_defl) * idx_scan_zn
                        idx_scan_c = nominal_tooth_position
                        gosub INDEX_SCAN_SUB
                    else
                        move("xyz", x_ioc[flk] - backoff_defl * xn_ioc[flk], y_ioc[flk] - backoff_defl * yn_ioc[flk], z_ioc[flk] - backoff_defl * zn_ioc[flk] + zadj)
                        if use_q qdecel()                                   ' this was added 1404.23
                        move("xyz", x_ioc[flk], y_ioc[flk], z_ioc[flk] + zadj)
                        if use_q qgo()
                        gosub DSP_ALL_SUB
                    endif

                    if (corr_pos_err)
                        pos_err[0] = (x_ioc[flk] - axis[0])
                        pos_err[1] = (y_ioc[flk] - axis[1])
                        pos_err[2] = (z_ioc[flk] + zadj - axis[2])
                        pos_err[3] = CLEN(nominal_tooth_position, axis[3])

                        probe[0] = probe[0] - pos_err[0]
                        probe[1] = probe[1] - pos_err[1]
                        probe[2] = probe[2] - pos_err[2]
                    endif

                    snapshot(snapshot_msg) ' record this in the log file
                    if debug_file fprint #1, flk_lbl[flk];",";probe[0];",";probe[1];",";probe[2];",";pos_err[0];",";pos_err[1];",";pos_err[2];",";pos_err[3]
					GOSUB PRINT_POS_SUB
					
                    t_err[flk] = 0.0
REM use defl dot surf unit norm to calc defl along normal
                    n_err = probe[0] * -xn_ioc[flk] + probe[1] * -yn_ioc[flk] + probe[2] * -zn_ioc[flk]
                    if (abs(n_err) > .01) REM bke 1005.17
REM if there was at least 10 microns deflection, normalize probe deflections and convert "ioff" back into the probe deflection
                        n_err = n_err + (insp_rad * sin((nom_ang_ioc[flk] + ioff[flk, minf(idx_tooth, num_teeth)] + pos_err[3]) / r_con) - nom_y_ioc[flk]) * -yn_ioc[flk]
                        t_err[flk] = (n_err - target_defl) / -yn_ioc[flk] REM transverse plane error

                        probe[0] = n_err * -xn_ioc[flk]
                        probe[1] = n_err * -yn_ioc[flk]
                        probe[2] = n_err * -zn_ioc[flk]
                    endif
                endif

                msg = "PROCESS POINT "+str(probe[1])+" "+str(probe[2])+" "+str(probe[0])+" "+str(axis[0])+" "+str(axis[1])
                if (((flk = 0) && (i = 0)) || ((flk = 1) && (i = (inspect_space_tooth <> 0))))
                    msg = msg+" "+setmsg[flk]+" "+str(setup_pos[flk])
                    tooth1[flk] = sqr(probe[0]^2 + probe[1]^2 + probe[2]^2)
                else
                    msg = msg+" "+str(axis[2])
                endif
                msg = msg+" "+str(axis[3])
                if (do_disp) chart(msg)
                if (do_indx_log) index_log(msg)

                if (((flk = 0) && (i = num_teeth)) || ((flk = 1) && (i = (num_teeth + (inspect_space_tooth <> 0)))))
                    if (abs(sqr(probe[0]^2 + probe[1]^2 + probe[2]^2) - tooth1[flk]) > tooth1_rpt_limit)
                        msg = "the part may have slipped because the first tooth repeatability exceeded " + left(str(tooth1_rpt_limit), 5) + " mm. select ok to continue or cancel to abort this test"
                        display(msg)
                    endif
                endif
            endif
        next flk
    next i

    if debug_file close #1
    return ' end of CL_INDEX_SUB
'#endregion @CL_INDEX_SUB
'#region @INDEX_SCAN_SUB
INDEX_SCAN_SUB
    z_scallop_scan = 1.5 * feedrate
    if (z_scallop_scan = 0)
        if (facewidth > 5.0)
            z_scallop_scan = 5.0
        else
            z_scallop_scan = 0.75 * facewidth
        endif
    endif

    z_scallop_bot = idx_scan_zp - 0.55 * z_scallop_scan
    z_scallop_top = idx_scan_zp + 0.55 * z_scallop_scan

    if (scallop_scan_dir = 1)
        z_scallop_bot = z_scallop_bot - z_scallop_padding
    else
        z_scallop_top = z_scallop_top + z_scallop_padding
    endif

    if (z_scallop_bot < base_height)
        z_scallop_top = z_scallop_top + (base_height - z_scallop_bot)
        z_scallop_bot = base_height
    endif

    if (z_scallop_top > fall_off_top)
        if ((z_scallop_bot + (fall_off_top - z_scallop_top)) > base_height)
            z_scallop_bot = z_scallop_bot + (fall_off_top - z_scallop_top)
        else
            z_scallop_bot = base_height
        endif

        z_scallop_top = fall_off_top                        
    endif

    z_scallop_scan = (z_scallop_top - z_scallop_bot - z_scallop_padding) / 1.1

'   z_scallop_scan_inc = z_scallop_scan / scallop_pts * get_axis_counts("z")
    z_scallop_scan_inc = 1.0 / scallop_pts_per_mm  * get_axis_counts("z")
    z_scallop_start = 0.5 * z_scallop_scan + z_scallop_padding
    z_scallop_scan = z_scallop_scan + z_scallop_padding
    c_scallop_start = z_scallop_start * ANG_PERZ(lead[flk])
    c_scallop_length = z_scallop_scan * ANG_PERZ(lead[flk])

    move("yzc", idx_scan_yb, idx_scan_zb - z_scallop_start * scallop_scan_dir, idx_scan_c - c_scallop_start * scallop_scan_dir)
    if use_q qdecel()
    move("x", idx_scan_xb)
    if use_q qdecel()
    move("xyz", idx_scan_xp, idx_scan_yp, idx_scan_zp - z_scallop_start * scallop_scan_dir)
    if use_q qgo()

    probe[0] = -idx_scan_xn
    probe[1] = -idx_scan_yn
    probe[2] = -idx_scan_zn
    axis[0] = idx_scan_xp
    axis[1] = idx_scan_yp
    axis[2] = idx_scan_zp
    axis[3] = idx_scan_c

    pos_cor(pcorr * (flk + 1)) REM enable servo error correction
    mdir[0] = 0 mdir[1] = 0 mdir[2] = 1 mdir[3] = 0
    scan_speed(helix_speed)

    if (z_scallop_padding > 0.0)
        scan_start(z_scallop_padding * get_axis_counts("z"))
    else
        scan_start(10)
    endif

    scan_increment(z_scallop_scan_inc)
    meas_feedrate = scallop_scan(z_scallop_scan * scallop_scan_dir, c_scallop_length * scallop_scan_dir, int(z_scallop_scan * scallop_pts_per_mm), feedrate, probe, axis, snapshot_msg)

    return ' end of INDEX_SCAN_SUB
'#endregion @INDEX_SCAN_SUB
'#region @UNLOAD_PROBE_SUB
UNLOAD_PROBE_SUB
    read_dsp(probe, oversamp)
    if (abs(probe[1]) > 0.015)
        c_inc = (probe[1] * r_con) / x_pos
        inc_move("c", -c_inc)
    endif
    return ' end of UNLOAD_PROBE_SUB
'#endregion @UNLOAD_PROBE_SUB
'#region @UNLOAD_PROBE_X_SUB
UNLOAD_PROBE_X_SUB
    read_dsp(probe, oversamp)
    if (abs(probe[0]) > 0.015)
        inc_move("x", probe[0])
    endif
    return ' end of UNLOAD_PROBE_X_SUB
'#endregion @UNLOAD_PROBE_X_SUB
'#region @STEPANG_SUB
STEPANG_SUB
    m~ = str(stime() - st) + ",INSPECTING TOOTH " + SINT(inspection_thread[i]) + " STEP ANGLE"
    c_inc = edge_c + (insp_rad - edge_rad) * tan(faceangle / r_con) * ANG_PERZ(tlead) - pitch * (inspection_thread[i] - 1) * tooth_direction REM 802.12

    if (tlead > 0)
        c_inc = c_inc - .5 * edge_ta
    else
        c_inc = c_inc + .5 * edge_ta
    endif

    move("zc", sa_zb + zstart_adj, c_inc)
    move("xy", sa_xb, sa_yb)
    slew_speed(min_slew)
    move("xyz", sa_xp, sa_yp, sa_zp + zstart_adj)
    scan_start(sa_start)
    scan_increment(sa_inc)
    mdir[0] = 0 mdir[1] = 0 mdir[2] = use_z mdir[3] = use_c
    display_status("INSPECTING " + step_label + " TOOTH " + SINT(inspection_thread[i]))
    inc_scan(mdir, "scramble", "zc", sa_zlen, sa_clen)
    if (do_disp) chart("PROCESS FILE")
    slew_speed(max_slew)
REM move("xyz", sa_xb, sa_yb, sa_zb, sa_zb)
    inc_move("xyz", sa_xb - sa_xp, sa_yb - sa_yp, sa_zb - sa_zp)				
    if (tlead <> 0.0) move("x", x_clear)
    return ' end of STEPANG_SUB
'#endregion @STEPANG_SUB
'#region @TOOTH_TIP_SCAN_SUB
TOOTH_TIP_SCAN_SUB
    m~ = str(stime() - st) + ",INSPECTING TOOTH " + SINT(scan_tooth) + " TIP SCAN"
    display_status("MOVING TO INSPECT TIP SCAN")
'    if use_q qbegin(1)
    move("x", x_clear)
    c_inc = edge_c + (base_height - edge_ht + strt_ht) * ANG_PERZ(tlead) - pitch * (scan_tooth - 1) * tooth_direction     REM 802.12
'    if use_q qdecel()
    move("xyzc", ca_xb_sub, ca_yb_sub, ca_zb_sub, c_inc)
'    if use_q qgo()
    slew_speed(min_slew)
    move("xyz", ca_xp_sub, ca_yp_sub, ca_zp_sub)
    scan_increment(ca_inc)
    mdir[0] = 0 mdir[1] = 0 mdir[2] = 1 mdir[3] = 0
    display_status("INSPECTING TIP SCAN TOOTH " + SINT(scan_tooth))
    inc_scan(mdir, "scramble", "xzc", scan_len * tan(odclearangle / r_con), scan_len, scan_len * ANG_PERZ(tlead))
    if (do_disp) chart("PROCESS FILE")
    slew_speed(max_slew)
    inc_move("xyz", ca_xb_sub - ca_xp_sub, ca_yb_sub - ca_yp_sub, ca_zb_sub - ca_zp_sub)
    return ' end of TOOTH_TIP_SCAN_SUB
'#endregion @TOOTH_TIP_SCAN_SUB
'#region @DEF_PROFILE_SETUP
DEF_PROFILE_SETUP
    if (find_gap)
		find_tooth_gap(x_pos - backoff_defl * xnorm, num_teeth) REM bke 911.09
	else
		jog_off("y")
		prompt = "ROTATE PART TO ALIGN PROBE WITH THE PROFILE CENTER"
		gosub PROMPT_SUB
	endif
	jog_on("xyzct")
	display_status("ALIGNING PROFILE")
	find_profile(ie_type)
    return ' end of DEF_PROFILE_SETUP
'#endregion @DEF_PROFILE_SETUP
'#region @SS_PS_SETUP
SS_PS_SETUP   REM  jjm 812.10  Corrected Name  
    display_status("FINDING STRAIGHT SIDED SPLINE SPACE LOCATION")
    read_dsp(axis, oversamp)
    temp_c2 = axis[3]

    if (ps_spline_off <> 0.0)
        move("y", ps_spline_off - (probe_rad - target_defl) * yn[1])
    endif

    touch(0.0, 0.0, 0.0, -360.0 / num_teeth * sgn(yn[0]))
    read_dsp(axis, oversamp)
    temp_c1 = axis[3]
    move("c", temp_c2)

    if (ps_spline_off <> 0.0)
        move("y", ps_spline_off - (probe_rad - target_defl) * yn[0])
    endif

    touch(0.0, 0.0, 0.0, -360.0 / num_teeth * sgn(yn[1]))  REM  jjm 812.10 increase rotary for touch 
    read_dsp(axis, oversamp)
    temp_c2 = axis[3]
    space_center = fmod(360.0 + temp_c1 + .5 * CLEN(temp_c2, temp_c1), 360.0) REM bke 802.22
    z_at_init = axis[2]

    temp_cdif = abs(CLEN(temp_c2, temp_c1)) REM bke 802.22

    while (temp_cdif > (1.1 * pitch))
	    space_center = fmod(360.0 + space_center - .5 * pitch, 360.0)
	    temp_cdif = temp_cdif - .5 * pitch
    wend

    if (is_ss_spline)
	    v_tpa = tpai[ynorm < 0] - temp_cdif * .5 * neg_if_int
    else
	    v_tpa = tpai[ynorm < 0]
    endif
    y_off[0] = insp_rad * sin(v_tpa / r_con)

'   x_pos = insp_rad * cos(v_tpa / r_con)
    x_pos = sqr(insp_rad^2 - y_off[0]^2)
    if (is_ss_spline)
	    if (ie_type = 1)	REM external gear
		    y_pos = -y_off[0] - probe_rad * abs(ynorm)
	    else			REM internal gear
		    y_pos = y_off[0] - probe_rad * abs(ynorm)
	    endif
    else
        if (inspect_space_tooth = 0)
            y_pos = y_off[0] - probe_rad * ynorm + ps_spline_off
        else
            y_pos = -y_off[0] - probe_rad * ynorm + ps_spline_off
        endif
    endif

REM before moving clear, rotate to center of space to avoid possible shank / over-deflect during the retract
    move("c", space_center)
REM use the more "extreme" x_posistion tip_clear(x_clear, y_pos) for internals, and just x_clear for externals
    if (ie_type = 1)    REM external
        move("x", x_clear)                      REM use maximum clearance - we should be clear at tip_clear(x_clear, 0.0) which equals x_clear
    else                REM internal
        move("x", tip_clear(x_clear, y_pos))    REM use minimum clearance - x_clear is not small enough when y_pos is 0.0 so we must use tip_clear()
    endif

    if ((is_ps_spline) && (inspect_space_tooth = 1)) space_center = fmod(360.0 + space_center - (180 / num_teeth), 360.0)

REM 901.27tlt	    if (is_ps_spline) ynorm = ynorm * nParAdj

    if (is_ss_spline)
	    move("yc", y_pos - ynorm * backoff_defl, space_center - tpai[ynorm < 0] * neg_if_int)
    else
	    move("yc", y_pos - ynorm * backoff_defl, space_center)
    endif
    move("x", x_pos)

    gosub FLANK_SETUP_SUB
    gosub DSP_POS_SUB

    y_off[0] = abs(position[1] + probe_rad * abs(ynorm)) REM this needs to use position[1]
    v_tpa = asin(y_off[0] / insp_rad) * r_con
    tpai_x2 = 2.0 * v_tpa
    x_pos = insp_rad * cos(v_tpa / r_con)
    x_pro_st = sap_ang[0] * .5 * cos(v_tpa / r_con) REM sap_ang[0] is a diameter when straight_sided
    x_pro_len = tip_diameter * .5 * cos(v_tpa / r_con) - x_pro_st
    xp[0] = x_pos
    xp[1] = xp[0]
    xb[0] = xp[0]
    xb[1] = xp[0]
REM bke 911.17 y_off vars should not include probe or defl offsets, this causes a problem in the index subs because they assume y_off is a nom pos
'   read_dsp(axis, oversamp)
'   y_off[0] = abs(axis[1] - ps_spline_off) REM this needs to use axis[1]
    y_off[0] = y_off[0] - ps_spline_off
    y_off[1] = y_off[0]

    if (inspect_space_tooth = 0)
REM bke 911.17 add probe offsets here instead
        yp[0] = y_off[0] + ps_spline_off - probe_rad * yn[0]
        yp[1] = -y_off[0] + ps_spline_off - probe_rad * yn[1]
    else
        yp[0] = -y_off[0] + ps_spline_off - probe_rad * yn[0]
        yp[1] = y_off[0] + ps_spline_off - probe_rad * yn[1]
    endif

REM bke 912.07 move("xy", x_pos, yp[0])	
    move("xy", axis[0], axis[1])
    return ' end of SS_PS_SETUP
'#endregion @SS_PS_SETUP
'#region @HELIX_SCAN_SUB
HELIX_SCAN_SUB
    m~ = str(stime() - st) + ",INSPECTING TOOTH " + SINT(scan_tooth) + " " + scan_label    
    temp_lead_current = 0.0

    for lc = 0 to nlocs - 1
        scan_speed(scan_spd)		
        scan_start(scan_st * get_axis_counts("z"))
        scan_increment(scan_inc * get_axis_counts("z"))
        xloc_adj = 0
        cloc_adj = 0
        if (((nlocs > 1) && (is_def_prof = 0)))
            if (straight_sided = 0)
                if (lead_loc[lc] > 0)
                    yloc_adj = (lead_loc[lc] * pi * bcr[flk] / 180.0 - y_off[flk]) * sgn(yp[flk])
                else
                    yloc_adj = (sqr((.5 * tip_diameter)^2 - bcr[flk]^2) + lead_loc[lc] * pi * bcr[flk] / 180.0 - y_off[flk]) * sgn(yp[flk])
                endif

                if (do_tests_cl)
                    cloc_adj = lead_loc[lc] - y_off[flk] * 180.0 / (pi * bcr[flk])
                    for j = 0 to 1
                        switch (cl_ref_type)
                        case 0:
                            rot = tan(tpai[flk] / r_con) + cloc_adj * j / r_con
                            break
                        case 1:
                            rot = tpai[flk] / r_con + cloc_adj * j / r_con
                            break
                        endswitch

                        pa = atn(rot)
                        inv = rot - pa
                        rad = bcr[flk] / cos(pa)
                        if (j = 0)
                            xloc_adj = rad * cos(inv)
                            yloc_adj = rad * sin(inv)
                        else
                            xloc_adj = rad * cos(inv) - xloc_adj
                            yloc_adj = (rad * sin(inv) - yloc_adj) * sgn(yp[flk]) REM (-1 + 2 * flk) * neg_if_int
                            cloc_adj = 0.0
                        endif
                    next j							
                else
                    cloc_adj = yloc_adj / (pi * bcr[flk]) * 180
                endif
            else
                xloc_adj = 0.5 * (lead_loc[lc] - inspection_diameter)
            endif
        else REM bke 1204.10 needed for surf check off insp dia
            cloc_adj = yloc_adj / (pi * bcr[flk]) * 180
        endif

        display_status("MOVING TO INSPECT " + flk_lbl[flk] + " FLANK " + scan_label)
        c_inc = cp[flk] - pitch * (scan_tooth - 1) * tooth_direction + z_move * ANG_PERZ(lead[flk]) - ioff[flk, scan_tooth - 1] REM 802.12 bke 904.15

        if (use_q) REM && (surf_meas = 0))   && (i = 0) && (lc = 0))
	        if (arbor_jump)
                move("x", tip_clear(x_clear))
                move("z", fall_off_top)
		        arc_to_z = fall_off_top
	        else
		        arc_to_z = zb[flk] + z_move
	        endif
		    arc_to_rad = x_clear
		    arc_to_y = yb[flk] + yloc_adj
		    arc_to_c = c_inc + cloc_adj
		    gosub ARC_TO_SUB
            if (arbor_jump) move("z", zb[flk] + z_move)
		    qbegin(1)
        else
            if (tip_clear(x_clear) > tip_clear(x_clear, yb[flk] + yloc_adj))
                move("x", tip_clear(x_clear))
            else
                move("x", tip_clear(x_clear, yb[flk] + yloc_adj)) REM 1212.13
            endif
            if (arbor_jump)
                move("z", zb[flk] + z_move)
                move("yc", yb[flk] + yloc_adj, c_inc + cloc_adj)
            else
                move("yzc", yb[flk] + yloc_adj, zb[flk] + z_move, c_inc + cloc_adj)
            endif
        endif

        if (is_def_prof)
            move("x", xb[flk])	REM this is backoff for defined profile
        else
            move("x", xp[flk] + xloc_adj)	REM this is bcr for gear, next move redundant on purpose
        endif

        if (touch_mode = 1)
            touch_mode = 0
            if use_q qgo()
            touch(xn[flk], yn[flk], zn[flk], 0.0)
            setdefl(target_defl, xn[flk], yn[flk], zn[flk])
        else
            if use_q qdecel()
            move("xyz", xp[flk] + xloc_adj, yp[flk] + yloc_adj, zp[flk] + z_move)
            if use_q qgo()
        endif

        if (nlocs > 1)
            display_status("INSPECTING " + flk_lbl[flk] + " FLANK " + scan_label + " TOOTH " + SINT(scan_tooth) + ", LOCATION " + SINT(lc + 1))
        else
            display_status("INSPECTING " + flk_lbl[flk] + " FLANK " + scan_label + " TOOTH " + SINT(scan_tooth))
        endif
        if (do_tests_cl) convert2bt((flk + 1) * neg_if_int)

        if (all_data_columns = 1)
            mdir[0] = 0.0 mdir[1] = 0.0 mdir[2] = 1.0 mdir[3] = 0.0
            inc_scan(mdir, "scramble", "xyzc", 0.0, 0.0, z_scan, z_scan * ANG_PERZ(lead[flk]))
            all_data_columns = 0
        else
            pos_cor(pcorr * (flk + 1)) REM enable servo error correction
            if(is_shaver)
                inc_scan("scramble", "zc_z", z_scan, z_scan * ANG_PERZ(lead[flk]), base_height - (probe_rad - target_defl) * zn[flk], facewidth)
            else
                inc_scan("scramble", "zc", z_scan, z_scan * ANG_PERZ(lead[flk]))
            endif
        endif
        if (do_disp) chart("PROCESS FILE")
        temp_lead_current = temp_lead_current + get_lead_current()
    next lc
    temp_lead_current = temp_lead_current / nlocs
    return ' end of HELIX_SCAN_SUB
'#endregion @HELIX_SCAN_SUB
'#region @PROFILE_SCAN_SUB
PROFILE_SCAN_SUB
' c_scan must be properly set before this call
' helix_y_per_z must be set before this call and will always be zero unless performing feed scallop profile along measured helix
    m~ = str(stime() - st) + ",INSPECTING TOOTH " + SINT(scan_tooth) + " " + scan_label    
    y_scan = c_scan * bcr[flk] / r_con    
    c_insp = y_off[flk] * r_con / bcr[flk]
    mdir[0] = 0.0 mdir[1] = 0.0 mdir[2] = 0.0 mdir[3] = 1.0 

    if (straight_sided)
        y_sap = y_off[flk]
    else
        y_sap = c_sap * bcr[flk] / r_con
    endif

    idir = -1
    if (flk = 1) idir = 1

    yp_inv = yp[flk] + (y_sap - y_off[flk]) * neg_if_int * idir ' y_sap & y_off are unsigned, y locations without probe radius or deflection adjustment
    yb_inv = yb[flk] + (y_sap - y_off[flk]) * neg_if_int * idir
    cp_inv = cp[flk] + (c_sap - c_insp) * neg_if_int * idir

    yrake_adj = 0.0
    zrake_adj = 0.0

    for lc = 0 to nlocs - 1
        scan_speed(scan_spd)

        if (straight_sided)
            scan_start(scan_st * get_axis_counts("x"))
            scan_increment(scan_inc * get_axis_counts("x"))
        else
            scan_start(scan_st * get_axis_counts("c"))
            scan_increment(scan_inc * get_axis_counts("c"))
        endif

        switch (part_type)
        case 0: REM gear - scallop adjustment
            zrake_adj = (y_sap - y_off[flk]) * tan(bha[flk] / r_con) * normal_path ' normal_path: 1 = normal, -1 = inverted normal, 0 = transverse
            yrake_adj = helix_y_per_z * zrake_adj * neg_if_int * idir
            break
        case 6: REM broach - rake adjustment
            sap_diam = get_pro_start_diam(flk)
REM bke/jjm 810.01      zrake_adj = get_broach_z_adj(sap_diam, tip_diameter, thread_lead)
            zrake_adj = get_broach_z_adj(sap_diam, inspection_diameter, thread_lead) REM bke/jjm 810.01
            break
        endswitch

        crake_adj = zrake_adj * ANG_PERZ(lead[flk])

        if (nlocs > 1)
            if (prof_loc[lc] > 0)
                zloc_adj = prof_loc[lc] + base_height - (zp[flk] + (probe_rad - target_defl) * zn[flk])
            else
                zloc_adj = prof_loc[lc] + fall_off_top - (zp[flk] + (probe_rad - target_defl) * zn[flk])
            endif
        endif

        zstart_adj = 0
        if (npitch <> 0)    REM - shavers, hobscal, etc.
            zloc_adj = zloc_adj - fmod(zloc_adj, npitch)
            zstart_adj = fmod_min(zinc_tooth * (scan_tooth - 1), npitch)REM  712.13 802.12 905.28
            if ((is_broach) && (tlead > 0.0)) zstart_adj = fmod(npitch - zstart_adj, npitch)
        endif

        cloc_adj = zloc_adj * ANG_PERZ(lead[flk])
        coff = zstart_adj * ANG_PERZ(lead[flk])

        if (straight_sided)
            c_inc = cp[flk] - pitch * (scan_tooth - 1) * tooth_direction - ioff[flk, scan_tooth - 1] REM bke 904.15
            tmp_pos = x_pro_st - scan_st * neg_if_int
        else
            c_inc = cp_inv - pitch * (scan_tooth - 1) * tooth_direction - ioff[flk, scan_tooth - 1] REM bke 904.15
            tmp_pos = xp[flk]
        endif

        if (xy_invo = 0) REM std invo
            display_status("MOVING TO INSPECT " + flk_lbl[flk] + " FLANK " + scan_label)

            if (use_q) REM && (surf_meas = 0) && (i = 0) && (lc = 0))
                if (arbor_jump)
                    move("x", tip_clear(x_clear))
                    move("z", fall_off_top)
                    arc_to_z = fall_off_top
                else
                    arc_to_z = zb[flk] + zstart_adj + zloc_adj + zrake_adj
                endif
                arc_to_rad = x_clear
                arc_to_y = yb_inv + yrake_adj
                arc_to_c = c_inc + coff + cloc_adj + crake_adj
                gosub ARC_TO_SUB
                if (arbor_jump) move("z", zb[flk] + zstart_adj + zloc_adj + zrake_adj)
		        qbegin(1)
            else
                if (tip_clear(x_clear) > tip_clear(x_clear, yb_inv))
                    move("x", tip_clear(x_clear))
                else
                    move("x", tip_clear(x_clear, yb_inv)) REM 1212.13
                endif
                if (arbor_jump)
                    move("z", fall_off_top) 
                    move("yc", yb_inv + yrake_adj, c_inc + coff + cloc_adj + crake_adj)
                    move("z", zb[flk] + zstart_adj + zloc_adj + zrake_adj) 
                else 
                    move("yzc", yb_inv + yrake_adj, zb[flk] + zstart_adj + zloc_adj + zrake_adj, c_inc + coff + cloc_adj + crake_adj)
                endif
            endif

            move("x", tmp_pos)

            if (touch_mode = 1)
                touch_mode = 0
                if use_q qgo()
                touch(xn[flk], yn[flk], zn[flk], 0.0)
                setdefl(target_defl, xn[flk], yn[flk], zn[flk])
            else
                if use_q qdecel()
                move("yz", yp_inv + yrake_adj, zp[flk] + zstart_adj + zloc_adj + zrake_adj)
                if use_q qgo()
            endif
        endif

        if (nlocs > 1)
            display_status("INSPECTING " + flk_lbl[flk] + " FLANK "+ scan_label +" TOOTH " + SINT(scan_tooth) + ", LOCATION " + SINT(lc + 1))
        else
            display_status("INSPECTING " + flk_lbl[flk] + " FLANK "+ scan_label +" TOOTH " + SINT(scan_tooth))
        endif

        if xy_invo
            convert2bt((flk + 1) * neg_if_int)
            pos_cor(pcorr * (flk + 1)) REM enable servo error correction
            xy_involute(scan_tooth, flk, x_clear, zstart_adj + zloc_adj) REM this currently does not support rake follow
        else
            if (straight_sided)
                if (is_broach)
                    m~ = str(stime() - st) + ",measure_broach_profile(" + str(tmp_pos) + ", " + str(yp_inv) + ", " + str(y_scan * neg_if_int * idir) + ", " + str(c_scan * neg_if_int * idir) + ", " + SINT(INT(abs(c_scan) / inv_inc)) + ", " + str(probe_rad) + ", " + str(target_defl) + ")"
                    measure_broach_profile(tmp_pos, yp_inv, x_pro_len, 0, INT(x_pro_len / inv_inc), probe_rad, target_defl)
                else
                    inc_scan("scramble", "x", x_pro_len)
                endif
            else
                pos_cor(pcorr * (flk + 1)) REM enable servo error correction
                if (is_broach)
                    m~ = str(stime() - st) + ",measure_broach_profile(" + str(tmp_pos) + ", " + str(yp_inv) + ", " + str(y_scan * neg_if_int * idir) + ", " + str(c_scan * neg_if_int * idir) + ", " + SINT(INT(abs(c_scan) / inv_inc)) + ", " + str(probe_rad) + ", " + str(target_defl) + ")"
                    measure_broach_profile(tmp_pos, yp_inv, (y_scan * neg_if_int * idir), (c_scan * neg_if_int * idir), INT(abs(c_scan) / inv_inc), probe_rad, target_defl)
                else
                    zrake_adj = y_scan * tan(bha[flk] / r_con) * normal_path ' normal_path: 1 = normal, -1 = inverted normal, 0 = transverse
                    crake_adj = zrake_adj * ANG_PERZ(lead[flk])
                    yrake_adj = helix_y_per_z * zrake_adj
                    pos_cor(pcorr * (flk + 1), (y_scan + yrake_adj) / c_scan * r_con) ' use 2nd arg as a temp bcr override if performing act helix comp feed scallop

                    if (all_data_columns = 1)
                        pos_cor(0) REM disable servo error correction in this mode
                        inc_scan(mdir, "scramble", "xyzc", 0.0, y_scan * neg_if_int * idir + yrake_adj, zrake_adj, c_scan * neg_if_int * idir + crake_adj)
                        all_data_columns = 0
                    else
                        inc_scan(mdir, "scramble-yc", "yzc", (y_scan  + yrake_adj) * neg_if_int * idir, zrake_adj, c_scan * neg_if_int * idir + crake_adj)
                    endif
                endif
            endif
        endif

        if (do_disp) chart("PROCESS FILE")
    next lc
    return ' end of PROFILE_SCAN_SUB
'#endregion @PROFILE_SCAN_SUB
'#region @PROBE_CHANGE_SUB
PROBE_CHANGE_SUB
    probe_diameter = change_rprobe_id(cur_probe_tip)
    probe_rad = .5 * probe_diameter
    probe_id = get_probe_name()
    return ' PROBE_CHANGE_SUB
'#endregion @PROBE_CHANGE_SUB
'#region @SHAP_RIGHT_EDGE_SUB
SHAP_RIGHT_EDGE_SUB
' find_edge returns transverse z height of the edge.
    shap_edge_z[ynorm < 0] = find_edge("up", lead[ynorm < 0], facewidth, bha[ynorm < 0])
' now call the top edge find sub to re-locate the edge exactly
    gosub SHAP_TOP_EDGE_FIND
'shap_edge_c and shap_edge_z are now set to the cutting edge location

' adjust zp for the measured edge of the part
    if (is_broach)
'the following is slightly flawed by not taking in to account the clearance angle (which we don't input)
        zadj = (shap_edge_z[1] + z_off + target_defl * zn[1]) - zp[1] REM bke/jjm 810.01
        zp[1] = zp[1] + zadj
        zb[1] = zb[1] + zadj
        cp[1] = fmod(cp[1] + 360.0 + zadj * ANG_PERZ(lead[1]), 360.0)
    else
'helical shapers - we want base_height and fall_off_top to be the lower of the two edges
'        if (shap_edge_z[1] > shap_edge_z[0])  'right flank is higher, shift right zp and zb down
'            zp[1] = zp[1] + shap_edge_z[0] - shap_edge_z[1]
'            zb[1] = zb[1] + shap_edge_z[0] - shap_edge_z[1]
'            cp[1] = fmod(cp[1] + 360.0 + (shap_edge_z[0] - shap_edge_z[1]) * ANG_PERZ(lead[1]), 360.0)            
'        else 'left was higher, shift left zp and zb down
'            zp[0] = zp[0] + shap_edge_z[1] - shap_edge_z[0]
'            zb[0] = zb[0] + shap_edge_z[1] - shap_edge_z[0]
'            cp[0] = fmod(cp[0] + 360.0 + (shap_edge_z[1] - shap_edge_z[0]) * ANG_PERZ(lead[0]), 360.0)
        if (shap_edge_z[1] < shap_edge_z[0])  'right flank is lower
            fall_off_top = shap_edge_z[1]
            base_height = fall_off_top - facewidth
        endif
        setup_pos[1] = fmod(360.0 + shap_edge_c[1] + (shap_edge_z[1] - shap_edge_z[0]) * ANG_PERZ(tlead), 360.0)
    endif
    return ' end of SHAP_RIGHT_EDGE_SUB
'#endregion @SHAP_RIGHT_EDGE_SUB
'#region @SHAP_OD_MEASURE_SUB
SHAP_OD_MEASURE_SUB
    edge_ta = tpai_x2 - fmod(shap_edge_c[1] - shap_edge_c[0] + 360.0, 360.0) REM bke 809.04
    edge_ta = tpai_x2 - fmod(setup_pos[1] - setup_pos[0] + 360.0, 360.0) 'tlt 912.12 setup_pos' are transverse
    tmp_pos = axis[0]
'912.12                    c2_adj = axis[3] + (zp1_ideal - axis[2]) * ANG_PERZ(lead[1])
    c2_adj = axis[3] + (zp[1] - axis[2]) * ANG_PERZ(lead[1])    '912.12
    cdist = fmod(c2_adj - cp[0] + 360.0, 360.0)
    prev_tip_diam = tip_diameter REM MPW 712.03 so sap value is relative to entered tip not actual tip (so roll angle positions REMain correct in display)
    if (is_shaper || is_broach)
        inc_move("xyz", x_clear - xp[1], -backoff_defl * ynorm, -backoff_defl * znorm)
        move("yzc", 0.0, inspection_height - probe_rad * sin(odclearangle / r_con), cp[0] + .5 * cdist)
        touch(-cos(odclearangle / r_con), 0.0, sin(odclearangle / r_con), sin(odclearangle / r_con) * ANG_PERZ(tlead))
        gosub DSP_POS_SUB
        z_inc = inspection_height - (probe_rad + backoff_defl) * sin(odclearangle / r_con) - position[2]
        if (is_broach)
            inc_move("zc", z_inc, CLEN(cp[0] + .5 * cdist, position[3]))
        else
            inc_move("xzc", -z_inc / tan(odclearangle / r_con), z_inc, CLEN(cp[0] + .5 * cdist, position[3]))
        endif
        gosub UNLOAD_PROBE_X_SUB
'        touch(-cos(odclearangle / r_con), 0.0, sin(odclearangle / r_con), 0.0)
        touch(-cos(odclearangle / r_con), 0.0, sin(odclearangle / r_con), sin(odclearangle / r_con) * ANG_PERZ(tlead))

REM this is at insp_ht	tip_diameter = (position[0] - probe_rad * cos(odclearangle / r_con)) * 2.0

        setdefl(target_defl, -cos(odclearangle / r_con), 0.0, sin(odclearangle / r_con))
        equal(rotate, "PCS") REM reset to pcs
        rotatey(rotate, odclearangle / r_con)
        edge_ht = find_edge(rotate, "up", tlead * cos(odclearangle / r_con), facewidth, 0.0) REM pass bha = 0.0 since on tip and using modified csys
        gosub DSP_POS_SUB
        transform(rotate, position[0], position[1], position[2], position[3]) REM bke 907.24
        inc_move("x", backoff_defl)
        inc_move(rotate, "zc", edge_ht - position[2], (edge_ht - position[2]) * ANG_PERZ(tlead))
        inc_move("x", -backoff_defl)
        gosub DSP_POS_SUB
REM     edge_rad = position[0] - probe_rad * cos(odclearangle / r_con)
REM     edge_ht = position[2] + probe_rad * sin(odclearangle / r_con)
        edge_rad = position[0] - probe_rad
        edge_ht = position[2]

        edge_c = position[3]
REM     edge_ta = tpai_x2 - fmod(shap_edge_c[1] - shap_edge_c[0] + 360.0, 360.0)    bke 809.04
        tip_diameter = 2.0 * edge_rad
    endif

    move("x", x_clear)
    msg = "PROCESS SIZE " + str(tip_diameter)
    if (do_disp) chart(msg)
    return ' end of SHAP_OD_MEASURE_SUB
'#endregion @SHAP_OD_MEASURE_SUB
'#region @XFORM_BT_2_CL
XFORM_BT_2_CL
    if (bt_cl = 0)
        bt_cl = 1

        switch (cl_ref_type)
        case 0:
            cp[0] = fmod(360.0 + cp[0] + tan(tpai[0] / r_con) * r_con * neg_if_int, 360.0) 
            cp[1] = fmod(360.0 + cp[1] - tan(tpai[1] / r_con) * r_con * neg_if_int, 360.0)
            break
        case 1:
            cp[0] = fmod(360.0 + cp[0] + tpai[0] * neg_if_int, 360.0)
            cp[1] = fmod(360.0 + cp[1] - tpai[1] * neg_if_int, 360.0)
            break
        endswitch

        equal(rotate, mcs) REM reset to mcs

        switch (cl_ref_type)
        case 0:
            rotatez(rotate, -tan(tpai[0] / r_con) * neg_if_int) REM rotate csys opp dir to move setup points and norms
            break
        case 1:
            rotatez(rotate, -tpai[0] / r_con * neg_if_int) REM rotate csys opp dir to move setup points and norms
            break
        endswitch

        transform(rotate, xn[0], yn[0])
        transform(rotate, xp[0], yp[0])

        equal(rotate, mcs) REM reset to mcs	

        switch (cl_ref_type)
        case 0:
            rotatez(rotate, tan(tpai[1] / r_con) * neg_if_int) REM rotate csys opp dir to move setup points and norms
            break
        case 1:
            rotatez(rotate, tpai[1] / r_con * neg_if_int) REM rotate csys opp dir to move setup points and norms
            break
        endswitch

        transform(rotate, xn[1], yn[1]) 
        transform(rotate, xp[1], yp[1])
    endif
    return ' end of XFORM_BT_2_CL
'#endregion @XFORM_BT_2_CL
'#region @XFORM_CL_2_BT
XFORM_CL_2_BT
    if (bt_cl = 1)
        bt_cl = 0
        switch (cl_ref_type)
        case 0:
            cp[0] = fmod(360.0 + cp[0] - tan(tpai[0] / r_con) * r_con * neg_if_int, 360.0)
            cp[1] = fmod(360.0 + cp[1] + tan(tpai[1] / r_con) * r_con * neg_if_int, 360.0)
            break
        case 1:
            cp[0] = fmod(360.0 + cp[0] - tpai[0] * neg_if_int, 360.0)
            cp[1] = fmod(360.0 + cp[1] + tpai[1] * neg_if_int, 360.0)
            break
        endswitch

        equal(rotate, mcs) REM reset to mcs
        switch (cl_ref_type)
        case 0:
            rotatez(rotate, tan(tpai[0] / r_con) * neg_if_int) REM rotate csys opp dir to move setup points and norms
            break
        case 1:
            rotatez(rotate, tpai[0] / r_con * neg_if_int) REM rotate csys opp dir to move setup points and norms
            break
        endswitch

        transform(rotate, xn[0], yn[0]) 
        transform(rotate, xp[0], yp[0])

        equal(rotate, mcs) REM reset to mcs	
        switch (cl_ref_type)
        case 0:
            rotatez(rotate, -tan(tpai[1] / r_con) * neg_if_int) REM rotate csys opp dir to move setup points and norms
            break
        case 1:
            rotatez(rotate, -tpai[1] / r_con * neg_if_int) REM rotate csys opp dir to move setup points and norms
            break
        endswitch

        transform(rotate, xn[1], yn[1]) 
        transform(rotate, xp[1], yp[1])
    endif
    return ' end of XFORM_CL_2_BT
'#endregion @XFORM_CL_2_BT
'#region @ARC_TO_SUB
ARC_TO_SUB
    arc_move(arc_to_rad, arc_to_y, arc_to_z, arc_to_c)
    return ' end of ARC_TO_SUB
'#endregion @ARC_TO_SUB

DEFFN INVO(tpa_deg) = (tan(tpa_deg / r_con) - tpa_deg / r_con) * r_con
DEFFN ANG_PERZ(ld) = -360.0 / (ld + (ld = 0.0)) * (ld <> 0)
DEFFN CLEN(ce, cs) = (ce - cs) - 360.0 * sgn(ce - cs) * (abs(ce - cs) > 180.0)
DEFFN ZBOT_REF(zvar) = zvar + facewidth * (zvar < 0.0)
DEFFN SINT(n2s) = left(str(n2s), 1 + log10(n2s)) REM bke 804.14
'#region @SETUP_FILE_SUB
SETUP_FILE_SUB
	filename = "c:\penta\data\indexdat.csv"
'	filename = filename + date + "-"
'	tstr = time
'	filename = filename + left(tstr, 2) + mid(tstr, 4, 2) + ".csv"
	kill(filename)
	file_err = 1
	file_err = fopen(filename, writing, sequential, 2)

	if(file_err <> 0)
		display("CANNOT OPEN " + filename)
		end
	endif
	return
'#endregion @SETUP_FILE_SUB
'#region @PRINT_POS_SUB	
PRINT_POS_SUB
	GOSUB DSP_POS_SUB
	fprint #2, flk; ","; i + 1; ","; position[0]; ","; position[1]; ","; position[2]; ","; position[3]; chr(13); chr(10)
	return
'#endregion @PRINT_POS_SUB

REM std_gear.bas end (keep null '00' at end of binary file).
