print "proximity sensor mapper psc1215 "
rem readings are taken at all (xpt,ypt,zpt) points, not at midpoints
rem 2-8-08
dim ref[3]
get_probe_ref[ref]	rem fetch the probe offsets for the active probe tip.
dim xpt[20]
dim ypt[20]
dim zpt[20]
dim point[4]
manual=0
xmax=23.229 - ref[0]
ymax=105.232 - ref[1]
zmax=134.118 - ref[2]
xmin=-245.6 - ref[0]
ymin=-264.917 - ref[1]
zmin=-325.586 - ref[2]
xdiv=20
ydiv=8
zdiv=10
rem mapping points in normalized coords. range = 0 to 255:
xpt[0]=0
xpt[1]=17
xpt[2]=34
xpt[3]=51
xpt[4]=68
xpt[5]=85
xpt[6]=102
xpt[7]=119
xpt[8]=136
xpt[9]=153
xpt[10]=170
xpt[11]=187
xpt[12]=204
xpt[13]=221
xpt[14]=227
xpt[15]=233
xpt[16]=239
xpt[17]=245
xpt[18]=250
xpt[19]=255
ypt[0]=0
ypt[1]=65
ypt[2]=130
ypt[3]=195
ypt[4]=210
ypt[5]=225
ypt[6]=240
ypt[7]=255
zpt[0]=0
zpt[1]=10
zpt[2]=20
zpt[3]=30
zpt[4]=40
zpt[5]=50
zpt[6]=60
zpt[7]=116
zpt[8]=182
zpt[9]=255
cmd_control("PROXENB=0") rem prox detection off
cmd_control("CMDMODE=1") rem command mode on
gosub zroff
gosub prxon
gosub clrprx
rem set filters to hi speed before creating map:
gosub cwait
rem decimation size = 501:
cmd_control("FLD1=5") rem set decimation size
cmd_control("FLD2=1") rem high byte
cmd_control("FLD3=245") rem lo byte
cnbr=8
rem gosub excmd
wait(1)
gosub cwait
rem taps = 501:
cmd_control("FLD1=0") rem set taps and go:
cmd_control("FLD2=1") rem high byte
cmd_control("FLD3=245") rem lo byte
cnbr=8
rem gosub excmd
wait(1)
gosub imap
print "map size: ",mapsize
rem set filters to hi resolution for mapping:
gosub cwait
rem decimation size = 8001:
cmd_control("FLD1=5") rem set decimation size
cmd_control("FLD2=31") rem high byte
cmd_control("FLD3=65") rem lo byte
cnbr=8
rem gosub excmd
wait(1)
gosub cwait
rem taps = 8001:
cmd_control("FLD1=0") rem set taps and go:
cmd_control("FLD2=31") rem high byte
cmd_control("FLD3=65") rem lo byte
cnbr=8
rem gosub excmd
wait(1)
rem set max and min map limits in Galil:
if(manual=1)
 print "move x to its maximum limit"
 read_axis(point)
 xmax=point[0]
else
 move("x",xmax)
 gosub inpsn
endif
cmd_control("MXMN[1]=_TPX")
rem save xmax point:
gosub cwait
cmd_control("FLD3=0") rem select x axis
cmd_control("FLD2=1") rem max
cmd_control("FLD1=MXMN[1]")
cnbr=24
gosub excmd
if(manual=1)
 print "move y to its maximum limit"
 read_axis(point)
 ymax=point[1]
else
 move("y",ymax)
 gosub inpsn
endif
cmd_control("MXMN[3]=_TPY")
rem save ymax point
gosub cwait
cmd_control("FLD3=1") rem select y axis
cmd_control("FLD2=1") rem max
cmd_control("FLD1=MXMN[3]")
cnbr=24
gosub excmd
if(manual=1)
 print "move z to its maximum limit"
 read_axis(point)
 zmax=point[2]
else
 move("z",zmax)
 gosub inpsn
endif
cmd_control("MXMN[5]=_TPZ")
rem save zmax point
gosub cwait
cmd_control("FLD3=2") rem select z axis
cmd_control("FLD2=1") rem max
cmd_control("FLD1=MXMN[5]")
cnbr=24
gosub excmd
if(manual=1)
 print "move z to its minimum limit"
 read_axis(point)
 zmin=point[2]
else
 move("z",zmin)
 gosub inpsn
endif
cmd_control("MXMN[4]=_TPZ")
rem save zmin
gosub cwait
cmd_control("FLD3=2") rem select z axis
cmd_control("FLD2=0") rem min
cmd_control("FLD1=MXMN[4]")
cnbr=24
gosub excmd
if(manual=1)
 print "move y to its minimum limit"
 read_axis(point)
 ymin=point[1]
else
 move("y",ymin)
 gosub inpsn
endif
cmd_control("MXMN[2]=_TPY")
rem save ymin
gosub cwait
cmd_control("FLD3=1") rem select y axis
cmd_control("FLD2=0") rem min
cmd_control("FLD1=MXMN[2]")
cnbr=24
gosub excmd
if(manual=1)
 print "move x to its minimum limit"
 read_axis(point)
 xmin=point[0]
else
 move("x",xmin)
 gosub inpsn
endif
rem set sensor 1 threshold:
rem gosub cwait
rem cmd_control("FLD3=1") rem select sensor 1
rem cmd_control("FLD1=200") rem threshold
rem cnbr=2
rem gosub excmd
rem set sensor 2 threshold:
rem gosub cwait
rem cmd_control("FLD3=2") rem select sensor 2
rem cmd_control("FLD1=100") rem threshold
rem cnbr=2
rem gosub excmd
cmd_control("MXMN[0]=_TPX")
rem save xmin
gosub cwait
cmd_control("FLD3=0") rem select x axis
cmd_control("FLD2=0") rem min
cmd_control("FLD1=MXMN[0]")
cnbr=24
gosub excmd
rem set map box edges:
cmd_control("FLD3=0") rem select x axis
for i=0 to xdiv-1
	gosub cwait
	cmd_control("FLD2="+str(i))
	cmd_control("FLD1="+str(xpt[i]))
	cnbr = 13
	gosub excmd  rem cmd 13 = 'set axis division'
next
gosub cwait
cmd_control("FLD3=1") rem select y axis
for i=0 to ydiv-1
	gosub cwait
	cmd_control("FLD2="+str(i))
	cmd_control("FLD1="+str(ypt[i]))
	cnbr = 13
	gosub excmd  rem cmd 13 = 'set axis division'
next
gosub cwait
cmd_control("FLD3=2") rem select z axis
for i=0 to zdiv-1
gosub cwait
	cmd_control("FLD2="+str(i))
	cmd_control("FLD1="+str(zpt[i]))
	cnbr = 13
	gosub excmd  rem cmd 13 = 'set axis division'
next
xd=(xmax-xmin)/255
rem print "xd = ",xd
yd=(ymax-ymin)/255
rem print "yd = ",yd
zd=(zmax-zmin)/255
rem print "zd = ",zd
slew_speed(80)
slew_acc(1000)
slew_dec(1000)
rem move("x",xmax)
rem move("yz",ymin,zmin)
rem move("x",xmin)
xdr=1
ydr=1
yi=0
xi=0
for k=0 to zdiv-1
	move("z",zmin+zpt[k]*zd)
	for j=0 to ydiv-1
		move("y",ymin+ypt[yi]*yd)
		for i=0 to xdiv-1
			move("x",xmin+xpt[xi]*xd)
			gosub emap  rem enter sensor readings into map
			xi=xi+xdr
		next
		if xdr=1
			xdr=-1
			xi=xdiv-1
		else
			xdr=1
			xi=0
		endif
		yi=yi+ydr
	next
	if ydr=1
		ydr=-1
		yi=ydiv-1
	else
		ydr=1
		yi=0
	endif
next
cmd_control("MAPOK=1") rem indicate map loaded
gosub flshsavemap  rem save map in flash memory
rem gosub filesavemap  rem save map in file
rem gosub filegetmap   rem get map from file TEST
cmd_control("PROXENB=1") rem prox detection on
gosub startup rem set prox board to startup conditions
cmd_control("CMDMODE=0") rem command mode off
print "end of program"
end

cwait
rem wait for 'ready' indication
wcount=200
while (val(cmd_control("MGCNBR")) > 0) & (wcount > 0)
	wcount = wcount - 1
	wait(.2)
wend
if (wcount = 0)
	print "timeout. cnbr =",cnbr
endif
return

excmd
cmd_control("CNBR="+str(cnbr))
return

inpsn
while (val(cmd_control("MGINPSN")) = 0)
wait(.2)
wend
return

rdsen
rem set s1 = sensor1-base1
gosub cwait
cmd_control("FLD3=1")
cnbr=5
gosub excmd
gosub cwait
s1=val(cmd_control("MGRESP"))
print "sensor = ",s1
return

emap
rem enter active sensor readings into map at [xi,yi,k]
rem print xi ,yi, k
gosub inpsn
wait(.3)
gosub cwait
cmd_control("FLD1=" + str(xi))
cmd_control("FLD2=" + str(yi))
cmd_control("FLD3=" + str(k))
cnbr=11
gosub excmd
rem gosub rdsen
return

imap
rem initialize map. return size of allocated map
gosub cwait
cmd_control("FLD1=" + str(xdiv))
cmd_control("FLD2=" + str(ydiv))
cmd_control("FLD3=" + str(zdiv))
cnbr=12
gosub excmd
gosub cwait
mapsize = val(cmd_control("MGRESP"))
return

flshsavemap
rem save map to flash memory
gosub cwait
cmd_control("FLD3=0") rem 0 = erase and save map #0, 1 = save next map.
cnbr=15
gosub excmd
wait(2)
return

flshgetmap
rem restore map from flash memory
gosub cwait
cmd_control("FLD2=1") rem >0 = don't change filters. 
cmd_control("FLD3=0") rem map number
cnbr=16
gosub excmd
return

prxoff
rem prox sensors off
gosub cwait
cmd_control("FLD3=0")
cnbr=3
gosub excmd
return

prxon
rem prox sensors 1 and 2 on
gosub cwait
cmd_control("FLD3=3")
cnbr=3
gosub excmd
return

zroff
rem auto zero off
gosub cwait
cmd_control("FLD1=0")
cnbr=27
gosub excmd
return

zron
rem set auto zero to one min. 
gosub cwait
cmd_control("FLD1=60")
cnbr=27
gosub excmd
return

startup
rem restore prox board startup settings
gosub cwait
cnbr=32
gosub excmd
return

rmap
rem read map value for current xyz position
gosub cwait
cnbr=14
gosub excmd
gosub cwait
mapval=val(cmd_control("MGRESP"))
return

clrprx
rem unlatch prox signal
gosub cwait
cnbr=17
gosub excmd
return

filesavemap
rem put map into file
fname = "c:\pecos\user\proxmap.dat"
gosub cwait
cmd_control("FLD2=10") rem select map
cmd_control("FLD3=1")  rem initialize
cnbr=18  rem record capture
gosub excmd
gosub cwait
cmd_control("FLD3=5")
cnbr = 18 rem get map size
gosub excmd
gosub cwait
size=val(cmd_control("MGRESP"))
cmd_control("FLD3=0")
cnbr=18
gosub excmd
gosub cwait
cnbr=18 rem set index,checksum to 0
gosub excmd
cksum = 0
gosub cwait
cmd_control("FLD3=4")
gosub cwait
open fname for writing, sequential as #1
for i=1 to size
 cnbr = 18 rem read 1 word
 gosub excmd
 gosub cwait
 vbl = cmd_control("MGRESP")
 cksum = cksum + val(vbl)
 fprint #1,vbl + chr(13) + chr(10)
next
close #1
cnbr = 18 rem read checksum
gosub excmd
gosub cwait
rcvcksum = val(cmd_control("MGRESP"))
if (cksum = rcvcksum)
 print "checksum ok"
else
 print "bad checksum: ",cksum,rcvcksum
endif
return

filegetmap
rem get map from file
fname = "c:\pecos\user\proxmap.dat"
open fname for reading, sequential as #1
finput #1,var
msize = val(var)
gosub cwait
cmd_control("FLD1=" + var)
cmd_control("FLD3=0")
cnbr = 22
gosub excmd
gosub cwait
cmd_control("FLD3=1")
cksum = 0
for i = 1 to msize-1
 finput #1,var
 cksum = cksum + val(var)
 gosub cwait
 cmd_control("FLD1=" + var)
 cnbr = 22
 gosub excmd
next
close #1
gosub cwait
cmd_control("FLD1=" + str(cksum))
cnbr = 22
gosub excmd
gosub cwait
cmd_control("FLD3=2")
cnbr = 22
gosub excmd
size=val(cmd_control("MGRESP"))
if size=msize
 print "checksum ok"
else
 print "checksum bad"
endif
return

