$MOD841                         ; Use 8052 predefined symbols
;11-7-09 UVC33071 BOARD CONTROLLER
;11-9-09 REARRANGED MS2 AND MS5
;11-16-09 CHANGED READ: TO SPEED IT UP. CHANGED FPGA LOADER TO SPEED IT UP
;1-21-10 ADDED MORE NOP DELAYS BETWEEN DSP RESET ON AND DSP RESET OFF.
;3-30-10 DLINIT(0): TURNED OFF CS_MEM AND CS_DSP VER 2:2
;4-15-10 VER 2:3 ADDED DWNRDY BIT TO PREVENT MULTIPLE LOADING OF DOWWNLOADED BLOCK.
;7-24-2012 VER 2:4 SWAPPED LED1 AND LED2 FOR REV B UVC CONTROLLER
;10-13-2012 VER 2:5 SET MOSI BEFORE DSP FLASH LOAD TO PREVENT INTERFERENCE WITH DSP MOSI.
;10-22-2012 VER 2:6 FIXED ABOVE.
;3-6-2013  VER 2:7 (207) PUT WATCHDOG BACK IN.
;3-8-2013  ADDED JMPTBL BOUNDS CHECK TO PREVENT PGM HANGUP.
;3-21-2013 VER 2:8 CHANGED REBOOT CMD FROM #1 TO #27. #1 RESERVED FOR DSP.

;CURRENT DSP SIGNAL PIN USAGE:
;MS2:	uart transmitter ready
;MS3:	handshake from DSP
;MS4:	handshake from DSP
;MS5:	handshake to DSP

;dsp .ldr file type:  SPI SLAVE, ASCII, 32 BIT
;SET DEBUG TO 1 TO USE DEBUGGER, 0 NORMALLY
DEBUG	EQU	0
;SET ASCII TO 1 FOR ASCII MODE COMMANDS
ASCII	EQU	0

MY_ADRS	EQU	081H	;DSP UART1 ADDRESS.

;BOARD TYPE 1492:
BRDTYPH	EQU	14
BRDTYPL	EQU	92

VERMAJ EQU	2
VERMIN EQU      8

MSIZ	EQU	4
CMDSIZ	EQU	20	;size of command buffer
RXBUFSIZ	EQU	20
TICKVAL	EQU	8	;TICK INTERVAL IN MS./5  (40 MS.)
UCOUNT	EQU	25	;UART TIMEOUT COUNT (TIME = UCOUNT * TICKVAL)

FLAG0	EQU	P1.0	;DSP FLASH MEMORY CHIP SEL. INPUT
SCLOCK 	EQU 	P3.5
MOSI	EQU	P3.4
MISO	EQU	P3.3
CS_MEM	EQU	P2.0	;CONTROLLER FLASH MEM CHIP SEL. OUTPUT
CS_DSP	EQU	P2.1	;
MBOOT	EQU	P2.6
MS2	EQU	P2.5
MS3	EQU	P1.1
MS4	EQU	P1.2
MS5	EQU	P2.2
DSP_RST	EQU	P2.7
CTS	EQU	P1.3
RTS	EQU	P3.7

;FPGA PROGRAMMING BITS:
nCONFIG		EQU	P3.6
nSTATUS		EQU	P1.6
DCLK		EQU	P2.3
DATA0		EQU	P2.4
CONF_DONE	EQU	P1.5

;LEDS:
LED1		EQU	P0.6
LED2		EQU	P0.7



;SHIFT A BYTE LEFT
SHBL	MACRO	BNAM
	MOV	A,BNAM
	RLC	A
	MOV	BNAM,A
	ENDM

;SHIFT A BYTE RIGHT
SHBR	MACRO	BNAM
	MOV	A,BNAM
	RRC	A
	MOV	BNAM,A
	ENDM

;SHIFT BYTE STRING LEFT. CLEAR C BEFORE CALLING.REG IS LEAST SIG BYTE, NBR IS NUMBER OF BYTES.
SBSL	MACRO  REG,	NBR
	SHBL	REG
	IF (NBR > 1)
	 SBSL REG-1,NBR-1
	ENDIF
	ENDM

;SHIFT BYTE STRING RIGHT. CLEAR C BEFORE CALLING.REG IS MOST SIG BYTE, NBR IS NUMBER OF BYTES.
SBSR	MACRO  REG,	NBR
	SHBR	REG
	IF (NBR > 1)
	 SBSR REG+1,NBR-1
	ENDIF
	ENDM

;SHIFT FAC1 MANTISSA LEFT 1 BIT (MULTIPLY BY 2)
FACX2	MACRO   FAC1
	CLR	C
	SBSL	FAC1,MSIZ
	ENDM

;ADD TWO BYTES PLUS CARRY. BYTEA + BYTEB -> BYTEA
PADD	MACRO	BYTEA,BYTEB
	MOV	A,BYTEA
	ADDC	A,BYTEB
	MOV	BYTEA,A
	ENDM

;ADD COMPLEMENT OF BYTE AND CARRY
ADCPL	MACRO	BYTEA
	MOV	A,BYTEA
	CPL	A
	ADDC	A,#0
	MOV	BYTEA,A
	ENDM

;ADD COMPLEMENT OF BYTE AT R1 AND CARRY
ADCPLR1	MACRO
	MOV	A,@R1
	CPL	A
	ADDC	A,#0
	MOV	@R1,A
	DEC	R1
	ENDM

;ADD COMPLEMENT OF BYTE STRING A TO CARRY. RESULT IN BYTE STRING A
;NBR IS NUMBER OF BYTES IN STRING. BSTA IS LEAST SIG. BYTE.
BSCPL	MACRO	BSTA,NBR
	ADCPL	BSTA
	IF (NBR > 1)
	 BSCPL BSTA-1,NBR-1
	ENDIF
	ENDM

;ADD COMPLEMENT OF BYTE STRING @R1 TO CARRY. RESULT IN BYTE STRING @R1
;NBR IS NUMBER OF BYTES IN STRING. @R1 IS LEAST SIG. BYTE.
BSCPLR1	MACRO	NBR
	ADCPLR1
	IF (NBR > 1)
	 BSCPLR1 NBR-1
	ENDIF
	ENDM

;ADD BYTE STRING A TO BYTE STRING B. RESULT IN BYTE STRING A
;NBR IS NUMBER OF BYTES IN STRING. BSTA, BSTB ARE LEAST SIG. BYTES. CLEAR C BEFORE USING
BSADD	MACRO	BSTA,BSTB,NBR
	PADD	BSTA,BSTB
	IF (NBR > 1)
	 BSADD BSTA-1,BSTB-1,NBR-1
	ENDIF
	ENDM

;SHIFT BYTE STRING LEFT. CLEAR C BEFORE CALLING.REGA,REGB IS LEAST SIG BYTE, NBR IS NUMBER OF BYTES.
;REGA IS SHIFTED STRING, REGB RECEIVES RESULT ALSO
SBSLB	MACRO  REGA,REGB,NBR
	SHBL	REGA
	MOV	REGB,A
	IF (NBR > 1)
	 SBSLB REGA-1,REGB-1,NBR-1
	ENDIF
	ENDM

;SHIFT FAC1 MANTISSA LIFT 1 BIT AND PUT IN FAC2.
FACX2B	MACRO   FAC1,FAC2
	CLR	C
	SBSLB	FAC1,FAC2,MSIZ
	ENDM

;ADD FAC2 MANTISSA TO FAC1 MANTISSA
ADDAB	MACRO   FAC1,FAC2
	CLR	C
	BSADD	FAC1,FAC2,MSIZ
	ENDM



;MULTIPLY FAC1 MANTISSA BY 10 (DECIMAL)
; FAC2 IS USED FOR TEMPORARY STORAGE
FACX10	MACRO   FAC1,FAC2
	FACX2B  FAC1,FAC2
	FACX2   FAC1
	FACX2   FAC1
	ADDAB   FAC1,FAC2
	ENDM

;REPLACE VA WITH VA + VB. LEAVE RESULT IN FAC
FADDS	MACRO	VA,VB
	MOV	R0,#VA
	CALL	GETA
	MOV	R0,#VB
	CALL	GETB
	CALL	FADDAB
	MOV	R0,#VA
	CALL SAVEFACR0
	ENDM

;REPLACE VA WITH VA - VB. LEAVE RESULT IN FAC
FSUBS	MACRO	VA,VB
	MOV	R0,#VA
	CALL	GETA
	MOV	R0,#VB
	CALL	GETB
	XRL	FACB,#1
	CALL	FADDAB
	MOV	R0,#VA
	CALL SAVEFACR0
	ENDM

;FAC <- VA + VB.
FADD	MACRO	VA,VB
	MOV	R0,#VA
	CALL	GETA
	MOV	R0,#VB
	CALL	GETB
	CALL	FADDAB
	ENDM

;FAC <- VA - VB.
FSUB	MACRO	VA,VB
	MOV	R0,#VA
	CALL	GETA
	MOV	R0,#VB
	CALL	GETB
	XRL	FACB,#1
	CALL	FADDAB
	ENDM

;FAC <- FAC + VB.
FADDF	MACRO	VB
	MOV	R0,#VB
	CALL	GETB
	CALL	FADDAB
	ENDM

;FAC <- FAC - VB.
FSUBF	MACRO	VB
	MOV	R0,#VB
	CALL	GETB
	XRL	FACB,#1
	CALL	FADDAB
	ENDM

;FAC <- FAC - (DPTR).
FSUBFEX	MACRO
	CALL	GETBEX
	XRL	FACB,#1
	CALL	FADDAB
	ENDM

;SHIFT BYTE @R0 LEFT 4 BITS. SHIFT IN BITS 0-3 OF B
SHL4S	MACRO
	MOV	A,@R0
	SWAP	A
	PUSH	ACC
	ANL	A,#0F0H
	ORL	A,B
	MOV	@R0,A
	POP	ACC
	ANL	A,#0FH
	MOV	B,A
	DEC	R0
	ENDM

;SHIFT BYTE @R0 LEFT 4 BITS. SHIFT IN BITS 0-3 OF B.LAST STEP
SHL4L	MACRO
	MOV	A,@R0
	SWAP	A
	ANL	A,#0F0H
	ORL	A,B
	MOV	@R0,A
	ENDM

;SHIFT MANTISSA LEFT 4 BITS.SHIFT IN B[3..0],R0 POINTS TO LEAST SIG. BYTE.
;NBR IS SIZE OF MANTISSA.
SHLMANT4	MACRO	NBR
	IF	(NBR > 1)
		SHL4S
	ELSE
		SHL4L
	ENDIF
	IF	(NBR >  1)
		SHLMANT4	NBR-1
	ENDIF
	ENDM

;SHIFT MANTISSA AT R0 LEFT 4 BITS. ADRS IS ADDRESS OF MOST SIG BYTE OF MANTISSA.
;A[3..0] IS SHIFTED INTO THE LOW ORDER BYTE.
SHLMANTA	MACRO
	ANL	A,#0FH
	MOV	B,A
	SHLMANT4	MSIZ
	ENDM

;JUMP TO ADRS IF A IS LESS THAN VAL:
JALT	MACRO	VAL,ADRS
	CJNE	A,#VAL,$+5
	SJMP	$+7
	JNC	$+5
	LJMP	ADRS
	ENDM

;JUMP TO ADRS IF A IS GREATER THAN VAL:
JAGT	MACRO	VAL,ADRS
	CJNE	A,#VAL,$+5
	SJMP	$+7
	JC	$+5
	LJMP	ADRS
	ENDM

;MOVE BYTES FROM @R1 TO @R2
FMOV1	MACRO	NBR
	MOV	A,@R1
	MOV	@R0,A
	IF	(NBR > 1)
		INC	R0
		INC	R1
		FMOV1	NBR-1
	ENDIF
	ENDM

;MOVE FLOAT VB TO VA
FMOV	MACRO	VA,VB
	MOV	R0,#VA
	MOV	R1,#VB
	FMOV1	FSIZ
	ENDM

INCEAD	MACRO
	inc	eadrl
	mov	a,eadrl
	jnz	$+4
	inc	eadrh
	ENDM

;SAVE 4 BYTES (1 PAGE) IN FLASH, INCREMENT PAGE ADDRESS:
SAV4	MACRO	NUM1,NUM2
	MOV	EDATA1,#LOW(NUM1)
	MOV	EDATA2,#HIGH(NUM1)
	MOV	EDATA3,#LOW(NUM2)
	MOV	EDATA4,#HIGH(NUM2)
	MOV	ECON,#5
	MOV	ECON,#2
	INCEAD
	ENDM

;GET 4 BYTES FROM FLASH, INCREMENT PAGE ADDRESS:
GET4	MACRO
	MOV	ECON,#1
	INCEAD
	ENDM



;SPI CLOCK HIGH OR LOW TIME = 8 + 3N CYCLES
;12.6 MHZ CLOCK: 2.9 MICROSECS IF N = 10
SPIWAIT	MACRO
;	NOP
;	PUSH	B
;	MOV	B,#1
;	DJNZ	B,$
;	POP	B
;	NOP
;	PUSH	B       ;OK WITHOUT THE PUSH & POP TOO
;	POP	B
;	NOP
	ENDM

INCR7R6	MACRO
	INC	R6
	CJNE	R6,#0,$+4
	INC	R7
	ENDM

;CRITICAL REGION ON:
CRON	MACRO
	PUSH	IE
	CLR	EA
	ENDM

;CRITICAL REGION OFF:
CROF	MACRO
	POP	IE
	ENDM

;REFRESH WATCHDOG TIMER
REFRESH	MACRO
        CRON              ; refresh watchdog timer
	SETB    WDWR
	SETB    WDE
	CROF
	ENDM

;TRANSMIT AND RECEIVE A BIT VIA SPI
SPI_BIT	MACRO	OUTBIT
	CLR	SCLOCK
	MOV	C,ACC.OUTBIT
	MOV	MOSI,C
;	SPIWAIT
	MOV	C,MISO
	MOV	ACC.OUTBIT,C
	SETB	SCLOCK
;	SPIWAIT
	ENDM

;READ AN SPI BIT AND LOAD BIT INTO FPGA
LOAD_FPGA_BIT	MACRO
	CLR	SCLOCK
	CLR	DCLK
	MOV	C,MISO
	MOV	DATA0,C
	SETB	SCLOCK
	SETB	DCLK
	ENDM

BSEG
ORG 0
numok:		dbit	1
cmdrcvd:	dbit	1
cmderr:		dbit	1


tick:		dbit	1
cmd_rdy:	dbit	1
usehex:		dbit	1
FLASH_RDY:	DBIT	1
TWTS:		DBIT	1	;1 = TWO WORDS TO SAVE IN FLASH MEMORY
DLMODE:		DBIT	1	;1 = IN DOWNLOAD MODE
PASSMODE:	DBIT	1	;1 = PASS RECEIVED CHARS FROM SERIAL PORT TO DSP
MS3B:		DBIT	1
TXRDY:		DBIT	1	;1 = UART READY TO TRANSMIT
DWNRDY:		DBIT	1	;1 = BLOCK HAS BEEN DOWNLOADED BUT NOT DISPATCHED

DSEG
ORG 30h
cbufptr:	ds	1	;command buffer pointer
;KEEP MUP, cparam, AND FLASHM TOGETHER:
MUP:		DS	1	;MAKEUP BYTE FOR CPARAM
cparam:		ds	MSIZ	;command parameter
FLASHM:		DS	MSIZ	;TEMPORARY STORAGE FOR SPI FLASH MEMORY
;
cparamb:       	ds	MSIZ	;temporary storage for FACX10
tickcount:	ds	1
cptrh:		ds	1
cptrl:		ds	1
WRD:		DS	MSIZ
rxptr:		ds	1
UTMOUT:		DS	1	;UART TIMEOUT COUNTER
BYTECOUNT:	DS	1
CMDNBR:		DS	1

SPI_OUT:	DS	MSIZ	;SPI DATA TO BE TRANSMITTED
SPI_IN:		DS	MSIZ	;SPI RECEIVED DATA

ADRS_HI:	DS	1
ADRS_MID:	DS	1
ADRS_LO:	DS	1
PGSTRT:		DS	3

FLASH_MODE:	DS	1

SEC_GRAN:	DS	1	;SECTOR GRANULARITY 0: 64K, 1: 32K, 2: 4K

BLOCKLENGTH:	DS	1
BLOCKCOUNT:	DS	1
BCKSUM:		DS	1


ISEG
ORG 080h
cmdbuf:		ds	20      ;command buffer
outbuf:		ds	10
rxbuf:		ds	RXBUFSIZ


ORG 0FFH
stack:

XSEG
ORG 00h
;stepbase:	ds	MAXSTEPS*STEPSIZE

DLBLOCK:	DS	256


;____________________________________________________________________
                                                  ; BEGINNING OF CODE
CSEG

ORG 0000h

        JMP     MAIN

;____________________________________________________________________
                                             ; INTERRUPT VECTOR SPACE

ORG 0003h	;EXTERNAL INT. 0       	PRIORITY 2
		RETI
ORG 000BH	;TIMER/COUNTER 0  	PRIORITY 4
		RETI
ORG 0013H	;EXTERNAL INT. 1	PRIORITY 5
		RETI
ORG 001BH	;TIMER/COUNTER 1	PRIORITY 6
		RETI
ORG 0023H	;UART RI + TI		PRIORITY 8
		jmp 	UARTINT
ORG 002BH	;TIMER/COUNTER 2	PRIORITY 9
		RETI
ORG 0033h	; ADC ISR               PRIORITY 3
                RETI
ORG 003BH	;SPI/IIC       		PRIORITY 7
		RETI
ORG 0043H	;PSMI	       		PRIORITY 1 (HIGHEST)
		RETI
ORG 0053H	;TII	       		PRIORITY 11 (LOWEST)
		RETI
ORG 005BH	;WATCHDOG      		PRIORITY 2
		RETI




;UART interrupt
UARTINT:
	NOP
	JNB	TI,UA1
    	;TRANSMITTER HAS FINISHED
    	JNB	CTS,UA1		;JMP IF CTS IS NOT READY
;;    	SETB	MS2		;INFORM DSP THAT TRANSMITTER IS READY
    	CLR	TI		;PREVENT SUCCESSIVE INTERRUPTS
    	SETB	TXRDY
UA1:
	jnb	ri,uartend
	SETB	RTS		;BUSY
	PUSH	B
	push	acc
	push	psw
	mov	psw,#010h	;select register bank 2
UA:
	mov	a,sbuf  	;get the char
	clr	ri              ;clear rcv interrupt flag
	JNB	DLMODE,BC00	;JMP IF NOT IN DOWNLOAD MODE
	CALL	DLCHAR
	JMP	UFIN
BC00:
	JNB	ACC.7,UC	;JMP IF NOT COMMAND
	;COMMAND. IS IT FOR DSP?
	CJNE	A,#MY_ADRS,UB
	;DSP COMMAND
	SETB	PASSMODE
	JMP	UC
UB:
	;LOCAL COMMAND
	CLR	PASSMODE
UC:
	JNB	PASSMODE,UD
	;PASS DATA TO DSP
	JNB	MS5,UF		;JMP IF DSP IS USING SPI
	SETB	CS_MEM
	CLR	CS_DSP	;CHIP SEL. ON
	CALL	SPI_TXRXR
	SETB	CS_DSP	;CHIP SEL. OFF
	SETB	MOSI
UF:	JMP	UFIN
UD:
	IF	(ASCII)
	JMP	ASCC	;JMP IF ASCII COMMUNICATIONS
	ENDIF
	;BINARY COMMUNICATIONS
	CALL	BINCHECK
	JMP	UFIN
	;save character in buffer
ASCC:	JZ	UFIN		;IGNORE NULL
	cjne	a,#0ah,savchr
	jmp	ufin		;ignore line feed
savchr:
	mov	r0,rxptr
	mov	@r0,a		;save char
	cjne	r0,#rxbuf+RXBUFSIZ-1,$+3
	jnc	ufin            ;jmp if buffer full
	inc	rxptr		;increment buffer pointer
	cjne	a,#0dh,ufin	;jmp if not cr
	call	process_buf	;char is cr: execute the command
ufin:	JB	ri,UA		;JMP IF ANOTHER CHAR HAS BEEN RECEIVED
	pop	psw
	pop	acc
	POP	B
	CLR	RTS		;READY
uartend:
	reti

DLCHAR:
	NOP
;IN DOWNLOAD MODE. SAVE CHARACTER IN XSEG
	MOV	DPTR,#DLBLOCK
	MOV	DPL,BLOCKCOUNT
	MOV	DPP,#0
	MOVX	@DPTR,A
	ADD	A,BCKSUM
	MOV	BCKSUM,A
	INC	BLOCKCOUNT
	MOV	A,BLOCKLENGTH
	CJNE	A,BLOCKCOUNT,DLC00
	CLR	DLMODE		;BLOCK HAS BEEN DOWNLOADED
	call	clrw  	;CLEAR WRD
	MOV	R4,#3
	MOV	WRD+MSIZ-1,BCKSUM
	MOV	R1,#WRD
	CALL	OUTNUM
	MOV	DPTR,#SEPARATOR
	CALL	SENDSTRING
	call	cmd_init
DLC00:	RET

BINCHECK:
	NOP
	CJNE	A,#0FFH,BIN1	;JMP IF NOT RUBOUT
SYNC:	MOV	BYTECOUNT,#0
	JMP	BINFIN
BIN1:
	;COMMAND?
	JNB	ACC.7,BDAT
	;HIGH ORDER BIT IS 1. ASSUME BYTE IS COMMAND
	MOV	CMDNBR,A
	MOV	BYTECOUNT,#1
	JMP	BINFIN
BDAT:
	;ASSUME BYTE IS DATA OR CHECKSUM
	PUSH	ACC		;SAVE CHAR
	MOV	A,BYTECOUNT
	INC	BYTECOUNT
	JNZ	BIN2
	;ERROR: NO COMMAND WAS RECEIVED
	POP	ACC
	JMP	SYNC
BIN2:
	ADD	A,#MUP-1	;A = ADDRESS TO STORE DATA
	JAGT	MUP+MSIZ,BINCS	;THIS SHOULD BE THE CHECKSUM
	MOV	R0,A
	POP	ACC
	MOV	@R0,A
	JMP	BINFIN
BINCS:
	JAGT	MUP+MSIZ+1,BIN5	;JMP IF TOO MANY CHARS
	;CHECKSUM RCVD
	MOV	R0,#MUP
	MOV	R1,#MSIZ+1
	POP	ACC
	ADD	A,CMDNBR
BINCS1:
	ADD	A,@R0
	INC	R0
	DJNZ	R1,BINCS1
	ANL	A,#7FH
	JNZ	SYNC	;JMP IF CHECKSUM ERROR
	;BINARY COMMAND HAS BEEN RECEIVED
	;INSERT MAKEUP BITS
	MOV	R1,#MSIZ
	MOV	R0,#cparam
INB0:
	MOV	A,MUP
	JNB	ACC.6,INB1
	;ADD MAKEUP BIT
	MOV	B,@R0
	ORL	B,#80H
	MOV	@R0,B
INB1:
	RL	A
	MOV	MUP,A
	INC	R0
	DJNZ	R1,INB0

	MOV	A,CMDNBR
	ANL	A,#7FH		;A = COMMAND NUMBER
	;TEST FOR JMPTBL BOUNDS:
	JAGT	MAXTBL,BINFIN	;JMP IF COMMAND NUMBER IS OUT OF RANGE
	RL	A		;A = OFFSET INTO JMPTBL
	;EXECUTE COMMAND:
	CPL	LED1
	setb	numok
	CALL	EXBIN
;if a = 0: bad command. a = 1: good command a = 2: good command, no '>'. a = 3: SPI busy.
	JB	DLMODE,BINFIN
	jnz	bp0
	mov	A,#'?'  ;0
	jmp	bpcp
bp0:    dec	a
	jnz	bp1
	mov	a,#'>'  ;1
	jmp	bpcp
bp1:	dec	a
	jnz	bp2
    	jmp	bpcig   ;2
bp2:	MOV     A,#'<'  ;3
	MOV	UTMOUT,#UCOUNT	;RESET UART TIMEOUT COUNT
bpcp:  	CALL    SENDCHAR
bpcig:	call	cmd_init
	JMP	BINFIN
BIN5:
	;ERROR: TOO MANY CHARS
	POP	ACC
	JMP	SYNC
BINFIN:
	RET

EXBIN:
	NOP
	mov	r0,a
	mov	dptr,#jmptbl
	movc	a,@a+dptr       ;high byte of addrs
	mov	r1,a
	mov	a,r0
	inc	a
	movc	a,@a+dptr       ;low byte of address
	mov	dpl,a
	mov	dph,r1
	clr	a
	jmp	@a+dptr		;jump to command procedure
	RET			;WE SHOULD NEVER GET HERE
;*****************************************************************************************
MAIN:                                                                                  ; *
;*****************************************************************************************
        MOV     SP,#stack-1
        SETB	TXRDY
        SETB	MS3B
        CLR	DLMODE
        CLR	DWNRDY
        CLR	PASSMODE
	SETB	DSP_RST
	SETB	FLAG0           ;INPUT FROM DSP. SPI FLASH MEM. CHIP SEL.
	SETB	SCLOCK
	SETB	MOSI
	SETB	MISO
	SETB	CS_MEM         	;OUTPUT. SPI FLASH MEM. CHIP ALL.
	SETB	CS_DSP          ;OUTPUT. SPI DSP CHIP SEL.
	CLR	MBOOT         	;OUTPUT TO DSP. 0 = SPI SLAVE BOOT, 1 = SPI MASTER BOOT
	SETB	RTS		;OUTPUT TO SERIAL PORT. 0 = READY TO RECEIVE
;;	SETB	CTS		;INPUT FROM SERIAL PORT. 0 = OK TO TRANSMIT
	SETB	MS2		;UART TX READY
;;	SETB	MS3		;INPUT FROM DSP. 0 = UART TRANSMIT REQUEST (DSP HAS CHARACTERS TO SEND)
        SETB	MS5		;OUTPUT TO DSP.  0 = REQUEST ACKNOWLEDGED
;;        SETB	MS4		;INPUT FROM DSP. 0 = SPI BUS REQUEST
        ;FPGA BITS:
        CLR	nCONFIG		;OUTPUT
;;        SETB	nSTATUS		;INPUT
        CLR	DCLK		;OUTPUT
        CLR	DATA0		;OUTPUT
;;        SETB	CONF_DONE	;INPUT
        ;SET ALL PORT1 BITS TO DIGITAL INPUTS:
        MOV	P1,#0H

	MOV	PSMCON,#0

	;LEDS OFF:
	SETB	LED1
	SETB	LED2

	MOV	SEC_GRAN,#0	;SET SECTOR SIZE TO 64K
;	CLR	DOG_RST
;	JNB	WDS,MN1
;	SETB	DOG_RST	;SET TO INDICATE WATCHDOG TIMEOUT
MN1:
;	;ENABLE WATCHDOG TIMER:
;	SETB	WDWR
;	MOV	WDCON,#72h	;2 SEC TIME-OUT

	mov	p0,#0ffh	;set up port 0 for inputs
	MOV	BYTECOUNT,#0	;INITIALIZE BINARY COM.
	MOV	UTMOUT,#UCOUNT	;UART TIMEOUT
	MOV	IP,#0H		;SET ALL INTERRUPTS TO LO.
	MOV	PSW,#00H	;SELECT REGISTER BANK 0
;        MOV     SP,#stack-1
        clr	usehex
        mov	tickcount,#TICKVAL
        clr	tick
        SETB	FLASH_RDY
        MOV	FLASH_MODE,#0

        orl	cfg841,#081h	;enable internal xram,extended stack pointer
        MOV	PLLCON,#01H     ;max rate for 3 volt part: xtal/2

        IF	(DEBUG=0)
	; CONFIGURE UART....
	;115200:
	MOV	T3CON,#081H
	MOV	T3FD,#32
        ;38400:
;        MOV     T3CON,#083h	;Will need to disable UART configuration
;        MOV     T3FD,#8		;when using debugger as any writes to UART

        MOV     SCON,#52h       ;timing or config registers could affect
                                ;the debugger interface
	CLR	RI
	CLR	TI
	ENDIF

;	MOV	RCAP2H,#0eah	;1 ms sample time
;	MOV	RCAP2L,#066h
	MOV	RCAP2H,#094h	;5 ms sample time
	MOV	RCAP2L,#00h
	MOV	T2CON,#04H      ;START TIMER 2

	;initialize command decoder
	push	psw
	mov	psw,#010h	;select register bank 2 (used exclusively by UART)
	call	cmd_init
	pop	psw
; CONFIGURE SPI
	MOV	SPICON,#0    	;disable SPI hardware


;	SETB	ES		;enable UART interrupt
	IF	(DEBUG=0)
	;;FPGA
	MOV	ADRS_HI,#02H
	MOV	ADRS_MID,#00H
	MOV	ADRS_LO,#00
	CALL	FPGA_BOOT
        ;DSP
        SETB	MOSI
        SETB	DSP_RST
	SETB	MBOOT         	;OUTPUT TO DSP. 0 = SPI SLAVE BOOT, 1 = SPI MASTER BOOT
	NOP
	NOP
	NOP
	NOP
	NOP
	CLR	DSP_RST		;FLASH BOOT LOAD

	;ENABLE WATCHDOG TIMER:
	SETB	WDWR
	MOV	WDCON,#72h	;2 SEC TIME-OUT


	SETB    EA      	;global interrupt enable
	CLR	RTS		;READY TO RECEIVE
	SETB	ES		;enable UART interrupt
	mov	a,#'>'  	;send an ack char in case we were rebooted
  	CALL    SENDCHAR
	ENDIF
WAIT:
	CPL	LED2
        REFRESH              ; refresh watchdog timer BACK IN
	IF	(DEBUG)
;***************************************************************
;***************************************************************
BOOTEST:
	SETB	MS2
	CLR	MS2
	JMP	BOOTEST


	MOV	ADRS_HI,#0FH
	MOV	ADRS_MID,#0F0H
	MOV	ADRS_LO,#00
	MOV	SEC_GRAN,#2  ;4K SECTOR
	CALL	SEC_ERASE

	MOV	ADRS_HI,#0FH
	MOV	ADRS_MID,#0F0H
	MOV	ADRS_LO,#00
	CALL	READ_START
AGAIN:
	CALL	READ
	JMP	AGAIN

;	CALL	FPGA_BOOT
	SETB	DSP_RST
	CLR	MBOOT
	CLR	DSP_RST
	CALL	RDSR
	MOV	A,#0
	CALL	WRSR	;CLEAR SPRL BIT
	CALL	RDSR
	MOV	A,#0
	CALL	WRSR	;CLEAR SEC. PROTECTION BITS
	CALL	RDSR
	CALL	WREN
	CALL	RDSR


	MOV	DPCON,#0
;	JMP	DBWT2
	SETB	DSP_RST
	CLR	MBOOT         	;0 = SPI SLAVE BOOT, 1 = SPI MASTER BOOT
	NOP
	NOP
	NOP
	NOP
	NOP
	CLR	DSP_RST         ;DSP RESET MUST BE OFF, OR FLASH HARDWARE WRITE PROTECT IS ON.
	;ERASE FIRST SECTOR:
	MOV	ADRS_HI,#0
	MOV	ADRS_MID,#0
	MOV	ADRS_LO,#0   ;THIS IS PAGE ADDRESS
	CALL	SEC_ERASE
DBWT1:
	CALL	RDSR
	JB	ACC.0,DBWT1

	MOV	PGSTRT,#0	;HI
	MOV	PGSTRT+1,#0     ;MID
	MOV	PGSTRT+2,#0     ;LO
	;TRANSFER TEST DATA BLOCK FROM CODE MEM TO XRAM:
	MOV	R0,#0
	MOV	R1,#0
	MOV	DPTR,#PAGE0
XLOOP:
;	CLR	A
	MOV	A,R1
	MOVC	A,@A+DPTR
	JZ	XL1
	NOP
XL1:
	MOVX	@R1,A
	INC	R1
;	INC	DPTR
	DJNZ	R0,XLOOP
	MOV	BLOCKCOUNT,#0
	CALL	PROGRAM_PAGE
        CALL	VERIFY_PAGE

;	MOV	R0,#0
;	MOV	DPTR,#PAGE0
;	CALL	PP_START
;DBLP0:
;	CLR	A
;	MOVC	A,@A+DPTR
;	INC	DPTR
;	CALL	SPI_TXRXR
;	DJNZ	R0,DBLP0
;	CALL	PP_END

DBWT2:
	CALL	RDSR
	JB	ACC.0,DBWT2

	MOV	R0,#0
	MOV	DPTR,#PAGE0
	MOV	ADRS_HI,#0
	MOV	ADRS_MID,#0
	MOV	ADRS_LO,#00H
	CALL	READ_START
RAG1:	CALL	READR
;	CALL	BITREVA		;BYTES ARE STORED IN FLASH BACKWARD BY FLASHLOAD
	MOV	B,A             ;B = FLASH
	CLR	A
	MOVC	A,@A+DPTR       ;A = PROGRAMMED VAL
	INC	DPTR
	CLR	C
	SUBB	A,B
	JZ	DBGG2
	NOP     ;ERROR
DBGG2:
	DJNZ	R0,RAG1
	CALL	READ_END




	CALL	RDSR
	CALL	FLASHLOAD
	CALL	RDSR
	CALL	FLASHSTAT

	CALL	RDSR
	CLR	DSP_RST
	CALL	RDSR
	MOV	A,#0
	CALL	WRSR	;CLEAR SPRL BIT
	CALL	RDSR
	MOV	A,#0
	CALL	WRSR	;CLEAR SEC. PROTECTION BITS
	CALL	RDSR
	CALL	WREN
	CALL	RDSR






	MOV	ADRS_HI,#0
	MOV	ADRS_MID,#0
	MOV	ADRS_LO,#00H
	CALL	READ_START
RAG:	CALL	READR
;	CALL	BITREVA		;BYTES ARE STORED IN FLASH BACKWARD BY FLASHLOAD
	JMP	RAG

AGN:
	MOV	ADRS_HI,#02H
	MOV	ADRS_MID,#00H
	MOV	ADRS_LO,#00
	CALL	FPGA_BOOT
        JMP	AGN

	CALL	FLASHTEST

	CLR	MBOOT         	;OUTPUT TO DSP. 0 = SPI SLAVE BOOT, 1 = SPI MASTER BOOT
	CLR	DSP_RST

CALL	RES
	MOV	ADRS_HI,#02H
	MOV	ADRS_MID,#00H
	MOV	ADRS_LO,#00
	CALL	FPGA_BOOT
	CALL	RDID
	MOV	A,#0
	CALL	WRSR

	CALL	RDSR
MOV	A,#86H
CALL	BINCHECK
MOV	A,#00H
CALL	BINCHECK
MOV	A,#00H
CALL	BINCHECK
MOV	A,#00H
CALL	BINCHECK
MOV	A,#04H
CALL	BINCHECK
MOV	A,#58H
CALL	BINCHECK
MOV	A,#1EH
CALL	BINCHECK

MOV	ADRS_HI,#0FH
MOV	ADRS_MID,#81H
MOV	ADRS_LO,#0C8H
CALL	READ_START
CALL	FR4


CALL	FPGA_BOOT
	CLR	DSP_RST		;TRY FLASH BOOT LOAD

DBL0:

	ENDIF


;*****************************************************
;*****************************************************
; TEST FOR DSP SPI CONTROL REQUESTS:
;	JNB	CS_MEM,TST3	;SKIP TESTS IF DSP HAS CONTROL OF SPI BUS
;	JB	MS3,TST0	;JMP IF NO DSP UART REQUEST
;	JNB	MS2 ,TST3	;JMP IF ACKNOWLEDGED
;	CLR	MS2		;ACK. WILL BE SET BY UART TX COMPLETE INTERRUPT
;	SETB	DSP_ACK
;	JMP	TST3
;TST0:
;	JB	MS2,TST1	;JMP IF NO ACK
;	JNB	DSP_ACK,TST1
;	;CHAR SHOULD BE LOADED. READ AND TRANSMIT OUT UART:
;	JNB	TXRDY,TST3	;WAIT FOR UART
;	JMP	FWDCHR

	JNB	FLAG0,TST3      ;EXIT IF DSP IS USING SPI (FLASH MEM)
	JB	MS3,TST0        ;JMP IF DSP SPI REQUEST
	JNB	MS3B,TST3
	CLR	MS3B
	CLR	MS2
	JMP	TST3
TST0:	JNB	MS3B,FWDCHR     ;JMP IF LEADING EDGE OF SPI REQUEST

;SPI MASTER REQUEST TEST:
TST1:	JB	MS4,TST2	;JMP IF NO DSP SPI MASTER REQ.
	CLR	MS5		;ACK.	DSP CAN NOW BECOME SPI MASTER TO READ OR WRITE FLASH MEM.
	JMP	TST3
TST2:	SETB	MS5
TST3:	JMP	WAIT
FWDCHR: SETB	MS3B            ;SET MS3B BIT SO REQUEST WILL NOT BE RECOGNIZED AGAIN
	;RCV CHAR FROM DSP AND SEND OUT UART
	CLR	A	;SEND A ZERO TO DSP
	SETB	CS_MEM
	CLR	CS_DSP	;CHIP SEL. ON
	SPI_BIT 0	;FIRST BIT
	SPI_BIT 1
	SPI_BIT 2
	SPI_BIT 3
	SPI_BIT 4
	SPI_BIT 5
	SPI_BIT 6
	SPI_BIT 7

	SETB	CS_DSP	;CHIP SEL. OFF
	SETB	MOSI
;;;	CALL	SENDCHAR
        PUSH	ACC
        MOV	A,#200		;TIMEOUT VAL
FW0:
        JB	TXRDY,FW1
      	DJNZ	ACC,FW0
FW1:
        MOV	A,#200		;TIMEOUT VAL
FW2:
        JNB	CTS,FW3
      	DJNZ	ACC,FW2
FW3:
	POP	ACC
        CLR     TXRDY		;WILL BE SET BY TX COMPLETE INTERRUPT
        MOV     SBUF,A

	SETB	MS2		;INFORM DSP : READY TO ACCEPT A CHARACTER.
;;	CLR	DSP_ACK
	JMP	TST3



$INCLUDE(UARTIO.asm)
;SEND AND RECEIVE A BYTE FROM SPI. A = BYTE TO SEND, A = RECEIVED BYTE. MSB FIRST
SPI_TXRX:
	NOP
	PUSH	B
	MOV	B,#8
	SETB	SCLOCK
	SPIWAIT
SPILOOP:
	RLC	A
	MOV	MOSI,C       ;CLK 1->0: OUTPUT DATA
	CLR	SCLOCK
	SPIWAIT
	MOV	C,MISO       	;CLK 0->1: READ DATA
	MOV	ACC.0,C
	SETB	SCLOCK
	SPIWAIT
	DJNZ	B,SPILOOP
	POP	B
	RET

;SEND AND RECEIVE A BYTE FROM SPI. A = BYTE TO SEND, A = RECEIVED BYTE. LSB FIRST
SPI_TXRXR_W:
	NOP
	PUSH	B
	MOV	B,#8
;	SETB	SCLOCK	;SHOULD ALREADY BE SET
;	SPIWAIT
SPILOOPR:
	CLR	SCLOCK
	RRC	A
	MOV	MOSI,C       ;CLK 1->0: OUTPUT DATA
;	CLR	SCLOCK
	SPIWAIT
	MOV	C,MISO       	;CLK 0->1: READ DATA
	MOV	ACC.7,C
	SETB	SCLOCK
	SPIWAIT
	DJNZ	B,SPILOOPR
	POP	B
	RET

;UNWRAPPED SPI_TXRXR:
SPI_TXRXR:
	NOP
	SPI_BIT 0	;FIRST BIT
	SPI_BIT 1
	SPI_BIT 2
	SPI_BIT 3
	SPI_BIT 4
	SPI_BIT 5
	SPI_BIT 6
	SPI_BIT 7
	RET

;SEND THE WORD IN SPI_OUT TO DSP LSB FIRST.
XWORD:
	NOP
	MOV	R1,#SPI_OUT+MSIZ-1
	MOV	R0,#MSIZ
	SETB	CS_MEM
	CLR	CS_DSP
XWL:	MOV	A,@R1
	CALL	SPI_TXRXR
	DEC	R1
	DJNZ	R0,XWL
	SETB	CS_DSP
	SETB	MOSI
	RET


;RECEIVE A WORD FROM DSP LSB FIRST.
RWORD:
	NOP
	MOV	R1,#SPI_IN+MSIZ-1
	MOV	R0,#MSIZ
	SETB	CS_MEM
	CLR	CS_DSP
RWL:	MOV	A,#0
	CALL	SPI_TXRXR
	MOV	@R1,A
	DEC	R1
	DJNZ	R0,RWL
	SETB	CS_DSP
	SETB	MOSI
	RET




;;SEPARATOR:      DB 10,13,0
SEPARATOR:      DB 13,0
;DIVIDE MANTISSA AT R1 BY 16. SET A TO REMAINDER IN ASCII (HEX)
MDIV16:
	NOP
	MOV	R2,#MSIZ
	MOV	B,#0
	MOV	A,R1
	PUSH 	ACC		;SAVE R1
MD16A:
	MOV	A,@R1           ;GET BYTE
	SWAP	A               ;SHIFT BY 4
	PUSH	ACC             ;TEMPORARLY SAVE
	ANL	A,#0FH          ;MASK OFF HIGH NIBBLE
	ORL	A,B             ;REPLACE HIGH NIBBLE WITH LAST HI NIBBLE
	MOV	@R1,A           ;SAVE SHIFTED BYTE
	POP	B               ;GET SHIFTER BYTE
	ANL	B,#0F0H         ;MASK OFF LOW NIBBLE
	INC	R1
	DJNZ	R2,MD16A
	MOV	A,B
	SWAP	A
	CJNE	A,#10,$+3
	JC	MD16B		;JMP IF A < 10
	ADD	A,#7
MD16B:	ADD	A,#30H
	POP	B
	MOV	R1,B		;RESTORE R1
	RET

;MULTIPLY MANTISSA AT R1 BY 16
MMUL16:
	NOP
	MOV	R2,#MSIZ
	MOV	B,#0
	MOV	A,#MSIZ-1
	ADD	A,R1
	MOV	R1,A           	;R1 POINTS TO LEAST SIG. BYTE
MM16A:
	MOV	A,@R1           ;GET BYTE
	SWAP	A               ;SHIFT BY 4
	PUSH	ACC		;SAVE FOR LATER
	ANL	A,#0F0H         ;MASK OFF LOW NIBBLE
	ORL	A,B             ;REPLACE WITH LOW NIBBLE FROM PREVIOUS SHIFT
	MOV	@R1,A           ;SAVE BYTE
	POP	B               ;GET SHIFTED BYTE
	ANL	B,#0FH          ;MASK OFF HIGH NIBBLE
	DEC	R1
	DJNZ	R2,MM16A
	RET

;DIVIDE MANTISSA AT R1 BY 10. set a to remainder in ascii. uses r0,r1,r2,r3,a,b
MDIV10:
	NOP
	mov	r2,#MSIZ*2	;number of nibbles
	MOV	a,#0            ;initialize remainder
	push	acc
	mov	a,#MSIZ-1
	add	a,r1
	mov	r3,a            ;points to least sig byte of mantissa
mdl1:
	mov	a,r3
	mov	r0,a            ;r0 points to least sig byte.(for upcoming shift)
	pop	acc             ;previous remainder
	swap	a
	mov	b,a		;previous remainder in high order nibble of b
	MOV	A,@R1
	SWAP	A               ;move remainder to high order nibble
	ANL	A,#0FH          ;a = highest order nibble
	orl	a,b             ;add in the previous remainder
	MOV	B,#10
	DIV	AB
	PUSH	B               ;save remainder
	SHLMANTA                ;shift mantissa lift 1 nibble. shift in quotient nibble.
	djnz	r2,mdl1
	pop	acc		;final remainder
	add	a,#'0'		;convert to ascii char.
     	ret

;print the mantissa at r1 in decimal. R4 = NUMBER OF DIGITS
out5:
	NOP
	MOV	A,R4
	MOV	R6,A
	mov	r5,#outbuf+10;was 5
	mov	r7,#0	;zero counter
p10a:	call	mdiv10
	cjne	a,#'0',p10ab
	inc	r7
	jmp	p10aa
p10ab:	mov	r7,#0
p10aa:	push	acc
	dec	r5
	mov	a,r5
	mov	r0,a
	pop	acc
	mov	@r0,a
	djnz	r4,p10a
	mov	a,R6   ;number of digits
	clr	c
	subb	a,r7    ;number of digits with leading zeros removed
	jnz	p10ac
	;number is zero: print 1 zero
	mov	a,#'0'
	call	SENDCHAR
	jmp	p10ret
p10ac:	mov	r4,a    ;r4 = # digits to print
	mov	a,r5
	add	a,r7
	mov	r5,a
p10b:	mov	a,r5
	mov	r0,a
	mov	a,@r0
	call	SENDCHAR
	inc	r5
	djnz	r4,p10b
p10ret:	ret


;print the mantissa at r1 in hex. R4 = NUMBER OF DIGITS
out4h:
	NOP
	mov	r5,#outbuf+10		;1-15-08 was outbuf+4
	mov	r7,#0	;zero counter
p4a:	call	mdiv16
	cjne	a,#'0',p4ab
	inc	r7
	jmp	p4aa
p4ab:	mov	r7,#0
p4aa:	push	acc
	dec	r5
	mov	a,r5
	mov	r0,a
	pop	acc
	mov	@r0,a
	djnz	r4,p4a
	mov	a,#4   ;number of digits
	clr	c
	subb	a,r7    ;number of digits with leading zeros removed
	jnz	p4ac
	;number is zero: print 1 zero
	mov	a,#'0'
	call	SENDCHAR
	jmp	p4ret
p4ac:	mov	r4,a    ;r4 = # digits to print
	mov	a,r5
	add	a,r7
	mov	r5,a
p4b:	mov	a,r5
	mov	r0,a
	mov	a,@r0
	call	SENDCHAR
	inc	r5
	djnz	r4,p4b
p4ret:	ret





zskip:
sk00:
	NOP
	clr	a
	movc	a,@a+dptr
	jnz	sk01
	jmp	sk02
sk01:	inc	dptr
	jmp	sk00
sk02:	inc	dptr
	inc	r6
	ret

;search for match between char string in cmdbuf and cmdtbl. length of string is in r3.
ismatch:
	NOP
	mov	r6,#0	;command position counter
	mov	dptr,#cmdtbl
im00:
	mov	r0,#cmdbuf
	mov	a,r3	;string length
	mov	r5,a
im03:
	clr	a
	movc	a,@a+dptr
	jnz	im04
	inc	dptr
	inc	r6
	jmp	im05
im04:
	clr	c
	subb	a,@r0
	jnz	im01
	;chars match
	djnz	r5,im02
	jmp	match
im02:
	inc	r0
	inc	dptr
	jmp	im03
im01:
	call	zskip
im05:
	movc	a,@a+dptr
	jnz	im00
	;end of command table
nomatch:
        clr	a
;        call	cmd_init
        jmp	imrt
match:
	inc	dptr
	clr	a
	movc	a,@a+dptr
	jnz	nomatch
        REFRESH              ; refresh watchdog timer
	;2*r6 is offset into jmptbl:
	mov	a,r6
	add	a,r6
	mov	r0,a
	mov	dptr,#jmptbl
	movc	a,@a+dptr       ;high byte of addrs
	mov	r1,a
	mov	a,r0
	inc	a
	movc	a,@a+dptr       ;low byte of address
	mov	cptrl,a
	MOV	cptrh,r1
;	setb	cmd_rdy
	mov	dpl,a
	mov	dph,r1
	clr	a
	jmp	@a+dptr		;jump to command procedure
imrt:
	ret

;initialize command decoder (register bank 2)
cmd_init:
	NOP
	mov	r3,#0	;command char count
	clr	numok
	clr	cmdrcvd
	clr	cmderr
	clr	cmd_rdy
	mov	cbufptr,#cmdbuf
	mov	rxptr,#rxbuf	;UART receive buffer
	mov	r0,#cparam
	mov	r1,#MSIZ
z00:	mov	@r0,#0
	inc	r0
	djnz	r1,z00
	ret

;a = received char
process_char:
	NOP
	;decide what to do with char:
	cjne	a,#0dh,pcctnu
	jmp	eocmd
eocmd:  ;end of line
	clr	a
	jb	cmdrcvd,excmd	;jmp if command entered
	cjne	r3,#0,excmd	;in case command does not need parameter
	jmp 	pcfin           ;extra end of line: ignore
        jb	cmderr,pcq
excmd:	call	ismatch		;this will set up cptrh.cptrl
;if a = 0: bad command. a = 1: good command
pcq:
	jnz	pcb
	mov	A,#'?'
	jmp	pcp
pcb:	cjne	a,#2,pcg
    	jmp	pcig            ;GOOD CMD, NO ACK
pcg:	MOV     A,#'>'
	MOV	UTMOUT,#UCOUNT	;RESET UART TIMEOUT COUNT
pcp:    CALL    SENDCHAR
pcig:	call	cmd_init
	jmp	pcfin
pcctnu:
	jb	cmdrcvd,pc01	;jump if command has been received
	;complete command not received yet
	;delimiter?
	cjne	a,#' ',pc001
	jmp	isdelim
pc001:	cjne	a,#',',pc002
	jmp	isdelim
pc002:	cjne	a,#'=',pc003
	jmp	isdelim
pc003:	cjne	a,#09h,pc004    ;tab
isdelim:
	cjne	r3,#0,pc005
	jmp	numerr		;no command has been entered
pc005:	setb	cmdrcvd         ;mark command as being received
	jmp	pcfin
pc004:
	;must be alphabetic:
	;upper case:
ucck:	JALT	'A',numerr
	JAGT	'Z',cklc
	;upper case. convert to lower case
	orl	a,#020h
cklc:	JALT	'a',numerr
	JAGT	'z',numerr
	;alphabetic char. put in buffer
	mov	r0,cbufptr
	cjne	r0,#cmdbuf+CMDSIZ,pc00  ;jmp if buffer not full
	;buffer full
	jmp	numerr
pc00:	mov	@r0,a
	inc	r3	;increment char count
	inc	r0
	mov	cbufptr,r0
	jmp pcfin
pc01:
	;must be a number
	JNB	USEHEX,PC01A
	;HEX
	CALL	ASCII2HEX
	JNC	PC01B
	JMP	NUMERR
PC01A:
	JALT	'0',numerr
	JAGT	'9',numerr
PC01B:
	;char is numeric
	anl	a,#0fh
	setb	numok		;set the numeric bit
	;decimal to hex conversion:
	push	acc
	JB	USEHEX,PC01C
	FACX10	cparam+MSIZ-1,cparamb+MSIZ-1		;multiply cparam by 10
	JMP	PC01D
PC01C:	MOV	R1,#CPARAM
	CALL	MMUL16			;MULTIPLY CPARAM BY 16
PC01D:
	;add digit to cparam:
	mov	r0,#cparam+MSIZ-1
	mov	r1,#MSIZ
	pop	acc
	clr	c
pc02:	addc	a,@r0
	mov	@r0,a
	clr	a
	dec	r0
	djnz	r1,pc02
        jmp	pcfin
numerr:
	setb	cmderr
pcfin:
	ret

process_buf:
	NOP
	mov	rxptr,#rxbuf
pb1:
	mov	r0,rxptr
	mov	a,@r0
	cjne	a,#0dh,pb2	;jmp if char is not cr
	;char is cr: end of command
	call	process_char
	ret
pb2:
	call	process_char
	inc	rxptr
	jmp	pb1
;end of process_buf

OUTNUM:
	NOP
	JB	USEHEX,PNTHEX
	CALL	OUT5
	RET
PNTHEX:	CALL	OUT4H
	RET

;COMMANDS
;a = 0 at start
;return with a=0 if error, a=1 if ok and '>' response wanted, a=2 if ok and no '>'.


clrw:
	NOP
	mov	r0,#wrd
	mov	r1,#MSIZ
clrw1:
	mov	@r0,#0
	inc	r0
	djnz	r1,clrw1
	ret

hexordec:
	NOP
	jnb	numok,hdbd
	mov	a,cparam+MSIZ-1
	jz	hdd
	setb	usehex
	jmp	hdd1
hdd:	clr	usehex
hdd1:	mov	a,#1
	jmp	hdok
hdbd:	clr	a
hdok:	ret



wdog_test:
	CLR	EA
	SETB	WDWR
	MOV	WDCON,#82H	;IMMEDIATE RESET
	jmp	$       ;WAIT HERE UNTIL WATCHDOG TIMER RESETS COMPUTER
dsp:	ret     	;we should never get here

version:
	NOP
	call	clrw
	MOV	WRD+MSIZ-2,#VERMAJ
	MOV	WRD+MSIZ-1,#VERMIN
	MOV	R1,#WRD
	MOV	r4,#4	;# chars
	JB	USEHEX,VER1
	MOV	R4,#5
VER1:	CALL	OUTNUM
	MOV	DPTR,#SEPARATOR
	CALL	SENDSTRING
	mov	a,#2
	ret


brdtyp:		;BOARD TYPE AND FPGA STATUS
	NOP
	call	clrw
	clr	a
	jb	CONF_DONE,bt0
	orl	a,#1
bt0:
	jnb	nCONFIG,bt1
	orl	a,#2
bt1:
	mov	wrd+MSIZ-3,a
	MOV	WRD+MSIZ-2,#BRDTYPH
	MOV	WRD+MSIZ-1,#BRDTYPL
	MOV	R1,#WRD
	MOV	r4,#6	;# chars
	JB	USEHEX,BTP1
	MOV	R4,#6
BTP1:	CALL	OUTNUM
	MOV	DPTR,#SEPARATOR
	CALL	SENDSTRING
	mov	a,#2
	ret

;INITIALIZE DSP FOR BOOT LOAD. CPARAM = 0: DOWNLOAD FROM HERE, USING DLOAD COMMAND
; CPARAM != 0: START SPI MASTER BOOT.
DLINIT:
	NOP
	MOV	A,cparam+MSIZ-1
	JZ	DSPDL
	;SET UP FOR FLASH MEMORY LOAD OF DSP:
	SETB	MOSI
	SETB	DSP_RST		;RESET DSP
	SETB	MBOOT		;SPI MASTER BOOT MODE
	;BE SURE THESE CHIP SELECTS ARE OFF:
	SETB	CS_MEM
	SETB	CS_DSP
	NOP
	NOP
	CLR	DSP_RST		;START THE BOOT LOAD
	JMP	DLINITX
	;SET UP FOR DIRECT LOAD OF DSP:
DSPDL:
	SETB	DSP_RST		;DSP RESET ON
	CLR	MBOOT		;DSP SPI SLAVE BOOT MODE
	SETB	CS_MEM
	SETB	CS_DSP
	NOP
	NOP
	NOP
	CLR	DSP_RST        ;DSP RESET OFF
DLINITX:
	MOV	A,#1
	RET

;DIRECT LOAD OF DSP. SEND MSIZ BYTES TO DSP VIA SPI. DSP MUST HAVE BEEN INITIALIZED FOR SPI
;  SLAVE BOOT.
DLWORD:
	NOP
	MOV	R0,#cparam+MSIZ-1
	MOV	R1,#MSIZ
	SETB	CS_MEM
	CLR	CS_DSP	;CHIP SEL. ON
DLOAD1:
	MOV	A,@R0
	CALL	SPI_TXRXR
	DEC	R0
	DJNZ	R1,DLOAD1
	SETB	CS_DSP	;CHIP SEL. OFF
	SETB	MOSI
	MOV	A,#1
	RET

;SEND BLOCKCOUNT BYTES TO DESTINATION. FLD3 = 0: DSP VIA SPI. DSP MUST HAVE BEEN INITIALIZED FOR SPI
;  SLAVE BOOT. FLD3 = 1: FPGA
DISPATCHBLOCK:
	NOP
	JNB	DWNRDY,DLB2	;IGNORE IF NO DOWNLOADED BLOCK
	CLR	DWNRDY		;CLEAR FLAG SO THAT THERE WILL NOT BE MULTIPLE DISPATCHES OF THE SAME BLOCK.
	MOV	DPTR,#DLBLOCK	;START OF DOWNLOADED BLOCK IN XRAM
	MOV	DPP,#0
	MOV	R1,BLOCKCOUNT   ;BYTE COUNT
	MOV	A,CPARAM+MSIZ-1	;DESTINATION
	JZ	DBDSP
	;SEND TO FPGA
DLB0:
	MOVX	A,@DPTR
	INC	DPTR
	MOV	R2,#8
DLB00:
	CLR	DCLK
;;	NOP
;;	NOP
;;	NOP
;;	NOP
	RRC	A		;LS BIT FIRST TO C
;;	NOP
;;	NOP
;;	NOP
	MOV	DATA0,C
	SETB	DCLK
	DJNZ	R2,DLB00
	CLR	DCLK
	DJNZ	R1,DLB0
	JB	nSTATUS,DLB2
      	MOV	A,#0		;ERROR- STATUS SHOULD BE 1.
      	JMP	DLB3
DBDSP:
	SETB	CS_MEM
	CLR	CS_DSP	;CHIP SEL. ON
DLB1:
	MOVX	A,@DPTR
	CALL	SPI_TXRXR
	INC	DPTR
	DJNZ	R1,DLB1
	SETB	CS_DSP	;CHIP SEL. OFF
	SETB	MOSI
DLB2:	MOV	A,#1
DLB3:	RET



;CURRENTLY,
;1) ENTIRE SECTOR IS ERASED EVEN IF STARTING ADDRESS IS NOT AT BEGINNING OF SECTOR.
;2) RESIDUAL BYTES ARE NOT STORED. THIS HAPPENS IF STARTING ADRS IS NOT DIVISIBLE BY 4.
FLASHLOAD:
	NOP
	;RESET DSP IN SPI SLAVE BOOT MODE. FLAG0 WILL BE HIGH AND DSP_RST WILL BE LOW.
	SETB	DSP_RST
	CLR	MBOOT         	;0 = SPI SLAVE BOOT, 1 = SPI MASTER BOOT
	NOP
	NOP
	NOP
	NOP
	NOP
	CLR	DSP_RST         ;DSP RESET MUST BE OFF, OR FLASH HARDWARE WRITE PROTECT IS ON.
	;SET UP FOR STORING CHARS IN XRAM:
	MOV	BLOCKCOUNT,#0
	MOV	BCKSUM,#0

;	CALL	RES	;GET ID. SHOULD BE 11H
;	CJNE	A,#011H,BADID
;	;ENABLE PROGRAMMING OF ALL SECS:
;	MOV	A,#00H
;	CALL	WRSR                    ;WRITE STATUS REGISTER
	;ERASE FIRST SECTOR:
	MOV	ADRS_HI,cparam+MSIZ-3
	MOV	ADRS_MID,cparam+MSIZ-2
	MOV	ADRS_LO,cparam+MSIZ-1   ;THIS IS PAGE ADDRESS
	JNB	FLAG0,BADID		;JMP IF FLAG0 IS 0
;;	CALL	SEC_ERASE               ;SECTOR ERASE
;;	MOV	FLASH_MODE,#1
	CLR	TWTS
	MOV	FLASH_MODE,#3
	CLR	FLASH_RDY
	JMP	FLOK
BADID:	CLR	A
        RET
FLOK:	MOV	A,#1
	RET

;INITIALIZE FOR FLASH LOAD OF DOWNLOAD BLOCKS
FLASHBLOCKINIT:
	NOP
	;RESET DSP IN SPI SLAVE BOOT MODE. FLAG0 WILL BE HIGH AND DSP_RST WILL BE LOW.
	SETB	DSP_RST
	CLR	MBOOT         	;0 = SPI SLAVE BOOT, 1 = SPI MASTER BOOT
	NOP
	NOP
	NOP
	NOP
	NOP
	CLR	DSP_RST         ;DSP RESET MUST BE OFF, OR FLASH HARDWARE WRITE PROTECT IS ON.
	;SET UP FOR STORING CHARS IN XRAM:
	;ERASE FIRST SECTOR:
	MOV	ADRS_HI,cparam+MSIZ-3
	MOV	ADRS_MID,cparam+MSIZ-2
	MOV	ADRS_LO,cparam+MSIZ-1   ;THIS IS PAGE ADDRESS
	CALL	SEC_ERASE               ;SECTOR ERASE
	MOV	A,#1
	RET


BITREVA:
	NOP
	;BIT REVERSE A
	RRC	A
	MOV	B.7,C
	RRC	A
	MOV	B.6,C
	RRC	A
	MOV	B.5,C
	RRC	A
	MOV	B.4,C
	RRC	A
	MOV	B.3,C
	RRC	A
	MOV	B.2,C
	RRC	A
	MOV	B.1,C
	RRC	A
	MOV	B.0,C
	MOV	A,B
	RET

FLASHREAD:
	NOP
	MOV	A,cparam
	JNZ	FR4
	;RESET DSP IN SPI SLAVE BOOT MODE. FLAG0 WILL BE HIGH AND DSP_RST WILL BE LOW.
	SETB	DSP_RST
	CLR	MBOOT         	;0 = SPI SLAVE BOOT, 1 = SPI MASTER BOOT
;	NOP
;	NOP
;	NOP
;	NOP
;	NOP
;	CLR	DSP_RST         ;DSP RESET MUST BE OFF, OR FLASH HARDWARE WRITE PROTECT IS ON.
	MOV	ADRS_HI,cparam+MSIZ-3
	MOV	ADRS_MID,cparam+MSIZ-2
	MOV	ADRS_LO,cparam+MSIZ-1
	CALL	READ_START
FR4:
	CALL	READR
;	CALL	BITREVA		;BYTES ARE STORED IN FLASH BACKWARD BY FLASHLOAD
	MOV	WRD+3,A
	CALL	READR
;	CALL	BITREVA
	MOV	WRD+2,A
	CALL	READR
;	CALL	BITREVA
	MOV	WRD+1,A
	CALL	READR
;	CALL	BITREVA
	MOV	WRD,A
	MOV	R1,#WRD
	MOV	R4,#10
	CALL	OUTNUM
	MOV	DPTR,#SEPARATOR
	CALL	SENDSTRING
	MOV	A,#2
	RET

FLASHREADEND:
	NOP
	CALL	READ_END
	SETB	DSP_RST
	SETB	MBOOT         	;0 = SPI SLAVE BOOT, 1 = SPI MASTER BOOT
	NOP
	NOP
	NOP
	NOP
	NOP
	CLR	DSP_RST		;DSP RESET OFF
	MOV	A,#1
	RET



FPGAFLASHLOAD:
	NOP
	MOV	ADRS_HI,cparam+MSIZ-3
	MOV	ADRS_MID,cparam+MSIZ-2
	MOV	ADRS_LO,cparam+MSIZ-1
	CALL	FPGA_BOOT
	MOV	A,#1
	RET

;START OF FPGA LOAD
FPGASTRT:
	NOP
;;	MOV	A,#1
;;	SETB	nSTATUS
	CLR	nCONFIG
	CLR	DCLK
	MOV	R0,#4
	DJNZ	R0,$		;2 MICROSEC WAIT
	JB	nSTATUS,NOCHIP	;nSTATUS SHOULD BE 0 HERE
	SETB	nCONFIG
	MOV	R0,#8
	DJNZ	R0,$		;4 MICROSEC WAIT
	JB	nSTATUS,STRTOK	;nSTATUS SHOULD BE 1 HERE
NOCHIP:	DEC	A		;ERROR
STRTOK:
	RET

;LOAD A GROUP OF BYTES INTO FPGA.
FPGALOAD:
	NOP
	MOV	R1,#MSIZ        	;NBR OF BYTES
	MOV	R0,#cparam+MSIZ-1       ;ADRS OF LSB
FPLD0:
	MOV	A,@R0
	DEC	R0
	MOV	R2,#8
FPLD1:
	CLR	DCLK
	RRC	A		;LS BIT FIRST TO C
	MOV	DATA0,C
	SETB	DCLK
	DJNZ	R2,FPLD1
	CLR	DCLK
	DJNZ	R1,FPLD0
	MOV	A,#1
	JB	nSTATUS,FPLD2
      	DEC	A		;ERROR- STATUS SHOULD BE 1.
FPLD2:
	RET


;FLASH MEMORY STATUS. 1 = BUSY
FLASHSTAT:
	NOP
        ;SEE IF FLASH MEMORY IS BUSY:
        ;DON'T ACCESS FLASH IF PAGE LOADING IS IN PROGRESS:
	call	clrw  	;CLEAR WRD
;;        MOV	A,FLASH_MODE
;;        CJNE	A,#2,FS1
;;        MOV	A,#0;1 ;0
;;        JMP	FS2
FS1:    CALL	RDSR            ;READ STATUS REGISTER
        JB	ACC.0,FS0
        SETB	FLASH_RDY
FS0:    ;ANL	A,#1
FS2:	MOV	R4,#3
	MOV	WRD+MSIZ-1,A
	MOV	R1,#WRD
	CALL	OUTNUM
	MOV	DPTR,#SEPARATOR
	CALL	SENDSTRING
	MOV	A,#2
	RET

FLASHMODE:
	NOP
	call	clrw
	MOV	A,FLASH_MODE
	MOV	R4,#1
	MOV	WRD+MSIZ-1,A
	MOV	R1,#WRD
	CALL	OUTNUM
	MOV	DPTR,#SEPARATOR
	CALL	SENDSTRING
	MOV	A,#2
	RET


;READ BACK FWORD
RDBK:
	NOP
	MOV	R1,#WRD
	MOV	R4,#10
	CALL	OUTNUM
	MOV	DPTR,#SEPARATOR
	CALL	SENDSTRING
	MOV	A,#2
	RET

;ONE WORD (4 BYTES) HAS BEEN SENT TO BE LOADED INTO FLASH MEMORY
FWORD:
	NOP
	MOV	R0,#CPARAM
	MOV	R1,#WRD
	MOV	R2,#MSIZ
LW00:
	MOV	A,@R0
	MOV	@R1,A
	INC	R0
	INC	R1
	DJNZ	R2,LW00

	MOV	R2,ADRS_LO		;ADRS_LO = PAGE ADRS.
	MOV	R1,#MSIZ        	;NBR OF BYTES
	MOV	R0,#cparam+MSIZ-1       ;ADRS OF LSB
	MOV	A,FLASH_MODE
	JNZ	FW01
	JMP	PAPBD		;JMP IF 0 IDLE
FW01:	DEC	A
	JZ	PAP0	        ;JMP IF 1 READY TO LOAD PAGE
	DEC	A
	JZ	PAP1		;JMP IF 2 LOADING PAGE
	;ASSUME MODE 3:
	;ERASE SECTOR:
	CALL	SEC_ERASE
	MOV	FLASH_MODE,#1
	CLR	FLASH_RDY
	;SAVE BYTES:
	MOV	R1,#cparam
	MOV	R0,#FLASHM
	MOV	R2,#MSIZ
FSB:	MOV	A,@R1
	MOV	@R0,A
	INC	R0
	INC	R1
	DJNZ	R2,FSB
	SETB	TWTS
	JMP	PAPX

PAP0:	;SECTOR JUST ERASED OR PAGE JUST PROGRAMMED.
	JNB	TWTS,PAP
	;TWO WORDS TO SAVE:
	MOV	R1,#2*MSIZ
	MOV	R0,#FLASHM+MSIZ-1       ;ADRS OF LSB
	CLR	TWTS
PAP:
;	;INITIALIZE FOR PROGRAMMING A PAGE:
;	CALL	PP_START
	MOV	BLOCKCOUNT,#0
	MOV	BCKSUM,#0
	MOV	PGSTRT,ADRS_HI
	MOV	PGSTRT+1,ADRS_MID
	MOV	PGSTRT+2,ADRS_LO
PAP1:
	;SET XRAM POINTER:
	MOV	DPTR,#DLBLOCK
	MOV	DPL,BLOCKCOUNT
	MOV	DPP,#0
PAP1L:
	MOV	A,@R0
;	CALL	SPI_TXRXRU		;DSP EXPECTS LEAST SIG. BIT FIRST
	MOVX	@DPTR,A
	ADD	A,BCKSUM
	MOV	BCKSUM,A
	INC	DPTR
	INC	BLOCKCOUNT

	DEC	R0
	INC	R2
	MOV	A,R2
	JZ	FULP		;JUMP IF FULL PAGE
	DJNZ	R1,PAP1L
	;NOT A FULL PAGE YET
	MOV	FLASH_MODE,#2	;LOADING A PAGE
	MOV	ADRS_LO,R2
	JMP	PAPX
FULP:
	;A PAGE HAS BEEN FILLED
;        CALL	PP_END          ;SAVE PAGE
	CALL	PROGRAM_PAGE

        MOV	ADRS_LO,R2
	INC	ADRS_MID
	MOV	A,ADRS_MID
	JNZ	PAP3
        INC	ADRS_HI
PAP3:
	;SEE IF WE HAVE CROSSED A SECTOR BOUNDARY:
	MOV	A,SEC_GRAN
	JZ	SG64
	DEC	A
	JZ	SG32
	;ASSUME 4K SECTOR SIZE
	MOV	A,ADRS_MID
	ANL	A,#0FH
	JZ	PAP4
	JMP	PAP2
SG64:
	MOV	A,ADRS_MID
	JZ	PAP4
	JMP	PAP2
SG32:
	MOV	A,ADRS_MID
	ANL	A,#7FH
	JNZ	PAP2
PAP4:
        MOV	FLASH_MODE,#3	;LAST PAGE OF SECTOR IS BEING PROGRAMMED
        CLR	FLASH_RDY
        JMP	PAPX
PAP2:
	MOV	FLASH_MODE,#1   ;PAGE IS BEING PROGRAMMED
	CLR	FLASH_RDY
	;WAIT FOR PAGE TO BE PROGRAMMED:
PAPX:
	MOV	A,#1
	JMP	PAPR
PAPBD:
	MOV	A,#0
PAPR:
	RET

FLASHTERM:
	NOP
;	CALL	PP_END
	CALL	PROGRAM_PAGE
	MOV	FLASH_MODE,#0
;;	CALL	WRDI	;WRITE DISABLE
;	MOV	A,#080H
;	CALL	WRSR  	;SET WRITE PROTECT BIT
	SETB	DSP_RST
	SETB	MBOOT         	;0 = SPI SLAVE BOOT, 1 = SPI MASTER BOOT
	NOP
	NOP
	NOP
	NOP
	NOP
	CLR	DSP_RST		;DSP RESET OFF
	MOV	A,#1
	RET

;SEND DATA TO DSP
SPIXD:
	NOP
	MOV	A,#3
	JNB	MS5,XD2		;JMP IF DSP IS USING SPI
	MOV	R0,#cparam+MSIZ-1
	MOV	R1,#MSIZ
	SETB	CS_MEM
	CLR	CS_DSP	;CHIP SEL. ON
XD1:
	MOV	A,@R0
	CALL	SPI_TXRXR
	DEC	R0
	DJNZ	R1,XD1
	SETB	CS_DSP	;CHIP SEL. OFF
	SETB	MOSI
	MOV	A,#1
XD2:	RET

;RECEIVE A CHARACTER FROM DSP AND SEND IT OUT THE UART
SPIRXTX:
	NOP
	CLR	A	;SEND A ZERO TO DSP
	SETB	CS_MEM
	CLR	CS_DSP	;CHIP SEL. ON
	CALL	SPI_TXRXR
	SETB	CS_DSP	;CHIP SEL. OFF
	SETB	MOSI
	CALL	SENDCHAR
	RET

;RECEIVE DATA FROM DSP
SPIRCV:
	NOP
	MOV	A,#3
	JNB	MS5,RCVX		;JMP IF DSP IS USING SPI.
	MOV	R0,#cparam+MSIZ-1
	MOV	R1,#WRD+MSIZ-1
	MOV	R2,#MSIZ
	SETB	CS_MEM
	CLR	CS_DSP	;CHIP SEL. ON
RCV1:
	MOV	A,@R0
	CALL	SPI_TXRXR
	MOV	@R1,A
	DEC	R0
	DEC	R1
	DJNZ	R2,RCV1
	SETB	CS_DSP	;CHIP SEL. OFF
	SETB	MOSI
	MOV	R1,#WRD
	MOV	R4,#10
	CALL	OUTNUM
	MOV	DPTR,#SEPARATOR
	CALL	SENDSTRING
	MOV	A,#2
RCVX:	RET

;RETURN SPI STATUS: 1 = BUSY, 0 = NOT BUSY
SPIBSY:
	NOP
	call	clrw  	;CLEAR WRD
	MOV	R4,#1
	MOV	R1,#WRD
	CLR	A
	JB	MS5,FREE
	INC	A
FREE:
	MOV	WRD+MSIZ-1,A
	CALL	OUTNUM
	MOV	DPTR,#SEPARATOR
	CALL	SENDSTRING
	MOV	A,#2
	RET


blinit:
	NOP
	;RESET DSP IN SPI SLAVE BOOT MODE. FLAG0 WILL BE HIGH AND DSP_RST WILL BE LOW.
	SETB	DSP_RST
	CLR	MBOOT         	;0 = SPI SLAVE BOOT, 1 = SPI MASTER BOOT
	NOP
	NOP
	NOP
	NOP
	NOP
	CLR	DSP_RST         ;DSP RESET MUST BE OFF, OR FLASH HARDWARE WRITE PROTECT IS ON.
;	;ENABLE PROGRAMMING OF ALL SECS:
;	MOV	A,#00H
;	CALL	WRSR                    ;WRITE STATUS REGISTER
	MOV	ADRS_HI,cparam+MSIZ-3
	MOV	ADRS_MID,cparam+MSIZ-2
	MOV	ADRS_LO,cparam+MSIZ-1   ;THIS IS PAGE ADDRESS
	MOV	A,#1
	RET


download_block:
	MOV	BCKSUM,#0
	MOV	BLOCKCOUNT,#0
	MOV	BLOCKLENGTH,cparam+MSIZ-1	;LENGTH OF BLOCK. 0 = 256
	SETB	DLMODE
	SETB	DWNRDY	 			;INDICATE A BLOCK HAS BEEN DOWNLOADED INTO XRAM
	RET

dwnload:
	mov	a,#'>'
	call	sendchar
        JNB     TI,$            ; wait until present char gone
	;RESET DSP IN SPI SLAVE BOOT MODE. FLAG0 WILL BE HIGH AND DSP_RST WILL BE LOW.
	SETB	DSP_RST
	CLR	MBOOT         	;0 = SPI SLAVE BOOT, 1 = SPI MASTER BOOT
	ljmp	downloader
	ret	;should never get here


CMDTBL:
		db	'version',0    	;0
		db	'dsp',0		;1
		db	'dsbl',0	;2
		db	'x',0		;3
		db	'fl',0		;4
		db	'fs',0		;5
		db	'y',0		;6
		db	'fm',0		;7
		db	'ft',0		;8
		db	'spid',0	;9
		db	'brdtyp',0	;10
		db	'spircv',0	;11
		db	'fpgaflsh',0	;12
		db	'fpgastrt',0	;13
		db	'fpgaload',0	;14
		db	'fread',0	;15
		db	'freadend',0	;16
		db	'blinit',0	;17
		db	'rdbk',0	;18
		db	'vfy',0		;19
		db	'dlb',0		;20
		db	'fbi',0		;21
		db	'fbs',0		;22
		db	'serase',0	;23
		db	'dbds',0	;24
		db	'spibsy',0	;25
		db	'dwnload',0	;26
		db	'reset',0	;27
		db	0  		;end of CMDTBL

JMPTBL:
		dw	version            	;0
		dw	dsp	   		;1
		dw	dlinit			;2
		dw	dlword   		;3
		dw	flashload		;4
		dw	flashstat		;5
		dw	fword			;6
		dw	flashmode		;7
		dw	flashterm		;8
		dw 	spixd			;9
		dw	brdtyp			;10
		dw	spircv			;11
		dw	fpgaflashload		;12
		dw	fpgastrt		;13
		dw	fpgaload		;14
		dw	flashread		;15
		dw	flashreadend		;16
		dw	blinit			;17
		dw	rdbk			;18
		dw	verify_page		;19
		dw	download_block		;20
		dw	flashblockinit		;21
		dw	flashblock		;22
		dw	erasesector		;23
		dw	dispatchblock		;24
		dw	spibsy			;25
		dw	dwnload			;26
		dw	wdog_test		;27
;end of JMPTBL

;** BE SURE TO UPDATE MAXTBL IF NUMBER OF COMMANDS CHANGES:
MAXTBL	EQU	27

;32 MBIT FLASH ROUTINES:

RES:	;READ SIGNATURE (SHOULD BE 11H). RETURN IN A
	NOP
	SETB	CS_DSP
	CLR	CS_MEM
	MOV	A,#0ABH         ;"RESUME FROM DEEP POWER-DOWN"
	CALL	SPI_TXRX        ;COMMAND
	CALL	SPI_TXRX        ;DUMMY 1
	CALL	SPI_TXRX        ;DUMMY 2
	CALL	SPI_TXRX        ;DUMMY 3
	CALL	SPI_TXRX	;READ BYTE
	SETB	CS_MEM
	SETB	MOSI
	MOV	A,#11H  	;FORCE EXPECTED RESPONSE
	RET

RDID:	;READ MANUFACTURER ID
	NOP
	SETB	CS_DSP
	CLR	CS_MEM
	MOV	A,#09FH
	CALL	SPI_TXRX        ;COMMAND
	CALL	SPI_TXRX	;READ BYTE
	CALL	SPI_TXRX	;READ BYTE
	CALL	SPI_TXRX	;READ BYTE
	SETB	CS_MEM
	SETB	MOSI
	RET

WREN:	;WRITE ENABLE
	NOP
	SETB	CS_DSP
	CLR	CS_MEM
	MOV	A,#06H
	CALL	SPI_TXRX
	SETB	CS_MEM
	SETB	MOSI
	RET

WRDI:	;WRITE DISABLE
	NOP
	SETB	CS_DSP
	CLR	CS_MEM
	MOV	A,#04H
	CALL	SPI_TXRX
	SETB	CS_MEM
	SETB	MOSI
	RET

RDSR:	;READ STATUS REGISTER. RETURN WITH A = STATUS.
	NOP
	SETB	CS_DSP
	CLR	CS_MEM
	MOV	A,#05H
	CALL	SPI_TXRX
	CALL	SPI_TXRX
	SETB	CS_MEM
	SETB	MOSI
	RET

WRSR:	;WRITE A TO STATUS REGISTER
	NOP
	PUSH	ACC
	CALL	WREN
	SETB	CS_DSP
	CLR	CS_MEM
	MOV	A,#01H
	CALL	SPI_TXRX
	POP	ACC
	CALL	SPI_TXRX
	SETB	CS_MEM
	SETB	MOSI
	RET

READ_START:
	NOP
	SETB	CS_DSP
	CLR	CS_MEM
	MOV	A,#03H
	CALL	SPI_TXRX
	MOV	A,ADRS_HI
	CALL	SPI_TXRX
	MOV	A,ADRS_MID
	CALL	SPI_TXRX
	MOV	A,ADRS_LO
	CALL	SPI_TXRX
;	CALL	SPI_TXRX        ;A = MEMORY BYTE
	RET

;READ:
;	NOP
;	CLR	A
;	CALL	SPI_TXRX
;	RET

READ:   ;READ A BYTE FROM SPI- MSB FIRST
	NOP
	CLR	A
;	SETB	SCLOCK
	SPI_BIT	7
	SPI_BIT	6
	SPI_BIT	5
	SPI_BIT	4
	SPI_BIT	3
	SPI_BIT	2
	SPI_BIT	1
	SPI_BIT	0
	RET

READR:   ;READ A BYTE FROM SPI- LSB FIRST
	NOP
	CLR	A
;	SETB	SCLOCK
	SPI_BIT	0
	SPI_BIT	1
	SPI_BIT	2
	SPI_BIT	3
	SPI_BIT	4
	SPI_BIT	5
	SPI_BIT	6
	SPI_BIT	7
	RET

READ_END:
	NOP
	SETB	CS_MEM
	SETB	MOSI
	RET

;RETURN WITH A=0 IF PAGE FROM ADRS_HI.ADRS_MID.ADRS_LO TO END OF PAGE IS ERASED, NON-ZERO OTHERWISE.
IS_ERASED:
	NOP
	CALL	READ_START
	MOV	R0,ADRS_LO	;RO = STARTING PAGE ADDRESS
ISE00:
	CALL	READ
	CJNE	A,#0FFH,ISE01	;FF IS ERASED CONDITION
	INC	R0
	CJNE	R0,#0,ISE00  	;JMP IF NOT END OF PAGE
ISE01:
	CALL	READ_END
	INC	A
	RET

PP_START:      	;PAGE PROGRAM
	NOP
	MOV	A,#0
	CALL	WRSR	;CLEAR SPRL BIT
	MOV	A,#0
	CALL	WRSR	;CLEAR SEC. PROTECTION BITS
	CALL	WREN
	SETB	CS_DSP
	CLR	CS_MEM
	MOV	A,#02H
	CALL	SPI_TXRX
	MOV	A,ADRS_HI
	CALL	SPI_TXRX
	MOV	A,ADRS_MID
	CALL	SPI_TXRX
	MOV	A,ADRS_LO
	CALL	SPI_TXRX
	RET

PP:
	NOP
	CALL	SPI_TXRX
	RET

PP_END:
	NOP
	SETB	CS_MEM
	SETB	MOSI
	RET

SEC_ERASE:
	NOP
	;SECTOR ERASE
	MOV	A,#0
	CALL	WRSR	;CLEAR SPRL BIT
	MOV	A,#0
	CALL	WRSR	;CLEAR SEC. PROTECTION BITS
	CALL	WREN
	MOV	A,SEC_GRAN
	JZ	E64
	DEC	A
	JZ	E32
	;ASSUME 4K SECTOR
	MOV	A,#20H  	;ERASE 4K BLOCK
	JMP	SER1
E64:	MOV	A,#0D8H 	;ERASE 64K BLOCK
	JMP	SER1
E32:	MOV	A,#52H 		;ERASE 32K BLOCK
SER1:
	SETB	CS_DSP
	CLR	CS_MEM
	CALL	SPI_TXRX
	;ADDRESS IS WITHIN DESIRED SECTOR:
	MOV	A,ADRS_HI
	CALL	SPI_TXRX
	MOV	A,ADRS_MID
	CALL	SPI_TXRX
	MOV	A,ADRS_LO
	CALL	SPI_TXRX
	SETB	CS_MEM
	SETB	MOSI
	RET

BE:	;BULK ERASE
	NOP
	CALL	WREN
	SETB	CS_DSP
	CLR	CS_MEM
	MOV	A,#0C7H
	CALL	SPI_TXRX
	SETB	CS_MEM
	SETB	MOSI
	RET

DP:	;DEEP POWER DOWN
	NOP
	SETB	CS_DSP
	CLR	CS_MEM
	MOV	A,#0B9H
	CALL	SPI_TXRX
	SETB	CS_MEM
	SETB	MOSI
	RET

DCLKOFF:
	NOP
	PUSH	B
	MOV	B,#2
	DJNZ	B,$
	POP	B
	CLR	DCLK
	RET

DCLKON:
	NOP
	PUSH	B
	MOV	B,#2
	DJNZ	B,$
	POP	B
	SETB	DCLK
	RET



;LOAD FPGA FROM FLASH. STARTING ADDRESS IS ADRS_HI.ADRS_MID.ADRS_LO
FPGA_BOOT:
	NOP
	;THIS SHOULD LOAD UP TO 718569 BYTES (0AF6E9H)
	;LOAD IN GROUPS OF 16 BYTES
	;MAX GROUP COUNT = 0AF6FH:
	MOV	R3,#0AFH
	MOV	R4,#06FH
	;RESET DSP IN SPI SLAVE BOOT MODE. FLAG0 WILL BE HIGH AND DSP_RST WILL BE LOW.
	SETB	DSP_RST
	CLR	MBOOT         	;0 = SPI SLAVE BOOT, 1 = SPI MASTER BOOT
	NOP
	NOP
	NOP
	NOP
	NOP
	CLR	DSP_RST         ;DSP RESET MUST BE OFF, OR FLASH HARDWARE WRITE PROTECT IS ON.
	CALL	FPGASTRT
	JZ	FPGEX		;JMP IF ERROR
	CLR	MOSI 		;
	CALL	READ_START
FPG1:
	MOV	R1,#128          ;BIT COUNT FOR 16 BYTES
LDBIT:
	;READ A BIT FROM FLASH MEM. AND LOAD IT INTO FPGA
	LOAD_FPGA_BIT
	DJNZ	R1,LDBIT
	JNB	nSTATUS,FPGBD
	;BYTE COUNT CHECK:
	;SUBTRACT 1 FROM R3.R4
	MOV	A,R4
	CLR	C
	SUBB	A,#1
	MOV	R4,A
	MOV	A,R3
	SUBB	A,#0
	MOV	R3,A
	JNC	FPG3

	;TOO MANY BYTES- STOP
FPGBD:
	JB	CONF_DONE,FPG4
	JMP	FPGEX
FPG3:	JNB	CONF_DONE,FPG1
	;CONFIGURATION COMPLETED, 10 MORE CLOCKS REQUIRED
FPG4:	MOV	R0,#FLASHM+3
	MOV	R1,#1
	CALL	FPLD0
FPGEX:
	CALL	READ_END
	RET

IF	(DEBUG)
FLASHTEST:
	;2MB FLASH TESTS:
	CLR	MBOOT         	;OUTPUT TO DSP. 0 = SPI SLAVE BOOT, 1 = SPI MASTER BOOT
	CLR	DSP_RST
cr:	;CALL	RES	;GET ID. SHOULD BE 11H
;	cjne	a,#11h,cr1
;	jmp	cr
cr1:
	CALL	RDSR
	jmp	rbp 	;BYPASS FLASH PROGRAMMING
	;ENABLE PROGRAMMING OF SEC 0,1:
	;00H:SECS 0,1,2,3 04H: SECS 0,1,2 08H: SECS 0,1 0CH: NO SECS
;	MOV	A,#00H
;	CALL	WRSR
;	CALL	RDSR
	;ERASE FIRST SECTOR:
	MOV	ADRS_HI,#0	;0:SEC 0, 1: SEC 1, 2:SEC 2, 3: SEC 3
	MOV	SEC_GRAN,#0	;64K SECTOR SIZE
	CALL	SEC_ERASE
	;WAIT FOR COMPLETION:
SEWAIT:
	CALL	RDSR
	JB	ACC.0,SEWAIT
;	jmp	rbp
	;PROGRAM PAGE 0:
	MOV	ADRS_HI,#0
	MOV	ADRS_MID,#0
	MOV	ADRS_LO,#0
	MOV	R0,#0FFH
	CALL	PP_START
PP_LOOP:
	MOV	A,R0
;	mov	a,#5ah
	CALL	PP
	DJNZ	R0,PP_LOOP
	CALL	PP_END
	;WAIT FOR PAGE TO BE PROGRAMMED:
PPWAIT:
	CALL	RDSR
	JB	ACC.0,PPWAIT
	CALL	WRDI	;WRITE DISABLE
;	MOV	A,#080H
;	CALL	WRSR  	;SET WRITE PROTECT BIT
	;READ BACK PAGE:
rbp:	MOV	ADRS_HI,#0
	MOV	ADRS_MID,#0
	MOV	ADRS_LO,#0

;	mov	adrs_hi,#00
;	mov	adrs_mid,#6fh
;	mov	adrs_lo,#08h
	MOV	R0,#0FFH
	CALL	READ_START
RDLOOP:
	CALL	READ
	CLR	C
	SUBB	A,R0
	JNZ	RDERR
	DJNZ	R0,RDLOOP
	CALL	READ_END
	JMP	WAIT
RDERR:	JMP	RDLOOP ;$

PAGE0:
;0x00000000
	DB	0,0,0,0
;0x00000000
	DB	0,0,0,0
;0x00000000
	DB	0,0,0,0
;0x00000000
	DB	0,0,0,0
;0x00000000
	DB	0,0,0,0
;0x00000000
	DB	0,0,0,0
;0x00000000
	DB	0,0,0,0
;0x00c90000
	DB	0,0,0C9H,0
;0x06be0408*
	DB	08H,04H,0BEH,06H
;0x00000000
	DB	0,0,0,0
;0x00000f7b
	DB	07BH,0FH,0,0
;0x0f7a0000
	DB	0,0,7AH,0FH
;0x00001000
	DB	0,10H,0,0
;0x00001402
	DB	02H,14H,0,0
;0x0f300000
	DB	0,0,30H,0FH
;0x00000000
	DB	0,0,0,0
;0x00000f34
	DB	34H,0FH,0,0
;0x0f380000
	DB	0,0,38H,0FH
;0x00000000
	DB	0,0,0,0
;0x00000f3c
	DB	3CH,0FH,0,0
;0x0f3f0000
	DB	0,0,3FH,0FH
;0x00000000
	DB	0,0,0,0
;0x00010f25
	DB	25H,0FH,1,0
;0x0f260000
	DB	0,0,26H,0FH
;0x00000000
	DB	0,0,0,0
;0x00010f2d
	DB	2DH,0FH,1,0
;0x0f2e0000
	DB	0,0,2EH,0FH
;0x00030024
	DB	24H,0,3,0
;0x1cb0100b
	DB	0BH,10H,0B0H,1CH
;0x013e0002
	DB	2,0,3EH,1
;0x00030024
	DB	24H,0,3,0
;0x00310f14
	DB	14H,0FH,31H,0
;0x06be0008
	DB	8,0,0BEH,6
;0x00080003
	DB	3,0,8,0
;0x00041008
	DB	8,10H,4,0
;0x10020008*
	DB	8,0,2,10H
;0x00080005
	DB	5,0,8,0
;0x10801003
	DB	3,10H,80H,10H
;0x013e0002
	DB	2,0,3EH,1
;0x000800b4
	DB	0B4H,0,8,0
;0xa0000600
	DB	0,6,0,0A0H
;0x013e0002
	DB	2,0,3EH,1
;0x00080040
	DB	40H,0,8,0
;0xa0000600
	DB	0,6,0,0A0H
;0x013e0002
	DB	2,0,3EH,1
;0x00080046
	DB	46H,0,8,0
;0xa0000600
	DB	0,6,0,0A0H
;0x013e0002
	DB	2,0,3EH,1
;0x0008004b
	DB	4BH,0,8,0
;0xa0000600
	DB	0,6,0,0A0H
;0x013e0002
	DB	2,0,3EH,1
;0x0008005e
	DB	5EH,0,8,0
;0xa0000600
	DB	0,6,0,0A0H
;0x013e0002
	DB	2,0,3EH,1
;0x00080065
	DB	65H,0,8,0
;0xa0000600
	DB	0,6,0,0A0H
;0x013e0002
	DB	2,0,3EH,1
;0x00080076
	DB	76H,0,8,0
;0xa0000600
	DB	0,6,0,0A0H
;0x013e0002
	DB	2,0,3EH,1
;0x0008007f
	DB	7FH,0,8,0
;0xa0000600
	DB	0,6,0,0A0H
;0x013e0002
	DB	2,0,3EH,1
;0x00080083
	DB	83H,0,8,0


ENDIF

PROGRAM_PAGE:
	NOP
;	CALL	PP_START
	CRON
	MOV	A,#0
	CALL	WRSR	;CLEAR SPRL BIT
	MOV	A,#0
	CALL	WRSR	;CLEAR SEC. PROTECTION BITS
	CALL	WREN
	SETB	CS_DSP
	CLR	CS_MEM
	MOV	A,#02H
	CALL	SPI_TXRX
	MOV	A,PGSTRT
	CALL	SPI_TXRX
	MOV	A,PGSTRT+1
	CALL	SPI_TXRX
	MOV	A,PGSTRT+2
	CALL	SPI_TXRX

	MOV	R0,BLOCKCOUNT
	MOV	DPTR,#DLBLOCK
	MOV	DPP,#0
PGPG00:
	MOVX	A,@DPTR
	INC	DPTR
	CALL	SPI_TXRXR
	DJNZ	R0,PGPG00
	SETB	CS_MEM
	SETB	MOSI
	CROF
	RET
;PROGRAM DOWNLOADED BLOCK INTO FLASH. RETURN 1 IF OK TO PGM A FOLLOWING BLOCK.
;RETURN 3 IF SECTOR MUST BE ERASED BEFORE PROGRAMMING NEXT BLOCK;
flashblock:
	NOP
	CRON
	MOV	A,#0
	CALL	WRSR	;CLEAR SPRL BIT
	MOV	A,#0
	CALL	WRSR	;CLEAR SEC. PROTECTION BITS
	CALL	WREN
	SETB	CS_DSP
	CLR	CS_MEM
	MOV	A,#02H
	CALL	SPI_TXRX
	MOV	A,ADRS_HI
	CALL	SPI_TXRX
	MOV	A,ADRS_MID
	CALL	SPI_TXRX
	MOV	A,ADRS_LO
	CALL	SPI_TXRX

	MOV	R0,BLOCKCOUNT
	MOV	DPTR,#DLBLOCK
	MOV	DPP,#0
FBLK00:
	MOVX	A,@DPTR
	INC	DPTR
	CALL	SPI_TXRXR
	DJNZ	R0,FBLK00
	SETB	CS_MEM		;START THE PROGRAMMING PROCESS
	SETB	MOSI
	CROF

        MOV	ADRS_LO,#0
	INC	ADRS_MID
	MOV	A,ADRS_MID
	JNZ	FBL3
        INC	ADRS_HI
FBL3:
	;SEE IF WE HAVE CROSSED A SECTOR BOUNDARY:
	MOV	A,SEC_GRAN
	JZ	FBL64
	DEC	A
	JZ	FBL32
	;ASSUME 4K SECTOR SIZE
	MOV	A,ADRS_MID
	ANL	A,#0FH
	JZ	FBL4
	JMP	FBL2
FBL64:
	MOV	A,ADRS_MID
	JZ	FBL4
	JMP	FBL2
FBL32:
	MOV	A,ADRS_MID
	ANL	A,#7FH
	JNZ	FBL2
FBL4:
        MOV	A,#3	;LAST PAGE OF SECTOR IS BEING PROGRAMMED
        JMP	FBLX
FBL2:
	MOV	A,#1
FBLX:
	call	clrw  	;CLEAR WRD
	MOV	R4,#3
	MOV	WRD+MSIZ-1,A
	MOV	R1,#WRD
	CALL	OUTNUM
	MOV	DPTR,#SEPARATOR
	CALL	SENDSTRING
	MOV	A,#2
	RET

erasesector:
	CALL	SEC_ERASE
	MOV	A,#1
	RET

;SEND 1 IF FLASH PAGE OK
VERIFY_PAGE:
	NOP
	SETB	CS_DSP
	CLR	CS_MEM
	MOV	A,#03H
	CALL	SPI_TXRX
	MOV	A,PGSTRT
	CALL	SPI_TXRX
	MOV	A,PGSTRT+1
	CALL	SPI_TXRX
	MOV	A,PGSTRT+2
	CALL	SPI_TXRX

	MOV	R0,BLOCKCOUNT
	MOV	DPTR,#DLBLOCK
	MOV	DPP,#0
VPG00:
	MOVX	A,@DPTR
	INC	DPTR
	MOV	B,A
	CALL	READR
;	CALL	BITREVA
	CLR	C
	SUBB	A,B
	JNZ	VPBD
	DJNZ	R0,VPG00
VPBD:
	inc a
	SETB	CS_MEM
	SETB	MOSI
	call	clrw  	;CLEAR WRD
	MOV	R4,#3
	MOV	WRD+MSIZ-1,A
	MOV	R1,#WRD
	CALL	OUTNUM
	MOV	DPTR,#SEPARATOR
	CALL	SENDSTRING
	MOV	A,#2
	RET

$INCLUDE(dwnldr.asm)

END

