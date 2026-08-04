;********************************************************************
;
; Author        : ADI - Apps            www.analog.com/MicroConverter
;
; Date          : 12 October 1999
;
; File          : UARTIO.hex
;
; Hardware      : any 8051 based microcontroller or MicroConverter
;
; Description   : standard UART I/O subroutines.  total size of this
;                 code when assembled is 155 bytes.  routines for use
;                 external to this file are:
;
;                 SENDSTRING - sends a string of characters
;                 SENDCHAR   - sends a single character
;                 ASCII2HEX  - converts from ASCII to HEX
;********************************************************************

;____________________________________________________________________
                                                         ; SENDSTRING

SENDSTRING:     ; sends string to UART starting at location
                ; DPTR and ending with a null (0) value
        NOP
        PUSH    ACC
        PUSH    B
        CLR     A
        MOV     B,A
IO0010: MOV     A,B
        INC     B
        MOVC    A,@A+DPTR
        JZ      IO0020
        CALL    SENDCHAR
        JMP     IO0010
IO0020: POP     B
        POP     ACC

        RET

;____________________________________________________________________
                                                           ; SENDCHAR

SENDCHAR:       ; sends A to UART
	IF	(DEBUG=0)
        PUSH	ACC
        MOV	A,#200		;TIMEOUT VAL
SCHR0:
        JB	TXRDY,SCHR1
      	DJNZ	ACC,SCHR0
SCHR1:
        MOV	A,#200		;TIMEOUT VAL
SCHR2:
        JNB	CTS,SCHR3
      	DJNZ	ACC,SCHR2
SCHR3:
	POP	ACC
        CLR     TXRDY		;WILL BE SET BY TX COMPLETE INTERRUPT
        MOV     SBUF,A
	ENDIF
        RET


ASCII2HEX:      ; converts A from an ASCII digit ('0'-'9' or 'A'-'F')
                ; into the corresponding number (0-15).  returns C=1
                ; when input is other than an ASCII digit,
                ; indicating invalid output (returned as 255).

        CLR     C
        SUBB    A,#'0'
        CJNE    A,#10,$+3
        JC      IO0050          ; if '0'<=char<='9', return OK
        CJNE    A,#17,$+3
        JC      IO0040          ; if '9'<char<'A', return FAIL
        SUBB    A,#7
        CJNE    A,#10h,$+3
        JC      IO0050          ; if 'A'<=char<='F', return OK
        CJNE    A,#42,$+3
        JC      IO0040          ; if 'F'<char<'a', return FAIL
        SUBB    A,#20h
        CJNE    A,#10h,$+3
        JC      IO0050          ; if 'a'<=char<='f', return OK..

IO0040: CLR     C               ; ..else return FAIL
        MOV     A,#0FFh

IO0050: CPL     C
        RET

