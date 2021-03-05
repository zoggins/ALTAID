;----------------------------------------------------------------------- 
;
; Bootstrap loader for Altair BASIC 1.0 (88-2SIO Version)
;
;     This loader loads BASIC 1.0 through an 88-2SIO at I/O address
;     10h/11h (default address for the 1st port on a 2SIO)
;
;	1) Enter this bootstrap loader at 1800h (014000 octal).
;
;	2) Examine 1800h (014000 octal) on the front panel.
;
;	3) Position the tape in the NULL leader and start
;	   the tape reader.
;
;	4) Immediately depress RUN on the front panel.
;
;	5) After the tape finishes reading, STOP-RESET the machine
;	   and then depress RUN. The BASIC initialization dialog is
;	   then displayed on the console.
;
;  Version      Date   		Description
;    1.0    19-NOV-2018	    M. Douglas, Original
;
;----------------------------------------------------------------------- 

; Equates

SIOST	equ	10h		;2-SIO status register
SIODR	equ	11h		;2-SIO data register

	org	1800h		;location of this loader

; Initialize the 2SIO port

	mvi	a,3		;reset the ACIA
	out	SIOST
	mvi	a,011h		;015h for 8N1, 011h for 8N2
	out	SIOST

; Store bytes in sequentially increasing memory pointed to by HL.
;    If the SYNC byte is detected (6Ah, ends the leader, marks the
;    start of data), HL is reset to zero. This code assumes the
;    leader is less than 255 bytes long.

	mvi	h,0		;HL->where to load binary (L set later)

loop	lxi	sp,stack	;init SP so a RET jumps to loop
	in	SIOST		;get sio status
	rrc			;new byte available?
	rnc			;no (jumps back to loop)

	in	SIODR		;get the byte and store in memory
	mov	m,a
	inx	h

	sui	6Ah		;sync byte?
	rnz			;no (jumps back to loop)

	mov	l,a		;restart hl at zero (sync in 1st 256 bytes)
	ret			;jump back to loop

stack	dw	loop		;stack content jumps to loop when RET executed

	end

