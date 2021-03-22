; DEMO.ASM
;
; ALTAIDS computer demo
; based on Altair demo from Popular Electronics, Feb 1975
;
; COMMAND TO ASSEMBLE: 
;	a85 demo.asm -l demo.prn -o demo.hex
; 
;
	ORG	2000H
DEMO:	LDA	VAR1
	MOV	B,A
	LDA	VAR2
	ADD	B
	STA	VAR3
	JMP	DEMO
;	
	ORG	2080H
VAR1:	DB	1CH
VAR2:	DB	08H
VAR3:	DB	0

	END