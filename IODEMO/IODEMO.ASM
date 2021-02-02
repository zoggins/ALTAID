;************************************
;* Demo using my speaker/switch
;* board.
;*
;* Hardware: v3 speaker board
;*
;* Connections: J1 1-8 = Altaid J2 Pins 1-8
;*              J1 9   = Altaid J2 Pin 9
;*              J1 10  = Altaid J2 Pin 10
;*              J2 1   = 5v
;*              J2 2   = GND
;* 
;* Adapted from the demo in the
;* KIM-1 manual.
;*
;************************************

			ORG 	2000H
INIT: 		MVI	 	B,0			; Set B to initial value of 0
START: 		INR 	B			; Toggle Speaker
			MOV	 	A,B	
			OUT 	61H	 
			IN 		60H			; Read buttons
			CMA
			RRC		
			ANI		7FH
DELAY: 		NOP
			NOP
			NOP
			DCR		A			; Delay by an amount specified
			JP		DELAY		; by the count in A
			JM		START		; Go Start
			END
