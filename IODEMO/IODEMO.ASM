;************************************
;* Demo using my speaker/switch
;* board.
;*
;* PA0-PA7 = J2 pins 0-7
;* 
;* Adapted from the demo in the
;* KIM-1 manual.
;*
;* NOTE: The sound is SUPER SUPER
;* quiet.  I suspect its because
;* the Altaid is vampiring the voltage
;* from the board.
;************************************

			ORG 	2000H
INIT: 		MVI	 	B,0			; Set B to initial value of 0
START: 		INR 	B			; Toggle Speaker
			MOV	 	A,B	
			OUT 	61H	 
			IN 		60H			; Read buttons
			RRC		
			ANI		7FH
DELAY: 		NOP
			NOP
			NOP
			DCR		A			; Delay by an amount specified
			JP		DELAY		; by the count in A
			JM		START		; Go Start
			END
