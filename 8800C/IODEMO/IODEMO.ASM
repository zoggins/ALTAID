;*****************************************************
;* Demo using my speaker/switch
;* board.
;*
;* Hardware: v3 speaker board with R4-R10 shorted
;*			 Cromemco 8PIO Parallel Card
;*
;* Connections: J1 1   = 8PIO BIT D0 PORT 3
;*				J1 2-8 = 8PIO BITs D0 - D6 PORT 2
;*              J1 9   = 8PIO OUTPUT STROBE 2
;*              J1 10  = 8PIO GROUND RETURN
;*              J2 1   = 5v
;*				J2 2   = 8PIO LATCH INPUT 2
;*              J2 3   = GND
;* 
;* Adapted from the demo in the
;* KIM-1 manual.
;*
;*****************************************************

			ORG 	2000H
INIT: 		MVI	 	B,0			; Set B to initial value of 0
			MVI		A, 08H
			OUT		86H
START: 		INR 	B			; Toggle Speaker
			MOV	 	A,B	
			OUT 	83H	 
			IN 		82h			; Read buttons
			CMA
			ANI		7FH
DELAY: 		NOP
			NOP
			NOP
			DCR		A			; Delay by an amount specified
			JP		DELAY		; by the count in A
			JM		START		; Go Start
			END
