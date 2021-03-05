	title 'System Control Block Definition for CP/M3 Loader BIOS'

; This version of the SCB should be linked when making CPMLDR since
;   CPMLDR does not pass through the GENCPM procedure. That, in turn
;   leaves the FExxh equates of SCB.ASM actually locating these
;   vectors in RAM at FExxh which isn't desired.
;
; M. Douglas, 3/19/19

	public @civec,@covec,@aivec,@aovec,@lovec,@mxtpa


	cseg
@CIVEC  ds	2		; Console Input Redirection 
                                ; Vector (word, r/w)
@COVEC  ds	2		; Console Output Redirection 
                                ; Vector (word, r/w)
@AIVEC	ds	2		; Auxiliary Input Redirection 
                                ; Vector (word, r/w)
@AOVEC	ds	2		; Auxiliary Output Redirection 
                                ; Vector (word, r/w)
@LOVEC	ds	2		; List Output Redirection 
                                ; Vector (word, r/w)
@MXTPA  ds  	2    		; Top of User TPA 
                                ; (address at 6,7)(word, r/o)
	end
