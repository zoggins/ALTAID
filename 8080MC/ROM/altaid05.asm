;Altaid 8800
;
;8080MC
;
;Monitor program for 8080A based computer.
;Written by Josh Bensadon
;Free for public use, following the common shareware policies.
;
;
;
;
;Version 0.0 - Josh Bensadon. Basic operation of LED's & Switches
;Version 0.1 - Josh Bensadon. Serial Put_Char, Get_Char, Read/Write Sectors
;Version 0.2 - Josh Bensadon. CP/M
;Version 0.3 - Josh Bensadon. 38,400 BAUD, DISK XMODEM
;
;
;Hardware
;Input:	8 Data Momentary Data Switches
;	1 Run  M-Switch
;	1 Mode M-Switch
;	1 Next M-Switch
;	1 RESET M-Switch (Hardwired) Press and hold, discharges capacitor in 2 seconds to activate Reset Line
;
;Output: 8 Data LED's
;	16 Address LED's
;	1 Run LED
;	1 Mode LED
;	1 Red LED (Disk Read)
;	1 Blue LED (Disk Write)
;
;Serial Port:	1 TXD (RS-232 or TTL)
;		1 RXD (RS-232 or TTL)
;		1 RTS (TTL) Hardwired to RESET instantly
;
;Switch Functionality
;Upon Boot up:
;D7 - Selects 9,600 Baud
;D6 - Selects 19,200 Baud
;D5 - Selects 38,400 Baud (stay & store)
;D4 - Selects 38,400 Baud (quick exit)
;
;Next - Force Cold Boot (Release when Red LED comes on)
;
;When Front Panel is Running:
;Mode selects one of four states that dictate what the data switches will do
;	1- Select High Address
;	2- Select Low Address
;	3- Select Data (memory at this location) (if ROM selected, Data LEDs will not change)
;	4- Do nothing
;
;NEXT advances to the next memory location.
;Holding down next will engage auto increment
;Holding down D7 while pressing NEXT will retreat to the previous location
;
;RUN turns on RUN Led & Jumps to the Address Location selected
;
;
;
;
;Memory Map
;0-3K ROM	-Start up
;		-Monitor Menu
;		-Monitor Functions
;		-Front Panel Operation (438 bytes)
;		-Serial TX
;		-HEX Load
;		-XModem
;		-Read/Write Sector
;		-Format RAM DISK
;
;3K-3.3K ROM	-CP/M BIOS (ROM extension) including Load CP/M (ROM to RAM)
;3.3K-9K ROM	-CP/M Image (CCP & BDOS) (to be copied to RAM at D800)
;9K-9.1K ROM	-BIOS (to be copied to RAM at EE00)
;9.1K-9.6K	-ISR routine to be copied to RAM at EEC7
;
; 000 (16K)	-PIP
; 000	0E00	PIP.COM
; 800	0E10	PIP.COM
; 000	0E20	PIP.COM
; 800	0E30	PIP.COM
; 000	0E40	XM.COM
;-6780	0E4F	Sector 0, directory enteries for PIP & XM
; 800	0E50	spare
; 000	0E60	spare
; 800	0E70	spare
;
;
;RAM
;D800		-CCP
;E006		-BDOS
;EE00		-BIOS
;EEAE		-ISR Timer
;EEC7		-ISR Serial RX
;-EF21
;
;FD00		-CP/M Workspace
;-FDB7
;
;FE00		-RX Data Buffer
;-FEFF
;
;FF00		-Front Panel, Xmodem & other Monitor/Base system RAM variables
;-FF3F		
;FF40		-STACK
;-FF7F
;FF80		-Sector Buffer (write)
;-FFFF


;	READ SECTOR FROM RAM
;512K RAM  allows for 64K System + 14 Tracks of 256 Sectors
;BANK
;0 - 64K System RAM, Stack, etc.
;1 - 512 Sectors    0 to  511   Track 0, 1
;2 - 512 Sectors  512 to 1023	Track 2, 3
;3 - 512 Sectors 1024 to 1535	Track 4, 5
;4 - 512 Sectors 1536 to 2047	Track 6, 7
;5 - 512 Sectors 2048 to 2559	Track 8, 9
;6 - 512 Sectors 2560 to 3071	Track 10, 11
;7 - 512 Sectors 3072 to 3583	Track 12, 13        3583 = 0x0DFF
;
;Total 3584 Sectors in RAM
;
;32K ROM allows for 16K of additional ROM DISK area for Read Only Files
;We must use this space entirely for Files.  
;CP/M Must not be allowed to consider any of this space writable.
;PIP.COM 8K  (4 blocks, 224, 225, 226, 227) Sectors 0x0E00 to 0x0E3F
;XM.COM  2K  (1 block, 228) Sectors 0x0E40 to 0x0E4F.  
;Sector 0x0E4F is not used in XM.COM, so it is used to set the initial directory at Sector 0
;1 Block = 2K = 16 Sectors
;
;8 sectors = 1K, but we must go by blocks of 16 sectors (2K)
;
;ROM	Sec
; 000	0E00	PIP.COM
; 800	0E10	PIP.COM
; 000	0E20	PIP.COM
; 800	0E30	PIP.COM
; 000	0E40	XM.COM
; 800	0E50	spare
; 000	0E60	spare
; 800	0E70	spare
;
;64K ROM 27512
;8000	0E80	spare
;8800	0E90	spare
;9000	0EA0	spare
;9800	0EB0	spare
;A000	0EC0	spare
;A800	0ED0	spare
;B000	0EE0	spare
;B800	0EF0	spare
;C000	0F00	spare
;C800	0F10	spare
;D000	0F20	spare
;D800	0F30	spare
;E000	0F40	spare
;E800	0F50	spare
;F000	0F60	spare
;F800	0F70	spare




;           #INCLUDE        "macros.h"


		.CSEG

CR		.EQU 0DH
LF		.EQU 0AH
ESC		.EQU 1BH

OUTPUT_PORT	.EQU 0C0H	;Output Port
INPUT_PORT	.EQU 040H	;Input Port

				;Control (output) Port
ROM_HI		.EQU 040H	;Set to map ROM to High Address (Clr = RAM)
ROM_LOW		.EQU 041H	;Clear to map ROM to Low Addres (Set = RAM)
B16		.EQU 042H	;RAM Bank Select (A16)
B18		.EQU 043H	;RAM Bank Select (A18)
CASSETTE	.EQU 044H	;Cassette output
B15		.EQU 045H	;ROM Bank Select (A15)
TIMER		.EQU 046H	;
B17		.EQU 047H	;RAM Bank Select (A17)

		;RAM BANK SELECT:
		;RAM is to be 628512 (512KB), requires 19 address lines.
		;A0-A15 addresses 64K of RAM (when all RAM is selected)
		;B16, B17, B18 select 1 of 8 blocks of 64K
		;Moving data between blocks requires storage in CPU registers.
		;
		;ROM BANK SELECT:
		;ROM is to be 27512 (64KB), requires 16 address lines
		;A0-A14 addresses 32K of ROM
		;B15 selects 1 of 2 blocks of 32K (lower or upper half of the 64K)
		;Breaking ROM into 2 banks allows RAM in the Upper 32K memory space.
		;
		;All ROM space is "Shadow ROM", meaning, reads are from ROM and any
		;memory writes will happen to the RAM at that address.
		;The Shadow ROM can be turned off, thus allowing the RAM to be read.
		;
		;ROM_LOW, when low, enables the ROM bank selected in the lower 32K space.
		;ROM_Hi, when hi, enables 16K of the ROM bank selected in the first 16K of
		;the upper 32K space.
		;
		;At Reset, all Control Output port bits are cleared to 0.
		;This allows ROM_LOW to enable shadow ROM at 0000 for CPU startup.
		;ROM Bank 0, RAM Bank 0.
		;
		
GO_CPM		.EQU	0EE00H		

;                 ******       *****      *****    ********* 
;                 *******     *******    *******   ********* 
;                 **    **   ***   ***  ***   ***     ***    
;                 **    **   **     **  **     **     ***    
;                 *******    **     **  **     **     ***    
;                 *******    **     **  **     **     ***    
;                 **    **   **     **  **     **     ***    
;                 **    **   ***   ***  ***   ***     ***    
;                 *******     *******    *******      ***    
;                 ******       *****      *****       ***    



				;Initialize System
				; -Format RAM DISK with E5's (blank)
				; -Init System RAM for Front Panel Operation
				; -Test INT pin, if stuck low, send message and Loop back
				; -Print Welcome (version)
				; -Load CP/M from ROM to upper RAM (includes ISR)
				; -Insert Jump to ISR in low RAM
				; -Enable Interrupts
				;
				;Proceed to Main Menu
				;				
		
		.ORG 0000H
		LXI	SP,STACK

		LHLD	RAM_SIGNATURE
		DCR	H
		DCR	H
		JNZ	COLD_BOOT
		MOV	A,L
		CPI	0F8H
		JZ	WARM_BOOT
		
					;Initialize BITMAP at FF00			FF00 00000001
COLD_BOOT	LXI	H,BITMAP	;HL=FF00					FF01 00000010
		XRA	A		;A=00						FF02 00000100
		STC			;Carry set					FF03 00001000
BM_FILL		RAL			;March single bit left through Acc		FF04 00010000
		MOV	M,A		;Save it in memory				FF05 00100000
		INR	L		;						FF06 01000000
		JNC	BM_FILL		;Repeat until bit comes out to Carry again.	FF07 10000000
BM_ZERO		MOV	M,A		;Clear Remainder of RAM to FFFFh
		INR	L
		CMP	L
		JNZ	BM_ZERO

		CALL	DO_BAUD1

		LXI	H,MSG_COLD_BOOT
		SHLD	XSECTOR
		JMP	WARM_BOOT1


		#if ($ > IRQ_VECTOR)	;Protect IRQ Vector
             		!!!IRQ_VECTOR-CONFLICT!!!
		#endif

		.ORG 0038H	;IRQ
IRQ_VECTOR	;RST	038H	;11   5.5uSEC
		JMP	ISR_RAM	;10	Jump to ISR

WARM_BOOT	LXI	H,MSG_WARM_BOOT
WARM_BOOT1	SHLD	XSECTOR

		LHLD	BAUD_SET_PTR	;Set Saved Baud Rate
		CALL	JMP_HL	;PCHL

;		XRA	A		;KILL Timer (If Enabled by BAUD SET)
;		OUT	TIMER

		LXI	H,RXBUFFER
		SHLD	RXBHEAD
		SHLD	RXBTAIL

		LXI	H,FP_LED_MAT	;Init LED pointer
		SHLD	FP_MAT_PTR	
	
		MVI	A,64H		;Read High Data Switches
		CALL	LED_OUT
		IN	INPUT_PORT
		RRC
		RRC
		RRC
		JNC	COLD_BOOT

					;Cold or Warm boot, Data Switches will select Baud rate
		MVI	A,50H		;Read High Data Switches
		CALL	LED_OUT
		IN	INPUT_PORT
		LXI	H,SW_BAUD_END	;Load return Address on Stack
		PUSH	H
		RRC
		JNC	DO_BAUD4
		RRC
		JNC	DO_BAUD3
		RRC
		JNC	DO_BAUD2
		RRC
		JNC	DO_BAUD1
		;POP	H		;Leave stack messy.
SW_BAUD_END

		IN	INPUT_PORT
		MOV	D,A

		MVI	A,1		;Allow Timer Interrupt to Trigger
		OUT	TIMER
		MVI	C,0
		CALL	DELAY_SCAN
		IN	INPUT_PORT
		MOV	E,A
		ORI	20H		;Ignore Timer Interrupt (keep only RX input)
		MOV	B,A

		XRA	A		;Cancel Interrupt
		OUT	TIMER

		CALL	DELAY_SCAN
		IN	INPUT_PORT
		MOV	H,A
		ANA	B
		CMA
		ANI	0A0H		;Test RX and Timer inputs for Interrupt
		JZ	INT_TEST_END
		CALL	PUT_DE
		CALL	PUT_HL
		CALL	PRINTI		;System Start, Display Welcome Message
		.text "-Int pin stuck low\r\n\000"
		JMP	$0000		;Restart
INT_TEST_END	
		CALL	LED_OFF		;Set LED OFF

		LXI	H,02F8H		;RAM Signature is "to fate"
		SHLD	RAM_SIGNATURE

		CALL	PUT_NEW_LINE
		LHLD	XSECTOR
		CALL	PRINT		;Print COLD/WARM

		CALL	PRINTI		;System Start, Display Welcome Message
		.text "-BOOT\r\nAltaid 8800 v0.5 Itty Bitty Micro Co Nov 4, 2019 Chksum=\000"
		
		CALL	CHKSUM_ROM
		CALL	PUT_DE
		
		LDA	XSECTOR
		CPI	MSG_COLD_BOOT & $FF
		JZ	SKIP_RAM_CHKSUM
		
		CALL	CHKSUM_RAM
		LHLD	RAMDISK_CS
		MOV	A,H
		XRA	D
		MOV	H,A
		MOV	A,L
		XRA	E
		ORA	H
		LXI	H,MSG_GOOD
		JZ	RCT_GOOD
		LXI	H,MSG_BAD		
RCT_GOOD	CALL	PRINT		;Print GOOD/BAD
		
SKIP_RAM_CHKSUM


		CALL	LOAD_CPM

		LHLD	IRQ_VECTOR	;Copy ISR Jump to RAM
		SHLD	IRQ_VECTOR
		LHLD	IRQ_VECTOR+2
		SHLD	IRQ_VECTOR+2
		
		MVI	A,255
		STA	INT_ENABLED
		
		LXI	H,TIC_COUNTER+1	;Point front panel to view TIC_Counter
		SHLD	FPM_ADDR



;------------------------------------------------------ MAIN MENU
; **        **      ***      ****   **       **
; ***      ***    *** ***     **    ***      **
; ****    ****   **     **    **    ****     **
; ** **  ** **   **     **    **    ** **    **
; **  ****  **   *********    **    **  **   **
; **   **   **   *********    **    **   **  **
; **   **   **   **     **    **    **    ** **
; **   **   **   **     **    **    **     ****
; **   **   **   **     **    **    **      ***
; **   **   **   **     **   ****   **       **
;------------------------------------------------------ MAIN MENU


MAIN_MENU_FPON	MVI	A,81H		;Echo On, FP On
		STA	GC_OPTIONS	


MAIN_MENU:	EI
		LDA	TIMER_RUN	;Start TIC interrupt (If enabled to RUN)
		OUT	TIMER
		LXI	SP,STACK
		LXI	H, MAIN_MENU	;Push Mainmenu onto stack as default return address
		PUSH	H
		CALL	PRINTI		;Monitor Start, Display Welcome Message
		.text "\r\nMENU>\000"
		
		LDA	GC_OPTIONS
		ORI	80H		;Echo On
		STA	GC_OPTIONS
		CALL	LED_OFF

		CALL 	GET_CHAR	;get char
		CPI	CR
		JZ	MAIN_MENU
		CPI	'?'
		JZ	DISP_HELP
		CPI	'+'
		JZ	INC_FPM_ADDR
		CPI	'-'
		JZ	DEC_FPM_ADDR
MM_0		CPI	':'
		JZ 	GETHEXFILE	; : = START HEX FILE LOAD
		ANI 	5Fh		;to upper case
					;Branch to Command entered
		CPI 	'B'		
		JZ 	DO_BAUD		; B = BAUD
		CPI 	'C'		
		JZ 	GO_CPM		; C = CP/M
		CPI 	'D'		
		JZ 	MEM_DUMP	; D = Memory Dump
		CPI 	'E'
		JZ 	MEM_EDIT	; E = Edit Memory
		CPI 	'F'
		JZ 	FORMAT		; F = Format RAM DISK
		CPI 	'K'
		JZ 	CHKSUM_RAM_SET	; K = RAM DISK Check SUM
		CPI 	'G'
		JZ 	MEM_EXEC	; G = Go (Execute at)
		CPI 	'O'
		JZ 	PORT_OUT	; O = Output to port
		CPI 	'I'
		JZ 	PORT_INP	; I = Input from Port
		CPI 	'M'
		JZ	MEM_SELECT	; M = SELECT MEMORY SOURCE
		CPI 	'R'
		JZ	READ_SECTOR_T	; R = READ SECTOR
		CPI 	'W'
		JZ	WRITE_SECTOR_T	; W = WRITE SECTOR
		CPI 	'X'
		JZ	XMODEM		; X = XMODEM UP OR DOWN
		CPI 	'T'
		JZ	TOGGLE_TIMER
		CPI 	'L'
		JZ	TOGGLE_FP_PANEL
		CPI 	'V'
		JZ	VIEW_ADDR
		
		CALL 	PRINTI		;Display Err when input is invalid
		.text "\r\nINVALID CMD\000"

DISP_HELP	CALL 	PRINTI		;Display Err when input is invalid
		.text "\r\nHELP\r\n"
		.text "\r\nB -BAUD 1,2,3,4
		.text "\r\nC -CP/M"
		.text "\r\nD -Dump"
		.text "\r\nE -Edit"
		.text "\r\nF -Format RAM DISK"
		.text "\r\nK -RAM DISK ChkSum"
		.text "\r\nM -RAM/ROM Select"
		.text "\r\nG -Go (Exec)"
		.text "\r\nO -Output to port"
		.text "\r\nI -Input to port"
		.text "\r\nR -Read Sector"
		.text "\r\nW -Write Sector"
		.text "\r\nX -XMODEM R,S"
		.text "\r\nT -ISR_TIMER"
		.text "\r\nL -LED_PANEL"
		.text "\r\nV -SET FP Addr"
		.text "\r\n>\000"
		JMP 	MAIN_MENU



;1 - 512 Sectors    0 to  511   0000 to 01FF
;2 - 512 Sectors  512 to 1023	0200 TO 03FF
;3 - 512 Sectors 1024 to 1535	0400 TO 05FF
;4 - 512 Sectors 1536 to 2047	0600 TO 07FF
;5 - 512 Sectors 2048 to 2559	0800 TO 09FF
;6 - 512 Sectors 2560 to 3071	0A00 TO 0BFF
;7 - 512 Sectors 3072 to 3583	0C00 TO 0DFF        3583 = 0x0DFF

FORMAT		CALL 	PRINTI		;Display Err when input is invalid
		.text "\r\nFORMAT?\000"
		CALL 	GET_CHAR	;get char
		CPI	'Y'
		RNZ

		MVI	B,7	;Bank = 7
		LXI	D,0E5E5H ;E5 = Blank format

FORMAT_LOOP	CALL	PUT_SPACE
		MOV	A,B
		CALL	PUT_HEX		

		DI
		LXI	H,0	;Save Stack
		DAD	SP
		SHLD	SP_TEMP
		
		MVI	A,0E8H		;TXDATA HIGH, Blue LED ON
		CALL	LED_OUT

		LXI	SP,0H	;Use SP for Formating RAM DISK
		MOV	A,B
		OUT	B16	;Set A16 ;Set 1 of 8 Banks for 512KB
		RRC		;Can't CALL/PUSH/POP, RAM is shifting under our feet
		OUT	B17	;Set A17
		RRC
		OUT	B18	;Set A18
		
		LXI	H,02000H
FILL		PUSH	D	;11
		PUSH	D	;11
		PUSH	D	;11
		PUSH	D	;11
		DCR	L	;5 
		JNZ	FILL	;10  58 * 256 = 14848
		DCR	H	;5
		JNZ	FILL	;10  14848 * 32 ~= 0.5M
		
		XRA	A
		OUT	B16	;Set A16 ;Bank 0
		OUT	B17	;Set A17
		OUT	B18	;Set A18
		
		LHLD	SP_TEMP
		SPHL		;SP = Restored
		EI

		DCR	B
		JNZ	FORMAT_LOOP

		LXI	B,0E4FH		;Write the initial Directory with PIP and XM
		CALL	READ_SEC_DB
		LXI	B,0
		CALL	WRITE_SEC

		RET


INC_FPM_ADDR	LHLD	FPM_ADDR
		INX	H
		JMP	DFAR
DEC_FPM_ADDR	LHLD	FPM_ADDR
		DCX	H
DFAR		SHLD	FPM_ADDR
		RET

VIEW_ADDR	CALL	SPACE_GET_WORD	;Input start address
		XCHG			;HL = ADDRESS
		SHLD	FPM_ADDR	
		RET


TOGGLE_FP_PANEL	LDA	GC_OPTIONS
		XRI	1
		STA	GC_OPTIONS
		ANI	1
		CALL 	PRINTI		;Display Err when input is invalid
		.text " FP:\000"
		CALL	PUT_HEX
		JMP	TT_0

TOGGLE_TIMER	LDA	TIMER_RUN
		ORA	A
		JZ	TT_1
TT_0		CALL	LED_OFF
		XRA	A
		DCR	A
TT_1		INR	A
		STA	TIMER_RUN
		OUT	TIMER
		CALL 	PRINTI		;Display Err when input is invalid
		.text " ISR_TIMER:\000"
		CALL	PUT_HEX
		RET
		


DO_BAUD		CALL 	GET_CHAR	;get char
		DI
		CPI 	'1'		
		JZ 	DO_BAUD1	; BAUD 9600
		CPI 	'2'		
		JZ 	DO_BAUD2	; BAUD 19200
		CPI 	'3'		
		JZ 	DO_BAUD3	; BAUD 38400 (Stay in IRQ)
		CPI 	'4'		
		JZ 	DO_BAUD4	; BAUD 38400 (Quick Return)
		RET

DO_BAUD1	CALL	BAUD_SET	;Save this Routine as address to set Baud
		LXI	H,IR1_ROM	;Source
		LXI	B,IR1_SIZE	;#Bytes to copy
		PUSH	H
		LXI	H,PUT_CHAR1
		MVI	A,1		;TIMER ON
		JMP	DB_EXIT
		
DO_BAUD2	CALL	BAUD_SET	;Save this Routine as address to set Baud
		LXI	H,IR2_ROM	;Source
		LXI	B,IR2_SIZE	;#Bytes to copy
		PUSH	H
		LXI	H,PUT_CHAR2
		MVI	A,1		;TIMER ON
		JMP	DB_EXIT

DO_BAUD3	CALL	BAUD_SET	;Save this Routine as address to set Baud
		LXI	H,IR3_ROM	;Source
		LXI	B,IR3_SIZE	;#Bytes to copy
		PUSH	H
		LXI	H,PUT_CHAR3
		XRA	A		;TIMER OFF
		JMP	DB_EXIT

DO_BAUD4	CALL	BAUD_SET	;Save this Routine as address to set Baud
		LXI	H,IR4_ROM	;Source
		LXI	B,IR4_SIZE	;#Bytes to copy
		PUSH	H
		LXI	H,PUT_CHAR3
		XRA	A		;TIMER OFF

DB_EXIT		OUT	TIMER
		STA	TIMER_RUN
		SHLD	PC_PTR
		POP	H
		LXI	D,ISR_RAM	;Destination
		CALL	COPY_RAM_BC
		RET

BAUD_SET	POP	H		;Saves Return address as the BAUD Routine to be set
		PUSH	H
		SHLD	BAUD_SET_PTR
		RET


MEM_SELECT	LXI	H,MEM_SOURCE
		MVI	A,1
		XRA	M
		MOV	M,A

MEM_DISPLAY	LDA	MEM_SOURCE
		ORA	A
		JZ	MD_ROM
		CALL	PRINTI
		.text "\r\nRAM\000"
		RET
MD_ROM		CALL	PRINTI
		.text "\r\nROM\000"
		RET


;=============================================================================
;MEMORY DUMP
;-----------------------------------------------------------------------------
MEM_DUMP:	CALL	SPACE_GET_WORD	;Input start address
		XCHG			;HL = Start
		CALL	SPACE_GET_WORD	;Input end address (DE = end)
		CALL	MEM_DISPLAY

MEM_DUMP_LP:	CALL	PUT_NEW_LINE
		CALL	DUMP_LINE	;Dump 16 byte lines (advances HL)
		RZ			;RETURN WHEN HL=DE
		MOV	A,L
		ORA	A
		JNZ	MEM_DUMP_LP	;Dump 1 Page, then prompt for continue
		CALL	GET_CONTINUE
		JMP	MEM_DUMP_LP


GET_CONTINUE	CALL	PUT_NEW_LINE
		CALL	PRINTI
		.text "Press any key to continue\000"
		CALL	GET_CHAR
		CPI	27
		RNZ
		POP	H		;Scrap return address
		RET


;-----------------------------------------------------------------------------
;DUMP_LINE -- Dumps a line
;xxx0:  <pre spaces> XX XX XX XX XX After spaces | ....ASCII....
;-----------------------------------------------------------------------------
DUMP_LINE:	PUSH	B		;+1
		PUSH	H		;+2 Save H for 2nd part of display
		PUSH	H		;+3 Start line with xxx0 address
		MOV	A,L
		ANI	0F0h		;Mask FFF0
		MOV	L,A
		CALL	PUT_HL		;Print Address
		CALL	PRINTI
		.text ": \000"
		POP	H		;-3
		MOV	A,L
		ANI	0Fh		;Fetch how many prespaces to print
		MOV	C,A
		MOV	B,A		;Save count of prespaces for part 2 of display
		CALL	PUT_3C_SPACES

DL_P1L		CALL	PUT_SPACE
		CALL	READ_MEMORY
		CALL	PUT_BYTE
		CALL	CMP_HL_DE
		JZ	DL_P1E
		INX	H
		MOV	A,L
		ANI	0Fh
		JNZ	DL_P1L
		JMP	DL_P2

DL_P1E		MOV	A,L
		CMA
		ANI	0Fh
		MOV	C,A
		CALL	PUT_3C_SPACES

DL_P2		CALL	PRINTI		;Print Seperator between part 1 and part 2
		.text " | \000"

DL_PSL2		MOV	A,B		;Print prespaces for part 2
		ORA	A
		JZ	DL_PSE2
		CALL	PUT_SPACE
		DCR	B
		JMP	DL_PSL2
DL_PSE2
		POP	H		;-2
		POP	B		;-1
DL_P2L		CALL	READ_MEMORY
		CPI	' '		;A - 20h	Test for Valid ASCII characters
		JP	DL_P2K1
		MVI	A,'.'				;Replace with . if not ASCII
DL_P2K1		CPI	07Fh		;A - 07Fh
		JM	DL_P2K2
		MVI	A,'.'
DL_P2K2		CALL	PUT_CHAR

		CALL	CMP_HL_DE
		RZ
		INX	H
		MOV	A,L
		ANI	0Fh
		JNZ	DL_P2L

;-----------------------------------------------------------------------------
;Compare HL with DE
;Exit:		Z=1 if HL=DE
;		M=1 if DE > HL
CMP_HL_DE	MOV	A,H
		CMP	D		;H-D
		RNZ			;M flag set if D > H
		MOV	A,L
		CMP	E		;L-E
		RET


PUT_3C_SPACES	MOV	A,C		;Print 3C Spaces
		ORA	A
		RZ
		DCR	C		;Count down Prespaces
		CALL	PRINTI		;Print pre spaces
		.text "   \000"
		JMP	PUT_3C_SPACES


;-----------------------------------------------------------------------------
;EDIT MEMORY
;Edit memory from a starting address until X is pressed.
;Display mem loc, contents, and results of write.
;-----------------------------------------------------------------------------
MEM_EDIT:	CALL	SPACE_GET_WORD	;Input Address
		XCHG			;HL = Address to edit
		CALL	MEM_DISPLAY
ME_LP		CALL	PUT_NEW_LINE
		CALL	PUT_HL		;Print current contents of memory
		CALL	PUT_SPACE
		MVI	A, ':'
		CALL	PUT_CHAR
		CALL	PUT_SPACE
		CALL	READ_MEMORY
		CALL	PUT_BYTE
		CALL	SPACE_GET_BYTE	;Input new value or Exit if invalid
		RC			;Exit to Command Loop
		MOV	M, A		;or Save new value
		CALL	PUT_SPACE
		CALL	READ_MEMORY
		CALL	PUT_BYTE
		INX	H		;Advance to next location
		JMP	ME_LP		;repeat input


;=============================================================================
;	MEM_EXEC - Execute at
;	Get an address and jump to it
;-----------------------------------------------------------------------------
MEM_EXEC:	CALL	SPACE_GET_WORD	;Input address
		XCHG			;HL = Address
		PCHL			;Jump to HL


;===============================================
;Input from port, print contents
PORT_INP:	CALL	SPACE_GET_BYTE
		MOV	B, A
		CALL	PUT_SPACE
		MVI	C, 0DBh
		CALL	GOBYTE
		CALL	PUT_BYTE
		RET

;Get a port address, write byte out
PORT_OUT:	CALL	SPACE_GET_BYTE
		MOV	B, A
		CALL	SPACE_GET_BYTE
		MVI	C, 0D3h

;===============================================
;GOBYTE -- Push a two-byte instruction and RET
;         and jump to it
;
;pre: B register contains operand
;pre: C register contains opcode
;post: code executed, returns to caller
;-----------------------------------------------
GOBYTE:		LXI	H, 0000
		DAD	SP	;HL = STACK
		DCX	H
		MVI	M, 0C9h	;Stuff RET instruction in STACK RAM
		DCX	H
		MOV	M, B	;Stuff Port
		DCX	H
		MOV	M, C	;Stuff Input or Output instruction
		PCHL


;=============================================================================
SPACE_GET_BYTE	CALL	PUT_SPACE

;=============================================================================
;GET_BYTE -- Get byte from console as hex
;
;in:	Nothing
;out:	A = Byte (if CY=0)
;	A = non-hex char input (if CY=1)
;-----------------------------------------------------------------------------
GET_BYTE:	CALL	GET_HEX_CHAR	;Get 1st HEX CHAR
		JNC	GB_1
		CPI	' '		;Exit if not HEX CHAR (ignoring SPACE)
		JZ	GET_BYTE	;Loop back if first char is a SPACE
		STC			;Set Carry
		RET			;or EXIT with delimiting char
GB_1		PUSH	D		;Process 1st HEX CHAR
		RLC
		RLC
		RLC
		RLC
		ANI	0F0h
		MOV	D,A
		CALL	GET_HEX_CHAR
		JNC	GB_2		;If 2nd char is HEX CHAR
		CPI	' '
		JZ	GB_UNDO		;If 2nd char is SPACE, Move 1st back to lower nibble
		CPI	CR
		JZ	GB_UNDO		;If 2nd char is CR, Move 1st back to lower nibble		
		STC			;Set Carry
		POP	D
		RET			;or EXIT with delimiting char
GB_2		ORA	D
		POP	D
		RET
GB_UNDO		MOV	A,D		;Move 1st back to lower nibble
		RRC
		RRC
		RRC
		RRC
GB_RET		ORA	A
		POP	D
		RET


;=============================================================================
SPACE_GET_WORD	CALL	PUT_SPACE

;=============================================================================
;GET_WORD -- Get word from console as hex
;
;in:	Nothing
;out:	A = non-hex char input
;	DE = Word
;-----------------------------------------------------------------------------
GET_WORD:	LXI	D,0
		CALL	GET_HEX_CHAR	;Get 1st HEX CHAR
		JNC	GW_LP
		CPI	' '		;Exit if not HEX CHAR (ignoring SPACE)
		JZ	GET_WORD	;Loop back if first char is a SPACE
		ORA	A		;Clear Carry
		RET			;or EXIT with delimiting char
GW_LP		MOV	E,A
		CALL	GET_HEX_CHAR
		RC			;EXIT when a delimiting char is entered
		XCHG			;Else, shift new HEX Char Value into DE
		DAD	H
		DAD	H
		DAD	H
		DAD	H
		XCHG
		ORA	E
		JMP	GW_LP



;===============================================
;Get HEX CHAR
;in:	Nothing
;out:	A = Value of HEX Char when CY=0
;	A = Received (non-hex) char when CY=1
;-----------------------------------------------
GET_HEX_CHAR:	CALL	GET_CHAR
		CPI	'0'
		JM	GHC_NOT_RET
		CPI	'9'+1
		JM	GHC_NRET
		CPI	'A'
		JM	GHC_NOT_RET
		CPI	'F'+1
		JM	GHC_ARET
		CPI	'a'
		JM	GHC_NOT_RET
		CPI	'f'+1
		JM	GHC_ARET
GHC_NOT_RET	STC
		RET
GHC_ARET	SUI	07h
GHC_NRET	ANI	0Fh
		RET




;===============================================
;ASCHEX -- Convert ASCII coded hex to nibble
;
;pre:	A register contains ASCII coded nibble
;post:	A register contains nibble
;-----------------------------------------------
ASCHEX:		SUI	30h
		CPI	0Ah
		RM
		ANI	5Fh
		SUI	07h
		RET

;===============================================
;PUT_SPACE -- Print a space to the console
;
;pre: none
;post: 0x20 printed to console
;-----------------------------------------------
PUT_SPACE:	MVI	A, ' '
		JMP	PUT_CHAR



;----------------------------------------------------------------------------------------------------; ASCII HEXFILE TRANSFER
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;----------------------------------------------------------------------------------------------------; ASCII HEXFILE TRANSFER
GETHEXFILE	CALL	ECHO_FP_OFF	;TURN OFF ECHO
		XRA	A
		MOV	E,A		;ZERO ERROR COUNTER
		JMP	GHDOLINE

GHWAIT		CALL	GET_CHAR
		CPI	':'
		JNZ	GHWAIT

GHDOLINE	CALL	GET_BYTE	;GET BYTE COUNT
		MOV	C,A		;BYTE COUNTER
		MOV	D,A		;CHECKSUM

		CALL	GET_BYTE	;GET HIGH ADDRESS
		MOV	H,A
		ADD	D
		MOV	D,A

		CALL	GET_BYTE	;GET LOW ADDRESS
		MOV	L,A
		ADD	D
		MOV	D,A

		CALL	GET_BYTE	;GET RECORD TYPE
		CPI	1
		JZ	GHEND	;IF RECORD TYPE IS 01 THEN END
		ADD	D
		MOV	D,A

GHLOOP		CALL	GET_BYTE	;GET DATA
		MOV	M,A
		ADD	D
		MOV	D,A
		INX	H

		DCR	C
		JNZ	GHLOOP

		CALL	GET_BYTE	;GET CHECKSUM
		ADD	D
		JZ	GHWAIT
		INR	E
		JNZ	GHWAIT
		DCR	E
		JMP	GHWAIT

GHEND		CALL	PRINTI
		.text "\r\nHEX TRANSFER COMPLETE ERRORS=\000"
		MOV	A,E
		CALL	PUT_BYTE
		RET


;-----------------------------------------------------------------------------------; XMODEM ROUTINES
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;-----------------------------------------------------------------------------------; XMODEM ROUTINES

SOH	.equ	1	;Start of Header
EOT	.equ	4	;End of Transmission
ACK	.equ	6
DLE	.equ	16
DC1	.equ	17	; (X-ON)
DC3	.equ	19	; (X-OFF)
NAK	.equ	21
SYN	.equ	22
CAN	.equ	24	;(Cancel)

;---------------------------------------------------------------------------------
;XMODEM MENU
;ENTRY:	TOP OF STACK HOLDS RETURN ADDRESS (EXIT MECHANISM IF XMODEM IS CANCELLED)
;---------------------------------------------------------------------------------
XMODEM		CALL	PUT_SPACE
		CALL	GET_CHAR	;get char
		ANI	5Fh		;to upper case
		CPI	'S'
		JZ	XMDN		; S = Send (Download) (Save Disk image to PC/Terminal)
		CPI	'R'
		JZ	XMUP		; R = Receive / UPLOAD (Load PC to MC)
		CALL 	PRINTI
		.text "?\000"
		RET

;---------------------------------------------------------------------------------
;XMDN - XMODEM DOWNLOAD (send file from IMSAI to Terminal)
;INPUT STARTING ADDRESS AND COUNT OF BLOCKS (WORD)
;WAIT FOR 'C' OR NAK FROM HOST TO START CRC/CS TRANSFER
;---------------------------------------------------------------------------------
XMDN		CALL	ECHO_FP_OFF

		

	;HL = Address of data to send from the IMSAI 8080
	;DE = Count of Blocks to send.

		CALL	XMS_INIT	;Starts the Seq, Sets the CS/CRC format
					;Cancelled Transfers will cause a RET
		LXI	H,0		;Sector 0
		JMP	XMDN_0
		
XMDN_LP		CALL	READ_SEC_DB
		CALL	XMS_SEND	;Sends the packet @HL, Resends if NAK
					;Cancelled Transfers will cause a RET
		LHLD	XSECTOR
		INX	H
XMDN_0		SHLD	XSECTOR
		MOV	B,H
		MOV	C,L

		LXI	D,-3584		;Total 3584 Sectors in RAM
		DAD	D
		JNC	XMDN_LP

		CALL	XMS_EOT		;Send End of Transmission
		JMP	PURGE


;---------------------------------------------------------------------------------
;XMUP - XMODEM UPLOAD (receive file from Terminal to IMSAI 8080)
;INPUT STARTING ADDRESS
;SEND 'C' OR NAK TO HOST TO START CRC/CS TRANSFER
;---------------------------------------------------------------------------------
XMUP		CALL	ECHO_FP_OFF


		LXI	H,0		;Sector 0
		SHLD	XSECTOR

		CALL	XMR_INIT	;Starts the transfer & Receives first PACKET
					;Cancelled Transfers will cause a RET

XMUP_LP		CALL	XMR_RECV	;Receives the next packet @HL, Resends if NAK
					;Cancelled Transfers will cause a RET
		JC	XMUP_LP		;Jump until EOT Received
		JMP	PURGE		;Purge and RET



;---------------------------------------------------------------------------------
;INIT FOR SENDING XMODEM PROTOCOL, GET NAK OR 'C', SAVE THE XMTYPE
;---------------------------------------------------------------------------------
XMS_INIT	MVI	A,1		;First SEQ number
		STA	XMSEQ

		MVI	B,10		;10 retries for initiating the transfer
XMS_INIT_LP	MVI	A,45		;GET CHAR, 45 SECONDS TIMEOUT (EXPECT C OR NAK)
		CALL	TIMED_GETCHAR
		JC	XM_CANCEL	;Cancel if Host Timed out

		CPI	NAK		;If NAK, Start Checksum Download
		JZ	XMS_DO
		CPI	'C'		;If C, Start CRC Download
		JZ	XMS_DO
		DCR	B		;Count down Retries
		JNZ	XMS_INIT_LP
		JMP	XM_CANCEL	;Cancel XModem if all retries exhausted

XMS_DO		STA	XMTYPE
		RET

;---------------------------------------------------------------------------------
;SEND A PACKET (RESEND UPON NAK)
;---------------------------------------------------------------------------------
XMS_SEND	LXI	H,SECTOR_BUFFER	;Default Buffer

		MVI	A,SOH		;SEND THE HEADER FOR CRC OR CHECKSUM
		CALL	PUT_CHAR
		LDA	XMSEQ
		CALL	PUT_CHAR
		CMA
		CALL	PUT_CHAR
		LXI	D,0000H		;Init DE=0000 (CRC Accumulator)
		MVI	C,0		;Init C=00 (CS Accumulator)
		MVI	B,128		;Count 128 bytes per block
XMS_BLP		MOV	A,M		;Fetch bytes to send  -------------------\
		CALL	PUT_CHAR	;Send them
		ADD	C		;Update the CS
		MOV	C,A
		MOV	A,M
		CALL	CRC_UPDATE	;Update the CRC
		INX	H		;Advance to next byte in block
		DCR	B		;Count down bytes sent
		JNZ	XMS_BLP		;Loop back until 128 bytes are sent -----^
		LDA	XMTYPE
		CPI	NAK		;If NAK, send Checksum
		JZ	XMS_CS		;----------------------v
		MOV	A,D		;else, Send the CRC next
		CALL	PUT_CHAR
		MOV	C,E
XMS_CS		MOV	A,C		;----------------------/
		CALL	PUT_CHAR
					;Packet Sent, get Ack/Nak Response
		MVI	A,45		;GET CHAR, 45 SECONDS TIMEOUT (EXPECT C OR NAK)
		CALL	TIMED_GETCHAR

		JC	XM_CANCEL	;Cancel download if no response within 45 seconds
		CPI	NAK
		JZ	XMS_SEND	;Loop back to resend packet
		CPI	CAN
		JZ	XM_CANCEL
		CPI	ACK
		JNZ	XM_CANCEL

		LDA	XMSEQ
		INR	A		;NEXT SEQ
		STA	XMSEQ
		RET


;---------------------------------------------------------------------------------
;XMDN - DOWNLOAD XMODEM PACKET
;---------------------------------------------------------------------------------
XMS_EOT		MVI	A,EOT		;HANDLE THE END OF TRANSFER FOR CRC OR CHECKSUM
		CALL	PUT_CHAR
		MVI	A,45		;GET CHAR, 45 SECONDS TIMEOUT (EXPECT C OR NAK)
		CALL	TIMED_GETCHAR
		JC	XM_CANCEL
		CPI	NAK
		JZ	XMS_EOT
		CPI	ACK
		JNZ	XM_CANCEL

XM_DONE		CALL	PRINTI
		.text "\r\nTRANSFER COMPLETE\r\n\000"
		XRA	A		;CLEAR A, CY
		RET

;FINISHING CODE PRIOR TO LEAVING XMODEM
XM_CANCEL	MVI	A,CAN
		CALL	PUT_CHAR
		CALL	PUT_CHAR
		CALL	PURGE
		CALL	PRINTI
		.text "TRANSFER CANCELED\r\n\000"
		POP	B		;SCRAP CALLING ROUTINE AND HEAD TO PARENT
		RET






;---------------------------------------------------------------------------------
;START XMODEM RECEIVING and RECEIVE FIRST PACKET
;---------------------------------------------------------------------------------
XMR_INIT	MVI	E,45		;ATTEMPTS TO INITIATE XMODEM CRC TRANSFER
		MVI	A,1		;EXPECTED SEQ NUMBER starts at 1
		STA	XMSEQ
XMR_CRC		CALL	PURGE
		MVI	A,'C'		;Send C
		STA	XMTYPE		;Save as XM Type (CRC or CS)
		CALL	PUT_CHAR
		CALL	XMGET_HDR	;Await a packet
		JNC	XMR_TSEQ	;Jump if first packet received
		JNZ	XM_CANCEL	;Cancel if there was a response that was not a header
		DCR	E		;Otherwise, if no response, retry a few times
		JNZ	XMR_CRC

		MVI	E,15		;ATTEMPTS TO INITIATE XMODEM CHECKSUM TRANSFER
XMR_CS		CALL	PURGE
		MVI	A,NAK		;Send NAK
		STA	XMTYPE		;Save as XM Type (CRC or CS)
		CALL	PUT_CHAR
		CALL	XMGET_HDR	;Await a packet
		JNC	XMR_TSEQ	;Jump if first packet received
		JNZ	XM_CANCEL	;Cancel if there was a response that was not a header
		DCR	E		;Otherwise, if no response, retry a few times
		JNZ	XMR_CS
		JMP	XM_CANCEL	;Abort


;--------------------- XMODEM RECEIVE
;Entry:	XMR_TSEQ in the middle of the routine
;Pre:	C=1 (expected first block as received when negogiating CRC or Checksum)
;	HL=Memory to dump the file to
;Uses:	B to count the 128 bytes per block
;	C to track Block Number expected
;	DE as CRC (Within Loop) (D is destroyed when Getting Header)
;------------------------------------
XMR_RECV	
XMR_LP		CALL	XMGET_HDR
		JNC	XMR_TSEQ
		JZ	XMR_NAK		;NACK IF TIMED OUT
		CPI	EOT
		JNZ	XM_CANCEL	;CANCEL IF CAN RECEIVED (OR JUST NOT EOT)
		MVI	A,ACK
		CALL	PUT_CHAR
		JMP	XM_DONE

XMR_TSEQ	MOV	C,A		;Save Seq number from packet
		LDA	XMSEQ
		CMP	C		;CHECK IF THIS SEQ IS EXPECTED
		JZ	XMR_SEQ_OK	;Jump if CORRECT SEQ
		DCR	A		;Else test if Previous SEQ
		STA	XMSEQ
		CMP	C
		JNZ	XM_CANCEL	;CANCEL IF SEQUENCE ISN'T PREVIOUS BLOCK
		CALL	PURGE		;ELSE, PURGE AND SEND ACK (ASSUMING PREVIOUS ACK WAS NOT RECEIVED)
		JMP	XMR_ACK

XMR_SEQ_OK	MVI	B,128		;128 BYTES PER BLOCK
		MVI	C,0		;Clear Checksum
		LXI	D,0000H		;CLEAR CRC
		LXI	H,SECTOR_BUFFER	;Default Buffer
XMR_BLK_LP	CALL	TIMED1_GETCHAR
		JC	XMR_NAK
		MOV	M,A		;SAVE DATA BYTE
		CALL	CRC_UPDATE
		MOV	A,M		;Update checksum
		ADD	C
		MOV	C,A
		INX	H		;ADVANCE
		DCR	B
		JNZ	XMR_BLK_LP
					;After 128 byte packet, verify error checking byte(s)
		LDA	XMTYPE		;Determine if we are using CRC or Checksum
		CPI	NAK		;If NAK, then use Checksum
		JZ	XMR_CCS
		
		CALL	TIMED1_GETCHAR	;Check with CRC
		JC	XMR_NAK
		CMP	D
		JNZ	XMR_NAK
		CALL	TIMED1_GETCHAR
		JC	XMR_NAK
		CMP	E
		JNZ	XMR_NAK
		JMP	XMR_SAVE

XMR_CCS		CALL	TIMED1_GETCHAR
		JC	XMR_NAK
		CMP	C
		JNZ	XMR_NAK

		;If we were transfering to a FILE, this is where we would write the
		;sector and reset HL to the same 128 byte sector buffer.
		;CALL	WRITE_SECTOR

XMR_SAVE	LHLD	XSECTOR
		MOV	B,H
		MOV	C,L
		INX	H
		SHLD	XSECTOR
		CALL	WRITE_SEC


XMR_ACK		MVI	A,ACK		;The sending of the Ack is done by
		CALL	PUT_CHAR	;the calling routine, to allow writes to disk
			
		LDA	XMSEQ
		INR	A		;Advance to next SEQ BLOCK
		STA	XMSEQ
		STC			;Carry set when NOT last packet
		RET

XMR_NAK		CALL	PURGE
		MVI	A,NAK
		CALL	PUT_CHAR
		JMP	XMR_LP


;--------------------- XMODEM - GET HEADER
;
;pre:	Nothing
;post:	Carry Set: A=0, (Zero set) if Timeout
;	Carry Set: A=CAN (Not Zero) if Cancel received
;	Carry Set: A=EOT (Not Zero) if End of Tranmission received
;	Carry Clear and A = B = Seq if Header found and is good
;------------------------------------------
XMGET_HDR	CALL	TIMED1_GETCHAR	;GET CHAR, 1 SECONDS TIMEOUT (EXPECT SOH)
		RC			;Return if Timed out
		CPI	SOH		;TEST IF START OF HEADER
		JZ	GS_SEQ		;IF SOH RECEIVED, GET SEQ NEXT
		CPI	EOT		;TEST IF END OF TRANSMISSION
		JZ	GS_ESC		;IF EOT RECEIVED, TERMINATE XMODEM
		CPI	CAN		;TEST IF CANCEL
		JNZ	XMGET_HDR
GS_ESC		ORA	A		;Clear Z flag (because A<>0)
		STC
		RET
GS_SEQ		CALL	TIMED1_GETCHAR	;GET SEQ CHAR
		RC			;Return if Timed out
		MOV	B,A		;SAVE SEQ
		CALL	TIMED1_GETCHAR	;GET SEQ COMPLEMENT
		RC			;Return if Timed out
		CMA
		CMP	B		;TEST IF SEQ VALID
		JNZ	XMGET_HDR	;LOOP BACK AND TRY AGAIN IF HEADER INCORRECT (SYNC FRAME)
		RET

;------------------------------------------ CRC_UPDATE
;HANDLE THE CRC CALCULATION FOR UP/DOWNLOADING
;Total Time=775 cycles = 388uSec
;In:	A  = New char to roll into CRC accumulator
;	DE = 16bit CRC accumulator
;Out:	DE = 16bit CRC accumulator
;------------------------------------------
;CRC_UPDATE	XRA	D		;4
;		MOV	D,A		;5
;		PUSH	B		;11
;		MVI	B,8		;7	PRELOOP=27
;CRCU_LP	ORA	A		;4	CLEAR CARRY
;		MOV	A,E		;5
;		RAL			;4
;		MOV	E,A		;5
;		MOV	A,D		;5
;		RAL			;4
;		MOV	D,A		;5
;		JNC	CRCU_NX		;10
;		MOV	A,D		;5
;		XRI	10h		;7
;		MOV	D,A		;5
;		MOV	A,E		;5
;		XRI	21H		;7
;		MOV	E,A		;5
;CRCU_NX		DCR	B		;5
;		JNZ	CRCU_LP		;10	LOOP=91*8 (WORSE CASE)
;		POP	B		;10	POSTLOOP=20
;		RET			;10


;------------------------------------------ CRC_UPDATE
;HANDLE THE CRC CALCULATION FOR UP/DOWNLOADING
;Total Time=604 cycles = 302uSec MAX
;In:	A  = New char to roll into CRC accumulator
;	DE = 16bit CRC accumulator
;Out:	DE = 16bit CRC accumulator
;------------------------------------------
CRC_UPDATE	XCHG			;4
		XRA	H		;4
		MOV	H,A		;5
		DAD	H		;10	Shift HL Left 1
		CC	CRC_UPC		;17 (10/61)
		DAD	H		;10	Shift HL Left 2
		CC	CRC_UPC		;17
		DAD	H		;10	Shift HL Left 3
		CC	CRC_UPC		;17
		DAD	H		;10	Shift HL Left 4
		CC	CRC_UPC		;17
		DAD	H		;10	Shift HL Left 5
		CC	CRC_UPC		;17
		DAD	H		;10	Shift HL Left 6
		CC	CRC_UPC		;17
		DAD	H		;10	Shift HL Left 7
		CC	CRC_UPC		;17
		DAD	H		;10	Shift HL Left 8
		CC	CRC_UPC		;17
		XCHG			;4
		RET			;10

CRC_UPC		MOV	A,H		;5
		XRI	10h		;7
		MOV	H,A		;5
		MOV	A,L		;5
		XRI	21H		;7
		MOV	L,A		;5
		RET			;10


;===============================================
;TIMED1_GETCHAR - Gets a character within 1 second
;
;pre:	nothing
;post: 	Carry Set = No Char, Time Out
;	Carry Clear, A = Char
;-----------------------------------------------
TIMED1_GETCHAR	MVI	A,1

;===============================================
;TIMED_GETCHAR - Gets a character within a time limit
;
;pre:	A contains # of seconds to wait before returning
;post: 	Carry Set & Zero Set = No Char, Time Out
;	Carry Clear, A = Char
;-----------------------------------------------
TIMED_GETCHAR	PUSH	D
		PUSH	B
		MOV	D,A
		LXI	B,0	;B,C=Loop Count down until timeout
TGC_LP1		LDA	GC_OPTIONS
		RRC
		MVI	B,8		;XX = 1 Second
		JC	TGC_LP2
		MVI	B,66		;XX = 1 Second
TGC_LP2		CALL	GET_STAT	;45  TEST FOR RX DATA
		JNZ	TGC_DO	;10
		DCR	C	;5
		JNZ	TGC_LP2	;10	;73 Cycles Loop time. 39*256*.5 ~= 9.3 mSec
		DCR	B
		JNZ	TGC_LP2	;	1 Second waiting
		DCR	D		;Count down Seconds until Time Out
		JNZ	TGC_LP1
		STC		;SET CARRY TO INDICATE TIME OUT
		;MVI	A,0
		JMP	TGC_RET
TGC_DO		CALL	GET_CHAR
TGC_RET		POP	B
		POP	D
		RET


;===============================================
;PURGE - Clears all in coming bytes until the line is clear for a full 2 seconds
;-----------------------------------------------
PURGE		MVI	A,2	;2 seconds for time out
		CALL	TIMED_GETCHAR
		JNC	PURGE
		RET


ECHO_FP_OFF	XRA	A
		STA	GC_OPTIONS	;Echo Off, FP Off
		JMP	LED_OFF		



;XModem implementation on 8080 Monitor (CP/M-80)
;
;Terminal uploads to 8080 system:
;-Terminal user enters command "XU aaaa"
;-8080 "drives" the protocol since it's the receiver
;-8080 sends <Nak> every 10 seconds until the transmitter sends a packet
;-if transmitter does not begin within 10 trys (100 seconds), 8080 aborts XMODEM
;-a packet is:
; <SOH> [seq] [NOT seq] [128 bytes of data] [checksum or CRC]
;
;<SOH> = 1 (Start of Header)
;<EOT> = 4 (End of Transmission)
;<ACK> = 6
;<DLE> = 16
;<DC1> = 17 (X-ON)
;<DC3> = 19 (X-OFF)
;<NAK> = 21
;<SYN> = 22
;<CAN> = 24 (Cancel)
;
;Checksum is the Modulo 256 sum of all 128 data bytes
;
;                                     <<<<<          [NAK]
;       [SOH][001][255][...][csum]    >>>>>
;                                     <<<<<          [ACK]
;       [SOH][002][254][...][csum]    >>>>>
;                                     <<<<<          [ACK]
;       [SOH][003][253][...][csum]    >>>>>
;                                     <<<<<          [ACK]
;       [EOT]                         >>>>>
;                                     <<<<<          [ACK]
;
;-if we get <EOT> then ACK and terminate XModem
;-if we get <CAN> then terminate XModem
;-if checksum invalid, then NAK
;-if seq number not correct as per [NOT seq], then NAK
;-if seq number = previous number, then ACK (But ignore block)
;-if seq number not the expected number, then <CAN><CAN> and terminate XModem
;-if data not received after 10 seconds, then NAK (inc Timeout Retry)
;-if timeout retry>10 then <CAN><CAN> and terminate XModem
;
;-To keep synchronized,
;  -Look for <SOH>, qualify <SOH> by checking the [seq] / [NOT seq]
;  -if no <SOH> found after 135 chars, then NAK
;
;-False EOT condtion
;  -NAK the first EOT
;  -if the next char is EOT again, then ACK and leave XModem
;
;-False <CAN>, expect a 2nd <CAN> ?
;
;-Using CRC, send "C" instead of <NAK> for the first packet
;  -Send "C" every 3 seconds for 3 tries, then degrade to checksums by sending <NAK>
;
;
;
;* The character-receive subroutine should be called with a
;parameter specifying the number of seconds to wait.  The
;receiver should first call it with a time of 10, then <nak> and
;try again, 10 times.
;  After receiving the <soh>, the receiver should call the
;character receive subroutine with a 1-second timeout, for the
;remainder of the message and the <cksum>.  Since they are sent
;as a continuous stream, timing out of this implies a serious
;like glitch that caused, say, 127 characters to be seen instead
;of 128.
;
;* When the receiver wishes to <nak>, it should call a "PURGE"
;subroutine, to wait for the line to clear.  Recall the sender
;tosses any characters in its UART buffer immediately upon
;completing sending a block, to ensure no glitches were mis-
;interpreted.
;  The most common technique is for "PURGE" to call the
;character receive subroutine, specifying a 1-second timeout,
;and looping back to PURGE until a timeout occurs.  The <nak> is
;then sent, ensuring the other end will see it.
;
;* You may wish to add code recommended by Jonh Mahr to your
;character receive routine - to set an error flag if the UART
;shows framing error, or overrun.  This will help catch a few
;more glitches - the most common of which is a hit in the high
;bits of the byte in two consecutive bytes.  The <cksum> comes
;out OK since counting in 1-byte produces the same result of
;adding 80H + 80H as with adding 00H + 00H.




;------------------------------------------------------ Subroutines
		;All ROM space is "Shadow ROM", meaning, reads are from ROM and any
		;memory writes will happen to the RAM at that address.
		;The Shadow ROM can be turned off, thus allowing the RAM to be read.
		;
		;ROM_LOW, when low, enables the ROM bank selected in the lower 32K space.
		;ROM_Hi, when hi, enables 16K of the ROM bank selected in the first 16K of
		;the upper 32K space.
;ROM_HI		.EQU 040H	;Set to map ROM to High Address (Clr = RAM)
;ROM_LOW	.EQU 041H	;Clear to map ROM to Low Addres (Set = RAM)
		
RAM_CODE



;	READ SECTOR FROM RAM
;512K RAM  allows for 64K System + 14 Tracks of 256 Sectors
;BANK
;0 - 64K System RAM, Stack, etc.
;1 - 512 Sectors    0 to  511   Track 0, 1
;2 - 512 Sectors  512 to 1023	Track 2, 3
;3 - 512 Sectors 1024 to 1535	Track 4, 5
;4 - 512 Sectors 1536 to 2047	Track 6, 7
;5 - 512 Sectors 2048 to 2559	Track 8, 9
;6 - 512 Sectors 2560 to 3071	Track 10, 11
;7 - 512 Sectors 3072 to 3583	Track 12, 13        3583 = 0x0DFF
;
;Total 3584 Sectors in RAM
;
;32K ROM allows for 16K of additional ROM DISK area for Read Only Files
;We must use this space entirely for Files.  
;CP/M Must not be allowed to consider any of this space writable.
;PIP.COM 8K  (4 blocks, 224, 225, 226, 227) Sectors 0x0E00 to 0x0E3F
;XM.COM  2K  (1 block, 228) Sectors 0x0E40 to 0x0E4F.  
;Sector 0x0E4F is not used in XM.COM, so it is used to set the initial directory at Sector 0
;1 Block = 2K = 16 Sectors
;
;8 sectors = 1K, but we must go by blocks of 16 sectors (2K)
;
;ROM	Sec
;  00	0E00	PIP.COM
;  00	0E10	PIP.COM
;  00	0E20	PIP.COM
;  00	0E30	PIP.COM
;  00	0E40	XM.COM
;  00	0E50	spare
;  00	0E60	spare
;  00	0E70	spare
;
;64K ROM 27512
;8000	0E80	spare
;8800	0E90	spare
;9000	0EA0	spare
;9800	0EB0	spare
;A000	0EC0	spare
;A800	0ED0	spare
;B000	0EE0	spare
;B800	0EF0	spare
;C000	0F00	spare
;C800	0F10	spare
;D000	0F20	spare
;D800	0F30	spare
;E000	0F40	spare
;E800	0F50	spare
;F000	0F60	spare
;F800	0F70	spare

;If reading from an even track, we need to read RAM in the lower 32K, therefore we must
;run the routine in upper 32K
;If reading from an odd track, we need to read RAM in upper 32K.
;Writting to RAM is never a problem.
;Routine must run from ROM, since bank switching RAM will not affect ROM.

					;Prompt User for Sector number to read
READ_SECTOR_T	CALL	SPACE_GET_WORD	;Input Sector
		MOV	B,D	;BC <- DE
		MOV	C,E
		CALL	READ_SEC_DB	;Read Sector into Default Buffer

		;LXI	H,SECTOR_BUFFER	;Dump Sector to screen
		LXI	D,0FFFFH
		JMP	MEM_DUMP_LP

		
READ_SEC_DB	LXI	H,SECTOR_BUFFER	;Default Buffer
		SHLD	DMAADD	;Destination, CP/M Data Buffer


			;Entry, BC = Sector#
			;	DMAADD = Location to store Sector
READ_SEC	PUSH	PSW
		PUSH	B
		PUSH	D
		PUSH	H
		
		;CALL	PRINTI
		;.text " R:\000"
		;CALL	PUT_BC

		MVI	A,0E4H		;TXDATA HIGH, RED LED ON
		OUT	OUTPUT_PORT	;Set LED ON

		CALL	HASH_SECTOR	;Convert Sector in BC to Address in DE and Bank in A
					;DE =ADDRESS, A=BANK & HIGH/LOW 32K.  Z=1 for ROM
					
		DI		;No interrupts (due to RAM BANKING)
		JZ	RS_ROM_SEC	;Read from ROM Sector
		LXI	H,0	;Save Stack
		DAD	SP
		SHLD	SP_TEMP
		XCHG		;HL = Read address
		SPHL		;SP = Read address
		LHLD	DMAADD	;Destination, CP/M Data Buffer (in Bank 0)

		RAR
		JNC	RS_ROM_HI ;Reading from LOW 32K RAM, so code must Continue Read in HIGH ROM
				
				;Else, Reading from HIGH 32K RAM
				;Bank of Source Sector in A
				;Bank of Source Sector in A, A is also loop counter
		ANI	7	;c=0, upper 5 bits=0
RS_LOOP		OUT	B16	;Write A16 ;Set 1 of 8 Banks for 512KB
		RAR
		OUT	B17	;Write A17
		RAR
		OUT	B18	;Write A18
		RAL
		RAL
				
		POP	D	;Read 4 bytes
		POP	B
		
		RAL		;Bank 0 (because c is 0)
		OUT	B16	;Clr A16
		OUT	B17	;Clr A17
		OUT	B18	;Clr A18
		RAR
		
		MOV	M,E
		INX	H
		MOV	M,D
		INX	H
		MOV	M,C
		INX	H
		MOV	M,B
		INX	H
		ADI	8	;c=1 after 32 itterations
		JNC	RS_LOOP

RS_EXIT		LHLD	SP_TEMP
		SPHL		;SP = Restored
		
RS_EXIT2	EI
		MVI	A,80H		;TXDATA HIGH
		OUT	OUTPUT_PORT	;Set LED off
		POP	H
		POP	D
		POP	B
		POP	PSW
		RET

				;Sector is in ROM
RS_ROM_SEC	LHLD	DMAADD	;Destination, CP/M Data Buffer
		XCHG		;HL = Read address, DE = Destination Address
		MOV	A,H	;Offset to start of EPROM ROM DRIVE
		ADI	ROM_DISK >>8	;ROM_DISK
		MOV	H,A
		JM	RS_ERR_RH ;Jump to map in Upper ROM, not implemented.

		MVI	B,128
		CALL	COPY_RAM	;Copy the ROM_SECTOR to CP/M DMAADD
		JMP	RS_EXIT2

RS_ERR_RH	CALL	PRINTI		;System Start, Display Welcome Message
		.text "\r\nUpper EPROM not implemented\000"
		JMP	RS_EXIT2


RS_ROM_HI	STC
		RAL
		OUT	ROM_HI	;MAP ROM TO HIGH ADDRESS
		JMP	RS_ROM_HIGH+8000H ;CONTINUE EXECUTION HIGH
RS_ROM_HIGH	OUT	ROM_LOW	;MAP LOW ROM OUT
		RAR

; RLC       | Rotate A Left,  CY<-MSB
; RRC       | Rotate A Right, CY<-LSB
; RAL       | Rotate A Left  through Carry
; RAR       | Rotate A Right through Carry
		
				;Bank of Source Sector in A, A is also loop counter
		ANI	7	;c=0, upper 5 bits=0
RSRH_LOOP	OUT	B16	;Write A16 ;Set 1 of 8 Banks for 512KB
		RAR		
		OUT	B17	;Write A17
		RAR
		OUT	B18	;Write A18
		RAL
		RAL
		
		POP	D	;Read 4 bytes
		POP	B
		
		RAL		;Bank 0 (because c = 0)
		OUT	B16	;Clr A16
		OUT	B17	;Clr A17
		OUT	B18	;Clr A18
		RAR
		
		MOV	M,E
		INX	H
		MOV	M,D
		INX	H
		MOV	M,C
		INX	H
		MOV	M,B
		INX	H
		ADI	8	;c=1 after 32 itterations
		JNC	RSRH_LOOP+8000H
		
		XRA	A
		OUT	ROM_LOW	;MAP LOW ROM BACKIN
		JMP	RS_ROM_LOW ;CONTINUE EXECUTION LOW
RS_ROM_LOW	OUT	ROM_HI	;MAP HIGH ROM OUT
		JMP	RS_EXIT



;	WRITE SECTOR TO RAM
;
;It does not matter if the RAM we write to is in Shadow ROM space, since all Writes go to RAM.
;This routine will easily work from LOW ROM
;Routine must run from ROM, since bank switching RAM will not affect ROM.


WRITE_SECTOR_T	CALL	SPACE_GET_WORD	;Input Sector
		MOV	B,D	;BC <- DE
		MOV	C,E

			;Entry, BC = Sector#
WRITE_SEC	PUSH	PSW
		PUSH	B
		PUSH	D
		PUSH	H

		;CALL	PRINTI
		;.text " W:\000"
		;CALL	PUT_BC
		
		
		MVI	A,0E8H		;TXDATA HIGH, Blue LED ON
		OUT	OUTPUT_PORT	;Set LED ON
		
		CALL	HASH_SECTOR	;Convert Sector in BC to Address in DE and Bank in A
		
		JZ	WRITE_ERR
		DI
		LXI	H,0
		DAD	SP
		SHLD	SP_TEMP
		LXI	SP,SECTOR_BUFFER
		XCHG		;HL = Write address

		;ORA	A	;Clear carry
		RAR		;Shift out High/Low 32K, leave BANK only in A
				
		ANI	7	;c=0, upper 5 bits=0
WS_LOOP		RAL		;Bank 0 (because c = 0)
		OUT	B16	;Clr A16
		OUT	B17	;Clr A17
		OUT	B18	;Clr A18
		RAR

		POP	D	;Read 4 bytes
		POP	B
		
				;Bank of Destination Sector in A
		OUT	B16	;Write A16 ;Set 1 of 8 Banks for 512KB
		RAR		
		OUT	B17	;Write A17
		RAR
		OUT	B18	;Write A18
		RAL
		RAL
		
		
		MOV	M,E
		INX	H
		MOV	M,D
		INX	H
		MOV	M,C
		INX	H
		MOV	M,B
		INX	H
		ADI	8	;c=1 after 32 itterations
		JNC	WS_LOOP

		XRA	A
		OUT	B16	;Clr A16
		OUT	B17	;Clr A17
		OUT	B18	;Clr A18

		LHLD	SP_TEMP
		SPHL		;SP = Restored
		
		EI
WS_EXIT		MVI	A,80H		;TXDATA HIGH
		OUT	OUTPUT_PORT	;Set LED off

		POP	H
		POP	D
		POP	B
		POP	PSW
		RET

WRITE_ERR	CALL	PRINTI		;
		.text "\r\nWrite to ROM Fail:\000"
		CALL	PUT_HEX
		JMP	WS_EXIT


;Input:	BC has Sector number.  Valid are 0000 to 0F7F
;				RAM is 0000 to 0DFF
;				ROM is 0E00 to 0F7F
;
;Output: DE = Memory Address in RAM
;	 A  = Bank and High/Low 32 K
;	Zero Flag set if Sector is in ROM
;
		;B:C   xxxxbbbA : BCDEFGHI   512 Sectors per 64KB block
		;to
		;D:E             ABCDEFGH : I0000000  = Base Address of Sec
		;A     xxxxnnnA  (where nnn = bbb + 1) 0 to 6 now becomes bank 1 to 7 for RAM
		;					7 now becomes bank 8 for ROM (but with Zero Flag SET)
				
HASH_SECTOR	MOV	A,B	;A = xxxxbbbA
		RAR		;A = xxxxxbbb   c=A   (Put bit A in Carry)
		MOV	A,C	;A = BCDEFGHI
		RAR		;A = ABCDEFGH   c=I
		MOV	D,A	;D = ABCDEFGH
		
		MVI	A,0	;A = 00000000
		RAR		;A = I0000000
		MOV	E,A	;E = I0000000   (lower/upper 128 bytes of a 256 page)
		
		MOV	A,B	;A = xxxxbbbA
		INR	A	;Set Bank base = 1 (Bank never to exceed 7)
		INR	A	;Set Bank base = 1 (Bank never to exceed 7)
		CPI	10H
		RC		;If Bank <=7 Return
		RZ		;If Bank =8 Return
		CPI	11H
		RZ		;If Bank =8 Return

		CALL	PRINTI		;
		.text "\r\nInvalid Sector:\000"
		CALL	PUT_HEX
		
		LXI	B,0
		JMP	HASH_SECTOR
		

; RLC       | Rotate A Left,  CY<-MSB
; RRC       | Rotate A Right, CY<-LSB
; RAL       | Rotate A Left  through Carry
; RAR       | Rotate A Right through Carry

;HASH_SECTOR	MOV	A,C	;B:C   xxxxbbbA : BCDEFGHI   512 Sectors per 64KB block
;      		RRC		;to
;    		ANI	80H	;D:E             ABCDEFGH : I0000000  = Base Address of Sec
;      		MOV	E,A
;		
;      		MOV	A,B
;     		RAR
;  		INR	A	;Set Bank base = 1 (Bank never to exceed 7)
;   		PUSH	PSW
; 		CPI	8
;		JNC	SEC_ERROR ;Invalid Sector # (messy Stack)
;      		POP	PSW		
;  		PUSH	PSW
;      		MOV	A,C
;    		RAR
;     		MOV	D,A
;      		POP	PSW	;A = BANK, CARRY = HIGH/LOW 32K
;      		RAL
;      		RET
		
		
;LOOP_BACK	CALL	GET_CHAR
;		JZ	LOOP_BACK
;		CALL	PUT_CHAR
;		JMP	LOOP_BACK


CHKSUM_ROM	LXI	H,0		;Start Address = 0
		XRA	A
		MOV	D,A		;Clear MSB of checksum
		LXI	B,8000H
CSR_LOOP	ADD	M
		JNC	CSR_NEXT
		INR	D
CSR_NEXT	INX	H
		DCR	C
		JNZ	CSR_LOOP
		DCR	B
		JNZ	CSR_LOOP
		MOV	E,A
		RET


CHKSUM_RAM_SET	CALL	CHKSUM_RAM
		XCHG
		SHLD	RAMDISK_CS
		RET

CHKSUM_RAM	CALL	PRINTI		;System Start, Display Welcome Message
		.text "\r\nRAM DISK Chksum=\000"

		MVI	B,7	;Bank = 7
		XRA	A
		MOV	D,A
		MOV	E,A
		STA	INT_ENABLED
		DI
		LXI	H,0	;Save Stack
		DAD	SP
		SHLD	SP_TEMP

		MVI	A,0E4H		;TXDATA HIGH, RED LED ON
		CALL	LED_OUT		;Set LED ON
		
CSA_LOOP1	MOV	A,B
		OUT	B16	;Set A16 ;Set 1 of 8 Banks for 512KB
		RRC		;Can't CALL/PUSH/POP, RAM is shifting under our feet
		OUT	B17	;Set A17
		RRC
		OUT	B18	;Set A18

		MOV	A,B
		ORI	0A8H
		OUT	OUTPUT_PORT	;Set LED


		LXI	SP,08000H	;Use SP for Check Sum RAM DISK
		LXI	H,04000H
		MOV	A,E
		MOV	E,B	;Save Bank into E
CSA_LOOP2	POP	B	;11
		ADD	B
		JNC	CSA_NEXT1
		INR	D
CSA_NEXT1	
		ADD	C
		JNC	CSA_NEXT2
		INR	D
CSA_NEXT2
		DCR	L	
		JNZ	CSA_LOOP2
		DCR	H	
		JNZ	CSA_LOOP2
		
		MOV	B,E	;Restore Bank
		MOV	E,A	;Save Checksum Accumulator

		XRA	A
		OUT	B16	;Set A16 ;Bank 0
		OUT	B17	;Set A17
		OUT	B18	;Set A18
		
		LHLD	SP_TEMP
		SPHL		;SP = Restored
		
		DCR	B
		JNZ	CSA_LOOP1

		MVI	A,1
		OUT	ROM_HI	;MAP ROM TO HIGH ADDRESS
		JMP	CSAH_ROM_HIGH+8000H ;CONTINUE EXECUTION HIGH
CSAH_ROM_HIGH	OUT	ROM_LOW	;MAP LOW ROM OUT

		MVI	B,7	;Bank = 7
		
CSAH_LOOP1	MOV	A,B
		OUT	B16	;Set A16 ;Set 1 of 8 Banks for 512KB
		RRC		;Can't CALL/PUSH/POP, RAM is shifting under our feet
		OUT	B17	;Set A17
		RRC
		OUT	B18	;Set A18
		
		MOV	A,B
		ORI	0A0H
		OUT	OUTPUT_PORT	;Set LED

		LXI	SP,00000H	;Use SP for Check Sum RAM DISK
		LXI	H,04000H
		MOV	A,E
		MOV	E,B	;Save Bank into E
CSAH_LOOP2	POP	B	;11
		ADD	B
		JNC	CSAH_NEXT1+8000H
		INR	D
CSAH_NEXT1	
		ADD	C
		JNC	CSAH_NEXT2+8000H
		INR	D
CSAH_NEXT2
		DCR	L	
		JNZ	CSAH_LOOP2+8000H
		DCR	H	
		JNZ	CSAH_LOOP2+8000H
		
		MOV	B,E	;Restore Bank
		MOV	E,A	;Save Checksum Accumulator

		DCR	B
		JNZ	CSAH_LOOP1+8000H

		XRA	A
		OUT	B16	;Set A16 ;Bank 0
		OUT	B17	;Set A17
		OUT	B18	;Set A18

		XRA	A
		OUT	ROM_LOW	;MAP LOW ROM BACKIN
		JMP	CSA_ROM_LOW ;CONTINUE EXECUTION LOW
CSA_ROM_LOW	OUT	ROM_HI	;MAP HIGH ROM OUT

		LHLD	SP_TEMP
		SPHL		;SP = Restored

		DCR	A	;A=FF
		STA	INT_ENABLED
		
		CALL	PUT_DE

		RET


;===============================================
;PUT_NEW_LINE -- Start a new line on the console
;
;pre: none
;post: 0x0A printed to console
;-----------------------------------------------
PUT_NEW_LINE:	CALL	PRINTI
		.text "\r\n\000"
		RET

;===============================================
;PUT_BC Prints BC Word
;-----------------------------------------------
PUT_BC:		PUSH	PSW
		MOV	A, B
		CALL	PUT_BYTE
		MOV	A, C
		CALL	PUT_BYTE
		POP	PSW
		RET

;===============================================
;PUT_DE Prints DE Word
;-----------------------------------------------
PUT_DE:		PUSH	PSW
		MOV	A, D
		CALL	PUT_BYTE
		MOV	A, E
		CALL	PUT_BYTE
		POP	PSW
		RET

;===============================================
;PUT_HL Prints HL Word
;-----------------------------------------------
PUT_HL:		PUSH	PSW
		MOV	A, H
		CALL	PUT_BYTE
		MOV	A, L
		CALL	PUT_BYTE
		POP	PSW
		RET

;===============================================
;PUT_BYTE -- Output byte to console as hex
;
;pre:	A register contains byte to be output
;post:
;-----------------------------------------------
PUT_BYTE:	PUSH	PSW
		RRC
		RRC
		RRC
		RRC
		CALL	PUT_HEX
		POP	PSW
		PUSH	PSW
		CALL	PUT_HEX
		POP	PSW
		RET

;===============================================
;PUT_HEX -- Convert nibble to ASCII char
;
;pre: A register contains nibble
;post: A register contains ASCII char
;-----------------------------------------------
PUT_HEX:	ANI	0Fh
		ADI	90h
		DAA
		ACI	40h
		DAA
		JMP	PUT_CHAR


;===============================================
;PRINT -- Print a null-terminated string
;
;pre: HL contains pointer to start of a null-
;     terminated string
;-----------------------------------------------
PRINT:		MOV	A, M
		INX	H
		ORA	A
		RZ
		CALL	PUT_CHAR
		JMP	PRINT

MSG_COLD_BOOT	.text "COLD\000"
MSG_WARM_BOOT	.text "WARM\000"
MSG_GOOD	.text " =GOOD\000"
MSG_BAD		.text " =BAD\000"

;===============================================
;PRINT IMMEDIATE
;-----------------------------------------------
PRINTI:		XTHL	;HL = Top of Stack
		PUSH	PSW
		CALL	PRINT
		POP	PSW
		XTHL	;Move updated return address back to stack
		RET

;===============================================
;PRINT B-LENGTH
;-----------------------------------------------
PRINTB		MOV	A, M
		CALL	PUT_CHAR
		INX	H
		DCR	B
		JNZ	PRINTB
		RET



				;Get status of buffer,	ACC=0, Z=1 if Buffer Empty
				;			ACC=FF, Z=0 if Buffer has chars
GET_STAT	PUSH	H
		LXI	H,FP_PRECOUNT
		DCR	M
		CZ	FP_OPERATE
		LHLD	RXBTAIL	;HL = Tail of Buffer
		LDA	RXBHEAD	;Test if Buffer Empty
		SUB	L
		POP	H
		RZ		;Exit if Tail = Head (=Empty)
		MVI	A,0FFH
		RET


ECHO_OFF	LDA	GC_OPTIONS	;Echo off
		ANI	7FH
		STA	GC_OPTIONS
		RET

				;Gets next Char from Buffer,  	ACC=0, Z=1 if Buffer Empty
				;				ACC = Char if Buffer not Empty
GET_CHAR	CALL	GET_STAT
		JZ	GET_CHAR
		PUSH	H
		LHLD	RXBTAIL	;HL = Tail of Buffer
		LDA	GC_OPTIONS
		RLC		
		MOV	A,M
		INR	L
		SHLD	RXBTAIL
		POP	H
		RNC		;Return if NO Echo
		
		CPI	' '	;Do not echo control chars
		RM

PUT_CHAR:	PUSH	H
		LHLD	PC_PTR
		;PCHL			;Jump to Put_Char1, Put_Char2, Put_Char3
JMP_HL		PCHL		;PC = HL


PUT_CHAR1	;PUSH	H		;H makes Delay, L holds Output Port Latch
		PUSH	B		;D makes Delay, E holds Output Port
		MOV	B,A
		MVI	C,9
		LDA	OUTPUT_LAST
		RLC
		MOV	L,A

		STC			;Start Bit
		CMC
		DI
PC1_BIT_LOOP	MOV	A,L
		RAR
		OUT	OUTPUT_PORT	;Set RS232 TX
		CALL	PC1_DELAY
		MOV	A,B
		RRC
		MOV	B,A
		DCR	C
		JNZ	PC1_BIT_LOOP	;208t = 104uSec
		
		MVI	H,9
		JMP	PC_EXIT		


PUT_CHAR2	;PUSH	H		;H makes Delay, L holds Output Port Latch
		PUSH	B		;B holds Char to send, C counts bits
		MOV	B,A
		MVI	C,9
		LDA	OUTPUT_LAST
		RLC
		MOV	L,A

		STC			;Start Bit
		CMC
		DI
PC2_BIT_LOOP	MOV	A,L		;5
		RAR			;4
		OUT	OUTPUT_PORT	;10 Set RS232 TX
		CALL	PC2_DELAY
		MOV	A,B		;5
		RRC			;4
		MOV	B,A		;5
		DCR	C		;5
		JNZ	PC2_BIT_LOOP	;10   104t = 52uSec
		
		MVI	H,1
		JMP	PC_EXIT		

					;38,400, 52t = 26uSec
PUT_CHAR3	;PUSH	H		;H makes Delay, L holds Output Port Latch
		PUSH	B		;B holds Char to send, C counts bits
		MOV	B,A
		MVI	C,9
		LDA	OUTPUT_LAST
		RLC
		MOV	L,A

		STC			;Start Bit
		CMC
		DI
PC3_BIT_LOOP	MOV	A,L		;5
		RAR			;4
		OUT	OUTPUT_PORT	;10 Set RS232 TX
		MOV	A,B		;5
		MOV	A,B		;5
		RRC			;4
		MOV	B,A		;5
		DCR	C		;5
		JNZ	PC3_BIT_LOOP	;10   52t = 26uSec
		
		MVI	H,1
		
PC_EXIT		LDA	OUTPUT_LAST	;Stop Bit
		OUT	OUTPUT_PORT	;Set RS232 TX
		CALL	PC1_DELAY_LOOP

		LDA	INT_ENABLED
		ORA	A
		JZ	PC_NO_INT
		EI
PC_NO_INT	
		MOV	A,B		;Return with Char in A
		RLC
		POP	B
		POP	H
		RET

		
PC1_DELAY	MVI	H,9
PC1_DELAY_LOOP	DCR	H
		JNZ	PC1_DELAY_LOOP
		RET
PC2_DELAY	MVI	H,1
		JMP	PC1_DELAY_LOOP



;------------------------------------------------------ ISR IN ROM
; *********   *******    ********         ****   **       **       ********     *****    **     **
; *********  *********   *********         **    ***      **       *********   *******   ***   ***
;    ***     **     **   **     **         **    ****     **       **     **  ***   ***  **** ****
;    ***     **          **     **         **    ** **    **       **     **  **     **  *********
;    ***     *******     ********          **    **  **   **       ********   **     **  ** *** **
;    ***       *******   ********          **    **   **  **       ********   **     **  ** *** **
;    ***            **   **  **            **    **    ** **       **  **     **     **  **     **
;    ***     **     **   **   **           **    **     ****       **   **    ***   ***  **     **
; *********  *********   **    **          **    **      ***       **    **    *******   **     **
; *********   *******    **     **        ****   **       **       **     **    *****    **     **
;------------------------------------------------------ 



ISR_TIMER_ROM	
;		CALL	FP_OPERATE
;		RET

;------------------------------------------------------ 
FP_OPERATE	LDA	GC_OPTIONS
		RRC
		RNC	;EXIT IF GC_OPTIONS.0 is off
		MVI	A,20
		STA	FP_PRECOUNT
		LHLD	TIC_COUNTER
		INX	H
		SHLD	TIC_COUNTER

		CALL	FP_MAT_SCAN
		LHLD	FPM_ADDR
		CALL	PUT_LED_AD
		CALL	READ_MEMORY	;MOV A,M (from RAM or ROM)
		CALL	PUT_LED_DATA
		CALL	GET_KEY
		PUSH	B
		CNZ	DO_KEY
		POP	B
		CALL	REPEAT_KEYS
		CALL	DISP_MODIFIERS
		CALL	STAT_UPDATE
		RET

;---------------------- Service Front Panel LED Matrix (multiplexed)--------
;FP_MAT_PTR = Pointer to RAM with image of what LED's should be on/off
;FP_LED_MAT = RAM image of LEDs
;FP_SW_MAT  = Input of Switches Read (reads all Rows, must be 7 bytes after LEDs)
;Output:                                    HighAddr  Low-Addr --Data--  Stat
;1000 dcba : Low  nibble of High Address    ----xxxx  -------- --------  ----
;1001 dcba : High nibble of High Address    xxxx----  -------- --------  ----
;1010 dcba : Low  nibble of Low  Address    --------  ----xxxx --------  ----
;1011 dcba : High nibble of Low  Address    --------  xxxx---- --------  ----
;1100 dcba : Low  nibble of Data & Switches --------  -------- ----xxxx  ---- & Low  Data Switches
;1101 dcba : High nibble of Data & Switches --------  -------- xxxx----  ---- & High Data Switches
;1110 dcba : Status & Switches              --------  -------- --------  xxxx & Control Switches
;1111 dcba : Not decoded

FP_MAT_SCAN	LHLD	FP_MAT_PTR	;HL = Base of LED output matrix (FP_LED_MAT)

		MOV	A,M		;Get how LED's should be (low nibble)
		ANI	0FH		;Clear upper nibble (reserved for column)
		MOV	H,A
		
		MOV	A,L		;Get column (from low nibble of address)
		ADD	A		;Shift up to upper nibble
		ADD	A		;(use ADD to do arithmetic shift, low nibble =0)
		ADD	A
		ADD	A
		ORA	H		;Include LED's
		CALL	LED_OUT
					;Read switches while scanning.
		MVI	H,(FP_LED_MAT >> 8)
		MOV	A,L
		ADI	7		;Add 7 to index into FP_SW_MAT
		MOV	L,A
		IN	INPUT_PORT	;Read Input Switches
		MOV	M,A		;Save to Switch Matrix in RAM
		MOV	A,L
		SBI	6		; Subtract 6 for net effect of +1 to count
		MOV	L,A               
		CPI	(FP_LED_MAT & 0FFH)+7
		JNZ	FPMS_LED_NOROLL   
		MVI	L,FP_LED_MAT & 0FFH
FPMS_LED_NOROLL	SHLD	FP_MAT_PTR        
                                          
                                        ;Debounce Switches
		LDA	FP_SW_PTR	; Get switch pointer
		ADI	(FP_SW_DBC & 0FFH )
		MOV	L,A
		PUSH	H		;H=Pointer to Debounce counter for this key

		LDA	FP_SW_PTR	;Get switch pointer
		ANI	3		;break out bit mapped
		MOV	L,A		;BITMAP (assume H=FF from above)
		MOV	H,M		;H=Bitmapped (0001, 0010, 0100, 1000)
		LDA	FP_SW_PTR	;Get swtich pointer
		RRC			;break out memory location for switch inputs
		RRC	
		ANI	3
		ADI	(FP_SW_MAT & 0FFH)+4  ;Start of Switches (D3 to D0)
		MOV	L,A
		MOV	A,H		;Get Bit map of switch to sample
		MVI	H,(FP_LED_MAT >> 8)
		ANA	M		;Read Raw Switch input sampled (clear carry)

	;Count is set to 00 if switch input is logic 1 (not pressed)
	;Count advances up to FF if switch is pressed
	;Count does not advance if Positive
	;Main line code may test the count for 00 to indicate a key pressed
	;Main line code may then advanced the count to 01 to "acknowledge" the press (for single events)

		POP	H		;M(H)=Counter
		MVI	A,0		;Reset count to 00h when key is up.
		JNZ	FPMS_SAVE	;Jump if key is up

FPMS_DOWN	MOV	A,M		
		INR	A
		JZ	FPMS_NEXT	;Jump to do nothing
		CPI	08H		;Check for trigger to flag (Sensitivity for Key Down event)
		STC			;
		CMC			;Clear carry
		JNZ	FPMS_SAVE
		STC			;Set carry to Save Key Pressed		
FPMS_SAVE	MOV	M,A		;Save Debounce Count to Switch Debounce Counters (SW_DBC)
FPMS_NEXT	LDA	FP_SW_PTR	;Get switch pointer
		INR	A		;Increment to next switch
		JNC	FPMS_NOFLAG
		STA	FP_SW_FLAG	;Save flagged key (base 1)
FPMS_NOFLAG	CPI	11		;Test for last switch
		JNZ	FPMS_SW_NOROLL
		XRA	A
FPMS_SW_NOROLL	STA	FP_SW_PTR
		RET



;------------------------------------------------------ 
PUT_LED_AD	MOV	A,H              
		CALL	PUT_LED_ADHI     
		MOV	A,L              
;		CALL	PUT_LED_ADLO     
;		RET                      
                                         
PUT_LED_ADLO	STA	FP_LED_MAT+2
		RRC
		RRC
		RRC
		RRC
		STA	FP_LED_MAT+3
		RET

PUT_LED_ADHI	STA	FP_LED_MAT
		RRC
		RRC
		RRC
		RRC                      
		STA	FP_LED_MAT+1     
		RET                      

PUT_LED_DATA	STA	FP_LED_MAT+4
		RRC
		RRC
		RRC
		RRC
		STA	FP_LED_MAT+5
		RET

PUT_LED_STAT	STA	FP_LED_MAT+6
		RET

;------------------------------------------------------ 
GET_KEY		LDA	FP_SW_FLAG	;Get Key pressed (1 to 12 or 0 if no key)
		ORA	A
		RZ			;Exit if 0 (no key)
		PUSH	PSW		;Stack the Key
		XRA	A		;Clear Flag 
		STA	FP_SW_FLAG
		POP	PSW		;Return with A = Key
		RET


;------------------------------------------------------ 
DO_KEY		CPI	11		;Check for NeXT key
		JZ	KEY_NEXT
		CPI	10		;Check for MODify Key
		JZ	KEY_MOD
		CPI	9		;Check for RUN Key
		JZ	KEY_RUN
					;By Default, Key is a Data Button
		DCR	A
		MOV	L,A
		MVI	H,(BITMAP >> 8)
		MOV	B,M		;Get BitMap of key pressed

		LDA	FPM_MOD_FIELD
		ORI	80H
		STA	FPM_MOD_FIELD	;Stop Flashing of field (Don't change low 2 bits for field edit)
		ANI	3
		JZ	MODO_NULL
		DCR	A
		JZ	MODO_ADHI
		DCR	A
		JZ	MODO_ADLO
;		DCR	A		;By default, we are modifying Memory
;		JZ	MODO_DATA
MODO_DATA	LHLD	FPM_ADDR	;Data
		MOV	A,M
		XRA	B
		MOV	M,A
		CMP	M		;Test for ROM
		RZ
		LDA	FPM_MOD_FIELD	;If ROM start flashing of field
		ANI	7FH
		STA	FPM_MOD_FIELD	
		RET
MODO_ADHI	LDA	FPM_ADDR+1	;High Address
		XRA	B
		STA	FPM_ADDR+1	;High Address
		RET
MODO_ADLO	LDA	FPM_ADDR	 ;Low Address
		XRA	B               
		STA	FPM_ADDR	 ;Low Address
		RET                     
                                        
MODO_NULL				 ;DATA Key pressed without any Mod Field
					 ;Do Nothing
		RET			 

                                        
KEY_NEXT	LDA	FPM_MOD_FIELD
		DCR	A
		JZ	KEY_NEXT_1	;Cancel if modifying field is High Address
		DCR	A
		JNZ	DO_NEXT_PREV	;Cancel if modifying field is Low Address
KEY_NEXT_1	;XRA	A		;Cancel any modifying field
		STA	FPM_MOD_FIELD	
		JMP	DO_NEXT_PREV

		

KEY_MOD		LDA	FPM_MOD_FIELD	;Advance Modify Field
		INR	A
		ANI	3
		STA	FPM_MOD_FIELD
		RET
		
KEY_RUN		MVI	A,1		;RUN LED
		CALL	PUT_LED_STAT
		LHLD	FPM_ADDR	;Execute code
		PCHL


;------------------------------------------------------ 
REPEAT_KEYS	LDA	FPM_REPEAT     
		INR	A
		INR	A
		STA	FPM_REPEAT
		RNZ
		LDA	FP_SW_DBC+10	;NEXT Switch
		CPI	40H
		RC
		MVI	A,0FFH
		STA	FP_SW_DBC+10	;NEXT Switch
		INR	A
		RNZ
		;CALL	DO_NEXT_PREV
DO_NEXT_PREV	LHLD	FPM_ADDR	;Execut NEXT/PREV
		INX	H
		LDA	FP_SW_DBC+7	;Test for D7 as shift and do Key Previous
		ORA	A
		JZ	DNP_UP
		DCX	H
		DCX	H	
DNP_UP		SHLD	FPM_ADDR
		RET


;------------------------------------------------------ 
DISP_MODIFIERS	LDA	FPM_MOD_FIELD
		ORA	A
		RZ
		DCR	A
		JZ	MOD_ADHI
		DCR	A
		JZ	MOD_ADLO
		DCR	A
		RNZ
MOD_DATA	LHLD	FPM_ADDR	;Data
		MOV	A,M
		CALL	FLASH
		CALL	PUT_LED_DATA
		RET
MOD_ADHI	LDA	FPM_ADDR+1	;High Address
		CALL	FLASH          
		CALL	PUT_LED_ADHI   
		RET                    
MOD_ADLO	LDA	FPM_ADDR	  ;Low Address
		CALL	FLASH          
		CALL	PUT_LED_ADLO   
		RET                    

FLASH		PUSH	PSW
		LDA	FPM_FLASH_COD
		INR	A
		STA	FPM_FLASH_COD
		RLC
		RLC
		RLC
		ANI	3
		DCR	A
		JZ	FLASH_OFF
		DCR	A
		JZ	FLASH_ON
		POP	PSW
		RET
FLASH_OFF	POP	PSW
		XRA	A
		RET
FLASH_ON	POP	PSW
		MVI	A,0FFH
		RET


;------------------------------------------------------ 
STAT_UPDATE	LXI	H,FP_LED_MAT+6
		MVI	A,2
		ORA	M
		MOV	M,A
		LDA	FPM_MOD_FIELD
		ANI	07FH
		RNZ
		MVI	A,0FDH
		ANA	M
		MOV	M,A
		RET
                                      
LED_OFF		XRA	A
LED_OUT		ORI	80H		;TXDATA HIGH
		STA	OUTPUT_LAST
		OUT	OUTPUT_PORT	;Set LED off
		RET
                                       

DELAY_SCAN	CALL	FP_MAT_SCAN
		DCR	C
		JNZ	DELAY_SCAN
		RET








MSIZE		.EQU  61	;MEMORY SIZE IN KBYTES.

IOBYTE		.EQU  3		;ADDRESS OF I/O BYTE.
CCP		.EQU  (MSIZE-7)*1024	;START OF CPM (D800)
BDOS		.EQU  CCP+806H		;START OF BDOS (E006)
;BIOS		.EQU  CCP+1600H		;START OF BIOS (EE00)

;       Page Zero Definitions.
CDISK		.EQU	4		;Location of current disk
DBUF		.EQU	80h		;Default sector buffer


		
		.ORG	($ & 0FF00H) + 103H

ROM_JUMP_TABLE	JMP  	CBOOT	;FROM COLD START LOADER.
		JMP  	WBOOT 	;FROM WARM BOOT.
		JMP  	GET_STAT ;CHECK CONSOLE KB STATUS.
		JMP  	CONIN 	;READ CONSOLE CHARACTER.
		JMP  	CONOT 	;WRITE CONSOLE CHARACTER.
		JMP  	LIST 	;WRITE LISTING CHAR.
		JMP  	PUNCH 	;WRITE PUNCH CHAR.
		JMP  	READER 	;READ READER CHAR.
		JMP  	HOME 	;MOVE DISK TO TRACK ZERO.
		JMP  	SELDSK 	;SELECT DISK DRIVE.
		JMP  	SETTRK 	;SEEK TO TRACK IN REG A.
		JMP  	SETSEC 	;SET SECTOR NUMBER.
		JMP  	SETDMA 	;SET DISK STARTING ADR.
		JMP  	DREAD 	;READ SELECTED SECTOR.
		JMP  	DWRITE 	;WRITE SELECTED SECTOR.
		JMP  	LISTST 	;List status (output)
		JMP  	SECTRN 	;Translate sector number


LOAD_CPM	LXI	H,RAM_IMG	;Source
		LXI	D,RAM_BASE	;Destination
		LXI	B,RAM_SIZE	;#Bytes to copy
		
COPY_RAM_BC	INR	B		;Pre-inrement Count High byte
CRBC_LOOP	MOV	A,M		;A=M(HL)
		XCHG
		MOV	M,A		;M(DE)=A
		XCHG
		INX	H
		INX	D
		DCR	C
		JNZ	CRBC_LOOP
		DCR	B
		JNZ	CRBC_LOOP
		RET

;       B O O T   C P / M   f r o m   d i s k.
CBOOT		LXI  SP,STACK		;SET STACK POINTER.
		CALL PRINTI
		.text "\r\nAltaid 8800 - 61K CP/M 2.2\r\n\000"
	
	        XRA     A
	        STA     CDISK           ;Force A drive
	        STA     IOBYTE          ;Clear I/O byte

WBOOT		LXI  SP,STACK		;SET STACK POINTER.

		CALL PRINTI
		.text "\r\nWBOOT\r\n\000"

        		      		;Boot CP/M
		CALL	LOAD_CPM	;Copy CPM image from ROM to RAM

					; SET UP JUMPS IN CP/M PAGE ZERO.

		LXI  B,DBUF	;SET DEFAULT DMA ADR.
		CALL SETDMA
		MVI  A,0C3H	;PUT JMP TO WBOOT
		STA  0		;ADR AT ZERO.
		STA  5
		LXI  H,WBOOTV
		SHLD 1
		LXI  H,BDOS	;PUT JUMP TO BDOS
		SHLD 6		;AT ADR 5,6,7.

		CALL	ECHO_OFF		
	
		JMP  	RAM_WBOOT

;===============================================
;       C O N S O L   S T A T U S
;
;       This routine samples the Console status and
;       returns the following values in the A register.
;
;       EXIT    A = 0 (zero), means no character
;               currently ready to read.
;
;               A = FFh (255), means character
;               currently ready to read.
;-----------------------------------------------
;CONST		JMP	GET_STAT

;===============================================
;       C O N S O L   I N P U T
;
;       Read the next character into the A register, clearing the high
;       order bit.  If no character currently ready to read then wait
;       for a character to arrive before returning.
;
;       EXIT    A = character read from terminal.
;-----------------------------------------------
CONIN:	;CALL	ECHO_OFF
	CALL	GET_CHAR
	ANI	7FH	;MAKE MOST SIG. BIT = 0.
	CPI	7FH	;IS IT A RUBOUT?
	RNZ		;RETURN IF NOT.
	STA	CONOTF	;SET NO PRINT FLAG.
	RET		;RETURN FROM CONIN.
	
;===============================================
;       C O N S O L   O U T P U T
;
;       Send a character to the console.  If the console is not ready to
;       receive a character wait until the console is ready.
;
;       ENTRY   C = ASCII character to output to console.
;-----------------------------------------------
CONOT:	MOV	A,C	;GET CHARACTER.
	CPI	7FH	;IS IT A RUBOUT?
	RZ		;IF SO, DON'T PRINT IT.
	PUSH	H
	LXI	H,CONOTF
	INR	M	;GET NO PRINT FLAG.
	MVI	M,0	;RESET THE FLAG
	JP   	CONOTA	;NOT SET, SO PRINT.
	CALL	PRINTI
	.text "\008 \008\000"
	POP	H
	RET		;RETURN.


			;ANOTHER BACKSPACE.
CONOTA:	MOV	A,C	;GET CHARACTER.
	CALL	PUT_CHAR ;PRINT IT.
	POP	H
	RET		;RETURN.

;
; WRITE A CHARACTER ON LISTING DEVICE.
;
LIST:	RET		;RETURN FROM LIST.


;
; PUNCH PAPER TAPE.
;
PUNCH:	MOV	A,C	;GET CHARACTER.
	CALL	PUT_CHAR ;PRINT IT.
	RET		;RETURN FROM PUNCH.

;
;  NORMALLY USED TO READ PAPER TAPE.
;
READER:	CALL	GET_CHAR
	RET		;RETURN FROM READER.

LISTST	RET


;       S E L E C T   D I S K   D R I V E
;
;       Select the disk drive for subsequent disk transfers and
;       return the appropriate DPH address. 
;
;       ENTRY   C = disk Selection value.
;
;       EXIT    HL = 0, if drive not selectable.
;               HL = DPH address if drive is selectable.
;
SELDSK:	LXI	H,0
	MOV	A,C	;GET NEW DISK NUMBER.
	CPI	NDSK
	RNC		;If Disk invalid...EXIT
	
	LXI	H,DPBASE
	XRA	A	;SET A = 0.
	RET		;RETURN FROM SELDSK.


; MOVE DISK TO TRACK ZERO.
;
HOME:	LXI  B,0	;SEEK TO TRACK ZERO.

;
; SET TRACK NUMBER TO WHATEVER IS IN REGISTER C.
; ALSO PERFORM MOVE TO THE CORRECT TRACK (SEEK).
;
SETTRK:	MOV  H,B	;GET NEW TRACK NUMBER.
	MOV  L,C	;MOVE B&C TO H&L.
	SHLD  TRK	;UPDATE OLD WITH NEW.

;	CALL PRINTI
;	.text "\r\nTrk:\000"
;	CALL	PUT_HL
	
; MOVE THE HEAD TO THE TRACK IN REGISTER A.

	XRA  A		;Clear flags
	RET		;RETURN FROM SEEK.

;
; SET DISK SECTOR NUMBER.
;
SETSEC:	MOV  A,C	;GET SECTOR NUMBER.
	STA  SECT	;PUT AT SECT # ADDRESS.

;	CALL PRINTI
;	.text "\r\nSec:\000"
;	CALL	PUT_BYTE

	XRA  A		;Clear flags
	RET		;RETURN FROM SETSEC.
;
; SET DISK DMA ADDRESS.
;
SETDMA:	PUSH H
	MOV  H,B	;MOVE B&C TO H&L.
	MOV  L,C
	SHLD DMAADD	;PUT AT DMA ADR ADDRESS.
	POP  H
	RET		;RETURN FROM SETDMA.


;       Translate sector number from logical to physical.
;
;       ENTRY   DE = 0, no translation required.
;               DE = translation table address.
;               BC = sector number to translate.
;
;       EXIT    HL = translated sector.

SECTRN:	MOV     L,C	;No Translation
	MOV     H,B
	RET
	

;
; READ THE SECTOR AT SECT, FROM THE PRESENT DISK/TRACK/SSECT.
; USE STARTING ADDRESS AT DMAADD.
;
DREAD:		LHLD	SECT	;H=Track,L=Sector
;	CALL PRINTI
;	.text "\r\nRead:\000"
;	CALL	PUT_HL

		MOV	B,H
		MOV	C,L
		CALL	READ_SEC	;Reads to DMAADD CP/M Buffer
		
		XRA  A			;SET FLAGS.
		RET

;
; WRITE THE SECTOR AT SECT, ON THE PRESENT TRACK.
; USE STARTING ADDRESS AT DMAADD.
;
DWRITE:		LHLD	SECT	;H=Track,L=Sector
;	CALL PRINTI
;	.text "\r\nWrite:\000"
;	CALL	PUT_HL
		MOV	B,H
		MOV	C,L
		JMP	WRITE_SEC
		
;-----------------------------------------------------------------------------------------------------
TO_UPPER	CPI	'a'
		RC		;Return if ACC < 'a'
		CPI	'z'+1
		RNC		;Return if ACC > 'z'
		ANI	5Fh	;Flag upper case
		RET


;-----------------------------------------------------------------------------------------------------
		;HL = (HL) word at memory location HL
LD_HL_HL	MOV	A,M		;Fetch L from (HL)
		INX	H
		MOV	H,M		;Fetch H from (HL+1)
		MOV	L,A
		RET

;-----------------------------------------------------------------------------------------------------
;	FILL_BLOCK, Fills a block of RAM with value in A
;	Input:	A = value
;		HL = Start Address
;		B = Length of Fill (MAX = 0 = 256 bytes)
;-----------------------------------------------------------------------------------------------------
FILL_BLOCK	PUSH	B
		PUSH	H
FB_LP		MOV	M,A
		INX	H
		DCR	B
		JNZ	FB_LP
		POP	H
		POP	B
		RET

;-----------------------------------------------------------------------------------------------------
VCALL		PCHL		;Jump to HL
		

		
;		.ORG	5800h


;The following is CP/M v2.2
;It is copied from EPROM at this location (RAM_IMG) to it's correct location in RAM at (RAM_BASE)

		
RAM_IMG		.EQU	$		;Copy CP/M & BIOS to RAM
RAM_BASE	.EQU	CCP
RB		.EQU	RAM_BASE - RAM_IMG ;Offset for Assembler

				;Image of DISK A with CP/M.  Skip first 128 byte boot loader
   .DB 0C3h,05Ch,0DBh,0C3h,058h,0DBh,07Fh,000h,043h,06Fh,070h,079h,072h,069h,067h,068h  ;0080h
   .DB 074h,020h,031h,039h,037h,039h,020h,028h,063h,029h,020h,062h,079h,020h,044h,069h  ;0090h
   .DB 067h,069h,074h,061h,06Ch,020h,052h,065h,073h,065h,061h,072h,063h,068h,020h,020h  ;00A0h
   .DB 020h,020h,020h,020h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;00B0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;00C0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;00D0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;00E0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;00F0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,008h,0D8h,000h,000h,05Fh,00Eh,002h,0C3h  ;0100h
   .DB 005h,000h,0C5h,0CDh,08Ch,0D8h,0C1h,0C9h,03Eh,00Dh,0CDh,092h,0D8h,03Eh,00Ah,0C3h  ;0110h
   .DB 092h,0D8h,03Eh,020h,0C3h,092h,0D8h,0C5h,0CDh,098h,0D8h,0E1h,07Eh,0B7h,0C8h,023h  ;0120h
   .DB 0E5h,0CDh,08Ch,0D8h,0E1h,0C3h,0ACh,0D8h,00Eh,00Dh,0C3h,005h,000h,05Fh,00Eh,00Eh  ;0130h
   .DB 0C3h,005h,000h,0CDh,005h,000h,032h,0EEh,0DFh,03Ch,0C9h,00Eh,00Fh,0C3h,0C3h,0D8h  ;0140h
   .DB 0AFh,032h,0EDh,0DFh,011h,0CDh,0DFh,0C3h,0CBh,0D8h,00Eh,010h,0C3h,0C3h,0D8h,00Eh  ;0150h
   .DB 011h,0C3h,0C3h,0D8h,00Eh,012h,0C3h,0C3h,0D8h,011h,0CDh,0DFh,0C3h,0DFh,0D8h,00Eh  ;0160h
   .DB 013h,0C3h,005h,000h,0CDh,005h,000h,0B7h,0C9h,00Eh,014h,0C3h,0F4h,0D8h,011h,0CDh  ;0170h
   .DB 0DFh,0C3h,0F9h,0D8h,00Eh,015h,0C3h,0F4h,0D8h,00Eh,016h,0C3h,0C3h,0D8h,00Eh,017h  ;0180h
   .DB 0C3h,005h,000h,01Eh,0FFh,00Eh,020h,0C3h,005h,000h,0CDh,013h,0D9h,087h,087h,087h  ;0190h
   .DB 087h,021h,0EFh,0DFh,0B6h,032h,004h,000h,0C9h,03Ah,0EFh,0DFh,032h,004h,000h,0C9h  ;01A0h
   .DB 0FEh,061h,0D8h,0FEh,07Bh,0D0h,0E6h,05Fh,0C9h,03Ah,0ABh,0DFh,0B7h,0CAh,096h,0D9h  ;01B0h
   .DB 03Ah,0EFh,0DFh,0B7h,03Eh,000h,0C4h,0BDh,0D8h,011h,0ACh,0DFh,0CDh,0CBh,0D8h,0CAh  ;01C0h
   .DB 096h,0D9h,03Ah,0BBh,0DFh,03Dh,032h,0CCh,0DFh,011h,0ACh,0DFh,0CDh,0F9h,0D8h,0C2h  ;01D0h
   .DB 096h,0D9h,011h,007h,0D8h,021h,080h,000h,006h,080h,0CDh,042h,0DCh,021h,0BAh,0DFh  ;01E0h
   .DB 036h,000h,023h,035h,011h,0ACh,0DFh,0CDh,0DAh,0D8h,0CAh,096h,0D9h,03Ah,0EFh,0DFh  ;01F0h
   .DB 0B7h,0C4h,0BDh,0D8h,021h,008h,0D8h,0CDh,0ACh,0D8h,0CDh,0C2h,0D9h,0CAh,0A7h,0D9h  ;0200h
   .DB 0CDh,0DDh,0D9h,0C3h,082h,0DBh,0CDh,0DDh,0D9h,0CDh,01Ah,0D9h,00Eh,00Ah,011h,006h  ;0210h
   .DB 0D8h,0CDh,005h,000h,0CDh,029h,0D9h,021h,007h,0D8h,046h,023h,078h,0B7h,0CAh,0BAh  ;0220h
   .DB 0D9h,07Eh,0CDh,030h,0D9h,077h,005h,0C3h,0ABh,0D9h,077h,021h,008h,0D8h,022h,088h  ;0230h
   .DB 0D8h,0C9h,00Eh,00Bh,0CDh,005h,000h,0B7h,0C8h,00Eh,001h,0CDh,005h,000h,0B7h,0C9h  ;0240h
   .DB 00Eh,019h,0C3h,005h,000h,011h,080h,000h,00Eh,01Ah,0C3h,005h,000h,021h,0ABh,0DFh  ;0250h
   .DB 07Eh,0B7h,0C8h,036h,000h,0AFh,0CDh,0BDh,0D8h,011h,0ACh,0DFh,0CDh,0EFh,0D8h,03Ah  ;0260h
   .DB 0EFh,0DFh,0C3h,0BDh,0D8h,011h,028h,0DBh,021h,000h,0E0h,006h,006h,01Ah,0BEh,0C2h  ;0270h
   .DB 0CFh,0DBh,013h,023h,005h,0C2h,0FDh,0D9h,0C9h,0CDh,098h,0D8h,02Ah,08Ah,0D8h,07Eh  ;0280h
   .DB 0FEh,020h,0CAh,022h,0DAh,0B7h,0CAh,022h,0DAh,0E5h,0CDh,08Ch,0D8h,0E1h,023h,0C3h  ;0290h
   .DB 00Fh,0DAh,03Eh,03Fh,0CDh,08Ch,0D8h,0CDh,098h,0D8h,0CDh,0DDh,0D9h,0C3h,082h,0DBh  ;02A0h
   .DB 01Ah,0B7h,0C8h,0FEh,020h,0DAh,009h,0DAh,0C8h,0FEh,03Dh,0C8h,0FEh,05Fh,0C8h,0FEh  ;02B0h
   .DB 02Eh,0C8h,0FEh,03Ah,0C8h,0FEh,03Bh,0C8h,0FEh,03Ch,0C8h,0FEh,03Eh,0C8h,0C9h,01Ah  ;02C0h
   .DB 0B7h,0C8h,0FEh,020h,0C0h,013h,0C3h,04Fh,0DAh,085h,06Fh,0D0h,024h,0C9h,03Eh,000h  ;02D0h
   .DB 021h,0CDh,0DFh,0CDh,059h,0DAh,0E5h,0E5h,0AFh,032h,0F0h,0DFh,02Ah,088h,0D8h,0EBh  ;02E0h
   .DB 0CDh,04Fh,0DAh,0EBh,022h,08Ah,0D8h,0EBh,0E1h,01Ah,0B7h,0CAh,089h,0DAh,0DEh,040h  ;02F0h
   .DB 047h,013h,01Ah,0FEh,03Ah,0CAh,090h,0DAh,01Bh,03Ah,0EFh,0DFh,077h,0C3h,096h,0DAh  ;0300h
   .DB 078h,032h,0F0h,0DFh,070h,013h,006h,008h,0CDh,030h,0DAh,0CAh,0B9h,0DAh,023h,0FEh  ;0310h
   .DB 02Ah,0C2h,0A9h,0DAh,036h,03Fh,0C3h,0ABh,0DAh,077h,013h,005h,0C2h,098h,0DAh,0CDh  ;0320h
   .DB 030h,0DAh,0CAh,0C0h,0DAh,013h,0C3h,0AFh,0DAh,023h,036h,020h,005h,0C2h,0B9h,0DAh  ;0330h
   .DB 006h,003h,0FEh,02Eh,0C2h,0E9h,0DAh,013h,0CDh,030h,0DAh,0CAh,0E9h,0DAh,023h,0FEh  ;0340h
   .DB 02Ah,0C2h,0D9h,0DAh,036h,03Fh,0C3h,0DBh,0DAh,077h,013h,005h,0C2h,0C8h,0DAh,0CDh  ;0350h
   .DB 030h,0DAh,0CAh,0F0h,0DAh,013h,0C3h,0DFh,0DAh,023h,036h,020h,005h,0C2h,0E9h,0DAh  ;0360h
   .DB 006h,003h,023h,036h,000h,005h,0C2h,0F2h,0DAh,0EBh,022h,088h,0D8h,0E1h,001h,00Bh  ;0370h
   .DB 000h,023h,07Eh,0FEh,03Fh,0C2h,009h,0DBh,004h,00Dh,0C2h,001h,0DBh,078h,0B7h,0C9h  ;0380h
   .DB 044h,049h,052h,020h,045h,052h,041h,020h,054h,059h,050h,045h,053h,041h,056h,045h  ;0390h
   .DB 052h,045h,04Eh,020h,055h,053h,045h,052h,000h,016h,000h,000h,000h,000h,021h,010h  ;03A0h
   .DB 0DBh,00Eh,000h,079h,0FEh,006h,0D0h,011h,0CEh,0DFh,006h,004h,01Ah,0BEh,0C2h,04Fh  ;03B0h
   .DB 0DBh,013h,023h,005h,0C2h,03Ch,0DBh,01Ah,0FEh,020h,0C2h,054h,0DBh,079h,0C9h,023h  ;03C0h
   .DB 005h,0C2h,04Fh,0DBh,00Ch,0C3h,033h,0DBh,0AFh,032h,007h,0D8h,031h,0ABh,0DFh,0C5h  ;03D0h
   .DB 079h,01Fh,01Fh,01Fh,01Fh,0E6h,00Fh,05Fh,0CDh,015h,0D9h,0CDh,0B8h,0D8h,032h,0ABh  ;03E0h
   .DB 0DFh,0C1h,079h,0E6h,00Fh,032h,0EFh,0DFh,0CDh,0BDh,0D8h,03Ah,007h,0D8h,0B7h,0C2h  ;03F0h
   .DB 098h,0DBh,031h,0ABh,0DFh,0CDh,098h,0D8h,0CDh,0D0h,0D9h,0C6h,061h,0CDh,08Ch,0D8h  ;0400h
   .DB 03Eh,03Eh,0CDh,08Ch,0D8h,0CDh,039h,0D9h,011h,080h,000h,0CDh,0D8h,0D9h,0CDh,0D0h  ;0410h
   .DB 0D9h,032h,0EFh,0DFh,0CDh,05Eh,0DAh,0C4h,009h,0DAh,03Ah,0F0h,0DFh,0B7h,0C2h,0A5h  ;0420h
   .DB 0DEh,0CDh,02Eh,0DBh,021h,0C1h,0DBh,05Fh,016h,000h,019h,019h,07Eh,023h,066h,06Fh  ;0430h
   .DB 0E9h,077h,0DCh,01Fh,0DDh,05Dh,0DDh,0ADh,0DDh,010h,0DEh,08Eh,0DEh,0A5h,0DEh,021h  ;0440h
   .DB 0F3h,076h,022h,000h,0D8h,021h,000h,0D8h,0E9h,001h,0DFh,0DBh,0C3h,0A7h,0D8h,052h  ;0450h
   .DB 065h,061h,064h,020h,065h,072h,072h,06Fh,072h,000h,001h,0F0h,0DBh,0C3h,0A7h,0D8h  ;0460h
   .DB 04Eh,06Fh,020h,066h,069h,06Ch,065h,000h,0CDh,05Eh,0DAh,03Ah,0F0h,0DFh,0B7h,0C2h  ;0470h
   .DB 009h,0DAh,021h,0CEh,0DFh,001h,00Bh,000h,07Eh,0FEh,020h,0CAh,033h,0DCh,023h,0D6h  ;0480h
   .DB 030h,0FEh,00Ah,0D2h,009h,0DAh,057h,078h,0E6h,0E0h,0C2h,009h,0DAh,078h,007h,007h  ;0490h
   .DB 007h,080h,0DAh,009h,0DAh,080h,0DAh,009h,0DAh,082h,0DAh,009h,0DAh,047h,00Dh,0C2h  ;04A0h
   .DB 008h,0DCh,0C9h,07Eh,0FEh,020h,0C2h,009h,0DAh,023h,00Dh,0C2h,033h,0DCh,078h,0C9h  ;04B0h
   .DB 006h,003h,07Eh,012h,023h,013h,005h,0C2h,042h,0DCh,0C9h,021h,080h,000h,081h,0CDh  ;04C0h
   .DB 059h,0DAh,07Eh,0C9h,0AFh,032h,0CDh,0DFh,03Ah,0F0h,0DFh,0B7h,0C8h,03Dh,021h,0EFh  ;04D0h
   .DB 0DFh,0BEh,0C8h,0C3h,0BDh,0D8h,03Ah,0F0h,0DFh,0B7h,0C8h,03Dh,021h,0EFh,0DFh,0BEh  ;04E0h
   .DB 0C8h,03Ah,0EFh,0DFh,0C3h,0BDh,0D8h,0CDh,05Eh,0DAh,0CDh,054h,0DCh,021h,0CEh,0DFh  ;04F0h
   .DB 07Eh,0FEh,020h,0C2h,08Fh,0DCh,006h,00Bh,036h,03Fh,023h,005h,0C2h,088h,0DCh,01Eh  ;0500h
   .DB 000h,0D5h,0CDh,0E9h,0D8h,0CCh,0EAh,0DBh,0CAh,01Bh,0DDh,03Ah,0EEh,0DFh,00Fh,00Fh  ;0510h
   .DB 00Fh,0E6h,060h,04Fh,03Eh,00Ah,0CDh,04Bh,0DCh,017h,0DAh,00Fh,0DDh,0D1h,07Bh,01Ch  ;0520h
   .DB 0D5h,0E6h,003h,0F5h,0C2h,0CCh,0DCh,0CDh,098h,0D8h,0C5h,0CDh,0D0h,0D9h,0C1h,0C6h  ;0530h
   .DB 041h,0CDh,092h,0D8h,03Eh,03Ah,0CDh,092h,0D8h,0C3h,0D4h,0DCh,0CDh,0A2h,0D8h,03Eh  ;0540h
   .DB 03Ah,0CDh,092h,0D8h,0CDh,0A2h,0D8h,006h,001h,078h,0CDh,04Bh,0DCh,0E6h,07Fh,0FEh  ;0550h
   .DB 020h,0C2h,0F9h,0DCh,0F1h,0F5h,0FEh,003h,0C2h,0F7h,0DCh,03Eh,009h,0CDh,04Bh,0DCh  ;0560h
   .DB 0E6h,07Fh,0FEh,020h,0CAh,00Eh,0DDh,03Eh,020h,0CDh,092h,0D8h,004h,078h,0FEh,00Ch  ;0570h
   .DB 0D2h,00Eh,0DDh,0FEh,009h,0C2h,0D9h,0DCh,0CDh,0A2h,0D8h,0C3h,0D9h,0DCh,0F1h,0CDh  ;0580h
   .DB 0C2h,0D9h,0C2h,01Bh,0DDh,0CDh,0E4h,0D8h,0C3h,098h,0DCh,0D1h,0C3h,086h,0DFh,0CDh  ;0590h
   .DB 05Eh,0DAh,0FEh,00Bh,0C2h,042h,0DDh,001h,052h,0DDh,0CDh,0A7h,0D8h,0CDh,039h,0D9h  ;05A0h
   .DB 021h,007h,0D8h,035h,0C2h,082h,0DBh,023h,07Eh,0FEh,059h,0C2h,082h,0DBh,023h,022h  ;05B0h
   .DB 088h,0D8h,0CDh,054h,0DCh,011h,0CDh,0DFh,0CDh,0EFh,0D8h,03Ch,0CCh,0EAh,0DBh,0C3h  ;05C0h
   .DB 086h,0DFh,041h,06Ch,06Ch,020h,028h,079h,02Fh,06Eh,029h,03Fh,000h,0CDh,05Eh,0DAh  ;05D0h
   .DB 0C2h,009h,0DAh,0CDh,054h,0DCh,0CDh,0D0h,0D8h,0CAh,0A7h,0DDh,0CDh,098h,0D8h,021h  ;05E0h
   .DB 0F1h,0DFh,036h,0FFh,021h,0F1h,0DFh,07Eh,0FEh,080h,0DAh,087h,0DDh,0E5h,0CDh,0FEh  ;05F0h
   .DB 0D8h,0E1h,0C2h,0A0h,0DDh,0AFh,077h,034h,021h,080h,000h,0CDh,059h,0DAh,07Eh,0FEh  ;0600h
   .DB 01Ah,0CAh,086h,0DFh,0CDh,08Ch,0D8h,0CDh,0C2h,0D9h,0C2h,086h,0DFh,0C3h,074h,0DDh  ;0610h
   .DB 03Dh,0CAh,086h,0DFh,0CDh,0D9h,0DBh,0CDh,066h,0DCh,0C3h,009h,0DAh,0CDh,0F8h,0DBh  ;0620h
   .DB 0F5h,0CDh,05Eh,0DAh,0C2h,009h,0DAh,0CDh,054h,0DCh,011h,0CDh,0DFh,0D5h,0CDh,0EFh  ;0630h
   .DB 0D8h,0D1h,0CDh,009h,0D9h,0CAh,0FBh,0DDh,0AFh,032h,0EDh,0DFh,0F1h,06Fh,026h,000h  ;0640h
   .DB 029h,011h,000h,001h,07Ch,0B5h,0CAh,0F1h,0DDh,02Bh,0E5h,021h,080h,000h,019h,0E5h  ;0650h
   .DB 0CDh,0D8h,0D9h,011h,0CDh,0DFh,0CDh,004h,0D9h,0D1h,0E1h,0C2h,0FBh,0DDh,0C3h,0D4h  ;0660h
   .DB 0DDh,011h,0CDh,0DFh,0CDh,0DAh,0D8h,03Ch,0C2h,001h,0DEh,001h,007h,0DEh,0CDh,0A7h  ;0670h
   .DB 0D8h,0CDh,0D5h,0D9h,0C3h,086h,0DFh,04Eh,06Fh,020h,073h,070h,061h,063h,065h,000h  ;0680h
   .DB 0CDh,05Eh,0DAh,0C2h,009h,0DAh,03Ah,0F0h,0DFh,0F5h,0CDh,054h,0DCh,0CDh,0E9h,0D8h  ;0690h
   .DB 0C2h,079h,0DEh,021h,0CDh,0DFh,011h,0DDh,0DFh,006h,010h,0CDh,042h,0DCh,02Ah,088h  ;06A0h
   .DB 0D8h,0EBh,0CDh,04Fh,0DAh,0FEh,03Dh,0CAh,03Fh,0DEh,0FEh,05Fh,0C2h,073h,0DEh,0EBh  ;06B0h
   .DB 023h,022h,088h,0D8h,0CDh,05Eh,0DAh,0C2h,073h,0DEh,0F1h,047h,021h,0F0h,0DFh,07Eh  ;06C0h
   .DB 0B7h,0CAh,059h,0DEh,0B8h,070h,0C2h,073h,0DEh,070h,0AFh,032h,0CDh,0DFh,0CDh,0E9h  ;06D0h
   .DB 0D8h,0CAh,06Dh,0DEh,011h,0CDh,0DFh,0CDh,00Eh,0D9h,0C3h,086h,0DFh,0CDh,0EAh,0DBh  ;06E0h
   .DB 0C3h,086h,0DFh,0CDh,066h,0DCh,0C3h,009h,0DAh,001h,082h,0DEh,0CDh,0A7h,0D8h,0C3h  ;06F0h
   .DB 086h,0DFh,046h,069h,06Ch,065h,020h,065h,078h,069h,073h,074h,073h,000h,0CDh,0F8h  ;0700h
   .DB 0DBh,0FEh,010h,0D2h,009h,0DAh,05Fh,03Ah,0CEh,0DFh,0FEh,020h,0CAh,009h,0DAh,0CDh  ;0710h
   .DB 015h,0D9h,0C3h,089h,0DFh,0CDh,0F5h,0D9h,03Ah,0CEh,0DFh,0FEh,020h,0C2h,0C4h,0DEh  ;0720h
   .DB 03Ah,0F0h,0DFh,0B7h,0CAh,089h,0DFh,03Dh,032h,0EFh,0DFh,0CDh,029h,0D9h,0CDh,0BDh  ;0730h
   .DB 0D8h,0C3h,089h,0DFh,011h,0D6h,0DFh,01Ah,0FEh,020h,0C2h,009h,0DAh,0D5h,0CDh,054h  ;0740h
   .DB 0DCh,0D1h,021h,083h,0DFh,0CDh,040h,0DCh,0CDh,0D0h,0D8h,0CAh,06Bh,0DFh,021h,000h  ;0750h
   .DB 001h,0E5h,0EBh,0CDh,0D8h,0D9h,011h,0CDh,0DFh,0CDh,0F9h,0D8h,0C2h,001h,0DFh,0E1h  ;0760h
   .DB 011h,080h,000h,019h,011h,000h,0D8h,07Dh,093h,07Ch,09Ah,0D2h,071h,0DFh,0C3h,0E1h  ;0770h
   .DB 0DEh,0E1h,03Dh,0C2h,071h,0DFh,0CDh,066h,0DCh,0CDh,05Eh,0DAh,021h,0F0h,0DFh,0E5h  ;0780h
   .DB 07Eh,032h,0CDh,0DFh,03Eh,010h,0CDh,060h,0DAh,0E1h,07Eh,032h,0DDh,0DFh,0AFh,032h  ;0790h
   .DB 0EDh,0DFh,011h,05Ch,000h,021h,0CDh,0DFh,006h,021h,0CDh,042h,0DCh,021h,008h,0D8h  ;07A0h
   .DB 07Eh,0B7h,0CAh,03Eh,0DFh,0FEh,020h,0CAh,03Eh,0DFh,023h,0C3h,030h,0DFh,006h,000h  ;07B0h
   .DB 011h,081h,000h,07Eh,012h,0B7h,0CAh,04Fh,0DFh,004h,023h,013h,0C3h,043h,0DFh,078h  ;07C0h
   .DB 032h,080h,000h,0CDh,098h,0D8h,0CDh,0D5h,0D9h,0CDh,01Ah,0D9h,0CDh,000h,001h,031h  ;07D0h
   .DB 0ABh,0DFh,0CDh,029h,0D9h,0CDh,0BDh,0D8h,0C3h,082h,0DBh,0CDh,066h,0DCh,0C3h,009h  ;07E0h
   .DB 0DAh,001h,07Ah,0DFh,0CDh,0A7h,0D8h,0C3h,086h,0DFh,042h,061h,064h,020h,06Ch,06Fh  ;07F0h
   .DB 061h,064h,000h,043h,04Fh,04Dh,0CDh,066h,0DCh,0CDh,05Eh,0DAh,03Ah,0CEh,0DFh,0D6h  ;0800h
   .DB 020h,021h,0F0h,0DFh,0B6h,0C2h,009h,0DAh,0C3h,082h,0DBh,000h,000h,000h,000h,000h  ;0810h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,024h,024h,024h  ;0820h
   .DB 020h,020h,020h,020h,020h,053h,055h,042h,000h,000h,000h,000h,000h,000h,000h,000h  ;0830h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,020h,020h  ;0840h
   .DB 020h,020h,020h,020h,020h,020h,020h,020h,020h,000h,000h,000h,000h,000h,020h,020h  ;0850h
   .DB 020h,020h,020h,020h,020h,020h,020h,020h,020h,000h,000h,000h,000h,000h,000h,000h  ;0860h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0870h
   .DB 000h,016h,000h,000h,000h,000h,0C3h,011h,0E0h,099h,0E0h,0A5h,0E0h,0ABh,0E0h,0B1h  ;0880h
   .DB 0E0h,0EBh,022h,043h,0E3h,0EBh,07Bh,032h,0D6h,0EDh,021h,000h,000h,022h,045h,0E3h  ;0890h
   .DB 039h,022h,00Fh,0E3h,031h,041h,0E3h,0AFh,032h,0E0h,0EDh,032h,0DEh,0EDh,021h,074h  ;08A0h
   .DB 0EDh,0E5h,079h,0FEh,029h,0D0h,04Bh,021h,047h,0E0h,05Fh,016h,000h,019h,019h,05Eh  ;08B0h
   .DB 023h,056h,02Ah,043h,0E3h,0EBh,0E9h,003h,0EEh,0C8h,0E2h,090h,0E1h,0CEh,0E2h,012h  ;08C0h
   .DB 0EEh,00Fh,0EEh,0D4h,0E2h,0EDh,0E2h,0F3h,0E2h,0F8h,0E2h,0E1h,0E1h,0FEh,0E2h,07Eh  ;08D0h
   .DB 0ECh,083h,0ECh,045h,0ECh,09Ch,0ECh,0A5h,0ECh,0ABh,0ECh,0C8h,0ECh,0D7h,0ECh,0E0h  ;08E0h
   .DB 0ECh,0E6h,0ECh,0ECh,0ECh,0F5h,0ECh,0FEh,0ECh,004h,0EDh,00Ah,0EDh,011h,0EDh,02Ch  ;08F0h
   .DB 0E5h,017h,0EDh,01Dh,0EDh,026h,0EDh,02Dh,0EDh,041h,0EDh,047h,0EDh,04Dh,0EDh,00Eh  ;0900h
   .DB 0ECh,053h,0EDh,004h,0E3h,004h,0E3h,09Bh,0EDh,021h,0CAh,0E0h,0CDh,0E5h,0E0h,0FEh  ;0910h
   .DB 003h,0CAh,000h,000h,0C9h,021h,0D5h,0E0h,0C3h,0B4h,0E0h,021h,0E1h,0E0h,0C3h,0B4h  ;0920h
   .DB 0E0h,021h,0DCh,0E0h,0CDh,0E5h,0E0h,0C3h,000h,000h,042h,064h,06Fh,073h,020h,045h  ;0930h
   .DB 072h,072h,020h,04Fh,06Eh,020h,020h,03Ah,020h,024h,042h,061h,064h,020h,053h,065h  ;0940h
   .DB 063h,074h,06Fh,072h,024h,053h,065h,06Ch,065h,063h,074h,024h,046h,069h,06Ch,065h  ;0950h
   .DB 020h,052h,02Fh,04Fh,024h,0E5h,0CDh,0C9h,0E1h,03Ah,042h,0E3h,0C6h,041h,032h,0C6h  ;0960h
   .DB 0E0h,001h,0BAh,0E0h,0CDh,0D3h,0E1h,0C1h,0CDh,0D3h,0E1h,021h,00Eh,0E3h,07Eh,036h  ;0970h
   .DB 000h,0B7h,0C0h,0C3h,009h,0EEh,0CDh,0FBh,0E0h,0CDh,014h,0E1h,0D8h,0F5h,04Fh,0CDh  ;0980h
   .DB 090h,0E1h,0F1h,0C9h,0FEh,00Dh,0C8h,0FEh,00Ah,0C8h,0FEh,009h,0C8h,0FEh,008h,0C8h  ;0990h
   .DB 0FEh,020h,0C9h,03Ah,00Eh,0E3h,0B7h,0C2h,045h,0E1h,0CDh,006h,0EEh,0E6h,001h,0C8h  ;09A0h
   .DB 0CDh,009h,0EEh,0FEh,013h,0C2h,042h,0E1h,0CDh,009h,0EEh,0FEh,003h,0CAh,000h,000h  ;09B0h
   .DB 0AFh,0C9h,032h,00Eh,0E3h,03Eh,001h,0C9h,03Ah,00Ah,0E3h,0B7h,0C2h,062h,0E1h,0C5h  ;09C0h
   .DB 0CDh,023h,0E1h,0C1h,0C5h,0CDh,00Ch,0EEh,0C1h,0C5h,03Ah,00Dh,0E3h,0B7h,0C4h,00Fh  ;09D0h
   .DB 0EEh,0C1h,079h,021h,00Ch,0E3h,0FEh,07Fh,0C8h,034h,0FEh,020h,0D0h,035h,07Eh,0B7h  ;09E0h
   .DB 0C8h,079h,0FEh,008h,0C2h,079h,0E1h,035h,0C9h,0FEh,00Ah,0C0h,036h,000h,0C9h,079h  ;09F0h
   .DB 0CDh,014h,0E1h,0D2h,090h,0E1h,0F5h,00Eh,05Eh,0CDh,048h,0E1h,0F1h,0F6h,040h,04Fh  ;0A00h
   .DB 079h,0FEh,009h,0C2h,048h,0E1h,00Eh,020h,0CDh,048h,0E1h,03Ah,00Ch,0E3h,0E6h,007h  ;0A10h
   .DB 0C2h,096h,0E1h,0C9h,0CDh,0ACh,0E1h,00Eh,020h,0CDh,00Ch,0EEh,00Eh,008h,0C3h,00Ch  ;0A20h
   .DB 0EEh,00Eh,023h,0CDh,048h,0E1h,0CDh,0C9h,0E1h,03Ah,00Ch,0E3h,021h,00Bh,0E3h,0BEh  ;0A30h
   .DB 0D0h,00Eh,020h,0CDh,048h,0E1h,0C3h,0B9h,0E1h,00Eh,00Dh,0CDh,048h,0E1h,00Eh,00Ah  ;0A40h
   .DB 0C3h,048h,0E1h,00Ah,0FEh,024h,0C8h,003h,0C5h,04Fh,0CDh,090h,0E1h,0C1h,0C3h,0D3h  ;0A50h
   .DB 0E1h,03Ah,00Ch,0E3h,032h,00Bh,0E3h,02Ah,043h,0E3h,04Eh,023h,0E5h,006h,000h,0C5h  ;0A60h
   .DB 0E5h,0CDh,0FBh,0E0h,0E6h,07Fh,0E1h,0C1h,0FEh,00Dh,0CAh,0C1h,0E2h,0FEh,00Ah,0CAh  ;0A70h
   .DB 0C1h,0E2h,0FEh,008h,0C2h,016h,0E2h,078h,0B7h,0CAh,0EFh,0E1h,005h,03Ah,00Ch,0E3h  ;0A80h
   .DB 032h,00Ah,0E3h,0C3h,070h,0E2h,0FEh,07Fh,0C2h,026h,0E2h,078h,0B7h,0CAh,0EFh,0E1h  ;0A90h
   .DB 07Eh,005h,02Bh,0C3h,0A9h,0E2h,0FEh,005h,0C2h,037h,0E2h,0C5h,0E5h,0CDh,0C9h,0E1h  ;0AA0h
   .DB 0AFh,032h,00Bh,0E3h,0C3h,0F1h,0E1h,0FEh,010h,0C2h,048h,0E2h,0E5h,021h,00Dh,0E3h  ;0AB0h
   .DB 03Eh,001h,096h,077h,0E1h,0C3h,0EFh,0E1h,0FEh,018h,0C2h,05Fh,0E2h,0E1h,03Ah,00Bh  ;0AC0h
   .DB 0E3h,021h,00Ch,0E3h,0BEh,0D2h,0E1h,0E1h,035h,0CDh,0A4h,0E1h,0C3h,04Eh,0E2h,0FEh  ;0AD0h
   .DB 015h,0C2h,06Bh,0E2h,0CDh,0B1h,0E1h,0E1h,0C3h,0E1h,0E1h,0FEh,012h,0C2h,0A6h,0E2h  ;0AE0h
   .DB 0C5h,0CDh,0B1h,0E1h,0C1h,0E1h,0E5h,0C5h,078h,0B7h,0CAh,08Ah,0E2h,023h,04Eh,005h  ;0AF0h
   .DB 0C5h,0E5h,0CDh,07Fh,0E1h,0E1h,0C1h,0C3h,078h,0E2h,0E5h,03Ah,00Ah,0E3h,0B7h,0CAh  ;0B00h
   .DB 0F1h,0E1h,021h,00Ch,0E3h,096h,032h,00Ah,0E3h,0CDh,0A4h,0E1h,021h,00Ah,0E3h,035h  ;0B10h
   .DB 0C2h,099h,0E2h,0C3h,0F1h,0E1h,023h,077h,004h,0C5h,0E5h,04Fh,0CDh,07Fh,0E1h,0E1h  ;0B20h
   .DB 0C1h,07Eh,0FEh,003h,078h,0C2h,0BDh,0E2h,0FEh,001h,0CAh,000h,000h,0B9h,0DAh,0EFh  ;0B30h
   .DB 0E1h,0E1h,070h,00Eh,00Dh,0C3h,048h,0E1h,0CDh,006h,0E1h,0C3h,001h,0E3h,0CDh,015h  ;0B40h
   .DB 0EEh,0C3h,001h,0E3h,079h,03Ch,0CAh,0E0h,0E2h,03Ch,0CAh,006h,0EEh,0C3h,00Ch,0EEh  ;0B50h
   .DB 0CDh,006h,0EEh,0B7h,0CAh,091h,0EDh,0CDh,009h,0EEh,0C3h,001h,0E3h,03Ah,003h,000h  ;0B60h
   .DB 0C3h,001h,0E3h,021h,003h,000h,071h,0C9h,0EBh,04Dh,044h,0C3h,0D3h,0E1h,0CDh,023h  ;0B70h
   .DB 0E1h,032h,045h,0E3h,0C9h,03Eh,001h,0C3h,001h,0E3h,000h,002h,000h,000h,000h,000h  ;0B80h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0B90h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0BA0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0BB0h
   .DB 000h,000h,000h,000h,000h,000h,000h,021h,00Bh,0E0h,05Eh,023h,056h,0EBh,0E9h,00Ch  ;0BC0h
   .DB 00Dh,0C8h,01Ah,077h,013h,023h,0C3h,050h,0E3h,03Ah,042h,0E3h,04Fh,0CDh,01Bh,0EEh  ;0BD0h
   .DB 07Ch,0B5h,0C8h,05Eh,023h,056h,023h,022h,0B3h,0EDh,023h,023h,022h,0B5h,0EDh,023h  ;0BE0h
   .DB 023h,022h,0B7h,0EDh,023h,023h,0EBh,022h,0D0h,0EDh,021h,0B9h,0EDh,00Eh,008h,0CDh  ;0BF0h
   .DB 04Fh,0E3h,02Ah,0BBh,0EDh,0EBh,021h,0C1h,0EDh,00Eh,00Fh,0CDh,04Fh,0E3h,02Ah,0C6h  ;0C00h
   .DB 0EDh,07Ch,021h,0DDh,0EDh,036h,0FFh,0B7h,0CAh,09Dh,0E3h,036h,000h,03Eh,0FFh,0B7h  ;0C10h
   .DB 0C9h,0CDh,018h,0EEh,0AFh,02Ah,0B5h,0EDh,077h,023h,077h,02Ah,0B7h,0EDh,077h,023h  ;0C20h
   .DB 077h,0C9h,0CDh,027h,0EEh,0C3h,0BBh,0E3h,0CDh,02Ah,0EEh,0B7h,0C8h,021h,009h,0E0h  ;0C30h
   .DB 0C3h,04Ah,0E3h,02Ah,0EAh,0EDh,00Eh,002h,0CDh,0EAh,0E4h,022h,0E5h,0EDh,022h,0ECh  ;0C40h
   .DB 0EDh,021h,0E5h,0EDh,04Eh,023h,046h,02Ah,0B7h,0EDh,05Eh,023h,056h,02Ah,0B5h,0EDh  ;0C50h
   .DB 07Eh,023h,066h,06Fh,079h,093h,078h,09Ah,0D2h,0FAh,0E3h,0E5h,02Ah,0C1h,0EDh,07Bh  ;0C60h
   .DB 095h,05Fh,07Ah,09Ch,057h,0E1h,02Bh,0C3h,0E4h,0E3h,0E5h,02Ah,0C1h,0EDh,019h,0DAh  ;0C70h
   .DB 00Fh,0E4h,079h,095h,078h,09Ch,0DAh,00Fh,0E4h,0EBh,0E1h,023h,0C3h,0FAh,0E3h,0E1h  ;0C80h
   .DB 0C5h,0D5h,0E5h,0EBh,02Ah,0CEh,0EDh,019h,044h,04Dh,0CDh,01Eh,0EEh,0D1h,02Ah,0B5h  ;0C90h
   .DB 0EDh,073h,023h,072h,0D1h,02Ah,0B7h,0EDh,073h,023h,072h,0C1h,079h,093h,04Fh,078h  ;0CA0h
   .DB 09Ah,047h,02Ah,0D0h,0EDh,0EBh,0CDh,030h,0EEh,04Dh,044h,0C3h,021h,0EEh,021h,0C3h  ;0CB0h
   .DB 0EDh,04Eh,03Ah,0E3h,0EDh,0B7h,01Fh,00Dh,0C2h,045h,0E4h,047h,03Eh,008h,096h,04Fh  ;0CC0h
   .DB 03Ah,0E2h,0EDh,00Dh,0CAh,05Ch,0E4h,0B7h,017h,0C3h,053h,0E4h,080h,0C9h,02Ah,043h  ;0CD0h
   .DB 0E3h,011h,010h,000h,019h,009h,03Ah,0DDh,0EDh,0B7h,0CAh,071h,0E4h,06Eh,026h,000h  ;0CE0h
   .DB 0C9h,009h,05Eh,023h,056h,0EBh,0C9h,0CDh,03Eh,0E4h,04Fh,006h,000h,0CDh,05Eh,0E4h  ;0CF0h
   .DB 022h,0E5h,0EDh,0C9h,02Ah,0E5h,0EDh,07Dh,0B4h,0C9h,03Ah,0C3h,0EDh,02Ah,0E5h,0EDh  ;0D00h
   .DB 029h,03Dh,0C2h,090h,0E4h,022h,0E7h,0EDh,03Ah,0C4h,0EDh,04Fh,03Ah,0E3h,0EDh,0A1h  ;0D10h
   .DB 0B5h,06Fh,022h,0E5h,0EDh,0C9h,02Ah,043h,0E3h,011h,00Ch,000h,019h,0C9h,02Ah,043h  ;0D20h
   .DB 0E3h,011h,00Fh,000h,019h,0EBh,021h,011h,000h,019h,0C9h,0CDh,0AEh,0E4h,07Eh,032h  ;0D30h
   .DB 0E3h,0EDh,0EBh,07Eh,032h,0E1h,0EDh,0CDh,0A6h,0E4h,03Ah,0C5h,0EDh,0A6h,032h,0E2h  ;0D40h
   .DB 0EDh,0C9h,0CDh,0AEh,0E4h,03Ah,0D5h,0EDh,0FEh,002h,0C2h,0DEh,0E4h,0AFh,04Fh,03Ah  ;0D50h
   .DB 0E3h,0EDh,081h,077h,0EBh,03Ah,0E1h,0EDh,077h,0C9h,00Ch,00Dh,0C8h,07Ch,0B7h,01Fh  ;0D60h
   .DB 067h,07Dh,01Fh,06Fh,0C3h,0EBh,0E4h,00Eh,080h,02Ah,0B9h,0EDh,0AFh,086h,023h,00Dh  ;0D70h
   .DB 0C2h,0FDh,0E4h,0C9h,00Ch,00Dh,0C8h,029h,0C3h,005h,0E5h,0C5h,03Ah,042h,0E3h,04Fh  ;0D80h
   .DB 021h,001h,000h,0CDh,004h,0E5h,0C1h,079h,0B5h,06Fh,078h,0B4h,067h,0C9h,02Ah,0ADh  ;0D90h
   .DB 0EDh,03Ah,042h,0E3h,04Fh,0CDh,0EAh,0E4h,07Dh,0E6h,001h,0C9h,021h,0ADh,0EDh,04Eh  ;0DA0h
   .DB 023h,046h,0CDh,00Bh,0E5h,022h,0ADh,0EDh,02Ah,0C8h,0EDh,023h,0EBh,02Ah,0B3h,0EDh  ;0DB0h
   .DB 073h,023h,072h,0C9h,0CDh,05Eh,0E5h,011h,009h,000h,019h,07Eh,017h,0D0h,021h,00Fh  ;0DC0h
   .DB 0E0h,0C3h,04Ah,0E3h,0CDh,01Eh,0E5h,0C8h,021h,00Dh,0E0h,0C3h,04Ah,0E3h,02Ah,0B9h  ;0DD0h
   .DB 0EDh,03Ah,0E9h,0EDh,085h,06Fh,0D0h,024h,0C9h,02Ah,043h,0E3h,011h,00Eh,000h,019h  ;0DE0h
   .DB 07Eh,0C9h,0CDh,069h,0E5h,036h,000h,0C9h,0CDh,069h,0E5h,0F6h,080h,077h,0C9h,02Ah  ;0DF0h
   .DB 0EAh,0EDh,0EBh,02Ah,0B3h,0EDh,07Bh,096h,023h,07Ah,09Eh,0C9h,0CDh,07Fh,0E5h,0D8h  ;0E00h
   .DB 013h,072h,02Bh,073h,0C9h,07Bh,095h,06Fh,07Ah,09Ch,067h,0C9h,00Eh,0FFh,02Ah,0ECh  ;0E10h
   .DB 0EDh,0EBh,02Ah,0CCh,0EDh,0CDh,095h,0E5h,0D0h,0C5h,0CDh,0F7h,0E4h,02Ah,0BDh,0EDh  ;0E20h
   .DB 0EBh,02Ah,0ECh,0EDh,019h,0C1h,00Ch,0CAh,0C4h,0E5h,0BEh,0C8h,0CDh,07Fh,0E5h,0D0h  ;0E30h
   .DB 0CDh,02Ch,0E5h,0C9h,077h,0C9h,0CDh,09Ch,0E5h,0CDh,0E0h,0E5h,00Eh,001h,0CDh,0B8h  ;0E40h
   .DB 0E3h,0C3h,0DAh,0E5h,0CDh,0E0h,0E5h,0CDh,0B2h,0E3h,021h,0B1h,0EDh,0C3h,0E3h,0E5h  ;0E50h
   .DB 021h,0B9h,0EDh,04Eh,023h,046h,0C3h,024h,0EEh,02Ah,0B9h,0EDh,0EBh,02Ah,0B1h,0EDh  ;0E60h
   .DB 00Eh,080h,0C3h,04Fh,0E3h,021h,0EAh,0EDh,07Eh,023h,0BEh,0C0h,03Ch,0C9h,021h,0FFh  ;0E70h
   .DB 0FFh,022h,0EAh,0EDh,0C9h,02Ah,0C8h,0EDh,0EBh,02Ah,0EAh,0EDh,023h,022h,0EAh,0EDh  ;0E80h
   .DB 0CDh,095h,0E5h,0D2h,019h,0E6h,0C3h,0FEh,0E5h,03Ah,0EAh,0EDh,0E6h,003h,006h,005h  ;0E90h
   .DB 087h,005h,0C2h,020h,0E6h,032h,0E9h,0EDh,0B7h,0C0h,0C5h,0CDh,0C3h,0E3h,0CDh,0D4h  ;0EA0h
   .DB 0E5h,0C1h,0C3h,09Eh,0E5h,079h,0E6h,007h,03Ch,05Fh,057h,079h,00Fh,00Fh,00Fh,0E6h  ;0EB0h
   .DB 01Fh,04Fh,078h,087h,087h,087h,087h,087h,0B1h,04Fh,078h,00Fh,00Fh,00Fh,0E6h,01Fh  ;0EC0h
   .DB 047h,02Ah,0BFh,0EDh,009h,07Eh,007h,01Dh,0C2h,056h,0E6h,0C9h,0D5h,0CDh,035h,0E6h  ;0ED0h
   .DB 0E6h,0FEh,0C1h,0B1h,00Fh,015h,0C2h,064h,0E6h,077h,0C9h,0CDh,05Eh,0E5h,011h,010h  ;0EE0h
   .DB 000h,019h,0C5h,00Eh,011h,0D1h,00Dh,0C8h,0D5h,03Ah,0DDh,0EDh,0B7h,0CAh,088h,0E6h  ;0EF0h
   .DB 0C5h,0E5h,04Eh,006h,000h,0C3h,08Eh,0E6h,00Dh,0C5h,04Eh,023h,046h,0E5h,079h,0B0h  ;0F00h
   .DB 0CAh,09Dh,0E6h,02Ah,0C6h,0EDh,07Dh,091h,07Ch,098h,0D4h,05Ch,0E6h,0E1h,023h,0C1h  ;0F10h
   .DB 0C3h,075h,0E6h,02Ah,0C6h,0EDh,00Eh,003h,0CDh,0EAh,0E4h,023h,044h,04Dh,02Ah,0BFh  ;0F20h
   .DB 0EDh,036h,000h,023h,00Bh,078h,0B1h,0C2h,0B1h,0E6h,02Ah,0CAh,0EDh,0EBh,02Ah,0BFh  ;0F30h
   .DB 0EDh,073h,023h,072h,0CDh,0A1h,0E3h,02Ah,0B3h,0EDh,036h,003h,023h,036h,000h,0CDh  ;0F40h
   .DB 0FEh,0E5h,00Eh,0FFh,0CDh,005h,0E6h,0CDh,0F5h,0E5h,0C8h,0CDh,05Eh,0E5h,03Eh,0E5h  ;0F50h
   .DB 0BEh,0CAh,0D2h,0E6h,03Ah,041h,0E3h,0BEh,0C2h,0F6h,0E6h,023h,07Eh,0D6h,024h,0C2h  ;0F60h
   .DB 0F6h,0E6h,03Dh,032h,045h,0E3h,00Eh,001h,0CDh,06Bh,0E6h,0CDh,08Ch,0E5h,0C3h,0D2h  ;0F70h
   .DB 0E6h,03Ah,0D4h,0EDh,0C3h,001h,0E3h,0C5h,0F5h,03Ah,0C5h,0EDh,02Fh,047h,079h,0A0h  ;0F80h
   .DB 04Fh,0F1h,0A0h,091h,0E6h,01Fh,0C1h,0C9h,03Eh,0FFh,032h,0D4h,0EDh,021h,0D8h,0EDh  ;0F90h
   .DB 071h,02Ah,043h,0E3h,022h,0D9h,0EDh,0CDh,0FEh,0E5h,0CDh,0A1h,0E3h,00Eh,000h,0CDh  ;0FA0h
   .DB 005h,0E6h,0CDh,0F5h,0E5h,0CAh,094h,0E7h,02Ah,0D9h,0EDh,0EBh,01Ah,0FEh,0E5h,0CAh  ;0FB0h
   .DB 04Ah,0E7h,0D5h,0CDh,07Fh,0E5h,0D1h,0D2h,094h,0E7h,0CDh,05Eh,0E5h,03Ah,0D8h,0EDh  ;0FC0h
   .DB 04Fh,006h,000h,079h,0B7h,0CAh,083h,0E7h,01Ah,0FEh,03Fh,0CAh,07Ch,0E7h,078h,0FEh  ;0FD0h
   .DB 00Dh,0CAh,07Ch,0E7h,0FEh,00Ch,01Ah,0CAh,073h,0E7h,096h,0E6h,07Fh,0C2h,02Dh,0E7h  ;0FE0h
   .DB 0C3h,07Ch,0E7h,0C5h,04Eh,0CDh,007h,0E7h,0C1h,0C2h,02Dh,0E7h,013h,023h,004h,00Dh  ;0FF0h
   .DB 0C3h,053h,0E7h,03Ah,0EAh,0EDh,0E6h,003h,032h,045h,0E3h,021h,0D4h,0EDh,07Eh,017h  ;1000h
   .DB 0D0h,0AFh,077h,0C9h,0CDh,0FEh,0E5h,03Eh,0FFh,0C3h,001h,0E3h,0CDh,054h,0E5h,00Eh  ;1010h
   .DB 00Ch,0CDh,018h,0E7h,0CDh,0F5h,0E5h,0C8h,0CDh,044h,0E5h,0CDh,05Eh,0E5h,036h,0E5h  ;1020h
   .DB 00Eh,000h,0CDh,06Bh,0E6h,0CDh,0C6h,0E5h,0CDh,02Dh,0E7h,0C3h,0A4h,0E7h,050h,059h  ;1030h
   .DB 079h,0B0h,0CAh,0D1h,0E7h,00Bh,0D5h,0C5h,0CDh,035h,0E6h,01Fh,0D2h,0ECh,0E7h,0C1h  ;1040h
   .DB 0D1h,02Ah,0C6h,0EDh,07Bh,095h,07Ah,09Ch,0D2h,0F4h,0E7h,013h,0C5h,0D5h,042h,04Bh  ;1050h
   .DB 0CDh,035h,0E6h,01Fh,0D2h,0ECh,0E7h,0D1h,0C1h,0C3h,0C0h,0E7h,017h,03Ch,0CDh,064h  ;1060h
   .DB 0E6h,0E1h,0D1h,0C9h,079h,0B0h,0C2h,0C0h,0E7h,021h,000h,000h,0C9h,00Eh,000h,01Eh  ;1070h
   .DB 020h,0D5h,006h,000h,02Ah,043h,0E3h,009h,0EBh,0CDh,05Eh,0E5h,0C1h,0CDh,04Fh,0E3h  ;1080h
   .DB 0CDh,0C3h,0E3h,0C3h,0C6h,0E5h,0CDh,054h,0E5h,00Eh,00Ch,0CDh,018h,0E7h,02Ah,043h  ;1090h
   .DB 0E3h,07Eh,011h,010h,000h,019h,077h,0CDh,0F5h,0E5h,0C8h,0CDh,044h,0E5h,00Eh,010h  ;10A0h
   .DB 01Eh,00Ch,0CDh,001h,0E8h,0CDh,02Dh,0E7h,0C3h,027h,0E8h,00Eh,00Ch,0CDh,018h,0E7h  ;10B0h
   .DB 0CDh,0F5h,0E5h,0C8h,00Eh,000h,01Eh,00Ch,0CDh,001h,0E8h,0CDh,02Dh,0E7h,0C3h,040h  ;10C0h
   .DB 0E8h,00Eh,00Fh,0CDh,018h,0E7h,0CDh,0F5h,0E5h,0C8h,0CDh,0A6h,0E4h,07Eh,0F5h,0E5h  ;10D0h
   .DB 0CDh,05Eh,0E5h,0EBh,02Ah,043h,0E3h,00Eh,020h,0D5h,0CDh,04Fh,0E3h,0CDh,078h,0E5h  ;10E0h
   .DB 0D1h,021h,00Ch,000h,019h,04Eh,021h,00Fh,000h,019h,046h,0E1h,0F1h,077h,079h,0BEh  ;10F0h
   .DB 078h,0CAh,08Bh,0E8h,03Eh,000h,0DAh,08Bh,0E8h,03Eh,080h,02Ah,043h,0E3h,011h,00Fh  ;1100h
   .DB 000h,019h,077h,0C9h,07Eh,023h,0B6h,02Bh,0C0h,01Ah,077h,013h,023h,01Ah,077h,01Bh  ;1110h
   .DB 02Bh,0C9h,0AFh,032h,045h,0E3h,032h,0EAh,0EDh,032h,0EBh,0EDh,0CDh,01Eh,0E5h,0C0h  ;1120h
   .DB 0CDh,069h,0E5h,0E6h,080h,0C0h,00Eh,00Fh,0CDh,018h,0E7h,0CDh,0F5h,0E5h,0C8h,001h  ;1130h
   .DB 010h,000h,0CDh,05Eh,0E5h,009h,0EBh,02Ah,043h,0E3h,009h,00Eh,010h,03Ah,0DDh,0EDh  ;1140h
   .DB 0B7h,0CAh,0E8h,0E8h,07Eh,0B7h,01Ah,0C2h,0DBh,0E8h,077h,0B7h,0C2h,0E1h,0E8h,07Eh  ;1150h
   .DB 012h,0BEh,0C2h,01Fh,0E9h,0C3h,0FDh,0E8h,0CDh,094h,0E8h,0EBh,0CDh,094h,0E8h,0EBh  ;1160h
   .DB 01Ah,0BEh,0C2h,01Fh,0E9h,013h,023h,01Ah,0BEh,0C2h,01Fh,0E9h,00Dh,013h,023h,00Dh  ;1170h
   .DB 0C2h,0CDh,0E8h,001h,0ECh,0FFh,009h,0EBh,009h,01Ah,0BEh,0DAh,017h,0E9h,077h,001h  ;1180h
   .DB 003h,000h,009h,0EBh,009h,07Eh,012h,03Eh,0FFh,032h,0D2h,0EDh,0C3h,010h,0E8h,021h  ;1190h
   .DB 045h,0E3h,035h,0C9h,0CDh,054h,0E5h,02Ah,043h,0E3h,0E5h,021h,0ACh,0EDh,022h,043h  ;11A0h
   .DB 0E3h,00Eh,001h,0CDh,018h,0E7h,0CDh,0F5h,0E5h,0E1h,022h,043h,0E3h,0C8h,0EBh,021h  ;11B0h
   .DB 00Fh,000h,019h,00Eh,011h,0AFh,077h,023h,00Dh,0C2h,046h,0E9h,021h,00Dh,000h,019h  ;11C0h
   .DB 077h,0CDh,08Ch,0E5h,0CDh,0FDh,0E7h,0C3h,078h,0E5h,0AFh,032h,0D2h,0EDh,0CDh,0A2h  ;11D0h
   .DB 0E8h,0CDh,0F5h,0E5h,0C8h,02Ah,043h,0E3h,001h,00Ch,000h,009h,07Eh,03Ch,0E6h,01Fh  ;11E0h
   .DB 077h,0CAh,083h,0E9h,047h,03Ah,0C5h,0EDh,0A0h,021h,0D2h,0EDh,0A6h,0CAh,08Eh,0E9h  ;11F0h
   .DB 0C3h,0ACh,0E9h,001h,002h,000h,009h,034h,07Eh,0E6h,00Fh,0CAh,0B6h,0E9h,00Eh,00Fh  ;1200h
   .DB 0CDh,018h,0E7h,0CDh,0F5h,0E5h,0C2h,0ACh,0E9h,03Ah,0D3h,0EDh,03Ch,0CAh,0B6h,0E9h  ;1210h
   .DB 0CDh,024h,0E9h,0CDh,0F5h,0E5h,0CAh,0B6h,0E9h,0C3h,0AFh,0E9h,0CDh,05Ah,0E8h,0CDh  ;1220h
   .DB 0BBh,0E4h,0AFh,0C3h,001h,0E3h,0CDh,005h,0E3h,0C3h,078h,0E5h,03Eh,001h,032h,0D5h  ;1230h
   .DB 0EDh,03Eh,0FFh,032h,0D3h,0EDh,0CDh,0BBh,0E4h,03Ah,0E3h,0EDh,021h,0E1h,0EDh,0BEh  ;1240h
   .DB 0DAh,0E6h,0E9h,0FEh,080h,0C2h,0FBh,0E9h,0CDh,05Ah,0E9h,0AFh,032h,0E3h,0EDh,03Ah  ;1250h
   .DB 045h,0E3h,0B7h,0C2h,0FBh,0E9h,0CDh,077h,0E4h,0CDh,084h,0E4h,0CAh,0FBh,0E9h,0CDh  ;1260h
   .DB 08Ah,0E4h,0CDh,0D1h,0E3h,0CDh,0B2h,0E3h,0C3h,0D2h,0E4h,0C3h,005h,0E3h,03Eh,001h  ;1270h
   .DB 032h,0D5h,0EDh,03Eh,000h,032h,0D3h,0EDh,0CDh,054h,0E5h,02Ah,043h,0E3h,0CDh,047h  ;1280h
   .DB 0E5h,0CDh,0BBh,0E4h,03Ah,0E3h,0EDh,0FEh,080h,0D2h,005h,0E3h,0CDh,077h,0E4h,0CDh  ;1290h
   .DB 084h,0E4h,00Eh,000h,0C2h,06Eh,0EAh,0CDh,03Eh,0E4h,032h,0D7h,0EDh,001h,000h,000h  ;12A0h
   .DB 0B7h,0CAh,03Bh,0EAh,04Fh,00Bh,0CDh,05Eh,0E4h,044h,04Dh,0CDh,0BEh,0E7h,07Dh,0B4h  ;12B0h
   .DB 0C2h,048h,0EAh,03Eh,002h,0C3h,001h,0E3h,022h,0E5h,0EDh,0EBh,02Ah,043h,0E3h,001h  ;12C0h
   .DB 010h,000h,009h,03Ah,0DDh,0EDh,0B7h,03Ah,0D7h,0EDh,0CAh,064h,0EAh,0CDh,064h,0E5h  ;12D0h
   .DB 073h,0C3h,06Ch,0EAh,04Fh,006h,000h,009h,009h,073h,023h,072h,00Eh,002h,03Ah,045h  ;12E0h
   .DB 0E3h,0B7h,0C0h,0C5h,0CDh,08Ah,0E4h,03Ah,0D5h,0EDh,03Dh,03Dh,0C2h,0BBh,0EAh,0C1h  ;12F0h
   .DB 0C5h,079h,03Dh,03Dh,0C2h,0BBh,0EAh,0E5h,02Ah,0B9h,0EDh,057h,077h,023h,014h,0F2h  ;1300h
   .DB 08Ch,0EAh,0CDh,0E0h,0E5h,02Ah,0E7h,0EDh,00Eh,002h,022h,0E5h,0EDh,0C5h,0CDh,0D1h  ;1310h
   .DB 0E3h,0C1h,0CDh,0B8h,0E3h,02Ah,0E5h,0EDh,00Eh,000h,03Ah,0C4h,0EDh,047h,0A5h,0B8h  ;1320h
   .DB 023h,0C2h,09Ah,0EAh,0E1h,022h,0E5h,0EDh,0CDh,0DAh,0E5h,0CDh,0D1h,0E3h,0C1h,0C5h  ;1330h
   .DB 0CDh,0B8h,0E3h,0C1h,03Ah,0E3h,0EDh,021h,0E1h,0EDh,0BEh,0DAh,0D2h,0EAh,077h,034h  ;1340h
   .DB 00Eh,002h,000h,000h,021h,000h,000h,0F5h,0CDh,069h,0E5h,0E6h,07Fh,077h,0F1h,0FEh  ;1350h
   .DB 07Fh,0C2h,000h,0EBh,03Ah,0D5h,0EDh,0FEh,001h,0C2h,000h,0EBh,0CDh,0D2h,0E4h,0CDh  ;1360h
   .DB 05Ah,0E9h,021h,045h,0E3h,07Eh,0B7h,0C2h,0FEh,0EAh,03Dh,032h,0E3h,0EDh,036h,000h  ;1370h
   .DB 0C3h,0D2h,0E4h,0AFh,032h,0D5h,0EDh,0C5h,02Ah,043h,0E3h,0EBh,021h,021h,000h,019h  ;1380h
   .DB 07Eh,0E6h,07Fh,0F5h,07Eh,017h,023h,07Eh,017h,0E6h,01Fh,04Fh,07Eh,01Fh,01Fh,01Fh  ;1390h
   .DB 01Fh,0E6h,00Fh,047h,0F1h,023h,06Eh,02Ch,02Dh,02Eh,006h,0C2h,08Bh,0EBh,021h,020h  ;13A0h
   .DB 000h,019h,077h,021h,00Ch,000h,019h,079h,096h,0C2h,047h,0EBh,021h,00Eh,000h,019h  ;13B0h
   .DB 078h,096h,0E6h,07Fh,0CAh,07Fh,0EBh,0C5h,0D5h,0CDh,0A2h,0E8h,0D1h,0C1h,02Eh,003h  ;13C0h
   .DB 03Ah,045h,0E3h,03Ch,0CAh,084h,0EBh,021h,00Ch,000h,019h,071h,021h,00Eh,000h,019h  ;13D0h
   .DB 070h,0CDh,051h,0E8h,03Ah,045h,0E3h,03Ch,0C2h,07Fh,0EBh,0C1h,0C5h,02Eh,004h,00Ch  ;13E0h
   .DB 0CAh,084h,0EBh,0CDh,024h,0E9h,02Eh,005h,03Ah,045h,0E3h,03Ch,0CAh,084h,0EBh,0C1h  ;13F0h
   .DB 0AFh,0C3h,001h,0E3h,0E5h,0CDh,069h,0E5h,036h,0C0h,0E1h,0C1h,07Dh,032h,045h,0E3h  ;1400h
   .DB 0C3h,078h,0E5h,00Eh,0FFh,0CDh,003h,0EBh,0CCh,0C1h,0E9h,0C9h,00Eh,000h,0CDh,003h  ;1410h
   .DB 0EBh,0CCh,003h,0EAh,0C9h,0EBh,019h,04Eh,006h,000h,021h,00Ch,000h,019h,07Eh,00Fh  ;1420h
   .DB 0E6h,080h,081h,04Fh,03Eh,000h,088h,047h,07Eh,00Fh,0E6h,00Fh,080h,047h,021h,00Eh  ;1430h
   .DB 000h,019h,07Eh,087h,087h,087h,087h,0F5h,080h,047h,0F5h,0E1h,07Dh,0E1h,0B5h,0E6h  ;1440h
   .DB 001h,0C9h,00Eh,00Ch,0CDh,018h,0E7h,02Ah,043h,0E3h,011h,021h,000h,019h,0E5h,072h  ;1450h
   .DB 023h,072h,023h,072h,0CDh,0F5h,0E5h,0CAh,00Ch,0ECh,0CDh,05Eh,0E5h,011h,00Fh,000h  ;1460h
   .DB 0CDh,0A5h,0EBh,0E1h,0E5h,05Fh,079h,096h,023h,078h,09Eh,023h,07Bh,09Eh,0DAh,006h  ;1470h
   .DB 0ECh,073h,02Bh,070h,02Bh,071h,0CDh,02Dh,0E7h,0C3h,0E4h,0EBh,0E1h,0C9h,02Ah,043h  ;1480h
   .DB 0E3h,011h,020h,000h,0CDh,0A5h,0EBh,021h,021h,000h,019h,071h,023h,070h,023h,077h  ;1490h
   .DB 0C9h,02Ah,0AFh,0EDh,03Ah,042h,0E3h,04Fh,0CDh,0EAh,0E4h,0E5h,0EBh,0CDh,059h,0E3h  ;14A0h
   .DB 0E1h,0CCh,047h,0E3h,07Dh,01Fh,0D8h,02Ah,0AFh,0EDh,04Dh,044h,0CDh,00Bh,0E5h,022h  ;14B0h
   .DB 0AFh,0EDh,0C3h,0A3h,0E6h,03Ah,0D6h,0EDh,021h,042h,0E3h,0BEh,0C8h,077h,0C3h,021h  ;14C0h
   .DB 0ECh,03Eh,0FFh,032h,0DEh,0EDh,02Ah,043h,0E3h,07Eh,0E6h,01Fh,03Dh,032h,0D6h,0EDh  ;14D0h
   .DB 0FEh,01Eh,0D2h,075h,0ECh,03Ah,042h,0E3h,032h,0DFh,0EDh,07Eh,032h,0E0h,0EDh,0E6h  ;14E0h
   .DB 0E0h,077h,0CDh,045h,0ECh,03Ah,041h,0E3h,02Ah,043h,0E3h,0B6h,077h,0C9h,03Eh,022h  ;14F0h
   .DB 0C3h,001h,0E3h,021h,000h,000h,022h,0ADh,0EDh,022h,0AFh,0EDh,0AFh,032h,042h,0E3h  ;1500h
   .DB 021h,080h,000h,022h,0B1h,0EDh,0CDh,0DAh,0E5h,0C3h,021h,0ECh,0CDh,072h,0E5h,0CDh  ;1510h
   .DB 051h,0ECh,0C3h,051h,0E8h,0CDh,051h,0ECh,0C3h,0A2h,0E8h,00Eh,000h,0EBh,07Eh,0FEh  ;1520h
   .DB 03Fh,0CAh,0C2h,0ECh,0CDh,0A6h,0E4h,07Eh,0FEh,03Fh,0C4h,072h,0E5h,0CDh,051h,0ECh  ;1530h
   .DB 00Eh,00Fh,0CDh,018h,0E7h,0C3h,0E9h,0E5h,02Ah,0D9h,0EDh,022h,043h,0E3h,0CDh,051h  ;1540h
   .DB 0ECh,0CDh,02Dh,0E7h,0C3h,0E9h,0E5h,0CDh,051h,0ECh,0CDh,09Ch,0E7h,0C3h,001h,0E7h  ;1550h
   .DB 0CDh,051h,0ECh,0C3h,0BCh,0E9h,0CDh,051h,0ECh,0C3h,0FEh,0E9h,0CDh,072h,0E5h,0CDh  ;1560h
   .DB 051h,0ECh,0C3h,024h,0E9h,0CDh,051h,0ECh,0CDh,016h,0E8h,0C3h,001h,0E7h,02Ah,0AFh  ;1570h
   .DB 0EDh,0C3h,029h,0EDh,03Ah,042h,0E3h,0C3h,001h,0E3h,0EBh,022h,0B1h,0EDh,0C3h,0DAh  ;1580h
   .DB 0E5h,02Ah,0BFh,0EDh,0C3h,029h,0EDh,02Ah,0ADh,0EDh,0C3h,029h,0EDh,0CDh,051h,0ECh  ;1590h
   .DB 0CDh,03Bh,0E8h,0C3h,001h,0E7h,02Ah,0BBh,0EDh,022h,045h,0E3h,0C9h,03Ah,0D6h,0EDh  ;15A0h
   .DB 0FEh,0FFh,0C2h,03Bh,0EDh,03Ah,041h,0E3h,0C3h,001h,0E3h,0E6h,01Fh,032h,041h,0E3h  ;15B0h
   .DB 0C9h,0CDh,051h,0ECh,0C3h,093h,0EBh,0CDh,051h,0ECh,0C3h,09Ch,0EBh,0CDh,051h,0ECh  ;15C0h
   .DB 0C3h,0D2h,0EBh,02Ah,043h,0E3h,07Dh,02Fh,05Fh,07Ch,02Fh,02Ah,0AFh,0EDh,0A4h,057h  ;15D0h
   .DB 07Dh,0A3h,05Fh,02Ah,0ADh,0EDh,0EBh,022h,0AFh,0EDh,07Dh,0A3h,06Fh,07Ch,0A2h,067h  ;15E0h
   .DB 022h,0ADh,0EDh,0C9h,03Ah,0DEh,0EDh,0B7h,0CAh,091h,0EDh,02Ah,043h,0E3h,036h,000h  ;15F0h
   .DB 03Ah,0E0h,0EDh,0B7h,0CAh,091h,0EDh,077h,03Ah,0DFh,0EDh,032h,0D6h,0EDh,0CDh,045h  ;1600h
   .DB 0ECh,02Ah,00Fh,0E3h,0F9h,02Ah,045h,0E3h,07Dh,044h,0C9h,0CDh,051h,0ECh,03Eh,002h  ;1610h
   .DB 032h,0D5h,0EDh,00Eh,000h,0CDh,007h,0EBh,0CCh,003h,0EAh,0C9h,0E5h,000h,000h,000h  ;1620h
   .DB 000h,080h ,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1630h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1640h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1650h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1660h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1670h

;BIOS Jump Table
;   .DB 0C3h,000h,000h,0C3h,000h,000h,0C3h,000h,000h,0C3h,000h,000h,0C3h,000h,000h,0C3h  ;1680h
;   .DB 000h,000h,0C3h,000h,000h,0C3h,000h,000h,0C3h,000h,000h,0C3h,000h,000h,0C3h,000h  ;1690h
;   .DB 000h,0C3h,000h,000h,0C3h,000h,000h,0C3h,000h,000h,0C3h,000h,000h,0C3h,000h,000h  ;16A0h
;   .DB 0C3h,000h,000h,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh  ;16B0h

;***************************************************
;*** THIS BEGINS THE AREA WHICH REQUIRES CHANGES ***
;***      FOR DIFFERENT CONSOLE I/O SYSTEMS      ***
;***************************************************


;		.ORG	BIOS

; I/O JUMP VECTOR
; THIS IS WHERE CP/M CALLS WHENEVER IT NEEDS
; TO DO ANY INPUT/OUTPUT OPERATION.
; USER PROGRAMS MAY USE THESE ENTRY POINTS
; ALSO, BUT NOTE THAT THE LOCATION OF THIS
; VECTOR CHANGES WITH THE MEMORY SIZE.
;
	CALL	GO_ROM	;FROM COLD START LOADER.
WBOOTV	.EQU	$ + RB
	CALL	GO_ROM	;FROM WARM BOOT.
	CALL	GO_ROM	;CHECK CONSOLE KB STATUS.
	CALL	GO_ROM	;READ CONSOLE CHARACTER.
	CALL	GO_ROM	;WRITE CONSOLE CHARACTER.
	CALL	GO_ROM	;WRITE LISTING CHAR.
	CALL	GO_ROM	;WRITE PUNCH CHAR.
	CALL	GO_ROM 	;READ READER CHAR.
	CALL	GO_ROM	;MOVE DISK TO TRACK ZERO.
	CALL	GO_ROM 	;SELECT DISK DRIVE.
	CALL	GO_ROM 	;SEEK TO TRACK IN REG A.
	CALL	GO_ROM 	;SET SECTOR NUMBER.
	CALL	GO_ROM 	;SET DISK STARTING ADR.
	CALL	GO_ROM	;READ SELECTED SECTOR.
	CALL	GO_ROM2	;WRITE SELECTED SECTOR.
	CALL	GO_ROM 	;List status (output)
	CALL	GO_ROM 	;Translate sector number

GO_ROM2		.EQU	$ + RB	;Special Case for Disk Write.  Must fetch DMA Buffer to Sector Buffer first.
		LHLD	DMAADD	;Source CP/M Data Buffer
		LXI	D,SECTOR_BUFFER
		MVI	B,128
		CALL	COPY_RAM	;Copy the CP/M DMAADD to SECTOR_BUFFER
	
GO_ROM		.EQU	$ + RB
;-------------------------------------------------------------------------
;Use Return address on Stack to select the ROM call to be placed
;Add new Return address on Stack for proper return to RAM space
;	STACK 				NEW BIOS STACK
;	Before		After		
;	CP/M RETURN	CP/M RETURN	RAM_RET
;	BIOS+3 RETURN			CALL (Routine Returns to this address)
;			
;
		SHLD	HL_TEMP		;Save HL for elbow room
		LXI	H,RAM_RET
		SHLD	STACK-2	;Put on New Stack
		POP	H	;HL = BIOS+3 RETURN
		MVI	H,(ROM_JUMP_TABLE >> 8)
		SHLD	STACK-4	;Put on New Stack
		LXI	H,0
		DAD	SP
		SHLD	SP_BIOS
		LHLD	HL_TEMP		;Restore HL
		LXI	SP,STACK-4	;Assign NEW STACK in upper RAM!

JMP_ROM_LOW	.EQU	$ + RB
		XRA	A
		STA	ROM_SELECT
		OUT	ROM_LOW		;Map ROM into Lower 32K
		RET		;JUMP to Modified Address (ROM JUMP TABLE)


RAM_RET		.EQU	$ + RB
		PUSH	PSW
		MVI	A,1
		STA	ROM_SELECT
		OUT	ROM_LOW		;Map RAM back into Lower 32K
		POP	PSW
		SHLD	HL_TEMP
		LHLD	SP_BIOS
		SPHL		;SP = Restored
		LHLD	HL_TEMP
		RET		

	
NDSK	.EQU	1

;	Control Blocks for disk drives DPH

DPBASE	.EQU	$ + RB
	.DW	SKEW,0,0,0,DIRBUF,DPB0,CSV0,ALV0	;Drive A:

SKEW	.equ	0

;512K RAM  allows for 64K System + 14 Tracks of 256 Sectors
; 14 * 256 = 3584 Sectors
; 3584 / 16 = 224 Blocks of 2K (448K Disk)  458,752 Bytes
; Plus an additional 10K of ROM
;       Disk type definition blocks for each particular mode.
DPB0	.EQU	$ + RB
	.DW	256		;SEC PER TRACK
	.DB	4		;BLOCK SHIFT
	.DB	15		;BLOCK MASK
	.DB	1		;EXTNT MASK
	.DW	223		;DISK SIZE-1    224 = 448K RAM, 229 = 458K RAM/ROM
	.DW	127		;DIRECTORY MAX  128 entries * 32 = 4K
	.DB	11000000b	;ALLOC0         2 blocks for dir = 4K
	.DB	00000000b	;ALLOC1
	.DW	32		;CHECK SIZE
	.DW	0		;OFFSET

RAM_WBOOT	.EQU	$ + RB
		MVI	A,1
		STA	ROM_SELECT
		OUT	ROM_LOW
		LDA	CDISK
		MOV	C,A
		JMP	CCP             ;Go to CPM

READ_MEMORY	.EQU	$ + RB
		LDA	MEM_SOURCE	;1=RAM, 0=ROM
		DI
		OUT	ROM_LOW		;Map RAM back into Lower 32K
		MOV	A,M
		PUSH	PSW
		XRA	A
		OUT	ROM_LOW		;Map ROM into Lower 32K
		EI
		POP	PSW
		RET

;-----------------------------------------------------------------------------------------------------
COPY_RAM	.EQU	$ + RB
		MOV	A,M
		STAX	D
		INX	H
		INX	D
		DCR	B
		JNZ	COPY_RAM
		RET


;-----------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------

;                       *********   *******    ********                         
;                       *********  *********   *********                        
;                          ***     **     **   **     **                        
;                          ***     **          **     **                        
;---------------------     ***     *******     ********   --------------------- 
;---------------------     ***       *******   ********   --------------------- 
;                          ***            **   **  **                           
;                          ***     **     **   **   **                          
;                       *********  *********   **    **                         
;                       *********   *******    **     **                        
;                                                                               









ISR_OTHER	.EQU	$ + RB
		RAL
		RAL
		JC	ISR_EXIT
		
ISR_TIMER	.EQU	$ + RB
		XRA	A	;TURN OFF TIMER
		OUT	TIMER
		EI		;4

		XRA	A
		LXI	H,ISR_LOCK
		ORA	M
		JNZ	ISR_EXIT	;Exit if ISR_TIMER is already running
		INR	M

		LXI	H,0
		DAD	SP
		SHLD	SP_ISR		
		LXI	SP,STACK_ISR	;Assign NEW STACK in upper RAM!

		XRA	A
		OUT	ROM_LOW		;Map ROM into Lower 32K
		
		CALL	ISR_TIMER_ROM		
		LDA	ROM_SELECT
		OUT	ROM_LOW		;Restore ROM/RAM Mapped


		LHLD	SP_ISR
		SPHL		;SP = Restored

		XRA	A	;Clear Lock on ISR
		STA	ISR_LOCK

		LDA	TIMER_RUN
		OUT	TIMER
		

ISR_EXIT	.EQU	$ + RB
		POP	H	;10
		POP	PSW	;10
		EI		;4
		RET		;10	tc=83


RAM_SIZE	.EQU	$ - RAM_IMG 	;$16F1

;-----------------------------------------------------------------------------------------------------
				;9,600, 208t = 104uSec
ISR_RAM		.EQU	$ + RB

IR1_ROM		.EQU	$
		;RST	038H	;11   5.5uSEC
		;JMP	ISR_RAM	;10	Jump to ISR

		PUSH	PSW	;11 
		PUSH	H	;11
		
		IN	INPUT_PORT ;10  Sample Start Bit
		RAL		;4
		JC	ISR_OTHER ;10

		LDA	RXBHEAD	;Test over flow of buffer during
		INR	A	;Start Bit delay (to minimize exit time)
				;Test if buffer has over ran
		LXI	H,RXBTAIL
		CMP	M	;If Tail = Head Then Tail = Tail + 1 & Flag overrun
		JNZ	IR1_NOT_OVER
		INR	M	;Advance Tail (lose char)
		
IR1_NOT_OVER	.EQU	$ + RB

		MVI	H,8	;7

				;     Need 1.5 bit time to sample first data bit
				;     = 104 + 52 = 156uS = 312 Cycles
				;     
		MVI	A,12	;7
IR1_LP1		.EQU	$ + RB
		DCR	A	;5	;58 + D * 15
		JNZ	IR1_LP1	;10	;58 + 17 * 15 = 313 tc

		
IR1_LP2		.EQU	$ + RB
		IN	INPUT_PORT ;10	;Fetch bit      ;REQUIRE 208 CYCLES BETWEEN BITS
		RLC		;4	;Carry = bit
		MOV	A,L	;5
		RAR		;4	
		MOV	L,A	;5	
		NOP		;4		
		NOP		;4     36
		MVI	A,10	;7
IR1_LP3		.EQU	$ + RB
		DCR	A	;5	;58 + D * 15
		JNZ	IR1_LP3	;10	;58 + 10 * 15 = 208
		
		DCR	H	;5
		JNZ	IR1_LP2	;10

		MOV	A,L
		LHLD	RXBHEAD
		MOV	M,A	;Save Received byte into RX Buffer
		INR	L	;Advance Head Ptr of RX Buffer, Head = Head + 1
		SHLD	RXBHEAD
		POP	H	;10
		POP	PSW	;10
		EI		;4
		RET		;10	tc=83
IR1_SIZE	.EQU	$ - IR1_ROM 

;-----------------------------------------------------------------------------------------------------
				;19,200, 104t = 52uSec
IR2_ROM		.EQU	$
		;RST	038H	;11   5.5uSEC
		;JMP	ISR_RAM	;10	Jump to ISR

		PUSH	PSW	;11 
		PUSH	H	;11
		
		IN	INPUT_PORT ;10  Sample Start Bit
		RAL		;4
		JC	ISR_OTHER ;10

		LDA	RXBHEAD	;13 Test over flow of buffer during
		INR	A	;5  Start Bit delay (to minimize exit time)
				;   Test if buffer has over ran
		LXI	H,RXBTAIL ;10
		CMP	M	;7  If Tail = Head Then Tail = Tail + 1 & Flag overrun
		JNZ	IR2_NOT_OVER ;10
		INR	M	;Advance Tail (lose char)
IR2_NOT_OVER	.EQU	$ + RB - IR1_SIZE 
		MVI	L,07FH	;7
				; t = 119

				;     Need 1.5 bit time to sample first data bit
				;     = 52 + 26 = 78uS = 156 Cycles
				;     
		
IR2_LP2		.EQU	$ + RB - IR1_SIZE 
		IN	INPUT_PORT ;10	;Fetch bit      ;REQUIRE 104 CYCLES BETWEEN BITS
		RLC		;4	;Carry = bit
		MOV	A,L	;5
		RAR		;4	
		MOV	L,A	;5
		MVI	A,4	;7
IR2_LP3		.EQU	$ + RB - IR1_SIZE 
		DCR	A	;5
		JNZ	IR2_LP3	;10
		JC	IR2_LP2	;10	tc=104

		MOV	A,L	;5
		LHLD	RXBHEAD	;16
		MOV	M,A	;7 Save Received byte into RX Buffer
		INR	L	;5 Advance Head Ptr of RX Buffer, Head = Head + 1
		SHLD	RXBHEAD	;16
		POP	H	;10
		POP	PSW	;10
		EI		;4
		RET		;10	tc=83
IR2_SIZE	.EQU	$ - IR2_ROM 

;-----------------------------------------------------------------------------------------------------
				;38,400, 52t = 26uSec
IR3_ROM		.EQU	$
		;RST	038H	;11   5.5uSEC
		;JMP	ISR_RAM	;10	Jump to ISR

		PUSH	PSW	;11
		PUSH	H	;11

				;T=53

IR3_LP1		.EQU	$ + RB - (IR1_SIZE + IR2_SIZE)

		MVI	H,07FH	;7   
		JMP	IR3_LP3

IR3_RP1		.EQU	$ + RB - (IR1_SIZE + IR2_SIZE)
		NOP		;4
		MOV	A,M	;7
		MOV	A,M	;7
		JMP	IR3_LP1	;10
		
IR3_LP2		.EQU	$ + RB - (IR1_SIZE + IR2_SIZE)

		MOV	H,A	;5	
		MOV	L,M	;7
		MOV	L,M	;7
IR3_LP3		.EQU	$ + RB - (IR1_SIZE + IR2_SIZE)
		IN	INPUT_PORT ;10	;Fetch bit      ;REQUIRE 52 CYCLES BETWEEN BITS
		RLC		;4	;Carry = bit
		MOV	A,H	;5
		RAR		;4	
		JC	IR3_LP2	;10	TC=52
		
		LHLD	RXBHEAD	;16
		MOV	M,A	;7 Save Received byte into RX Buffer
		INR	L	;5 Advance Head Ptr of RX Buffer, Head = Head + 1
		SHLD	RXBHEAD	;16

				;Look for consecutive bytes
		IN	INPUT_PORT ;10	;Fetch bit
		RAL		;4
		JNC	IR3_RP1 ;10

		IN	INPUT_PORT ;10	;Fetch bit
		RAL		;4
		JNC	IR3_RP1 ;10

		IN	INPUT_PORT ;10	;Fetch bit
		RAL		;4
		JNC	IR3_RP1 ;10
		
		POP	H	;10
		POP	PSW	;10
		EI		;4
		RET		;10

IR3_SIZE	.EQU	$ - IR3_ROM 


;-----------------------------------------------------------------------------------------------------
				;38,400, 52t = 26uSec
IR4_ROM		.EQU	$
		;RST	038H	;11   5.5uSEC
		;JMP	ISR_RAM	;10	Jump to ISR


		PUSH	PSW	;11 
		PUSH	H	;11

		NOP		;4
		NOP		;4
		NOP		;4
					;REQUIRE 52 CYCLES BETWEEN BITS
		IN	INPUT_PORT ;10	;Fetch bit D0

		LHLD	RXBHEAD	;16

		MOV	H,A	;5	
		MOV	H,A	;5	
		INR	L	;5 Advance Head Ptr of RX Buffer, Head = Head + 1
		IN	INPUT_PORT ;10	;Fetch bit D1     
		IN	INPUT_PORT ;10	;Fetch bit D1     

		RLC		;4	;Carry = bit
		MOV	A,H	;5
		RAR		;4	
		MOV	H,A	;5
		NOP		;4
		IN	INPUT_PORT ;10	;Fetch bit D2     
		IN	INPUT_PORT ;10	;Fetch bit D2     
		IN	INPUT_PORT ;10	;Fetch bit D2

		RLC		;4	;Carry = bit
		MOV	A,H	;5
		RAR		;4	
		MOV	H,A	;5
		MOV	H,A	;5
		MOV	A,L	;5
		STA	RXBHEAD	;13
		IN	INPUT_PORT ;10	;Fetch bit D3

		RLC		;4	;Carry = bit
		MOV	A,H	;5
		RAR		;4	
		MOV	H,A	;5	
		MOV	A,M	;7
		MOV	A,M	;7
		IN	INPUT_PORT ;10	;Fetch bit D4
		IN	INPUT_PORT ;10	;Fetch bit D4


		RLC		;4	;Carry = bit
		MOV	A,H	;5
		RAR		;4	
		MOV	H,A	;5	
		MOV	A,M	;7
		MOV	A,M	;7
		IN	INPUT_PORT ;10	;Fetch bit D5     
		IN	INPUT_PORT ;10	;Fetch bit D5

		RLC		;4	;Carry = bit
		MOV	A,H	;5
		RAR		;4	
		MOV	H,A	;5	
		MOV	A,M	;7
		MOV	A,M	;7
		IN	INPUT_PORT ;10	;Fetch bit D6     
		IN	INPUT_PORT ;10	;Fetch bit D6

		RLC		;4	;Carry = bit
		MOV	A,H	;5
		RAR		;4	
		MOV	H,A	;5	
		MOV	A,M	;7
		DCR	L	;5 
		IN	INPUT_PORT ;10	;Fetch bit D7     
		IN	INPUT_PORT ;10	;Fetch bit D7     

		RLC		;4	;Carry = bit
		MOV	A,H	;5
		RAR		;4	
		
		MVI	H,RXBUFFER >> 8 ;7
		MOV	M,A	;7 Save Received byte into RX Buffer
		
		POP	H	;10
		POP	PSW	;10
		EI		;4
		RET		;10

IR4_SIZE	.EQU	$ - IR4_ROM
		


;----------------------------------------------------------------------------------------------------; RAM SPACE
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;----------------------------------------------------------------------------------------------------; RAM SPACE

;                       ********      ***     **     **                         
;                       *********    *****    ***   ***                         
;                       **     **   *** ***   **** ****                         
;                       **     **  ***   ***  *********                         
;---------------------  ********   *********  ** *** **  ---------------------  
;---------------------  ********   *********  ** *** **  ---------------------  
;                       **  **     **     **  **     **                         
;                       **   **    **     **  **     **                         
;                       **    **   **     **  **     **                         
;                       **     **  **     **  **     **                         






		.ORG	0FD00H
		.DSEG
		;DEFINE RAM AREAS FOR BDOS OPERATION
DIRBUF		.BLOCK	128
ALV0		.BLOCK	31
CSV0		.BLOCK	16

; SPECIAL FLAGS.
CONOTF		.BLOCK  1	;NO-PRINT FLAG (WHEN FF).

CLEAR_RAM	.EQU	$	;Clear all RAM after this point on INIT

LOGICAL_SEC	.BLOCK	2	;Logical Sector for next Read/Write Operation

; BIOS SCRATCH AREA.
;
					;SECT and Trk concatenate to form Logical Sector
SECT:		.BLOCK   1		;CURRENT SECTOR NUMBER (1 Byte!!)
TRK:		.BLOCK   2		;CURRENT TRACK NUMBER.
DMAADD:		.BLOCK   2		;DISK TRANSFER ADDRESS.
DISKNO:		.BLOCK   1		;DISK NUMBER (TO CP/M).


		.ORG	0FE00H
STACK_ISR
RXBUFFER	.BLOCK	256


		.ORG	0FF00H
BITMAP		.BLOCK  8	;Bits 0 to 7
FP_LED_MAT	.BLOCK	7	;FRONT PANEL MATRIX LED OUTPUT (align with low address nibble=0)
				;xxxx LLLL = A11 to A8
				;xxxx LLLL = A15 to A12
				;xxxx LLLL = A3  to A0
				;xxxx LLLL = A7  to A4
				;xxxx LLLL = D3  to D0
				;xxxx LLLL = D7  to D4
				;xxxx LLLL = ALO, AHI, DATA, RUN
				
FP_SW_MAT	.BLOCK	7	;Switch input matrix
				;.... .... = Don't care,not connected
				;.... .... = Don't care,not connected
				;.... .... = Don't care,not connected
				;.... .... = Don't care,not connected
				;.... SSSS = D3 to D0
				;.... SSSS = D7 to D4
				;.... .SSS = NEXT, MODE, RUN
				
FP_MAT_PTR	.BLOCK	2	;Pointer to next LED output
FP_SW_PTR	.BLOCK	1	;Pointer to next Switch to Debounce
FP_SW_DBC	.BLOCK	12	;Switch Debounce Counters (must be in low half of page, Sign bit=0)
FP_SW_FLAG	.BLOCK	1	;Key pressed (0-11 or 80H=No key)
FPM_ADDR	.BLOCK	2	;Pointer to current memory to display
FPM_REPEAT	.BLOCK	1	;Counter to slow repeat function
FPM_MOD_FIELD	.BLOCK	1	;Pointer to Modifiying field (eg 0=None, 1=A-Hi, 2= A-Low, 3= Data)
FPM_FLASH_COD	.BLOCK	1	;Flash code for modify field (eg 0= OFF, 1=2= NULL, 3=ON)
OUTPUT_LAST	.BLOCK	1	;Last byte to output for LED's
GC_OPTIONS	.BLOCK	1	;Options for Get Char
				;Bit 0 - Enable Front Panel Ops during Get_Char
				;Bit 7 - Enable Echo
INT_ENABLED	.BLOCK	1	;Software flag for Interrupt Enable (Allow PUT_CHAR before INT)

RXBHEAD		.BLOCK	2	;Pointer to Head of Buffer (incomming chars)
RXBTAIL		.BLOCK	2	;Poniter to Tail of Buffer.  If Tail=Head then Buffer is empty.

XMSEQ		.BLOCK	1	;XMODEM SEQUENCE NUMBER
XMTYPE		.BLOCK	1	;XMODEM BLOCK TYPE (CRC/CS)
XSECTOR		.BLOCK	2	;Sector of xmodem transfer

SP_TEMP		.BLOCK	2	;Stack Pointer Saved when doing READ/WRITE SECTORS
HL_TEMP		.BLOCK	2	;Also saves HL when setting up new & old Stack in BIOS
SP_BIOS		.BLOCK	2	;Stack Pointer Saved when doing CPM BIOS
SP_ISR		.BLOCK	2	;Stack Pointer Saved when doing TIMER ISR
ISR_LOCK	.BLOCK	1	;Flag to prevent re-entry to ISR
PC_PTR		.BLOCK	2	;Location of Put_Char
BAUD_SET_PTR	.BLOCK	2	;Location of Baud Set Routine
MEM_SOURCE	.BLOCK	1	;ROM/RAM Source for memory dump/edit.  1=RAM, 0=ROM.
ROM_SELECT	.BLOCK	1	;ROM/RAM Selected for code.  1=RAM, 0=ROM.
TIMER_RUN	.BLOCK	1	;TIC Interrupt ON/OFF  (1/0)
TIC_COUNTER	.BLOCK	2	;Timer TIC Counter
FP_PRECOUNT	.BLOCK	1	;PRECOUNTER FOR FP_OPERATE
RAM_SIGNATURE	.BLOCK	2	;Set to a unique value to indicate RAM has been initialized (cold/warm boot)
RAMDISK_CS	.BLOCK	2	;Saved Value of RAM Checksum


	#if ($ >= STACK - 40)	;Reserve 40 bytes for Stack
             !!!OUT-OF-STACK-SPACE!!!
	#endif

		.ORG	0FF80H
STACK		.BLOCK	0
SECTOR_BUFFER	.BLOCK	128
                                                                  
;  ********   **     **  *********  *********  *********  ******** 
;  *********  **     **  *********  *********  *********  *********
;  **     **  **     **  **         **         **         **     **
;  **     **  **     **  **         **         **         **     **
;  ********   **     **  *******    *******    *******    ******** 
;  ********   **     **  *******    *******    *******    ******** 
;  **     **  **     **  **         **         **         **  **   
;  **     **  **     **  **         **         **         **   **  
;  *********  *********  **         **         *********  **    ** 
;  ********    *******   **         **         *********  **     **
;                                                                  



;ROM DISK AREA
;Last Firmware ROM @ 2826h   

ROM_DISK	.ORG	2B00H	;Start 1 page less than a 1K boundary, allows for MONITOR command

;ROM	SECTOR	BLOCK
;2B00	0E00	E0
;2B80	0E01	E0
;2C00	0E02	E0
;2C80	0E03	E0
;2D00	0E04	E0
;2D80	0E05	E0
;2E00	0E06	E0
;2E80	0E07	E0
;2F00	0E08	E0
;2F80	0E09	E0
;3000	0E0A	E0
;3080	0E0B	E0
;3100	0E0C	E0
;3180	0E0D	E0
;3200	0E0E	E0
;3280	0E0F	E0
;3300	0E10	E1
;
;3B00	0E20	E2
;
;4300	0E30	E3
;
;4B00	0E40	E4
;
;5300	0E50	E5
;
;5B00	0E60	E6
;
;6300	0E70	E7
;
;6B00	0E80	E8
;
;7300	0E90	E9
;
;7B00	0EA0	EA
;

											;PIP.COM
											;BLOCK E0
   .DB 0C3h,0CEh,004h,0C9h,000h,000h,0C9h,000h,000h,01Ah,000h,000h,000h,000h,000h,000h  ;0000h
   .DB 028h,049h,04Eh,050h,03Ah,02Fh,04Fh,055h,054h,03Ah,053h,050h,041h,043h,045h,029h  ;0010h
   .DB 028h,049h,04Eh,050h,03Ah,02Fh,04Fh,055h,054h,03Ah,053h,050h,041h,043h,045h,029h  ;0020h
   .DB 028h,049h,04Eh,050h,03Ah,02Fh,04Fh,055h,054h,03Ah,053h,050h,041h,043h,045h,029h  ;0030h
   .DB 028h,049h,04Eh,050h,03Ah,02Fh,04Fh,055h,054h,03Ah,053h,050h,041h,043h,045h,029h  ;0040h
   .DB 028h,049h,04Eh,050h,03Ah,02Fh,04Fh,055h,054h,03Ah,053h,050h,041h,043h,045h,029h  ;0050h
   .DB 028h,049h,04Eh,050h,03Ah,02Fh,04Fh,055h,054h,03Ah,053h,050h,041h,043h,045h,029h  ;0060h
   .DB 028h,049h,04Eh,050h,03Ah,02Fh,04Fh,055h,054h,03Ah,053h,050h,041h,043h,045h,029h  ;0070h
   .DB 028h,049h,04Eh,050h,03Ah,02Fh,04Fh,055h,054h,03Ah,053h,050h,041h,043h,045h,029h  ;0080h
   .DB 028h,049h,04Eh,050h,03Ah,02Fh,04Fh,055h,054h,03Ah,053h,050h,041h,043h,045h,029h  ;0090h
   .DB 028h,049h,04Eh,050h,03Ah,02Fh,04Fh,055h,054h,03Ah,053h,050h,041h,043h,045h,029h  ;00A0h
   .DB 028h,049h,04Eh,050h,03Ah,02Fh,04Fh,055h,054h,03Ah,053h,050h,041h,043h,045h,029h  ;00B0h
   .DB 028h,049h,04Eh,050h,03Ah,02Fh,04Fh,055h,054h,03Ah,053h,050h,041h,043h,045h,029h  ;00C0h
   .DB 028h,049h,04Eh,050h,03Ah,02Fh,04Fh,055h,054h,03Ah,053h,050h,041h,043h,045h,029h  ;00D0h
   .DB 028h,049h,04Eh,050h,03Ah,02Fh,04Fh,055h,054h,03Ah,053h,050h,041h,043h,045h,029h  ;00E0h
   .DB 028h,049h,04Eh,050h,03Ah,02Fh,04Fh,055h,054h,03Ah,053h,050h,041h,043h,045h,029h  ;00F0h
   .DB 020h,020h,020h,043h,04Fh,050h,059h,052h,049h,047h,048h,054h,020h,028h,043h,029h  ;0100h
   .DB 020h,031h,039h,037h,039h,02Ch,020h,044h,049h,047h,049h,054h,041h,04Ch,020h,052h  ;0110h
   .DB 045h,053h,045h,041h,052h,043h,048h,02Ch,020h,020h,050h,049h,050h,020h,056h,045h  ;0120h
   .DB 052h,053h,020h,031h,02Eh,035h,003h,001h,006h,001h,000h,024h,024h,024h,020h,020h  ;0130h
   .DB 020h,020h,020h,053h,055h,042h,000h,000h,000h,020h,03Dh,02Eh,03Ah,02Ch,03Ch,03Eh  ;0140h
   .DB 00Dh,05Fh,05Bh,05Dh,049h,04Eh,050h,049h,052h,044h,050h,054h,052h,055h,052h,031h  ;0150h
   .DB 055h,052h,032h,052h,044h,052h,04Fh,055h,054h,04Ch,050h,054h,055h,04Ch,031h,050h  ;0160h
   .DB 052h,04Eh,04Ch,053h,054h,050h,054h,050h,055h,050h,031h,055h,050h,032h,050h,055h  ;0170h
   .DB 04Eh,054h,054h,059h,043h,052h,054h,055h,043h,031h,043h,04Fh,04Eh,04Eh,055h,04Ch  ;0180h
   .DB 045h,04Fh,046h,000h,044h,049h,053h,04Bh,020h,052h,045h,041h,044h,020h,045h,052h  ;0190h
   .DB 052h,04Fh,052h,024h,044h,049h,053h,04Bh,020h,057h,052h,049h,054h,045h,020h,045h  ;01A0h
   .DB 052h,052h,04Fh,052h,024h,056h,045h,052h,049h,046h,059h,020h,045h,052h,052h,04Fh  ;01B0h
   .DB 052h,024h,04Eh,04Fh,054h,020h,041h,020h,043h,048h,041h,052h,041h,043h,054h,045h  ;01C0h
   .DB 052h,020h,053h,049h,04Eh,04Bh,024h,052h,045h,041h,044h,045h,052h,020h,053h,054h  ;01D0h
   .DB 04Fh,050h,050h,049h,04Eh,047h,00Dh,00Ah,024h,04Eh,04Fh,054h,020h,041h,020h,043h  ;01E0h
   .DB 048h,041h,052h,041h,043h,054h,045h,052h,020h,053h,04Fh,055h,052h,043h,045h,024h  ;01F0h
   .DB 041h,042h,04Fh,052h,054h,045h,044h,024h,042h,041h,044h,020h,050h,041h,052h,041h  ;0200h
   .DB 04Dh,045h,054h,045h,052h,024h,049h,04Eh,056h,041h,04Ch,049h,044h,020h,055h,053h  ;0210h
   .DB 045h,052h,020h,04Eh,055h,04Dh,042h,045h,052h,024h,052h,045h,043h,04Fh,052h,044h  ;0220h
   .DB 020h,054h,04Fh,04Fh,020h,04Ch,04Fh,04Eh,047h,024h,049h,04Eh,056h,041h,04Ch,049h  ;0230h
   .DB 044h,020h,044h,049h,047h,049h,054h,024h,045h,04Eh,044h,020h,04Fh,046h,020h,046h  ;0240h
   .DB 049h,04Ch,045h,02Ch,020h,043h,054h,04Ch,02Dh,05Ah,03Fh,024h,043h,048h,045h,043h  ;0250h
   .DB 04Bh,053h,055h,04Dh,020h,045h,052h,052h,04Fh,052h,024h,043h,04Fh,052h,052h,045h  ;0260h
   .DB 043h,054h,020h,045h,052h,052h,04Fh,052h,02Ch,020h,054h,059h,050h,045h,020h,052h  ;0270h
   .DB 045h,054h,055h,052h,04Eh,020h,04Fh,052h,020h,043h,054h,04Ch,02Dh,05Ah,024h,049h  ;0280h
   .DB 04Eh,056h,041h,04Ch,049h,044h,020h,046h,04Fh,052h,04Dh,041h,054h,024h,048h,045h  ;0290h
   .DB 058h,024h,024h,024h,024h,04Eh,04Fh,020h,044h,049h,052h,045h,043h,054h,04Fh,052h  ;02A0h
   .DB 059h,020h,053h,050h,041h,043h,045h,024h,04Eh,04Fh,020h,046h,049h,04Ch,045h,024h  ;02B0h
   .DB 043h,04Fh,04Dh,024h,053h,054h,041h,052h,054h,020h,04Eh,04Fh,054h,020h,046h,04Fh  ;02C0h
   .DB 055h,04Eh,044h,024h,051h,055h,049h,054h,020h,04Eh,04Fh,054h,020h,046h,04Fh,055h  ;02D0h
   .DB 04Eh,044h,024h,043h,041h,04Eh,04Eh,04Fh,054h,020h,043h,04Ch,04Fh,053h,045h,020h  ;02E0h
   .DB 044h,045h,053h,054h,049h,04Eh,041h,054h,049h,04Fh,04Eh,020h,046h,049h,04Ch,045h  ;02F0h
   .DB 024h,044h,045h,053h,054h,049h,04Eh,041h,054h,049h,04Fh,04Eh,020h,049h,053h,020h  ;0300h
   .DB 052h,02Fh,04Fh,02Ch,020h,044h,045h,04Ch,045h,054h,045h,020h,028h,059h,02Fh,04Eh  ;0310h
   .DB 029h,03Fh,024h,02Ah,02Ah,04Eh,04Fh,054h,020h,044h,045h,04Ch,045h,054h,045h,044h  ;0320h
   .DB 02Ah,02Ah,024h,024h,024h,024h,024h,024h,024h,04Eh,04Fh,054h,020h,046h,04Fh,055h  ;0330h
   .DB 04Eh,044h,024h,043h,04Fh,050h,059h,049h,04Eh,047h,020h,02Dh,024h,052h,045h,051h  ;0340h
   .DB 055h,049h,052h,045h,053h,020h,043h,050h,02Fh,04Dh,020h,032h,02Eh,030h,020h,04Fh  ;0350h
   .DB 052h,020h,04Eh,045h,057h,045h,052h,020h,046h,04Fh,052h,020h,04Fh,050h,045h,052h  ;0360h
   .DB 041h,054h,049h,04Fh,04Eh,02Eh,024h,055h,04Eh,052h,045h,043h,04Fh,047h,04Eh,049h  ;0370h
   .DB 05Ah,045h,044h,020h,044h,045h,053h,054h,049h,04Eh,041h,054h,049h,04Fh,04Eh,024h  ;0380h
   .DB 043h,041h,04Eh,04Eh,04Fh,054h,020h,057h,052h,049h,054h,045h,024h,049h,04Eh,056h  ;0390h
   .DB 041h,04Ch,049h,044h,020h,050h,049h,050h,020h,046h,04Fh,052h,04Dh,041h,054h,024h  ;03A0h
   .DB 043h,041h,04Eh,04Eh,04Fh,054h,020h,052h,045h,041h,044h,024h,049h,04Eh,056h,041h  ;03B0h
   .DB 04Ch,049h,044h,020h,053h,045h,050h,041h,052h,041h,054h,04Fh,052h,024h,031h,0F2h  ;03C0h
   .DB 01Dh,001h,080h,000h,0C5h,01Eh,080h,001h,0CCh,01Eh,0CDh,018h,00Ah,03Ah,0CCh,01Eh  ;03D0h
   .DB 0D6h,000h,0D6h,001h,09Fh,032h,0A5h,01Eh,0CDh,04Ch,008h,0EBh,03Eh,020h,0CDh,084h  ;03E0h
   .DB 01Dh,0D2h,0FDh,004h,001h,04Dh,004h,0CDh,039h,008h,0CDh,000h,000h,0CDh,016h,009h  ;03F0h
   .DB 032h,0C0h,01Eh,011h,000h,000h,00Eh,019h,0CDh,005h,000h,032h,0FCh,01Dh,031h,0F2h  ;0400h
   .DB 01Dh,0CDh,040h,01Ah,03Ah,0C0h,01Eh,032h,0C1h,01Eh,021h,06Fh,01Fh,036h,000h,02Bh  ;0410h
   .DB 036h,000h,02Bh,036h,000h,021h,0A6h,01Eh,036h,001h,023h,036h,000h,021h,0F3h,01Dh  ;0420h
   .DB 036h,000h,023h,036h,0FEh,03Ah,0A5h,01Eh,01Fh,0D2h,047h,005h,00Eh,02Ah,0CDh,01Ch  ;0430h
   .DB 008h,0CDh,06Fh,009h,0CDh,02Eh,008h,021h,04Eh,01Fh,036h,0FFh,03Ah,0CCh,01Eh,0FEh  ;0440h
   .DB 000h,0C2h,05Eh,005h,02Ah,0FCh,01Dh,04Dh,0CDh,05Eh,008h,0CDh,000h,000h,021h,04Bh  ;0450h
   .DB 01Eh,036h,000h,021h,003h,01Eh,036h,000h,021h,0A4h,01Eh,036h,000h,02Bh,036h,000h  ;0460h
   .DB 001h,027h,01Eh,0CDh,020h,012h,03Ah,0A9h,01Eh,0FEh,003h,0C2h,081h,005h,0C3h,024h  ;0470h
   .DB 006h,03Ah,0A9h,01Eh,0FEh,004h,0C2h,0C3h,005h,03Ah,094h,01Fh,03Dh,032h,04Bh,01Eh  ;0480h
   .DB 0CDh,00Ch,01Dh,001h,006h,01Eh,0CDh,020h,012h,03Ah,0A9h,01Eh,0FEh,002h,0CAh,0A4h  ;0490h
   .DB 005h,0CDh,05Ch,018h,03Ah,0F5h,01Dh,01Fh,0D2h,0B7h,005h,001h,05Ch,000h,0CDh,0EEh  ;04A0h
   .DB 01Ch,0CDh,078h,01Bh,0C3h,0C0h,005h,001h,027h,01Eh,0CDh,0EEh,01Ch,0CDh,0B2h,01Ah  ;04B0h
   .DB 0C3h,0DBh,007h,03Ah,0A9h,01Eh,0D6h,002h,0C6h,0FFh,09Fh,021h,0F5h,01Dh,0B6h,01Fh  ;04C0h
   .DB 0D2h,0D6h,005h,0CDh,05Ch,018h,0CDh,0A2h,01Ch,0CDh,00Ch,01Dh,001h,006h,01Eh,0CDh  ;04D0h
   .DB 020h,012h,03Ah,0A9h,01Eh,0FEh,004h,0C2h,005h,006h,0CDh,088h,01Ch,0CDh,0C6h,01Ch  ;04E0h
   .DB 001h,027h,01Eh,0C5h,01Eh,021h,001h,006h,01Eh,0CDh,018h,00Ah,0CDh,0DFh,01Ch,0CDh  ;04F0h
   .DB 0B2h,01Ah,0C3h,0DBh,007h,03Ah,0A9h,01Eh,0FEh,002h,0C2h,024h,006h,0CDh,011h,012h  ;0500h
   .DB 03Ah,0A8h,01Eh,0FEh,00Dh,0CAh,01Bh,006h,0C3h,024h,006h,0CDh,088h,01Ch,0CDh,0B2h  ;0510h
   .DB 01Ah,0C3h,0DBh,007h,021h,04Eh,01Fh,036h,0FFh,001h,027h,01Eh,0CDh,020h,012h,03Ah  ;0520h
   .DB 0A9h,01Eh,0D6h,002h,09Fh,021h,0F5h,01Dh,0B6h,01Fh,0D2h,043h,006h,001h,077h,004h  ;0530h
   .DB 0CDh,0AFh,009h,021h,005h,01Eh,036h,000h,03Ah,0A9h,01Eh,0FEh,002h,0C2h,05Eh,006h  ;0540h
   .DB 0CDh,0A2h,01Ch,0CDh,063h,018h,021h,0A8h,01Eh,036h,0FFh,0C3h,07Bh,006h,03Ah,0A8h  ;0550h
   .DB 01Eh,0D6h,013h,09Fh,02Fh,0F5h,03Eh,005h,021h,0A8h,01Eh,096h,09Fh,02Fh,0C1h,048h  ;0560h
   .DB 0B1h,01Fh,0D2h,07Bh,006h,001h,090h,004h,0CDh,0AFh,009h,03Ah,0A8h,01Eh,03Ch,032h  ;0570h
   .DB 0A3h,01Eh,0FEh,00Fh,0C2h,08Ah,006h,0CDh,0CFh,015h,001h,006h,01Eh,0CDh,020h,012h  ;0580h
   .DB 03Ah,0A9h,01Eh,0D6h,001h,0C6h,0FFh,09Fh,0F5h,03Ah,0A8h,01Eh,0D6h,03Dh,0C6h,0FFh  ;0590h
   .DB 09Fh,0C1h,048h,0B1h,01Fh,0D2h,0AEh,006h,001h,09Dh,004h,0CDh,0AFh,009h,021h,0A7h  ;05A0h
   .DB 01Fh,036h,001h,03Ah,0A7h,01Fh,01Fh,0D2h,0BEh,007h,03Ah,0C0h,01Eh,032h,0C1h,01Eh  ;05B0h
   .DB 001h,006h,01Eh,0CDh,020h,012h,021h,004h,01Eh,036h,000h,03Ah,0A9h,01Eh,0D6h,002h  ;05C0h
   .DB 0D6h,001h,09Fh,0F5h,03Ah,0F5h,01Dh,02Fh,0C1h,048h,0A1h,01Fh,0D2h,0EDh,006h,0CDh  ;05D0h
   .DB 088h,01Ch,0CDh,0BEh,018h,021h,0A8h,01Eh,036h,0FFh,0C3h,013h,007h,03Ah,0A9h,01Eh  ;05E0h
   .DB 0D6h,003h,0C6h,0FFh,09Fh,0F5h,03Eh,00Ah,021h,0A8h,01Eh,096h,09Fh,02Fh,0F5h,03Eh  ;05F0h
   .DB 005h,096h,09Fh,0C1h,048h,0A1h,0C1h,048h,0B1h,01Fh,0D2h,013h,007h,001h,0B0h,004h  ;0600h
   .DB 0CDh,0AFh,009h,03Ah,05Eh,01Fh,021h,004h,01Eh,0B6h,077h,03Ah,0A8h,01Eh,03Ch,032h  ;0610h
   .DB 0A4h,01Eh,03Ah,0A8h,01Eh,0FEh,013h,0C2h,030h,007h,0CDh,0CFh,015h,0C3h,07Dh,007h  ;0620h
   .DB 03Ah,0A8h,01Eh,0FEh,014h,0C2h,040h,007h,00Eh,01Ah,0CDh,045h,00Eh,0C3h,07Dh,007h  ;0630h
   .DB 03Ah,0A8h,01Eh,0D6h,005h,09Fh,021h,005h,01Eh,0A6h,01Fh,0D2h,053h,007h,021h,057h  ;0640h
   .DB 01Fh,036h,001h,03Ah,0A3h,01Eh,0FEh,00Ah,0C2h,07Ah,007h,021h,05Dh,01Fh,036h,001h  ;0650h
   .DB 03Ah,063h,01Fh,0FEh,000h,0C2h,06Dh,007h,021h,063h,01Fh,036h,008h,03Ah,05Fh,01Fh  ;0660h
   .DB 0FEh,000h,0C2h,07Ah,007h,021h,05Fh,01Fh,036h,001h,0CDh,06Ch,01Ah,0CDh,012h,019h  ;0670h
   .DB 001h,006h,01Eh,0CDh,020h,012h,03Ah,0A9h,01Eh,0D6h,001h,0C6h,0FFh,09Fh,0F5h,03Ah  ;0680h
   .DB 0A8h,01Eh,0D6h,02Ch,0C6h,0FFh,09Fh,0F5h,03Ah,0A8h,01Eh,0D6h,00Dh,0C6h,0FFh,09Fh  ;0690h
   .DB 0C1h,048h,0A1h,0C1h,048h,0B1h,01Fh,0D2h,0B0h,007h,001h,0BCh,004h,0CDh,0AFh,009h  ;06A0h
   .DB 03Ah,0A8h,01Eh,0D6h,00Dh,0C6h,0FFh,09Fh,032h,0A7h,01Fh,0C3h,0B3h,006h,03Ah,0A3h  ;06B0h
   .DB 01Eh,0FEh,00Fh,0C2h,0CEh,007h,00Eh,01Ah,0CDh,045h,00Eh,0CDh,0CFh,015h,03Ah,0A3h  ;06C0h
   .DB 01Eh,0FEh,000h,0C2h,0DBh,007h,00Eh,000h,0CDh,031h,019h,03Ah,0A5h,01Eh,032h,0CCh  ;06D0h
   .DB 01Eh,0C3h,014h,005h,0FBh,076h,021h,0F2h,01Dh,071h,021h,0F2h,007h,0E5h,02Ah,038h  ;06E0h
   .DB 002h,0E9h,0C9h,021h,0FBh,007h,0E5h,02Ah,036h,002h,0E9h,03Ah,009h,001h,0C9h,03Eh  ;06F0h
   .DB 0FAh,0CDh,0A6h,01Dh,03Eh,0FAh,0CDh,0A6h,01Dh,0C9h,011h,000h,000h,00Eh,003h,0CDh  ;0700h
   .DB 005h,000h,0C9h,011h,000h,000h,00Eh,001h,0CDh,005h,000h,0C9h,021h,0ABh,01Eh,071h  ;0710h
   .DB 03Ah,0ABh,01Eh,0E6h,07Fh,05Fh,016h,000h,00Eh,002h,0CDh,005h,000h,0C9h,00Eh,00Dh  ;0720h
   .DB 0CDh,01Ch,008h,00Eh,00Ah,0CDh,01Ch,008h,0C9h,021h,0ADh,01Eh,070h,02Bh,071h,0CDh  ;0730h
   .DB 02Eh,008h,02Ah,0ACh,01Eh,0EBh,00Eh,009h,0CDh,005h,000h,0C9h,011h,000h,000h,00Eh  ;0740h
   .DB 00Ch,0CDh,005h,000h,0C9h,011h,000h,000h,00Eh,00Dh,0CDh,005h,000h,0C9h,021h,0AFh  ;0750h
   .DB 01Eh,071h,02Ah,0AFh,01Eh,026h,000h,0EBh,00Eh,00Eh,0CDh,005h,000h,0C9h,021h,0B1h  ;0760h
   .DB 01Eh,070h,02Bh,071h,02Ah,0B0h,01Eh,0EBh,00Eh,00Fh,0CDh,005h,000h,032h,0AEh,01Eh  ;0770h
   .DB 0C9h,021h,0B3h,01Eh,070h,02Bh,071h,02Ah,0B2h,01Eh,0EBh,00Eh,010h,0CDh,005h,000h  ;0780h
   .DB 032h,0AEh,01Eh,0C9h,021h,0B5h,01Eh,070h,02Bh,071h,02Ah,0B4h,01Eh,0EBh,00Eh,011h  ;0790h
   .DB 0CDh,005h,000h,032h,0AEh,01Eh,0C9h,011h,000h,000h,00Eh,012h,0CDh,005h,000h,032h  ;07A0h
   .DB 0AEh,01Eh,0C9h,021h,0B7h,01Eh,070h,02Bh,071h,02Ah,0B6h,01Eh,0EBh,00Eh,013h,0CDh  ;07B0h
   .DB 005h,000h,0C9h,021h,0B9h,01Eh,070h,02Bh,071h,02Ah,0B8h,01Eh,0EBh,00Eh,014h,0CDh  ;07C0h
   .DB 005h,000h,0C9h,021h,0BBh,01Eh,070h,02Bh,071h,02Ah,0BAh,01Eh,0EBh,00Eh,015h,0CDh  ;07D0h
   .DB 005h,000h,0C9h,021h,0BDh,01Eh,070h,02Bh,071h,02Ah,0BCh,01Eh,0EBh,00Eh,016h,0CDh  ;07E0h
   .DB 005h,000h,032h,0AEh,01Eh,0C9h,021h,0BFh,01Eh,070h,02Bh,071h,02Ah,0BEh,01Eh,0EBh  ;07F0h
											;BLOCK E1
   .DB 00Eh,017h,0CDh,005h,000h,0C9h,021h,0C3h,01Eh,070h,02Bh,071h,02Ah,0C2h,01Eh,0EBh  ;0800h
   .DB 00Eh,01Eh,0CDh,005h,000h,0C9h,011h,0FFh,000h,00Eh,020h,0CDh,005h,000h,0C9h,021h  ;0810h
   .DB 0C4h,01Eh,071h,02Ah,0C4h,01Eh,026h,000h,0EBh,00Eh,020h,0CDh,005h,000h,0C9h,02Ah  ;0820h
   .DB 0C0h,01Eh,04Dh,0CDh,01Fh,009h,0C9h,02Ah,0C1h,01Eh,04Dh,0CDh,01Fh,009h,0C9h,021h  ;0830h
   .DB 0C6h,01Eh,070h,02Bh,071h,02Ah,0C5h,01Eh,0EBh,00Eh,021h,0CDh,005h,000h,0C9h,021h  ;0840h
   .DB 0C8h,01Eh,070h,02Bh,071h,02Ah,0C7h,01Eh,0EBh,00Eh,022h,0CDh,005h,000h,0C9h,021h  ;0850h
   .DB 0CAh,01Eh,070h,02Bh,071h,02Ah,0C9h,01Eh,0EBh,00Eh,024h,0CDh,005h,000h,0C9h,021h  ;0860h
   .DB 0CBh,01Eh,036h,080h,011h,0CBh,01Eh,00Eh,00Ah,0CDh,005h,000h,0C9h,011h,000h,000h  ;0870h
   .DB 00Eh,00Bh,0CDh,005h,000h,0C9h,021h,06Bh,01Fh,070h,02Bh,071h,02Ah,06Ah,01Fh,0EBh  ;0880h
   .DB 00Eh,01Ah,0CDh,005h,000h,0C9h,03Eh,00Ch,0D3h,001h,03Eh,008h,0D3h,001h,0DBh,001h  ;0890h
   .DB 007h,007h,007h,01Fh,0DAh,0AAh,009h,0C3h,09Eh,009h,0DBh,003h,0E6h,07Fh,0C9h,021h  ;08A0h
   .DB 071h,01Fh,070h,02Bh,071h,0CDh,02Fh,009h,02Ah,070h,01Fh,044h,04Dh,0CDh,039h,008h  ;08B0h
   .DB 00Eh,03Ah,0CDh,01Ch,008h,00Eh,020h,0CDh,01Ch,008h,03Ah,04Dh,01Fh,032h,072h,01Fh  ;08C0h
   .DB 03Ah,04Eh,01Fh,021h,072h,01Fh,0BEh,0DAh,0F8h,009h,021h,0CCh,01Eh,03Ah,072h,01Fh  ;08D0h
   .DB 0BEh,0D2h,0F1h,009h,02Ah,072h,01Fh,026h,000h,001h,0CDh,01Eh,009h,04Eh,0CDh,01Ch  ;08E0h
   .DB 008h,021h,072h,01Fh,034h,0C2h,0D0h,009h,021h,0CCh,01Eh,036h,000h,001h,03Ah,002h  ;08F0h
   .DB 0CDh,094h,008h,03Ah,0AEh,01Eh,0FEh,0FFh,0CAh,011h,00Ah,001h,03Ah,002h,0CDh,0B3h  ;0900h
   .DB 008h,0CDh,02Eh,008h,0C3h,00Eh,005h,0C9h,021h,077h,01Fh,073h,02Bh,070h,02Bh,071h  ;0910h
   .DB 02Bh,0D1h,0C1h,070h,02Bh,071h,0D5h,03Ah,077h,01Fh,03Dh,032h,077h,01Fh,0FEh,0FFh  ;0920h
   .DB 0CAh,04Eh,00Ah,02Ah,073h,01Fh,0E5h,02Ah,075h,01Fh,0C1h,00Ah,077h,02Ah,073h,01Fh  ;0930h
   .DB 023h,022h,073h,01Fh,02Ah,075h,01Fh,023h,022h,075h,01Fh,0C3h,027h,00Ah,0C9h,021h  ;0940h
   .DB 000h,000h,022h,09Dh,01Eh,02Ah,003h,01Eh,04Dh,0CDh,05Eh,008h,0CDh,037h,009h,021h  ;0950h
   .DB 078h,01Fh,036h,000h,03Ah,0FBh,01Dh,021h,078h,01Fh,0BEh,0DAh,0BEh,00Ah,02Ah,09Dh  ;0960h
   .DB 01Eh,0EBh,02Ah,001h,01Eh,019h,044h,04Dh,0CDh,086h,009h,001h,006h,01Eh,0CDh,0C3h  ;0970h
   .DB 008h,032h,079h,01Fh,0FEh,000h,0CAh,0ADh,00Ah,03Ah,079h,01Fh,0FEh,001h,0CAh,097h  ;0980h
   .DB 00Ah,001h,094h,002h,0CDh,0AFh,009h,02Ah,09Dh,01Eh,022h,09Fh,01Eh,0EBh,02Ah,001h  ;0990h
   .DB 01Eh,019h,036h,01Ah,03Ah,0FBh,01Dh,032h,078h,01Fh,0C3h,0B7h,00Ah,011h,080h,000h  ;09A0h
   .DB 02Ah,09Dh,01Eh,019h,022h,09Dh,01Eh,021h,078h,01Fh,034h,0C2h,064h,00Ah,021h,000h  ;09B0h
   .DB 000h,022h,09Dh,01Eh,0CDh,02Fh,009h,0C9h,00Eh,007h,021h,0A1h,01Eh,0CDh,06Ah,01Dh  ;09C0h
   .DB 07Dh,03Dh,032h,07Ch,01Fh,0FEh,0FFh,0C2h,0DBh,00Ah,0C9h,021h,000h,000h,022h,0A1h  ;09D0h
   .DB 01Eh,02Ah,04Bh,01Eh,04Dh,0CDh,05Eh,008h,001h,027h,01Eh,0CDh,05Fh,009h,021h,07Ah  ;09E0h
   .DB 01Fh,036h,000h,03Ah,07Ch,01Fh,021h,07Ah,01Fh,0BEh,0DAh,031h,00Bh,02Ah,0A1h,01Eh  ;09F0h
   .DB 001h,0CAh,01Fh,009h,022h,07Dh,01Fh,02Ah,07Dh,01Fh,044h,04Dh,0CDh,086h,009h,001h  ;0A00h
   .DB 027h,01Eh,0CDh,0D3h,008h,0FEh,000h,0CAh,020h,00Bh,001h,0A4h,002h,0CDh,0AFh,009h  ;0A10h
   .DB 011h,080h,000h,02Ah,0A1h,01Eh,019h,022h,0A1h,01Eh,021h,07Ah,01Fh,034h,0C2h,0F3h  ;0A20h
   .DB 00Ah,03Ah,065h,01Fh,01Fh,0D2h,0C9h,00Bh,021h,000h,000h,022h,0A1h,01Eh,001h,080h  ;0A30h
   .DB 000h,0CDh,086h,009h,021h,07Ah,01Fh,036h,000h,03Ah,07Ch,01Fh,021h,07Ah,01Fh,0BEh  ;0A40h
   .DB 0DAh,0C0h,00Bh,001h,027h,01Eh,0CDh,03Fh,009h,0D6h,000h,0D6h,001h,09Fh,032h,07Fh  ;0A50h
   .DB 01Fh,02Ah,048h,01Eh,023h,022h,048h,01Eh,021h,07Bh,01Fh,036h,000h,03Ah,07Bh,01Fh  ;0A60h
   .DB 0D6h,080h,09Fh,021h,07Fh,01Fh,0A6h,01Fh,0D2h,0A2h,00Bh,02Ah,07Bh,01Fh,026h,000h  ;0A70h
   .DB 001h,080h,000h,009h,03Ah,07Bh,01Fh,011h,0A1h,01Eh,0E5h,0CDh,034h,01Dh,001h,0CAh  ;0A80h
   .DB 01Fh,009h,0C1h,00Ah,096h,0D6h,001h,09Fh,032h,07Fh,01Fh,021h,07Bh,01Fh,034h,0C3h  ;0A90h
   .DB 06Dh,00Bh,011h,080h,000h,02Ah,0A1h,01Eh,019h,022h,0A1h,01Eh,03Ah,07Fh,01Fh,01Fh  ;0AA0h
   .DB 0DAh,0B9h,00Bh,001h,0B5h,002h,0CDh,0AFh,009h,021h,07Ah,01Fh,034h,0C2h,049h,00Bh  ;0AB0h
   .DB 001h,027h,01Eh,0CDh,0D3h,008h,032h,07Fh,01Fh,021h,000h,000h,022h,0A1h,01Eh,0C9h  ;0AC0h
   .DB 021h,080h,01Fh,071h,03Ah,080h,01Fh,0FEh,020h,0DAh,0F4h,00Bh,021h,0F3h,01Dh,034h  ;0AD0h
   .DB 03Eh,000h,021h,053h,01Fh,0BEh,0D2h,0F4h,00Bh,03Ah,053h,01Fh,021h,0F3h,01Dh,0BEh  ;0AE0h
   .DB 0D2h,0F4h,00Bh,0C9h,03Ah,003h,000h,032h,081h,01Fh,02Ah,0A3h,01Eh,04Dh,006h,000h  ;0AF0h
   .DB 021h,0DDh,00Ch,009h,009h,05Eh,023h,056h,0EBh,0E9h,001h,0FFh,01Dh,011h,0A1h,01Eh  ;0B00h
   .DB 0CDh,08Eh,01Dh,0DAh,019h,00Ch,0CDh,0C8h,00Ah,02Ah,0A1h,01Eh,001h,0CAh,01Fh,009h  ;0B10h
   .DB 03Ah,080h,01Fh,077h,02Ah,0A1h,01Eh,023h,022h,0A1h,01Eh,0C3h,005h,00Dh,0C3h,03Dh  ;0B20h
   .DB 00Ch,0C3h,03Dh,00Ch,0C3h,03Dh,00Ch,0C3h,03Dh,00Ch,0C3h,03Dh,00Ch,001h,0C2h,002h  ;0B30h
   .DB 0CDh,0AFh,009h,0C3h,005h,00Dh,02Ah,080h,01Fh,04Dh,0CDh,0E6h,007h,0C3h,005h,00Dh  ;0B40h
   .DB 021h,003h,000h,036h,080h,0C3h,071h,00Ch,0C3h,005h,00Dh,021h,003h,000h,036h,0C0h  ;0B50h
   .DB 0C3h,071h,00Ch,0C3h,005h,00Dh,021h,003h,000h,036h,080h,0C3h,071h,00Ch,0C3h,005h  ;0B60h
   .DB 00Dh,02Ah,080h,01Fh,026h,000h,0EBh,00Eh,005h,0CDh,005h,000h,0C3h,005h,00Dh,021h  ;0B70h
   .DB 003h,000h,036h,010h,0C3h,0A0h,00Ch,0C3h,005h,00Dh,021h,003h,000h,036h,020h,0C3h  ;0B80h
   .DB 0A0h,00Ch,0C3h,005h,00Dh,021h,003h,000h,036h,030h,0C3h,0A0h,00Ch,0C3h,005h,00Dh  ;0B90h
   .DB 02Ah,080h,01Fh,026h,000h,0EBh,00Eh,004h,0CDh,005h,000h,0C3h,005h,00Dh,021h,003h  ;0BA0h
   .DB 000h,036h,000h,0C3h,0CFh,00Ch,0C3h,005h,00Dh,021h,003h,000h,036h,001h,0C3h,0CFh  ;0BB0h
   .DB 00Ch,0C3h,005h,00Dh,021h,003h,000h,036h,003h,0C3h,0CFh,00Ch,0C3h,005h,00Dh,02Ah  ;0BC0h
   .DB 080h,01Fh,026h,000h,0EBh,00Eh,002h,0CDh,005h,000h,0C3h,005h,00Dh,00Ah,00Ch,02Eh  ;0BD0h
   .DB 00Ch,031h,00Ch,034h,00Ch,037h,00Ch,03Ah,00Ch,03Dh,00Ch,046h,00Ch,050h,00Ch,05Bh  ;0BE0h
   .DB 00Ch,066h,00Ch,071h,00Ch,07Fh,00Ch,08Ah,00Ch,095h,00Ch,0A0h,00Ch,0AEh,00Ch,0B9h  ;0BF0h
   .DB 00Ch,0C4h,00Ch,0CFh,00Ch,03Ah,081h,01Fh,032h,003h,000h,0C9h,021h,082h,01Fh,071h  ;0C00h
   .DB 03Ah,082h,01Fh,0FEh,009h,0CAh,022h,00Dh,02Ah,082h,01Fh,04Dh,0CDh,0D0h,00Bh,0C3h  ;0C10h
   .DB 06Eh,00Dh,03Ah,063h,01Fh,0FEh,000h,0C2h,034h,00Dh,02Ah,082h,01Fh,04Dh,0CDh,0D0h  ;0C20h
   .DB 00Bh,0C3h,06Eh,00Dh,03Ah,0F3h,01Dh,032h,083h,01Fh,021h,063h,01Fh,03Ah,083h,01Fh  ;0C30h
   .DB 0BEh,0DAh,051h,00Dh,021h,063h,01Fh,03Ah,083h,01Fh,096h,032h,083h,01Fh,0C3h,03Ah  ;0C40h
   .DB 00Dh,021h,083h,01Fh,03Ah,063h,01Fh,096h,077h,03Eh,000h,021h,083h,01Fh,0BEh,0D2h  ;0C50h
   .DB 06Eh,00Dh,021h,083h,01Fh,035h,00Eh,020h,0CDh,0D0h,00Bh,0C3h,059h,00Dh,03Ah,082h  ;0C60h
   .DB 01Fh,0FEh,00Dh,0C2h,07Bh,00Dh,021h,0F3h,01Dh,036h,000h,0C9h,021h,084h,01Fh,071h  ;0C70h
   .DB 03Ah,084h,01Fh,0D6h,000h,0D6h,001h,09Fh,021h,06Ch,01Fh,0A6h,077h,01Fh,0D2h,099h  ;0C80h
   .DB 00Dh,00Eh,020h,0CDh,00Ch,00Dh,0C3h,0A2h,00Dh,03Ah,084h,01Fh,0C6h,030h,04Fh,0CDh  ;0C90h
   .DB 00Ch,00Dh,0C9h,021h,085h,01Fh,071h,03Ah,085h,01Fh,0E6h,0F8h,01Fh,01Fh,01Fh,01Fh  ;0CA0h
   .DB 04Fh,0CDh,07Ch,00Dh,03Ah,085h,01Fh,0E6h,00Fh,04Fh,0CDh,07Ch,00Dh,0C9h,021h,086h  ;0CB0h
   .DB 01Fh,036h,001h,03Ah,05Dh,01Fh,0D6h,001h,0D6h,001h,09Fh,032h,06Ch,01Fh,03Ah,06Fh  ;0CC0h
   .DB 01Fh,086h,027h,032h,06Fh,01Fh,03Ah,06Eh,01Fh,0CEh,000h,027h,032h,06Eh,01Fh,03Ah  ;0CD0h
   .DB 06Dh,01Fh,0CEh,000h,027h,032h,06Dh,01Fh,02Ah,06Dh,01Fh,04Dh,0CDh,0A3h,00Dh,02Ah  ;0CE0h
   .DB 06Eh,01Fh,04Dh,0CDh,0A3h,00Dh,02Ah,06Fh,01Fh,04Dh,0CDh,0A3h,00Dh,03Ah,05Dh,01Fh  ;0CF0h
   .DB 0FEh,001h,0C2h,012h,00Eh,00Eh,03Ah,0CDh,00Ch,00Dh,00Eh,020h,0CDh,00Ch,00Dh,0C3h  ;0D00h
   .DB 017h,00Eh,00Eh,009h,0CDh,00Ch,00Dh,0C9h,02Ah,0A1h,01Eh,07Dh,0E6h,07Fh,032h,089h  ;0D10h
   .DB 01Fh,011h,080h,0FFh,0CDh,044h,01Dh,022h,087h,01Fh,0CDh,0C8h,00Ah,02Ah,087h,01Fh  ;0D20h
   .DB 001h,0CAh,01Fh,009h,0E5h,02Ah,089h,01Fh,0EBh,0CDh,018h,00Ah,02Ah,089h,01Fh,026h  ;0D30h
   .DB 000h,022h,0A1h,01Eh,0C9h,021h,08Ah,01Fh,071h,03Ah,055h,01Fh,01Fh,0D2h,059h,00Eh  ;0D40h
   .DB 03Ah,08Ah,01Fh,0FEh,00Ch,0C2h,059h,00Eh,0C9h,03Ah,0A6h,01Eh,01Fh,0D2h,0A9h,00Eh  ;0D50h
   .DB 03Ah,08Ah,01Fh,0FEh,00Ch,0CAh,0A9h,00Eh,03Ah,05Fh,01Fh,032h,08Bh,01Fh,0FEh,000h  ;0D60h
   .DB 0CAh,098h,00Eh,03Ah,08Bh,01Fh,0FEh,001h,0C2h,080h,00Eh,021h,08Bh,01Fh,036h,03Ch  ;0D70h
   .DB 03Ah,0F4h,01Dh,03Ch,032h,0F4h,01Dh,021h,08Bh,01Fh,0BEh,0DAh,098h,00Eh,021h,0F4h  ;0D80h
   .DB 01Dh,036h,000h,00Eh,00Ch,0CDh,00Ch,00Dh,03Eh,000h,021h,05Dh,01Fh,0BEh,0D2h,0A4h  ;0D90h
   .DB 00Eh,0CDh,0BEh,00Dh,021h,0A6h,01Eh,036h,000h,03Ah,051h,01Fh,01Fh,0D2h,0CCh,00Eh  ;0DA0h
   .DB 03Ah,08Ah,01Fh,0D6h,013h,0D6h,001h,09Fh,0F5h,03Ah,0A3h,01Eh,0D6h,000h,0D6h,001h  ;0DB0h
   .DB 09Fh,0C1h,048h,0A1h,01Fh,0D2h,0CCh,00Eh,0CDh,018h,00Eh,0C9h,03Ah,08Ah,01Fh,0FEh  ;0DC0h
   .DB 00Ch,0C2h,0D9h,00Eh,021h,0F4h,01Dh,036h,000h,02Ah,08Ah,01Fh,04Dh,0CDh,00Ch,00Dh  ;0DD0h
   .DB 03Ah,08Ah,01Fh,0FEh,00Ah,0C2h,0EDh,00Eh,021h,0A6h,01Eh,036h,001h,0C9h,021h,08Ch  ;0DE0h
   .DB 01Fh,071h,03Ah,08Ch,01Fh,0D6h,061h,09Fh,02Fh,0F5h,03Eh,07Ah,021h,08Ch,01Fh,096h  ;0DF0h
   .DB 09Fh,02Fh,0C1h,048h,0A1h,01Fh,0D2h,011h,00Fh,03Ah,08Ch,01Fh,0E6h,05Fh,032h,08Ch  ;0E00h
   .DB 01Fh,03Ah,08Ch,01Fh,0C9h,021h,08Dh,01Fh,071h,03Ah,08Dh,01Fh,0D6h,041h,09Fh,02Fh  ;0E10h
   .DB 0F5h,03Eh,05Ah,021h,08Dh,01Fh,096h,09Fh,02Fh,0C1h,048h,0A1h,01Fh,0D2h,038h,00Fh  ;0E20h
   .DB 03Ah,08Dh,01Fh,0F6h,020h,032h,08Dh,01Fh,03Ah,08Dh,01Fh,0C9h,03Ah,0A4h,01Eh,03Dh  ;0E30h
   .DB 04Fh,03Eh,005h,0B9h,0DAh,06Dh,00Fh,03Ah,057h,01Fh,021h,051h,01Fh,0B6h,0F5h,0CDh  ;0E40h
   .DB 07Dh,009h,0C1h,048h,0A1h,01Fh,0D2h,06Dh,00Fh,0CDh,013h,008h,0FEh,01Ah,0C2h,064h  ;0E50h
   .DB 00Fh,03Eh,01Ah,0C9h,001h,0D7h,002h,0CDh,039h,008h,03Eh,013h,0C9h,021h,090h,01Fh  ;0E60h
   .DB 036h,001h,03Ah,003h,000h,032h,08Eh,01Fh,02Ah,0A4h,01Eh,04Dh,006h,000h,021h,045h  ;0E70h
   .DB 010h,009h,009h,05Eh,023h,056h,0EBh,0E9h,001h,0FDh,01Dh,011h,09Dh,01Eh,0CDh,08Eh  ;0E80h
   .DB 01Dh,0DAh,097h,00Fh,0CDh,04Fh,00Ah,02Ah,09Dh,01Eh,0EBh,02Ah,001h,01Eh,019h,07Eh  ;0E90h
   .DB 032h,08Fh,01Fh,02Ah,09Dh,01Eh,023h,022h,09Dh,01Eh,0C3h,06Dh,010h,0CDh,0F3h,007h  ;0EA0h
   .DB 032h,08Fh,01Fh,0C3h,06Dh,010h,0CDh,096h,009h,032h,08Fh,01Fh,0C3h,06Dh,010h,021h  ;0EB0h
   .DB 003h,000h,036h,004h,0C3h,0E0h,00Fh,0C3h,06Dh,010h,021h,003h,000h,036h,008h,0C3h  ;0EC0h
   .DB 0E0h,00Fh,0C3h,06Dh,010h,021h,003h,000h,036h,00Ch,0C3h,0E0h,00Fh,0C3h,06Dh,010h  ;0ED0h
   .DB 011h,000h,000h,00Eh,003h,0CDh,005h,000h,0E6h,07Fh,032h,08Fh,01Fh,0C3h,06Dh,010h  ;0EE0h
   .DB 0C3h,008h,010h,0C3h,008h,010h,0C3h,008h,010h,0C3h,008h,010h,0C3h,008h,010h,0C3h  ;0EF0h
   .DB 008h,010h,0C3h,008h,010h,0C3h,008h,010h,001h,0E9h,002h,0CDh,0AFh,009h,0C3h,06Dh  ;0F00h
   .DB 010h,021h,003h,000h,036h,000h,0C3h,032h,010h,0C3h,06Dh,010h,021h,003h,000h,036h  ;0F10h
   .DB 001h,0C3h,032h,010h,0C3h,06Dh,010h,021h,003h,000h,036h,003h,0C3h,032h,010h,0C3h  ;0F20h
   .DB 06Dh,010h,021h,090h,01Fh,036h,000h,011h,000h,000h,00Eh,001h,0CDh,005h,000h,032h  ;0F30h
   .DB 08Fh,01Fh,0C3h,06Dh,010h,088h,00Fh,0ADh,00Fh,0B6h,00Fh,0BFh,00Fh,0CAh,00Fh,0D5h  ;0F40h
   .DB 00Fh,0E0h,00Fh,0F0h,00Fh,0F3h,00Fh,0F6h,00Fh,0F9h,00Fh,0FCh,00Fh,0FFh,00Fh,002h  ;0F50h
   .DB 010h,005h,010h,008h,010h,011h,010h,01Ch,010h,027h,010h,032h,010h,03Ah,08Eh,01Fh  ;0F60h
   .DB 032h,003h,000h,03Ah,054h,01Fh,01Fh,0D2h,092h,010h,03Ah,0A3h,01Eh,032h,08Eh,01Fh  ;0F70h
   .DB 021h,0A3h,01Eh,036h,013h,02Ah,08Fh,01Fh,04Dh,0CDh,045h,00Eh,03Ah,08Eh,01Fh,032h  ;0F80h
   .DB 0A3h,01Eh,03Ah,090h,01Fh,01Fh,0D2h,0DCh,010h,03Ah,004h,01Eh,01Fh,0D2h,0B2h,010h  ;0F90h
   .DB 03Ah,0A7h,01Eh,03Ch,032h,0A7h,01Eh,0D6h,000h,0D6h,001h,09Fh,032h,090h,01Fh,0C3h  ;0FA0h
   .DB 0BDh,010h,03Ah,08Fh,01Fh,0D6h,00Ah,0D6h,001h,09Fh,032h,090h,01Fh,03Ah,090h,01Fh  ;0FB0h
   .DB 01Fh,0D2h,0DCh,010h,0CDh,07Dh,009h,01Fh,0D2h,0DCh,010h,0CDh,013h,008h,0FEh,01Ah  ;0FC0h
   .DB 0C2h,0D6h,010h,03Eh,01Ah,0C9h,001h,000h,003h,0CDh,0AFh,009h,03Ah,069h,01Fh,01Fh  ;0FD0h
   .DB 0D2h,0EBh,010h,03Ah,08Fh,01Fh,0E6h,07Fh,032h,08Fh,01Fh,03Ah,064h,01Fh,01Fh,0D2h  ;0FE0h
   .DB 0FAh,010h,02Ah,08Fh,01Fh,04Dh,0CDh,0EEh,00Eh,0C9h,03Ah,05Bh,01Fh,01Fh,0D2h,009h  ;0FF0h
											;BLOCK E2
   .DB 011h,02Ah,08Fh,01Fh,04Dh,0CDh,015h,00Fh,0C9h,03Ah,08Fh,01Fh,0C9h,03Eh,000h,021h  ;1000h
   .DB 0FAh,01Dh,0BEh,0D2h,028h,011h,03Ah,0FAh,01Dh,03Dh,032h,0FAh,01Dh,0FEh,001h,0C2h  ;1010h
   .DB 025h,011h,03Eh,00Ah,0C9h,03Eh,01Ah,0C9h,03Eh,000h,021h,0F8h,01Dh,0BEh,0D2h,046h  ;1020h
   .DB 011h,021h,0F8h,01Dh,035h,02Bh,04Eh,006h,000h,021h,0CDh,01Eh,009h,07Eh,032h,091h  ;1030h
   .DB 01Fh,021h,0F7h,01Dh,034h,0C9h,0CDh,03Ch,00Fh,032h,091h,01Fh,0FEh,01Ah,0C2h,054h  ;1040h
   .DB 011h,03Eh,01Ah,0C9h,03Eh,000h,021h,062h,01Fh,0BEh,0D2h,07Dh,011h,02Ah,062h,01Fh  ;1050h
   .DB 04Dh,0CDh,0ADh,011h,01Fh,0D2h,07Ah,011h,03Ah,062h,01Fh,032h,0F7h,01Dh,021h,062h  ;1060h
   .DB 01Fh,036h,000h,03Ah,0F9h,01Dh,03Ch,032h,0F8h,01Dh,0C3h,0A9h,011h,03Eh,000h,021h  ;1070h
   .DB 060h,01Fh,0BEh,0D2h,0A5h,011h,02Ah,060h,01Fh,04Dh,0CDh,0ADh,011h,01Fh,0D2h,09Eh  ;1080h
   .DB 011h,021h,060h,01Fh,036h,000h,021h,0FAh,01Dh,036h,002h,03Eh,00Dh,0C9h,03Ah,091h  ;1090h
   .DB 01Fh,0C9h,0C3h,0A9h,011h,03Ah,091h,01Fh,0C9h,0C3h,028h,011h,0C9h,021h,092h,01Fh  ;10A0h
   .DB 071h,03Ah,0F9h,01Dh,021h,092h,01Fh,086h,077h,04Fh,006h,000h,021h,0CDh,01Eh,009h  ;10B0h
   .DB 07Eh,032h,093h,01Fh,0FEh,01Ah,0C2h,0D9h,011h,02Ah,092h,01Fh,026h,000h,001h,0CDh  ;10C0h
   .DB 01Eh,009h,03Ah,091h,01Fh,077h,03Eh,001h,0C9h,021h,091h,01Fh,03Ah,093h,01Fh,0BEh  ;10D0h
   .DB 0C2h,0EAh,011h,021h,0F9h,01Dh,034h,0C3h,0EFh,011h,021h,0F9h,01Dh,036h,000h,03Eh  ;10E0h
   .DB 000h,0C9h,03Ah,04Eh,01Fh,03Ch,032h,04Eh,01Fh,021h,0CCh,01Eh,0BEh,0DAh,003h,012h  ;10F0h
   .DB 03Eh,00Dh,0C9h,02Ah,04Eh,01Fh,026h,000h,001h,0CDh,01Eh,009h,04Eh,0CDh,0EEh,00Eh  ;1100h
   .DB 0C9h,0CDh,0F2h,011h,032h,0A8h,01Eh,0FEh,020h,0C2h,01Fh,012h,0C3h,011h,012h,0C9h  ;1110h
   .DB 021h,096h,01Fh,070h,02Bh,071h,021h,0F5h,01Dh,036h,000h,021h,0A9h,01Eh,036h,000h  ;1120h
   .DB 02Bh,036h,020h,021h,0AAh,01Eh,036h,000h,03Ah,0AAh,01Eh,0FEh,020h,0D2h,053h,012h  ;1130h
   .DB 03Ah,0AAh,01Eh,0FEh,00Bh,0C2h,04Dh,012h,021h,0A8h,01Eh,036h,000h,0CDh,067h,014h  ;1140h
   .DB 0C3h,038h,012h,0CDh,011h,012h,03Ah,04Eh,01Fh,032h,04Dh,01Fh,02Ah,0A8h,01Eh,04Dh  ;1150h
   .DB 0CDh,038h,014h,01Fh,0D2h,070h,012h,0CDh,0C1h,015h,021h,0A9h,01Eh,036h,001h,0C9h  ;1160h
   .DB 021h,094h,01Fh,036h,000h,021h,097h,01Fh,036h,000h,03Eh,019h,021h,097h,01Fh,0BEh  ;1170h
   .DB 0DAh,095h,012h,02Ah,097h,01Fh,026h,000h,001h,050h,01Fh,009h,036h,000h,021h,097h  ;1180h
   .DB 01Fh,034h,0C2h,07Ah,012h,021h,0F6h,01Dh,036h,000h,021h,0F8h,01Dh,036h,000h,023h  ;1190h
   .DB 036h,000h,023h,036h,000h,021h,0AAh,01Eh,036h,000h,02Ah,0A8h,01Eh,04Dh,0CDh,038h  ;11A0h
   .DB 014h,01Fh,0DAh,0DAh,012h,03Ah,0AAh,01Eh,0FEh,008h,0DAh,0BEh,012h,0C9h,03Ah,0A8h  ;11B0h
   .DB 01Eh,0FEh,02Ah,0C2h,0CEh,012h,00Eh,008h,0CDh,087h,014h,0C3h,0D1h,012h,0CDh,067h  ;11C0h
   .DB 014h,0CDh,0F2h,011h,032h,0A8h,01Eh,0C3h,0AAh,012h,03Ah,0A8h,01Eh,0FEh,03Ah,0C2h  ;11D0h
   .DB 0BFh,013h,03Ah,094h,01Fh,0FEh,000h,0CAh,0EBh,012h,0C9h,03Ah,0AAh,01Eh,0FEh,001h  ;11E0h
   .DB 0C2h,02Ch,013h,00Eh,001h,0CDh,0A1h,014h,0D6h,041h,03Ch,032h,094h,01Fh,04Fh,03Eh  ;11F0h
   .DB 01Ah,0B9h,0D2h,006h,013h,0C9h,0CDh,011h,012h,02Ah,0A8h,01Eh,04Dh,0CDh,038h,014h  ;1200h
   .DB 01Fh,0D2h,029h,013h,03Ah,0A8h,01Eh,0FEh,05Bh,0C2h,01Fh,013h,0CDh,0B1h,014h,021h  ;1210h
   .DB 04Eh,01Fh,035h,021h,0A9h,01Eh,036h,004h,0C9h,0C3h,0B1h,013h,03Ah,0AAh,01Eh,0FEh  ;1220h
   .DB 003h,0CAh,035h,013h,0C9h,021h,0A1h,01Fh,036h,0FFh,023h,036h,000h,03Eh,014h,021h  ;1230h
   .DB 0A2h,01Fh,0BEh,0DAh,0B0h,013h,021h,0A0h,01Fh,036h,000h,03Ah,0A0h,01Fh,03Ch,032h  ;1240h
   .DB 0A0h,01Fh,04Fh,03Eh,003h,091h,09Fh,02Fh,0F5h,03Ah,0A0h,01Fh,021h,0A1h,01Fh,086h  ;1250h
   .DB 04Fh,006h,000h,021h,054h,002h,009h,0E5h,02Ah,0A0h,01Fh,04Dh,0CDh,0A1h,014h,0E1h  ;1260h
   .DB 096h,0D6h,001h,09Fh,0C1h,048h,0A1h,01Fh,0D2h,07Eh,013h,0C3h,04Bh,013h,03Ah,0A0h  ;1270h
   .DB 01Fh,0FEh,004h,0C2h,0A1h,013h,021h,0A9h,01Eh,036h,003h,0CDh,0F2h,011h,0FEh,05Bh  ;1280h
   .DB 0C2h,096h,013h,0CDh,0B1h,014h,021h,04Eh,01Fh,035h,03Ah,0A2h,01Fh,032h,0A8h,01Eh  ;1290h
   .DB 0C9h,03Ah,0A1h,01Fh,0C6h,003h,032h,0A1h,01Fh,021h,0A2h,01Fh,034h,0C2h,03Dh,013h  ;12A0h
   .DB 0C9h,03Ah,0A8h,01Eh,0FEh,05Bh,0C2h,0BCh,013h,0CDh,0B1h,014h,0C3h,034h,014h,03Ah  ;12B0h
   .DB 0AAh,01Eh,0FEh,000h,0C2h,0C8h,013h,0C9h,021h,0AAh,01Eh,036h,008h,03Ah,0A8h,01Eh  ;12C0h
   .DB 0FEh,02Eh,0C2h,002h,014h,0CDh,0F2h,011h,032h,0A8h,01Eh,04Fh,0CDh,038h,014h,01Fh  ;12D0h
   .DB 0DAh,002h,014h,03Ah,0AAh,01Eh,0FEh,00Bh,0DAh,0ECh,013h,0C9h,03Ah,0A8h,01Eh,0FEh  ;12E0h
   .DB 02Ah,0C2h,0FCh,013h,00Eh,00Bh,0CDh,087h,014h,0C3h,0FFh,013h,0CDh,067h,014h,0C3h  ;12F0h
   .DB 0D5h,013h,03Ah,0A8h,01Eh,0FEh,05Bh,0C2h,00Dh,014h,0CDh,0B1h,014h,021h,04Eh,01Fh  ;1300h
   .DB 035h,021h,0A9h,01Eh,036h,002h,03Ah,094h,01Fh,0FEh,000h,0C2h,025h,014h,03Ah,0FCh  ;1310h
   .DB 01Dh,03Ch,032h,094h,01Fh,02Ah,095h,01Fh,036h,000h,001h,020h,000h,02Ah,095h,01Fh  ;1320h
   .DB 009h,036h,000h,0C9h,0C3h,0A5h,012h,0C9h,021h,09Ah,01Fh,071h,021h,09Bh,01Fh,036h  ;1330h
   .DB 000h,03Eh,00Ah,021h,09Bh,01Fh,0BEh,0DAh,064h,014h,02Ah,09Bh,01Fh,026h,000h,001h  ;1340h
   .DB 049h,002h,009h,03Ah,09Ah,01Fh,0BEh,0C2h,05Dh,014h,03Eh,001h,0C9h,021h,09Bh,01Fh  ;1350h
   .DB 034h,0C2h,041h,014h,03Eh,000h,0C9h,03Ah,0AAh,01Eh,03Ch,032h,0AAh,01Eh,04Fh,006h  ;1360h
   .DB 000h,02Ah,095h,01Fh,009h,03Ah,0A8h,01Eh,077h,03Ah,0A8h,01Eh,0FEh,03Fh,0C2h,086h  ;1370h
   .DB 014h,021h,0F5h,01Dh,036h,001h,0C9h,021h,09Ch,01Fh,071h,021h,0A8h,01Eh,036h,03Fh  ;1380h
   .DB 021h,09Ch,01Fh,03Ah,0AAh,01Eh,0BEh,0D2h,0A0h,014h,0CDh,067h,014h,0C3h,090h,014h  ;1390h
   .DB 0C9h,021h,09Dh,01Fh,071h,02Ah,09Dh,01Fh,026h,000h,0EBh,02Ah,095h,01Fh,019h,07Eh  ;13A0h
   .DB 0C9h,021h,0F6h,01Dh,036h,001h,03Ah,0C0h,01Eh,032h,0C1h,01Eh,0CDh,0F2h,011h,032h  ;13B0h
   .DB 0A8h,01Eh,03Ah,0A8h,01Eh,0D6h,00Dh,0D6h,001h,09Fh,0F5h,03Ah,0A8h,01Eh,0D6h,05Dh  ;13C0h
   .DB 0D6h,001h,09Fh,0C1h,048h,0B1h,01Fh,0DAh,0BAh,015h,03Ah,0A8h,01Eh,0D6h,041h,032h  ;13D0h
   .DB 09Eh,01Fh,04Fh,03Eh,019h,0B9h,0D2h,003h,015h,03Ah,0A8h,01Eh,0FEh,020h,0C2h,0FAh  ;13E0h
   .DB 014h,0CDh,0F2h,011h,032h,0A8h,01Eh,0C3h,000h,015h,001h,008h,003h,0CDh,0AFh,009h  ;13F0h
   .DB 0C3h,0B7h,015h,03Ah,0A8h,01Eh,0D6h,053h,0D6h,001h,09Fh,0F5h,03Ah,0A8h,01Eh,0D6h  ;1400h
   .DB 051h,0D6h,001h,09Fh,0C1h,048h,0B1h,01Fh,0D2h,049h,015h,03Ah,04Eh,01Fh,03Ch,032h  ;1410h
   .DB 09Fh,01Fh,0CDh,0F2h,011h,032h,0A8h,01Eh,0D6h,01Ah,0D6h,001h,09Fh,0F5h,03Ah,0A8h  ;1420h
   .DB 01Eh,0D6h,00Dh,0D6h,001h,09Fh,0C1h,048h,0B1h,01Fh,0DAh,040h,015h,0C3h,022h,015h  ;1430h
   .DB 0CDh,0F2h,011h,032h,0A8h,01Eh,0C3h,08Dh,015h,0CDh,0F2h,011h,032h,0A8h,01Eh,0D6h  ;1440h
   .DB 030h,032h,09Fh,01Fh,04Fh,03Eh,009h,0B9h,0D2h,063h,015h,021h,09Fh,01Fh,036h,001h  ;1450h
   .DB 0C3h,08Dh,015h,0CDh,0F2h,011h,032h,0A8h,01Eh,0D6h,030h,032h,099h,01Fh,04Fh,03Eh  ;1460h
   .DB 009h,0B9h,0DAh,08Dh,015h,02Ah,09Fh,01Fh,026h,000h,0CDh,04Fh,01Dh,0E5h,02Ah,099h  ;1470h
   .DB 01Fh,026h,000h,0C1h,009h,0EBh,021h,09Fh,01Fh,073h,0C3h,063h,015h,02Ah,09Eh,01Fh  ;1480h
   .DB 026h,000h,001h,050h,01Fh,009h,03Ah,09Fh,01Fh,077h,03Ah,09Eh,01Fh,0FEh,006h,0C2h  ;1490h
   .DB 0B7h,015h,03Eh,01Fh,021h,09Fh,01Fh,0BEh,0D2h,0B1h,015h,001h,016h,003h,0CDh,0AFh  ;14A0h
   .DB 009h,03Ah,09Fh,01Fh,032h,0C1h,01Eh,0C3h,0C2h,014h,0CDh,0F2h,011h,032h,0A8h,01Eh  ;14B0h
   .DB 0C9h,03Ah,0A8h,01Eh,0FEh,05Fh,0C2h,0CEh,015h,021h,0A8h,01Eh,036h,03Dh,0C9h,021h  ;14C0h
   .DB 0A3h,01Fh,036h,000h,03Eh,027h,021h,0A3h,01Fh,0BEh,0DAh,0E9h,015h,00Eh,000h,0CDh  ;14D0h
   .DB 045h,00Eh,021h,0A3h,01Fh,034h,0C2h,0D4h,015h,0C9h,021h,0A9h,01Fh,070h,02Bh,071h  ;14E0h
   .DB 02Ah,0A8h,01Fh,0E5h,01Eh,003h,001h,030h,01Eh,0CDh,018h,00Ah,0C9h,021h,0ADh,01Fh  ;14F0h
   .DB 072h,02Bh,073h,02Bh,070h,02Bh,071h,02Ah,0ACh,01Fh,07Eh,0FEh,024h,0CAh,037h,016h  ;1500h
   .DB 02Ah,0ACh,01Fh,03Eh,07Fh,0A6h,02Ah,0AAh,01Fh,0F5h,03Eh,07Fh,0A6h,0C1h,048h,0B9h  ;1510h
   .DB 0CAh,026h,016h,03Eh,000h,0C9h,02Ah,0AAh,01Fh,023h,022h,0AAh,01Fh,02Ah,0ACh,01Fh  ;1520h
   .DB 023h,022h,0ACh,01Fh,0C3h,007h,016h,03Eh,001h,0C9h,0CDh,00Dh,011h,032h,0A8h,01Eh  ;1530h
   .DB 03Ah,004h,01Eh,01Fh,0D2h,052h,016h,001h,09Dh,01Eh,011h,09Fh,01Eh,0CDh,08Eh,01Dh  ;1540h
   .DB 09Fh,0C9h,03Ah,0A8h,01Eh,0D6h,01Ah,0D6h,001h,09Fh,0C9h,021h,0AFh,01Fh,036h,001h  ;1550h
   .DB 021h,09Ch,01Eh,036h,000h,0CDh,03Dh,017h,032h,0B5h,01Fh,0FEh,03Ah,0CAh,099h,016h  ;1560h
   .DB 021h,09Ch,01Eh,036h,000h,03Ah,0B5h,01Fh,0FEh,01Ah,0C2h,093h,016h,001h,048h,003h  ;1570h
   .DB 0CDh,039h,008h,0CDh,013h,008h,0FEh,01Ah,0C2h,08Eh,016h,03Eh,001h,0C9h,021h,09Ch  ;1580h
   .DB 01Eh,036h,000h,0CDh,02Dh,017h,0C3h,065h,016h,021h,0B5h,01Fh,036h,000h,0CDh,0BBh  ;1590h
   .DB 017h,032h,0B4h,01Fh,0FEh,000h,0C2h,0C7h,016h,0CDh,03Dh,017h,032h,0B4h,01Fh,0FEh  ;15A0h
   .DB 01Ah,0CAh,0BAh,016h,0CDh,02Dh,017h,0C3h,0A9h,016h,03Ah,0AFh,01Fh,01Fh,0D2h,0C4h  ;15B0h
   .DB 016h,03Eh,001h,0C9h,03Eh,002h,0C9h,0CDh,0C4h,017h,022h,0B7h,01Fh,0CDh,0BBh,017h  ;15C0h
   .DB 032h,0B6h,01Fh,03Ah,0B4h,01Fh,0D6h,000h,0C6h,0FFh,09Fh,021h,0AFh,01Fh,0A6h,01Fh  ;15D0h
   .DB 0D2h,0F0h,016h,021h,0B4h,01Fh,035h,0CDh,0BBh,017h,032h,0B3h,01Fh,0C3h,0D3h,016h  ;15E0h
   .DB 0CDh,0ACh,017h,021h,0B5h,01Fh,086h,0FEh,000h,0CAh,002h,017h,001h,05Ch,003h,0CDh  ;15F0h
   .DB 012h,017h,0CDh,02Dh,017h,03Ah,0AFh,01Fh,01Fh,0D2h,00Fh,017h,03Eh,000h,0C9h,03Eh  ;1600h
   .DB 002h,0C9h,021h,0B1h,01Fh,070h,02Bh,071h,03Ah,0AFh,01Fh,01Fh,0D2h,02Ch,017h,021h  ;1610h
   .DB 0AFh,01Fh,036h,000h,02Ah,0B0h,01Fh,044h,04Dh,0CDh,039h,008h,0C9h,03Ah,0AEh,01Fh  ;1620h
   .DB 01Fh,0D2h,03Ch,017h,021h,0AEh,01Fh,036h,000h,0CDh,018h,00Eh,0C9h,03Ah,0AFh,01Fh  ;1630h
   .DB 01Fh,0D2h,07Ah,017h,0CDh,00Dh,011h,032h,0B2h,01Fh,0FEh,013h,0C2h,057h,017h,021h  ;1640h
   .DB 0AEh,01Fh,036h,001h,0C3h,044h,017h,02Ah,09Ch,01Eh,026h,000h,001h,04Ch,01Eh,009h  ;1650h
   .DB 03Ah,0B2h,01Fh,077h,03Ah,09Ch,01Eh,03Ch,032h,09Ch,01Eh,0FEh,04Fh,0DAh,076h,017h  ;1660h
   .DB 001h,02Ah,003h,0CDh,012h,017h,03Ah,0B2h,01Fh,0C9h,03Eh,01Ah,0C9h,0CDh,03Dh,017h  ;1670h
   .DB 032h,0B9h,01Fh,0D6h,030h,04Fh,03Eh,009h,0B9h,0DAh,092h,017h,03Ah,0B9h,01Fh,0D6h  ;1680h
   .DB 030h,0C9h,03Ah,0B9h,01Fh,0D6h,041h,04Fh,03Eh,005h,0B9h,0D2h,0A4h,017h,001h,03Ah  ;1690h
   .DB 003h,0CDh,012h,017h,03Ah,0B9h,01Fh,0D6h,041h,0C6h,00Ah,0C9h,0CDh,07Dh,017h,087h  ;16A0h
   .DB 087h,087h,087h,0F5h,0CDh,07Dh,017h,0C1h,048h,0B1h,0C9h,0CDh,0ACh,017h,021h,0B5h  ;16B0h
   .DB 01Fh,086h,077h,0C9h,0CDh,0BBh,017h,04Fh,006h,000h,060h,069h,00Eh,008h,0CDh,064h  ;16C0h
   .DB 01Dh,0E5h,0CDh,0BBh,017h,0E1h,0CDh,056h,01Dh,0C9h,0CDh,05Bh,016h,032h,0BAh,01Fh  ;16D0h
   .DB 04Fh,03Eh,001h,0B9h,0DAh,032h,018h,03Ah,0BAh,01Fh,0D6h,001h,0D6h,001h,09Fh,021h  ;16E0h
   .DB 058h,01Fh,0A6h,01Fh,0DAh,01Ch,018h,021h,0BBh,01Fh,036h,001h,03Ah,09Ch,01Eh,021h  ;16F0h
   .DB 0BBh,01Fh,0BEh,0DAh,01Ch,018h,03Ah,0BBh,01Fh,03Dh,04Fh,006h,000h,021h,04Ch,01Eh  ;1700h
   .DB 009h,04Eh,0CDh,045h,00Eh,021h,0BBh,01Fh,034h,0C2h,0FCh,017h,00Eh,00Dh,0CDh,045h  ;1710h
   .DB 00Eh,00Eh,00Ah,0CDh,045h,00Eh,03Ah,0BAh,01Fh,0FEh,001h,0C2h,02Fh,018h,0C9h,0C3h  ;1720h
   .DB 0DAh,017h,0CDh,02Eh,008h,02Ah,09Ch,01Eh,026h,000h,001h,04Ch,01Eh,009h,036h,024h  ;1730h
   .DB 001h,04Ch,01Eh,0CDh,039h,008h,001h,06Bh,003h,0CDh,039h,008h,0CDh,02Eh,008h,0CDh  ;1740h
   .DB 013h,008h,0FEh,01Ah,0C2h,058h,018h,0C9h,0C3h,0DAh,017h,0C9h,001h,08Fh,003h,0CDh  ;1750h
   .DB 0AFh,009h,0C9h,02Ah,04Bh,01Eh,04Dh,0CDh,05Eh,008h,011h,09Eh,003h,001h,030h,01Eh  ;1760h
   .DB 0CDh,0FDh,015h,032h,005h,01Eh,001h,030h,01Eh,0C5h,01Eh,003h,001h,0A4h,01Fh,0CDh  ;1770h
   .DB 018h,00Ah,03Ah,030h,01Eh,0E6h,07Fh,032h,030h,01Eh,03Ah,031h,01Eh,0E6h,07Fh,032h  ;1780h
   .DB 031h,01Eh,001h,0A2h,003h,0CDh,0EAh,015h,001h,027h,01Eh,0CDh,0B3h,008h,001h,027h  ;1790h
   .DB 01Eh,0CDh,0E3h,008h,03Ah,0AEh,01Eh,0FEh,0FFh,0C2h,0B2h,018h,001h,0A5h,003h,0CDh  ;17A0h
   .DB 0AFh,009h,021h,047h,01Eh,036h,000h,021h,000h,000h,022h,0A1h,01Eh,0C9h,021h,0FFh  ;17B0h
   .DB 0FFh,022h,09Fh,01Eh,0CDh,037h,009h,02Ah,003h,01Eh,04Dh,0CDh,05Eh,008h,001h,006h  ;17C0h
   .DB 01Eh,0CDh,06Eh,008h,0CDh,02Fh,009h,03Ah,061h,01Fh,02Fh,0F5h,03Ah,010h,01Eh,007h  ;17D0h
   .DB 0C1h,048h,0A1h,01Fh,0D2h,0ECh,018h,021h,0AEh,01Eh,036h,0FFh,03Ah,0AEh,01Eh,0FEh  ;17E0h
   .DB 0FFh,0C2h,0FAh,018h,001h,0B8h,003h,0CDh,0AFh,009h,021h,026h,01Eh,036h,000h,011h  ;17F0h
											;BLOCK E3
   .DB 0C0h,003h,001h,00Fh,01Eh,0CDh,0FDh,015h,032h,004h,01Eh,02Ah,0FDh,01Dh,022h,09Dh  ;1800h
   .DB 01Eh,0C9h,03Eh,000h,021h,062h,01Fh,0BEh,0D2h,021h,019h,001h,0C4h,003h,0CDh,0AFh  ;1810h
   .DB 009h,03Eh,000h,021h,060h,01Fh,0BEh,0D2h,030h,019h,001h,0D4h,003h,0CDh,0AFh,009h  ;1820h
   .DB 0C9h,021h,0BCh,01Fh,071h,03Ah,0BCh,01Fh,01Fh,0D2h,045h,019h,03Ah,013h,01Eh,032h  ;1830h
   .DB 034h,01Eh,0C3h,04Ah,019h,021h,034h,01Eh,036h,000h,02Ah,0A1h,01Eh,07Dh,0E6h,07Fh  ;1840h
   .DB 0FEh,000h,0CAh,061h,019h,021h,034h,01Eh,034h,00Eh,01Ah,0CDh,045h,00Eh,0C3h,04Ah  ;1850h
   .DB 019h,0CDh,012h,019h,0CDh,0C8h,00Ah,02Ah,04Bh,01Eh,04Dh,0CDh,05Eh,008h,001h,027h  ;1860h
   .DB 01Eh,0CDh,081h,008h,03Ah,0AEh,01Eh,0FEh,0FFh,0C2h,082h,019h,001h,0E3h,003h,0CDh  ;1870h
   .DB 0AFh,009h,001h,0A4h,01Fh,0CDh,0EAh,015h,021h,033h,01Eh,036h,000h,001h,027h,01Eh  ;1880h
   .DB 0CDh,06Eh,008h,03Ah,0AEh,01Eh,0FEh,0FFh,0CAh,0E9h,019h,03Ah,030h,01Eh,007h,01Fh  ;1890h
   .DB 0D2h,0E3h,019h,03Ah,066h,01Fh,01Fh,0DAh,0D5h,019h,001h,001h,004h,0CDh,039h,008h  ;18A0h
   .DB 0CDh,013h,008h,04Fh,0CDh,0EEh,00Eh,0FEh,059h,0CAh,0D2h,019h,001h,023h,004h,0CDh  ;18B0h
   .DB 039h,008h,0CDh,02Eh,008h,001h,033h,004h,0CDh,0EAh,015h,001h,027h,01Eh,0CDh,0B3h  ;18C0h
   .DB 008h,0C9h,0CDh,02Eh,008h,03Ah,030h,01Eh,0E6h,07Fh,032h,030h,01Eh,001h,027h,01Eh  ;18D0h
   .DB 0CDh,006h,009h,001h,027h,01Eh,0CDh,0B3h,008h,001h,027h,01Eh,0C5h,01Eh,010h,001h  ;18E0h
   .DB 037h,01Eh,0CDh,018h,00Ah,001h,036h,004h,0CDh,0EAh,015h,001h,027h,01Eh,0CDh,0F6h  ;18F0h
   .DB 008h,0C9h,00Eh,007h,021h,0FFh,01Dh,0CDh,06Ah,01Dh,03Eh,0FFh,0CDh,041h,01Dh,02Bh  ;1900h
   .DB 0EBh,021h,0FBh,01Dh,073h,0C9h,021h,0CAh,01Fh,022h,001h,01Eh,001h,000h,040h,011h  ;1910h
   .DB 0FFh,01Dh,0CDh,093h,01Dh,0DAh,031h,01Ah,021h,080h,07Fh,022h,0FFh,01Dh,0C3h,03Ch  ;1920h
   .DB 01Ah,02Ah,0FDh,01Dh,0EBh,02Ah,0FFh,01Dh,019h,022h,0FFh,01Dh,0CDh,002h,01Ah,0C9h  ;1930h
   .DB 001h,0CAh,01Fh,011h,006h,000h,0CDh,093h,01Dh,00Eh,001h,0E5h,0CDh,06Eh,01Dh,001h  ;1940h
   .DB 0CAh,01Fh,009h,022h,001h,01Eh,011h,000h,0FFh,0E1h,0CDh,044h,01Dh,00Eh,001h,0CDh  ;1950h
   .DB 06Eh,01Dh,022h,0FDh,01Dh,022h,0FFh,01Dh,0CDh,002h,01Ah,0C9h,03Ah,0A4h,01Eh,0D6h  ;1960h
   .DB 000h,0C6h,0FFh,09Fh,021h,051h,01Fh,0A6h,032h,0BDh,01Fh,01Fh,0D2h,082h,01Ah,0CDh  ;1970h
   .DB 016h,01Ah,03Ah,058h,01Fh,021h,057h,01Fh,0B6h,01Fh,0D2h,093h,01Ah,0CDh,0DAh,017h  ;1980h
   .DB 0C3h,0A4h,01Ah,0CDh,03Ah,016h,01Fh,0DAh,0A4h,01Ah,02Ah,0A8h,01Eh,04Dh,0CDh,045h  ;1990h
   .DB 00Eh,0C3h,093h,01Ah,03Ah,0BDh,01Fh,01Fh,0D2h,0B1h,01Ah,0CDh,018h,00Eh,0CDh,040h  ;19A0h
   .DB 01Ah,0C9h,0CDh,040h,01Ah,03Ah,04Fh,01Fh,032h,04Dh,01Fh,0CDh,063h,018h,0CDh,0BEh  ;19B0h
   .DB 018h,021h,0BEh,01Fh,036h,001h,023h,036h,000h,03Eh,019h,021h,0BFh,01Fh,0BEh,0DAh  ;19C0h
   .DB 029h,01Bh,02Ah,0BFh,01Fh,026h,000h,001h,050h,01Fh,009h,07Eh,0FEh,000h,0CAh,022h  ;19D0h
   .DB 01Bh,03Ah,0BFh,01Fh,0D6h,006h,0D6h,001h,09Fh,0F5h,03Ah,0BFh,01Fh,0D6h,00Eh,0D6h  ;19E0h
   .DB 001h,09Fh,0C1h,048h,0B1h,0F5h,03Ah,0BFh,01Fh,0D6h,011h,0D6h,001h,09Fh,0C1h,048h  ;19F0h
   .DB 0B1h,0F5h,03Ah,0BFh,01Fh,0D6h,015h,0D6h,001h,09Fh,0C1h,048h,0B1h,0F5h,03Ah,0BFh  ;1A00h
   .DB 01Fh,0D6h,016h,0D6h,001h,09Fh,0C1h,048h,0B1h,01Fh,0DAh,022h,01Bh,021h,0BEh,01Fh  ;1A10h
   .DB 036h,000h,021h,0BFh,01Fh,034h,0C2h,0C9h,01Ah,03Ah,0BEh,01Fh,01Fh,0D2h,05Fh,01Bh  ;1A20h
   .DB 0CDh,016h,01Ah,0CDh,06Ah,01Bh,01Fh,0DAh,059h,01Bh,0CDh,04Fh,00Ah,0CDh,06Ah,01Bh  ;1A30h
   .DB 01Fh,0D2h,04Dh,01Bh,02Ah,09Fh,01Eh,022h,0A1h,01Eh,0C3h,053h,01Bh,02Ah,0FFh,01Dh  ;1A40h
   .DB 022h,0A1h,01Eh,0CDh,0C8h,00Ah,0C3h,033h,01Bh,0CDh,040h,01Ah,0C3h,062h,01Bh,0CDh  ;1A50h
   .DB 06Ch,01Ah,02Ah,0BEh,01Fh,04Dh,0CDh,031h,019h,0C9h,001h,0FFh,0FFh,011h,09Fh,01Eh  ;1A60h
   .DB 0CDh,093h,01Dh,0B5h,0C6h,0FFh,09Fh,0C9h,021h,000h,000h,022h,0C0h,01Fh,022h,0C4h  ;1A70h
   .DB 01Fh,0CDh,037h,009h,02Ah,003h,01Eh,04Dh,0CDh,05Eh,008h,001h,080h,000h,0CDh,086h  ;1A80h
   .DB 009h,001h,05Ch,000h,0CDh,094h,008h,021h,000h,000h,022h,0C2h,01Fh,03Ah,0AEh,01Eh  ;1A90h
   .DB 0D6h,0FFh,0C6h,0FFh,09Fh,001h,0C0h,01Fh,011h,0C2h,01Fh,0F5h,0CDh,08Eh,01Dh,09Fh  ;1AA0h
   .DB 0C1h,048h,0A1h,01Fh,0D2h,0C4h,01Bh,02Ah,0C2h,01Fh,023h,022h,0C2h,01Fh,0CDh,0A7h  ;1AB0h
   .DB 008h,0C3h,09Dh,01Bh,0CDh,02Fh,009h,03Ah,0AEh,01Eh,0FEh,0FFh,0C2h,0E5h,01Bh,03Eh  ;1AC0h
   .DB 000h,011h,0C4h,01Fh,0CDh,09Bh,01Dh,0B5h,0C2h,0E1h,01Bh,001h,039h,004h,0CDh,0AFh  ;1AD0h
   .DB 009h,0CDh,02Eh,008h,0C9h,02Ah,0C2h,01Fh,023h,022h,0C0h,01Fh,03Ah,0AEh,01Eh,0E6h  ;1AE0h
   .DB 003h,087h,087h,087h,087h,087h,05Fh,016h,000h,021h,080h,000h,019h,0E5h,01Eh,010h  ;1AF0h
   .DB 001h,027h,01Eh,0CDh,018h,00Ah,021h,027h,01Eh,036h,000h,021h,033h,01Eh,036h,000h  ;1B00h
   .DB 001h,027h,01Eh,0C5h,01Eh,010h,001h,006h,01Eh,0CDh,018h,00Ah,03Ah,031h,01Eh,007h  ;1B10h
   .DB 02Fh,021h,061h,01Fh,0B6h,01Fh,0D2h,045h,01Ch,02Ah,0C4h,01Fh,023h,022h,0C4h,01Fh  ;1B20h
   .DB 03Eh,001h,0CDh,07Ah,01Dh,0B5h,0C2h,03Fh,01Ch,001h,043h,004h,0CDh,039h,008h,0CDh  ;1B30h
   .DB 049h,01Ch,0CDh,0B2h,01Ah,0C3h,081h,01Bh,0C9h,0CDh,02Eh,008h,021h,0C6h,01Fh,036h  ;1B40h
   .DB 001h,03Eh,00Bh,021h,0C6h,01Fh,0BEh,0DAh,087h,01Ch,02Ah,0C6h,01Fh,026h,000h,001h  ;1B50h
   .DB 027h,01Eh,009h,07Eh,032h,0C7h,01Fh,0FEh,020h,0CAh,080h,01Ch,03Ah,0C6h,01Fh,0FEh  ;1B60h
   .DB 009h,0C2h,079h,01Ch,00Eh,02Eh,0CDh,01Ch,008h,02Ah,0C7h,01Fh,04Dh,0CDh,01Ch,008h  ;1B70h
   .DB 021h,0C6h,01Fh,034h,0C2h,051h,01Ch,0C9h,03Eh,000h,021h,094h,01Fh,0BEh,0D2h,09Bh  ;1B80h
   .DB 01Ch,03Ah,094h,01Fh,03Dh,032h,003h,01Eh,0C3h,0A1h,01Ch,03Ah,0FCh,01Dh,032h,003h  ;1B90h
   .DB 01Eh,0C9h,03Ah,0F6h,01Dh,01Fh,0D2h,0ACh,01Ch,0CDh,05Ch,018h,03Eh,000h,021h,094h  ;1BA0h
   .DB 01Fh,0BEh,0D2h,0BFh,01Ch,03Ah,094h,01Fh,03Dh,032h,04Bh,01Eh,0C3h,0C5h,01Ch,03Ah  ;1BB0h
   .DB 0FCh,01Dh,032h,04Bh,01Eh,0C9h,021h,0C0h,01Eh,03Ah,0C1h,01Eh,0BEh,0CAh,0D1h,01Ch  ;1BC0h
   .DB 0C9h,021h,003h,01Eh,03Ah,04Bh,01Eh,0BEh,0C2h,0DEh,01Ch,0CDh,05Ch,018h,0C9h,0CDh  ;1BD0h
   .DB 011h,012h,03Ah,0A8h,01Eh,0FEh,00Dh,0CAh,0EDh,01Ch,0CDh,05Ch,018h,0C9h,021h,0C9h  ;1BE0h
   .DB 01Fh,070h,02Bh,071h,0CDh,088h,01Ch,0CDh,0DFh,01Ch,001h,006h,01Eh,0C5h,02Ah,0C8h  ;1BF0h
   .DB 01Fh,044h,04Dh,01Eh,021h,0CDh,018h,00Ah,0CDh,0C6h,01Ch,0C9h,001h,006h,01Eh,0CDh  ;1C00h
   .DB 020h,012h,03Ah,0A9h,01Eh,0D6h,001h,0D6h,001h,09Fh,0F5h,03Ah,0A8h,01Eh,0D6h,03Dh  ;1C10h
   .DB 0D6h,001h,09Fh,0C1h,048h,0A1h,01Fh,0DAh,02Dh,01Dh,0CDh,05Ch,018h,03Ah,04Eh,01Fh  ;1C20h
   .DB 032h,04Fh,01Fh,0C9h,0EBh,05Fh,016h,000h,0EBh,01Ah,085h,06Fh,013h,01Ah,08Ch,067h  ;1C30h
   .DB 0C9h,05Fh,016h,000h,07Bh,0A5h,06Fh,07Ah,0A4h,067h,0C9h,05Eh,023h,056h,0EBh,029h  ;1C40h
   .DB 0E5h,029h,029h,0C1h,009h,0C9h,05Fh,016h,000h,07Bh,0B5h,06Fh,07Ah,0B4h,067h,0C9h  ;1C50h
   .DB 05Eh,023h,056h,0EBh,029h,00Dh,0C2h,064h,01Dh,0C9h,05Eh,023h,056h,0EBh,07Ch,0B7h  ;1C60h
   .DB 01Fh,067h,07Dh,01Fh,06Fh,00Dh,0C2h,06Eh,01Dh,0C9h,05Fh,016h,000h,07Bh,095h,06Fh  ;1C70h
   .DB 07Ah,09Ch,067h,0C9h,04Fh,006h,000h,07Bh,091h,06Fh,07Ah,098h,067h,0C9h,069h,060h  ;1C80h
   .DB 04Eh,023h,046h,01Ah,091h,06Fh,013h,01Ah,098h,067h,0C9h,06Fh,026h,000h,01Ah,095h  ;1C90h
   .DB 06Fh,013h,01Ah,09Ch,067h,0C9h,006h,00Ch,048h,00Dh,0C2h,0A9h,01Dh,03Dh,0C2h,0A8h  ;1CA0h
   .DB 01Dh,0C9h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1CB0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1CC0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1CD0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1CE0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1CF0h
   .DB 07Ah,09Ch,067h,0C9h,04Fh,006h,000h,07Bh,091h,06Fh,07Ah,098h,067h,0C9h,069h,060h  ;1D00h
   .DB 04Eh,023h,046h,01Ah,091h,06Fh,013h,01Ah,098h,067h,0C9h,06Fh,026h,000h,01Ah,095h  ;1D10h
   .DB 06Fh,013h,01Ah,09Ch,067h,0C9h,006h,00Ch,048h,00Dh,0C2h,0A9h,01Dh,03Dh,0C2h,0A8h  ;1D20h
   .DB 01Dh,0C9h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1D30h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1D40h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1D50h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1D60h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1D70h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1D80h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1D90h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1DA0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1DB0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1DC0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1DD0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1DE0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1DF0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1E00h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1E10h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1E20h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1E30h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1E40h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1E50h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1E60h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1E70h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1E80h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1E90h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1EA0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1EB0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1EC0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1ED0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1EE0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1EF0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1F00h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1F10h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1F20h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1F30h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1F40h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1F50h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1F60h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1F70h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1F80h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1F90h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1FA0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1FB0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1FC0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1FD0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1FE0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;1FF0h
											;BLOCK E4				
   .DB 031h,000h,009h,0CDh,039h,005h,005h,0C2h,07Eh,001h,0CDh,046h,004h,0CDh,062h,004h  ;0000h
   .DB 0AFh,032h,03Dh,004h,0CDh,0C4h,002h,0DAh,063h,001h,02Ah,03Bh,004h,023h,022h,03Bh  ;0010h
   .DB 004h,03Eh,001h,0CDh,06Ch,003h,03Ah,03Bh,004h,0CDh,06Ch,003h,02Fh,0CDh,06Ch,003h  ;0020h
   .DB 021h,000h,000h,022h,03Eh,004h,001h,000h,080h,021h,000h,007h,07Eh,0CDh,06Ch,003h  ;0030h
   .DB 0CDh,0D7h,002h,023h,005h,0C2h,03Ch,001h,03Ah,045h,004h,0B7h,0CAh,057h,001h,02Ah  ;0040h
   .DB 03Eh,004h,07Ch,0CDh,06Ch,003h,04Dh,0CDh,04Fh,003h,0CDh,075h,002h,0C2h,021h,001h  ;0050h
   .DB 0C3h,010h,001h,03Eh,004h,0CDh,06Ch,003h,0CDh,075h,002h,0C2h,063h,001h,0CDh,08Ah  ;0060h
   .DB 003h,04Fh,04Bh,00Dh,00Ah,053h,065h,06Eh,074h,020h,000h,0C3h,0ACh,001h,0CDh,093h  ;0070h
   .DB 004h,0CDh,0BDh,001h,0DAh,094h,001h,02Ah,03Bh,004h,023h,022h,03Bh,004h,0CDh,06Ah  ;0080h
   .DB 003h,0C3h,081h,001h,0CDh,06Ah,003h,0CDh,095h,002h,0CDh,08Ah,003h,04Fh,04Bh,000h  ;0090h
   .DB 0CDh,08Ah,003h,052h,065h,063h,065h,069h,076h,065h,064h,000h,02Ah,03Bh,004h,0CDh  ;00A0h
   .DB 01Dh,003h,0CDh,033h,004h,020h,062h,06Ch,06Fh,063h,06Bh,073h,000h,0AFh,032h,03Dh  ;00B0h
   .DB 004h,01Eh,014h,0CDh,0FDh,002h,0DAh,0E1h,001h,0FEh,001h,0CAh,0F7h,001h,0FEh,004h  ;00C0h
   .DB 037h,0C8h,006h,000h,011h,026h,004h,005h,0CAh,09Fh,003h,0CDh,0FBh,002h,0D2h,0D4h  ;00D0h
   .DB 001h,03Ah,040h,004h,0CDh,06Ch,003h,021h,03Dh,004h,034h,07Eh,0FEh,00Ah,0DAh,0C1h  ;00E0h
   .DB 001h,011h,0DFh,003h,0C3h,09Fh,003h,03Eh,015h,032h,040h,004h,0CDh,0FBh,002h,0DAh  ;00F0h
   .DB 0E1h,001h,057h,0CDh,0FBh,002h,0DAh,0E1h,001h,02Fh,0BAh,0C2h,0D2h,001h,032h,03Ah  ;0100h
   .DB 004h,021h,000h,000h,022h,03Eh,004h,001h,000h,080h,021h,000h,007h,0CDh,0FBh,002h  ;0110h
   .DB 0DAh,0E1h,001h,077h,0CDh,0D7h,002h,023h,005h,0C2h,01Dh,002h,03Ah,045h,004h,0B7h  ;0120h
   .DB 0CAh,043h,002h,0CDh,0FBh,002h,0DAh,0E1h,001h,0E5h,02Ah,03Eh,004h,04Dh,0BCh,0E1h  ;0130h
   .DB 0C2h,0D2h,001h,0CDh,0FBh,002h,0DAh,0E1h,001h,0B9h,0C2h,0E1h,001h,03Ah,03Bh,004h  ;0140h
   .DB 047h,03Ah,03Ah,004h,090h,0CCh,06Ah,003h,0CAh,0BDh,001h,03Dh,0C2h,06Fh,002h,011h  ;0150h
   .DB 05Ch,000h,00Eh,015h,0CDh,079h,003h,011h,009h,004h,0B7h,0C2h,09Fh,003h,0C9h,011h  ;0160h
   .DB 0FDh,003h,0C3h,09Fh,003h,01Eh,078h,0CDh,0FDh,002h,0DAh,087h,002h,0FEh,006h,0C8h  ;0170h
   .DB 0FEh,015h,0C2h,087h,002h,0B7h,0C9h,021h,03Dh,004h,034h,07Eh,0FEh,00Ah,0D8h,011h  ;0180h
   .DB 0EFh,003h,0C3h,09Fh,003h,011h,05Ch,000h,00Eh,010h,0CDh,079h,003h,03Ch,0C0h,0CDh  ;0190h
   .DB 030h,004h,046h,049h,04Ch,045h,020h,043h,04Ch,04Fh,053h,045h,020h,045h,052h,052h  ;01A0h
   .DB 04Fh,052h,021h,020h,04Dh,061h,079h,020h,062h,065h,020h,063h,06Fh,072h,072h,075h  ;01B0h
   .DB 070h,074h,02Eh,000h,03Ah,043h,004h,0B7h,037h,0C0h,011h,05Ch,000h,00Eh,014h,0CDh  ;01C0h
   .DB 079h,003h,032h,043h,004h,0B7h,0C9h,057h,081h,04Fh,07Ah,0C5h,0E5h,02Ah,03Eh,004h  ;01D0h
   .DB 0ACh,067h,011h,021h,010h,006h,008h,029h,0D2h,0F1h,002h,07Ch,0AAh,067h,07Dh,0ABh  ;01E0h
   .DB 06Fh,005h,0C2h,0E7h,002h,022h,03Eh,004h,0E1h,0C1h,0C9h,01Eh,002h,0E5h,021h,0ACh  ;01F0h
   .DB 00Dh,0CDh,010h,003h,0CDh,065h,003h,0CAh,001h,003h,0CDh,060h,003h,0B7h,0E1h,0C9h  ;0200h
   .DB 02Bh,07Dh,0B4h,0C0h,021h,0ACh,00Dh,01Dh,0C0h,0E1h,0E1h,037h,0C9h,016h,000h,001h  ;0210h
   .DB 0F0h,0D8h,0CDh,03Bh,003h,001h,018h,0FCh,0CDh,03Bh,003h,001h,09Ch,0FFh,0CDh,03Bh  ;0220h
   .DB 003h,001h,0F6h,0FFh,0CDh,03Bh,003h,07Dh,0C3h,04Ch,003h,03Eh,0FFh,0D5h,054h,05Dh  ;0230h
   .DB 03Ch,009h,0DAh,03Eh,003h,0EBh,0D1h,05Fh,0B2h,0C8h,057h,07Bh,0C6h,030h,04Fh,03Eh  ;0240h
   .DB 012h,0E5h,0D5h,0C5h,0CDh,05Bh,003h,0C1h,0D1h,0E1h,0C9h,02Ah,001h,000h,06Fh,0E9h  ;0250h
   .DB 03Eh,015h,0C3h,051h,003h,03Eh,006h,0C3h,051h,003h,03Eh,006h,0C5h,04Fh,0CDh,04Fh  ;0260h
   .DB 003h,079h,0C1h,0C9h,00Eh,013h,011h,05Ch,000h,0E5h,0D5h,0C5h,0CDh,005h,000h,0C1h  ;0270h
   .DB 0D1h,0E1h,0C9h,0CDh,08Dh,003h,00Dh,00Ah,000h,0C9h,0CDh,083h,003h,0E3h,0F5h,0CDh  ;0280h
   .DB 095h,003h,0F1h,0E3h,0C9h,07Eh,023h,0B7h,0C8h,0CDh,06Ch,003h,0C3h,095h,003h,0CDh  ;0290h
   .DB 08Ah,003h,041h,042h,04Fh,052h,054h,03Ah,000h,0EBh,0CDh,095h,003h,03Ah,044h,004h  ;02A0h
   .DB 0B7h,0C2h,037h,004h,0CDh,095h,002h,02Ah,03Bh,004h,07Ch,0B5h,0C2h,0A0h,001h,0CDh  ;02B0h
   .DB 074h,003h,03Ch,0CAh,037h,004h,0CDh,030h,004h,045h,06Dh,070h,074h,079h,020h,066h  ;02C0h
   .DB 069h,06Ch,065h,020h,064h,065h,06Ch,065h,074h,065h,064h,000h,05Eh,043h,000h,031h  ;02D0h
   .DB 0D0h,020h,062h,06Ch,06Fh,063h,06Bh,020h,065h,072h,072h,06Fh,072h,073h,000h,031h  ;02E0h
   .DB 0D0h,020h,041h,043h,04Bh,020h,065h,072h,072h,06Fh,072h,073h,000h,06Ch,06Fh,073h  ;02F0h
   .DB 074h,020h,062h,06Ch,06Fh,063h,06Bh,073h,000h,064h,069h,073h,06Bh,020h,077h,072h  ;0300h
   .DB 069h,074h,065h,020h,066h,061h,069h,06Ch,000h,055h,041h,052h,054h,020h,054h,078h  ;0310h
   .DB 020h,066h,061h,069h,06Ch,000h,073h,079h,06Eh,063h,020h,066h,061h,069h,06Ch,000h  ;0320h
   .DB 0CDh,083h,003h,0E1h,0CDh,095h,003h,0C3h,000h,000h,000h,000h,000h,000h,000h,000h  ;0330h
   .DB 015h,000h,000h,000h,0FFh,043h,011h,05Ch,000h,00Eh,00Fh,0CDh,079h,003h,03Ch,0C0h  ;0340h
   .DB 0CDh,030h,004h,046h,069h,06Ch,065h,020h,06Eh,06Fh,074h,020h,066h,06Fh,075h,06Eh  ;0350h
   .DB 064h,000h,006h,03Ch,021h,045h,004h,036h,000h,011h,07Dh,004h,005h,0CAh,09Fh,003h  ;0360h
   .DB 0CDh,0FBh,002h,0FEh,015h,0C8h,0FEh,043h,0C2h,069h,004h,077h,0C9h,06Eh,06Fh,020h  ;0370h
   .DB 069h,06Eh,069h,074h,020h,066h,072h,06Fh,06Dh,020h,072h,065h,063h,065h,069h,076h  ;0380h
   .DB 065h,072h,000h,011h,05Ch,000h,00Eh,011h,0CDh,079h,003h,03Ch,0CAh,0DDh,004h,0CDh  ;0390h
   .DB 08Ah,003h,046h,069h,06Ch,065h,020h,065h,078h,069h,073h,074h,073h,02Eh,020h,04Fh  ;03A0h
   .DB 076h,065h,072h,077h,072h,069h,074h,065h,020h,028h,059h,02Fh,04Eh,029h,03Fh,000h  ;03B0h
   .DB 021h,080h,000h,036h,010h,0EBh,00Eh,00Ah,0CDh,079h,003h,0EBh,023h,035h,0C2h,037h  ;03C0h
   .DB 004h,023h,07Eh,0F6h,020h,0FEh,079h,0C2h,037h,004h,0CDh,074h,003h,0CDh,08Ah,003h  ;03D0h
   .DB 046h,069h,06Ch,065h,020h,063h,072h,065h,061h,074h,065h,000h,00Eh,016h,0CDh,079h  ;03E0h
   .DB 003h,03Ch,0CAh,016h,005h,0CDh,08Dh,003h,064h,00Dh,00Ah,052h,065h,063h,065h,069h  ;03F0h
   .DB 076h,065h,000h,001h,064h,000h,03Ah,045h,004h,0B7h,0CAh,010h,005h,032h,040h,004h  ;0400h
   .DB 03Ah,040h,004h,0C3h,06Ch,003h,0CDh,033h,004h,020h,066h,061h,069h,06Ch,02Eh,020h  ;0410h
   .DB 057h,072h,069h,074h,065h,020h,070h,072h,06Fh,074h,065h,063h,074h,03Fh,020h,044h  ;0420h
   .DB 069h,072h,020h,066h,075h,06Ch,06Ch,03Fh,000h,011h,080h,000h,01Ah,032h,042h,004h  ;0430h
   .DB 013h,0CDh,0DCh,006h,0DAh,0D0h,005h,0CDh,0EEh,006h,0DAh,05Eh,005h,0FEh,02Fh,0C2h  ;0440h
   .DB 056h,005h,01Bh,034h,03Eh,020h,0FEh,020h,0C2h,047h,005h,0CDh,0A3h,005h,001h,020h  ;0450h
   .DB 00Dh,021h,05Ch,000h,03Eh,02Fh,023h,005h,0CAh,076h,005h,0BEh,0C2h,066h,005h,036h  ;0460h
   .DB 020h,023h,005h,0C2h,06Fh,005h,070h,023h,00Dh,0C2h,076h,005h,011h,000h,007h,00Eh  ;0470h
   .DB 01Ah,0CDh,079h,003h,03Ah,044h,004h,047h,03Ch,0C0h,0CDh,030h,004h,04Dh,075h,073h  ;0480h
   .DB 074h,020h,073h,070h,065h,063h,069h,066h,079h,020h,02Fh,052h,020h,06Fh,072h,020h  ;0490h
   .DB 02Fh,053h,000h,0CDh,0DCh,006h,0D8h,001h,0A3h,005h,0C5h,0FEh,02Fh,0C2h,0B0h,006h  ;04A0h
   .DB 0CDh,0EEh,006h,0DAh,0B0h,006h,0FEh,052h,0CAh,0CAh,005h,0FEh,053h,0CAh,0CAh,005h  ;04B0h
   .DB 0FEh,043h,0C2h,097h,006h,0AFh,032h,045h,004h,0C9h,0E6h,001h,032h,044h,004h,0C9h  ;04C0h
   .DB 0CDh,033h,004h,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh  ;04D0h
   .DB 03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,00Dh,00Ah,058h,04Dh  ;04E0h
   .DB 04Fh,044h,045h,04Dh,020h,032h,02Eh,035h,020h,042h,079h,020h,04Dh,02Eh,020h,045h  ;04F0h
   .DB 062h,065h,072h,068h,061h,072h,064h,00Dh,00Ah,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh  ;0500h
   .DB 03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh,03Dh  ;0510h
   .DB 03Dh,03Dh,00Dh,00Ah,00Ah,055h,073h,061h,067h,065h,03Ah,020h,058h,04Dh,04Fh,044h  ;0520h
   .DB 045h,04Dh,020h,03Ch,066h,069h,06Ch,065h,06Eh,061h,06Dh,065h,03Eh,020h,03Ch,06Fh  ;0530h
   .DB 070h,074h,069h,06Fh,06Eh,020h,06Ch,069h,073h,074h,03Eh,00Dh,00Ah,020h,02Fh,052h  ;0540h
   .DB 020h,074h,06Fh,020h,072h,065h,063h,065h,069h,076h,065h,020h,06Fh,072h,020h,02Fh  ;0550h
   .DB 053h,020h,074h,06Fh,020h,073h,065h,06Eh,064h,00Dh,00Ah,020h,02Fh,043h,020h,072h  ;0560h
   .DB 065h,063h,065h,069h,076h,065h,020h,077h,069h,074h,068h,020h,063h,068h,065h,063h  ;0570h
   .DB 06Bh,073h,075h,06Dh,073h,020h,069h,06Eh,073h,074h,065h,061h,064h,020h,06Fh,066h  ;0580h
   .DB 020h,043h,052h,043h,00Dh,00Ah,000h,032h,09Eh,006h,0CDh,08Ah,003h,02Fh,026h,020h  ;0590h
   .DB 075h,06Eh,072h,065h,063h,06Fh,067h,06Eh,069h,07Ah,065h,064h,000h,0C3h,0B8h,006h  ;05A0h
   .DB 0CDh,08Ah,003h,04Ah,075h,06Eh,06Bh,000h,0CDh,033h,004h,020h,069h,06Eh,020h,063h  ;05B0h
   .DB 06Fh,06Dh,06Dh,061h,06Eh,064h,020h,06Ch,069h,06Eh,065h,000h,0CDh,08Ah,003h,042h  ;05C0h
   .DB 061h,064h,020h,076h,061h,06Ch,075h,065h,000h,0C3h,0B8h,006h,0CDh,0EEh,006h,0D8h  ;05D0h
   .DB 0CAh,0DCh,006h,0FEh,020h,0CAh,0DCh,006h,0FEh,009h,0CAh,0DCh,006h,0C9h,021h,042h  ;05E0h
   .DB 004h,07Eh,0B7h,037h,0C8h,01Ah,0E6h,07Fh,013h,035h,0FEh,00Dh,0C8h,0FEh,00Ah,0C9h  ;05F0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0600h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0610h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0620h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0630h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0640h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0650h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0660h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0670h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0680h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0690h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;06A0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;06B0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;06C0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;06D0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;06E0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;06F0h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0700h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0710h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0720h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0730h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0740h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0750h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0760h
   .DB 000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0770h
   .DB 000h
   .text    "PIP     "
   .DB                                              0C3h,04Fh,04Dh,000h,000h,000h,03Ah  ;0780h
   .DB 0E0h,0E1h,0E2h,0E3h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;0790h
   .DB 000h
   .text    "XM      "
   .DB                                              0C3h,04Fh,04Dh,000h,000h,000h,00Ch  ;07A0h
   .DB 0E4h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;07B0h
   .DB 000h
   .text    "MONITOR "
   .DB                                              0C3h,04Fh,04Dh,000h,000h,000h,001h  ;07A0h
   .DB 0EAh,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h,000h  ;07B0h
   .DB 0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h  ;07E0h
   .DB 0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h,0E5h  ;07F0h
											;BLOCK E5



;7B00	0EA0	EA
		.ORG	7B00H
		LXI	SP,STACK	;Assign NEW STACK in upper RAM!
		LXI	H,MAIN_MENU_FPON
		PUSH	H
		LXI	H,CHKSUM_RAM_SET
		PUSH	H
		JMP	JMP_ROM_LOW

		.end


;                       *********   *******    *********                        
;                       *********  *********   *********                        
;                       **         **     **   **                               
;                       **         **     **   **                               
;---------------------  *******    **     **   *******    --------------------- 
;---------------------  *******    **     **   *******    --------------------- 
;                       **         **     **   **                               
;                       **         **     **   **                               
;                       *********  *********   **                               
;                       *********   *******    **                               







;----------------------------------------------------------------------------------------------------; INSTRUCTION LIST REFERENCE
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>;
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<;
;----------------------------------------------------------------------------------------------------;

;DATA TRANSFER GROUP
;'Mnem.     'TC'Description                 'Notes                '
;'----------+--+----------------------------+---------------------'
; MOV r1,r2 | 5| r1 <- r2                   |r = A,B,C,D,E,H,L
; MOV r,M   | 7| r <- (HL)
; MOV M,r   | 7| (HL) <- r
; HLT       | 7| HALT
; MVI r,d   | 7| r <- d, Move Immediate data
; MVI M,d   | 7| (HL) <- d, Immediate data
; INR r     | 5| r <- r+1
; DCR r     | 5| r <- r-1
; INR M     |10| (HL) <- (HL)+1
; DCR M     |10| (HL) <- (HL)-1
; ADD r     | 4| A <- A+r
; ADC r     | 4| A <- A+r+CY                |Add with Carry
; SUB r     | 4| A <- A-r
; SBB r     | 4| A <- A-r-CY                |Subtract with Borrow
; ANA r     | 4| A <- A AND r
; XRA r     | 4| A <- A XOR r
; ORA r     | 4| A <- A OR r
; CMP r     | 4| A-r                        |Compare
; ADD M     | 7| A <- A+(HL)
; ADC M     | 7|
; SUB M     | 7|
; SBB M     | 7|
; ANA M     | 7|
; XRA M     | 7|
; ORA M     | 7|
; CMP M     | 7|
; ADI d     | 7| A <- A+d, ADD Immediate data
; ACI d     | 7|
; SUI d     | 7|
; SBI d     | 7|
; ANI d     | 7|
; XRI d     | 7|
; ORI d     | 7|
; CPI d     | 7| A-d, Z=1 if A=d, CY=0 if A>d
; RLC       | 4| Rotate A Left,  CY<-MSB      Only Carry Affected
; RRC       | 4| Rotate A Right, CY<-LSB      Only Carry Affected
; RAL       | 4| Rotate A Left  through Carry Only Carry Affected
; RAR       | 4| Rotate A Right through Carry Only Carry Affected
; JMP addr  |10| Jump Address
; JC  addr  |10| Jump on Carry
; JNC addr  |10| Jump on NOT Cary
; JZ  addr  |10| Jump on ZERO
; JNZ addr  |10| Jump on NOT ZERO
; JP  addr  |10| Jump on Positive (MSB=0)
; JM  addr  |10| Jump on Minus (MSB=1)
; JPE addr  |10| Jump on Parity Even (Parity bit =1)
; JPO addr  |10| Jump on Parity Odd (Parity bit =0)
; CALL addr |17| Call subroutine
; CC  addr  |11/17|   
; CNC addr  |11/17|   
; CZ  addr  |11/17|   
; CNZ addr  |11/17|   
; CP  addr  |11/17|   
; CM  addr  |11/17|   
; CPE addr  |11/17|   
; CPO addr  |11/17|      
; RET       |10| Return from subroutine
; RC        |5/11|   
; RNC       |5/11|   
; RZ        |5/11|   
; RNZ       |5/11|   
; RP        |5/11|   
; RM        |5/11|   
; RPE       |5/11|   
; RPO       |5/11|      
; RST n     |11| Restart to Vector n        | n=0,1,2,3,4,5,6,7
; IN  p     |10| A <- Port p, Input
; OUT p     |10| Port p <- A, Output
; LXI B,dd  |10| BC <- dd, Load Immediate data, 16 bit to Register Pair
; LXI D,dd  |10| DE <- dd
; LXI H,dd  |10| HL <- dd
; LXI SP,dd |10| SP <- dd
; PUSH B    |11| PUSH BC register pair to STACK
; PUSH D    |11| PUSH DE register pair to STACK
; PUSH H    |11| PUSH HL register pair to STACK
; PUSH PSW  |11| PUSH A,Flags register pair to STACK
; POP B     |10| POP BC register pair from STACK
; POP D     |10| POP DE register pair from STACK
; POP H     |10| POP HL register pair from STACK
; POP PSW   |10| POP A,Flags register pair from STACK
; STA addr  |13| (addr) <- A, Store A Direct
; LDA addr  |13| A <- (addr), Load A Direct
; XCHG      | 4| Exchange HL <> DE
; XTHL      |18| Exchange HL <> (SP), Exchange HL with Top of Stack
; SPHL      | 5| SP <- HL, Move HL to SP
; PCHL      | 5| PC <- HL, Move HL to PC
; DAD B     |10| HL <- HL+BC, Add 16 bit register pairs
; DAD D     |10| HL <- HL+DE, Add 16 bit register pairs
; DAD H     |10| HL <- HL+HL, Add 16 bit register pairs (16 bit arithmetic shift left)
; DAD SP    |10| HL <- HL+SP, Add 16 bit register pairs (only way to read SP)
; STAX B    | 7| (BC) <- A, Store A Indirect
; STAX D    | 7| (DE) <- A, Store A Indirect
; LDAX B    | 7| A <- (BC), Load A Indirect
; LDAX D    | 7| A <- (DE), Load A Indirect
; INX B     | 5| BC <- BC+1, Increment 16 bit register pair
; INX D     | 5| DE <- DE+1, Increment 16 bit register pair
; INX H     | 5| HL <- HL+1, Increment 16 bit register pair
; INX SP    | 5| SP <- SP+1, Increment 16 bit register
; DCX B     | 5| BC <- BC-1, Decrement 16 bit register pair
; DCX D     | 5| DE <- DE-1, Decrement 16 bit register pair
; DCX H     | 5| HL <- HL-1, Decrement 16 bit register pair
; DCX SP    | 5| SP <- SP-1, Decrement 16 bit register
; CMA       | 4| A <- /A, Complement Accumulator
; STC       | 4| Set Carry
; CMC       | 4| Complement Carry
; DAA       | 4| Decimal Adjust Accumulator
; SHLD addr |16| (addr) <- HL, Store HL Direct
; LHLD addr |16| HL <- (addr), Load HL Direct
; EI        | 4| Enable Interrupts
; DI        | 4| Disable Interrupts
; NOP       | 4| No Op


