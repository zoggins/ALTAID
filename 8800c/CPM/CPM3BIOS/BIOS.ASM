;****************************************************************************
;
; CP/M 3 VERSION (v1.0)
;    This is the Altair CP/M 2.2b BIOS ported to run under CP/M 3. This
;    BIOS provides full support of the CP/M 3 DEVICE utility which replaced
;    the IOBYTE feature used in CP/M 2.2. 
;
;    This BIOS can also be used as the loader BIOS with CPMLDR without
;    changes. During initialization, the BOOT code detects whether
;    is is running with CPMLDR or as part of CPM3.SYS.
;
;    Author: Mike Douglas
;
;    Ver     Date	  Description
;    ---   ---------	---------------------
;    1.0   3/21/2019	Original

;---------------------------------------------------------------------------
;
; CP/M 2.2b BIOS for MITS Altair 8800 and either the Altair 8" Floppy
;    Drive or the Altair 5.25" Mini Disk Floppy Drive. Conditional
;    assembly allows this BIOS to work for either type of drive.
;
;    This BIOS for CP/M 2.2 implements full track buffering on both reads
;    and writes to improve performance. Compared to the original CP/M 1.4
;    and 2.2 versions for the 8" Altair floppy, loading programs and other
;    sequential reads improve by more than 50%. Performance of virtually
;    all programs with substantial disk I/O improves by 25%-75%. And note
;    these speed comparisons were made with write verify enabled in this
;    BIOS compared to write verify off in the older CP/M versions.
;
;    The same disk layout as used with the original versions of CP/M 1.4 
;    and 2.2 is maintained for full backward and forward compatibility.
;
;    This version implements a disk select timeout feature (works only on
;    the 8" floppy drive) so if a non-present drive is selected, it will
;    eventually timeout. To still allow the operator time to insert a disk,
;    close the door, and wait for the several second drive enable one-shot
;    to expire, the default timeout is set to seven seconds. This value can
;    be patched in the SELTIME (select timeout) at offset 60h from the 
;    start of the BIOS.
;
;    Author: Mike Douglas
;
;    Ver     Date	  Description
;    ---   ---------	---------------------
;    2.0   11/1/2017	Complete rewrite from scratch. Previous versions were
;			created using Burcon CP/M from 1980 as the starting 
;			point which contains a lot of convoluted code I was 
;			never happy with. The update provides an additional
;			5%-20% speed improvement over previous versions of
;			CP/M 2.2b. The update also adds some new features:
;			  1) Works with Altair 8" or Altair Mini-Disk with
;			     conditional assembly.
;			  2) Disk select timeout (8" floppy only).
;			  3) Individual sectors can be bad within a track
;			     and the remaining sectors still accessed.
;			  4) To improve speed, checksum and stop byte vali-
;			     dation are done on the fly as the sector is read.
;			     (only Altair software I know of that does this)
;			  5) Smarter and faster error detection and recovery:
;			     Up to 12 retries per sector for all 32 sectors
;			     on a track and a possible track restore and
;			     re-seek all complete in less than 3 seconds.
;
;****************************************************************************
TRUE	equ	-1
FALSE	equ	not TRUE

VERSION	equ	10		;BIOS Version x.y
MINIDSK	equ	FALSE		;set TRUE for Altair Mini-Disk

;-----------------------------
;  8" floppy equates
;-----------------------------
   if NOT MINIDSK
BIOSLEN	equ	1900h		;length of BIOS for 8" floppy

NUMTRK	equ	77		;number of tracks on the disk
NUMSEC	equ	32		;sectors per track
DATATRK	equ	6		;1st data format track
SECMASK	equ	1Fh		;five bit sector numbers

BSH	equ	4		;allocation block shift factor
BLM	equ	0Fh		;allocation block mask
EXM	equ	0		;extent mask
DSM	equ	149		;max block number (150 blocks of 2K bytes)
DRM	equ	63		;max directory entry number (64 entries) 
AL0	equ	0C0h		;directory allocation block bits byte 0
AL1	equ	0		;directory allocation block bits byte 1
RESTRK	equ	2		;reserved tracks for boot image
CKS	equ	(DRM+1)/4	;directory check space
   endif

;-----------------------------
;  Mini Disk equates
;-----------------------------
   if MINIDSK
BIOSLEN	equ	1000h		;length of BIOS for mini disk

NUMTRK	equ	35		;number of tracks on the disk
NUMSEC	equ	16		;sectors per track
DATATRK	equ	4		;1st data format track
SECMASK	equ	0Fh		;four bit sector numbers

BSH	equ	3		;allocation block shift factor
BLM	equ	07h		;allocation block mask
EXM	equ	0		;extent mask
DSM	equ	61		;max block number (62 blocks of 1K bytes)
DRM	equ	31		;max directory entry number (32 entries) 
AL0	equ	80h		;directory allocation block bits byte 0
AL1	equ	0		;directory allocation block bits byte 1
RESTRK	equ	4		;reserved tracks for boot image
CKS	equ	(DRM+1)/4	;directory check space
   endif

;-----------------------------
; Common disk parameters
;-----------------------------
NUMDISK	equ	4		;four drives supported
CSECLEN	equ	128		;CP/M sector length
SSECLEN	equ	133		;Altair system sector length
DSECLEN	equ	136		;Altair data sector length
TSECLEN	equ	DSECLEN+1	;length of sector in track table

; System tracks (0-5) sector format

SYSTRK	equ	0		;offset of track number
SYSDATA	equ	3		;offset of 128 byte data payload
SYSSTOP	equ	131		;offset of stop byte (FFh)
SYSCSUM	equ	132		;offset of checksum

; Data tracks (6-76) sector format

DATTRK	equ	0		;offset of track number
DATSEC	equ	1		;offset of sector number
DATCSUM	equ	4		;offset of checksum
DATDATA	equ	7		;offset of 128 byte data payload
DATSTOP	equ	135		;offset of stop byte (FFh)

;-----------------------------
; Altair disk controller 
;-----------------------------
DRVSLCT	equ	08h		;drive select register (out)
cDSLCT	equ	80h		;deselect drive

DRVSTAT	equ	08h		;drive status register (in)
sENWD	equ	01h		;enter new write data flag
sMOVEOK	equ	02h		;OK to move head
sHDSTAT	equ	04h		;head status flag
sDSKEN	equ	08h		;disk is selected and enabled
sINTEN	equ	20h		;processor interrupts enabled
sTRACK0	equ	40h		;on track zero flag
sNRDA	equ	80h		;new read data available

DRVCMD	equ	09h		;drive command register (out)
cSTEPI	equ	01h		;step in
cSTEPO	equ	02h		;step out
cHDLOAD	equ	04h		;head load
cRESTMR	equ	04h		;restart motor-off timer (MINIDSK)
cHDUNLD	equ	08h		;head unload
cINTEN	equ	10h		;interrupt enable
cINTDIS	equ	20h		;interrupt disable
cHCSON	equ	40h		;reduce head current switch
cWRTEN	equ	80h		;write enable

DRVSEC	equ	09h		;drive sector position (in)
sNEWSEC	equ	01h		;new sector flag (sector true)

DRVDATA	equ	0Ah		;drive read/write data (in/out)

;-----------------------------
; 2SIO Serial Board 
;-----------------------------
SIO1CTL	equ	10h		;1st port on 2SIO board - control register
SIO1DAT	equ	11h		;1st port on 2SIO board - data register
SIO2CTL	equ	12h		;2nd port on 2SIO board - control register
SIO2DAT	equ	13h		;2nd port on 2SIO board - data register
SIORDRF	equ	01h		;read data register full flag
SIOTDRE	equ	02h		;transmit data register empty flag

;-----------------------------
; SIO Serial Board 
;-----------------------------
SIOCTL	equ	00h		;SIO board at 0 - control register
SIODAT	equ	01h		;SIO board at 0 - data register
ACRCTL	equ	06h		;SIO board at 6 - control register (cassette)
ACRDAT	equ	07h		;SIO board at 6 - data register (cassette)
SIORCV	equ	01h		;data received bit (inverted)
SIOXMT	equ	80h		;read to transmit (inverted)

;-----------------------------
; BDOS equates
;-----------------------------
GETCH	equ	1		;get character
PRINT	equ	9		;display string
OPEN	equ	15		;open file
READSEQ	equ	20		;read sequential
DMA	equ	26		;set address for file read
SETMULT	equ	44		;set multi-record I/O

;-----------------------------
; RAM equates
;-----------------------------
TPA	equ	100h
DFFCB	equ	5Ch
STACK	equ	100h
BDOS	equ	5
WBOOTV	equ	00h		;warm boot vector location
BDOSV	equ	05h		;bdos entry vector location

;-----------------------------
; Misc equates
;-----------------------------
NUMCDEV	equ	4		;number of character I/O devices
RDTRIES	equ	6		;read tries (must be <= 8)
VFTRIES	equ	4		;verify tries per write (must be <= 8)
WRTRIES	equ	3		;track writes to try

UNDEF	equ	0FFh		;undefined value
CR	equ	13		;ascii for carriage return
LF	equ	10		;ascii for line feed
EOF	equ	01Ah		;ctrl-z

;------------------------------
; External references
;------------------------------
	maclib	modebaud	;character I/O equates
	extrn	@civec,@covec,@aivec,@aovec,@lovec,@mxtpa

	cseg
;-----------------------------------------------------------------------------
;  BIOS Entry Jump Table
;-----------------------------------------------------------------------------
	jmp	boot		;cold start
wbJump	jmp	wBoot		;warm start
	jmp	conIst		;console input status
	jmp	conIn		;console character in (disk flush too)
	jmp	conOut		;console character out
	jmp	listOut		;list character out
	jmp	auxOut		;aux character out
	jmp	auxIn		;aux character in
	jmp	home		;move head to home position
	jmp	selDsk		;select disk
	jmp	setTrk		;set track number
	jmp	setSec		;set sector number
	jmp	setDma		;set dma address
	jmp	read		;read disk
	jmp	write		;write disk
	jmp	listOst		;list device output status
	jmp	secTran		;sector translate

; new CP/M 3 entry points

	jmp	conOst		;return console output status
	jmp	auxIst		;return aux device input status
	jmp	auxOst		;return aux device output status
	jmp	getDev		;return address of character device table
	jmp	devIni		;init character i/o devices
	jmp	getDrv		;return address of disk drive table
	jmp	multio		;set number of consec. sec. to read/write
	jmp	flush		;flush user [de]blocking buffers
	jmp	move		;copy memory to memory
	jmp	xmove		;set banks for next move
	jmp	selMem		;select memory bank
	jmp	setBnk		;set bank for next DMA
	jmp	userf		;reserved for me.
	jmp	wBoot
	jmp	wBoot		;reserved for DRI

; selTime - drive select timeout in seconds, at offset 60h

selTime	db	7		;drive select timeout in seconds

;----------------------------------------------------------------------------
; boot - Cold boot BIOS entry.
;----------------------------------------------------------------------------
boot	lxi	h,8000h		;assign console to 2SIO10 device
	shld	@civec
	shld	@covec

	lxi	h,4000h
	shld	@lovec		;assign printer to 2SIO12 device
	shld	@aivec		;assign aux to 2SIO12 device
	shld	@aovec

	lxi	h,boot		;a 16 bit immediate is required for RMAC
	mvi	a,3Fh		;A=MSB of last page in lower 16K
	cmp	h		;are we in lower 16K?
	jc	stdBios		;no,running as a standard BIOS

	mvi	a,03h		;reset 2SIO ports
	out	SIO1CTL
	out	SIO2CTL

	mvi	a,11h		;select 8N2
	out	SIO1CTL
	out	SIO2CTL

	ret			;return to BDOS

stdBios	call	dispMsg		;display sign-on message
	db	CR,LF,'CP/M 3 BIOS v'
	db	VERSION/10+'0', '.', VERSION mod 10+'0'
	db	' for Altair 8" Floppy',CR,LF,0

				;fall into warm boot

;----------------------------------------------------------------------------
; wBoot - Warm boot BIOS entry.
;----------------------------------------------------------------------------
wBoot	lxi	sp,STACK	;init stack pinter
	mvi	a,JMP		;8080 "jump" opcode
	sta	WBOOTV		;store in 1st byte of warm boot vector
	sta	BDOSV		;and 1st byte of BDOS entry vector

	lxi	h,wbJump	;get the warm boot jump address
	shld	WBOOTV+1	;and put it after the jump

	lhld	@mxtpa		;BDOS entry address
	shld	BDOSV+1		;put it after the jump opcode

	xra	a		;zero the extent
	sta	fcbCcp+15
	lxi	h,0		;start at beginning of file
	shld	fcbNr

	lxi	d,fcbCcp	;DE->FCB to open CCP.COM
	mvi	c,OPEN
	call	BDOS
	ora	a		;test for error
	jnz	openErr

	lxi	d,TPA		;set load address to the TPA (0100h)
	mvi	c,DMA
	call	BDOS

	mvi	e,128		;read up to 16K
	mvi	c,SETMULT	;set multiple-record read
	call	BDOS

	lxi	d,fcbCcp	;DE->FCB to read CCP.COM
	mvi	c,READSEQ	;read CCP.COM into memory
	call	BDOS
	
	cpi	1		;only legal exit = past end of file
	jnz	readErr
	jmp	TPA		;run the CCP	

; Warm start error messages

openErr	call	dispMsg
	db	CR,LF,'BIOS can''t open CCP.COM ',0
	jmp	errWt

readErr	call	dispMsg
	db	CR,LF,'BIOS has bad sector in CCP.COM ',0

errWt	call	conIn		;wait for operator response
	jmp	wBoot		;try again

fcbCcp	db	1,'CCP     ','COM',0,0,0,0
	dw	0,0,0,0,0,0,0,0
fcbNr	db	0,0,0

;--------------------------------------------------------
; BIOS entry points added in CP/M 3
;--------------------------------------------------------
getDev	lxi	h,charTbl	;return HL->character I/O table
	ret

getDrv	lxi	h,drvTbl	;return HL->drive table
	ret

move	ldax	d		;move BC bytes from DE to HL
	mov	m,a
	inx	h
	inx	d
	dcx	b
	mov	a,b
	ora	c
	jnz	move
	ret

flush	xra	a		;use only our flush logic
	ret

devIni
multio
xmove
selMem
setBnk
userf	ret			;no action for these entry points

;------------------------------------------------------------------------------
; BIOS Character I/O routines entered from the BIOS jump table
;------------------------------------------------------------------------------

; conIn - Console input BIOS entry point. Console input is used as an
;    idle indicator and as a good time to flush a dirty track buffer,
;    unload the head, and invalidate the track buffer.

conIn	mvi	a,UNDEF		;invalidate track buffer
	sta	bfDrive
	call	ckFlush		;flush the track if needed
	ei			;restore interrupts

   if NOT MINIDSK
	mvi	a,cHDUNLD	;unload head
	out	DRVCMD
   endif

	lhld	@civec		;HL=console input device vector	
	jmp	inScan		;do input scan

conOut	lhld	@covec		;HL=console output device vector	
	jmp	outScan		;do output scan

conIst	lhld	@civec		;HL=console input device vector	
	jmp	istScan		;do input status scan

conOst	lhld	@covec		;HL=console output device vector	
	jmp	ostScan		;do output status scan

auxIn	lhld	@aivec		;HL=aux input device vector	
	jmp	inScan		;do input status scan

auxOut	lhld	@aovec		;HL=aux output device vector	
	jmp	outScan		;do output status scan

auxIst	lhld	@aivec		;HL=aux input device vector	
	jmp	istScan		;do input status scan

auxOst	lhld	@aovec		;HL=aux output device vector	
	jmp	ostScan		;do output scan

listOut	lhld	@lovec		;HL=list output device vector	
	jmp	outScan		;do output status scan

listOst	lhld	@lovec		;HL=list output device vector	
	jmp	inScan		;do output scan

;------------------------------------------------------------------------------
; inScan - Input character scan of all selected devices
;------------------------------------------------------------------------------
inScan	push	h		;save device vector
	mvi	b,0		;B=current device number

inLoop	dad	h		;shift device vector left
	mvi	a,0		;assume device not ready
	cc	devIS		;call device input status routine
	ora	a		;this device have character?
	jnz	inGet		;yes, go get the character

	inr	b		;move to next device number
	mov	a,h		;any more device bits set?
	ora	l
	jnz	inLoop		;yes, continue

	pop	h		;restore original device vector
	jmp	inScan		;loop until we find character

inGet	pop	h		;discard saved vector
	call	doDevIo
	dw	sio1In
	dw	sio2In
	dw	sioIn
	dw	acrIn

;------------------------------------------------------------------------------
; istScan - Input status scan of all selected devices
;------------------------------------------------------------------------------
istScan	mvi	b,0		;B=current device number

isLoop	dad	h		;shift device vector left
	mvi	a,0		;assume device not ready
	cc	devIS		;call device input status routine
	ora	a		;if any device is ready, return true
	rnz

	inr	b		;move to next device number
	mov	a,h		;any more device bits set?
	ora	l
	jnz	isLoop		;yes, continue

	ret			;return with zero (no character ready)

devIS	call	doDevIo
	dw	sio1IS
	dw	sio2IS
	dw	sioIS
	dw	acrIS

;------------------------------------------------------------------------------
; outScan - Output character scan of all selected devices.
;------------------------------------------------------------------------------
outScan	mvi	b,0		;B=current device number

outLoop	dad	h		;shift device vector left
	jnc	osNxDev		;not selected, do next device

osWait	call	devOS		;call device output status routine
	ora	a		;is device ready?
	jz	osWait		;wait until device ready

	call	devOut		;output character to device
	
osNxDev	inr	b		;move to next device number
	mov	a,h		;any more device bits set?
	ora	l
	jnz	outLoop		;yes, continue
	ret

devOut	call	doDevIo
	dw	sio1Out
	dw	sio2Out
	dw	sioOut
	dw	acrOut

;------------------------------------------------------------------------------
; ostScan - Output status scan of all selected devices.
;------------------------------------------------------------------------------
ostScan	mvi	b,0		;B=current device number

osLoop	dad	h		;shift device vector left
	mvi	a,0FFh		;assume device is ready to xmit
	cc	devOS		;call device output status routine
	ora	a		;is device ready?
	rz			;if any device not ready, return false

	inr	b		;move to next device number
	mov	a,h		;any more device bits set?
	ora	l
	jnz	osLoop		;yes, continue

	ori	0FFh		;all devices ready, return true
	ret

devOS	call	doDevIo
	dw	sio1OS
	dw	sio2OS
	dw	sioOS
	dw	acrOS

;---------------------------------------------------------------------------
; doDevIo - Jump to address specified in the jump table at (SP), indexed
;   by the device number in B.
; Clobbers D,E
;--------------------------------------------------------------------------- 
doDevIo	mov	a,b		;A=device number
	ani	3		;limit to four devices (shouldn't happen)
	add	a		;two bytes per table entry
	mov	e,a		;form DE=table offset
	mvi	d,0
	xthl			;HL=table address from stack, save HL
	dad	d		;HL->address in table based on device number

	mov	e,m		;E=lsb of where to jump
	inx	h
	mov	d,m		;D=msb of where to jump
	xchg			;HL->where to jump

	xthl			;jump address on stack, restore HL
	mov	a,c		;character to send in A for out routines
	ret			;jump to address specified in table

;---------------------------------------------------------------------------
; Input status routines. Return FFh if character ready, else zero 
;---------------------------------------------------------------------------
sio1IS in	SIO1CTL		;read 2SIO #1 status/control register
	ani	SIORDRF		;data present?
	rz			;no, return zero

	mvi	a,0FFh		;else return FFh
	ret

sio2IS	in	SIO2CTL		;read 2SIO #2 status/control register
	ani	SIORDRF		;data present?
	rz			;no, return zero

	mvi	a,0FFh		;else return FFh
	ret

sioIS	in	SIOCTL		;read SIO status/control register
	xri	0FFh		;convert to positive logic
	ani	SIORCV		;data present?
	rz			;no, return zero

	mvi	a,0FFh		;else return FFh
	ret

acrIS	in	ACRCTL		;read ACR (SIO) control register
	xri	0FFh		;convert to positive logic
	ani	SIORCV		;data present?

	rz			;no, return zero
	mvi	a,0FFh		;else return FFh
	ret

;---------------------------------------------------------------------------
; Character input routines (character ready assumed)
;---------------------------------------------------------------------------
sio1In	in	SIO1DAT		;read and return the character
	ret

sio2In	in	SIO2DAT		;read and return the character
	ret

sioIn	in	SIODAT		;read and return the character
	ret

acrIn	in	ACRDAT		;read and return the character
	ret

;---------------------------------------------------------------------------
; Output status routines. Return FFh if ready to send, else zero 
;---------------------------------------------------------------------------
sio1OS	in	SIO1CTL		;read 2SIO #1 status/control register
	ani	SIOTDRE		;0=busy
	rz			;not ready, return 0

	mvi	a,0FFh		;else return FFh
	ret

sio2OS	in	SIO2CTL		;read 2SIO #2 status/control register
	ani	SIOTDRE		;0=busy
	rz			;not ready, return 0

	mvi	a,0FFh		;else return FFh
	ret
 
sioOS	in	SIOCTL		;read SIO status/control register
	xri	0FFh		;convert to positive logic
	ani	SIOXMT		;0=busy
	rz			;not ready, return 0

	mvi	a,0FFh		;else return FFh
	ret

acrOS	in	ACRCTL		;read SIO status/control register
	xri	0FFh		;convert to positive logic
	ani	SIOXMT		;0=busy
	rz			;not ready, return 0

	mvi	a,0FFh		;else return FFh
	ret

;---------------------------------------------------------------------------
; Character output routines (transmit ready assumed)
;---------------------------------------------------------------------------
sio1Out	out	SIO1DAT		;send character
	ret

sio2Out	out	SIO2DAT		;send character
	ret

sioOut	out	SIODAT		;send character
	ret

acrOut	out	ACRDAT		;send character
	ret

;----------------------------------------------------------------------------
; selDsk - Select Disk BIOS entry. C contains the disk number to select.
;    Validate the disk number and return a pointer to the disk parameter
;    header in HL. Zero is returned in HL for invalid drive number. The
;    disk number is stored in drvNum. No drive activity takes place.
;
; On Entry
;    C = disk number to select
;
; On Exit
;    drvNum = disk number to select
;    Success: HL->DPH for this drive. Error: HL=0
;    Clobbers A,D,E,H,L
;----------------------------------------------------------------------------
selDsk	mov	l,c		;L=drive number
	mvi	h,0		;HL=drive number
	dad	h		;HL=drive number*2 (16 bit values in table)
	lxi	d,drvTbl	;DE->drive table
	dad	d		;HL->DPH pointer for current drive

	mov	a,m		;A=LSB of DPH pointer
	inx	h
	mov	h,m		;H=MSB of DPH pointer
	mov	l,a		;HL=DPH pointer

	ora	h		;if HL=0, invalid disk
	rz

	mov	a,c		;update drvNum with drive number
	sta	drvNum
	ret

;----------------------------------------------------------------------------
; home - Home BIOS entry. Set trkNum to zero. No drive activity takes place.
;
; On Entry
;
; On Exit
;    trkNum set to zero
;    Clobbers A
;----------------------------------------------------------------------------
home	xra	a		;set trkNum to zero
	sta	trkNum
	ret

;----------------------------------------------------------------------------
; setTrk - Set Track BIOS entry. C contains the desired track number.
;    The track number is saved in trkNum for later use.
;
; On Entry
;    C = track number to set
;
; On Exit
;    trkNum = track specified in C
;    Clobbers A
;----------------------------------------------------------------------------
setTrk	mov	a,c		;save track number passed in C
	sta	trkNum
	ret

;----------------------------------------------------------------------------
; setSec - Set Sector BIOS entry. C contains the 1-indexed CPM physical 
;   sector. Saved in secNum as a zero-indexed value.  
;
; On Entry
;    C = 1-indexed sector number to set
;
; On Exit
;    secNum = zero-indexed sector number
;    Clobbers A
;----------------------------------------------------------------------------
setSec	mov	a,c		;A=1-indexed sector number
	dcr	a		;convert to zero indexed
	sta	secNum
	ret

;----------------------------------------------------------------------------
; setDma - Set DMA BIOS entry. BC contains the address for reading or
;    writing sector data for subsequent I/O operations. The address is
;    stored in dmaAddr.
;
; On Entry
;    BC = I/O buffer address
;
; On Exit
;    dmaAddr = address specified in BC
;    Clobbers H,L
;----------------------------------------------------------------------------
setDma	mov	h,b		;save buffer address passed in BC
	mov	l,c
	shld	dmaAddr
	ret
	
;----------------------------------------------------------------------------
; secTran - Sector translation BIOS entry. Convert zero-indexed logical 
;    sector number in BC to a CPM one-indexed physical sector number in HL
;    based on the translate table passed in DE.
;
; On Entry
;    BC = logical sector number
;    DE->sector translate table for selected drive
;
; On Exit
;    HL = physical sector number
;    Clobbers B,D,E,H,L
;----------------------------------------------------------------------------
secTran	xchg			;HL->translate table
	mvi	b,0		;make sure msb of sector is zero
	dad	b		;HL->translated sector num
	mov	l,m		;L=lsb of sector
	mov	h,b		;H=0=msb of sector
	ret

;----------------------------------------------------------------------------
; read - Read sector BIOS entry. Read one sector using the drvNum, 
;    trkNum, secNum, and dmaAddr previously specified.
;
; On Entry
;    drvNum = drive to read from
;    trkNum = track to read from
;    secNum = sector number to read (0-31)
;    dmaAddr = address of buffer to read into
;
; On Exit
;    If read successful
;	sector read into (dmaAddr)
;	HL = (dmaAddr) + CSECLEN
;	A=0, Z flag set true
;    Else
;	A=1, Z flag set false
;    Interrupts enabled
;    Clobbers all
;----------------------------------------------------------------------------
read	call	ckFlush		;flush track buffer if needed
	ei			;re-enable interrupts

	call	readTrk		;fill track buffer if needed
	ei			;re-enable interrupts
	jnz	exitDio		;track read error, exit
	
	call	movRead		;move sector to (dmaAddr)

exitDio	mvi	a,0		;if zero is true, return zero
	rz

	inr	a		;else return A<>0, Z false
	ret
	
;----------------------------------------------------------------------------
; write - Write sector BIOS entry. Write one sector using the drvNum,
;    trkNum, secNum and dmaAddr specified. 
;
; On Entry
;    drvNum = drive to write
;    trkNum = track to write
;    secNum = sector number to write (0-31)
;    dmaAddr = address of buffer to write from
;
; On Exit
;    If successful
;	sector written to trkBuf from (dmaAddr)
;	A=0, Z flag set true
;    Else
;	A=1, Z flag set false
;    Interrupts enabled
;    Clobbers all
;----------------------------------------------------------------------------
write	call	ckFlush		;fluhsh track buffer if needed
	ei			;re-enable interrupts

	call	readTrk		;fill track buffer if needed
	ei			;re-enable interrupts
	jnz	exitDio		;track read error, exit

	jmp	movWrt		;move (dmaAddr) to sector and exit
	
;----------------------------------------------------------------------------
; movRead - Move sector data from track buffer to (dmaAddr) for a
;    CPM read request
;
; On Entry
;    trkNum = track to read
;    secNum = sector number to read (0-31)
;    dmaAddr = address of buffer to read into
;
; On Exit
;    If successful (good sector)
;	sector data moved to (dmaAddr), Z flag set true
;	HL = (dmaAddr) + CSECLEN
;    Else (sector flagged as bad)
;	Z flag set false
;    Clobbers all
;----------------------------------------------------------------------------
movRead	call	altSkew		;secNum to hard sector in A
	call	dSecAdr		;HL->sector in trkBuf
	
	mov	a,m		;A=sector valid flag
	ora	a
	rnz			;bad sector, return error
	
	lxi	d,DATDATA+1	;DE=offset to data portion of sector
	lda	trkNum		;on a data track?
	cpi	DATATRK
	jnc	mrMove		;yes, data track (DE already correct)
	
	lxi	d,SYSDATA+1	;DE=offset to data portion of sector

mrMove	dad	d		;HL->data portion of sector
	xchg			;DE->data portion of sector
	lhld	dmaAddr		;HL->destination for data
	mvi	b,CSECLEN	;B=number of bytes to move
	
mrLoop	ldax	d		;move sector from trkBuf to (dmaAddr) 
	mov	m,a
	inx	h		;increment pointers
	inx	d
	dcr	b		;loop count
	jnz	mrLoop 

	ret			;exit with zero	status
	
;----------------------------------------------------------------------------
; movWrt - Move sector data from (dmaAddr) to track buffer and create 
;    metadata for the sector for a CPM write request.
;
; On Entry
;    trkNum = track to write
;    secNum = sector number to write (0-31)
;    dmaAddr = address of buffer to write from
;
; On Exit
;    Sector data moved to trkBuf from (dmaAddr)
;    A=0, Z status true
;    Clobbers all
;----------------------------------------------------------------------------
movWrt	call	altSkew		;secNum to hard sector in A
	call	dSecAdr		;HL->sector in trkBuf
	
	mvi	m,0		;flag sector as good
	
	inx	h		;HL->track
	lda	trkNum		;A=track
	ori	80h		;set sync bit
	mov	m,a		;set track in sector
	
	inx	h		;HL->byte after track
	cpi	DATATRK+80h	;on a system or data track?
	jnc	wDatTrk		;data track 6-76 (mini disk 4-34)

; Create Altair sector for system tracks 0-5 (mini disk 0-3)

	xra	a		;put 0100h (16 bit) at offset 1,2
	mov	m,a
	inx	h		;HL->offset 2
	inr	a		;A=1
	mov	m,a
	
	inx	h		;HL->128 byte CPM sector in Altair sector
	call	mwMove		;move (dmaAddr) to sector in trkBuf
	
	mvi	m,0FFh		;offset 131 is stop byte (0FFh)
	
	inx	h		;offset 132 is checksum
	mov	m,b		;store checksum at offset 132
	
	jmp	mwExit		;exit

; wDatTrk- Create Altair sector for tracks 6-76 (mindisk 4-34)

wDatTrk	lda	secNum		;A=sector before Altair skew
	mov	m,a		;store Altair logical sector number
	
	inx	h		;HL->offset 2 in sector
	xra	a		;store zero at offsets 2-6
	mov	m,a		;offset 2
	
	inx	h
	mov	m,a		;zero at offset 3
	
	inx	h
	push	h		;save address of offset 4 = checksum
	
	inx	h
	mov	m,a		;zero at offset 5
	
	inx	h
	mov	m,a		;zero at offset 6
	
	inx	h		;HL->128 byte CPM sector in Altair sector
	call	mwMove		;move (dmaAddr) to sector in trkBuf

	mvi	m,0FFh		;offset 135 is stop byte (0FFh)
	
	pop	h		;HL->checksum byte in Altair sector
	mov	m,b		;store the checksum
	
; mwExit - set dirty flag true, return success status

mwExit	mvi	a,0FFh		;set dirty flag true
	sta	bfDirty
	
	xra	a		;return success status
	ret

;------------------------------------------------------------------------------
; mwMove - Move sector buffer (128 bytes) from (dmaAddr) to (HL) as part
;   of a CPM write command. Compute checksum on all bytes moved and return
;   the checksum in B.
;
; On Entry
;    HL->destination sector in trkBuf
;    dmaAddr = address of buffer to move from
;
; On Exit
;    128 bytes moved from (dmaAddr) to (HL)
;    HL = HL + 128
;    B = checksum of the 128 bytes moved
;    Clobbers all
;------------------------------------------------------------------------------
mwMove	xchg			;DE->destination CPM sector in trkBuf
	lhld	dmaAddr		;HL->source buffer
	lxi	b,CSECLEN	;B=checksum (0), C=128 byte count
	
mwLoop	mov	a,m		;move from (HL) to (DE)
	stax	d
	
	add	b		;add byte to checksum
	mov	b,a
	
	inx	d		;increment both pointers
	inx	h
	dcr	c		;decrement character count
	jnz	mwLoop		;loop until count = 0
	
	xchg			;return with buffer pointer in HL	
	ret
	
;----------------------------------------------------------------------------
; altSkew - Perform Altair skew on the sector number in secNum and return
;    the result in A. The skew is based on the track as:
;
;	Tracks 0-5, secOut = secIn
;	Tracks 6-76, secOut = (secIn * 17) MOD 32
;
;    The skew computation for tracks 6-76 is implemented as:
;	secOut = secIn if secIn is even
;	secOut = secIn XOR 10h if secIn is odd
;
; On Entry
;    trkNum = current track
;    secNum = sector number (0-31)
;
; On Exit
;    A = sector number after Altair skew
;    Clobbers A,B
;----------------------------------------------------------------------------	
altSkew	equ	$

   if MINIDSK
	lda	secNum		;no skewing done for mini disk
	ret
   endif

   if NOT MINIDSK
	lda	trkNum		;on a data track?
	cpi	DATATRK
	lda	secNum		;A=unmodified sector number
	rc			;system track, no change to sector
	
	mov	b,a		;save secNum in B
	rrc			;test for even/odd
	mov	a,b		;restore secNum in A
	rnc			;return with secNum if even
	
	xri	10h		;else translate as in comments above
	ret
   endif
		
;----------------------------------------------------------------------------
; readTrk- read full track into track buffer if the requested 
;    drive (drvNum) or track (trkNum) does not match the buffered
;    drive (bfDrive) or buffered track (bfTrack). The status byte
;    at the start of each sector is set to zero if the sector is
;    good, non-zero if the sector couldn't be read.
;
; On Entry
;    drvNum = drive to read
;    trkNum = track to read
;    bfDrive = drive from which trkBuf was filled
;    bfTrack = track from which trkBuf was filled
;
; On Exit
;    trkNum on drvNum read into trkBuf
;    Zero true if track read or already there, zero false otherwise
;    Clobbers all
;----------------------------------------------------------------------------
readTrk	lhld	bfDrive		;L=buffered drive, H=buffered track

	lda	drvNum		;A=requested drive
	cmp	l		;same drive buffered?
	jnz	rtNew		;drive doesn't match, need a new buffer

	lda	trkNum		;A=requested track
	cmp	h		;same track buffered?
	rz			;yes, already have this buffer

; rtNew - New track needs to be read

rtNew	mvi	a,UNDEF		;invalidate buffered data
	sta	bfDrive
	
	call	dSelDrv		;select drive, load head
	rnz			;drive select failed, exit with error

	mvi	a,2		;init restore/seek try counter
	sta	skRetry
	
rtRtry	call	dSeek		;seek to trkNum (disables interrupts)
	rnz			;a restore was required and failed
	
	call	initTrk		;init all sectors to "bad" (not read)
	lxi	d,RDTRIES*NUMSEC*256 + NUMSEC
				;D=max sector reads = RDTRIES revolutions
				;E=sectors remaining to fill

; Read sector loop - All sectors are originally marked "bad" (unread) and
;    a sector counter in E is started at NUMSEC (a full track of sectors).
;    Sectors are read until sector counter E reaches zero or the total
;    reads counter in D reaches zero (RDTRIES revolutions). The 1st time
;    the total reads counter reaches zero, a restore and re-seek is 
;    performed in case we're on the wrong track. The second time it reaches
;    zero, the bad sectors remain marked as bad in the track buffer.

; Within dRead, 152 cycles execute after the last byte is read until we're
;    executing the instruction here following the dRead call. An additional
;    78 cycles are executed here until we're in dNxtSec looking for sector
;    true. This totals 230 cycles (115us) from the last byte read until
;    we're hunting for sector true. Allowing for 200us of index alignment
;    error and 10% speed variance, we have at least 180us to get this done.

; For the Mini Disk, a full track is 16 sectors instead of 32 and the time
;    available after a sector is read is well over 1000us, so timing is not
;    an issue.

rdLoop	call	dNxtSec		;(17) wait for next sector, HL->sector buf
	mov	a,m		;A=sector flag byte
	ora	a		;this sector already read?
	jz	rdNext		;yes, skip it

	push	h		;save sector address
	inx	h		;HL->1st read location in sector
	call	dRead		;read the sector
	pop	h		;(10) restore sector address in HL
	jnz	rdNext		;(10) read error

	xra	a		;(4) set sector flag to zero (good sector)
	mov	m,a		;(7) 

	dcr	e		;(5) decrement sectors left to fill
	jz	rtExit		;(10) all sectors read without error

rdNext	dcr	d		;(5) decrement total sectors counter
	jnz	rdLoop		;(10) go read next sector

	lxi	h,skRetry	;HL->seek retry counter
	dcr	m		;decrement the counter
	jz	rtExit		;re-seek(s) done, we're finished

	call	dRestor		;restore to track zero
	jz	rtRtry		;start over after the re-seek

	ret			;else, exit with error

rtExit	lhld	drvNum		;L=drvNum, H=trkNum
	shld	bfDrive		;set the buffered drive and track values
	ret			;exit with Z flag true

;----------------------------------------------------------------------------
; initTrk - Set the flag byte in each sector in trkBuf to "bad" to
;    indicate none of the sectors have been read.
;
; On Entry
;
; On Exit
;    All sectors in trkBuf marked "bad" (unread)
;    Clobbers all
;----------------------------------------------------------------------------
initTrk	lxi	b,UNDEF*256 + NUMSEC;  B=UNDEF (bad), C=NUMSEC
	lxi	h,trkBuf	;HL->track buffer
	lxi	d,TSECLEN	;DE=length of each sector in track buffer

itLoop	mov	m,b		;mark sector as bad (not read)
	dad	d		;HL->next sector in trkBuf
	dcr	c		;repeat for all sectors
	jnz	itLoop
	
	ret

;----------------------------------------------------------------------------
; ckFlush - Check if track buffer should be flushed. This
;    function must be called before any drive selection or seek
;    operation. The buffer is flushed if the bfDirty flag is
;    set and the drive or track number are different than the
;    buffered track.
;
; On Entry
;    Drive still selected and on same track as in trkBuf
;    drvNum, trkNum updated for the new I/O call
;
; On Exit
;    Zero true for no error, zero false if write error occured
;    Clobbers all
;----------------------------------------------------------------------------
ckFlush	lda	bfDirty		;see if track buffer is dirty
	ora	a
	rz			;no, exit with Z set

	lhld	bfDrive		;L=buffered drive, H=buffered track
	lda	drvNum		;A=requested drive number
	cmp	l		;same drive?
	jnz	wrtTrk		;no, flush

	lda	trkNum		;A=requested track number
	cmp	h		;same track?
	rz			;yes, no need to flush

wrtTrk	equ	$

   if MINIDSK
	mvi	a,cRESTMR	;restart motor timeout
	out	DRVCMD
 
	lda	curDrv		;make sure current drive still enabled
	out	DRVSLCT
   endif

	mvi	e,cWRTEN	;E=write command, normal head current
	mvi	a,42		;tracks 0-42 are normal head current
	cmp	h		;compare 42 - current track
	jnc	wrtInit		;track is 0-42, E is correct
	
	mvi	e,cWRTEN+cHCSON	;E=write command with reduced current
	
wrtInit	xra	a		;clear buffer dirty flag
	sta	bfDirty

	mvi	a,WRTRIES	;initialize write retry count
	sta	wrRetry

	di			;disable interrupts

; rtryWrt - Write track retry entry point. Write a full track starting
;    with any sector. Don't write sectors that are flagged as bad.

rtryWrt	mvi	d,NUMSEC	;D=count of sectors to write
	xra	a		;zero the count of sectors actually written
	sta	secCnt

wrtLoop	call	dNxtSec		;(78) wait for next sector, HL->sector buf
	mov	a,m		;(7) check sector flag
	ora	a		;(4) 
	jnz	wrtNext		;(10) skip if bad or previously verified
	
	mov	a,e		;(5) issue write command to drive
	out	DRVCMD		;(10) at 114 cycles 57us
	
	inx 	h		;point to 1st byte of sector
	call	dWrite		;write the sector

	lxi	h,secCnt	;count number of sectors written	
	inr	m

wrtNext	dcr	d		;decrement write sector count
	jnz	wrtLoop		;loop until all sectors processed

	call	dNxtSec		;force a 1 sector delay for trim erase
	mvi	d,NUMSEC*VFTRIES   ;D=maximum number of sector reads
	
vfyLoop	call	dNxtSec		;wait for next sector, HL->sector buf
	mov	a,m		;test sector flag, only good and
	ora	a		;   not previously verified sectors
	jnz	vfyNext		;   are verified
		
	push	h		;save pointer to sector flag
	inx	h		;point to 1st byte of sector
	call	dVerify		;verify the sector
	pop	h		;HL->sector flag for current sector
	jnz	vfyNext		;didn't match
	
	mvi	m,UNDEF		;set flag to indicate sector is verified

	lxi	h,secCnt	;HL->sectors left to verify
	dcr	m		;decrement the count
	rz			;track verified, exit

vfyNext	dcr	d		;decrement sector count
	jnz	vfyLoop		;loop until all verify tries expired

; Read re-tries expired, decrement the write retry count and write again

	lxi	h,wrRetry	;decrement the write retry counter
	dcr	m
	jnz	rtryWrt		;retry starting with the write

	call	dispMsg
	db	CR,LF,'Delayed Write Error', CR, LF, 0

	inr	a		;dispMsg returns zero
	ret			;exit with 1 for error
	
;------------------------------------------------------------------------------
; dispMsg - Display the null-terminated string following the dispMsg call
;
; On Entry SP->message
;
; On Exit
;    Clobbers C,H,L
;------------------------------------------------------------------------------
dispMsg	pop	h		;HL->string

dmLoop	mov	a,m		;A=next character
	inx	h		;bump string pointer
	ora	a		;null terminator?
	jz	dmExit		;yes, exit

	mov	c,a		;conOut needs character in C
	push	h		;preserve character pointer
	call	conOut		;send the character
	pop	h
	jmp	dmLoop

dmExit	pchl			;return past message string


;****************************************************************************
;
; Altair disk I/O routines
;
;****************************************************************************

;---------------------------------------------------------------------------
; dSelDrv - Select the drive specified in drvNum and load the head.
;    Save the track (curTrk) the current drive is on into the track table,
;    load curTrk for the new drive from the track table.
;
; On Entry
;    drvNum = drive to be selected
;    curDrv = drive currently selected
;    selTime = timeout in seconds to wait for drive select
;    curTrk = track the current drive is on
;
; On Exit
;    Drive selected and head loaded
;    curDrv = drvNum
;    curTrk = track the newly selected drive is on
;    Zero true for success, zero false for error
;    Clobbers all
;---------------------------------------------------------------------------
dSelDrv	equ	$

   if MINIDSK
	mvi	a,cRESTMR	;restart motor timeout
	out	DRVCMD
   endif

	lxi	h,curDrv	;HL->currently selected drive
	mov	e,m		;E=currently selected drive

	lda	drvNum		;A=desired drive
	mov	c,a		;save new drive in C
	cmp	e		;same or different drive?
	jnz	dNewDrv		;different, go select a new drive

	in	DRVSTAT		;drive still selected and enabled?
	ani	sDSKEN
	jz	dSelExt		;yes, exit

	jmp	dReSel		;otherwise, re-select the drive

; dNewDrv - Save current track (curTrk) for the current drive to the track
;    table, load curTrk for the new drive from the track table

dNewDrv	mov	m,c		;save new drive in curDrv
	
	mvi	d,0		;DE=currently selected drive
	lxi	h,trkTbl	;HL->track table
	dad	d		;HL->trkTbl entry for current drive

	lda	curTrk		;A=track current drive is on
	mov	m,a		;save it in the track table

	lxi	h,trkTbl	;HL->track table
	mov	e,c		;DE=offset in trkTbl for new drive
	dad	d		;HL->trkTbl for new drive

	mov	a,m		;A=track new drive is on
	sta	curTrk		;update current track

; Select the new drive with the timeout specified in selTime

dReSel	lxi	h,selTime	;move select timeout in seconds into B
	mov	b,m

dSelLp1	lxi	h,25641		;HL=count of 78 cycle loops for 1 second

dSelLp	mvi	a,cDSLCT	;(7) deselect a possibly attached drive
	out	DRVSLCT		;(10)

	mov	a,c		;(5) A=drive to select
	out	DRVSLCT		;(10) select it

	in	DRVSTAT		;(10) did drive select work?
	ani	sDSKEN		;(7)
	jz	dSelExt		;(5) yes, exit

	dcx	h		;(5) decr 1 sec counter
	mov	a,h		;(5) test HL counter for zero
	ora	l		;(4)
	jnz	dSelLp		;(10) loop for one second

	dcr	b		;decrement seconds expired
	jnz	dSelLp1		;repeat for a new 1 second

	inr	a		;timeout, return non-zero
	ret

dSelExt	mvi	a,cHDLOAD	;issue head load command
	out	DRVCMD		;restarts timeout for mini disk
  
	ret			;return with zero status

;---------------------------------------------------------------------------
; dSeek - Seek to track in trkNum
;
; On Entry
;    Drive selected and ready
;    trkNum = desired track
;    curTrk = current track, UNDEF means we don't know
;
; On Exit
;    curTrk = trkNum
;    Interrupts disabled
;    Zero true for success, zero false for failure
;	(failure is a failed restore to track zero)
;    Clobbers A,B,C,H,L
;---------------------------------------------------------------------------
dSeek	di			;disable interrupts
	lda	curTrk		;A=current track
	cpi	UNDEF		;valid track number?
	jnz	dTrkVld		;yes, track is valid

	call	dRestor		;otherwise, restore to track zero
	rnz			;exit if restore failed

; dTrkVld - Current track is valid, compute direction and number
;     of steps

dTrkVld	lxi	h,trkNum	;HL->trkNum
	sub	m		;A=current-requested
	rz			;return if already on correct track

	mvi	b,cSTEPO	;B=step out (assume requested<current)
	jnc	dDoSeek		;it is
	
	mvi	b,cSTEPI	;B=step in (requested>current)
	cma			;compute A=-A
	inr	a

; Save target track in curTrk, step there, then verify track

dDoSeek	mov	c,a		;C=number of steps

	mov	a,m		;save new track in curTrk
	sta	curTrk
	
	call	dStep		;do the seek
	xra	a		;return zero
	ret
	
;---------------------------------------------------------------------------
; dRestor - Restore to track 0 and then delay to ensure any subsequent
;    seek meets the minimum direction change period.
;
; On Entry
;    Drive selected and ready
;
; On Exit
;    If successful
;       curTrk = 0
;	A=0
;	Zero true
;    else
;       curTrk = UNDEF
;       Zero false
;    Clobbers A,B,C
;---------------------------------------------------------------------------
dRestor	xra	a
	sta	curTrk		;set current drive track to zero

	in	DRVSTAT		;test for track 0
	ani	sTRACK0
	rz			;at track 0, exit

; Step in three tracks, then step out to track 0

	mvi	c,3		;C=step in 3 tracks
	mvi	b,cSTEPI	;B=step in command
	call	dStep		;do 3 steps in

	mvi	a,20		;20ms delay puts us past the 1ms step window
	call	delayMs		;  and ensures proper dir change delay

; Seek back out until track 0 detected

	mvi	c,NUMTRK+10	;C=maximum number of step outs to try
	mvi	b,cSTEPO	;B=step out command
	call	dStep		;step until track 0 detected
	jnz	drFail		;restore failed

	mvi	a,20		;20ms delay puts us past the 1ms step window
	call	delayMs		;  and ensures proper dir change delay	
	ret			;return with zero

drFail	mvi	a,UNDEF		;restore failed, track is still undefined
	sta	curTrk
	ret			;return with non-zero

;---------------------------------------------------------------------------
; dStep - Step head number of steps in C, direction command in B.
;   If track zero is detected during a step-out operation, curTrk
;   is forced to zero and zero is returned in A.
;
; On Entry
;    C = number of steps > 0
;    B = step command (cSTEPO or cSTEPI)
;
; On Exit
;    If stepping out and track zero hit
;	curTrk = 0
;	A = 0
;	Zero true
;    else
;	Zero false
;    Clobbers A,B,C
;---------------------------------------------------------------------------
dStep	in	DRVSTAT		;loop until OK to move the head 
	ani	sMOVEOK
	jnz	dStep

	in	DRVSTAT		;see if we are at track 0
	ani	sTRACK0		;at track zero?
	jnz	dStep1		;no, go on

	mov	a,b		;stepping out?
	sbi	cSTEPO
	jz	dStep0		;yes, we've hit track 0

dStep1	mov	a,b		;A=step command and direction
	out	DRVCMD		;issue the step	

	dcr	c		;decrement step counter
	jnz	dStep		;loop until count reaches zero

	inr	c		;force non-zero
	ret

dStep0	sta	curTrk		;curTrk=0
	ret			;return with zero true and A=0

;---------------------------------------------------------------------------
; dNxtSec - Wait for next (any) sector. Returns pointer to the sector
;    buffer within trkBuf. Control is back to caller at 78 cycles (39us).
;
; On Entry
;    Drive is selected, head loaded
;
; On Exit
;    HL->sector buffer in trkBuf for sector found
;    Clobbers A,B,C,H,L
;---------------------------------------------------------------------------
dNxtSec	equ	$

   if MINIDSK
	mvi	a,cRESTMR	;restart motor timeout
	out	DRVCMD
   endif

dnLoop	in	DRVSEC		;read sector position register
	rar			;wait for sector true (0=true)
	jc	dnLoop
	
	ani	SECMASK		;(7) get sector number alone
				;fall into dSecAdr

;---------------------------------------------------------------------------
; dSecAdr - Convert hard sector in A to address within trkBuf for
;    the specified sector
;
; On Entry
;    A = Hard sector number
;
; On Exit
;    HL->sector buffer in trkBuf
;    Clobbers A,B,C,H,L
;---------------------------------------------------------------------------
dSecAdr	lxi	h,secAddr  	;(10) HL->sector address table
	mvi	b,0		;(7) form BC=sector*2
	rlc			;(4) A=sector*2 (2 bytes per table entry)
	mov	c,a		;(5) BC=sector*2
	dad	b		;(10) HL->address table entry for passed sector
	
	mov	a,m		;(7) A=lsb of sector buffer address
	inx	h		;(5)
	mov	h,m		;(7) H=msb of sector buffer address
	mov	l,a		;(5) HL->sector buffer
	ret			;(11)

;----------------------------------------------------------------------------
; delayMs - Delay for number of ms specified in A 
;
; On Entry
;    A = ms to delay
;
; On Exit
;    A=0
;    Zero true
;    Clobbers A,B
;----------------------------------------------------------------------------
delayMs	mvi	b,(2000/19)	;19 cycles in the loop below

delayLp	nop			;(4)
	dcr	b		;(5)
	jnz	delayLp		;(10)

	dcr	a		;decrement ms counter
	jnz	delayMs
	ret

;---------------------------------------------------------------------------
; dRead - Read a sector, verify checksum, stop byte, and track number.
;    Verification is done after the sector is read in the time between
;    the last byte of the sector and before the start of the next sector.
;    With index alignment and rotation speed tolerance accounted for,
;    we safely have 360 cycles from the last byte of the sector until code
;    should be in the sector true hunt loop. The RET is complete from this
;    routine 152 cycles worst case after the last byte is read.
;
; On Entry
;    Drive is selected, sector true just detected
;    HL->sector buffer
;
; On Exit
;    Sector read to (HL)
;    C = track number from disk with sync bit set
;    Zero true if no error, Zero false for checksum error, missing 
;	FF stop byte, or track number error
;    Clobbers A,B,C,H,L
;---------------------------------------------------------------------------
dRead	push	h		;(11) save pointer to start of sector

	lxi	b,256+DSECLEN	;(10) B=chksum (-FF), C=bytes to read
	lda	curTrk		;(13) A=track we're on
	cpi	DATATRK		;(7) data track or system track?
	jnc	drSecWt		;(10) data track, length in C is correct

	mvi	c,SSECLEN	;(7) C=length of a system sector

; The sector transfer loop is 116 cycles for two bytes read (has to be
;     less than 128) and computes checksum over all bytes read. For the
;     mini disk, only one byte is read per loop iteration.

drSecWt	in	DRVSTAT		;(10) get drive status byte
	ora	a		;(4) wait for NRDA flag true (zero)
	jm	drSecWt		;(10) 

   if NOT MINIDSK		;8" reads two bytes per loop
	in	DRVDATA		;(10) read first byte at 24-48 cycles
	mov	m,a		;(7) store in buffer
	add	b		;(4) update checksum
	mov	b,a		;(5) keep checksum in B
	inx	h		;(5) increment buffer pointer
	dcr	c		;(5) decrement byte count
	jz	drSecDn		;(10) exit if done
   endif

	in	DRVDATA		;(10) read at 70-94 cycles (data at 64 and 128)
	mov	m,a		;(7) store in buffer
	add	b		;(4) update checksum
	mov	b,a		;(5) keep checksum in B
	inx	h		;(5) increment buffer pointer
	dcr	c		;(5) decrement byte count
	jnz	drSecWt		;(10) repeat until done 

; drSecDn - Sector read is complete, now validate the sector based on whether
;   it is from a system track or data track.

drSecDn	lda	curTrk		;(10) A=track we're supposed to be on
	cpi	DATATRK		;(7) system or data track?
	jnc	drDatTk		;(10) data track

; Validate a system track sector. A system track sector is three bytes
;    bytes shorter, so this codes starts 3*64 cycles sooner than for a
;    data sector. This code completes before a data sector would even
;    finish reading. Therefore, a data sector is the worst timing path.

	mov	a,b		;(5) A=calculated checksum over all bytes
	dcx	h		;(5) HL->checksum in sector
	sub	m		;(7) subtract from computed checksum
	sub	m		;(7) final sum will be zero

	dcx	h		;(5) HL->stop byte
	mov	b,m		;(7) B=stop byte
	pop	h		;(10) restore HL->start of buffer
	inr	b		;(4) was FF stop byte there?
	rnz			;(5/11) no, return error

	mov	c,m		;(7) C=track number from disk
	sub	c		;(4) take track out of checksum

	inx	h		;(5) HL->2nd byte of sector
	sub	m		;(7) take out of checksum
	
	inx	h		;(5) HL->3rd byte of sector
	jmp	drExit		;(10) do common compares and exit

; Validate a data track sector (125 + 27 cycles for sector type jump)

drDatTk	dcx	h		;(5) HL->stop byte
	mov	a,m		;(7) A=stop byte
	pop	h		;(10) restore HL->start of buffer
	inr	a		;(4) was FF stop byte there?
	rnz			;(5/11) no, return error

	mov	a,b		;(5) A=calculated checksum over all bytes
	mov	c,m		;(7) C=track number from disk
	sub	c		;(4) take track out of checksum

	inx	h		;(5) HL->sector number
	sub	m		;(7) take out of checksum
	
	inx	h		;(5) move to checksum byte
	inx	h		;(5)
	inx	h		;(5) HL->checksum
	
	sub	m		;(7) subtract from checksum
drExit	sub	m		;(7) checksums match?
	rnz			;(5/11) no, exit with error
	
	lda	curTrk		;(10) A=track we should be on
	ori	80h		;(7) set sync bit
	cmp	c		;(4) same track as from sector?
	ret			;(11) exit with compare status

;---------------------------------------------------------------------------
; dVerify - Verify a sector
;    The 2nd read at 67 cycles only gives 3 cycles of headroom (67-64)
;    which, in turn, gives about 4% speed tolerance. However, we are
;    verifying our  own write, so the primary tolerance issue is ISV,
;    not the average rotation rate, so tolerance is still within the
;    +/-1.5% ISV spec.
;
; On Entry
;    Drive is selected, sector true just detected
;    HL->sector buffer to compare against
;
; On Exit
;    Zero true if match, zero false for error
;    Clobbers A,B,C,H,L
;---------------------------------------------------------------------------
dVerify lxi	b,DSECLEN	;(10) B=0, C=data sector length
	lda	curTrk		;(13) A=track we're on
	cpi	DATATRK		;(7) data track or system track?
	jnc	dVfLoop		;(10) data track, length in C is correct

	mvi	c,SSECLEN	;(7) set C=length of a system sector

dVfLoop	in	DRVSTAT		;(10) get drive status byte
	ora	a		;(4) wait for NRDA flag true (zero)
	jm	dVfLoop		;(10)

   if NOT MINIDSK		;8" reads two bytes per loop
	in	DRVDATA		;(10) read first byte at 24-48 cycles
	xra	m		;(4) verify data matches buffer
	ora	b		;(4) accumulate errors in b
	mov	b,a		;(5)

	inx	h		;(5) increment buffer pointer
	dcr	c		;(5) decrement characters remaining counter
	jz	dVfDone		;(10) done verifying bytes
   endif

	in	DRVDATA		;(10) read 2nd byte at 67-91 cycles
	xra	m		;(4) verify data matches buffer
	ora	b		;(4) accumlate errors in b
	mov	b,a		;(5) 

	inx	h		;(5) increment buffer pointer
	dcr	c		;(5) decrement byte counter
	jnz	dVfLoop		;(10) loop for all bytes

dVfDone	ora	a		;A=zero if all bytes matched
	ret			;return status

;---------------------------------------------------------------------------
; dWrite - Write a sector
;    The write loop is 61 cycles which gives just 3 cycles (64-61) of 
;    headroom. However, since the write timing required by the FDC board 
;    is derived from the 2 MHz CPU/bus clock, this loop is always exactly
;    in sync with the FDC write timing. 3 cycles of headroom is plenty.
;
; On Entry
;    Drive is selected, sector true just detected, and
;	the write command has already been issued
;    HL->sector buffer to write
;
; On Exit
;   Clobbers A,C,H,L
;---------------------------------------------------------------------------
dWrite	mvi	c,DSECLEN	;C=number of bytes to write
	lda	curTrk		;(13) A=track we're on
	cpi	DATATRK		;(7) data track or system track?
	jnc	dWrLoop		;(10) data track, length in C is correct

	mvi	c,SSECLEN	;(7) set C=length of a system sector

dWrLoop	in	drvStat		;(10) read drive status register
	rrc			;(4) wait for ENWD flag (zero)
	jc	dWrLoop		;(10)

	mov	a,m		;(7) A=next byte to write
	out	drvData		;(10) write 2nd byte at 68-92 cycles
	inx	h		;(5) increment buffer pointer
	dcr	c		;(5) decrement bytes remaining
	jnz	dWrLoop		;(10) loop until all bytes written

dWrDone	in	drvStat		;wait for another write flag
	rrc
	jc	dWrDone
	
	xra	a		;write a trailing zero
	out	drvData
	ret

;******************************************************************************
;
;   BIOS Data Area
;
;******************************************************************************

;---------------------------------------------------------------------------
; secAddr - Sector address table. Returns sector buffer address within
;    the track buffer when indexed by hard sector number. Each sector
;    requires TSECLEN bytes.
;---------------------------------------------------------------------------
secAddr	dw	trkBuf+00*TSECLEN,trkBuf+01*TSECLEN,trkBuf+02*TSECLEN
	dw	trkBuf+03*TSECLEN,trkBuf+04*TSECLEN,trkBuf+05*TSECLEN
	dw	trkBuf+06*TSECLEN,trkBuf+07*TSECLEN,trkBuf+08*TSECLEN
	dw	trkBuf+09*TSECLEN,trkBuf+10*TSECLEN,trkBuf+11*TSECLEN
	dw	trkBuf+12*TSECLEN,trkBuf+13*TSECLEN,trkBuf+14*TSECLEN
	dw	trkBuf+15*TSECLEN

   if NOT MINIDSK
	dw	trkBuf+16*TSECLEN,trkBuf+17*TSECLEN,trkBuf+18*TSECLEN
	dw	trkBuf+19*TSECLEN,trkBuf+20*TSECLEN,trkBuf+21*TSECLEN
	dw	trkBuf+22*TSECLEN,trkBuf+23*TSECLEN,trkBuf+24*TSECLEN
	dw	trkBuf+25*TSECLEN,trkBuf+26*TSECLEN,trkBuf+27*TSECLEN
	dw	trkBuf+28*TSECLEN,trkBuf+29*TSECLEN,trkBuf+30*TSECLEN
	dw	trkBuf+31*TSECLEN
   endif
	
;-----------------------------------------------------------------------------
; Drive Tables
;-----------------------------------------------------------------------------

; drvTbl - Points to the DPH for each of 16 possible drives

drvTbl	dw	dph0
	dw	dph1
	dw	dph2
	dw	dph3
	dw	0,0,0,0,0,0,0,0,0,0,0,0

; dph - Disk Parameter Headers (DPH). The data BCB is set to FFFFh since
;    physical=logical sector size (128). Directory hashing disabled.

dph0	dw	tranTbl,0,0,0,0,0,dpb,csv0,alv0,dirBcb,-1,-1
	db	0
dph1	dw	tranTbl,0,0,0,0,0,dpb,csv1,alv1,dirBcb,-1,-1
	db	0
dph2	dw	tranTbl,0,0,0,0,0,dpb,csv2,alv2,dirBcb,-1,-1
	db	0
dph3	dw	tranTbl,0,0,0,0,0,dpb,csv3,alv3,dirBcb,-1,-1
	db	0	

; dpb - Disk Parameter Block. This table gives a block size of 1024 bytes
;   and 64 directory entries.

dpb	dw	NUMSEC		;sectors per track
	db	BSH		;allocation block shift factor (BSH)
	db	BLM		;data location block mask (BLM)
	db	EXM		;extent mask (EXM)
	dw	DSM		;maximum block number (DSM 242)	
	dw	DRM		;maximum directory entry number (DRM 63)
	db	AL0,AL1		;AL0, AL1 
	dw	CKS		;CKS=(DRM+1)/4
	dw	RESTRK		;reserved tracks for CPM and bootloader
	db	0		;physical=logical sector (128 bytes)
	db	0		;physical=logical sector (128 bytes)

; dirBcb - Directory Buffer Control Block (BCB)

dirBcb	db	0FFh		;drive number (FF means not used yet)
	db	0,0,0		;record position
	db	0		;dirty (write) flag
	db	0		;scratch byte
	dw	0		;track
	dw	0		;sector
	dw	dirBuf		;buffer address
	dw	0		;bank
	dw	0		;link to next BCB (none)

   if MINIDSK
tranTbl	db	1,3,5,7,9,11,13,15,2,4,6,8,10,12,14,16
   endif

   if NOT MINIDSK
tranTbl	db	01,09,17,25,03,11,19,27,05,13,21,29,07,15,23,31
	db	02,10,18,26,04,12,20,28,06,14,22,30,08,16,24,32
   endif

;----------------------------------------------------------------------------
; charTbl - Character I/O table for DEVICE program
;----------------------------------------------------------------------------
charTbl	db '2SIOA '		; device 0, 2SIO at 10h/11h
	db mb$in$out+mb$serial
	db baud$none
	db '2SIOB '		; device 1, 2SIO at 12h/13h
	db mb$in$out+mb$serial
	db baud$none
	db 'SIO   '		; device 2, SIO at 0/1
	db mb$in$out+mb$serial
	db baud$none
	db 'ACR   '		; device 3, SIO at 6/7 (ACR)
	db mb$in$out+mb$serial
	db baud$none
	db 0			; table terminator

;----------------------------------------------------------------------------
; Initialized data
;----------------------------------------------------------------------------
coldSt	db	0FFh		;FF during cold start, 0 otherwise

curDrv	db	0		;drive number currently selected
curTrk	db	UNDEF		;track the selected drive is on
trkTbl	db	UNDEF,UNDEF,UNDEF,UNDEF		;track each drive is on

bfDirty	db	0		;non-zero if buffer has been written to
bfDrive	db	UNDEF		;drive currently in track buffer
bfTrack	db	UNDEF		;currently buffered track (MUST follow bfDrive)

;----------------------------------------------------------------------------
; Non-initialized data
;----------------------------------------------------------------------------
drvNum	ds	1		;drive number from CP/M call
trkNum	ds	1		;track number from CP/M call (MUST follow drvNum)
secNum	ds	1		;sector number from CP/M call
dmaAddr	ds	2		;dma address for disk operations from CP/M call

secCnt	ds	1		;count of bad sectors remaining in trkBuf
wrRetry	ds	1		;write retry counter
skRetry	ds	1		;restore and seek retry counter

;-----------------------------------------------------------------------------
;  Disk scratchpad areas defined in the DPH table
;-----------------------------------------------------------------------------
dirBuf	ds	128		;BDOS directory scratchpad
alv0	ds	(DSM/8 + 1)	;allocation vector storage
csv0	ds	CKS		;change disk scratchpad
alv1	ds	(DSM/8 + 1)
csv1	ds	CKS
alv2	ds	(DSM/8 + 1)
csv2	ds	CKS
alv3	ds	(DSM/8 + 1)
csv3	ds	CKS

;-----------------------------------------------------------------------------
; Track buffer. One extra byte is allocated at the start of each sector
;    to flag a bad sector (read failure). This byte is zero if the sector
;    is good, non-zero if the sector is bad.
;-----------------------------------------------------------------------------
trkBuf	ds	NUMSEC*TSECLEN

	end
