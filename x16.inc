!ifdef CX16_INC !eof
CX16_INC=1

; IRQ
IRQ_VEC		= $0314

; Emulator
GIFCTRL		= $9FB5

; KERNAL version address
KERNALVER	= $FF80

; VIA
VIA1		= $9F60
VIA1PA		= VIA1+1
VIA1PB		= VIA1+0
VIA2		= $9F70
VIA2PA		= VIA2+1
VIA2PB		= VIA2+0

; ROM bank allocations
KERNAL_BANK	= 0
KEYBD_BANK	= 1
CBDOS_BANK	= 2
GEOS_BANK	= 3
BASIC_BANK	= 4
MONITOR_BANK	= 5

; NES joypad keycodes
NES_A		= $80
NES_B		= $40
NES_SEL		= $20
NES_STA		= $10
JOY_UP		= $08
JOY_DN		= $04
JOY_LT		= $02
JOY_RT		= $01

; CX16 KERNAL API calls
JOY_SCAN	= $FF53
JOY_GET		= $FF56

; CX16/C64 KERNAL API calls
CHROUT		= $FFD2
GETIN		= $FFE4
PLOT		= $FFF0
SCRMOD		= $FF5F

; Text mode ports (not supported and changes between ROM releases)
COLOR_PORT	= $0376
NUMCOLS		= $0386
NUMLINES	= $0387

; virtual 16 bit registers for "16 bit" ABI
r0		= $02
r0L		= r0+0
r0H		= r0+1
r1		= $04
r1L		= r1+0
r1H		= r1+1
r2		= $06
r2L		= r2+0
r2H		= r2+1
r3		= $08
r3L		= r3+0
r3H		= r3+1
r4		= $0A
r4L		= r4+0
r4H		= r4+1
r5		= $0C
r5L		= r5+0
r5H		= r5+1
r6		= $0E
r6L		= r6+0
r6H		= r6+1
r7		= $10
r7L		= r7+0
r7H		= r7+1
r8		= $12
r8L		= r8+0
r8H		= r8+1
r9		= $14
r9L		= r9+0
r9H		= r9+1
r10		= $16
r10L		= r10+0
r10H		= r10+1
r11		= $18
r11L		= r11+0
r11H		= r11+1
r12		= $1A
r12L		= r12+0
r12H		= r12+1
r13		= $1C
r13L		= r13+0
r13H		= r13+1
r14		= $1E
r14L		= r14+0
r14H		= r14+1
r15		= $20
r15L		= r15+0
r15H		= r15+1

; ZeroPage variables/pointers
TMP0		= $70
TMP1		= $71
TMP2		= $72
TMP3		= $73
TMP4		= $74
TMP5		= $75
TMP6		= $76
TMP7		= $77
TMP8		= $78
TMP9		= $79
TMPa		= $7A
TMPb		= $7B
TMPc		= $7C
TMPd		= $7D
TMPe		= $7E
TMPf		= $7f

; PETSCII color codes
PET_BLACK	= $90
PET_WHITE	= $05
PET_RED		= $1C
PET_CYAN	= $9F
PET_PURPLE	= $9C
PET_GREEN	= $1E
PET_BLUE	= $1F
PET_YELLOW	= $9E
PET_ORANGE	= $81
PET_BROWN	= $95
PET_LIGHTRED	= $96
PET_DARKGRAY	= $97
PET_MIDGRAY	= $98
PET_LIGHTGREEN	= $99
PET_LIGHTBLUE	= $9A
PET_LIGHTGRAY	= $9B

; Color values
BLACK		= 0
WHITE		= 1
RED		= 2
CYAN		= 3
PURPLE		= 4
GREEN		= 5
BLUE		= 6
YELLOW		= 7
ORANGE		= 8
BROWN		= 9
LIGHTRED	= 10
DARKGRAY	= 11
MIDGRAY		= 12
LIGHTGREEN	= 13
LIGHTBLUE	= 14
LIGHTGRAY	= 15

; *****************************************************************************
; Save the address of the original interrupt handler
; *****************************************************************************
; USES:		A
; OUTPUTS:	Address of old handler will be passed back in .old_handler
; *****************************************************************************
!macro SAVE_INT_VECTOR .old_handler {
	lda	IRQ_VEC
	sta	.old_handler
	lda	IRQ_VEC+1
	sta	.old_handler+1
}

; *****************************************************************************
; Install new interrupt handler
; *****************************************************************************
; INPUTS:	.handler = address of new interrupt handler
; USES:		A
; *****************************************************************************
!macro INSTALL_INT_HANDLER .handler {
	sei			; Disable interrupts
	lda	#1		; Tell VERA that we want interrupts
	sta	VERA_IEN	; on each VSYNC
	lda	#<.handler	; Write address of local interrupt
	sta	IRQ_VEC		; handler to the interrupt
	lda	#>.handler	; vector address ($0314)
	sta	IRQ_VEC+1
	cli			; Enable interrupts
}

; *****************************************************************************
; Check if the VSYNC interrupt has occurred. If it has,
; call the do_game routine to keep the game going.
; *****************************************************************************
; INPUTS	.handler = address of interrupt handler
; USES:		A
; *****************************************************************************
!macro CHECK_IRQ .handler {
	lda	IRQ_TRIG
	beq	.done
	; VSYNC IRQ has occurred, handle
	jsr	.handler
	lsr	IRQ_TRIG
.done:
}

;******************************************************************************
; Code to enable program to be startet with the RUN command
;******************************************************************************
!macro SYS_LINE {
	!word	$080C			; Pointer to next BASIC line
	!word	$000A			; Line number $000A = 10
	!byte	$9E			; SYS BASIC token
	!pet	" $810",0		; Address where ASM starts
	!word	$0000			; EOF BASIC program
}

;******************************************************************************
; Get KERNAL version and convert it from 2's compliment number if it is a
; pre-release
;******************************************************************************
; RETURNS:	.A contains the KERNAL version
;		.C cleared if pre-release version
;******************************************************************************
!macro GET_KERNAL_VER {
	lda	VIA1PB			; Save current ROM bank
	sta	TMP0			; In TMP0
	and	#KERNAL_BANK|#$F8	; Set ROM bank to 0, only 3 lowest bits
	sta	VIA1PB			; are used to set ROM bank
	lda	KERNALVER		; Read KERNAL version
	sta	TMP1			; SAve it in TMP1
	lda	TMP0			; Restore ROM bank
	sta	VIA1PB
	lda	TMP1			; Get KERNAL version into .A
	sec
	bpl	.done			; If positive, we are done
	eor	#$FF			; Invert and add 1 to convert the 2's
	clc				; compliment number to the KERNAL version
	adc	#1
	clc
.done:
}
