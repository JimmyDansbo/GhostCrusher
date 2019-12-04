

IRQ_VEC		= $0314

JOY1		= $02BC
JOY2		= $02BF

NES_A		= $80
NES_B		= $40
NES_SEL		= $20
NES_STA		= $10

JOY_UP		= $08
JOY_DN		= $04
JOY_LT		= $02
JOY_RT		= $01

GETJOY		= $FF06
CHROUT		= $FFD2
PLOT		= $FFF0
SCRMOD		= $FF5F

COLOR_PORT	= $0286

VERA_BASE	= $9F20
VERA_ADDR_LOW	= VERA_BASE+0
VERA_ADDR_HIGH	= VERA_BASE+1
VERA_ADDR_BANK	= VERA_BASE+2
VERA_DATA0	= VERA_BASE+3
VERA_DATA1	= VERA_BASE+4
VERA_CTRL	= VERA_BASE+5
VERA_IEN	= VERA_BASE+6
VERA_ISR	= VERA_BASE+7

TMP0		= $00
TMP1		= $01
TMP2		= $02
TMP3		= $03
TMP4		= $04
TMP5		= $05
TMP6		= $06
TMP7		= $07
TMP8		= $08
TMP9		= $09

; *******************************************************************
; Initialize VERA with sane values
; *******************************************************************
; USES:		A
; *******************************************************************
!macro vera_init {
	lda	#0
	sta	VERA_CTRL	; No reset, use data0
	lda	#$10
	sta	VERA_ADDR_BANK	; Increment by 1
}

; *******************************************************************
; Save the address of the original interrupt handler
; *******************************************************************
; USES:		A
; *******************************************************************
!macro save_int_vector {
	lda	IRQ_VEC
	sta	old_irq_handler
	lda	IRQ_VEC+1
	sta	old_irq_handler+1
}

; *******************************************************************
; Install new interrupt handler
; *******************************************************************
; USES:		A
; *******************************************************************
!macro install_int_handler {
	sei			; Disable interrupts
	lda	#1		; Tell VERA that we want interrupts
	sta	VERA_IEN	; on each VSYNC
	lda	#<handle_irq	; Write address of local interrupt
	sta	IRQ_VEC		; handler to the interrupt
	lda	#>handle_irq	; vector address ($0314)
	sta	IRQ_VEC+1
	cli			; Enable interrupts
}

; *******************************************************************
; Check if the VSYNC interrupt has occurred. If it has,
; call the do_game routine to keep the game going.
; *******************************************************************
; USES:		A
; *******************************************************************
!macro check_irq {
	lda	IRQ_TRIG
	beq	.done
	; VSYNC IRQ has occurred, handle
	jsr	do_game
	lda	#0
	sta	IRQ_TRIG
.done:
}

	jmp	X16_DONE

; *******************************************************************
; When a new interrupt handler is installed, it should pass control
; to the original interrupt hanlder upon completion. This label
; will be updated to point to the original interrupt handler.
; *******************************************************************
old_irq_handler:
	!word	$FFFF

X16_DONE:
