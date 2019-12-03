

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

!macro vera_goxy .xcord, .ycord {
	lda	#.ycord
	sta	VERA_ADDR_HIGH
	lda	#.xcord
	asl
	sta	VERA_ADDR_LOW
}

!macro vera_chrout .char {
	lda	#.char
	sta	VERA_DATA0
}

!macro vera_color .col {
	ldy	#.col
	sty	VERA_DATA0
}

!macro vera_color .col, .add {
	ldy	VERA_ADDR_LOW
	iny
	sty	VERA_ADDR_LOW
	ldy	#.col
	sty	VERA_DATA0
}

!macro vera_init {
	lda	#0
	sta	VERA_CTRL
	lda	#$10
	sta	VERA_ADDR_BANK
}

!macro save_int_vector {
	lda	IRQ_VEC
	sta	old_irq_handler+1
	lda	IRQ_VEC+1
	sta	old_irq_handler+2
}

	jmp	X16_DONE

; *******************************************************************
; When a new interrupt handler is installed, it should pass control
; to the original interrupt hanlder upon completion. This label
; will be updated to point to the original interrupt handler.
; *******************************************************************
old_irq_handler:
	jmp	$FFFF

X16_DONE:
