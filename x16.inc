

IRQ_VEC		= $0314

JOY1		= $02BC
JOY2		= $02BF

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
	sta	VERA_ADDR_BANK
}

;	jmp	X16_DONE



X16_DONE:
