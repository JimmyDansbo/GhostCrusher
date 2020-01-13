!ifdef VERA_INC !eof
VERA_INC = 1

VERA_BASE	= $9F20
VERA_ADDR_LOW	= VERA_BASE+0
VERA_ADDR_HIGH	= VERA_BASE+1
VERA_ADDR_BANK	= VERA_BASE+2
VERA_DATA0	= VERA_BASE+3
VERA_DATA1	= VERA_BASE+4
VERA_CTRL	= VERA_BASE+5
VERA_IEN	= VERA_BASE+6
VERA_ISR	= VERA_BASE+7

; *******************************************************************
; Initialize VERA with sane values
; *******************************************************************
; USES:		.A
; *******************************************************************
!macro VERA_INIT {
	lda	#0
	sta	VERA_CTRL	; No reset, use data0
	lda	#$10
	sta	VERA_ADDR_BANK	; Increment by 1
}

; *******************************************************************
; Set cursor to location
; *******************************************************************
; USES:		.A
; *******************************************************************
!macro VERA_GO_XY .xcord, .ycord {
	lda	.ycord
	sta	VERA_ADDR_HIGH
	lda	.xcord
	asl
	sta	VERA_ADDR_LOW
}

!macro VERA_GO_XY .xcord, .ycord, .abs {
	lda	#.ycord
	sta	VERA_ADDR_HIGH
	lda	#.xcord
	asl
	sta	VERA_ADDR_LOW
}
