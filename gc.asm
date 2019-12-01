*=$0801
!byte $0C,$08,$0A,$00,$9E,$20,$32,$30,$36,$34,$00,$00,$00
*=$0810


!src "x16.inc"
!src "text.inc"


main:
	jsr	splash_screen

	+vera_init

	jmp	*
	rts

; **************************************************************************
; Ensure that the program is running in 80 column mode, set black
; background, clear the screen and write/draw the splash screen.
; **************************************************************************
splash_screen:
	lda	#2			; Ensure 80x60 mode
	sec
	jsr	SCRMOD

	lda	#WALL_COL		; Set black background
	sta	COLOR_PORT

	lda	#147			; Clear Screen
	jsr	CHROUT

	+gotoxy 2, 10
	ldx	#<.gc1
	ldy	#>.gc1
	jsr	print_str
	+gotoxy 2, 11
	ldx	#<.gc2
	ldy	#>.gc2
	jsr	print_str
	+gotoxy 2, 12
	ldx	#<.gc3
	ldy	#>.gc3
	jsr	print_str
	+gotoxy 2, 13
	ldx	#<.gc4
	ldy	#>.gc4
	jsr	print_str
	+gotoxy	2, 14
	ldx	#<.gc5
	ldy	#>.gc5
	jsr	print_str
	+gotoxy 23, 18
	ldx	#<.name
	ldy	#>.name
	jsr	print_str
	+gotoxy	27, 20
	ldx	#<.mail
	ldy	#>.mail
	jsr	print_str
	+gotoxy	27, 27
	ldx	#<.forcx16
	ldy	#>.forcx16
	jsr	print_str
	+gotoxy 24, 36
	ldx	#<.pstart
	ldy	#>.pstart
	jsr	print_str
	+gotoxy	38, 24
	ldx	#<.xl1
	ldy	#>.xl1
	jsr	print_str
	+gotoxy 39, 25
	ldx	#<.xl2
	ldy	#>.xl2
	jsr	print_str
	+gotoxy 40, 26
	ldx	#<.xl3
	ldy	#>.xl3
	jsr	print_str
	+gotoxy 40, 28
	ldx	#<.xl5
	ldy	#>.xl5
	jsr	print_str
	+gotoxy 39, 29
	ldx	#<.xl6
	ldy	#>.xl6
	jsr	print_str
	+gotoxy 38, 30
	ldx	#<.xl7
	ldy	#>.xl7
	jsr	print_str
	+gotoxy 35, 40
	ldx	#<.player_text
	ldy	#>.player_text
	jsr	print_str
	+gotoxy 35, 42
	ldx	#<.portal_text
	ldy	#>.portal_text
	jsr	print_str
	+gotoxy 35, 44
	ldx	#<.ghost_text
	ldy	#>.ghost_text
	jsr	print_str
	+gotoxy 35, 46
	ldx	#<.pghost_text
	ldy	#>.pghost_text
	jsr	print_str
	+gotoxy 35, 48
	ldx	#<.dghost_text
	ldy	#>.dghost_text
	jsr	print_str
	+gotoxy 35, 50
	ldx	#<.wall_text
	ldy	#>.wall_text
	jsr	print_str
	+gotoxy 35, 52
	ldx	#<.swall_text
	ldy	#>.swall_text
	jsr	print_str
	rts

print_str:
	; Store address of string in ZP memory
	stx	TMP0
	sty	TMP1
	ldy	#0		; Y register used to index string
.doprint
	lda	(TMP0), y	; Load character from string
	beq	.printdone	; If character is 0, we are done
	jsr	CHROUT		; Write charactoer to screen
	iny			; Inc Y to get next character
	jmp	.doprint	; Get next character
.printdone:
	rts
