*=$0801
!byte $0C,$08,$0A,$00,$9E,$20,$32,$30,$36,$34,$00,$00,$00
*=$0810


!src "x16.inc"
!src "text.inc"


main:
	lda	#0
	sta	VERA_CTRL
	sta	VERA_ADDR_BANK

	lda	#$05
	sta	COLOR_PORT

	lda	#147
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

	lda	#$04
	sta	COLOR_PORT
	+gotoxy	38, 24
	ldx	#<.xl1
	ldy	#>.xl1
	jsr	print_str
	lda	#$0E
	sta	COLOR_PORT
	+gotoxy 39, 25
	ldx	#<.xl2
	ldy	#>.xl2
	jsr	print_str
	lda	#$03
	sta	COLOR_PORT
	+gotoxy 40, 26
	ldx	#<.xl3
	ldy	#>.xl3
	jsr	print_str
	lda	#$07
	sta	COLOR_PORT
	+gotoxy 40, 28
	ldx	#<.xl5
	ldy	#>.xl5
	jsr	print_str
	lda	#$08
	sta	COLOR_PORT
	+gotoxy 39, 29
	ldx	#<.xl6
	ldy	#>.xl6
	jsr	print_str
	lda	#$02
	sta	COLOR_PORT
	+gotoxy 38, 30
	ldx	#<.xl7
	ldy	#>.xl7
	jsr	print_str

	+vera_goxy 2, 11	; In G
	+vera_chrout SWALL
	+vera_color SWALL_COL

	+vera_goxy 9, 11	; Player char in H
	+vera_chrout PLAYER
	+vera_color PLAY_COL

	+vera_goxy 14, 13	; In O
	+vera_chrout SWALL
	+vera_color SWALL_COL

	+vera_goxy 17, 12	; Ghost in O
	+vera_chrout GHOST
	+vera_color GHOST_COL

	+vera_goxy 22, 11	; Portal in S
	+vera_chrout PORTAL
	+vera_color GHOST_COL

	+vera_goxy 27, 10	; In T
	+vera_chrout SWALL
	+vera_color SWALL_COL

	+vera_goxy 35, 11	; Ghost in C
	+vera_chrout GHOST
	+vera_color GHOST_COL

	+vera_goxy 41, 12	; In R
	+vera_chrout SWALL
	+vera_color SWALL_COL

	+vera_goxy 41, 13	; Super Ghost in R
	+vera_chrout SGHOST
	+vera_color GHOST_COL

	+vera_goxy 47, 13	; Ghost in U
	+vera_chrout GHOST
	+vera_color GHOST_COL

	+vera_goxy 49, 14	; In U
	+vera_chrout SWALL
	+vera_color SWALL_COL

	+vera_goxy 52, 14	; In S
	+vera_chrout SWALL
	+vera_color SWALL_COL

	+vera_goxy 53, 11	; Portal in S
	+vera_chrout PORTAL
	+vera_color GHOST_COL

	+vera_goxy 58, 13	; In H
	+vera_chrout SWALL
	+vera_color SWALL_COL
	+vera_chrout HGHOST	; Hatched Ghost in H
	+vera_color GHOST_COL


	+vera_goxy 64, 10	; In E
	+vera_chrout SWALL
	+vera_color SWALL_COL

	+vera_goxy 67, 10	; In E
	+vera_chrout SWALL
	+vera_color SWALL_COL

	+vera_goxy 75, 11	; In R
	+vera_chrout SWALL
	+vera_color SWALL_COL

	jmp	*
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
