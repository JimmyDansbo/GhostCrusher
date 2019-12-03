*=$0801
!byte $0C,$08,$0A,$00,$9E,$20,$32,$30,$36,$34,$00,$00,$00
*=$0810

RANDNUM		=	$7E	; Last available ZP addresses (R34)
LEVEL		=	RANDNUM-1	; ZP address to store current level
LIVES		=	RANDNUM-2	; ZP address to store number of lives

!macro rand .min, .max {
-	jsr	randomize
	cmp	#(.max-.min)
	bcs	-
	clc
	adc	#.min
}

!src "x16.inc"
!src "text.inc"


main:
	jsr	init_vars
	jsr	splash_screen

	+vera_init
	+save_int_vector

	sei
	lda	#1
	sta	VERA_IEN
	lda	#<rnd_irq
	sta	IRQ_VEC
	lda	#>rnd_irq
	sta	IRQ_VEC+1
	cli

	; Wait for user to start game, use the time to
	; to do random numbers
.start_wait:
	jsr	randomize
	jsr	GETJOY
	lda	JOY1
	and	#NES_STA
	bne	.start_wait

	jsr	draw_border
	ldy	#12
	jsr	place_swalls
	lda	#6
	jsr	place_walls

-	jsr	GETJOY
	lda	JOY1
	and	#NES_STA
	bne	-

	jsr	clear_field

	jmp	.start_wait
	rts


; *******************************************************************
; Clear the playing field
; *******************************************************************
clear_field:
	lda	#$10
	sta	VERA_ADDR_BANK		; Increment by 1
	ldy	#3

.outloop:
	sty	VERA_ADDR_HIGH
	ldx	#78
	lda	#2
	sta	VERA_ADDR_LOW
.inloop:
	lda	#' '
	sta	VERA_DATA0
	lda	#WALL_COL
	sta	VERA_DATA0
	dex
	bne	.inloop
	iny
	cpy	#59
	bne	.outloop
	rts

; *******************************************************************
; Place walls randomly on the playing field
; *******************************************************************
; USES:		A
;		TMP0 and TMP1 for counters
; *******************************************************************
place_walls:
;	lda	#6
	sta	TMP0
	lda	#0
	sta	TMP1
	sta	VERA_ADDR_BANK		; No Increment

.out_loop:
.in_loop:
	+rand	3, 59
	sta	VERA_ADDR_HIGH
	+rand	1, 79
	asl
	sta	VERA_ADDR_LOW
	lda	VERA_DATA0
	cmp	#' '
	bne	.in_loop
	lda	#WALL
	sta	VERA_DATA0
	inc	VERA_ADDR_LOW
	lda	#WALL_COL
	sta	VERA_DATA0
	inc	TMP1
	bne	.in_loop
	dec	TMP0
	bne	.out_loop
	rts


; *******************************************************************
; Updates the RANDNUM variable each time the interrupt is called
; Uses A, but it is restored by KERNAL when the original intterupt
; handler is called.
; *******************************************************************
rnd_irq:
	lda	VERA_ISR
	and	#1		; Is this VSYNC?
	beq	.ri_end		; if not, end

	jsr	randomize
.ri_end:
	jmp	old_irq_handler	; Continue to original

; *******************************************************************
; Place a number of Static Wall Chars randomly in the playing field
; *******************************************************************
; INPUTS:	Y = number of Static Walls to place
; USES:		A
; *******************************************************************
place_swalls:
	+rand 2, 77
	asl				; Multiply by 2 for X coord
	sta	VERA_ADDR_LOW

	+rand 4, 57
	sta	VERA_ADDR_HIGH

	lda	#SWALL			; Set the SWALL character
	sta	VERA_DATA0
	lda	#SWALL_COL		; Set the SWALL_COL color
	sta	VERA_DATA0
	dey				; While Y > 0
	bne	place_swalls		; jump back to place wall

	rts

; *******************************************************************
; Initialize ZP variables with correct values for start
; *******************************************************************
; USES:		A
; *******************************************************************
init_vars:
	lda	#1		; LEVEL = 1
	sta	LEVEL
	sta	RANDNUM
	lda	#5		; LIVES = 5
	sta	LIVES
	sta	RANDNUM+1
	rts

; *******************************************************************
; Draw the border around the playing field with the Static Wall
; characters
; *******************************************************************
; USES:		A, X & Y
; *******************************************************************
draw_border:
	lda	#WALL_COL	; Clear screen with black background
	sta	COLOR_PORT
	lda	#147
	jsr	CHROUT

	lda	#$10
	sta	VERA_ADDR_BANK	; Set increment to 1

	ldy	#2		; Line 2 = Y coordinate
	sty	VERA_ADDR_HIGH
	ldy	#0		; Column 0 = X coordinate
	sty	VERA_ADDR_LOW

	; Change 80 characters across screen
	ldy	#80
.topline:
	lda	#SWALL
	sta	VERA_DATA0
	lda	#SWALL_COL
	sta	VERA_DATA0
	dey
	bne	.topline

	ldy	#59		; Line = 59 = Y coordinate
	sty	VERA_ADDR_HIGH
	ldy	#0		; Column 0 = X coordinate
	sty	VERA_ADDR_LOW

	ldy	#80
.bottomline:
	lda	#SWALL
	sta	VERA_DATA0
	lda	#SWALL_COL
	sta	VERA_DATA0
	dey
	bne	.bottomline

	ldy	#3
	ldx	#0
.leftline:
	stx	VERA_ADDR_LOW
	sty	VERA_ADDR_HIGH
	lda	#SWALL
	sta	VERA_DATA0
	lda	#SWALL_COL
	sta	VERA_DATA0
	iny
	cpy	#59
	bne	.leftline

	ldy	#3
	ldx	#(79*2)
.rightline:
	stx	VERA_ADDR_LOW
	sty	VERA_ADDR_HIGH
	lda	#SWALL
	sta	VERA_DATA0
	lda	#SWALL_COL
	sta	VERA_DATA0
	iny
	cpy	#59
	bne	.rightline

	rts

; *******************************************************************
; Generates a pseudo random number. It is vital that this function
; is called in some sort of loop that has user input, otherwise it
; just generates a sequence of numbers that will be the same every
; time. In other words, the randomness of this routine comes from
; the fact that the user presses keys at random times.
; It was found here:
; https://codebase64.org/doku.php?id=base:small_fast_8-bit_prng
; *******************************************************************
; USES:		A & RANDNUM ZP variable
; *******************************************************************
;randomize:
;	lda	RANDNUM
;	beq	doeor
;	asl
;	beq	noeor
;	bcc	noeor
;doeor:	eor	#$1D
;noeor:	sta	RANDNUM
;	rts

; *******************************************************************
; Generates a pseudo random number. It is vital that this function
; is called in some sort of loop that has user input, otherwise it
; just generates a sequence of numbers that will be the same every
; time. In other words, the randomness of this routine comes from
; the fact that the user presses keys at random times.
; It was found here:
; https://wiki.nesdev.com/w/index.php/Random_number_generator
; *******************************************************************
; USES:		A
;		RANDNUM (2 bytes)
; *******************************************************************
randomize:
	lda	RANDNUM+1
;	tax
	pha
	lsr
	lsr
	lsr
	sta	RANDNUM+1
	lsr
	eor	RANDNUM+1
	lsr
	eor	RANDNUM+1
	eor	RANDNUM+0
	sta	RANDNUM+1
;	txa
	pla
	sta	RANDNUM+0
	asl
	eor	RANDNUM+0
	asl
	eor	RANDNUM+0
	asl
	asl
	asl
	eor	RANDNUM+0
	sta	RANDNUM+0
	rts

; *******************************************************************
; Ensure that the program is running in 80 column mode, set black
; background, clear the screen and write/draw the splash screen.
; *******************************************************************
; USES:		A, X & Y
;		TMP0 & TMP1 is used because of print_str function
; *******************************************************************
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
	+gotoxy 41-((.mail-.name)/2), 18
	ldx	#<.name
	ldy	#>.name
	jsr	print_str
	+gotoxy	41-((.forcx16-.mail)/2), 20
	ldx	#<.mail
	ldy	#>.mail
	jsr	print_str
	+gotoxy	41-((.pstart-.forcx16)/2), 27
	ldx	#<.forcx16
	ldy	#>.forcx16
	jsr	print_str
	+gotoxy 41-((.player_text-.pstart)/2), 36
	ldx	#<.pstart
	ldy	#>.pstart
	jsr	print_str
	+gotoxy	39, 24
	ldx	#<.xl1
	ldy	#>.xl1
	jsr	print_str
	+gotoxy 40, 25
	ldx	#<.xl2
	ldy	#>.xl2
	jsr	print_str
	+gotoxy 41, 26
	ldx	#<.xl3
	ldy	#>.xl3
	jsr	print_str
	+gotoxy 41, 28
	ldx	#<.xl5
	ldy	#>.xl5
	jsr	print_str
	+gotoxy 40, 29
	ldx	#<.xl6
	ldy	#>.xl6
	jsr	print_str
	+gotoxy 39, 30
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
