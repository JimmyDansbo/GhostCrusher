*=$0801
!byte $0C,$08,$0A,$00,$9E,$20,$32,$30,$36,$34,$00,$00,$00
*=$0810

RANDNUM		=	$00		; 2 bytes for RANDOM seed
IRQ_TRIG	=	RANDNUM+2	; ZP address to show if IRQ triggered
LEVEL		=	IRQ_TRIG+1	; ZP address to store current level
LIVES		=	LEVEL+1		; ZP address to store number of lives
NUMGHOSTS	=	LIVES+1		; Number of 'normal' ghosts
NUMPGHOSTS	=	NUMGHOSTS+1	; Number of polter geists
NUMDGHOSTS	=	NUMPGHOSTS+1	; Number of dimentional ghosts
NUMPORTALS	=	NUMDGHOSTS+1	; Number of portals
POINTS		=	NUMPORTALS+1	; 3 bytes
JIFFIES		=	POINTS+3
PLAYER_DELAY	=	JIFFIES+1

	jmp	main

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

	lda	#60
	sta	JIFFIES

	lda	#20
	sta	PLAYER_DELAY

	lda	#2
	sta	NUMGHOSTS	; NUMGHOSTS = 2
	lda	#0
	sta	IRQ_TRIG	; IRQ_TRIG = 0
	sta	NUMPGHOSTS	; NUMPGHOSTS = 0
	sta	NUMDGHOSTS	; NUMDGHOSTS = 0
	sta	NUMPORTALS	; NUMPORTALS = 0
	sta	POINTS		; POINTS = 000000 (BCD)
	sta	POINTS+1
	sta	POINTS+2
	rts

; *******************************************************************
; Find a random number between .min and .max
; *******************************************************************
; USES:		A
; *******************************************************************
!macro rand .min, .max {
-	jsr	randomize
	cmp	#(.max-.min)
	bcs	-
	clc
	adc	#.min
}

!src "x16.inc"
!src "text.inc"

; *******************************************************************
; Starting point of program
; *******************************************************************
main:
	jsr	init_vars
	jsr	splash_screen

	+vera_init
	+save_int_vector
	+install_int_handler

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
	jsr	place_ghosts
	jsr	place_player

	; This is the main loop, it will check the IRQ_TRIG variable
	; each time it is set, it will call the do_game function and
	; reset the IRQ_TRIG variable. This means that the do_game
	; function will be called 60 times a second
game_loop:
	+check_irq
	jmp	game_loop
	rts


do_game:
	jsr	do_clock
	jsr	do_player
	rts

; *******************************************************************
; *******************************************************************
do_player:
	rts

; *******************************************************************
; Ensure that the clock is updated every second. The function
; expects that VERA is does not increment
; *******************************************************************
; USES:		A & Y
; *******************************************************************
do_clock:
	dec	JIFFIES		; Decrement JIFFIES and return
	beq	.do_update	; if it is not 0
	rts
.do_update:
	lda	#60		; Reset JIFFIES back to 60
	sta	JIFFIES		; There are 60 Jiffies in a second
	lda	#1		; The clock is located on line 1
	sta	VERA_ADDR_HIGH
;Do low sec
	lda	#79*2		; Set VERA to address of low part
	sta	VERA_ADDR_LOW	; of seconds
	ldy	VERA_DATA0	; Load low part of seconds
	iny
	cpy	#$3A		; If it is not equal to $3A
	bne	.do_write	; write it out and return
	ldy	#$30		; Else set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
;Do high sec
	lda	#78*2		; Set VERA to address of high part
	sta	VERA_ADDR_LOW	; of seconds
	ldy	VERA_DATA0	; Load high part of seconds
	iny
	cpy	#$36		; If it is not equal to $36 ='6'
	bne	.do_write	; write it out and return
	ldy	#$30		; Else set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
;Do low min
	lda	#76*2		; Set VERA to address of low part
	sta	VERA_ADDR_LOW	; of minutes
	ldy	VERA_DATA0	; Load low part of minutes
	iny
	cpy	#$3A		; If it is not equal to $3A
	bne	.do_write	; write it out and return
	ldy	#$30		; Else set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
;Do high min
	lda	#75*2		; Set VERA to address of high part
	sta	VERA_ADDR_LOW	; of minutes
	ldy	VERA_DATA0	; Load high part of minutes
	iny
	cpy	#$36		; If it is not equal to $36 = '6'
	bne	.do_write	; write it out and return
	ldy	#$30		; Else set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
;Do low hour
	lda	#73*2		; Set VERA to address of low part
	sta	VERA_ADDR_LOW	; of hours
	ldy	VERA_DATA0	; Load low part of hours
	iny
	cpy	#$3A		; If it is not equal to $3A
	bne	.do_write	; Write it out and return
	ldy	#$30		; Else set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
;Do high hour
	lda	#72*2		; Set VERA to address of high part
	sta	VERA_ADDR_LOW	; of hours
	ldy	VERA_DATA0	; Load high part of hours
	iny
	cpy	#$3A		; If it is not equal to $3A
	bne	.do_write	; Write it out and return
	ldy	#$30		; Esle set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
	rts
.do_write:
	sty	VERA_DATA0	; Write to screen
	rts

; *******************************************************************
; Place player randomly on the playing field
; *******************************************************************
; USES:
; *******************************************************************
place_player:
	jsr	find_empty
	lda	#PLAYER
	sta	VERA_DATA0
	inc	VERA_ADDR_LOW
	lda	#PLAY_COL
	sta	VERA_DATA0
	rts

; *******************************************************************
; Find an empty place on the playing field and set VERA address
; to point at it: This function assumes that VERA increment is 0
; *******************************************************************
; USES:		A
; *******************************************************************
find_empty:
	+rand	3, 59
	sta	VERA_ADDR_HIGH
	+rand	1, 79
	asl
	sta	VERA_ADDR_LOW
	lda	VERA_DATA0
	cmp	#' '
	bne	find_empty
	rts

; *******************************************************************
; Place ghosts and portals randomly on the playing field
; *******************************************************************
; INPUT:	Global ZP variables decides how many of each will
;		be placed.
; USES:
; *******************************************************************
place_ghosts:
	ldy	#0
	sty	VERA_ADDR_BANK		; No increment
	ldy	NUMGHOSTS
.doghosts:
	beq	.startpghosts
	; Handle ghosts
	jsr	find_empty
	lda	#GHOST
	sta	VERA_DATA0		; Write ghost
	inc	VERA_ADDR_LOW
	lda	#GHOST_COL		; Write ghost color
	sta	VERA_DATA0
	dey
	jmp	.doghosts
.startpghosts:
	ldy	NUMPGHOSTS
.dopghosts:
	beq	.startportals
	; Handle poltergeists
	jsr	find_empty
	lda	#PGHOST
	sta	VERA_DATA0		; Write ghost
	inc	VERA_ADDR_LOW
	lda	#GHOST_COL		; Write ghost color
	sta	VERA_DATA0
	dey
	jmp	.dopghosts
.startportals:
	ldy	NUMPORTALS
.doportals:
	beq	.pg_end
	; Handle portals
	jsr	find_empty
	lda	#PORTAL
	sta	VERA_DATA0		; Write ghost
	inc	VERA_ADDR_LOW
	lda	#GHOST_COL		; Write ghost color
	sta	VERA_DATA0
	dey
	jmp	.doportals
.pg_end:
	rts

; *******************************************************************
; Place walls randomly on the playing field
; *******************************************************************
; INPUT:	A * 256 wall chars will be written
; USES:		TMP0 and TMP1 for counters
; *******************************************************************
place_walls:
	sta	TMP0
	lda	#0
	sta	TMP1
	sta	VERA_ADDR_BANK		; No Increment

.out_loop:
.in_loop:
	jsr	find_empty
	lda	#WALL			; Place a wall char
	sta	VERA_DATA0
	inc	TMP1
	bne	.in_loop
	dec	TMP0
	bne	.out_loop
	rts

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
; Stores 1 in the IRQ_TRIG variable on eache VSYNC interrupt
; Uses A, but it is restored by KERNAL when the original intterupt
; handler is called.
; *******************************************************************
handle_irq:
	lda	VERA_ISR
	and	#1			; Is this VSYNC?
	beq	.vsync_end		; if not, end
	sta	IRQ_TRIG
.vsync_end:
	jmp	(old_irq_handler)	; Continue to original

; *******************************************************************
; Clear the playing field
; *******************************************************************
; USES:		A, X & Y
; *******************************************************************
clear_field:
	lda	#$10
	sta	VERA_ADDR_BANK		; Increment by 1
	ldy	#3			; Start on line 3

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
; Draw the border around the playing field with the Static Wall
; characters
; *******************************************************************
; USES:		A, X & Y
; *******************************************************************
draw_border:
	lda	#WALL_COL	; Clear screen with black background
	sta	COLOR_PORT
	lda	#147		; Do the actual clear
	jsr	CHROUT

	+gotoxy	0, 1
	ldx	#<.top_text
	ldy	#>.top_text
	jsr	print_str

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
; https://wiki.nesdev.com/w/index.php/Random_number_generator
; *******************************************************************
; USES:		A
;		RANDNUM (2 bytes)
; *******************************************************************
randomize:
	lda	RANDNUM+1
;	tax
	pha			; Using stack instead of reg X
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
	pla			; It is fast enough to use the stack
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
