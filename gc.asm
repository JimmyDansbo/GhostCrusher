!to "gc.prg", cbm

*=$0801
!byte $0C,$08,$0A,$00,$9E,' ','2','0','6','4',$00,$00,$00
*=$0810

RANDNUM		=	$00		; 2 bytes for RANDOM seed
IRQ_TRIG	=	$02		; ZP address to show if IRQ triggered
LEVEL		=	$03		; ZP address to store current level
LIVES		=	$04		; ZP address to store number of lives
NUMGHOSTS	=	$05		; Number of 'normal' ghosts
NUMPGHOSTS	=	$06		; Number of polter geists
NUMDGHOSTS	=	$07		; Number of dimentional ghosts
NUMPORTALS	=	$08		; Number of portals
POINTS		=	$09		; 3 bytes
BTN_UP		=	$0C
BTN_DN		=	$0D
BTN_LT		=	$0E
BTN_RT		=	$0F
JIFFIES		=	$10
PLAYER_DELAY	=	$11
PLAYER_X	=	$12
PLAYER_Y	=	$13
RANDSEED	=	$18		; 2 bytes

DIR_DOWN	= 1
DIR_LEFT	= 2
DIR_RIGHT	= 3
DIR_UP		= 4

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
!src "vera.inc"

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
	lda	#0		; Select first joystick
	jsr	JOY_GET
	and	#NES_STA
	beq	.start_wait

	ldx	#0		; If there is 0 in RANDSEED
	cpx	RANDSEED	; We save RANDNUM in RANDSEED
	bne	+		; Otherwise we copy RANDSEED to RANDNUM
	ldx	RANDNUM		; This makes it possible to make levels
	stx	RANDSEED	; that can be loaded by setting RANDSEED
	ldx	RANDNUM+1
	stx	RANDSEED+1
+	ldx	RANDSEED
	stx	RANDNUM
	ldx	RANDSEED+1
	stx	RANDNUM+1

	lda	#0		; Go to 40x30
	sec
	jsr	SCRMOD

	jsr	draw_border
	ldy	#4		; Place 4 static walls
	jsr	place_swalls
	lda	#85		; Place 85*5 walls
	jsr	place_walls

	jsr	place_ghosts	; Place ghosts according to ZP variables
	jsr	place_player

	; This is the main loop, it will check the IRQ_TRIG variable
	; each time it is set, it will call the do_game function and
	; reset the IRQ_TRIG variable. This means that the do_game
	; function will be called 60 times a second
game_loop:
	+check_irq do_game
	jmp	game_loop

.main_end:			; So far, we never get here
	rts


do_game:
	jsr	do_getjoy
	jsr	do_clock
	jsr	do_player
	rts

; *******************************************************************
; Get joystick input from 1st joystick and store in ZP varables
; *******************************************************************
; USES:		.A, .Y & TMP0
; *******************************************************************
do_getjoy:
	jsr	JOY_SCAN
	lda	#0		; Select first joystick
	jsr	JOY_GET
	ldy	#1
	sta	TMP0		; Save current joystick state
	and	#JOY_DN
	bne	.check_lt
	sty	BTN_DN
;	jmp	+
.check_lt:
;	lsr	BTN_DN
+	lda	TMP0
	and	#JOY_LT
	bne	.check_rt
	sty	BTN_LT
;	jmp	+
.check_rt:
;	lsr	BTN_LT
+	lda	TMP0
	and	#JOY_RT
	bne	.check_up
	sty	BTN_RT
;	jmp	+
.check_up:
;	lsr	BTN_RT
+	lda	TMP0
	and	#JOY_UP
	bne	.dgj_end
	sty	BTN_UP
;	rts
.dgj_end
;	lsr	BTN_UP
	rts

; *******************************************************************
; *******************************************************************
can_move:
.dir 		= TMP2
.prev_field	= TMP3
.cord_x		= TMP0
.cord_y		= TMP1
	sta	.dir		; Save direction in TMP2
	lda	#0
	sta	.prev_field	; Hold previous field
-	lda	.dir
	cmp	#DIR_DOWN
	bne	.is_lt
	inc	.cord_y		; INC Y
	jmp	.do_find
.is_lt:
	cmp	#DIR_LEFT
	bne	.is_rt
	dec	.cord_x		; DEC X
	jmp	.do_find
.is_rt:
	cmp	#DIR_RIGHT
	bne	.is_up
	inc	.cord_x		; INC X
	jmp	.do_find
.is_up:
	dec	.cord_y		; DEC Y

.do_find:
	lda	.cord_y		; Write Y to VERA
	sta	VERA_ADDR_HIGH
	lda	.cord_x		; Load X
	asl
	sta	VERA_ADDR_LOW	; Write X to VERA
	lda	VERA_DATA0	; Load char at address

	cmp	#' '		; If this is not a clear space, check next
	bne	.is_ghost
	lda	.prev_field	; Load previous field
	cmp	#GHOST
	beq	+
	cmp	#PGHOST
	beq	+
	cmp	#DGHOST
	beq	+
	cmp	#PORTAL
	beq	+
	lda	#0
	rts
+	lda	#1
.is_ghost:
	cmp	#GHOST		; If this is not ghost, check next
	bne	.is_pghost
	lda	.prev_field	; Load previous field
	cmp	#WALL		; If it is a WALL field, we need to
	beq	+		; see what next field is
	lda	#0		; Else return that we can move
	rts			; (allthough it will kill the player)
+	ldy	#GHOST		; Save the current field
	sty	.prev_field
	jmp	-		; Check next field

.is_pghost:
	cmp	#PGHOST		; If this is not pghost, check next
	bne	.is_dghost
	lda	.prev_field	; Load previous field
	cmp	#WALL		; If it is a WALL field, we need to
	beq	+		; see what next field is
	lda	#0		; Else return that we can move
	rts
+	ldy	#PGHOST		; Save the current field
	sty	.prev_field
	jmp	-		; Check next field
.is_dghost:
	cmp	#DGHOST		; If this is not dghost, check next
	bne	.is_portal
	lda	.prev_field	; Load previous field
	cmp	#WALL		; If it is a WALL field, we need to
	beq	+		; see what next field is
	lda	#0		; Else return that we can move
	rts
+	ldy	#DGHOST		; Save the current field
	sty	.prev_field
	jmp	-		; Check next field
.is_portal:
	cmp	#PORTAL		; If this is not portal, check next
	bne	.is_wall
	lda	.prev_field	; Load previous field
	cmp	#WALL		; If it is a WALL field, we need to
	beq	+		; see what next field is
	lda	#0		; Else return that we can move
	rts
+	ldy	#PORTAL		; Save the current field
	sty	.prev_field
	jmp	-		; Check next field
.is_wall:
	cmp	#WALL		; If this is not wall, check next
	bne	.is_swall
	lda	.prev_field	; Load previous field
	cmp	#GHOST		; If previous is not ghost, check next
	bne	+
	lda	#0		; Set zero flag and return (we can move)
	rts
+	cmp	#PORTAL		; If previous is not portal, check next
	bne	+
	lda	#0		; Set zero flag and return (we can move)
	rts
+	cmp	#PGHOST		; If previous is not pghost, check next
	bne	+
	lda	#1		; Reset zero and return (we can not move)
	rts
+	cmp	#DGHOST		; If previous is not dghost, check next
	bne	+
	lda	#1		; Reset zero and return (we can not move)
	rts
+	ldy	#WALL		; Save the current field
	sty	.prev_field
	jmp	-		; Check next field
.is_swall:
	lda	.prev_field	; Load previous field
	cmp	#GHOST		; If previous was not ghost, check next
	bne	+
	lda	#0		; Set zero and return (we can move)
	rts
+	cmp	#PORTAL		; If previous was not portal, check next
	bne	+
	lda	#0		; Set zero and return (we can move)
	rts
+	cmp	#DGHOST		; If previous was not dghost, check next
	bne	+
	lda	#0		; Set zero and return (we can move)
	rts
+	cmp	#PGHOST		; If previous was not pghost, check next
	bne	+
	lda	#0		; Set zero and return (we can move)
	rts
+	lda	#1		; Reset zero and return (we can not move)
	rts

; *******************************************************************
; *******************************************************************
do_move:
.cord_x = TMP0
.cord_y	= TMP1
;.prev_field = TMP3		Set in can_move function

	ldx	PLAYER_X	; Save current coordinates to do
	stx	.cord_x		; claculations on them ?
	ldx	PLAYER_Y
	stx	.cord_y

	cmp	#DIR_DOWN
	bne	.is_le
	inc	.cord_y		; INC Y
	jmp	.do_move
.is_le:
	cmp	#DIR_LEFT
	bne	.is_ri
	dec	.cord_x		; DEC X
	jmp	.do_move
.is_ri:
	cmp	#DIR_RIGHT
	bne	.is_north
	inc	.cord_x		; INC X
	jmp	.do_move
.is_north:
	dec	.cord_y		; DEC Y

.do_move:
	+vera_goxy .cord_x, .cord_y
	inc	VERA_ADDR_LOW	; We want the color
	lda	VERA_DATA0	; Save what was where player moves to
	sta	.prev_field
	dec	VERA_ADDR_LOW	; Now we overwrite both char and color

	lda	#PLAYER		; Draw player at new place
	sta	VERA_DATA0
	inc	VERA_ADDR_LOW
	lda	#PLAY_COL
	sta	VERA_DATA0

	+vera_goxy PLAYER_X, PLAYER_Y
	lda	#' '		; Draw a space where player was
	sta	VERA_DATA0
	inc	VERA_ADDR_LOW
	lda	#PLAY_COL
	sta	VERA_DATA0

	lda	.cord_y		; Set new player coordinates
	sta	PLAYER_Y
	lda	.cord_x
	sta	PLAYER_X

	lda	.prev_field
	cmp	#GHOST_COL
	

	rts

; *******************************************************************
; *******************************************************************
do_player:
	dec	PLAYER_DELAY
	beq	.do_player
	rts
.do_player:
	lda	#20		; This should be a variable
	sta	PLAYER_DELAY

	lda	PLAYER_X	; Save current coordinates to do
	sta	TMP0		; claculations on them ?
	lda	PLAYER_Y
	sta	TMP1

	lda	BTN_DN
	beq	.btn_lt
	; Handle down button
	lsr	BTN_DN		; Reset BTN_DN back to 0
	lda	#DIR_DOWN
	jsr	can_move
	bne	.dp_end
	lda	#DIR_DOWN
	jsr	do_move
	jmp	.dp_end
.btn_lt:
	lda	BTN_LT
	beq	.btn_rt
	; Handle left button
	lsr	BTN_LT		; Reset BTN_LT back to 0
	lda	#DIR_LEFT
	jsr	can_move
	bne	.dp_end
	lda	#DIR_LEFT
	jsr	do_move
	jmp	.dp_end
.btn_rt:
	lda	BTN_RT
	beq	.btn_up
	; Handle right button
	lsr	BTN_RT		; Reset BTN_RT back to 0
	lda	#DIR_RIGHT
	jsr	can_move
	bne	.dp_end
	lda	#DIR_RIGHT
	jsr	do_move
	jmp	.dp_end
.btn_up:
	lda	BTN_UP
	beq	.dp_end
	; Handle up button
	lsr	BTN_UP		; Reset BTN_UP back to 0
	lda	#DIR_UP
	jsr	can_move
	bne	.dp_end
	lda	#DIR_UP
	jsr	do_move

.dp_end:
	rts

; *******************************************************************
; Ensure that the clock is updated every second. The function
; expects that VERA does not increment
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
	lda	#39*2		; Set VERA to address of low part
	sta	VERA_ADDR_LOW	; of seconds
	ldy	VERA_DATA0	; Load low part of seconds
	iny
	cpy	#$3A		; If it is not equal to $3A
	bne	.do_write	; write it out and return
	ldy	#$30		; Else set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
;Do high sec
	lda	#38*2		; Set VERA to address of high part
	sta	VERA_ADDR_LOW	; of seconds
	ldy	VERA_DATA0	; Load high part of seconds
	iny
	cpy	#$36		; If it is not equal to $36 ='6'
	bne	.do_write	; write it out and return
	ldy	#$30		; Else set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
;Do low min
	lda	#36*2		; Set VERA to address of low part
	sta	VERA_ADDR_LOW	; of minutes
	ldy	VERA_DATA0	; Load low part of minutes
	iny
	cpy	#$3A		; If it is not equal to $3A
	bne	.do_write	; write it out and return
	ldy	#$30		; Else set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
;Do high min
	lda	#35*2		; Set VERA to address of high part
	sta	VERA_ADDR_LOW	; of minutes
	ldy	VERA_DATA0	; Load high part of minutes
	iny
	cpy	#$36		; If it is not equal to $36 = '6'
	bne	.do_write	; write it out and return
	ldy	#$30		; Else set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
;Do low hour
	lda	#33*2		; Set VERA to address of low part
	sta	VERA_ADDR_LOW	; of hours
	ldy	VERA_DATA0	; Load low part of hours
	iny
	cpy	#$3A		; If it is not equal to $3A
	bne	.do_write	; Write it out and return
	ldy	#$30		; Else set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
;Do high hour
	lda	#32*2		; Set VERA to address of high part
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
; USES:		.A, .X & .Y
; *******************************************************************
place_player:
	jsr	find_empty	; Find an empty field
	sty	PLAYER_Y	; Save X and Y coordinates
	stx	PLAYER_X
	lda	#PLAYER		; Draw the player char
	sta	VERA_DATA0
	inc	VERA_ADDR_LOW	; Set the correct color
	lda	#PLAY_COL
	sta	VERA_DATA0
	rts

; *******************************************************************
; Find an empty place on the playing field and set VERA address
; to point at it: This function assumes that VERA increment is 0
; *******************************************************************
; USES:		A
; *******************************************************************
; RETURNS:	X and Y as the coordinates found
; *******************************************************************
find_empty:
	+rand	3, 29		; Find number between 3 and 29
	sta	VERA_ADDR_HIGH	; Set it as Y coordinate
	tay			; Save it in Y register
	+rand	1, 39		; Find number between 1 and 39
	tax			; Save it in X register
	asl			; Multiply by 2
	sta	VERA_ADDR_LOW	; Set it as X coordinate
	lda	VERA_DATA0	; Read character at coordinate
	cmp	#' '
	bne	find_empty	; If char != ' ', try again
	rts

; *******************************************************************
; Place ghosts and portals randomly on the playing field
; *******************************************************************
; INPUT:	Global ZP variables decides how many of each will
;		be placed.
; USES:		A & Y
; *******************************************************************
place_ghosts:
	ldy	#0
	sty	VERA_ADDR_BANK		; No increment
	ldy	NUMGHOSTS		; Copy NUMGHOSTS to TMP to
	sty	TMP0			; use it as counter
.doghosts:
	beq	.startpghosts
	; Handle ghosts
	jsr	find_empty
	lda	#GHOST
	sta	VERA_DATA0		; Write ghost
	inc	VERA_ADDR_LOW
	lda	#GHOST_COL		; Write ghost color
	sta	VERA_DATA0
	dec	TMP0
	jmp	.doghosts
.startpghosts:
	ldy	NUMPGHOSTS
	sty	TMP0
.dopghosts:
	beq	.startportals
	; Handle poltergeists
	jsr	find_empty
	lda	#PGHOST
	sta	VERA_DATA0		; Write ghost
	inc	VERA_ADDR_LOW
	lda	#GHOST_COL		; Write ghost color
	sta	VERA_DATA0
	dec	TMP0
	jmp	.dopghosts
.startportals:
	ldy	NUMPORTALS
	sty	TMP0
.doportals:
	beq	.pg_end
	; Handle portals
	jsr	find_empty
	lda	#PORTAL
	sta	VERA_DATA0		; Write ghost
	inc	VERA_ADDR_LOW
	lda	#GHOST_COL		; Write ghost color
	sta	VERA_DATA0
	dec	TMP0
	jmp	.doportals
.pg_end:
	rts

; *******************************************************************
; Place walls randomly on the playing field
; *******************************************************************
; INPUT:	A * 5 wall chars will be written
; USES:		TMP0 and TMP1 for counters
; *******************************************************************
place_walls:
	sta	TMP0
	lda	#0
	sta	VERA_ADDR_BANK		; No Increment

.out_loop:
	lda	#5
	sta	TMP1
.in_loop:
	jsr	find_empty
	lda	#WALL			; Place a wall char
	sta	VERA_DATA0
	dec	TMP1
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
	+rand 2, 37
	asl				; Multiply by 2 for X coord
	sta	VERA_ADDR_LOW

	+rand 4, 27
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
	ldx	#38
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
	cpy	#29
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

	+gotoxy	0, 0
	ldx	#<.top_line1
	ldy	#>.top_line1
	jsr	print_str
	+gotoxy	0, 1
	ldx	#<.top_line2
	ldy	#>.top_line2
	jsr	print_str

	lda	#$10
	sta	VERA_ADDR_BANK	; Set increment to 1

	ldy	#2		; Line 2 = Y coordinate
	sty	VERA_ADDR_HIGH
	ldy	#0		; Column 0 = X coordinate
	sty	VERA_ADDR_LOW

	; Change 40 characters across screen
	ldy	#40
.topline:
	lda	#SWALL
	sta	VERA_DATA0
	lda	#SWALL_COL
	sta	VERA_DATA0
	dey
	bne	.topline

	ldy	#29		; Line = 29 = Y coordinate
	sty	VERA_ADDR_HIGH
	ldy	#0		; Column 0 = X coordinate
	sty	VERA_ADDR_LOW

	ldy	#40
.bottomline:
	lda	#SWALL
	sta	VERA_DATA0
	lda	#SWALL_COL
	sta	VERA_DATA0
	dey
	bne	.bottomline

	ldy	#2
	ldx	#0
.leftline:
	stx	VERA_ADDR_LOW
	sty	VERA_ADDR_HIGH
	lda	#SWALL
	sta	VERA_DATA0
	lda	#SWALL_COL
	sta	VERA_DATA0
	iny
	cpy	#29
	bne	.leftline

	ldy	#2
	ldx	#(39*2)
.rightline:
	stx	VERA_ADDR_LOW
	sty	VERA_ADDR_HIGH
	lda	#SWALL
	sta	VERA_DATA0
	lda	#SWALL_COL
	sta	VERA_DATA0
	iny
	cpy	#29
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
