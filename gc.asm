!to "gc.prg", cbm

*=$0801
!byte $0C,$08,$0A,$00,$9E,' ','2','0','6','4',$00,$00,$00
*=$0810

; *******************************************************************
; Definitions of variables in Zero Page
; *******************************************************************
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
PLAYER_SPEED	=	$1A
JOY_DELAY	=	$1B
LAST_DIR	=	$1C
NUMWALLS	=	$1D
NUMSWALLS	=	$1E

DIR_DOWN	= 1
DIR_LEFT	= 2
DIR_RIGHT	= 3
DIR_UP		= 4

; *******************************************************************
; Find a random number between .min and .max
; *******************************************************************
; USES:		.A
; *******************************************************************
!macro RAND .min, .max {
-	jsr	randomize
	cmp	#(.max-.min)
	bcs	-
	clc
	adc	#.min
}

; *******************************************************************
; Calculate new coordinates according to the direction stored in .A
; *******************************************************************
; INPUTS:	.A = direction
; *******************************************************************
!macro NEW_CORD .xcord, .ycord {
	cmp	#DIR_DOWN
	bne	.is_left
	inc	.ycord
	jmp	.endm
.is_left:
	cmp	#DIR_LEFT
	bne	.is_right
	dec	.xcord
	jmp	.endm
.is_right:
	cmp	#DIR_RIGHT
	bne	.is_up
	inc	.xcord
	jmp	.endm
.is_up:
	dec	.ycord
.endm:
}

; *******************************************************************
; Write .num to screen using VERA, .num must be BCD encoded
; *******************************************************************
; USES:		.A
; *******************************************************************
!macro WRITE_BCD_NUM .num {
	lda	.num
	lsr			; Move high nibble to low
	lsr
	lsr
	lsr
	ora	#$30		; Convert to PETSCII
	sta	VERA_DATA0
	lda	.num
	and	#$0F		; Remove high nibble
	ora	#$30		; Convert to PETSCII
	inc	VERA_ADDR_LOW	; Move to next char on screen
	inc	VERA_ADDR_LOW
	sta	VERA_DATA0
}

; *******************************************************************
; Add the total number of ghosts and write it to screen
; *******************************************************************
; USES:		.X, .Y
; RETURNS:	.Y = Total number of ghosts (BCD encoded).
; *******************************************************************
!macro SUM_GHOSTS {
	sed			; Turn BCD mode on
	lda	NUMGHOSTS	; Add up all the ghosts
	clc
	adc	NUMPORTALS
	adc	NUMPGHOSTS
	adc	NUMDGHOSTS
	cld			; Turn BCD mode off
	tay			; Store the total number of ghosts
	lsr			; Move high nibble to low nibble
	lsr
	lsr
	lsr
	ora	#$30		; Convert to petscii code
	tax			; Save result in .X
	lda	#0		; Coordinates 35x0
	sta	VERA_ADDR_HIGH
	lda	#35*2
	sta	VERA_ADDR_LOW
	stx	VERA_DATA0	; Write petscii digit
	inc	VERA_ADDR_LOW	; Coordinate 36x0
	inc	VERA_ADDR_LOW
	tya			; Restore total number of ghosts
	and	#$0F		; Remove top nibble
	ora	#$30		; Convert to petscii code
	sta	VERA_DATA0	; Write petscii digit
}

; *******************************************************************
; Add points to the totabl number of points
; *******************************************************************
; USES:		.A
; *******************************************************************
!macro ADD_POINTS .points {
	sed
	lda	POINTS+2	; Load low-byte of POINTS
	clc			; Ensure Carry is clear
	adc	#.points	; Add .points
	sta	POINTS+2	; Store low-byte of POINTS
	bcc	.end		; If carry is clear, we can end
	lda	POINTS+1	; Load mid-byte of POINTS
	adc	#0		; Add zero to actually add carry
	sta	POINTS+1	; Store mid-byte of POINTS
	bcc	.end		; If carry is clear, we can end
	lda	POINTS		; Load high-byte of POINTS
	adc	#0		; Add zero to actually add carry
	sta	POINTS		; Store high-byte of POINTS
.end:
	cld

	; Move cursor to 8,1. 3rd option tells macro that we are using
	; immediate values instead of variables
	+VERA_GO_XY 8, 1, 1
	+WRITE_BCD_NUM POINTS	; Write high-byte of POINTS to screen
	inc	VERA_ADDR_LOW
	inc	VERA_ADDR_LOW
	+WRITE_BCD_NUM POINTS+1	; Write mid-byte of POINTS to screen
	inc	VERA_ADDR_LOW
	inc	VERA_ADDR_LOW
	+WRITE_BCD_NUM POINTS+2	; Write low-byte of POINTS to screen
}

; *******************************************************************
; Includes
; *******************************************************************
!src "x16.inc"
!src "text.inc"
!src "vera.inc"

	jmp	main

; *******************************************************************
; Initialize ZP variables with correct values for start
; *******************************************************************
; USES:		A
; *******************************************************************
init_vars:
	lda	#5		; LIVES = 5
	sta	LIVES
	sta	RANDNUM
	sta	RANDNUM+1

	lda	#60
	sta	JIFFIES

	lda	#10
	sta	JOY_DELAY

	lda	#5
	sta	PLAYER_DELAY
	sta	PLAYER_SPEED

	lda	#0
	sta	IRQ_TRIG	; IRQ_TRIG = 0
	sta	POINTS		; POINTS = 000000 (BCD)
	sta	POINTS+1
	sta	POINTS+2
	rts

; *******************************************************************
; Level definitions
; *******************************************************************
Levels			; Level structure, total size 8 bytes
	!word	$47DE	; Random seed
	!byte	4	; Number of static walls
	!byte	85	; Number of walls
	!byte	2	; Number of ghosts
	!byte	0	; Number of portals
	!byte	0	; Number of poltergeists
	!byte	0	; Number of dimentional ghosts
	!word	$0000	; Random seed
	!byte	4	; Number of static walls
	!byte	85	; Number of walls
	!byte	3	; Number of ghosts
	!byte	0	; Number of portals
	!byte	0	; Number of poltergeists
	!byte	0	; Number of dimentional ghosts

; *******************************************************************
; Starting point of program
; *******************************************************************
main:

	jsr	init_vars
	jsr	splash_screen

	+VERA_INIT
	+SAVE_INT_VECTOR
	+INSTALL_INT_HANDLER handle_irq

	ldy	#1
	jsr	load_level

	; Wait for user to start game, use the time to
	; to do random numbers
@start_wait:
	jsr	randomize
	jsr	JOY_SCAN
	lda	#0		; Select first joystick
	jsr	JOY_GET

	and	#NES_STA
	beq	@start_wait

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

	lda	#0		; Go to 40x30 mode
	sec
	jsr	SCRMOD

	jsr	draw_border

;	jsr	clear_field

	jsr	place_swalls	; Place static walls
	jsr	place_walls	; Place walls
	jsr	place_ghosts	; Place ghosts according to ZP variables
	jsr	place_player
	jsr	write_level	; Write the current level to screen

	; This is the main loop, it will check the IRQ_TRIG variable
	; each time it is set, it will call the do_game function and
	; reset the IRQ_TRIG variable. This means that the do_game
	; function will be called 60 times a second
@game_loop:
	lda	IRQ_TRIG	; Load IRQ_TRIG
	beq	@game_loop
	; VSYNC IRQ has occurred, handle

	jsr	do_clock
	jsr	do_player
	jsr	do_getjoy

	lsr	IRQ_TRIG	; Reset IRQ_TRIG

	jmp	@game_loop

@main_end:			; So far, we never get here
	rts

; *******************************************************************
; Write the current level on screen
; *******************************************************************
; INPUTS:	LEVEL
; USES:		.A
; *******************************************************************
write_level:
	+VERA_GO_XY 22,0,1
	+WRITE_BCD_NUM LEVEL
	rts

; *******************************************************************
; Load level information into global zeropage variables
; *******************************************************************
; INPUTS:	.Y level to load
; USES:		.A & .Y
; OUTPUTS:	LEVEL, RANDSEED, NUMSWALLS, NUMWALLS,
;		NUMGHOSTS, NUMPORTALS, NUMPGHOSTS & NUMDGHOSTS
; *******************************************************************
load_level:
	sty	LEVEL		; Store level
	lda	#>Levels	; TMP0 pointer to start of Levels
	sta	TMP1
	lda	#<Levels
	sta	TMP0
@find_level:
	dey			; When Y reaches 0, we have found level
	beq	@do_load
	clc			; Add 8 to the TMP0 pointer to point to
	adc	#8		; next level
	sta	TMP0
	lda	TMP1
	adc	#0
	sta	TMP1
	jmp	@find_level	; Go back and see if we found right level
@do_load:
	lda	(TMP0),y	; Random Seed
	sta	RANDSEED+0
	iny
	lda	(TMP0),y
	sta	RANDSEED+1

	iny
	lda	(TMP0),y	; Number of static walls
	sta	NUMSWALLS

	iny
	lda	(TMP0),y	; Number of walls / 5
	sta	NUMWALLS

	iny
	lda	(TMP0),y	; Number of ghosts
	sta	NUMGHOSTS

	iny
	lda	(TMP0),y	; Number of portals
	sta	NUMPORTALS

	iny
	lda	(TMP0),y	; Number of poltergeists
	sta	NUMPGHOSTS

	iny
	lda	(TMP0),y	; Number of dimentional ghosts
	sta	NUMDGHOSTS
	rts

; *******************************************************************
; Get joystick input from 1st joystick and store in ZP varables
; *******************************************************************
; USES:		.A, .Y & TMP0
; *******************************************************************
do_getjoy:
	lda	#0		; Select first joystick
	jsr	JOY_GET
	sta	TMP0		; Save current joystick state
	cmp	#$FF		; If no key is pressed on joystick
	bne	@joy_start
	sta	LAST_DIR	; Make sure to reset LAST_DIR and
	lda	#10		; JOY_DELAY variables
	sta	JOY_DELAY
	jmp	@end

@joy_start:
	ldy	#1		; .Y is used to store into direction vars

	lda	TMP0		; Restore current joystick state
	and	#JOY_DN		; Is Down-key preseed?
	bne	@check_left	; If not check Left-key

	lda	TMP0		; Restore jystick state
	cmp	LAST_DIR
	bne	@mv_dn		; If it is equal to last time (key is held)
	lda	JOY_DELAY	; See if JOY_DELAY=0
	beq	@mv_dn		; If it does, we can move
	dec	JOY_DELAY	; Otherwise decrement JOY_DELAY =
	jmp	@end		; wait for 10 jiffies

@mv_dn:	sty	BTN_DN		; Store 1 in BTN_DN
	sta	LAST_DIR	; Save current direction
	jmp	@end

@check_left:
	lda	TMP0		; Restore current joystick state
	and	#JOY_LT		; Is Left-key pressed?
	bne	@check_right	; If not check Right-key

	lda	TMP0		; Restore jystick state
	cmp	LAST_DIR
	bne	@mv_lt		; If it is equal to last time (key is held)
	lda	JOY_DELAY	; See if JOY_DELAY=0
	beq	@mv_lt		; If it does, we can move
	dec	JOY_DELAY	; Otherwise decrement JOY_DELAY =
	jmp	@end		; wait for 10 jiffies

@mv_lt:	sty	BTN_LT		; Store 1 in BTN_LT
	sta	LAST_DIR	; Save current direction
	jmp	@end
@check_right:
	lda	TMP0		; Restore current joystick state
	and	#JOY_RT		; Is Right-key pressed?
	bne	@check_up	; If not check Up-key

	lda	TMP0		; Restore jystick state
	cmp	LAST_DIR
	bne	@mv_rt		; If it is equal to last time (key is held)
	lda	JOY_DELAY	; See if JOY_DELAY=0
	beq	@mv_rt		; If it does, we can move
	dec	JOY_DELAY	; Otherwise decrement JOY_DELAY =
	jmp	@end		; wait for 10 jiffies

@mv_rt:	sty	BTN_RT		; Store 1 in BTN_RT
	sta	LAST_DIR	; Save current direction
	jmp	@end
@check_up:
	lda	TMP0		; Restore current joystick state
	and	#JOY_UP		; Is UP-key pressed?
	bne	@end		; If not jump to end

	lda	TMP0		; Restore jystick state
	cmp	LAST_DIR
	bne	@mv_up		; If it is equal to last time (key is held)
	lda	JOY_DELAY	; See if JOY_DELAY=0
	beq	@mv_up		; If it does, we can move
	dec	JOY_DELAY	; Otherwise decrement JOY_DELAY =
	jmp	@end		; wait for 10 jiffies

@mv_up:	sty	BTN_UP		; Store 1 in BTN_UP
	sta	LAST_DIR	; Save current direction
@end:
	rts

; *******************************************************************
; Function check if it is possible to move in the direction the user
; has chosen.
; *******************************************************************
; USES:		.A, .Y, TMP0, TMP1, TMP2, TMP3
; RETURNS:	.Z set if move is possible
; *******************************************************************
can_move:
@dir 		= TMP2
@prev_field	= TMP3
@cord_x		= TMP0
@cord_y		= TMP1
	sta	@dir		; Save direction in TMP2
	lda	#0
	sta	@prev_field	; Hold previous field
-	lda	@dir

	+NEW_CORD @cord_x, @cord_y

	lda	@cord_y		; Write Y to VERA
	sta	VERA_ADDR_HIGH
	lda	@cord_x		; Load X
	asl
	sta	VERA_ADDR_LOW	; Write X to VERA
	lda	VERA_DATA0	; Load char at address

	cmp	#' '		; If this is not a clear space, check next
	bne	@is_ghost
	lda	@prev_field	; Load previous field
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
	rts
@is_ghost:
	cmp	#GHOST		; If this is not ghost, check next
	bne	@is_pghost
	lda	@prev_field	; Load previous field
	cmp	#WALL		; If it is a WALL field, we need to
	beq	+		; see what next field is
	lda	#0		; Else return that we can move
	rts			; (allthough it will kill the player)
+	ldy	#GHOST		; Save the current field
	sty	@prev_field
	jmp	-		; Check next field

@is_pghost:
	cmp	#PGHOST		; If this is not pghost, check next
	bne	@is_dghost
	lda	@prev_field	; Load previous field
	cmp	#WALL		; If it is a WALL field, we need to
	beq	+		; see what next field is
	lda	#0		; Else return that we can move
	rts
+	ldy	#PGHOST		; Save the current field
	sty	@prev_field
	jmp	-		; Check next field
@is_dghost:
	cmp	#DGHOST		; If this is not dghost, check next
	bne	@is_portal
	lda	@prev_field	; Load previous field
	cmp	#WALL		; If it is a WALL field, we need to
	beq	+		; see what next field is
	lda	#0		; Else return that we can move
	rts
+	ldy	#DGHOST		; Save the current field
	sty	@prev_field
	jmp	-		; Check next field
@is_portal:
	cmp	#PORTAL		; If this is not portal, check next
	bne	@is_wall
	lda	@prev_field	; Load previous field
	cmp	#WALL		; If it is a WALL field, we need to
	beq	+		; see what next field is
	lda	#0		; Else return that we can move
	rts
+	ldy	#PORTAL		; Save the current field
	sty	@prev_field
	jmp	-		; Check next field
@is_wall:
	cmp	#WALL		; If this is not wall, check next
	bne	@is_swall
	lda	@prev_field	; Load previous field
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
	sty	@prev_field
	jmp	-		; Check next field
@is_swall:
	lda	@prev_field	; Load previous field
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
; Move player in the direction chosen. This function assumes that it
; is possible to move in the direction chosen.
; *******************************************************************
; INPUTS:	.A containing the direction to move
;		PLAYER_X & PLAYER_Y current coordinates of player
; OUTPUTS:	PLAYER_X & PLAYER_Y new coordinates of player
; *******************************************************************
do_move:
@cord_x = TMP0
@cord_y	= TMP1
@prev_field = TMP3
@dir	= TMP2

	sta	@dir		; Save direction

	ldx	PLAYER_X	; Save current coordinates to do
	stx	@cord_x		; calculations on them
	ldx	PLAYER_Y
	stx	@cord_y

	+NEW_CORD @cord_x, @cord_y
	+VERA_GO_XY @cord_x, @cord_y
	inc	VERA_ADDR_LOW	; We want the color
	lda	VERA_DATA0	; Save what was where player moves to
	sta	@prev_field
	dec	VERA_ADDR_LOW	; Now we overwrite both char and color

	lda	#PLAYER		; Draw player at new place
	sta	VERA_DATA0
	inc	VERA_ADDR_LOW
	lda	#PLAY_COL
	sta	VERA_DATA0

	+VERA_GO_XY PLAYER_X, PLAYER_Y
	lda	#' '		; Draw a space where player was
	sta	VERA_DATA0
	inc	VERA_ADDR_LOW
	lda	#PLAY_COL
	sta	VERA_DATA0

	lda	@cord_y		; Set new player coordinates
	sta	PLAYER_Y
	lda	@cord_x
	sta	PLAYER_X

	lda	@prev_field	; Load color of the field that was overwritten
	cmp	#GHOST_COL	; If it is a ghost, the player dies
	bne	+		; else
	jsr	player_died	; Handle that player died
	rts

+	cmp	#PLAY_COL	; If it is PLAY_COL, it is an empty field and
	bne	@move_wall	; We are done.
	rts

	; Handle moving of walls.
@move_wall:
	lda	@dir
	+NEW_CORD @cord_x, @cord_y
	+VERA_GO_XY @cord_x, @cord_y
	lda	VERA_DATA0
	sta	@prev_field	; Save what is on current position
	inc	VERA_ADDR_LOW	; All ghosts have same color so check the color
	lda	VERA_DATA0
	cmp	#GHOST_COL	; Have we squashed a ghost
	bne	+
	lda	#WALL_COL	; Write the WALL color
	sta	VERA_DATA0
	dec	VERA_ADDR_LOW
	lda	#WALL		; Write the WALL character
	sta	VERA_DATA0
	jsr	ghost_killed	; Handle a dead ghost
	rts
+	cmp	#PLAY_COL	; Empty field
	bne	+
	lda	#WALL_COL	; Write the WALL color
	sta	VERA_DATA0
	dec	VERA_ADDR_LOW
	lda	#WALL		; Write the WALL character
	sta	VERA_DATA0
	rts

+	jmp	@move_wall
; END of do_move function

; *******************************************************************
; Handle the "crushing" of a ghost, that means add points and
; finish level when all ghosts are killed.
; *******************************************************************
; USES:		.A, .X & .Y
; *******************************************************************
ghost_killed:
@prev_field = TMP3
	lda	@prev_field	; Load the ghost that was 'killed'
	cmp	#GHOST		; is it a normal ghost?
	beq	+		; if not check next
	jmp	@is_portal
+	dec	NUMGHOSTS	; Remove 1 normal ghost
	+ADD_POINTS $15		; Normal ghost gives 15 points
	jmp	@addit
@is_portal:
	cmp	#PORTAL		; is it a portal?
	beq	+
	jmp	@is_pghost	; if not check next
+	dec	NUMPORTALS	; Remove 1 portal
	+ADD_POINTS $10		; Portal gives 10 points (it does not move)
	jmp	@addit
@is_pghost:
	cmp	#PGHOST		; is it a poltergeist?
	beq	+
	jmp	@is_dghost	; if not check next
+	dec	NUMPGHOSTS	; Remove 1 poltergeist
	+ADD_POINTS $20		; Poltergesit give 20 points
	jmp	@addit
@is_dghost:			; then it must be a dimentional ghost
	dec	NUMDGHOSTS	; Remove 1 dimentional ghost
	+ADD_POINTS $30		; Dimentional ghosts gives 30 points

@addit:
	+SUM_GHOSTS
	tya
	bne	+
	; Handle that all ghosts are killed
	!byte $ff
+	rts

; *******************************************************************
; *******************************************************************
player_died:
	jsr	*
	rts

; *******************************************************************
; *******************************************************************
do_player:
	dec	PLAYER_DELAY
	beq	@do_move
	rts
@do_move:
	lda	PLAYER_SPEED
	sta	PLAYER_DELAY

	lda	PLAYER_X	; Save current coordinates to do
	sta	TMP0		; calculations on them ?
	lda	PLAYER_Y
	sta	TMP1

	lda	BTN_DN
	beq	.btn_lt
	; Handle down button
	lsr	BTN_DN
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
	lsr	BTN_LT
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
	lsr	BTN_RT
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
	lsr	BTN_UP
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
	lda	VERA_ADDR_HIGH	; Save Y coordinate
	sta	PLAYER_Y
	lda	VERA_ADDR_LOW	; Save X coordinate
	lsr
	sta	PLAYER_X
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
; USES:		.A
; *******************************************************************
find_empty:
	+RAND	3, 29		; Find number between 3 and 29
	sta	VERA_ADDR_HIGH	; Set it as Y coordinate
	+RAND	1, 39		; Find number between 1 and 39
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
	beq	.startdghosts
	; Handle portals
	jsr	find_empty
	lda	#PORTAL
	sta	VERA_DATA0		; Write ghost
	inc	VERA_ADDR_LOW
	lda	#GHOST_COL		; Write ghost color
	sta	VERA_DATA0
	dec	TMP0
	jmp	.doportals
.startdghosts:
	ldy	NUMDGHOSTS
	sty	TMP0
.dodghosts:
	beq	.pg_end
	; Handle dimentional ghosts
	jsr	find_empty
	lda	#DGHOST
	sta	VERA_DATA0		; Write ghost
	inc	VERA_ADDR_LOW
	lda	#GHOST_COL		; Write ghost color
	sta	VERA_DATA0
	dec	TMP0
	jmp	.dodghosts
.pg_end:
	rts

; *******************************************************************
; Place walls randomly on the playing field
; *******************************************************************
; INPUT:	NUMWALLS * 5 wall chars will be written
; USES:		TMP0 and TMP1 for counters
; *******************************************************************
place_walls:
	lda	NUMWALLS
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
	inc	VERA_ADDR_LOW
	lda	#WALL_COL
	sta	VERA_DATA0
	dec	TMP1
	bne	.in_loop
	dec	TMP0
	bne	.out_loop
	rts

; *******************************************************************
; Place a number of Static Wall Chars randomly in the playing field
; *******************************************************************
; INPUTS:	NUMSWALLS = number of Static Walls to place
; USES:		.A & .Y
; *******************************************************************
place_swalls:
	ldy	NUMSWALLS
-	+RAND 2, 37
	asl				; Multiply by 2 for X coord
	sta	VERA_ADDR_LOW

	+RAND 4, 27
	sta	VERA_ADDR_HIGH

	lda	#SWALL			; Set the SWALL character
	sta	VERA_DATA0
	lda	#SWALL_COL		; Set the SWALL_COL color
	sta	VERA_DATA0
	dey				; While Y > 0
	bne	-			; jump back to place wall

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
	jmp	(Old_irq_handler)	; Continue to original

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
	lda	#PLAY_COL
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
	lda	#PLAY_COL	; Clear screen with black background
	sta	COLOR_PORT
	lda	#147		; Do the actual clear
	jsr	CHROUT

	+GOTO_XY 0, 0
	ldx	#<.top_line1
	ldy	#>.top_line1
	jsr	print_str
	+GOTO_XY 0, 1
	ldx	#<.top_line2
	ldy	#>.top_line2
	jsr	print_str

	lda	#$00
	sta	VERA_ADDR_BANK	; Set increment to 0
	+SUM_GHOSTS

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

	lda	#PLAY_COL		; Set black background
	sta	COLOR_PORT

	lda	#147			; Clear Screen
	jsr	CHROUT

	+GOTO_XY 2, 10
	ldx	#<.gc1
	ldy	#>.gc1
	jsr	print_str
	+GOTO_XY 2, 11
	ldx	#<.gc2
	ldy	#>.gc2
	jsr	print_str
	+GOTO_XY 2, 12
	ldx	#<.gc3
	ldy	#>.gc3
	jsr	print_str
	+GOTO_XY 2, 13
	ldx	#<.gc4
	ldy	#>.gc4
	jsr	print_str
	+GOTO_XY 2, 14
	ldx	#<.gc5
	ldy	#>.gc5
	jsr	print_str
	+GOTO_XY 41-((.mail-.name)/2), 18
	ldx	#<.name
	ldy	#>.name
	jsr	print_str
	+GOTO_XY 41-((.forcx16-.mail)/2), 20
	ldx	#<.mail
	ldy	#>.mail
	jsr	print_str
	+GOTO_XY 41-((.pstart-.forcx16)/2), 27
	ldx	#<.forcx16
	ldy	#>.forcx16
	jsr	print_str
	+GOTO_XY 41-((.player_text-.pstart)/2), 36
	ldx	#<.pstart
	ldy	#>.pstart
	jsr	print_str
	+GOTO_XY 39, 24
	ldx	#<.xl1
	ldy	#>.xl1
	jsr	print_str
	+GOTO_XY 40, 25
	ldx	#<.xl2
	ldy	#>.xl2
	jsr	print_str
	+GOTO_XY 41, 26
	ldx	#<.xl3
	ldy	#>.xl3
	jsr	print_str
	+GOTO_XY 41, 28
	ldx	#<.xl5
	ldy	#>.xl5
	jsr	print_str
	+GOTO_XY 40, 29
	ldx	#<.xl6
	ldy	#>.xl6
	jsr	print_str
	+GOTO_XY 39, 30
	ldx	#<.xl7
	ldy	#>.xl7
	jsr	print_str
	+GOTO_XY 35, 40
	ldx	#<.player_text
	ldy	#>.player_text
	jsr	print_str
	+GOTO_XY 35, 42
	ldx	#<.portal_text
	ldy	#>.portal_text
	jsr	print_str
	+GOTO_XY 35, 44
	ldx	#<.ghost_text
	ldy	#>.ghost_text
	jsr	print_str
	+GOTO_XY 35, 46
	ldx	#<.pghost_text
	ldy	#>.pghost_text
	jsr	print_str
	+GOTO_XY 35, 48
	ldx	#<.dghost_text
	ldy	#>.dghost_text
	jsr	print_str
	+GOTO_XY 35, 50
	ldx	#<.wall_text
	ldy	#>.wall_text
	jsr	print_str
	+GOTO_XY 35, 52
	ldx	#<.swall_text
	ldy	#>.swall_text
	jsr	print_str
	rts
