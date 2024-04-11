!cpu w65c02

*=$0801
!byte $0C,$08,$0A,$00,$9E,' ','2','0','6','4',$00,$00,$00
*=$0810

; *******************************************************************
; Includes
; *******************************************************************
!src "x16.inc"
!src "text.inc"
!src "vera.inc"
!src "farbranch.inc"

; *******************************************************************
; Definitions of variables in Zero Page
; *******************************************************************
RANDNUM		=	$22		; 2 bytes for RANDOM seed
IRQ_TRIG	=	RANDNUM+2	; ZP address to show if IRQ triggered
LEVEL		=	IRQ_TRIG+1	; ZP address to store current level
LIVES		=	LEVEL+1		; ZP address to store number of lives
NUMGHOSTS	=	LIVES+1		; Number of 'normal' ghosts
NUMPGHOSTS	=	NUMGHOSTS+1	; Number of polter geists
NUMDGHOSTS	=	NUMPGHOSTS+1	; Number of dimensional ghosts
NUMPORTALS	=	NUMDGHOSTS+1	; Number of portals
POINTS		=	NUMPORTALS+1	; 3 bytes
BTN_UP		=	POINTS+3
BTN_DN		=	BTN_UP+1
BTN_LT		=	BTN_DN+1
BTN_RT		=	BTN_LT+1
JIFFIES		=	BTN_RT+1
PLAYER_DELAY	=	JIFFIES+1
PLAYER_X	=	PLAYER_DELAY+1
PLAYER_Y	=	PLAYER_X+1
RANDSEED	=	PLAYER_Y+1	; 2 bytes
PLAYER_SPEED	=	RANDSEED+2
JOY_DELAY	=	PLAYER_SPEED+1
LAST_DIR	=	JOY_DELAY+1
NUMWALLS	=	LAST_DIR+1
NUMSWALLS	=	NUMWALLS+1
GHOSTS_DELAY	=	NUMSWALLS+1
GHOSTS_SPEED	=	GHOSTS_DELAY+1
PGHOSTS_DELAY	=	GHOSTS_SPEED+1
PGHOSTS_SPEED	=	PGHOSTS_DELAY+1
DGHOSTS_DELAY	=	PGHOSTS_SPEED+1
DGHOSTS_SPEED	=	DGHOSTS_DELAY+1
PORTALS_SPEED	=	DGHOSTS_SPEED+1

DIR_DOWN	= 1
DIR_LEFT	= 2
DIR_RIGHT	= 3
DIR_UP		= 4

	jmp	main

; *****************************************************************************
; Increment a 16bit value
; *****************************************************************************
; INPUT:	.num = the value to be incremented
; *****************************************************************************
!macro INC16 .num {
	inc	.num
	bne	.end
	inc	.num+1
.end:
}

; *****************************************************************************
; Decrement a 16bit value
; *****************************************************************************
; INPUT:	.num = the value to be decremented
; USES:		.A for testing for underflow
; *****************************************************************************
!macro DEC16 .num {
	lda .num
	bne .end
	dec	.num+1
.end:
	dec	.num
}

; *******************************************************************
; Find a random number between .min and .max
; *******************************************************************
; INPUT:	.min & .max indicates the range within to find number
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
;		.xcord and .ycord must contain current coordinates
; OUTPUTS:	.xcord and .ycord will contain the new coordinates
; *******************************************************************
!macro NEW_CORD .xcord, .ycord {
	cmp	#DIR_DOWN
	bne	.is_left
	inc	.ycord
	bra	.endm
.is_left:
	cmp	#DIR_LEFT
	bne	.is_right
	dec	.xcord
	bra	.endm
.is_right:
	cmp	#DIR_RIGHT
	bne	.is_up
	inc	.xcord
	bra	.endm
.is_up:
	dec	.ycord
.endm:
}

; *******************************************************************
; Write .num to screen using VERA, .num must be BCD encoded
; *******************************************************************
; INPUTS:	.num = the number to write to screen
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
	lda	#$B0+0		; Coordinates 35x0
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
; Add points to the total number of points
; *******************************************************************
; INPUTS:	.points = the amount of points to add to total
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
}

; *****************************************************************************
; Write the players current points to the screen
; *****************************************************************************
; USES:		.A
; *****************************************************************************
!macro WRITE_POINTS {
	; Move cursor to 8,1. 3rd option tells macro that we are using
	; immediate values instead of variables

	+VERA_GO_XY 8, $B0+1, 1
	+WRITE_BCD_NUM POINTS	; Write high-byte of POINTS to screen
	inc	VERA_ADDR_LOW
	inc	VERA_ADDR_LOW
	+WRITE_BCD_NUM POINTS+1	; Write mid-byte of POINTS to screen
	inc	VERA_ADDR_LOW
	inc	VERA_ADDR_LOW
	+WRITE_BCD_NUM POINTS+2	; Write low-byte of POINTS to screen
}

; *****************************************************************************
; Compare value in .A with value in .dist. If .A is smaller, update .dist
; and write .curx & .cury values to .newx and .newy variables.
; *****************************************************************************
; INPUTS:	.A & .dist = distance values to be compared
;		.curx & .cury = X & Y values that may be copied
;		.newx & .newy = variables to hold X & Y values
; OUTPUTS:	.dist, .newx & .newy (updated if .A < .dist)
; *****************************************************************************
!macro UPD_DIST_XY .dist, .curx, .cury, .newx, .newy {
	cmp	.dist		; If returned distance is shorter than the
	bcs	.end		; previous saved, save this dist and coords.
	sta	.dist
	lda	.curx
	sta	.newx
	lda	.cury
	sta	.newy
.end:
}

; *****************************************************************************
; Read and combine keyboard, joy1 & joy2 inputs
; *****************************************************************************
; USES:		.A, .X, .Y & TMPf
; OUTPUS:	.A contains the combined .A from joystick_get for all 3 "joys"
; *****************************************************************************
!macro READ_JOY {
	lda	#0
	jsr	JOY_GET
	sta	TMPf
	lda	#1
	jsr	JOY_GET
	and	TMPf
	sta	TMPf
	lda	#2
	jsr	JOY_GET
	and	TMPf
}

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
	sta	PLAYER_DELAY
	sta	PLAYER_SPEED

	lda	#10
	sta	JOY_DELAY

	lda	#60
	sta	JIFFIES

	ldy	#End_game-Portal_delay
	lda	#0
-	sta	Portal_delay,y
	dey
	bne	-

	stz	End_game
	stz	IRQ_TRIG	; IRQ_TRIG = 0
	stz	POINTS		; POINTS = 000000 (BCD)
	stz	POINTS+1
	stz	POINTS+2
	rts

; *****************************************************************************
; 2 bytes of memory to store address of original IRQ handler function
; *****************************************************************************
Old_irq_handler:
	!word	$0000

; *******************************************************************
; Level definitions
; *******************************************************************
NUM_LEVELS	=	20
Levels:			; Level structure
	; Level 1
	!word	$873B	; Random seed
	!byte	4	; Number of static walls
	!byte	85	; Number of walls
	!byte	2	; Number of ghosts
	!byte	40	; Ghost speed (number of jiffies before ghost moves)
	!byte	0	; Number of poltergeists
	!byte	0	; PGhost speed (number of jiffies before pghosts moves)
	!byte	0	; Number of dimensional ghosts
	!byte	0	; DGhost speed (number of jiffies before dghosts moves)
	!byte	0	; Number of portals
	!byte	0	; Portal delay (seconds before portal releases a ghost)

	; Level 2
	!word	$5711	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	  4,    80,    2,     40,    0,    30,    0,      25,   0,      30
	; Level 3
	!word	$28BC	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	  4,    80,    3,     40,    0,    30,    0,      25,   0,      30
	; Level 4
	!word	$CC96	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	  4,    80,    4,     40,    0,    30,    0,      25,   0,      30
	; Level 5
	!word	$5BF0	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	  4,    75,    4,     40,    0,    30,    0,      25,   0,      30
	; Level 6
	!word	$FBA0	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	  4,    70,    4,     40,    0,    30,    0,      25,   0,      30
	; Level 7
	!word	$97CF	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	 15,    70,    0,     40,    1,    30,    0,      25,   0,      30
	; Level 8
	!word	$B1AD	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	 15,    70,    1,     40,    1,    30,    0,      25,   0,      30
	; Level 9
	!word	$C2BD	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	 15,    70,    2,     40,    1,    30,    0,      25,   0,      30
	; Level 10
	!word	$E4D3	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	 15,    70,    3,     40,    1,    30,    0,      25,   0,      30
	; Level 11
	!word	$1383	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	 15,    70,    1,     40,    2,    30,    0,      25,   0,      30
	; Level 12
	!word	$C4E6	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	 15,    70,    2,     40,    2,    30,    0,      25,   0,      30
	; Level 13
	!word	$AEF0	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	 15,    70,    3,     40,    2,    30,    0,      25,   0,      30
	; Level 14
	!word	$C46F	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	 15,    70,    4,     40,    2,    30,    0,      25,   0,      30
	; Level 15
	!word	$A1B5	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	 15,    65,    4,     40,    2,    30,    0,      25,   0,      30
	; Level 16
	!word	$17A9	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	 15,    65,    4,     40,    3,    30,    0,      25,   0,      30
	; Level 17
	!word	$3966	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	 12,    65,    4,     40,    1,    30,    0,      25,   1,      30
	; Level 18
	!word	$481F	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	 12,    65,    4,     40,    2,    30,    0,      25,   2,      30
	; Level 19
	!word	$679E	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	 12,    65,    4,     40,    3,    30,    0,      25,   3,      30
	; Level 20
	!word	$1C2A	; Random seed
	;	SWALLS,WALLS,GHOSTS,SPEED,PGHOSTS,SPEED,DGHOSTS,SPEED,PORTALS,SPEED
	!byte	 10,    60,    4,     40,    4,    30,    0,      25,   4,      30


; *****************************************************************************
; X and Y coordinates for ghosts
; *****************************************************************************
MAX_GHOSTS	=	5
MAX_P_GHOSTS	=	5
MAX_D_GHOSTS	=	5
MAX_PORTALS	=	5
Ghost_X		!fill	MAX_GHOSTS+1,0
Ghost_Y		!fill	MAX_GHOSTS+1,0
D_Ghost_X	!fill	MAX_D_GHOSTS+1,0
D_Ghost_Y	!fill	MAX_D_GHOSTS+1,0
P_Ghost_X	!fill	MAX_P_GHOSTS+1,0
P_Ghost_Y	!fill	MAX_P_GHOSTS+1,0
Portal_X	!fill	MAX_PORTALS+1,0
Portal_Y	!fill	MAX_PORTALS+1,0
Portal_delay	!fill	MAX_PORTALS+1,0

End_game	!byte	$00

; *******************************************************************
; Starting point of program
; *******************************************************************
main:
	jsr	init_vars
	jsr	splash_screen

	+VERA_INIT
	+SAVE_INT_VECTOR Old_irq_handler
	+INSTALL_INT_HANDLER handle_irq

	ldy	#1
	jsr	load_level

	; Wait for user to start game, use the time to
	; to do random numbers
	jsr	wait_for_start

	lda	#3		; Go to 40x30 mode
	clc
	jsr	SCRMOD

	jsr	draw_border
	jsr	init_playfield

	; This is the main loop, it will check the IRQ_TRIG variable
	; each time it is set, it will call the functions to handle the game and
	; reset the IRQ_TRIG variable. This means that the functions
	; will be called 60 times a second
@game_loop:
	wai
	lda	IRQ_TRIG	; Load IRQ_TRIG
	beq	@game_loop
	; VSYNC IRQ has occurred, handle

+	jsr	randomize
	jsr	do_clock	; Update the clock
	jsr	do_player	; Move player according to joystick input
	jsr	do_getjoy	; Read joystick input
	jsr	do_ghosts	; Move ghosts

	lsr	IRQ_TRIG	; Reset IRQ_TRIG
	lda	End_game
	beq	@game_loop

	; Wait for user to press start/return button
	jsr	wait_for_start
	; Remove custom interrupt handler
	sei
	+RESTORE_INT_VECTOR Old_irq_handler
	cli
	; Set screen to 80x60
	lda	#0
	clc
	jsr	SCRMOD
	; Empty keyboard buffer
-	jsr	GETIN
	bne	-
	; Set blue background, white text and clear the screen
	lda	#(BLUE<<4)+WHITE
	sta	COLOR_PORT
	lda	#147
	jsr	CHROUT

	rts

; *****************************************************************************
; Calculate a fields distance to the player. Only fields that a ghost can move
; to actually have their distance calculated.
; *****************************************************************************
; INPUTS:	TMP4 & TMP5 = X & Y coordinates of field to do calculations for
; USES:		TMP6
; OUTPUT:	.A = Distance to player
; *****************************************************************************
calc_dist:
@field_x=TMP4
@field_y=TMP5

	+VERA_GO_XY @field_x, @field_y
	inc	VERA_ADDR_LOW
	lda	VERA_DATA0	; Read color of current field
	cmp	#PLAY_COL	; If it is PLAY_COL, it can be moved to
	beq	@handle_x
	lda	#$FF		; Else set Distance to $FF
	rts
@handle_x:
	lda	@field_x	; Subtract current X from players X
	sec
	sbc	PLAYER_X
	bpl	@handle_y	; If result positive handle Y coordinate
	eor	#$FF		; Else invert and inc to get positive result
	inc
@handle_y:
	sta	TMP6		; Save calculation
	lda	@field_y	; Subract current Y from players Y
	sec
	sbc	PLAYER_Y
	bpl	@store_dist	; If result positive, store the distance
	eor	#$FF		; Else invert and inc to get positive result
	inc
@store_dist:
	clc			; Add saved calculation to current for
	adc	TMP6		; complete distance
	rts

; *****************************************************************************
; Move ghosts in the direction of the player if there are any open spaces
; *****************************************************************************
; INPUTS:	TMP0 & TMP1 = Pointer to ghost X coordinate array
;		TMP2 & TMP3 = Pointer to ghost Y coordinate array
; USES:		.A, .X, .Y, TMP4-TMP9
; *****************************************************************************
move_ghost:
; Local variable names
@x_arr=TMP0
@y_arr=TMP2
@field_x=TMP4
@field_y=TMP5
@dist=TMP7
@new_x=TMP8
@new_y=TMP9

	ldy	#0		; .Y used as index in X,Y arrays
@loop:
	stz	@new_x		; Reset distance and new coordinates
	stz	@new_y
	lda	#$FF
	sta	@dist

	lda	(@x_arr),y	; Load current x coord
	+FBEQ	@end		; If it is zero, we are done
	sta	@field_x	; Store current x coord in ZP variable
	lda	(@y_arr),y
	sta	@field_y	; Store current y coord in ZP variable

	dec	@field_x	; Top left field
	dec	@field_y	; X-1, Y-1
	jsr	calc_dist

	+UPD_DIST_XY @dist, @field_x, @field_y, @new_x, @new_y

	inc	@field_x	; Top middle field
	jsr	calc_dist	; X,   Y-1

	+UPD_DIST_XY @dist, @field_x, @field_y, @new_x, @new_y

	inc	@field_x	; Top right field
	jsr	calc_dist	; X+1, Y-1

	+UPD_DIST_XY @dist, @field_x, @field_y, @new_x, @new_y

	inc	@field_y	; Right middle field
	jsr	calc_dist	; X+1, Y

	+UPD_DIST_XY @dist, @field_x, @field_y, @new_x, @new_y

	inc	@field_y	; Bottom right field
	jsr	calc_dist	; X+1, Y+1

	+UPD_DIST_XY @dist, @field_x, @field_y, @new_x, @new_y

	dec	@field_x	; Bottom middle field
	jsr	calc_dist	; X,   Y+1

	+UPD_DIST_XY @dist, @field_x, @field_y, @new_x, @new_y

	dec	@field_x	; Bottom left field
	jsr	calc_dist	; X-1, Y+1

	+UPD_DIST_XY @dist, @field_x, @field_y, @new_x, @new_y

	dec	@field_y	; Left middle field
	jsr	calc_dist	; X-1, Y

	+UPD_DIST_XY @dist, @field_x, @field_y, @new_x, @new_y

	lda	@new_x		; If new_x is 0, no moveable fields were found
	bne	@ghost_move	; Check next
	iny
	jmp	@loop		; Check next ghost

@ghost_move:
	lda	(@y_arr),y
	sta	VERA_ADDR_HIGH
	lda	(@x_arr),y
	asl
	sta	VERA_ADDR_LOW
	ldx	VERA_DATA0	; Save ghost character
	lda	#' '		; Write an empty field...
	sta	VERA_DATA0
	inc	VERA_ADDR_LOW	; with PLAY_COL color
	lda	#PLAY_COL
	sta	VERA_DATA0

	lda	@new_y		; Save new ghost coordinates
	sta	(@y_arr),y	; In X and Y array and in VERA
	sta	VERA_ADDR_HIGH
	lda	@new_x
	sta	(@x_arr),y
	asl
	sta	VERA_ADDR_LOW

	lda	VERA_DATA0	; Save what we are overwriting
	stx	VERA_DATA0	; Write the ghost character
	tax
	inc	VERA_ADDR_LOW
	lda	#GHOST_COL
	sta	VERA_DATA0

	cpx	#PLAYER		; If we overwrote the PLAYER character, the
	+FBEQ	player_died	; Player has died. (jmp instead of jsr to
				; avoid returning to this function)
	iny
	jmp	@loop		; Check next ghost
@end
	rts

; *****************************************************************************
; Handle moving of ghosts as well as converting portals to dimensional ghosts
; *****************************************************************************
; *****************************************************************************
do_ghosts:
@x_coord=TMP0
@y_coord=TMP2
	lda	NUMGHOSTS
	beq	@pghosts	; If there are no ghosts, check poltergeists
	; Move normal ghosts
	dec	GHOSTS_DELAY	; Only move ghosts after the delay is 0
	bne	@pghosts
	lda	GHOSTS_SPEED	; Reset the delay for next move
	sta	GHOSTS_DELAY
	lda	#<Ghost_X	; Use zp memory as pointers to ghost X & Y
	sta	@x_coord
	lda	#>Ghost_X
	sta	@x_coord+1
	lda	#<Ghost_Y
	sta	@y_coord
	lda	#>Ghost_Y
	sta	@y_coord+1
	jsr	move_ghost
@pghosts:
	lda	NUMPGHOSTS
	beq	@dghosts	; If there are no poltergeists, check dimensional
	; Move poltergeists
	dec	PGHOSTS_DELAY
	bne	@dghosts
	lda	PGHOSTS_SPEED
	sta	PGHOSTS_DELAY
	lda	#<P_Ghost_X
	sta	@x_coord
	lda	#>P_Ghost_X
	sta	@x_coord+1
	lda	#<P_Ghost_Y
	sta	@y_coord
	lda	#>P_Ghost_Y
	sta	@y_coord+1
	jsr	move_ghost
@dghosts:
	lda	NUMDGHOSTS
	beq	@portals	; If there are no dimensional ghosts, check portals
	; move dimensional ghosts
	dec	DGHOSTS_DELAY
	bne	@portals
	lda	DGHOSTS_SPEED
	sta	DGHOSTS_DELAY
	lda	#<D_Ghost_X
	sta	@x_coord
	lda	#>D_Ghost_X
	sta	@x_coord+1
	lda	#<D_Ghost_Y
	sta	@y_coord
	lda	#>D_Ghost_Y
	sta	@y_coord+1
	jsr	move_ghost
@portals:
	lda	NUMPORTALS
	beq	@end		; If there are no portals, just continue
	; Convert portal to dimensional ghost after delay
	lda	JIFFIES		; Only count down once a second
	cmp	#35
	bne	@end

	ldx	#0
-	dec	Portal_delay,x
	bne	+
	bra	convert_portal	; convert and return to caller
+	inx
	cpx	NUMPORTALS
	bne	-
@end:
	rts

; *****************************************************************************
; Convert a portal to a dimensional ghost
; *****************************************************************************
; INPUTS:	.X = index of portal that needs to be converted
; USES:		.A & .Y
; *****************************************************************************
convert_portal:
	ldy	NUMDGHOSTS
	lda	Portal_X,x
	sta	TMP0
	sta	D_Ghost_X,y
	asl
	sta	VERA_ADDR_LOW
	lda	Portal_Y,x
	sta	TMP1
	sta	D_Ghost_Y,y
	clc
	adc #$B0
	sta	VERA_ADDR_HIGH
	lda	#DGHOST
	sta	VERA_DATA0

	inc	NUMDGHOSTS

	lda	#<Portal_X
	sta	TMP2
	lda	#>Portal_X
	sta	TMP3
	lda	#<Portal_Y
	sta	TMP4
	lda	#>Portal_Y
	sta	TMP5
	jmp	rem_ghost_coords
;	rts

; *****************************************************************************
; Handle that the player has died. Remove a life and restart current level
; *****************************************************************************
; *****************************************************************************
player_died:
	dec	LIVES
	beq	+
	ldy	LEVEL
	jsr	load_level
	jmp	init_playfield
+
	jsr	write_lives
	+GOTO_XY 13, 13
	ldx	#<Game_over1
	ldy	#>Game_over1
	jsr	print_str
	+GOTO_XY 13, 14
	ldx	#<Game_over2
	ldy	#>Game_over2
	jsr	print_str
	+GOTO_XY 13, 15
	ldx	#<Game_over3
	ldy	#>Game_over3
	jsr	print_str
	+GOTO_XY 13, 16
	ldx	#<Game_over4
	ldy	#>Game_over4
	jsr	print_str
	+GOTO_XY 13, 17
	ldx	#<Game_over5
	ldy	#>Game_over5
	jsr	print_str
	lda	#$FF
	sta	End_game
	rts

; *****************************************************************************
; Wait for the Start button (or return) to be pressed ensuring to handle the
; different ways the program can be started in the emulator and also ensuring
; that the randomize function is called while waiting.
; *****************************************************************************
; USES:		.A, .X & .Y
; *****************************************************************************
wait_for_start:
	lda	#0		; Read 1st joystick (or keyboard)
	jsr	JOY_GET
	cmp	#$00		; If 0, program is started in emu from cmdline
	beq	@emu_run

@wait_for_settle:		; When run normally, the return key is read
	jsr	randomize	; several times after program is actually
	lda	#0		; Started, so wait for the JOY_GET function
	jsr	JOY_GET		; to settle and return no key presses
	cmp	#$FF
	bne	@wait_for_settle
	bra	@norm_run

@emu_run:			; When run form cmdline, we just wait for the
	jsr	randomize	; result to be something other than 0
	+READ_JOY
	cmp	#$00
	beq	@emu_run

@norm_run:			; When we reach this point, we can expect the
	jsr	randomize	; JOY_GET function to behave as documented and
	+READ_JOY		; we can loop until user has pressed start
	and	#NES_STA	; or return
	bne	@norm_run
	rts

; *******************************************************************
; Call the functions needed to initialize the entire playing field.
; *******************************************************************
; INPUTS:	Global Zero Page variables must be set
; USES:		.A, .X, Y
; *******************************************************************
init_playfield:
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

	jsr	zero_ghost_coords
	jsr	clear_field
	jsr	place_swalls	; Place static walls
	jsr	place_walls	; Place walls
	jsr	place_ghosts	; Place ghosts according to ZP variables
	jsr	place_player
	jsr	write_level	; Write the current level to screen
	jsr	write_lives	; Write the current amount of lives
	+SUM_GHOSTS
	rts

; *****************************************************************************
; Ensure that all ghost coordinates are zeroed out.
; *****************************************************************************
; USES:		.X
; *****************************************************************************
zero_ghost_coords:
	ldx	#Portal_delay-Ghost_X	; Size of all coordinate arrays
-	dex
	stz	Ghost_X,x
	bne	-
	rts

; *******************************************************************
; Write the current amount of lives
; *******************************************************************
; INPUTS:	LIVES
; USES:		.A
; *******************************************************************
write_lives:
	+VERA_GO_XY 10,$B0+0,1
	+WRITE_BCD_NUM LIVES
	rts

; *******************************************************************
; Write the current level on screen
; *******************************************************************
; INPUTS:	LEVEL
; USES:		.A & TMP0
; *******************************************************************
write_level:
	+VERA_GO_XY 22,$B0+0,1
	ldy	LEVEL
	lda	int_2_bcd,y
	sta	TMP0
	+WRITE_BCD_NUM TMP0
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
@level_ptr=TMP0
	sty	LEVEL		; Store level
	lda	#>Levels	; TMP0 pointer to start of Levels
	sta	@level_ptr+1
	lda	#<Levels
	sta	@level_ptr
@find_level:
	dey			; When Y reaches 0, we have found level
	beq	@do_load
	lda	@level_ptr
	clc			; Add 12 to the level pointer to point to
	adc	#12		; next level
	sta	@level_ptr
	lda	@level_ptr+1
	adc	#0
	sta	@level_ptr+1
	jmp	@find_level	; Go back and see if we found right level
@do_load:
	lda	(@level_ptr),y	; Random Seed
	sta	RANDSEED+0
	iny
	lda	(@level_ptr),y
	sta	RANDSEED+1

	iny
	lda	(@level_ptr),y	; Number of static walls
	sta	NUMSWALLS

	iny
	lda	(@level_ptr),y	; Number of walls / 5
	sta	NUMWALLS

	iny
	lda	(@level_ptr),y	; Number of ghosts
	sta	NUMGHOSTS

	iny
	lda	(@level_ptr),y	; Ghost delay/speed
	sta	GHOSTS_DELAY
	sta	GHOSTS_SPEED

	iny
	lda	(@level_ptr),y	; Number of poltergeists
	sta	NUMPGHOSTS

	iny
	lda	(@level_ptr),y	; Poltergeist delay/speed
	sta	PGHOSTS_DELAY
	sta	PGHOSTS_SPEED

	iny
	lda	(@level_ptr),y	; Number of dimensional ghosts
	sta	NUMDGHOSTS

	iny
	lda	(@level_ptr),y	; Dimensional ghost delay/speed
	sta	DGHOSTS_DELAY
	sta	DGHOSTS_SPEED

	iny
	lda	(@level_ptr),y	; Number of portals
	sta	NUMPORTALS

	iny
	lda	(@level_ptr),y	; Portal delay

	ldy	NUMPORTALS	; If > 0, then store portal delay
	beq	@end
	ldy	#0		; 1st portal uses portal delay, following
-	sta	Portal_delay,y	; portals wait in increments of 2 seconds
	inc
	inc
	iny
	cpy	NUMPORTALS
	bne	-
@end:
	rts

; *******************************************************************
; Get joystick input from 1st joystick and store in ZP varables
; *******************************************************************
; USES:		.A, .Y & TMP0
; *******************************************************************
do_getjoy:
	lda	#0		; Select first joystick
;	jsr	JOY_GET
	+READ_JOY
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
	stz	@prev_field	; Hold previous field

-	lda	@dir
	+NEW_CORD @cord_x, @cord_y
	+VERA_GO_XY @cord_x, @cord_y
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
	jmp	player_died	; Handle that player died

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
	jmp	ghost_killed	; Handle a dead ghost
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

; *****************************************************************************
; Remove a killed ghosts X and Y coordinates from the appropriate arrays.
; This function ensures that values are moved together so there are no
; "holes" in the coordinate arrays.
; *****************************************************************************
; INPUTS:	TMP0 & TMP1 = dead ghosts coordinates
;		TMP2 & TMP3 = pointer to Ghost_X array
;		TMP4 & TMP5 = pointer to Ghost_Y array
; USES:		.A, .X & .Y
; *****************************************************************************
rem_ghost_coords:
@cord_x = TMP0
@cord_y	= TMP1
@ghost_x = TMP2
@ghost_y = TMP4
	ldy	#0		; Use .Y as index
@check_x:
	lda	(@ghost_x),y	; Check if X coordinates match
	cmp	@cord_x
	beq	@check_y	; If x matches, check if y also matches
	iny			; Else increment .Y and check next element
	bra	@check_x

@check_y:
	lda	(@ghost_y),y	; Chck if Y coordinates match
	cmp	@cord_y
	beq	@do_remove	; If y matches, remove coordinates from arrays
	iny			; Else increment .Y and check next element
	bra	@check_x

	; Remove coordinates and ensure to move all coordinates after this
	; down 1 byte to ensure there are no "holes" in the array
@do_remove:
	lda	@ghost_x	; If we are removing a portal (found by checking
	cmp	#<Portal_X	; low-byte of variable address) we need to
	bne	+		; remove entries from Portal_delay array
	iny
	lda	Portal_delay,y
	dey
	sta	Portal_delay,y

+	iny			; Load next X and Y coordinates
	lda	(@ghost_x),y
	tax
	lda	(@ghost_y),y
	dey			; Overwrite current X and Y coordinates
	sta	(@ghost_y),y
	txa
	sta	(@ghost_x),y
	iny			; Check if next X coordinate
	lda	(@ghost_x),y
	bne	@do_remove	; If it is not 0, loop back to move next set
@end:				; of coordinates, else exit function
	rts

; *******************************************************************
; Handle the "crushing" of a ghost, that means add points and
; finish level when all ghosts are killed.
; *******************************************************************
; USES:		.A, .X & .Y
; *******************************************************************
ghost_killed:
@prev_field = TMP3
@ghost_x = TMP2
@ghost_y = TMP4
	lda	@prev_field	; Load the ghost that was 'killed'
	cmp	#GHOST		; is it a normal ghost?
	beq	+		; if not check next
	jmp	@is_portal
+	lda	#<Ghost_X
	sta	@ghost_x
	lda	#>Ghost_X
	sta	@ghost_x+1
	lda	#<Ghost_Y
	sta	@ghost_y
	lda	#>Ghost_Y
	sta	@ghost_y+1
	jsr	rem_ghost_coords
	dec	NUMGHOSTS	; Remove 1 normal ghost
	+ADD_POINTS $15		; Normal ghost gives 15 points
	jmp	@addit
@is_portal:
	cmp	#PORTAL		; is it a portal?
	beq	+
	jmp	@is_pghost	; if not check next
+	lda	#<Portal_X
	sta	@ghost_x
	lda	#>Portal_X
	sta	@ghost_x+1
	lda	#<Portal_Y
	sta	@ghost_y
	lda	#>Portal_Y
	sta	@ghost_y+1
	jsr	rem_ghost_coords
; remove portal delay
	dec	NUMPORTALS	; Remove 1 portal
	+ADD_POINTS $10		; Portal gives 10 points (it does not move)
	jmp	@addit
@is_pghost:
	cmp	#PGHOST		; is it a poltergeist?
	beq	+
	jmp	@is_dghost	; if not check next
+	lda	#<P_Ghost_X
	sta	@ghost_x
	lda	#>P_Ghost_X
	sta	@ghost_x+1
	lda	#<P_Ghost_Y
	sta	@ghost_y
	lda	#>P_Ghost_Y
	sta	@ghost_y+1
	jsr	rem_ghost_coords
	dec	NUMPGHOSTS	; Remove 1 poltergeist
	+ADD_POINTS $20		; Poltergesit give 20 points
	jmp	@addit
@is_dghost:			; then it must be a dimensional ghost
	lda	#<D_Ghost_X
	sta	@ghost_x
	lda	#>D_Ghost_X
	sta	@ghost_x+1
	lda	#<D_Ghost_Y
	sta	@ghost_y
	lda	#>D_Ghost_Y
	sta	@ghost_y+1
	jsr	rem_ghost_coords
	dec	NUMDGHOSTS	; Remove 1 dimensional ghost
	+ADD_POINTS $30		; Dimensional ghosts gives 30 points

@addit:	+WRITE_POINTS
	+SUM_GHOSTS
	tya
	bne	+
	; Handle that all ghosts are killed
	ldy	LEVEL
	iny
	cpy	#NUM_LEVELS+1
	beq	@game_over
	jsr	load_level
	jmp	init_playfield
@game_over:
	+GOTO_XY 13, 13
	ldx	#<Game_over1
	ldy	#>Game_over1
	jsr	print_str
	+GOTO_XY 13, 14
	ldx	#<Game_over2
	ldy	#>Game_over2
	jsr	print_str
	+GOTO_XY 13, 15
	ldx	#<Game_over3
	ldy	#>Game_over3
	jsr	print_str
	+GOTO_XY 13, 16
	ldx	#<Game_over4
	ldy	#>Game_over4
	jsr	print_str
	+GOTO_XY 13, 17
	ldx	#<Game_over5
	ldy	#>Game_over5
	jsr	print_str
	lda	#$FF
	sta	End_game
+	rts

; *****************************************************************************
; Handle player inputs, ensuring that moves are valid and that player only
; moves after PLAYER_SPEED jiffies
; *****************************************************************************
; INPUTS:	Global zp variables containing player information
; USES:		.A, TMP0 & TMP1
; *****************************************************************************
do_player:
	dec	PLAYER_DELAY
	beq	@do_move
@dp_end:
	rts
@do_move:
	lda	PLAYER_SPEED
	sta	PLAYER_DELAY

	lda	PLAYER_X	; Save current coordinates to do
	sta	TMP0		; calculations on them ?
	lda	PLAYER_Y
	sta	TMP1

	lda	BTN_DN
	beq	@btn_lt
	; Handle down button
	lsr	BTN_DN
	lda	#DIR_DOWN
	jsr	can_move
	bne	@dp_end
	lda	#DIR_DOWN
	jmp	do_move		; Call do_move and return from this function
@btn_lt:
	lda	BTN_LT
	beq	@btn_rt
	; Handle left button 
	lsr	BTN_LT
	lda	#DIR_LEFT
	jsr	can_move
	bne	@dp_end
	lda	#DIR_LEFT
	jmp	do_move		; Call do_move and return from this function
@btn_rt:
	lda	BTN_RT
	beq	@btn_up
	; Handle right button
	lsr	BTN_RT
	lda	#DIR_RIGHT
	jsr	can_move
	bne	@dp_end
	lda	#DIR_RIGHT
	jmp	do_move		; Call do_move and return from this function
@btn_up:
	lda	BTN_UP
	beq	@dp_end
	; Handle up button
	lsr	BTN_UP
	lda	#DIR_UP
	jsr	can_move
	bne	@dp_end
	lda	#DIR_UP
	jmp	do_move		; Call do_move and return from this function

; *******************************************************************
; Ensure that the clock is updated every second. The function
; expects that VERA does not increment
; *******************************************************************
; USES:		A & Y
; *******************************************************************
do_clock:
	dec	JIFFIES		; Decrement JIFFIES and return
	beq	@do_update	; if it is not 0
	rts
@do_update:
	lda	#60		; Reset JIFFIES back to 60
	sta	JIFFIES		; There are 60 Jiffies in a second
	lda	#$B0+1		; The clock is located on line 1
	sta	VERA_ADDR_HIGH
;Do low sec
	lda	#39*2		; Set VERA to address of low part
	sta	VERA_ADDR_LOW	; of seconds
	ldy	VERA_DATA0	; Load low part of seconds
	iny
	cpy	#$3A		; If it is not equal to $3A
	bne	@do_write	; write it out and return
	ldy	#$30		; Else set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
;Do high sec
	lda	#38*2		; Set VERA to address of high part
	sta	VERA_ADDR_LOW	; of seconds
	ldy	VERA_DATA0	; Load high part of seconds
	iny
	cpy	#$36		; If it is not equal to $36 ='6'
	bne	@do_write	; write it out and return
	ldy	#$30		; Else set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
;Do low min
	lda	#36*2		; Set VERA to address of low part
	sta	VERA_ADDR_LOW	; of minutes
	ldy	VERA_DATA0	; Load low part of minutes
	iny
	cpy	#$3A		; If it is not equal to $3A
	bne	@do_write	; write it out and return
	ldy	#$30		; Else set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
;Do high min
	lda	#35*2		; Set VERA to address of high part
	sta	VERA_ADDR_LOW	; of minutes
	ldy	VERA_DATA0	; Load high part of minutes
	iny
	cpy	#$36		; If it is not equal to $36 = '6'
	bne	@do_write	; write it out and return
	ldy	#$30		; Else set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
;Do low hour
	lda	#33*2		; Set VERA to address of low part
	sta	VERA_ADDR_LOW	; of hours
	ldy	VERA_DATA0	; Load low part of hours
	iny
	cpy	#$3A		; If it is not equal to $3A
	bne	@do_write	; Write it out and return
	ldy	#$30		; Else set to $30 = '0'
	sty	VERA_DATA0	; Write it to screen
;Do high hour
	lda	#32*2		; Set VERA to address of high part
	sta	VERA_ADDR_LOW	; of hours
	ldy	VERA_DATA0	; Load high part of hours
	iny
	cpy	#$3A		; If it is not equal to $3A
	bne	@do_write	; Write it out and return
	ldy	#$30		; Esle set to $30 = '0'
@do_write:
	sty	VERA_DATA0	; Write to screen
	rts

; *******************************************************************
; Place player randomly on the playing field
; *******************************************************************
; USES:		.A, TMP8, TMP9
; *******************************************************************
place_player:
	jsr	find_empty	; Find an empty field
	lda	TMP8		; Save Y coordinate
	sta	PLAYER_Y
	lda	TMP9		; Save X coordinate
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
; OUTPUT:	TMP8 = Y-coordinate, TMP9 = X-coordinate
; *******************************************************************
find_empty:
	+RAND	$B0+3, $B0+29		; Find number between 3 and 29
	sta	VERA_ADDR_HIGH	; Set it as Y coordinate
	sta	TMP8
	+RAND	1, 39		; Find number between 1 and 39
	sta	TMP9
	asl			; Multiply by 2
	sta	VERA_ADDR_LOW	; Set it as X coordinate
	lda	VERA_DATA0	; Read character at coordinate
	cmp	#' '
	bne	find_empty	; If char != ' ', try again
	rts

; *****************************************************************************
; Place ghosts and portals randomly on the playing field
; *****************************************************************************
; INPUT:	Global ZP variables decides how many of each will
;		be placed.
; USES:		.A, .X, TMP0-TMP3, TMP8=Y & TMP9=X
; *****************************************************************************
place_ghosts:
@x_ptr=TMP0
@y_ptr=TMP2
	lda #$01
	sta	VERA_ADDR_BANK		; No increment

	lda	#<Portal_X
	sta	@x_ptr
	lda	#>Portal_X
	sta	@x_ptr+1
	lda	#<Portal_Y
	sta	@y_ptr
	lda	#>Portal_Y
	sta	@y_ptr+1
	ldy	#PORTAL			; Portal character
	ldx	NUMPORTALS		; Number of portals
	jsr	p_ghost

	lda	#<Ghost_X
	sta	@x_ptr
	lda	#>Ghost_X
	sta	@x_ptr+1
	lda	#<Ghost_Y
	sta	@y_ptr
	lda	#>Ghost_Y
	sta	@y_ptr+1
	ldy	#GHOST			; Ghost character
	ldx	NUMGHOSTS		; Number of ghosts
	jsr	p_ghost

	lda	#<P_Ghost_X
	sta	@x_ptr
	lda	#>P_Ghost_X
	sta	@x_ptr+1
	lda	#<P_Ghost_Y
	sta	@y_ptr
	lda	#>P_Ghost_Y
	sta	@y_ptr+1
	ldy	#PGHOST			; Poltergeist character
	ldx	NUMPGHOSTS		; Number of poltergeists
	jsr	p_ghost

	lda	#<D_Ghost_X
	sta	@x_ptr
	lda	#>D_Ghost_X
	sta	@x_ptr+1
	lda	#<D_Ghost_Y
	sta	@y_ptr
	lda	#>D_Ghost_Y
	sta	@y_ptr+1
	ldy	#DGHOST			; Dimensional ghost character
	ldx	NUMDGHOSTS		; Number of dimensional ghosts
	bra	p_ghost
;	rts				Not needed as last jmp/bra to p_ghost
;					will ensure correct return to caller

; *****************************************************************************
; Place ghosts and portals randomly on the playing field
; *****************************************************************************
; INPUT:	.Y = Ghost-character to use
;		.X = Number of ghosts/portals to place
;		(must be the last one set before calling this function)
;		TMP0/TMP1 = Pointer to X coordinates
;		TMP2/TMP3 = Pointer to Y coordinates
; USES:		.A
; *****************************************************************************
p_ghost:
@x_ptr=TMP0
@y_ptr=TMP2
@empty_y=TMP8
@empty_x=TMP9
	beq	@end
	jsr	find_empty	; Find an empty field

	; Save coordinates
	lda	@empty_x	; Save ghost X coordinate
	sta	(@x_ptr)
	lda	@empty_y	; Save ghost Y coordinate
	sta	(@y_ptr)

	; Increment ZP pointers to be ready for next item
	+INC16	@x_ptr
	+INC16	@y_ptr

	tya			; Load the ghost-character
	sta	VERA_DATA0
	inc	VERA_ADDR_LOW
	lda	#GHOST_COL
	sta	VERA_DATA0
	dex			; While .X > 0
	bra	p_ghost:
@end:
	rts

; *******************************************************************
; Place walls randomly on the playing field
; *******************************************************************
; INPUT:	NUMWALLS * 5 wall chars will be written
; USES:		.A, .X & .Y, TMP2
; *******************************************************************
place_walls:
	ldx	NUMWALLS
	lda #$01
	sta	VERA_ADDR_BANK		; No Increment
@out_loop:
	ldy	#5
@in_loop:
	jsr	find_empty
	lda	#WALL			; Place a wall char
	sta	VERA_DATA0
	inc	VERA_ADDR_LOW
	lda	#WALL_COL
	sta	VERA_DATA0
	dey				; Whily .Y > 0
	bne	@in_loop
	dex				; While .X > 0
	bne	@out_loop
	rts

; *******************************************************************
; Place a number of Static Wall Chars randomly in the playing field
; *******************************************************************
; INPUTS:	NUMSWALLS = number of Static Walls to place
; USES:		.A & .X
; *******************************************************************
place_swalls:
	ldx	NUMSWALLS
@loop:
	+RAND 2, 37
	asl				; Multiply by 2 for X coord
	sta	VERA_ADDR_LOW

	+RAND $B0+4, $B0+27
	sta	VERA_ADDR_HIGH

	lda	#SWALL			; Set the SWALL character
	sta	VERA_DATA0
	lda	#SWALL_COL		; Set the SWALL_COL color
	sta	VERA_DATA0
	dex				; While X > 0
	bne	@loop			; jump back to place wall
	rts

; *******************************************************************
; Stores 1 in the IRQ_TRIG variable on eache VSYNC interrupt
; Uses A, but it is restored by KERNAL when the original intterupt
; handler is called.
; *******************************************************************
handle_irq:
	lda	VERA_ISR
	and	#1			; Is this VSYNC?
	beq	@vsync_end		; if not, end
	sta	IRQ_TRIG
@vsync_end:
	jmp	(Old_irq_handler)	; Continue to original

; *******************************************************************
; Clear the playing field
; *******************************************************************
; USES:		A, X & Y
; *******************************************************************
clear_field:
	lda	#$11
	sta	VERA_ADDR_BANK		; Increment by 1
	ldy	#$B0+3			; Start on line 3

@outloop:
	sty	VERA_ADDR_HIGH
	ldx	#38
	lda	#2			; Start on column 1 (every other=color)
	sta	VERA_ADDR_LOW
@inloop:
	lda	#' '
	sta	VERA_DATA0
	lda	#PLAY_COL
	sta	VERA_DATA0
	dex
	bne	@inloop
	iny
	cpy	#$B0+29
	bne	@outloop
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
	ldx	#<Top_line1
	ldy	#>Top_line1
	jsr	print_str
	+GOTO_XY 0, 1
	ldx	#<Top_line2
	ldy	#>Top_line2
	jsr	print_str

	lda	#$11
	sta	VERA_ADDR_BANK	; Set increment to 1

	ldy	#$B0+2		; Line 2 = Y coordinate
	sty	VERA_ADDR_HIGH
	ldy	#0		; Column 0 = X coordinate
	sty	VERA_ADDR_LOW

	; Change 40 characters across screen
	ldy	#40
@topline:
	lda	#SWALL
	sta	VERA_DATA0
	lda	#SWALL_COL
	sta	VERA_DATA0
	dey
	bne	@topline

	ldy	#$B0+29		; Line = 29 = Y coordinate
	sty	VERA_ADDR_HIGH
	ldy	#0		; Column 0 = X coordinate
	sty	VERA_ADDR_LOW

	ldy	#40
@bottomline:
	lda	#SWALL
	sta	VERA_DATA0
	lda	#SWALL_COL
	sta	VERA_DATA0
	dey
	bne	@bottomline

	ldy	#$B0+2
	ldx	#0
@leftline:
	stx	VERA_ADDR_LOW
	sty	VERA_ADDR_HIGH
	lda	#SWALL
	sta	VERA_DATA0
	lda	#SWALL_COL
	sta	VERA_DATA0
	iny
	cpy	#$B0+29
	bne	@leftline

	ldy	#$B0+2
	ldx	#(39*2)
@rightline:
	stx	VERA_ADDR_LOW
	sty	VERA_ADDR_HIGH
	lda	#SWALL
	sta	VERA_DATA0
	lda	#SWALL_COL
	sta	VERA_DATA0
	iny
	cpy	#$B0+29
	bne	@rightline

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
; USES:		.A
;		RANDNUM (2 bytes)
; *******************************************************************
randomize:
	lda	RANDNUM+1
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
; USES:		.A, .X & .Y
;		TMP0 & TMP1 is used because of print_str function
; *******************************************************************
splash_screen:
	lda	#0			; Ensure 80x60 mode
	clc
	jsr	SCRMOD

	lda	#PLAY_COL		; Set black background
	sta	COLOR_PORT

	lda	#147			; Clear Screen
	jsr	CHROUT

	+GOTO_XY 2, 10
	ldx	#<Gc1
	ldy	#>Gc1
	jsr	print_str
	+GOTO_XY 2, 11
	ldx	#<Gc2
	ldy	#>Gc2
	jsr	print_str
	+GOTO_XY 2, 12
	ldx	#<Gc3
	ldy	#>Gc3
	jsr	print_str
	+GOTO_XY 2, 13
	ldx	#<Gc4
	ldy	#>Gc4
	jsr	print_str
	+GOTO_XY 2, 14
	ldx	#<Gc5
	ldy	#>Gc5
	jsr	print_str
	+GOTO_XY 41-((Mail-Name)/2), 18
	ldx	#<Name
	ldy	#>Name
	jsr	print_str
	+GOTO_XY 41-((For_cx16-Mail)/2), 20
	ldx	#<Mail
	ldy	#>Mail
	jsr	print_str
	+GOTO_XY 41-((Pstart-For_cx16)/2), 27
	ldx	#<For_cx16
	ldy	#>For_cx16
	jsr	print_str
	+GOTO_XY 41-((Player_text-Pstart)/2), 36
	ldx	#<Pstart
	ldy	#>Pstart
	jsr	print_str
	+GOTO_XY 39, 24
	ldx	#<Xl1
	ldy	#>Xl1
	jsr	print_str
	+GOTO_XY 40, 25
	ldx	#<Xl2
	ldy	#>Xl2
	jsr	print_str
	+GOTO_XY 41, 26
	ldx	#<Xl3
	ldy	#>Xl3
	jsr	print_str
	+GOTO_XY 41, 28
	ldx	#<Xl5
	ldy	#>Xl5
	jsr	print_str
	+GOTO_XY 40, 29
	ldx	#<Xl6
	ldy	#>Xl6
	jsr	print_str
	+GOTO_XY 39, 30
	ldx	#<Xl7
	ldy	#>Xl7
	jsr	print_str
	+GOTO_XY 35, 40
	ldx	#<Player_text
	ldy	#>Player_text
	jsr	print_str
	+GOTO_XY 35, 42
	ldx	#<Portal_text
	ldy	#>Portal_text
	jsr	print_str
	+GOTO_XY 35, 44
	ldx	#<Ghost_text
	ldy	#>Ghost_text
	jsr	print_str
	+GOTO_XY 35, 46
	ldx	#<Pghost_text
	ldy	#>Pghost_text
	jsr	print_str
	+GOTO_XY 35, 48
	ldx	#<Dghost_text
	ldy	#>Dghost_text
	jsr	print_str
	+GOTO_XY 35, 50
	ldx	#<Wall_text
	ldy	#>Wall_text
	jsr	print_str
	+GOTO_XY 35, 52
	ldx	#<Swall_text
	ldy	#>Swall_text
	jsr	print_str
	rts

int_2_bcd ; conversion table: integer to BCD
!for Outer, 0, 9 {
	!for Inner, 0, 9 {
		!byte (Outer << 4) OR Inner
	}
}
