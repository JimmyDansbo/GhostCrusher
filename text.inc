
PLAYER		= $5A
PORTAL		= $57
GHOST		= $58
PGHOST		= $23	; Poltergeist
DGHOST		= $56	; Dimentional ghost (i.e. ghost from portal)
WALL		= $66
SWALL		= $60

PLAY_COL	= $0E
GHOST_COL	= $07
WALL_COL	= $05
SWALL_COL	= $22

PET_PLAYER	= $7A
PET_PORTAL	= $77
PET_GHOST	= $78
PET_PGHOST	= '#'
PET_DGHOST	= $76
PET_WALL	= $A6
PET_SWALL	= $A0


; *******************************************************************
; Set cursor to location
; *******************************************************************
; USES:		.X, .Y
; *******************************************************************
!macro GOTO_XY .xcord, .ycord {
	ldy	#.xcord
	ldx	#.ycord
	clc
	jsr	PLOT
}

	jmp	TEXT_DONE

; *******************************************************************
; Print a 0-terminated petscii string to screen using KERNAL API
; *******************************************************************
; INPUTS:	X and Y pointer to string
; USES:		A
;		TMP0 & TMP1 for storing the pointer to string
; *******************************************************************
print_str:
@ptr=TMP0
	; Store address of string in ZP memory
	stx	@ptr
	sty	@ptr+1
	ldy	#0		; Y register used to index string
@doprint
	lda	(@ptr), y	; Load character from string
	beq	@printdone	; If character is 0, we are done
	jsr	CHROUT		; Write charactoer to screen
	iny			; Inc Y to get next character
	jmp	@doprint	; Get next character
@printdone:
	rts

Top_line1	!pet	$05,"   lives: 05 - level: 01 - ghosts: 02",0
Top_line2	!pet	"points: 000000            time: 00:00:00",0

Gc1		!pet	$1E," ",$A6,$A6,$A6,"  ",$A6,"   ",$A6,"  ",$A6,$A6,$A6,"   ",$A6,$A6,$A6,$A6," ",$A6,$1C,$12,' ',$92,$1E,$A6,$A6,$A6,"    ",$A6,$A6,$A6,"  ",$A6,$A6,$A6,$A6,"  ",$A6,"   ",$A6,"  ",$A6,$A6,$A6,$A6," ",$A6,"   ",$A6," ",$1C,$12,' ',$92,$1E,$A6,$A6,$1C,$12,' ',$92,$1E,$A6," ",$A6,$A6,$A6,$A6,$A6,0
Gc2		!pet	$1C,$12,' ',$92,$1E,"   ",$A6," ",$A6,$9A,$7A,$1E,"  ",$A6," ",$A6,"   ",$A6," ",$A6," ",$9E,$77,$1E,"     ",$A6,"     ",$A6,$9E,$78,$1E,"  ",$A6," ",$A6,"   ",$A6," ",$A6,"   ",$A6," ",$A6,$9E,$77,$1E,"    ",$A6,"   ",$A6," ",$A6,"     ",$A6,"    ",$1c,$12,' ',$92,$1E,0
Gc3		!pet	$A6,"     ",$A6,$A6,$A6,$A6,$A6," ",$A6,"  ",$9E,$78,$1E,$A6,"  ",$A6,$A6,$A6,"    ",$A6,"     ",$A6,"     ",$A6,$1C,$12,' ',$92,$1E,$A6,$A6,"  ",$A6,"   ",$A6,"  ",$A6,$A6,$A6,"  ",$A6,$A6,$A6,$A6,$A6," ",$A6,$A6,$A6,"   ",$A6,$A6,$A6,$A6,$A6,0
Gc4		!pet	$A6,"  ",$A6,$A6," ",$A6,"   ",$A6," ",$1C,$12,' ',$92,$1E,"   ",$A6,"     ",$A6,"   ",$A6,"     ",$A6,"   ",$A6," ",$A6,$9E,'#',$1E,"  ",$A6," ",$A6,$9E,$78,$1E,"  ",$A6,"     ",$A6," ",$1C,$12,' ',$92,$9E,$76,$1E,"  ",$A6," ",$A6,"     ",$A6,"    ",$A6,0
Gc5		!pet	" ",$A6,$A6,$A6,"  ",$A6,"   ",$A6,"  ",$A6,$A6,$A6,"  ",$A6,$A6,$A6,$A6,"    ",$A6,"      ",$A6,$A6,$A6,"  ",$A6,"   ",$A6,"  ",$A6,$A6,$1C,$12,' ',$92,$1E,"  ",$1C,$12,' ',$92,$1E,$A6,$A6,$A6,"  ",$A6,"   ",$A6," ",$A6,$A6,$A6,$A6,$A6," ",$A6,"    ",$A6,0

Xl1		!pet	$9C,$7F,$12,"  ",$7F,$92,"   ",$12,$A9,"  ",$92,$A9,$1E,0
Xl2		!pet	$9A,$7F,$12,"  ",$7F,$92," ",$12,$A9,"  ",$92,$A9,$1E,0
Xl3		!pet	$9F,$7F,$12,"  ",$92," ",$12,"  ",$92,$A9,$1E,0
Xl5		!pet	$9E,$12,$A9,"  ",$92," ",$12,"  ",$7F,$92,$1E,0
Xl6		!pet	$81,$12,$A9,"  ",$92,$A9," ",$7F,$12,"  ",$7F,$92,$1E,0
Xl7		!pet	$1C,$12,$A9,"  ",$92,$A9,"   ",$7F,$12,"  ",$7F,$92,$1E,0

Name		!pet	"ghost crusher by jimmy dansbo",0
Mail		!pet	"(jimmy@dansbo.dk) 2020",0
For_cx16	!pet	"for commander  ",18," ",146," ",18," ",146,"  16",0
Pstart		!pet	"press start or enter to play",0

Player_text	!pet	$9A,$7A,$1E," = player",0
Portal_text	!pet	$9E,$77,$1E," = portal",0
Ghost_text	!pet	$9E,$78,$1E," = ghost",0
Pghost_text	!pet	$9E,'#',$1E," = poltergeist",0
Dghost_text	!pet	$9E,$76,$1E," = dimensional ghost",0
Wall_text	!pet	$A6," = wall",0
Swall_text	!pet	$1C,$12,' ',$92,$1E," = immovable wall",0

Game_over1	!pet	$1C,$12,           "             ",$92,0
Game_over2	!pet	$1C,$12,' ',$92,    "           ",$12,' ',$92,0
Game_over3	!pet	$1C,$12,' ',$92,$05," game over ",$1C,$12,' ',$92,0
Game_over4	!pet	$1C,$12,' ',$92,    "           ",$12,' ',$92,0
Game_over5	!pet	$1C,$12,           "             ",$92,0

TEXT_DONE:
