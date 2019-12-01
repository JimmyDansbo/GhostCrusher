
PLAYER		= $5A
PORTAL		= $57
GHOST		= $08
PGHOST		= $23	; Poltergeist
DGHOST		= $56	; Dimentional ghost (i.e. ghost from portal)

WALL		= $66
SWALL		= ' '

PLAY_COL	= $0E
GHOST_COL	= $08
WALL_COL	= $05
SWALL_COL	= $22

PET_PLAYER	= $7A
PET_PORTAL	= $77
PET_GHOST	= 'h'
PET_PGHOST	= '#'
PET_DGHOST	= $76
PET_WALL	= $A6
PET_SWALL	= ' '


!macro gotoxy .xcord, .ycord {
	ldy	#.xcord
	ldx	#.ycord
	clc
	jsr	PLOT
}

	jmp	TEXT_DONE

.gc1		!pet	" ",$A6,$A6,$A6,"  ",$A6,"   ",$A6,"  ",$A6,$A6,$A6,"   ",$A6,$A6,$A6,$A6," ",$A6,$1C,$12,' ',$92,$1E,$A6,$A6,$A6,"    ",$A6,$A6,$A6,"  ",$A6,$A6,$A6,$A6,"  ",$A6,"   ",$A6,"  ",$A6,$A6,$A6,$A6," ",$A6,"   ",$A6," ",$1C,$12,' ',$92,$1E,$A6,$A6,$1C,$12,' ',$92,$1E,$A6," ",$A6,$A6,$A6,$A6,$A6,0
.gc2		!pet	$1C,$12,' ',$92,$1E,"   ",$A6," ",$A6,$9A,$7A,$1E,"  ",$A6," ",$A6,"   ",$A6," ",$A6," ",$81,$77,$1E,"     ",$A6,"     ",$A6,$81,'h',$1E,"  ",$A6," ",$A6,"   ",$A6," ",$A6,"   ",$A6," ",$A6,$81,$77,$1E,"    ",$A6,"   ",$A6," ",$A6,"     ",$A6,"    ",$1c,$12,' ',$92,$1E,0
.gc3		!pet	$A6,"     ",$A6,$A6,$A6,$A6,$A6," ",$A6,"  ",$81,'h',$1E,$A6,"  ",$A6,$A6,$A6,"    ",$A6,"     ",$A6,"     ",$A6,$1C,$12,' ',$92,$1E,$A6,$A6,"  ",$A6,"   ",$A6,"  ",$A6,$A6,$A6,"  ",$A6,$A6,$A6,$A6,$A6," ",$A6,$A6,$A6,"   ",$A6,$A6,$A6,$A6,$A6,0
.gc4		!pet	$A6,"  ",$A6,$A6," ",$A6,"   ",$A6," ",$1C,$12,' ',$92,$1E,"   ",$A6,"     ",$A6,"   ",$A6,"     ",$A6,"   ",$A6," ",$A6,$81,'#',$1E,"  ",$A6," ",$A6,$81,'h',$1E,"  ",$A6,"     ",$A6," ",$1C,$12,' ',$92,$81,$76,$1E,"  ",$A6," ",$A6,"     ",$A6,"    ",$A6,0
.gc5		!pet	" ",$A6,$A6,$A6,"  ",$A6,"   ",$A6,"  ",$A6,$A6,$A6,"  ",$A6,$A6,$A6,$A6,"    ",$A6,"      ",$A6,$A6,$A6,"  ",$A6,"   ",$A6,"  ",$A6,$A6,$1C,$12,' ',$92,$1E,"  ",$1C,$12,' ',$92,$1E,$A6,$A6,$A6,"  ",$A6,"   ",$A6," ",$A6,$A6,$A6,$A6,$A6," ",$A6,"    ",$A6,0

.xl1		!pet	$9C,$7F,$12,"  ",$7F,$92,"   ",$12,$A9,"  ",$92,$A9,$1E,0
.xl2		!pet	$9A,$7F,$12,"  ",$7F,$92," ",$12,$A9,"  ",$92,$A9,$1E,0
.xl3		!pet	$9F,$7F,$12,"  ",$92," ",$12,"  ",$92,$A9,$1E,0
.xl5		!pet	$9E,$12,$A9,"  ",$92," ",$12,"  ",$7F,$92,$1E,0
.xl6		!pet	$81,$12,$A9,"  ",$92,$A9," ",$7F,$12,"  ",$7F,$92,$1E,0
.xl7		!pet	$1C,$12,$A9,"  ",$92,$A9,"   ",$7F,$12,"  ",$7F,$92,$1E,0

.name		!pet	"ghost crusher by jimmy dansbo",0
.mail		!pet	"(jimmy@dansbo.dk) 2019",0
.forcx16	!pet	"for commander  ",18," ",146," ",18," ",146,"  16",0
.pstart		!pet	"press start or enter to play",0

.player_text	!pet	$9A,$7A,$1E," = player",0
.portal_text	!pet	$81,$77,$1E," = portal",0
.ghost_text	!pet	$81,'h',$1E," = ghost",0
.pghost_text	!pet	$81,'#',$1E," = poltergeist",0
.dghost_text	!pet	$81,$76,$1E," = dimensional ghost",0
.wall_text	!pet	$A6," = wall",0
.swall_text	!pet	$1C,$12,' ',$92,$1E," = immovable wall",0


TEXT_DONE: