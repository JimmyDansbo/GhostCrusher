*=$0801
!byte $0C,$08,$0A,$00,$9E,$20,$32,$30,$36,$34,$00,$00,$00
*=$0810

	jmp	main
	
!src "x16.inc"
!src "text.inc"


main:
	rts
