
IRQ_VEC		= $0314
TIMER_LOW	= $0294		; The system clock is stored at
				; $0292 - $0294. The value at $0292
				; is updated 60 times a second.
				; This information was found by
				; using the debugger to step through a call
				; to RDTIM

NES_A		= $80
NES_B		= $40
NES_SEL		= $20
NES_STA		= $10	; zero flag is already set


JOY_UP		= $08
JOY_DN		= $04
JOY_LT		= $02
JOY_RT		= $01

JOY_SCAN	= $FF53
JOY_GET		= $FF56
CHROUT		= $FFD2
GETIN		= $FFE4
PLOT		= $FFF0
SCRMOD		= $FF5F

COLOR_PORT	= $0376

TMP0		= $76
TMP1		= $77
TMP2		= $78
TMP3		= $79
TMP4		= $7A
TMP5		= $7B
TMP6		= $7C
TMP7		= $7D
TMP8		= $7E
TMP9		= $7F

; *******************************************************************
; Save the address of the original interrupt handler
; *******************************************************************
; USES:		A
; *******************************************************************
!macro SAVE_INT_VECTOR {
	lda	IRQ_VEC
	sta	Old_irq_handler
	lda	IRQ_VEC+1
	sta	Old_irq_handler+1
}

; *******************************************************************
; Install new interrupt handler
; *******************************************************************
; USES:		A
; *******************************************************************
!macro INSTALL_INT_HANDLER .handler {
	sei			; Disable interrupts
	lda	#1		; Tell VERA that we want interrupts
	sta	VERA_IEN	; on each VSYNC
	lda	#<.handler	; Write address of local interrupt
	sta	IRQ_VEC		; handler to the interrupt
	lda	#>.handler	; vector address ($0314)
	sta	IRQ_VEC+1
	cli			; Enable interrupts
}

; *******************************************************************
; Check if the VSYNC interrupt has occurred. If it has,
; call the do_game routine to keep the game going.
; *******************************************************************
; USES:		A
; *******************************************************************
!macro CHECK_IRQ .handler {
	lda	IRQ_TRIG
	beq	.done
	; VSYNC IRQ has occurred, handle
	jsr	.handler
	lsr	IRQ_TRIG
.done:
}

	jmp	X16_DONE

; *******************************************************************
; When a new interrupt handler is installed, it should pass control
; to the original interrupt hanlder upon completion. This label
; will be updated to point to the original interrupt handler.
; *******************************************************************
Old_irq_handler:
	!word	$FFFF

X16_DONE:
