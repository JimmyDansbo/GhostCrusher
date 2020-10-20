!ifdef FARBRANCH_INC !eof
FARBRANCH_INC=1

;****************************************************************************
; Branch to far address if Carry Clear
; If the carry flag is clear then set the program counter to .addr
;****************************************************************************
; INPUT:	.addr - The label to branch to
;****************************************************************************
!macro FBCC .addr {
	bcs	+
	jmp	.addr
+
}

;****************************************************************************
; Branch to far address if Carry Set
; If the carry flag is set then set the program counter to .addr
;****************************************************************************
; INPUT:	.addr - The label to branch to
;****************************************************************************
!macro FBCS .addr {
	bcc	+
	jmp	.addr
+
}

;****************************************************************************
; Branch to far address if EQual
; If the zero flag is set then set the program counter to .addr
;****************************************************************************
; INPUT:	.addr - The label to branch to
;****************************************************************************
!macro FBEQ .addr {
	bne	+
	jmp	.addr
+
}

;****************************************************************************
; Branch to far address if MInus
; If the negative flag is set then set the program counter to .addr
;****************************************************************************
; INPUT:	.addr - The label to branch to
;****************************************************************************
!macro FBMI .addr {
	bpl	+
	jmp	.addr
+
}

;****************************************************************************
; Branch to far address if Not Equal
; If the zero flag is clear then set the program counter to .addr
;****************************************************************************
; INPUT:	.addr - The label to branch to
;****************************************************************************
!macro FBNE .addr {
	beq	+
	jmp	.addr
+
}

;****************************************************************************
; Branch to far address if PLus
; If the negative flag is clear then set the program counter to .addr
;****************************************************************************
; INPUT:	.addr - The label to branch to
;****************************************************************************
!macro FBPL .addr {
	bmi	+
	jmp	.addr
+
}

;****************************************************************************
; Branch to far address if oVerflow Clear
; If the overflow flag is clear then set the program counter to .addr
;****************************************************************************
; INPUT:	.addr - The label to branch to
;****************************************************************************
!macro FBVC .addr {
	bvs	+
	jmp	.addr
+
}

;****************************************************************************
; Branch to far address if oVerflow Set
; If the overflow flag is set then set the program counter to .addr
;****************************************************************************
; INPUT:	.addr - The label to branch to
;****************************************************************************
!macro FBVS .addr {
	bvc	+
	jmp	.addr
+
}
