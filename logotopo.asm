; z80dasm 1.1.5
; command line: z80dasm -g0x9470 -l -t TOPO

; $ shasum TOPO
; 765de5f1a2e6abce2cced1127e7251a57fb9b3d2  TOPO

; $ z80asm logotopo.asm |shasum a.bin 
; 765de5f1a2e6abce2cced1127e7251a57fb9b3d2  a.bin

	org	09470h
    
    WRTVRM: equ 0x004d
    
    NUM_COLS: equ 32
    NUM_ROWS: equ 24
    
    COLOR_TABLE: equ 0x2000
    COLOR_RED_BLACK: equ 0x81
    COLOR_WHITE_BLACK: equ 0xf1
    COLOR_WHITE_TRANSPARENT: equ 0xf0
    
    ; Position in the color table of the "S" of the "SOFT" text
    COLOR_TABLE_POS_S: equ COLOR_TABLE + 0xf78
    
    ; Length of a line in the "SOFT" text (8 chars)
    COLOR_TABLE_SOFT_LINE_LENGTH: equ 8*8
    
	jp START		;9470  Jump to start

; *******************************************************
; * Animation of the "SOFT" text with light reflections *
; *******************************************************
REFLECTION_ANIMATION:
	ld hl, COLOR_TABLE_POS_S	;9473
	ld de, COLOR_TABLE_POS_S + COLOR_TABLE_SOFT_LINE_LENGTH	;9476
    
    ; In BC it stores the offset that we need to add to HL (a pointer in
    ; the CT for a particular tile) to be in the tile just below.
    ; In fact, it's subtracting 1 to the result, resulting in the
    ; last line of the tile on the left. Is this a bug?
    ;
    ; 32 columns (we subtract 1 since after processing the previous
    ; tile the pointer has been already incremented), each tile being
    ; 8 bytes of color.
	ld bc, (NUM_COLS - 1)*8 - 1       ;9479
l947ch:
	
    ; Get out if HL == DE (it has arrive at the end of line)
    and a			;947c
	sbc hl,de		;947d
	ret z			;947f
    
    ; Not at the end of the line
	add hl,de		;9480 Fix subtraction

    ; Recover normal color, red over black. Upper line.
	ld a, COLOR_RED_BLACK		;9481
	call COPY_8_BYTES_TO_VRAM	;9483

    ; Recover normal color, red over black. Lower line.
	push hl			            ;9486
	add hl,bc			        ;9487
	ld a, COLOR_RED_BLACK	    ;9488
	call COPY_8_BYTES_TO_VRAM	;948a
	pop hl			            ;948d
    
    ; Set white reflection color. Upper line.
	push hl			            ;948e
	ld a, COLOR_WHITE_BLACK		;948f
	call COPY_8_BYTES_TO_VRAM	;9491
    
    ; Set white reflection color. Lower line.
	add hl,bc			        ;9494
	ld a,COLOR_WHITE_BLACK      ;9495
	call COPY_8_BYTES_TO_VRAM	;9497
	pop hl			            ;949a

    ; Wait for 3 vertical retraces
    ei			                ;949b
	push bc			            ;949c
	ld b,003h		            ;949d
l949fh:
	halt			            ;949f
	djnz l949fh		            ;94a0
	pop bc			            ;94a2

	di			                ;94a3
    ; Loop until the whole logo has been walked through
	jr l947ch		            ;94a4

; **********************************************
; * Write 8 times value A to VRAM's address HL *
; **********************************************
COPY_8_BYTES_TO_VRAM:
	push bc			;94a6
	ld b, 8 		;94a7
l94a9h:
	call WRTVRM		;94a9
	inc hl			;94ac
	djnz l94a9h		;94ad
	pop bc			;94af
	ret			    ;94b0

; ***************
; * Move object *
; ***************
MOVE_OBJECT:

; [STORE_2] <-- TABLE_3 + 2*P
AUTOMODIF_INST_2:
	ld hl,0000dh		;94b1 Parameter P is set outside, automodified code
	add hl,hl			;94b4
	ld de, TABLE_3		;94b5
	add hl,de			;94b8
	ld (STORE_2),hl		;94b9 [STORE_2] <-- TABLE_3 + 2*P. Ex: 0x9704

; Obtain the table of attributes of the object, according to its index Q
AUTOMODIF_OBJECT_IDX:
    ; D1 = HL <-- TABLE_1 + 2*Q
	ld hl,0000eh		;94bc Parameter Q is set outside, automodified code
                        ; Ex: HL=7

	add hl,hl			;94bf
	ld de, TABLE_1		;94c0
	add hl,de			;94c3 Ex: HL = 0x96A2

    ; DE <-- [D1] = TABLE_1[2*Q]
	ld e,(hl)			;94c4
	inc hl			    ;94c5
	ld d,(hl)			;94c6
    ; Ex: DE = 0x038E

    ; Obtain pattern stuff
	; D2 = [D1] + TABLE_2 = TABLE_2[TABLE_1[2*Q]]
    ld hl, TABLE_2		;94c7
	add hl,de			;94ca
    ; Ex: HL = 0x9AB6
    
    ; Explanation.
    ; First it obtains the address D1 = TABLE_1[2*Q]
    ; And it performs a second indirection: D2 = TABLE_2[D1] = TABLE_2[TABLE_1[2*Q]]
    
    ; Obtain the number of tile lines to fill each char row
    ; A = [D2] = TABLE_2[TABLE_1[2*Q]]
	ld a,(hl)			                                ;94cb Ex: A=48
	ld (AUTOMODIF_TILE_LINES_PER_CHAR_ROW + 1),a		;94cc

    ; Obtain the number of rows (chars) of the object
	inc hl			                ;94cf
	ld a,(hl)			            ;94d0 Ex: A=0xB
	ld (AUTOMODIF_NUM_ROWS + 1),a	;94d1

	inc hl			    ;94d4
	ld (TEMP_PATTERNS_ADDRESS),hl

	ld ix,(STORE_2)		;94d8 IX <-- [STORE_2] = TABLE_3 + 2*P. Ex: 0x9704



AUTOMODIF_NUM_ROWS:
	ld c, 5     		;94dc Number of rows (chars) of the object

write_all_tiles:
	ld e,(ix+000h)		;94de
	inc ix		        ;94e1
	ld d,(ix+000h)		;94e3
	inc ix		        ;94e6
    ; Ex: DE = 0xC600

; Get the address of the patterns in DE
; Get the VRAM destination in HL
AUTOMODIF_INST_3:
	ld hl,000b0h		;94e8
	add hl,de			;94eb
	ld de,(TEMP_PATTERNS_ADDRESS)		;94ec Ex: 0x9AB8

; Set the number of tile pattern lines that it needs to copy in that char row
; For example, if B=0x48 (72), then it's 72 / 8 = 9 tiles.
AUTOMODIF_TILE_LINES_PER_CHAR_ROW:
	ld b,018h		    ;94f0

; Draw a line of the object
draw_tile_line:
	ld a,(de)			;94f2	1a 	. 
; It can be a 0: NOP or a 0xB6: OR (HL)
AUTOMODIF_CODE:
	nop			    ;94f3
    ; Ex: DE = 0x9AB8 --> Patterns to draw
    ; Ex: HL = 0xC600 --> VRAM destination
	inc de			;94f4
	res 7,h		    ;94f5
	res 6,h		    ;94f7
    ; Ex: HL = 0x600
    
    ; Function: Writes data in VRAM
    ; Input   : HL - Address
    ;           A  - Value
	call WRTVRM		;94f9
	inc hl			;94fc
	set 6,h		    ;94fd
	set 7,h		    ;94ff
	djnz draw_tile_line	;9501

	ld (TEMP_PATTERNS_ADDRESS),de		    ;9503
	dec c			        ;9507
	jr nz,write_all_tiles	;9508
	ret			            ;950a

ROTATE_SOFT:
	ld a,00fh		;950b	3e 0f 	> . 
	ld (AUTOMODIF_INST_2 + 1),a		;950d	32 b2 94 	2 . . 
	ld a,078h		;9510	3e 78 	> x 
	ld (AUTOMODIF_INST_3 + 1),a		;9512	32 e9 94 	2 . . 
    
    ; Write a NOP in the auto-modificable code
	xor a			        ;9515
	ld (AUTOMODIF_CODE),a	;9516
	ld hl,0x96b2		    ;9519
l951ch:
	ld a,(hl)			;951c	7e 	~ 
	cp 0ffh		;951d	fe ff 	. . 
	ret z			;951f	c8 	. 
	ld (AUTOMODIF_OBJECT_IDX + 1),a		;9520	32 bd 94 	2 . . 
	push hl			;9523	e5 	. 
	ei			;9524	fb 	. 
	ld b,002h		;9525	06 02 	. . 
l9527h:
	halt			;9527	76 	v 
	djnz l9527h		;9528	10 fd 	. . 
	di			;952a	f3 	. 
	call MOVE_OBJECT		;952b	cd b1 94 	. . . 
	pop hl			;952e	e1 	. 
	inc hl			;952f	23 	# 
	jr l951ch		;9530	18 ea 	. . 

MOVE_T:
	ld a, 7	    	                ;9532
	ld (AUTOMODIF_OBJECT_IDX + 1),a		;9534

	ld a, 6 		                ;9537
	ld (AUTOMODIF_INST_2 + 1),a		;9539

	ld a, 0 		                ;953c
l953eh:
	cp NUM_ROWS	                    ;953e
	ret z			                ;9540

	ld (AUTOMODIF_INST_3 + 1),a		;9541
	push af			                ;9544
	call MOVE_OBJECT		            ;9545
	pop af			                ;9548
	add a, 8		                ;9549
	jr l953eh		                ;954b

MOVE_P:
	ld a,009h		;954d	3e 09 	> . 
	ld (AUTOMODIF_OBJECT_IDX + 1),a		;954f	32 bd 94 	2 . . 
	ld a,007h		;9552	3e 07 	> . 
	ld (AUTOMODIF_INST_2 + 1),a		;9554	32 b2 94 	2 . . 
	ld a,090h		;9557	3e 90 	> . 
l9559h:
	cp 050h		;9559	fe 50 	. P 
	ret z			;955b	c8 	. 
	ld (AUTOMODIF_INST_3 + 1),a		;955c	32 e9 94 	2 . . 
	push af			;955f	f5 	. 
	ei			;9560	fb 	. 
	halt			;9561	76 	v 
	di			;9562	f3 	. 
	call MOVE_OBJECT		;9563	cd b1 94 	. . . 
	pop af			;9566	f1 	. 
	sub 008h		;9567	d6 08 	. . 
	jr l9559h		;9569	18 ee 	. . 

FALL_O:
    ; Write a OR (HL) in the auto-modificable code
	ld a,0b6h		        ;956b Opcode for OR (HL)
	ld (AUTOMODIF_CODE),a	;956d

	ld a,008h		;9570	3e 08 	> . 
	ld (AUTOMODIF_OBJECT_IDX + 1),a		;9572	32 bd 94 	2 . . 
	ld a,038h		;9575	3e 38 	> 8 
	ld (AUTOMODIF_INST_3 + 1),a		;9577	32 e9 94 	2 . . 
	ld a,000h		;957a	3e 00 	> . 
l957ch:
	cp 007h		;957c	fe 07 	. . 
	jr z,l958bh		;957e	28 0b 	( . 
	ld (AUTOMODIF_INST_2 + 1),a		;9580	32 b2 94 	2 . . 
	push af			;9583	f5 	. 
	call MOVE_OBJECT		;9584	cd b1 94 	. . . 
	pop af			;9587	f1 	. 
	inc a			;9588	3c 	< 
	jr l957ch		;9589	18 f1 	. . 
l958bh:
	jp MOVE_OBJECT		;958b	c3 b1 94 	. . . 
JUMP_O:
	ld a,00ah		;958e	3e 0a 	> . 
	ld (AUTOMODIF_OBJECT_IDX + 1),a		;9590	32 bd 94 	2 . . 
	ld hl,0x96cc		;9593	21 cc 96 	! . . 
l9596h:
	ld a,(hl)			;9596	7e 	~ 
	cp 0ffh		;9597	fe ff 	. . 
	jr z,l95abh		;9599	28 10 	( . 
	inc hl			;959b	23 	# 
	ld (AUTOMODIF_INST_3 + 1),a		;959c	32 e9 94 	2 . . 
	ld a,(hl)			;959f	7e 	~ 
	inc hl			;95a0	23 	# 
	ld (AUTOMODIF_INST_2 + 1),a		;95a1	32 b2 94 	2 . . 
	push hl			;95a4	e5 	. 
	call MOVE_OBJECT		;95a5	cd b1 94 	. . . 
	pop hl			;95a8	e1 	. 
	jr l9596h		;95a9	18 eb 	. . 
l95abh:
	jp MOVE_OBJECT		;95ab	c3 b1 94 	. . . 
NICE_GLINT:
	ld hl,0x96e9		;95ae	21 e9 96 	! . . 
l95b1h:
	ld a,(hl)			;95b1	7e 	~ 
	cp 0ffh		;95b2	fe ff 	. . 
	ret z			;95b4	c8 	. 
	push hl			;95b5	e5 	. 
	ld (AUTOMODIF_OBJECT_IDX + 1),a		;95b6	32 bd 94 	2 . . 
	ld a,0b0h		;95b9	3e b0 	> . 
	ld (AUTOMODIF_INST_3 + 1),a		;95bb	32 e9 94 	2 . . 
	ld a,00dh		;95be	3e 0d 	> . 
	ld (AUTOMODIF_INST_2 + 1),a		;95c0	32 b2 94 	2 . . 
	ei			;95c3	fb 	. 
	ld b,004h		;95c4	06 04 	. . 
l95c6h:
	halt			;95c6	76 	v 
	djnz l95c6h		;95c7	10 fd 	. . 
	di			;95c9	f3 	. 
	call MOVE_OBJECT		;95ca	cd b1 94 	. . . 
	pop hl			;95cd	e1 	. 
	inc hl			;95ce	23 	# 
	jr l95b1h		;95cf	18 e0 	. . 

START:
	di			    ;95d1
    
    ; Write a NOP in the auto-modificable code
	xor a			;95d2
	ld (AUTOMODIF_CODE),a	;95d3

	call RESET_CGT	;95d6
	call MOVE_T	;95d9
	call MOVE_P	;95dc
	call sub_9688h	;95df
	call FALL_O	;95e2
	call sub_9688h	;95e5
	call JUMP_O	;95e8
	call ADD_COLOR_TO_TOPO	;95eb
	call sub_9614h	;95ee
	call ROTATE_SOFT	;95f1
	call ROTATE_SOFT	;95f4
	call ROTATE_SOFT	;95f7
	call REFLECTION_ANIMATION	;95fa
	call NICE_GLINT	;95fd
	ei			    ;9600
	ret			    ;9601

; 
; ********************************************************************
; * Reset the Characters Colour Table (CT) to white over transparent *
; ********************************************************************
RESET_CGT:
	ld hl,COLOR_TABLE	;9602
	ld bc, NUM_COLS * NUM_ROWS * 8  ;9605 Number of bytes in the CT
l9608h:
	ld a,COLOR_WHITE_TRANSPARENT    ;9608
	call WRTVRM		                ;960a
	inc hl			                ;960d
	dec bc			                ;960e
	ld a,b			                ;960f
	or c			                ;9610 Finished?
	jr nz,l9608h		            ;9611 No, keep copying
	ret			                    ;9613

sub_9614h:
	ld hl, COLOR_TABLE_POS_S	;9614
	ld a,COLOR_RED_BLACK		;9617
    ; Repeat the following 2 times
	ld c,2  		;9619
l961bh:
    ; Copy 64 times to HL++
	ld b,64 		;961b
l961dh:
	call WRTVRM		;961d Set value 0x81 to CGT
	inc hl			;9620
	djnz l961dh		;9621

	ld de,192   	;9623
	add hl,de		;9626 HL += 192
	dec c			;9627
	jr nz,l961bh	;9628 Repeat a second time
	ret			    ;962a

ADD_COLOR_TO_TOPO:
	ld de,00008h		;962b	11 08 00 	. . . 
	ld hl,02658h		;962e	21 58 26 	! X & 
l9631h:
	push hl			;9631	e5 	. 
	ld b,005h		;9632	06 05 	. . 
	push de			;9634	d5 	. 
l9635h:
	ld a,071h		;9635	3e 71 	> q 
	call sub_967eh		;9637	cd 7e 96 	. ~ . 
	ld de,000f8h		;963a	11 f8 00 	. . . 
	add hl,de			;963d	19 	. 
	djnz l9635h		;963e	10 f5 	. . 
	ld b,006h		;9640	06 06 	. . 
l9642h:
	ld a,031h		;9642	3e 31 	> 1 
	call sub_967eh		;9644	cd 7e 96 	. ~ . 
	add hl,de			;9647	19 	. 
	djnz l9642h		;9648	10 f8 	. . 
	pop de			;964a	d1 	. 
	pop hl			;964b	e1 	. 
	push hl			;964c	e5 	. 
	add hl,de			;964d	19 	. 
	push de			;964e	d5 	. 
	ld b,005h		;964f	06 05 	. . 
l9651h:
	ld a,071h		;9651	3e 71 	> q 
	call sub_967eh		;9653	cd 7e 96 	. ~ . 
	ld de,000f8h		;9656	11 f8 00 	. . . 
	add hl,de			;9659	19 	. 
	djnz l9651h		;965a	10 f5 	. . 
	ld b,006h		;965c	06 06 	. . 
l965eh:
	ld a,031h		;965e	3e 31 	> 1 
	call sub_967eh		;9660	cd 7e 96 	. ~ . 
	add hl,de			;9663	19 	. 
	djnz l965eh		;9664	10 f8 	. . 
	pop de			;9666	d1 	. 
	pop hl			;9667	e1 	. 
	ld a,e			;9668	7b 	{ 
	cp 098h		;9669	fe 98 	. . 
	ret z			;966b	c8 	. 
	ei			;966c	fb 	. 
	ld b,004h		;966d	06 04 	. . 
l966fh:
	halt			;966f	76 	v 
	djnz l966fh		;9670	10 fd 	. . 
	di			;9672	f3 	. 
	add a,010h		;9673	c6 10 	. . 
	ld e,a			;9675	5f 	_ 
	push de			;9676	d5 	. 
	ld de,0fff8h		;9677	11 f8 ff 	. . . 
	add hl,de			;967a	19 	. 
	pop de			;967b	d1 	. 
	jr l9631h		;967c	18 b3 	. . 
sub_967eh:
	ld c,008h		;967e	0e 08 	. . 
l9680h:
	call WRTVRM		;9680	cd 4d 00 	. M . 
	inc hl			;9683	23 	# 
	dec c			;9684	0d 	. 
	jr nz,l9680h		;9685	20 f9 	  . 
	ret			;9687	c9 	. 
sub_9688h:
	ld hl,00000h		;9688	21 00 00 	! . . 
	ld de,0c000h		;968b	11 00 c0 	. . . 
	ld bc,01800h		;968e	01 00 18 	. . . 
	jp 00059h		;9691	c3 59 00 	. Y . 

TABLE_1:
    dw 0x0, 0x82, 0x104, 0x186, 0x208, 0x28a, 0x30c, 0x38e
    dw 0x6a8, 0x7ea, 0x9cc, 0xbfe, 0xc78, 0xcf2, 0xd6c, 0x0101
    dw 0x202, 0x303, 0x404, 0x505, 0x606, 0x505, 0x404, 0x303
    dw 0x202, 0x101, 0x0, 0xffff, 0x630, 0x530, 0x430, 0x438
    dw 0x338, 0x240, 0x248, 0x150, 0x258, 0x260, 0x368, 0x470
    dw 0x570, 0x670, 0xbff, 0xd0c, 0xb0c, 0xd0c, 0xff0e, 0x0
TEMP_PATTERNS_ADDRESS:
	ld c,0a5h		;96f4
STORE_2:
	ld (de),a		;96f6
	sub a			;96f7

TABLE_3:
    dw 0xc000, 0xc100, 0xc200, 0xc300, 0xc400, 0xc500, 0xc600, 0xc700
    dw 0xc800, 0xc900, 0xca00, 0xcb00, 0xcc00, 0xcd00, 0xce00, 0xcf00
    dw 0xd000, 0xd100, 0xd200, 0xd300, 0xd400, 0xd500, 0xd600, 0xd700    
    

TABLE_2:
	ld b,b			;9728	40 	@ 
	ld (bc),a			;9729	02 	. 
	nop			;972a	00 	. 
	nop			;972b	00 	. 
	nop			;972c	00 	. 
	rra			;972d	1f 	. 
	ccf			;972e	3f 	? 
	ld a,b			;972f	78 	x 
	ld (hl),b			;9730	70 	p 
	ccf			;9731	3f 	? 
	nop			;9732	00 	. 
	nop			;9733	00 	. 
	nop			;9734	00 	. 
	ret po			;9735	e0 	. 
	ret m			;9736	f8 	. 
	inc a			;9737	3c 	< 
	ld bc,000fdh		;9738	01 fd 00 	. . . 
	nop			;973b	00 	. 
	nop			;973c	00 	. 
	rra			;973d	1f 	. 
	ld a,a			;973e	7f 	 
	ret m			;973f	f8 	. 
	ret nz			;9740	c0 	. 
	add a,b			;9741	80 	. 
	nop			;9742	00 	. 
	nop			;9743	00 	. 
	nop			;9744	00 	. 
	ret p			;9745	f0 	. 
	call m,0073eh		;9746	fc 3e 07 	. > . 
	inc bc			;9749	03 	. 
	nop			;974a	00 	. 
	nop			;974b	00 	. 
	nop			;974c	00 	. 
	inc bc			;974d	03 	. 
	rlca			;974e	07 	. 
	ld e,038h		;974f	1e 38 	. 8 
	ccf			;9751	3f 	? 
	nop			;9752	00 	. 
	nop			;9753	00 	. 
	nop			;9754	00 	. 
	rst 38h			;9755	ff 	. 
	rst 38h			;9756	ff 	. 
	nop			;9757	00 	. 
	nop			;9758	00 	. 
	add a,b			;9759	80 	. 
	nop			;975a	00 	. 
	nop			;975b	00 	. 
	nop			;975c	00 	. 
	sbc a,a			;975d	9f 	. 
	sbc a,a			;975e	9f 	. 
	sub b			;975f	90 	. 
	nop			;9760	00 	. 
	nop			;9761	00 	. 
	nop			;9762	00 	. 
	nop			;9763	00 	. 
	nop			;9764	00 	. 
	cp 0feh		;9765	fe fe 	. . 
	jp nz,0c0c0h		;9767	c2 c0 c0 	. . . 
	rrca			;976a	0f 	. 
	ld (hl),b			;976b	70 	p 
	jr c,l978dh		;976c	38 1f 	8 . 
	rlca			;976e	07 	. 
	nop			;976f	00 	. 
	nop			;9770	00 	. 
	nop			;9771	00 	. 
	defb 0fdh,01ch,038h	;illegal sequence		;9772	fd 1c 38 	. . 8 
	ret p			;9775	f0 	. 
	ret nz			;9776	c0 	. 
	nop			;9777	00 	. 
	nop			;9778	00 	. 
	nop			;9779	00 	. 
	ret nz			;977a	c0 	. 
	ret p			;977b	f0 	. 
	ld a,h			;977c	7c 	| 
	rra			;977d	1f 	. 
	rlca			;977e	07 	. 
	nop			;977f	00 	. 
	nop			;9780	00 	. 
	nop			;9781	00 	. 
	rlca			;9782	07 	. 
	ld e,07ch		;9783	1e 7c 	. | 
	ret p			;9785	f0 	. 
	ret nz			;9786	c0 	. 
	nop			;9787	00 	. 
	nop			;9788	00 	. 
	nop			;9789	00 	. 
	ccf			;978a	3f 	? 
	jr nc,l97bdh		;978b	30 30 	0 0 
l978dh:
	jr nc,l97bfh		;978d	30 30 	0 0 
	nop			;978f	00 	. 
	nop			;9790	00 	. 
	nop			;9791	00 	. 
	add a,b			;9792	80 	. 
	nop			;9793	00 	. 
	nop			;9794	00 	. 
	nop			;9795	00 	. 
	nop			;9796	00 	. 
	nop			;9797	00 	. 
	nop			;9798	00 	. 
	nop			;9799	00 	. 
	nop			;979a	00 	. 
	nop			;979b	00 	. 
	nop			;979c	00 	. 
	nop			;979d	00 	. 
	nop			;979e	00 	. 
	nop			;979f	00 	. 
	nop			;97a0	00 	. 
	nop			;97a1	00 	. 
	ret nz			;97a2	c0 	. 
	ret nz			;97a3	c0 	. 
	ret nz			;97a4	c0 	. 
	ret nz			;97a5	c0 	. 
	ret nz			;97a6	c0 	. 
	nop			;97a7	00 	. 
	nop			;97a8	00 	. 
	nop			;97a9	00 	. 
	ld b,b			;97aa	40 	@ 
	ld (bc),a			;97ab	02 	. 
	nop			;97ac	00 	. 
	nop			;97ad	00 	. 
	nop			;97ae	00 	. 
	nop			;97af	00 	. 
	nop			;97b0	00 	. 
	nop			;97b1	00 	. 
	rra			;97b2	1f 	. 
	ld a,b			;97b3	78 	x 
	nop			;97b4	00 	. 
	nop			;97b5	00 	. 
	nop			;97b6	00 	. 
	nop			;97b7	00 	. 
	nop			;97b8	00 	. 
	nop			;97b9	00 	. 
	ret po			;97ba	e0 	. 
	inc a			;97bb	3c 	< 
	nop			;97bc	00 	. 
l97bdh:
	nop			;97bd	00 	. 
	nop			;97be	00 	. 
l97bfh:
	nop			;97bf	00 	. 
	nop			;97c0	00 	. 
	nop			;97c1	00 	. 
	rra			;97c2	1f 	. 
	ret m			;97c3	f8 	. 
	nop			;97c4	00 	. 
	nop			;97c5	00 	. 
	nop			;97c6	00 	. 
	nop			;97c7	00 	. 
	nop			;97c8	00 	. 
	nop			;97c9	00 	. 
	ret p			;97ca	f0 	. 
	ld e,000h		;97cb	1e 00 	. . 
	nop			;97cd	00 	. 
	nop			;97ce	00 	. 
	nop			;97cf	00 	. 
	nop			;97d0	00 	. 
	nop			;97d1	00 	. 
	inc bc			;97d2	03 	. 
	ld e,000h		;97d3	1e 00 	. . 
	nop			;97d5	00 	. 
	nop			;97d6	00 	. 
	nop			;97d7	00 	. 
	nop			;97d8	00 	. 
	nop			;97d9	00 	. 
	rst 38h			;97da	ff 	. 
	nop			;97db	00 	. 
	nop			;97dc	00 	. 
	nop			;97dd	00 	. 
	nop			;97de	00 	. 
	nop			;97df	00 	. 
	nop			;97e0	00 	. 
	nop			;97e1	00 	. 
	sbc a,a			;97e2	9f 	. 
	sub b			;97e3	90 	. 
	nop			;97e4	00 	. 
	nop			;97e5	00 	. 
	nop			;97e6	00 	. 
	nop			;97e7	00 	. 
	nop			;97e8	00 	. 
	nop			;97e9	00 	. 
	cp 0c2h		;97ea	fe c2 	. . 
	ccf			;97ec	3f 	? 
	ld (hl),b			;97ed	70 	p 
	rra			;97ee	1f 	. 
	nop			;97ef	00 	. 
	nop			;97f0	00 	. 
	nop			;97f1	00 	. 
	nop			;97f2	00 	. 
	nop			;97f3	00 	. 
	defb 0fdh,01ch,0f0h	;illegal sequence		;97f4	fd 1c f0 	. . . 
	nop			;97f7	00 	. 
	nop			;97f8	00 	. 
	nop			;97f9	00 	. 
	nop			;97fa	00 	. 
	nop			;97fb	00 	. 
	add a,b			;97fc	80 	. 
	ret p			;97fd	f0 	. 
	ccf			;97fe	3f 	? 
	nop			;97ff	00 	. 
	nop			;9800	00 	. 
	nop			;9801	00 	. 
	nop			;9802	00 	. 
	nop			;9803	00 	. 
	inc bc			;9804	03 	. 
	ld e,0f8h		;9805	1e f8 	. . 
	nop			;9807	00 	. 
	nop			;9808	00 	. 
	nop			;9809	00 	. 
	nop			;980a	00 	. 
	nop			;980b	00 	. 
	ccf			;980c	3f 	? 
	jr nc,l983fh		;980d	30 30 	0 0 
	nop			;980f	00 	. 
	nop			;9810	00 	. 
	nop			;9811	00 	. 
	nop			;9812	00 	. 
	nop			;9813	00 	. 
	add a,b			;9814	80 	. 
	nop			;9815	00 	. 
	nop			;9816	00 	. 
	nop			;9817	00 	. 
	nop			;9818	00 	. 
	nop			;9819	00 	. 
	nop			;981a	00 	. 
	nop			;981b	00 	. 
	nop			;981c	00 	. 
	nop			;981d	00 	. 
	nop			;981e	00 	. 
	nop			;981f	00 	. 
	nop			;9820	00 	. 
	nop			;9821	00 	. 
	nop			;9822	00 	. 
	nop			;9823	00 	. 
	ret nz			;9824	c0 	. 
	ret nz			;9825	c0 	. 
	ret nz			;9826	c0 	. 
	nop			;9827	00 	. 
	nop			;9828	00 	. 
	nop			;9829	00 	. 
	nop			;982a	00 	. 
	nop			;982b	00 	. 
	ld b,b			;982c	40 	@ 
	ld (bc),a			;982d	02 	. 
	nop			;982e	00 	. 
	nop			;982f	00 	. 
	nop			;9830	00 	. 
	nop			;9831	00 	. 
	nop			;9832	00 	. 
	nop			;9833	00 	. 
	nop			;9834	00 	. 
	rra			;9835	1f 	. 
	nop			;9836	00 	. 
	nop			;9837	00 	. 
	nop			;9838	00 	. 
	nop			;9839	00 	. 
	nop			;983a	00 	. 
	nop			;983b	00 	. 
	nop			;983c	00 	. 
	ret po			;983d	e0 	. 
	nop			;983e	00 	. 
l983fh:
	nop			;983f	00 	. 
	nop			;9840	00 	. 
	nop			;9841	00 	. 
	nop			;9842	00 	. 
	nop			;9843	00 	. 
	nop			;9844	00 	. 
	ld a,a			;9845	7f 	 
	nop			;9846	00 	. 
	nop			;9847	00 	. 
	nop			;9848	00 	. 
	nop			;9849	00 	. 
	nop			;984a	00 	. 
	nop			;984b	00 	. 
	nop			;984c	00 	. 
	call m,00000h		;984d	fc 00 00 	. . . 
	nop			;9850	00 	. 
	nop			;9851	00 	. 
	nop			;9852	00 	. 
	nop			;9853	00 	. 
	nop			;9854	00 	. 
	inc bc			;9855	03 	. 
	nop			;9856	00 	. 
	nop			;9857	00 	. 
	nop			;9858	00 	. 
	nop			;9859	00 	. 
	nop			;985a	00 	. 
	nop			;985b	00 	. 
	nop			;985c	00 	. 
	rst 38h			;985d	ff 	. 
	nop			;985e	00 	. 
	nop			;985f	00 	. 
	nop			;9860	00 	. 
	nop			;9861	00 	. 
	nop			;9862	00 	. 
	nop			;9863	00 	. 
	nop			;9864	00 	. 
	sbc a,a			;9865	9f 	. 
	nop			;9866	00 	. 
	nop			;9867	00 	. 
	nop			;9868	00 	. 
	nop			;9869	00 	. 
	nop			;986a	00 	. 
	nop			;986b	00 	. 
	nop			;986c	00 	. 
	cp 03fh		;986d	fe 3f 	. ? 
	rra			;986f	1f 	. 
	nop			;9870	00 	. 
	nop			;9871	00 	. 
	nop			;9872	00 	. 
	nop			;9873	00 	. 
	nop			;9874	00 	. 
	nop			;9875	00 	. 
	defb 0fdh,0f0h,000h	;illegal sequence		;9876	fd f0 00 	. . . 
	nop			;9879	00 	. 
	nop			;987a	00 	. 
	nop			;987b	00 	. 
	nop			;987c	00 	. 
	nop			;987d	00 	. 
	ret nz			;987e	c0 	. 
	ld a,a			;987f	7f 	 
	nop			;9880	00 	. 
	nop			;9881	00 	. 
	nop			;9882	00 	. 
	nop			;9883	00 	. 
	nop			;9884	00 	. 
	nop			;9885	00 	. 
	rlca			;9886	07 	. 
	call m,00000h		;9887	fc 00 00 	. . . 
	nop			;988a	00 	. 
	nop			;988b	00 	. 
	nop			;988c	00 	. 
	nop			;988d	00 	. 
	ccf			;988e	3f 	? 
	jr nc,l9891h		;988f	30 00 	0 . 
l9891h:
	nop			;9891	00 	. 
	nop			;9892	00 	. 
	nop			;9893	00 	. 
	nop			;9894	00 	. 
	nop			;9895	00 	. 
	add a,b			;9896	80 	. 
	nop			;9897	00 	. 
	nop			;9898	00 	. 
	nop			;9899	00 	. 
	nop			;989a	00 	. 
	nop			;989b	00 	. 
	nop			;989c	00 	. 
	nop			;989d	00 	. 
	nop			;989e	00 	. 
	nop			;989f	00 	. 
	nop			;98a0	00 	. 
	nop			;98a1	00 	. 
	nop			;98a2	00 	. 
	nop			;98a3	00 	. 
	nop			;98a4	00 	. 
	nop			;98a5	00 	. 
	ret nz			;98a6	c0 	. 
	ret nz			;98a7	c0 	. 
	nop			;98a8	00 	. 
	nop			;98a9	00 	. 
	nop			;98aa	00 	. 
	nop			;98ab	00 	. 
	nop			;98ac	00 	. 
	nop			;98ad	00 	. 
	ld b,b			;98ae	40 	@ 
	ld (bc),a			;98af	02 	. 
	nop			;98b0	00 	. 
	nop			;98b1	00 	. 
	nop			;98b2	00 	. 
	nop			;98b3	00 	. 
	nop			;98b4	00 	. 
	nop			;98b5	00 	. 
	nop			;98b6	00 	. 
	nop			;98b7	00 	. 
	nop			;98b8	00 	. 
	nop			;98b9	00 	. 
	nop			;98ba	00 	. 
	nop			;98bb	00 	. 
	nop			;98bc	00 	. 
	nop			;98bd	00 	. 
	nop			;98be	00 	. 
	nop			;98bf	00 	. 
	nop			;98c0	00 	. 
	nop			;98c1	00 	. 
	nop			;98c2	00 	. 
	nop			;98c3	00 	. 
	nop			;98c4	00 	. 
	nop			;98c5	00 	. 
	nop			;98c6	00 	. 
	nop			;98c7	00 	. 
	nop			;98c8	00 	. 
	nop			;98c9	00 	. 
	nop			;98ca	00 	. 
	nop			;98cb	00 	. 
	nop			;98cc	00 	. 
	nop			;98cd	00 	. 
	nop			;98ce	00 	. 
	nop			;98cf	00 	. 
	nop			;98d0	00 	. 
	nop			;98d1	00 	. 
	nop			;98d2	00 	. 
	nop			;98d3	00 	. 
	nop			;98d4	00 	. 
	nop			;98d5	00 	. 
	nop			;98d6	00 	. 
	nop			;98d7	00 	. 
	nop			;98d8	00 	. 
	nop			;98d9	00 	. 
	nop			;98da	00 	. 
	nop			;98db	00 	. 
	nop			;98dc	00 	. 
	nop			;98dd	00 	. 
	nop			;98de	00 	. 
	nop			;98df	00 	. 
	nop			;98e0	00 	. 
	nop			;98e1	00 	. 
	nop			;98e2	00 	. 
	nop			;98e3	00 	. 
	nop			;98e4	00 	. 
	nop			;98e5	00 	. 
	nop			;98e6	00 	. 
	nop			;98e7	00 	. 
	nop			;98e8	00 	. 
	nop			;98e9	00 	. 
	nop			;98ea	00 	. 
	nop			;98eb	00 	. 
	nop			;98ec	00 	. 
	nop			;98ed	00 	. 
	nop			;98ee	00 	. 
	nop			;98ef	00 	. 
	ccf			;98f0	3f 	? 
	nop			;98f1	00 	. 
	nop			;98f2	00 	. 
	nop			;98f3	00 	. 
	nop			;98f4	00 	. 
	nop			;98f5	00 	. 
	nop			;98f6	00 	. 
	nop			;98f7	00 	. 
	defb 0fdh,000h,000h	;illegal sequence		;98f8	fd 00 00 	. . . 
	nop			;98fb	00 	. 
	nop			;98fc	00 	. 
	nop			;98fd	00 	. 
	nop			;98fe	00 	. 
	nop			;98ff	00 	. 
	rst 38h			;9900	ff 	. 
	nop			;9901	00 	. 
	nop			;9902	00 	. 
	nop			;9903	00 	. 
	nop			;9904	00 	. 
	nop			;9905	00 	. 
	nop			;9906	00 	. 
	nop			;9907	00 	. 
	rst 38h			;9908	ff 	. 
	nop			;9909	00 	. 
	nop			;990a	00 	. 
	nop			;990b	00 	. 
	nop			;990c	00 	. 
	nop			;990d	00 	. 
	nop			;990e	00 	. 
	nop			;990f	00 	. 
	ccf			;9910	3f 	? 
	nop			;9911	00 	. 
	nop			;9912	00 	. 
	nop			;9913	00 	. 
	nop			;9914	00 	. 
	nop			;9915	00 	. 
	nop			;9916	00 	. 
	nop			;9917	00 	. 
	rst 38h			;9918	ff 	. 
	nop			;9919	00 	. 
	nop			;991a	00 	. 
	nop			;991b	00 	. 
	nop			;991c	00 	. 
	nop			;991d	00 	. 
	nop			;991e	00 	. 
	nop			;991f	00 	. 
	sbc a,a			;9920	9f 	. 
	nop			;9921	00 	. 
	nop			;9922	00 	. 
	nop			;9923	00 	. 
	nop			;9924	00 	. 
	nop			;9925	00 	. 
	nop			;9926	00 	. 
	nop			;9927	00 	. 
	cp 000h		;9928	fe 00 	. . 
	nop			;992a	00 	. 
	nop			;992b	00 	. 
	nop			;992c	00 	. 
	nop			;992d	00 	. 
	nop			;992e	00 	. 
	nop			;992f	00 	. 
	ld b,b			;9930	40 	@ 
	ld (bc),a			;9931	02 	. 
	nop			;9932	00 	. 
	nop			;9933	00 	. 
	nop			;9934	00 	. 
	nop			;9935	00 	. 
	nop			;9936	00 	. 
	nop			;9937	00 	. 
	nop			;9938	00 	. 
	rra			;9939	1f 	. 
	nop			;993a	00 	. 
	nop			;993b	00 	. 
	nop			;993c	00 	. 
	nop			;993d	00 	. 
	nop			;993e	00 	. 
	nop			;993f	00 	. 
	nop			;9940	00 	. 
	ret p			;9941	f0 	. 
	nop			;9942	00 	. 
	nop			;9943	00 	. 
	nop			;9944	00 	. 
	nop			;9945	00 	. 
	nop			;9946	00 	. 
	nop			;9947	00 	. 
	nop			;9948	00 	. 
	ld a,a			;9949	7f 	 
	nop			;994a	00 	. 
	nop			;994b	00 	. 
	nop			;994c	00 	. 
	nop			;994d	00 	. 
	nop			;994e	00 	. 
	nop			;994f	00 	. 
	nop			;9950	00 	. 
	call m,00000h		;9951	fc 00 00 	. . . 
	nop			;9954	00 	. 
	nop			;9955	00 	. 
	nop			;9956	00 	. 
	nop			;9957	00 	. 
	nop			;9958	00 	. 
	jr nc,l995bh		;9959	30 00 	0 . 
l995bh:
	nop			;995b	00 	. 
	nop			;995c	00 	. 
	nop			;995d	00 	. 
	nop			;995e	00 	. 
	nop			;995f	00 	. 
	nop			;9960	00 	. 
	nop			;9961	00 	. 
	nop			;9962	00 	. 
	nop			;9963	00 	. 
	nop			;9964	00 	. 
	nop			;9965	00 	. 
	nop			;9966	00 	. 
	nop			;9967	00 	. 
	nop			;9968	00 	. 
	nop			;9969	00 	. 
	nop			;996a	00 	. 
	nop			;996b	00 	. 
	nop			;996c	00 	. 
	nop			;996d	00 	. 
	nop			;996e	00 	. 
	nop			;996f	00 	. 
	nop			;9970	00 	. 
	ret nz			;9971	c0 	. 
	ccf			;9972	3f 	? 
	rra			;9973	1f 	. 
	nop			;9974	00 	. 
	nop			;9975	00 	. 
	nop			;9976	00 	. 
	nop			;9977	00 	. 
	nop			;9978	00 	. 
	nop			;9979	00 	. 
	defb 0fdh,0e0h,000h	;illegal sequence		;997a	fd e0 00 	. . . 
	nop			;997d	00 	. 
	nop			;997e	00 	. 
	nop			;997f	00 	. 
	nop			;9980	00 	. 
	nop			;9981	00 	. 
	ret nz			;9982	c0 	. 
	ld a,a			;9983	7f 	 
	nop			;9984	00 	. 
	nop			;9985	00 	. 
	nop			;9986	00 	. 
	nop			;9987	00 	. 
	nop			;9988	00 	. 
	nop			;9989	00 	. 
	rlca			;998a	07 	. 
	call m,00000h		;998b	fc 00 00 	. . . 
	nop			;998e	00 	. 
	nop			;998f	00 	. 
	nop			;9990	00 	. 
	nop			;9991	00 	. 
	ccf			;9992	3f 	? 
	inc bc			;9993	03 	. 
	nop			;9994	00 	. 
	nop			;9995	00 	. 
	nop			;9996	00 	. 
	nop			;9997	00 	. 
	nop			;9998	00 	. 
	nop			;9999	00 	. 
	add a,b			;999a	80 	. 
	rst 38h			;999b	ff 	. 
	nop			;999c	00 	. 
	nop			;999d	00 	. 
	nop			;999e	00 	. 
	nop			;999f	00 	. 
	nop			;99a0	00 	. 
	nop			;99a1	00 	. 
	nop			;99a2	00 	. 
	sbc a,a			;99a3	9f 	. 
	nop			;99a4	00 	. 
	nop			;99a5	00 	. 
	nop			;99a6	00 	. 
	nop			;99a7	00 	. 
	nop			;99a8	00 	. 
	nop			;99a9	00 	. 
	ret nz			;99aa	c0 	. 
	cp 000h		;99ab	fe 00 	. . 
	nop			;99ad	00 	. 
	nop			;99ae	00 	. 
	nop			;99af	00 	. 
	nop			;99b0	00 	. 
	nop			;99b1	00 	. 
	ld b,b			;99b2	40 	@ 
	ld (bc),a			;99b3	02 	. 
	nop			;99b4	00 	. 
	nop			;99b5	00 	. 
	nop			;99b6	00 	. 
	nop			;99b7	00 	. 
	nop			;99b8	00 	. 
	nop			;99b9	00 	. 
	rra			;99ba	1f 	. 
	ld (hl),b			;99bb	70 	p 
	nop			;99bc	00 	. 
	nop			;99bd	00 	. 
	nop			;99be	00 	. 
	nop			;99bf	00 	. 
	nop			;99c0	00 	. 
	nop			;99c1	00 	. 
	ret p			;99c2	f0 	. 
	inc e			;99c3	1c 	. 
	nop			;99c4	00 	. 
	nop			;99c5	00 	. 
	nop			;99c6	00 	. 
	nop			;99c7	00 	. 
	nop			;99c8	00 	. 
	nop			;99c9	00 	. 
	ccf			;99ca	3f 	? 
	ret p			;99cb	f0 	. 
	nop			;99cc	00 	. 
	nop			;99cd	00 	. 
	nop			;99ce	00 	. 
	nop			;99cf	00 	. 
	nop			;99d0	00 	. 
	nop			;99d1	00 	. 
	ret m			;99d2	f8 	. 
	ld e,000h		;99d3	1e 00 	. . 
	nop			;99d5	00 	. 
	nop			;99d6	00 	. 
	nop			;99d7	00 	. 
	nop			;99d8	00 	. 
	nop			;99d9	00 	. 
	jr nc,l9a0ch		;99da	30 30 	0 0 
	nop			;99dc	00 	. 
	nop			;99dd	00 	. 
	nop			;99de	00 	. 
	nop			;99df	00 	. 
	nop			;99e0	00 	. 
	nop			;99e1	00 	. 
	nop			;99e2	00 	. 
	nop			;99e3	00 	. 
	nop			;99e4	00 	. 
	nop			;99e5	00 	. 
	nop			;99e6	00 	. 
	nop			;99e7	00 	. 
	nop			;99e8	00 	. 
	nop			;99e9	00 	. 
	nop			;99ea	00 	. 
	nop			;99eb	00 	. 
	nop			;99ec	00 	. 
	nop			;99ed	00 	. 
	nop			;99ee	00 	. 
	nop			;99ef	00 	. 
	nop			;99f0	00 	. 
	nop			;99f1	00 	. 
	ret nz			;99f2	c0 	. 
	ret nz			;99f3	c0 	. 
	ccf			;99f4	3f 	? 
	ld a,b			;99f5	78 	x 
	rra			;99f6	1f 	. 
	nop			;99f7	00 	. 
	nop			;99f8	00 	. 
	nop			;99f9	00 	. 
	nop			;99fa	00 	. 
	nop			;99fb	00 	. 
	defb 0fdh,03ch,0e0h	;illegal sequence		;99fc	fd 3c e0 	. < . 
	nop			;99ff	00 	. 
	nop			;9a00	00 	. 
	nop			;9a01	00 	. 
	nop			;9a02	00 	. 
	nop			;9a03	00 	. 
	add a,b			;9a04	80 	. 
	ret m			;9a05	f8 	. 
	rra			;9a06	1f 	. 
	nop			;9a07	00 	. 
	nop			;9a08	00 	. 
	nop			;9a09	00 	. 
	nop			;9a0a	00 	. 
	nop			;9a0b	00 	. 
l9a0ch:
	inc bc			;9a0c	03 	. 
	ld e,0f0h		;9a0d	1e f0 	. . 
	nop			;9a0f	00 	. 
	nop			;9a10	00 	. 
	nop			;9a11	00 	. 
	nop			;9a12	00 	. 
	nop			;9a13	00 	. 
	ccf			;9a14	3f 	? 
	ld e,003h		;9a15	1e 03 	. . 
	nop			;9a17	00 	. 
	nop			;9a18	00 	. 
	nop			;9a19	00 	. 
	nop			;9a1a	00 	. 
	nop			;9a1b	00 	. 
	add a,b			;9a1c	80 	. 
	nop			;9a1d	00 	. 
	rst 38h			;9a1e	ff 	. 
	nop			;9a1f	00 	. 
	nop			;9a20	00 	. 
	nop			;9a21	00 	. 
	nop			;9a22	00 	. 
	nop			;9a23	00 	. 
	nop			;9a24	00 	. 
	sub b			;9a25	90 	. 
	sbc a,a			;9a26	9f 	. 
	nop			;9a27	00 	. 
	nop			;9a28	00 	. 
	nop			;9a29	00 	. 
	nop			;9a2a	00 	. 
	nop			;9a2b	00 	. 
	ret nz			;9a2c	c0 	. 
	jp nz,000feh		;9a2d	c2 fe 00 	. . . 
	nop			;9a30	00 	. 
	nop			;9a31	00 	. 
	nop			;9a32	00 	. 
	nop			;9a33	00 	. 
	ld b,b			;9a34	40 	@ 
	ld (bc),a			;9a35	02 	. 
	nop			;9a36	00 	. 
	nop			;9a37	00 	. 
	nop			;9a38	00 	. 
	rlca			;9a39	07 	. 
	rra			;9a3a	1f 	. 
	jr c,l9aadh		;9a3b	38 70 	8 p 
	rrca			;9a3d	0f 	. 
	nop			;9a3e	00 	. 
	nop			;9a3f	00 	. 
	nop			;9a40	00 	. 
	ret nz			;9a41	c0 	. 
	ret p			;9a42	f0 	. 
	jr c,l9a61h		;9a43	38 1c 	8 . 
	defb 0fdh,000h,000h	;illegal sequence		;9a45	fd 00 00 	. . . 
	nop			;9a48	00 	. 
	rlca			;9a49	07 	. 
	rra			;9a4a	1f 	. 
	ld a,h			;9a4b	7c 	| 
	ret p			;9a4c	f0 	. 
	ret nz			;9a4d	c0 	. 
	nop			;9a4e	00 	. 
	nop			;9a4f	00 	. 
	nop			;9a50	00 	. 
	ret nz			;9a51	c0 	. 
	ret p			;9a52	f0 	. 
	ld a,h			;9a53	7c 	| 
	ld e,007h		;9a54	1e 07 	. . 
	nop			;9a56	00 	. 
	nop			;9a57	00 	. 
	nop			;9a58	00 	. 
	jr nc,l9a8bh		;9a59	30 30 	0 0 
	jr nc,l9a8dh		;9a5b	30 30 	0 0 
	ccf			;9a5d	3f 	? 
	nop			;9a5e	00 	. 
	nop			;9a5f	00 	. 
	nop			;9a60	00 	. 
l9a61h:
	nop			;9a61	00 	. 
	nop			;9a62	00 	. 
	nop			;9a63	00 	. 
	nop			;9a64	00 	. 
	add a,b			;9a65	80 	. 
	nop			;9a66	00 	. 
	nop			;9a67	00 	. 
	nop			;9a68	00 	. 
	nop			;9a69	00 	. 
	nop			;9a6a	00 	. 
	nop			;9a6b	00 	. 
	nop			;9a6c	00 	. 
	nop			;9a6d	00 	. 
	nop			;9a6e	00 	. 
	nop			;9a6f	00 	. 
	nop			;9a70	00 	. 
	ret nz			;9a71	c0 	. 
	ret nz			;9a72	c0 	. 
	ret nz			;9a73	c0 	. 
	ret nz			;9a74	c0 	. 
	ret nz			;9a75	c0 	. 
	ccf			;9a76	3f 	? 
	ld (hl),b			;9a77	70 	p 
	ld a,b			;9a78	78 	x 
	ccf			;9a79	3f 	? 
	rra			;9a7a	1f 	. 
	nop			;9a7b	00 	. 
	nop			;9a7c	00 	. 
	nop			;9a7d	00 	. 
	defb 0fdh,001h,03ch	;illegal sequence		;9a7e	fd 01 3c 	. . < 
	ret m			;9a81	f8 	. 
	ret po			;9a82	e0 	. 
	nop			;9a83	00 	. 
	nop			;9a84	00 	. 
	nop			;9a85	00 	. 
	add a,b			;9a86	80 	. 
	ret nz			;9a87	c0 	. 
	ret m			;9a88	f8 	. 
	ld a,a			;9a89	7f 	 
	rra			;9a8a	1f 	. 
l9a8bh:
	nop			;9a8b	00 	. 
	nop			;9a8c	00 	. 
l9a8dh:
	nop			;9a8d	00 	. 
	inc bc			;9a8e	03 	. 
	rlca			;9a8f	07 	. 
	ld a,0fch		;9a90	3e fc 	> . 
	ret p			;9a92	f0 	. 
	nop			;9a93	00 	. 
	nop			;9a94	00 	. 
	nop			;9a95	00 	. 
	ccf			;9a96	3f 	? 
	jr c,l9ab7h		;9a97	38 1e 	8 . 
	rlca			;9a99	07 	. 
	inc bc			;9a9a	03 	. 
	nop			;9a9b	00 	. 
	nop			;9a9c	00 	. 
	nop			;9a9d	00 	. 
	add a,b			;9a9e	80 	. 
	nop			;9a9f	00 	. 
	nop			;9aa0	00 	. 
	rst 38h			;9aa1	ff 	. 
	rst 38h			;9aa2	ff 	. 
	nop			;9aa3	00 	. 
	nop			;9aa4	00 	. 
	nop			;9aa5	00 	. 
	nop			;9aa6	00 	. 
	nop			;9aa7	00 	. 
	sub b			;9aa8	90 	. 
	sbc a,a			;9aa9	9f 	. 
	sbc a,a			;9aaa	9f 	. 
	nop			;9aab	00 	. 
	nop			;9aac	00 	. 
l9aadh:
	nop			;9aad	00 	. 
	ret nz			;9aae	c0 	. 
	ret nz			;9aaf	c0 	. 
	jp nz,0fefeh		;9ab0	c2 fe fe 	. . . 
	nop			;9ab3	00 	. 
	nop			;9ab4	00 	. 
	nop			;9ab5	00 	. 
	ld c,b			;9ab6	48 	H 
l9ab7h:
	dec bc			;9ab7	0b 	. 
	nop			;9ab8	00 	. 
	nop			;9ab9	00 	. 
	nop			;9aba	00 	. 
	nop			;9abb	00 	. 
	nop			;9abc	00 	. 
	nop			;9abd	00 	. 
	nop			;9abe	00 	. 
	nop			;9abf	00 	. 
	nop			;9ac0	00 	. 
	nop			;9ac1	00 	. 
	inc bc			;9ac2	03 	. 
	ld (bc),a			;9ac3	02 	. 
	ld (bc),a			;9ac4	02 	. 
	ld (bc),a			;9ac5	02 	. 
	ld (bc),a			;9ac6	02 	. 
	ld (bc),a			;9ac7	02 	. 
	nop			;9ac8	00 	. 
	nop			;9ac9	00 	. 
	rst 38h			;9aca	ff 	. 
	nop			;9acb	00 	. 
	ld a,a			;9acc	7f 	 
	ccf			;9acd	3f 	? 
	rra			;9ace	1f 	. 
	nop			;9acf	00 	. 
	nop			;9ad0	00 	. 
	nop			;9ad1	00 	. 
	rst 38h			;9ad2	ff 	. 
	nop			;9ad3	00 	. 
	ld (hl),b			;9ad4	70 	p 
	ld (ix+000h),d		;9ad5	dd 72 00 	. r . 
	nop			;9ad8	00 	. 
	nop			;9ad9	00 	. 
	rst 38h			;9ada	ff 	. 
	nop			;9adb	00 	. 
	nop			;9adc	00 	. 
	ld d,b			;9add	50 	P 
	xor b			;9ade	a8 	. 
	ld bc,00000h		;9adf	01 00 00 	. . . 
	ret p			;9ae2	f0 	. 
	djnz l9af5h		;9ae3	10 10 	. . 
	ld d,b			;9ae5	50 	P 
	ret nc			;9ae6	d0 	. 
	ret nc			;9ae7	d0 	. 
	nop			;9ae8	00 	. 
	nop			;9ae9	00 	. 
	nop			;9aea	00 	. 
	nop			;9aeb	00 	. 
	nop			;9aec	00 	. 
	nop			;9aed	00 	. 
	nop			;9aee	00 	. 
	nop			;9aef	00 	. 
	nop			;9af0	00 	. 
	nop			;9af1	00 	. 
	nop			;9af2	00 	. 
	nop			;9af3	00 	. 
	nop			;9af4	00 	. 
l9af5h:
	nop			;9af5	00 	. 
	nop			;9af6	00 	. 
	nop			;9af7	00 	. 
	nop			;9af8	00 	. 
	nop			;9af9	00 	. 
	nop			;9afa	00 	. 
	nop			;9afb	00 	. 
	nop			;9afc	00 	. 
	nop			;9afd	00 	. 
	nop			;9afe	00 	. 
	nop			;9aff	00 	. 
	nop			;9b00	00 	. 
	nop			;9b01	00 	. 
	nop			;9b02	00 	. 
	nop			;9b03	00 	. 
	nop			;9b04	00 	. 
	nop			;9b05	00 	. 
	nop			;9b06	00 	. 
	nop			;9b07	00 	. 
	ld (bc),a			;9b08	02 	. 
	ld (bc),a			;9b09	02 	. 
	ld (bc),a			;9b0a	02 	. 
	ld (bc),a			;9b0b	02 	. 
	ld (bc),a			;9b0c	02 	. 
	ld (bc),a			;9b0d	02 	. 
	ld (bc),a			;9b0e	02 	. 
	ld (bc),a			;9b0f	02 	. 
	rrca			;9b10	0f 	. 
	ex af,af'			;9b11	08 	. 
	add a,h			;9b12	84 	. 
	adc a,d			;9b13	8a 	. 
	ld b,c			;9b14	41 	A 
	adc a,d			;9b15	8a 	. 
	ld b,l			;9b16	45 	E 
	adc a,b			;9b17	88 	. 
	rst 38h			;9b18	ff 	. 
	nop			;9b19	00 	. 
	nop			;9b1a	00 	. 
	nop			;9b1b	00 	. 
	nop			;9b1c	00 	. 
	nop			;9b1d	00 	. 
	nop			;9b1e	00 	. 
	add a,b			;9b1f	80 	. 
	defb 0fdh,005h,005h	;illegal sequence		;9b20	fd 05 05 	. . . 
	dec b			;9b23	05 	. 
	dec b			;9b24	05 	. 
	dec b			;9b25	05 	. 
	dec b			;9b26	05 	. 
	dec b			;9b27	05 	. 
	ret nc			;9b28	d0 	. 
	ret nc			;9b29	d0 	. 
	ret nc			;9b2a	d0 	. 
	sub b			;9b2b	90 	. 
	ret nc			;9b2c	d0 	. 
	ret nc			;9b2d	d0 	. 
	ld d,b			;9b2e	50 	P 
	ret nc			;9b2f	d0 	. 
	nop			;9b30	00 	. 
	nop			;9b31	00 	. 
	nop			;9b32	00 	. 
	nop			;9b33	00 	. 
	nop			;9b34	00 	. 
	nop			;9b35	00 	. 
	nop			;9b36	00 	. 
	nop			;9b37	00 	. 
	nop			;9b38	00 	. 
	nop			;9b39	00 	. 
	nop			;9b3a	00 	. 
	nop			;9b3b	00 	. 
	nop			;9b3c	00 	. 
	nop			;9b3d	00 	. 
	nop			;9b3e	00 	. 
	nop			;9b3f	00 	. 
	nop			;9b40	00 	. 
	nop			;9b41	00 	. 
	nop			;9b42	00 	. 
	nop			;9b43	00 	. 
	nop			;9b44	00 	. 
	nop			;9b45	00 	. 
	nop			;9b46	00 	. 
	nop			;9b47	00 	. 
	nop			;9b48	00 	. 
	nop			;9b49	00 	. 
	nop			;9b4a	00 	. 
	nop			;9b4b	00 	. 
	nop			;9b4c	00 	. 
	nop			;9b4d	00 	. 
	nop			;9b4e	00 	. 
	nop			;9b4f	00 	. 
	ld (bc),a			;9b50	02 	. 
	ld (bc),a			;9b51	02 	. 
l9b52h:
	ld (bc),a			;9b52	02 	. 
	ld (bc),a			;9b53	02 	. 
	ld (bc),a			;9b54	02 	. 
	ld (bc),a			;9b55	02 	. 
	ld a,020h		;9b56	3e 20 	>   
	ld b,l			;9b58	45 	E 
	ld c,d			;9b59	4a 	J 
	call 0eda8h		;9b5a	cd a8 ed 	. . . 
	ld l,d			;9b5d	6a 	j 
	call 000eah		;9b5e	cd ea 00 	. . . 
	nop			;9b61	00 	. 
	ld b,b			;9b62	40 	@ 
	add a,b			;9b63	80 	. 
	ld b,b			;9b64	40 	@ 
	jr nz,l9ba7h		;9b65	20 40 	  @ 
	and b			;9b67	a0 	. 
	inc b			;9b68	04 	. 
	dec b			;9b69	05 	. 
	dec b			;9b6a	05 	. 
	dec b			;9b6b	05 	. 
	inc b			;9b6c	04 	. 
	dec b			;9b6d	05 	. 
	dec b			;9b6e	05 	. 
	dec b			;9b6f	05 	. 
	sub b			;9b70	90 	. 
	ret nc			;9b71	d0 	. 
	ld d,b			;9b72	50 	P 
	ret nc			;9b73	d0 	. 
	sub b			;9b74	90 	. 
	ret nc			;9b75	d0 	. 
	ld d,b			;9b76	50 	P 
	ret nz			;9b77	c0 	. 
	nop			;9b78	00 	. 
	nop			;9b79	00 	. 
	nop			;9b7a	00 	. 
	nop			;9b7b	00 	. 
	nop			;9b7c	00 	. 
	nop			;9b7d	00 	. 
	nop			;9b7e	00 	. 
	nop			;9b7f	00 	. 
	nop			;9b80	00 	. 
	nop			;9b81	00 	. 
	nop			;9b82	00 	. 
	nop			;9b83	00 	. 
	nop			;9b84	00 	. 
	nop			;9b85	00 	. 
	nop			;9b86	00 	. 
	nop			;9b87	00 	. 
	nop			;9b88	00 	. 
	nop			;9b89	00 	. 
	nop			;9b8a	00 	. 
	nop			;9b8b	00 	. 
	nop			;9b8c	00 	. 
	nop			;9b8d	00 	. 
	nop			;9b8e	00 	. 
	nop			;9b8f	00 	. 
	nop			;9b90	00 	. 
	nop			;9b91	00 	. 
	nop			;9b92	00 	. 
	nop			;9b93	00 	. 
	nop			;9b94	00 	. 
	nop			;9b95	00 	. 
	nop			;9b96	00 	. 
	nop			;9b97	00 	. 
	daa			;9b98	27 	' 
	inc hl			;9b99	23 	# 
	ld hl,02020h		;9b9a	21 20 20 	!     
	jr nz,l9bbfh		;9b9d	20 20 	    
	jr z,l9c0eh		;9b9f	28 6d 	( m 
	xor d			;9ba1	aa 	. 
	call 0ff0eh		;9ba2	cd 0e ff 	. . . 
	cp 0ffh		;9ba5	fe ff 	. . 
l9ba7h:
	rst 38h			;9ba7	ff 	. 
	djnz l9b52h		;9ba8	10 a8 	. . 
	ld b,h			;9baa	44 	D 
	xor b			;9bab	a8 	. 
	ld d,b			;9bac	50 	P 
	xor d			;9bad	aa 	. 
	ld b,l			;9bae	45 	E 
	xor d			;9baf	aa 	. 
	inc b			;9bb0	04 	. 
	dec b			;9bb1	05 	. 
	inc b			;9bb2	04 	. 
	inc b			;9bb3	04 	. 
	rlca			;9bb4	07 	. 
	nop			;9bb5	00 	. 
	nop			;9bb6	00 	. 
	nop			;9bb7	00 	. 
	cp b			;9bb8	b8 	. 
	ld a,h			;9bb9	7c 	| 
	ret m			;9bba	f8 	. 
	nop			;9bbb	00 	. 
	ret p			;9bbc	f0 	. 
	jr nz,l9bdfh		;9bbd	20 20 	    
l9bbfh:
	ld b,b			;9bbf	40 	@ 
	nop			;9bc0	00 	. 
	nop			;9bc1	00 	. 
	nop			;9bc2	00 	. 
	nop			;9bc3	00 	. 
	nop			;9bc4	00 	. 
	nop			;9bc5	00 	. 
	nop			;9bc6	00 	. 
	nop			;9bc7	00 	. 
	nop			;9bc8	00 	. 
	nop			;9bc9	00 	. 
	nop			;9bca	00 	. 
	nop			;9bcb	00 	. 
	nop			;9bcc	00 	. 
	nop			;9bcd	00 	. 
	nop			;9bce	00 	. 
	nop			;9bcf	00 	. 
	nop			;9bd0	00 	. 
	nop			;9bd1	00 	. 
	nop			;9bd2	00 	. 
	nop			;9bd3	00 	. 
	nop			;9bd4	00 	. 
	nop			;9bd5	00 	. 
	nop			;9bd6	00 	. 
	nop			;9bd7	00 	. 
	nop			;9bd8	00 	. 
	nop			;9bd9	00 	. 
	nop			;9bda	00 	. 
	nop			;9bdb	00 	. 
	nop			;9bdc	00 	. 
	nop			;9bdd	00 	. 
	nop			;9bde	00 	. 
l9bdfh:
	nop			;9bdf	00 	. 
	jr nz,l9c0ah		;9be0	20 28 	  ( 
	inc h			;9be2	24 	$ 
	jr z,l9c09h		;9be3	28 24 	( $ 
	jr z,l9c0bh		;9be5	28 24 	( $ 
	jr z,$+1		;9be7	28 ff 	( . 
	rst 38h			;9be9	ff 	. 
	rst 38h			;9bea	ff 	. 
	cp a			;9beb	bf 	. 
	cp a			;9bec	bf 	. 
	cp a			;9bed	bf 	. 
	sbc a,a			;9bee	9f 	. 
	add a,a			;9bef	87 	. 
	pop de			;9bf0	d1 	. 
	jp pe,0ead5h		;9bf1	ea d5 ea 	. . . 
	call p,0fffeh		;9bf4	f4 fe ff 	. . . 
	rst 38h			;9bf7	ff 	. 
	ld b,b			;9bf8	40 	@ 
	jr nz,l9c4bh		;9bf9	20 50 	  P 
	xor b			;9bfb	a8 	. 
	inc b			;9bfc	04 	. 
	xor e			;9bfd	ab 	. 
	ret nc			;9bfe	d0 	. 
	ret m			;9bff	f8 	. 
	ld b,b			;9c00	40 	@ 
	add a,b			;9c01	80 	. 
	add a,b			;9c02	80 	. 
	add a,b			;9c03	80 	. 
	add a,b			;9c04	80 	. 
	nop			;9c05	00 	. 
	nop			;9c06	00 	. 
	nop			;9c07	00 	. 
	nop			;9c08	00 	. 
l9c09h:
	nop			;9c09	00 	. 
l9c0ah:
	nop			;9c0a	00 	. 
l9c0bh:
	nop			;9c0b	00 	. 
	nop			;9c0c	00 	. 
	nop			;9c0d	00 	. 
l9c0eh:
	nop			;9c0e	00 	. 
	nop			;9c0f	00 	. 
	nop			;9c10	00 	. 
	nop			;9c11	00 	. 
	nop			;9c12	00 	. 
	nop			;9c13	00 	. 
	nop			;9c14	00 	. 
	nop			;9c15	00 	. 
	nop			;9c16	00 	. 
	nop			;9c17	00 	. 
	nop			;9c18	00 	. 
	nop			;9c19	00 	. 
	nop			;9c1a	00 	. 
	nop			;9c1b	00 	. 
	nop			;9c1c	00 	. 
	nop			;9c1d	00 	. 
	nop			;9c1e	00 	. 
	nop			;9c1f	00 	. 
	nop			;9c20	00 	. 
	nop			;9c21	00 	. 
	nop			;9c22	00 	. 
	nop			;9c23	00 	. 
	nop			;9c24	00 	. 
	nop			;9c25	00 	. 
	nop			;9c26	00 	. 
	nop			;9c27	00 	. 
	inc h			;9c28	24 	$ 
	jr z,l9c4fh		;9c29	28 24 	( $ 
	jr z,l9c53h		;9c2b	28 26 	( & 
	inc h			;9c2d	24 	$ 
	ld l,02ah		;9c2e	2e 2a 	. * 
	add a,b			;9c30	80 	. 
	add a,b			;9c31	80 	. 
	add a,b			;9c32	80 	. 
	add a,b			;9c33	80 	. 
	add a,b			;9c34	80 	. 
	add a,b			;9c35	80 	. 
	call m,00004h		;9c36	fc 04 00 	. . . 
	nop			;9c39	00 	. 
	nop			;9c3a	00 	. 
	nop			;9c3b	00 	. 
	nop			;9c3c	00 	. 
	nop			;9c3d	00 	. 
	nop			;9c3e	00 	. 
	nop			;9c3f	00 	. 
	nop			;9c40	00 	. 
	ld bc,00101h		;9c41	01 01 01 	. . . 
	ld bc,00300h		;9c44	01 00 03 	. . . 
	rrca			;9c47	0f 	. 
	nop			;9c48	00 	. 
	nop			;9c49	00 	. 
	nop			;9c4a	00 	. 
l9c4bh:
	nop			;9c4b	00 	. 
	add a,b			;9c4c	80 	. 
	add a,b			;9c4d	80 	. 
	add a,b			;9c4e	80 	. 
l9c4fh:
	add a,b			;9c4f	80 	. 
	nop			;9c50	00 	. 
	nop			;9c51	00 	. 
	nop			;9c52	00 	. 
l9c53h:
	nop			;9c53	00 	. 
	nop			;9c54	00 	. 
	nop			;9c55	00 	. 
	nop			;9c56	00 	. 
	nop			;9c57	00 	. 
	nop			;9c58	00 	. 
	nop			;9c59	00 	. 
	nop			;9c5a	00 	. 
	nop			;9c5b	00 	. 
	nop			;9c5c	00 	. 
	nop			;9c5d	00 	. 
	nop			;9c5e	00 	. 
	nop			;9c5f	00 	. 
	nop			;9c60	00 	. 
	nop			;9c61	00 	. 
	nop			;9c62	00 	. 
	nop			;9c63	00 	. 
	nop			;9c64	00 	. 
	nop			;9c65	00 	. 
	nop			;9c66	00 	. 
	nop			;9c67	00 	. 
	nop			;9c68	00 	. 
	nop			;9c69	00 	. 
	nop			;9c6a	00 	. 
	nop			;9c6b	00 	. 
	nop			;9c6c	00 	. 
	nop			;9c6d	00 	. 
	nop			;9c6e	00 	. 
	nop			;9c6f	00 	. 
	dec l			;9c70	2d 	- 
	dec hl			;9c71	2b 	+ 
	ld h,020h		;9c72	26 20 	&   
	ccf			;9c74	3f 	? 
	nop			;9c75	00 	. 
	nop			;9c76	00 	. 
	nop			;9c77	00 	. 
	ld h,h			;9c78	64 	d 
	call nz,00286h		;9c79	c4 86 02 	. . . 
	add a,d			;9c7c	82 	. 
	add a,e			;9c7d	83 	. 
	add a,c			;9c7e	81 	. 
	pop bc			;9c7f	c1 	. 
	nop			;9c80	00 	. 
	nop			;9c81	00 	. 
	inc bc			;9c82	03 	. 
	rlca			;9c83	07 	. 
	rra			;9c84	1f 	. 
	ccf			;9c85	3f 	? 
	rst 38h			;9c86	ff 	. 
	rst 38h			;9c87	ff 	. 
	ld a,a			;9c88	7f 	 
	rst 38h			;9c89	ff 	. 
	rst 38h			;9c8a	ff 	. 
	rst 38h			;9c8b	ff 	. 
	rst 38h			;9c8c	ff 	. 
	rst 38h			;9c8d	ff 	. 
	rst 38h			;9c8e	ff 	. 
	rst 38h			;9c8f	ff 	. 
	ret nz			;9c90	c0 	. 
	ret nz			;9c91	c0 	. 
	ret po			;9c92	e0 	. 
	ret po			;9c93	e0 	. 
	ret p			;9c94	f0 	. 
	ret m			;9c95	f8 	. 
	ret m			;9c96	f8 	. 
	call m,00000h		;9c97	fc 00 00 	. . . 
	nop			;9c9a	00 	. 
	nop			;9c9b	00 	. 
	nop			;9c9c	00 	. 
	nop			;9c9d	00 	. 
	nop			;9c9e	00 	. 
	nop			;9c9f	00 	. 
	nop			;9ca0	00 	. 
	nop			;9ca1	00 	. 
	nop			;9ca2	00 	. 
	nop			;9ca3	00 	. 
	nop			;9ca4	00 	. 
	nop			;9ca5	00 	. 
	nop			;9ca6	00 	. 
	nop			;9ca7	00 	. 
	nop			;9ca8	00 	. 
	nop			;9ca9	00 	. 
	nop			;9caa	00 	. 
	nop			;9cab	00 	. 
	nop			;9cac	00 	. 
	nop			;9cad	00 	. 
	nop			;9cae	00 	. 
	nop			;9caf	00 	. 
	nop			;9cb0	00 	. 
	nop			;9cb1	00 	. 
	nop			;9cb2	00 	. 
	nop			;9cb3	00 	. 
	nop			;9cb4	00 	. 
	nop			;9cb5	00 	. 
	nop			;9cb6	00 	. 
	nop			;9cb7	00 	. 
	nop			;9cb8	00 	. 
	nop			;9cb9	00 	. 
	nop			;9cba	00 	. 
	nop			;9cbb	00 	. 
	nop			;9cbc	00 	. 
	nop			;9cbd	00 	. 
	nop			;9cbe	00 	. 
	nop			;9cbf	00 	. 
	ld b,c			;9cc0	41 	A 
	ld b,b			;9cc1	40 	@ 
	ld h,b			;9cc2	60 	` 
	jr nz,$+50		;9cc3	20 30 	  0 
	ld (de),a			;9cc5	12 	. 
	add hl,de			;9cc6	19 	. 
	ld a,(bc)			;9cc7	0a 	. 
	rst 38h			;9cc8	ff 	. 
	rst 38h			;9cc9	ff 	. 
	rst 38h			;9cca	ff 	. 
	ld a,a			;9ccb	7f 	 
	ld a,a			;9ccc	7f 	 
	ccf			;9ccd	3f 	? 
	ccf			;9cce	3f 	? 
	sbc a,a			;9ccf	9f 	. 
	rst 38h			;9cd0	ff 	. 
	rst 38h			;9cd1	ff 	. 
	rst 38h			;9cd2	ff 	. 
	rst 38h			;9cd3	ff 	. 
	rst 38h			;9cd4	ff 	. 
	rst 38h			;9cd5	ff 	. 
	rst 38h			;9cd6	ff 	. 
	rst 38h			;9cd7	ff 	. 
	cp 0ffh		;9cd8	fe ff 	. . 
	rst 38h			;9cda	ff 	. 
	rst 38h			;9cdb	ff 	. 
	rst 38h			;9cdc	ff 	. 
	rst 38h			;9cdd	ff 	. 
	rst 38h			;9cde	ff 	. 
	rst 38h			;9cdf	ff 	. 
	nop			;9ce0	00 	. 
	nop			;9ce1	00 	. 
	ret nz			;9ce2	c0 	. 
	ret p			;9ce3	f0 	. 
	call m,0ffffh		;9ce4	fc ff ff 	. . . 
	rst 38h			;9ce7	ff 	. 
	nop			;9ce8	00 	. 
	nop			;9ce9	00 	. 
	nop			;9cea	00 	. 
	nop			;9ceb	00 	. 
	nop			;9cec	00 	. 
	ret nz			;9ced	c0 	. 
	rst 38h			;9cee	ff 	. 
	rst 38h			;9cef	ff 	. 
	nop			;9cf0	00 	. 
	nop			;9cf1	00 	. 
	nop			;9cf2	00 	. 
	ld bc,07f07h		;9cf3	01 07 7f 	. .  
	rst 38h			;9cf6	ff 	. 
	rst 38h			;9cf7	ff 	. 
	nop			;9cf8	00 	. 
	nop			;9cf9	00 	. 
	nop			;9cfa	00 	. 
	nop			;9cfb	00 	. 
	nop			;9cfc	00 	. 
	nop			;9cfd	00 	. 
	nop			;9cfe	00 	. 
	nop			;9cff	00 	. 
	nop			;9d00	00 	. 
	nop			;9d01	00 	. 
	nop			;9d02	00 	. 
	nop			;9d03	00 	. 
	nop			;9d04	00 	. 
	nop			;9d05	00 	. 
	nop			;9d06	00 	. 
	nop			;9d07	00 	. 
	dec c			;9d08	0d 	. 
	ld b,002h		;9d09	06 02 	. . 
	inc bc			;9d0b	03 	. 
	ld bc,00000h		;9d0c	01 00 00 	. . . 
	nop			;9d0f	00 	. 
	ld c,a			;9d10	4f 	O 
	adc a,a			;9d11	8f 	. 
	ld d,a			;9d12	57 	W 
	dec hl			;9d13	2b 	+ 
	sub l			;9d14	95 	. 
	adc a,05bh		;9d15	ce 5b 	. [ 
	ld l,(hl)			;9d17	6e 	n 
	rst 38h			;9d18	ff 	. 
	rst 38h			;9d19	ff 	. 
	rst 38h			;9d1a	ff 	. 
	rst 38h			;9d1b	ff 	. 
	rst 38h			;9d1c	ff 	. 
	rst 38h			;9d1d	ff 	. 
	ld a,a			;9d1e	7f 	 
	cp a			;9d1f	bf 	. 
	rst 38h			;9d20	ff 	. 
	rst 38h			;9d21	ff 	. 
	rst 38h			;9d22	ff 	. 
	rst 38h			;9d23	ff 	. 
	rst 38h			;9d24	ff 	. 
	rst 38h			;9d25	ff 	. 
	rst 38h			;9d26	ff 	. 
	rst 38h			;9d27	ff 	. 
	rst 38h			;9d28	ff 	. 
	rst 38h			;9d29	ff 	. 
	rst 38h			;9d2a	ff 	. 
	rst 38h			;9d2b	ff 	. 
	rst 38h			;9d2c	ff 	. 
	rst 38h			;9d2d	ff 	. 
	rst 38h			;9d2e	ff 	. 
	rst 38h			;9d2f	ff 	. 
	rst 38h			;9d30	ff 	. 
	rst 38h			;9d31	ff 	. 
	cp 0fdh		;9d32	fe fd 	. . 
	jp m,0fafdh		;9d34	fa fd fa 	. . . 
	push af			;9d37	f5 	. 
	jp m,0aad5h		;9d38	fa d5 aa 	. . . 
	ld d,l			;9d3b	55 	U 
	xor d			;9d3c	aa 	. 
	ld d,l			;9d3d	55 	U 
	xor d			;9d3e	aa 	. 
	ld d,l			;9d3f	55 	U 
	nop			;9d40	00 	. 
	nop			;9d41	00 	. 
	nop			;9d42	00 	. 
	nop			;9d43	00 	. 
	nop			;9d44	00 	. 
	nop			;9d45	00 	. 
	nop			;9d46	00 	. 
	nop			;9d47	00 	. 
	nop			;9d48	00 	. 
	nop			;9d49	00 	. 
	nop			;9d4a	00 	. 
	nop			;9d4b	00 	. 
	nop			;9d4c	00 	. 
	nop			;9d4d	00 	. 
	nop			;9d4e	00 	. 
	nop			;9d4f	00 	. 
	nop			;9d50	00 	. 
	nop			;9d51	00 	. 
	nop			;9d52	00 	. 
	nop			;9d53	00 	. 
	nop			;9d54	00 	. 
	nop			;9d55	00 	. 
	nop			;9d56	00 	. 
	nop			;9d57	00 	. 
	scf			;9d58	37 	7 
	ld a,(de)			;9d59	1a 	. 
	dec c			;9d5a	0d 	. 
	ld b,003h		;9d5b	06 03 	. . 
	nop			;9d5d	00 	. 
	nop			;9d5e	00 	. 
	nop			;9d5f	00 	. 
	rst 18h			;9d60	df 	. 
	rst 28h			;9d61	ef 	. 
	di			;9d62	f3 	. 
	ld a,l			;9d63	7d 	} 
	cp (hl)			;9d64	be 	. 
	rst 18h			;9d65	df 	. 
	ld h,a			;9d66	67 	g 
	dec sp			;9d67	3b 	; 
	rst 38h			;9d68	ff 	. 
	rst 38h			;9d69	ff 	. 
	rst 38h			;9d6a	ff 	. 
	rst 38h			;9d6b	ff 	. 
	ld a,a			;9d6c	7f 	 
	sbc a,a			;9d6d	9f 	. 
	rst 20h			;9d6e	e7 	. 
	ret pe			;9d6f	e8 	. 
	rst 38h			;9d70	ff 	. 
	rst 38h			;9d71	ff 	. 
	rst 38h			;9d72	ff 	. 
	rst 38h			;9d73	ff 	. 
	rst 38h			;9d74	ff 	. 
	rst 38h			;9d75	ff 	. 
	rst 38h			;9d76	ff 	. 
	rst 38h			;9d77	ff 	. 
	jp m,0eaf5h		;9d78	fa f5 ea 	. . . 
	push af			;9d7b	f5 	. 
	jp pe,0eaf5h		;9d7c	ea f5 ea 	. . . 
	push de			;9d7f	d5 	. 
	xor d			;9d80	aa 	. 
	ld d,l			;9d81	55 	U 
	xor d			;9d82	aa 	. 
	ld d,l			;9d83	55 	U 
	xor d			;9d84	aa 	. 
	ld d,l			;9d85	55 	U 
	xor d			;9d86	aa 	. 
	ld d,l			;9d87	55 	U 
	nop			;9d88	00 	. 
	nop			;9d89	00 	. 
	nop			;9d8a	00 	. 
	nop			;9d8b	00 	. 
	nop			;9d8c	00 	. 
	nop			;9d8d	00 	. 
	nop			;9d8e	00 	. 
	nop			;9d8f	00 	. 
	nop			;9d90	00 	. 
	nop			;9d91	00 	. 
	nop			;9d92	00 	. 
	nop			;9d93	00 	. 
	nop			;9d94	00 	. 
	nop			;9d95	00 	. 
	nop			;9d96	00 	. 
	nop			;9d97	00 	. 
	nop			;9d98	00 	. 
	nop			;9d99	00 	. 
	nop			;9d9a	00 	. 
	nop			;9d9b	00 	. 
	nop			;9d9c	00 	. 
	nop			;9d9d	00 	. 
	nop			;9d9e	00 	. 
	nop			;9d9f	00 	. 
	nop			;9da0	00 	. 
	nop			;9da1	00 	. 
	nop			;9da2	00 	. 
	nop			;9da3	00 	. 
	nop			;9da4	00 	. 
	nop			;9da5	00 	. 
	nop			;9da6	00 	. 
	nop			;9da7	00 	. 
	inc c			;9da8	0c 	. 
	rlca			;9da9	07 	. 
	ld bc,00000h		;9daa	01 00 00 	. . . 
	nop			;9dad	00 	. 
	nop			;9dae	00 	. 
	nop			;9daf	00 	. 
	cp 02bh		;9db0	fe 2b 	. + 
	adc a,071h		;9db2	ce 71 	. q 
	ld e,003h		;9db4	1e 03 	. . 
	nop			;9db6	00 	. 
	nop			;9db7	00 	. 
	rra			;9db8	1f 	. 
	ld b,b			;9db9	40 	@ 
	xor d			;9dba	aa 	. 
	dec d			;9dbb	15 	. 
	nop			;9dbc	00 	. 
	ret nz			;9dbd	c0 	. 
	ld a,(hl)			;9dbe	7e 	~ 
	inc bc			;9dbf	03 	. 
	jp pe,000d5h		;9dc0	ea d5 00 	. . . 
	nop			;9dc3	00 	. 
	nop			;9dc4	00 	. 
	nop			;9dc5	00 	. 
	nop			;9dc6	00 	. 
	rst 38h			;9dc7	ff 	. 
	xor d			;9dc8	aa 	. 
	ld b,b			;9dc9	40 	@ 
	nop			;9dca	00 	. 
	nop			;9dcb	00 	. 
	nop			;9dcc	00 	. 
	nop			;9dcd	00 	. 
	rrca			;9dce	0f 	. 
	ret m			;9dcf	f8 	. 
	jr z,l9dd9h		;9dd0	28 07 	( . 
l9dd2h:
	nop			;9dd2	00 	. 
	nop			;9dd3	00 	. 
	nop			;9dd4	00 	. 
	nop			;9dd5	00 	. 
	nop			;9dd6	00 	. 
	nop			;9dd7	00 	. 
	nop			;9dd8	00 	. 
l9dd9h:
	nop			;9dd9	00 	. 
	nop			;9dda	00 	. 
	nop			;9ddb	00 	. 
	nop			;9ddc	00 	. 
	nop			;9ddd	00 	. 
	nop			;9dde	00 	. 
	nop			;9ddf	00 	. 
	nop			;9de0	00 	. 
	nop			;9de1	00 	. 
	nop			;9de2	00 	. 
	nop			;9de3	00 	. 
	nop			;9de4	00 	. 
	nop			;9de5	00 	. 
	nop			;9de6	00 	. 
	nop			;9de7	00 	. 
	nop			;9de8	00 	. 
	nop			;9de9	00 	. 
	nop			;9dea	00 	. 
	nop			;9deb	00 	. 
	nop			;9dec	00 	. 
	nop			;9ded	00 	. 
	nop			;9dee	00 	. 
	nop			;9def	00 	. 
	nop			;9df0	00 	. 
	nop			;9df1	00 	. 
	nop			;9df2	00 	. 
	nop			;9df3	00 	. 
	nop			;9df4	00 	. 
	nop			;9df5	00 	. 
	nop			;9df6	00 	. 
	nop			;9df7	00 	. 
	nop			;9df8	00 	. 
	nop			;9df9	00 	. 
	nop			;9dfa	00 	. 
	nop			;9dfb	00 	. 
	nop			;9dfc	00 	. 
	nop			;9dfd	00 	. 
	nop			;9dfe	00 	. 
	nop			;9dff	00 	. 
	nop			;9e00	00 	. 
	nop			;9e01	00 	. 
	nop			;9e02	00 	. 
	nop			;9e03	00 	. 
	nop			;9e04	00 	. 
	nop			;9e05	00 	. 
	nop			;9e06	00 	. 
	nop			;9e07	00 	. 
	nop			;9e08	00 	. 
	rrca			;9e09	0f 	. 
	nop			;9e0a	00 	. 
	nop			;9e0b	00 	. 
l9e0ch:
	nop			;9e0c	00 	. 
	nop			;9e0d	00 	. 
	nop			;9e0e	00 	. 
	nop			;9e0f	00 	. 
	rst 38h			;9e10	ff 	. 
	add a,b			;9e11	80 	. 
	nop			;9e12	00 	. 
	nop			;9e13	00 	. 
	nop			;9e14	00 	. 
	nop			;9e15	00 	. 
	nop			;9e16	00 	. 
	nop			;9e17	00 	. 
	ret po			;9e18	e0 	. 
	ld a,000h		;9e19	3e 00 	> . 
	nop			;9e1b	00 	. 
	nop			;9e1c	00 	. 
	nop			;9e1d	00 	. 
	nop			;9e1e	00 	. 
	nop			;9e1f	00 	. 
	nop			;9e20	00 	. 
	nop			;9e21	00 	. 
	nop			;9e22	00 	. 
	nop			;9e23	00 	. 
	inc bc			;9e24	03 	. 
	ld b,00ch		;9e25	06 0c 	. . 
	ex af,af'			;9e27	08 	. 
	nop			;9e28	00 	. 
	nop			;9e29	00 	. 
	jr c,l9e0ch		;9e2a	38 e0 	8 . 
	add a,b			;9e2c	80 	. 
	nop			;9e2d	00 	. 
	nop			;9e2e	00 	. 
	ld bc,0350ah		;9e2f	01 0a 35 	. . 5 
	ld a,(bc)			;9e32	0a 	. 
	dec d			;9e33	15 	. 
	ld a,(bc)			;9e34	0a 	. 
	nop			;9e35	00 	. 
	ld hl,(0aa55h)		;9e36	2a 55 aa 	* U . 
	ld d,l			;9e39	55 	U 
	add a,e			;9e3a	83 	. 
	ld a,h			;9e3b	7c 	| 
	cp e			;9e3c	bb 	. 
	ld a,a			;9e3d	7f 	 
	add a,(hl)			;9e3e	86 	. 
	ld b,c			;9e3f	41 	A 
	add a,b			;9e40	80 	. 
	nop			;9e41	00 	. 
	add a,b			;9e42	80 	. 
	ret po			;9e43	e0 	. 
	jr c,l9dd2h		;9e44	38 8c 	8 . 
	call p,078f8h		;9e46	f4 f8 78 	. . x 
l9e49h:
	jr c,l9e4bh		;9e49	38 00 	8 . 
l9e4bh:
	ld bc,00303h		;9e4b	01 03 03 	. . . 
	rlca			;9e4e	07 	. 
	rrca			;9e4f	0f 	. 
	rrca			;9e50	0f 	. 
	rra			;9e51	1f 	. 
	ld l,d			;9e52	6a 	j 
	push af			;9e53	f5 	. 
	jp m,0faf5h		;9e54	fa f5 fa 	. . . 
	push af			;9e57	f5 	. 
	jp m,0aafdh		;9e58	fa fd aa 	. . . 
	ld d,l			;9e5b	55 	U 
	xor d			;9e5c	aa 	. 
	ld d,l			;9e5d	55 	U 
	xor d			;9e5e	aa 	. 
	ld d,l			;9e5f	55 	U 
	xor d			;9e60	aa 	. 
	ld d,l			;9e61	55 	U 
	and b			;9e62	a0 	. 
	ld b,b			;9e63	40 	@ 
	xor b			;9e64	a8 	. 
	ld d,b			;9e65	50 	P 
	and b			;9e66	a0 	. 
	ld d,h			;9e67	54 	T 
	xor b			;9e68	a8 	. 
	ld d,l			;9e69	55 	U 
	ex af,af'			;9e6a	08 	. 
	nop			;9e6b	00 	. 
	nop			;9e6c	00 	. 
	nop			;9e6d	00 	. 
	nop			;9e6e	00 	. 
	nop			;9e6f	00 	. 
	nop			;9e70	00 	. 
	ex af,af'			;9e71	08 	. 
	rra			;9e72	1f 	. 
	ccf			;9e73	3f 	? 
	ccf			;9e74	3f 	? 
	ccf			;9e75	3f 	? 
	ccf			;9e76	3f 	? 
	ld a,a			;9e77	7f 	 
	ccf			;9e78	3f 	? 
	rrca			;9e79	0f 	. 
	cp 0fdh		;9e7a	fe fd 	. . 
	cp 0ffh		;9e7c	fe ff 	. . 
	cp 0ffh		;9e7e	fe ff 	. . 
	rst 38h			;9e80	ff 	. 
	rst 38h			;9e81	ff 	. 
	xor d			;9e82	aa 	. 
	ld d,l			;9e83	55 	U 
	xor d			;9e84	aa 	. 
	ld d,l			;9e85	55 	U 
	xor d			;9e86	aa 	. 
	push de			;9e87	d5 	. 
	jp m,0aaffh		;9e88	fa ff aa 	. . . 
	ld d,l			;9e8b	55 	U 
	xor d			;9e8c	aa 	. 
	ld d,l			;9e8d	55 	U 
	xor d			;9e8e	aa 	. 
	ld d,l			;9e8f	55 	U 
	xor d			;9e90	aa 	. 
	ret nc			;9e91	d0 	. 
	inc d			;9e92	14 	. 
	jr c,l9e49h		;9e93	38 b4 	8 . 
	ld a,(02a9dh)		;9e95	3a 9d 2a 	: . * 
	rra			;9e98	1f 	. 
	ld c,000h		;9e99	0e 00 	. . 
	djnz l9ec5h		;9e9b	10 28 	. ( 
	djnz l9ea7h		;9e9d	10 08 	. . 
	inc b			;9e9f	04 	. 
	ld a,(bc)			;9ea0	0a 	. 
	ld bc,00000h		;9ea1	01 00 00 	. . . 
	nop			;9ea4	00 	. 
	nop			;9ea5	00 	. 
	nop			;9ea6	00 	. 
l9ea7h:
	nop			;9ea7	00 	. 
	nop			;9ea8	00 	. 
	nop			;9ea9	00 	. 
	nop			;9eaa	00 	. 
	nop			;9eab	00 	. 
	nop			;9eac	00 	. 
	nop			;9ead	00 	. 
	nop			;9eae	00 	. 
	nop			;9eaf	00 	. 
	nop			;9eb0	00 	. 
	nop			;9eb1	00 	. 
	nop			;9eb2	00 	. 
	nop			;9eb3	00 	. 
	nop			;9eb4	00 	. 
	nop			;9eb5	00 	. 
	nop			;9eb6	00 	. 
	nop			;9eb7	00 	. 
	nop			;9eb8	00 	. 
	nop			;9eb9	00 	. 
	nop			;9eba	00 	. 
	ld b,b			;9ebb	40 	@ 
	nop			;9ebc	00 	. 
	ld b,b			;9ebd	40 	@ 
	ld l,b			;9ebe	68 	h 
	ld d,h			;9ebf	54 	T 
	ld a,d			;9ec0	7a 	z 
	ld e,l			;9ec1	5d 	] 
	ld a,(bc)			;9ec2	0a 	. 
	inc b			;9ec3	04 	. 
	ld (bc),a			;9ec4	02 	. 
l9ec5h:
	ld bc,00102h		;9ec5	01 02 01 	. . . 
	nop			;9ec8	00 	. 
	nop			;9ec9	00 	. 
	add a,b			;9eca	80 	. 
	ld b,b			;9ecb	40 	@ 
	jr nz,l9f1eh		;9ecc	20 50 	  P 
	xor d			;9ece	aa 	. 
	ld d,l			;9ecf	55 	U 
	xor b			;9ed0	a8 	. 
	ld d,l			;9ed1	55 	U 
	nop			;9ed2	00 	. 
	nop			;9ed3	00 	. 
	nop			;9ed4	00 	. 
	nop			;9ed5	00 	. 
	add a,b			;9ed6	80 	. 
	ld d,b			;9ed7	50 	P 
	xor d			;9ed8	aa 	. 
	ld b,l			;9ed9	45 	E 
	nop			;9eda	00 	. 
	nop			;9edb	00 	. 
	nop			;9edc	00 	. 
	nop			;9edd	00 	. 
	nop			;9ede	00 	. 
	nop			;9edf	00 	. 
	nop			;9ee0	00 	. 
	ld d,l			;9ee1	55 	U 
	ld (hl),a			;9ee2	77 	w 
	ld a,a			;9ee3	7f 	 
	ld e,(hl)			;9ee4	5e 	^ 
	halt			;9ee5	76 	v 
	ld a,h			;9ee6	7c 	| 
	ld a,b			;9ee7	78 	x 
	ld a,b			;9ee8	78 	x 
	ld (hl),b			;9ee9	70 	p 
	nop			;9eea	00 	. 
	nop			;9eeb	00 	. 
	nop			;9eec	00 	. 
	nop			;9eed	00 	. 
	nop			;9eee	00 	. 
	nop			;9eef	00 	. 
	nop			;9ef0	00 	. 
	nop			;9ef1	00 	. 
	ld a,(bc)			;9ef2	0a 	. 
	dec b			;9ef3	05 	. 
	nop			;9ef4	00 	. 
	nop			;9ef5	00 	. 
	nop			;9ef6	00 	. 
	nop			;9ef7	00 	. 
	nop			;9ef8	00 	. 
	nop			;9ef9	00 	. 
	xor d			;9efa	aa 	. 
	ld d,l			;9efb	55 	U 
	and d			;9efc	a2 	. 
	dec d			;9efd	15 	. 
	nop			;9efe	00 	. 
	nop			;9eff	00 	. 
	nop			;9f00	00 	. 
	nop			;9f01	00 	. 
	ld hl,(0a854h)		;9f02	2a 54 a8 	* T . 
	nop			;9f05	00 	. 
	nop			;9f06	00 	. 
	nop			;9f07	00 	. 
	nop			;9f08	00 	. 
	nop			;9f09	00 	. 
	ld b,b			;9f0a	40 	@ 
	nop			;9f0b	00 	. 
	nop			;9f0c	00 	. 
	nop			;9f0d	00 	. 
	nop			;9f0e	00 	. 
	nop			;9f0f	00 	. 
	nop			;9f10	00 	. 
	nop			;9f11	00 	. 
	jr nc,l9f1eh		;9f12	30 0a 	0 . 
	nop			;9f14	00 	. 
	nop			;9f15	00 	. 
	nop			;9f16	00 	. 
	nop			;9f17	00 	. 
	nop			;9f18	00 	. 
	nop			;9f19	00 	. 
	nop			;9f1a	00 	. 
	nop			;9f1b	00 	. 
	nop			;9f1c	00 	. 
	nop			;9f1d	00 	. 
l9f1eh:
	nop			;9f1e	00 	. 
	nop			;9f1f	00 	. 
	nop			;9f20	00 	. 
	nop			;9f21	00 	. 
	nop			;9f22	00 	. 
	rrca			;9f23	0f 	. 
	nop			;9f24	00 	. 
	nop			;9f25	00 	. 
	nop			;9f26	00 	. 
	nop			;9f27	00 	. 
	nop			;9f28	00 	. 
	nop			;9f29	00 	. 
	rst 38h			;9f2a	ff 	. 
	add a,b			;9f2b	80 	. 
	nop			;9f2c	00 	. 
	nop			;9f2d	00 	. 
l9f2eh:
	nop			;9f2e	00 	. 
	nop			;9f2f	00 	. 
	nop			;9f30	00 	. 
	nop			;9f31	00 	. 
	ret po			;9f32	e0 	. 
	ld a,000h		;9f33	3e 00 	> . 
	nop			;9f35	00 	. 
	nop			;9f36	00 	. 
	nop			;9f37	00 	. 
	nop			;9f38	00 	. 
	nop			;9f39	00 	. 
	nop			;9f3a	00 	. 
	nop			;9f3b	00 	. 
	nop			;9f3c	00 	. 
	nop			;9f3d	00 	. 
	nop			;9f3e	00 	. 
	nop			;9f3f	00 	. 
	nop			;9f40	00 	. 
	nop			;9f41	00 	. 
	nop			;9f42	00 	. 
	nop			;9f43	00 	. 
	nop			;9f44	00 	. 
	nop			;9f45	00 	. 
	inc bc			;9f46	03 	. 
	ld b,004h		;9f47	06 04 	. . 
	nop			;9f49	00 	. 
	nop			;9f4a	00 	. 
	nop			;9f4b	00 	. 
	jr c,l9f2eh		;9f4c	38 e0 	8 . 
	add a,b			;9f4e	80 	. 
	nop			;9f4f	00 	. 
	nop			;9f50	00 	. 
	ld (bc),a			;9f51	02 	. 
	rrca			;9f52	0f 	. 
	ccf			;9f53	3f 	? 
	ld (bc),a			;9f54	02 	. 
	dec d			;9f55	15 	. 
	ld a,(bc)			;9f56	0a 	. 
	nop			;9f57	00 	. 
	dec d			;9f58	15 	. 
	xor d			;9f59	aa 	. 
	ld d,l			;9f5a	55 	U 
	xor d			;9f5b	aa 	. 
	add a,e			;9f5c	83 	. 
	ld a,h			;9f5d	7c 	| 
	xor l			;9f5e	ad 	. 
	ccf			;9f5f	3f 	? 
	ld b,(hl)			;9f60	46 	F 
	ld bc,00000h		;9f61	01 00 00 	. . . 
	add a,b			;9f64	80 	. 
	ret po			;9f65	e0 	. 
	jr c,$-50		;9f66	38 cc 	8 . 
	call p,078f8h		;9f68	f4 f8 78 	. . x 
	jr c,l9f6dh		;9f6b	38 00 	8 . 
l9f6dh:
	nop			;9f6d	00 	. 
	nop			;9f6e	00 	. 
	nop			;9f6f	00 	. 
	nop			;9f70	00 	. 
	nop			;9f71	00 	. 
	nop			;9f72	00 	. 
	nop			;9f73	00 	. 
	nop			;9f74	00 	. 
	ld bc,00303h		;9f75	01 03 03 	. . . 
	ld bc,00000h		;9f78	01 00 00 	. . . 
	ex af,af'			;9f7b	08 	. 
	ld a,a			;9f7c	7f 	 
	rst 38h			;9f7d	ff 	. 
	rst 38h			;9f7e	ff 	. 
	rst 38h			;9f7f	ff 	. 
	rst 38h			;9f80	ff 	. 
	rst 38h			;9f81	ff 	. 
	rst 38h			;9f82	ff 	. 
	ld a,a			;9f83	7f 	 
	push de			;9f84	d5 	. 
	jp pe,0ead5h		;9f85	ea d5 ea 	. . . 
	push de			;9f88	d5 	. 
	xor d			;9f89	aa 	. 
	push de			;9f8a	d5 	. 
	xor d			;9f8b	aa 	. 
	nop			;9f8c	00 	. 
	nop			;9f8d	00 	. 
	nop			;9f8e	00 	. 
	nop			;9f8f	00 	. 
	ld b,b			;9f90	40 	@ 
	add a,b			;9f91	80 	. 
	ld b,b			;9f92	40 	@ 
	add a,b			;9f93	80 	. 
	ex af,af'			;9f94	08 	. 
	nop			;9f95	00 	. 
	nop			;9f96	00 	. 
	nop			;9f97	00 	. 
	nop			;9f98	00 	. 
	ex af,af'			;9f99	08 	. 
	inc b			;9f9a	04 	. 
	ld a,(bc)			;9f9b	0a 	. 
	nop			;9f9c	00 	. 
	nop			;9f9d	00 	. 
	nop			;9f9e	00 	. 
	nop			;9f9f	00 	. 
	nop			;9fa0	00 	. 
	nop			;9fa1	00 	. 
	nop			;9fa2	00 	. 
	nop			;9fa3	00 	. 
	inc d			;9fa4	14 	. 
	jr c,l9fdbh		;9fa5	38 34 	8 4 
	ld a,(02a1dh)		;9fa7	3a 1d 2a 	: . * 
	rra			;9faa	1f 	. 
	ld c,07fh		;9fab	0e 7f 	.  
	ccf			;9fad	3f 	? 
	ccf			;9fae	3f 	? 
	ccf			;9faf	3f 	? 
	ccf			;9fb0	3f 	? 
	rra			;9fb1	1f 	. 
	rrca			;9fb2	0f 	. 
	inc bc			;9fb3	03 	. 
	push de			;9fb4	d5 	. 
	jp pe,0ead5h		;9fb5	ea d5 ea 	. . . 
	push af			;9fb8	f5 	. 
	jp m,0fefdh		;9fb9	fa fd fe 	. . . 
	ld d,b			;9fbc	50 	P 
	and b			;9fbd	a0 	. 
	ld d,b			;9fbe	50 	P 
	xor b			;9fbf	a8 	. 
	ld d,c			;9fc0	51 	Q 
	xor b			;9fc1	a8 	. 
	ld d,h			;9fc2	54 	T 
	xor b			;9fc3	a8 	. 
	inc d			;9fc4	14 	. 
	ld hl,(03e15h)		;9fc5	2a 15 3e 	* . > 
	ccf			;9fc8	3f 	? 
	ld a,a			;9fc9	7f 	 
	ccf			;9fca	3f 	? 
	ld c,000h		;9fcb	0e 00 	. . 
	nop			;9fcd	00 	. 
	nop			;9fce	00 	. 
	nop			;9fcf	00 	. 
	nop			;9fd0	00 	. 
	nop			;9fd1	00 	. 
	nop			;9fd2	00 	. 
	nop			;9fd3	00 	. 
	nop			;9fd4	00 	. 
	ld b,b			;9fd5	40 	@ 
	nop			;9fd6	00 	. 
	ld b,b			;9fd7	40 	@ 
	ld l,b			;9fd8	68 	h 
	ld d,h			;9fd9	54 	T 
	ld a,d			;9fda	7a 	z 
l9fdbh:
	ld e,l			;9fdb	5d 	] 
	nop			;9fdc	00 	. 
	ld d,b			;9fdd	50 	P 
	ld d,b			;9fde	50 	P 
	ld d,b			;9fdf	50 	P 
	and b			;9fe0	a0 	. 
	and b			;9fe1	a0 	. 
	and b			;9fe2	a0 	. 
	xor d			;9fe3	aa 	. 
	nop			;9fe4	00 	. 
	nop			;9fe5	00 	. 
	nop			;9fe6	00 	. 
	nop			;9fe7	00 	. 
	nop			;9fe8	00 	. 
	nop			;9fe9	00 	. 
	nop			;9fea	00 	. 
	and b			;9feb	a0 	. 
	nop			;9fec	00 	. 
	ld bc,00101h		;9fed	01 01 01 	. . . 
	nop			;9ff0	00 	. 
	nop			;9ff1	00 	. 
	nop			;9ff2	00 	. 
	nop			;9ff3	00 	. 
	nop			;9ff4	00 	. 
	nop			;9ff5	00 	. 
	nop			;9ff6	00 	. 
	inc b			;9ff7	04 	. 
	add a,d			;9ff8	82 	. 
	sub l			;9ff9	95 	. 
	xor d			;9ffa	aa 	. 
	sub l			;9ffb	95 	. 
	nop			;9ffc	00 	. 
	nop			;9ffd	00 	. 
	nop			;9ffe	00 	. 
	nop			;9fff	00 	. 
	nop			;a000	00 	. 
	nop			;a001	00 	. 
	nop			;a002	00 	. 
	nop			;a003	00 	. 
	ld (hl),a			;a004	77 	w 
	ld a,a			;a005	7f 	 
	ld e,(hl)			;a006	5e 	^ 
	halt			;a007	76 	v 
	ld a,l			;a008	7d 	} 
	ld a,d			;a009	7a 	z 
	ld a,c			;a00a	79 	y 
	ld (hl),d			;a00b	72 	r 
	dec d			;a00c	15 	. 
	ld l,d			;a00d	6a 	j 
	sub l			;a00e	95 	. 
	ld hl,(0aa15h)		;a00f	2a 15 aa 	* . . 
	ld d,l			;a012	55 	U 
	xor d			;a013	aa 	. 
	ld d,c			;a014	51 	Q 
	xor d			;a015	aa 	. 
	ld d,l			;a016	55 	U 
	xor d			;a017	aa 	. 
	ld d,l			;a018	55 	U 
	xor d			;a019	aa 	. 
	ld d,l			;a01a	55 	U 
	xor d			;a01b	aa 	. 
	ld b,b			;a01c	40 	@ 
	xor d			;a01d	aa 	. 
	ld d,l			;a01e	55 	U 
	xor d			;a01f	aa 	. 
	ld d,l			;a020	55 	U 
	xor d			;a021	aa 	. 
	ld d,l			;a022	55 	U 
	xor d			;a023	aa 	. 
	ld c,d			;a024	4a 	J 
	push de			;a025	d5 	. 
	ld l,d			;a026	6a 	j 
	and h			;a027	a4 	. 
	ld (hl),h			;a028	74 	t 
	cp b			;a029	b8 	. 
	ld a,b			;a02a	78 	x 
	ret p			;a02b	f0 	. 
	nop			;a02c	00 	. 
	nop			;a02d	00 	. 
	nop			;a02e	00 	. 
	nop			;a02f	00 	. 
	nop			;a030	00 	. 
	nop			;a031	00 	. 
	nop			;a032	00 	. 
	nop			;a033	00 	. 
	ld b,l			;a034	45 	E 
	ld hl,(06a55h)		;a035	2a 55 6a 	* U j 
	ld (hl),l			;a038	75 	u 
	ld l,d			;a039	6a 	j 
	ld (hl),l			;a03a	75 	u 
	ld a,d			;a03b	7a 	z 
la03ch:
	ld d,l			;a03c	55 	U 
	xor d			;a03d	aa 	. 
	ld d,l			;a03e	55 	U 
	xor d			;a03f	aa 	. 
	ld d,a			;a040	57 	W 
	xor a			;a041	af 	. 
	ld e,a			;a042	5f 	_ 
	cp a			;a043	bf 	. 
	ld d,l			;a044	55 	U 
	xor d			;a045	aa 	. 
	ld e,a			;a046	5f 	_ 
	cp a			;a047	bf 	. 
	rst 38h			;a048	ff 	. 
	ret m			;a049	f8 	. 
	ret m			;a04a	f8 	. 
	jp m,0ff55h		;a04b	fa 55 ff 	. U . 
	cp 0f9h		;a04e	fe f9 	. . 
	jp nz,00205h		;a050	c2 05 02 	. . . 
	nop			;a053	00 	. 
	ret nz			;a054	c0 	. 
	cp b			;a055	b8 	. 
	ld l,b			;a056	68 	h 
	cp b			;a057	b8 	. 
	ret po			;a058	e0 	. 
	ld c,b			;a059	48 	H 
	jr c,la03ch		;a05a	38 e0 	8 . 
	nop			;a05c	00 	. 
	nop			;a05d	00 	. 
	nop			;a05e	00 	. 
	nop			;a05f	00 	. 
	nop			;a060	00 	. 
	nop			;a061	00 	. 
	nop			;a062	00 	. 
	nop			;a063	00 	. 
	ccf			;a064	3f 	? 
	ld d,a			;a065	57 	W 
	dec hl			;a066	2b 	+ 
	ld d,l			;a067	55 	U 
	dec hl			;a068	2b 	+ 
	ld d,l			;a069	55 	U 
	ld hl,(0ff55h)		;a06a	2a 55 ff 	* U . 
	rst 38h			;a06d	ff 	. 
	rst 38h			;a06e	ff 	. 
	rst 38h			;a06f	ff 	. 
	rst 38h			;a070	ff 	. 
	rst 38h			;a071	ff 	. 
	rst 38h			;a072	ff 	. 
	rst 38h			;a073	ff 	. 
	ei			;a074	fb 	. 
	ei			;a075	fb 	. 
	ei			;a076	fb 	. 
	ei			;a077	fb 	. 
	pop af			;a078	f1 	. 
	ret po			;a079	e0 	. 
	ret nz			;a07a	c0 	. 
	add a,b			;a07b	80 	. 
	inc bc			;a07c	03 	. 
	sbc a,(hl)			;a07d	9e 	. 
	or b			;a07e	b0 	. 
	and b			;a07f	a0 	. 
	and b			;a080	a0 	. 
	jr nz,la0e3h		;a081	20 60 	  ` 
	ret nz			;a083	c0 	. 
	add a,b			;a084	80 	. 
	nop			;a085	00 	. 
	nop			;a086	00 	. 
	nop			;a087	00 	. 
	nop			;a088	00 	. 
	nop			;a089	00 	. 
	nop			;a08a	00 	. 
	nop			;a08b	00 	. 
	nop			;a08c	00 	. 
	nop			;a08d	00 	. 
	nop			;a08e	00 	. 
	nop			;a08f	00 	. 
	nop			;a090	00 	. 
	nop			;a091	00 	. 
	nop			;a092	00 	. 
	nop			;a093	00 	. 
	ld hl,(02a55h)		;a094	2a 55 2a 	* U * 
	ld d,l			;a097	55 	U 
	ld hl,(02855h)		;a098	2a 55 28 	* U ( 
	ld b,e			;a09b	43 	C 
	rst 38h			;a09c	ff 	. 
	ld a,(hl)			;a09d	7e 	~ 
	ret m			;a09e	f8 	. 
	ld (hl),l			;a09f	75 	u 
	adc a,d			;a0a0	8a 	. 
	dec (hl)			;a0a1	35 	5 
	ret pe			;a0a2	e8 	. 
	or e			;a0a3	b3 	. 
	ld bc,0a643h		;a0a4	01 43 a6 	. C . 
	ld c,h			;a0a7	4c 	L 
	cp b			;a0a8	b8 	. 
	ld h,b			;a0a9	60 	` 
	ret nz			;a0aa	c0 	. 
	add a,b			;a0ab	80 	. 
	add a,b			;a0ac	80 	. 
	nop			;a0ad	00 	. 
	nop			;a0ae	00 	. 
	nop			;a0af	00 	. 
	nop			;a0b0	00 	. 
	nop			;a0b1	00 	. 
	nop			;a0b2	00 	. 
	nop			;a0b3	00 	. 
	nop			;a0b4	00 	. 
	nop			;a0b5	00 	. 
	nop			;a0b6	00 	. 
	nop			;a0b7	00 	. 
	nop			;a0b8	00 	. 
	nop			;a0b9	00 	. 
	nop			;a0ba	00 	. 
	nop			;a0bb	00 	. 
	nop			;a0bc	00 	. 
	nop			;a0bd	00 	. 
	nop			;a0be	00 	. 
	nop			;a0bf	00 	. 
	nop			;a0c0	00 	. 
	nop			;a0c1	00 	. 
	nop			;a0c2	00 	. 
	nop			;a0c3	00 	. 
	rra			;a0c4	1f 	. 
	ld a,a			;a0c5	7f 	 
	ld a,(hl)			;a0c6	7e 	~ 
	ld (hl),c			;a0c7	71 	q 
	rrca			;a0c8	0f 	. 
	ld a,b			;a0c9	78 	x 
	ret nz			;a0ca	c0 	. 
	nop			;a0cb	00 	. 
	and 09ch		;a0cc	e6 9c 	. . 
	ld (hl),b			;a0ce	70 	p 
	ret nz			;a0cf	c0 	. 
	nop			;a0d0	00 	. 
	nop			;a0d1	00 	. 
	nop			;a0d2	00 	. 
	nop			;a0d3	00 	. 
	nop			;a0d4	00 	. 
	nop			;a0d5	00 	. 
	nop			;a0d6	00 	. 
	nop			;a0d7	00 	. 
	nop			;a0d8	00 	. 
	nop			;a0d9	00 	. 
	nop			;a0da	00 	. 
	nop			;a0db	00 	. 
	nop			;a0dc	00 	. 
	nop			;a0dd	00 	. 
	nop			;a0de	00 	. 
	nop			;a0df	00 	. 
	nop			;a0e0	00 	. 
	nop			;a0e1	00 	. 
	nop			;a0e2	00 	. 
la0e3h:
	nop			;a0e3	00 	. 
	nop			;a0e4	00 	. 
	nop			;a0e5	00 	. 
	nop			;a0e6	00 	. 
	nop			;a0e7	00 	. 
	nop			;a0e8	00 	. 
	nop			;a0e9	00 	. 
	nop			;a0ea	00 	. 
	nop			;a0eb	00 	. 
	nop			;a0ec	00 	. 
	nop			;a0ed	00 	. 
	nop			;a0ee	00 	. 
	nop			;a0ef	00 	. 
	nop			;a0f0	00 	. 
	nop			;a0f1	00 	. 
	nop			;a0f2	00 	. 
	nop			;a0f3	00 	. 
	jr c,la100h		;a0f4	38 0a 	8 . 
	nop			;a0f6	00 	. 
	nop			;a0f7	00 	. 
	nop			;a0f8	00 	. 
	nop			;a0f9	00 	. 
	nop			;a0fa	00 	. 
	nop			;a0fb	00 	. 
	nop			;a0fc	00 	. 
	nop			;a0fd	00 	. 
	nop			;a0fe	00 	. 
	nop			;a0ff	00 	. 
la100h:
	nop			;a100	00 	. 
	nop			;a101	00 	. 
	nop			;a102	00 	. 
	nop			;a103	00 	. 
	nop			;a104	00 	. 
	nop			;a105	00 	. 
	nop			;a106	00 	. 
	nop			;a107	00 	. 
	nop			;a108	00 	. 
	nop			;a109	00 	. 
	nop			;a10a	00 	. 
	nop			;a10b	00 	. 
	nop			;a10c	00 	. 
	nop			;a10d	00 	. 
	nop			;a10e	00 	. 
	nop			;a10f	00 	. 
	nop			;a110	00 	. 
	nop			;a111	00 	. 
	nop			;a112	00 	. 
	nop			;a113	00 	. 
	nop			;a114	00 	. 
	nop			;a115	00 	. 
	nop			;a116	00 	. 
	nop			;a117	00 	. 
	nop			;a118	00 	. 
	nop			;a119	00 	. 
	nop			;a11a	00 	. 
	nop			;a11b	00 	. 
	nop			;a11c	00 	. 
	nop			;a11d	00 	. 
	nop			;a11e	00 	. 
	nop			;a11f	00 	. 
	nop			;a120	00 	. 
	nop			;a121	00 	. 
	nop			;a122	00 	. 
	nop			;a123	00 	. 
	nop			;a124	00 	. 
	nop			;a125	00 	. 
	nop			;a126	00 	. 
	nop			;a127	00 	. 
	nop			;a128	00 	. 
	nop			;a129	00 	. 
	nop			;a12a	00 	. 
	nop			;a12b	00 	. 
	nop			;a12c	00 	. 
	nop			;a12d	00 	. 
	nop			;a12e	00 	. 
	nop			;a12f	00 	. 
	nop			;a130	00 	. 
	nop			;a131	00 	. 
	nop			;a132	00 	. 
	nop			;a133	00 	. 
	nop			;a134	00 	. 
	nop			;a135	00 	. 
	nop			;a136	00 	. 
	nop			;a137	00 	. 
	nop			;a138	00 	. 
	nop			;a139	00 	. 
	nop			;a13a	00 	. 
	nop			;a13b	00 	. 
	nop			;a13c	00 	. 
	nop			;a13d	00 	. 
	nop			;a13e	00 	. 
	nop			;a13f	00 	. 
	nop			;a140	00 	. 
	nop			;a141	00 	. 
	nop			;a142	00 	. 
	nop			;a143	00 	. 
	nop			;a144	00 	. 
	rrca			;a145	0f 	. 
	nop			;a146	00 	. 
	nop			;a147	00 	. 
	nop			;a148	00 	. 
	nop			;a149	00 	. 
	nop			;a14a	00 	. 
	nop			;a14b	00 	. 
	rst 38h			;a14c	ff 	. 
	add a,b			;a14d	80 	. 
	nop			;a14e	00 	. 
	nop			;a14f	00 	. 
	nop			;a150	00 	. 
	nop			;a151	00 	. 
	nop			;a152	00 	. 
	nop			;a153	00 	. 
	ret p			;a154	f0 	. 
	ld a,000h		;a155	3e 00 	> . 
	nop			;a157	00 	. 
la158h:
	nop			;a158	00 	. 
	nop			;a159	00 	. 
	nop			;a15a	00 	. 
	nop			;a15b	00 	. 
	nop			;a15c	00 	. 
	nop			;a15d	00 	. 
la15eh:
	nop			;a15e	00 	. 
	nop			;a15f	00 	. 
	nop			;a160	00 	. 
	nop			;a161	00 	. 
	nop			;a162	00 	. 
	nop			;a163	00 	. 
	nop			;a164	00 	. 
	nop			;a165	00 	. 
	nop			;a166	00 	. 
	nop			;a167	00 	. 
	nop			;a168	00 	. 
	nop			;a169	00 	. 
	nop			;a16a	00 	. 
	nop			;a16b	00 	. 
	nop			;a16c	00 	. 
	nop			;a16d	00 	. 
	nop			;a16e	00 	. 
	nop			;a16f	00 	. 
	inc bc			;a170	03 	. 
	ld b,004h		;a171	06 04 	. . 
	nop			;a173	00 	. 
	nop			;a174	00 	. 
	nop			;a175	00 	. 
	jr c,la158h		;a176	38 e0 	8 . 
	add a,b			;a178	80 	. 
	nop			;a179	00 	. 
	nop			;a17a	00 	. 
	inc bc			;a17b	03 	. 
	rrca			;a17c	0f 	. 
	ccf			;a17d	3f 	? 
	ld bc,0452ah		;a17e	01 2a 45 	. * E 
	nop			;a181	00 	. 
	jr z,la158h		;a182	28 d4 	( . 
	xor d			;a184	aa 	. 
	push de			;a185	d5 	. 
	ld b,e			;a186	43 	C 
	xor h			;a187	ac 	. 
	ld e,e			;a188	5b 	[ 
	ccf			;a189	3f 	? 
	ld b,001h		;a18a	06 01 	. . 
	nop			;a18c	00 	. 
	nop			;a18d	00 	. 
	add a,b			;a18e	80 	. 
	ret po			;a18f	e0 	. 
	jr c,la15eh		;a190	38 cc 	8 . 
	or (hl)			;a192	b6 	. 
	ei			;a193	fb 	. 
	ld a,l			;a194	7d 	} 
	ld a,000h		;a195	3e 00 	> . 
	nop			;a197	00 	. 
	nop			;a198	00 	. 
	nop			;a199	00 	. 
	nop			;a19a	00 	. 
	nop			;a19b	00 	. 
	add a,b			;a19c	80 	. 
	ret nz			;a19d	c0 	. 
	nop			;a19e	00 	. 
	nop			;a19f	00 	. 
	nop			;a1a0	00 	. 
	nop			;a1a1	00 	. 
	nop			;a1a2	00 	. 
	nop			;a1a3	00 	. 
	nop			;a1a4	00 	. 
	nop			;a1a5	00 	. 
	nop			;a1a6	00 	. 
	ld bc,00303h		;a1a7	01 03 03 	. . . 
	ld bc,00408h		;a1aa	01 08 04 	. . . 
	ld a,(bc)			;a1ad	0a 	. 
	ld a,a			;a1ae	7f 	 
	rst 38h			;a1af	ff 	. 
	rst 38h			;a1b0	ff 	. 
	rst 38h			;a1b1	ff 	. 
	rst 38h			;a1b2	ff 	. 
	rst 38h			;a1b3	ff 	. 
	rst 38h			;a1b4	ff 	. 
	ld a,a			;a1b5	7f 	 
	xor d			;a1b6	aa 	. 
	push de			;a1b7	d5 	. 
	xor d			;a1b8	aa 	. 
	push de			;a1b9	d5 	. 
	jp pe,0ead5h		;a1ba	ea d5 ea 	. . . 
	push af			;a1bd	f5 	. 
	nop			;a1be	00 	. 
	nop			;a1bf	00 	. 
	nop			;a1c0	00 	. 
	nop			;a1c1	00 	. 
	add a,b			;a1c2	80 	. 
la1c3h:
	nop			;a1c3	00 	. 
	add a,b			;a1c4	80 	. 
	ld b,b			;a1c5	40 	@ 
	rrca			;a1c6	0f 	. 
	rlca			;a1c7	07 	. 
	inc bc			;a1c8	03 	. 
	inc bc			;a1c9	03 	. 
	ld bc,00000h		;a1ca	01 00 00 	. . . 
	nop			;a1cd	00 	. 
	ld h,b			;a1ce	60 	` 
	or b			;a1cf	b0 	. 
	ret nc			;a1d0	d0 	. 
	ld e,b			;a1d1	58 	X 
	ret z			;a1d2	c8 	. 
	ld l,h			;a1d3	6c 	l 
	call p,00056h		;a1d4	f4 56 00 	. V . 
	nop			;a1d7	00 	. 
	nop			;a1d8	00 	. 
	nop			;a1d9	00 	. 
	nop			;a1da	00 	. 
	nop			;a1db	00 	. 
	nop			;a1dc	00 	. 
	nop			;a1dd	00 	. 
	inc d			;a1de	14 	. 
	ld hl,(03e15h)		;a1df	2a 15 3e 	* . > 
	ccf			;a1e2	3f 	? 
	ld a,a			;a1e3	7f 	 
	ccf			;a1e4	3f 	? 
	ld c,07fh		;a1e5	0e 7f 	.  
	ccf			;a1e7	3f 	? 
	ccf			;a1e8	3f 	? 
	ccf			;a1e9	3f 	? 
	ccf			;a1ea	3f 	? 
	rst 18h			;a1eb	df 	. 
	adc a,a			;a1ec	8f 	. 
	inc bc			;a1ed	03 	. 
	jp pe,0faf5h		;a1ee	ea f5 fa 	. . . 
	defb 0fdh,0ffh,0ffh	;illegal sequence		;a1f1	fd ff ff 	. . . 
	rst 38h			;a1f4	ff 	. 
	rst 38h			;a1f5	ff 	. 
	and b			;a1f6	a0 	. 
	ld d,b			;a1f7	50 	P 
	xor b			;a1f8	a8 	. 
	ld d,l			;a1f9	55 	U 
	xor d			;a1fa	aa 	. 
	push de			;a1fb	d5 	. 
	jp m,000ffh		;a1fc	fa ff 00 	. . . 
	ld bc,01103h		;a1ff	01 03 11 	. . . 
	xor h			;a202	ac 	. 
	ld d,h			;a203	54 	T 
	cp b			;a204	b8 	. 
	ret po			;a205	e0 	. 
	ld a,d			;a206	7a 	z 
	or d			;a207	b2 	. 
	xor d			;a208	aa 	. 
	sub e			;a209	93 	. 
	xor c			;a20a	a9 	. 
	sub l			;a20b	95 	. 
	ld c,c			;a20c	49 	I 
	ld b,c			;a20d	41 	A 
	nop			;a20e	00 	. 
	nop			;a20f	00 	. 
	nop			;a210	00 	. 
	nop			;a211	00 	. 
	nop			;a212	00 	. 
	nop			;a213	00 	. 
	nop			;a214	00 	. 
	nop			;a215	00 	. 
	nop			;a216	00 	. 
	nop			;a217	00 	. 
	nop			;a218	00 	. 
	inc b			;a219	04 	. 
	ld (bc),a			;a21a	02 	. 
	dec d			;a21b	15 	. 
	ld hl,(00015h)		;a21c	2a 15 00 	* . . 
	djnz la231h		;a21f	10 10 	. . 
	djnz la1c3h		;a221	10 a0 	. . 
	and b			;a223	a0 	. 
	and h			;a224	a4 	. 
	ld l,d			;a225	6a 	j 
	nop			;a226	00 	. 
	nop			;a227	00 	. 
	nop			;a228	00 	. 
	nop			;a229	00 	. 
	nop			;a22a	00 	. 
	nop			;a22b	00 	. 
	nop			;a22c	00 	. 
	add a,b			;a22d	80 	. 
	nop			;a22e	00 	. 
	nop			;a22f	00 	. 
	nop			;a230	00 	. 
la231h:
	nop			;a231	00 	. 
	nop			;a232	00 	. 
	nop			;a233	00 	. 
	nop			;a234	00 	. 
	nop			;a235	00 	. 
	nop			;a236	00 	. 
	nop			;a237	00 	. 
	nop			;a238	00 	. 
	nop			;a239	00 	. 
	nop			;a23a	00 	. 
	nop			;a23b	00 	. 
	nop			;a23c	00 	. 
	nop			;a23d	00 	. 
	ld b,c			;a23e	41 	A 
	ld b,c			;a23f	41 	A 
	ld b,c			;a240	41 	A 
	ld b,c			;a241	41 	A 
	and c			;a242	a1 	. 
	sub e			;a243	93 	. 
	xor d			;a244	aa 	. 
	sub d			;a245	92 	. 
	nop			;a246	00 	. 
	nop			;a247	00 	. 
	nop			;a248	00 	. 
	nop			;a249	00 	. 
	nop			;a24a	00 	. 
	nop			;a24b	00 	. 
	nop			;a24c	00 	. 
	nop			;a24d	00 	. 
	ld a,(bc)			;a24e	0a 	. 
	dec d			;a24f	15 	. 
	ld a,(bc)			;a250	0a 	. 
	inc b			;a251	04 	. 
	dec b			;a252	05 	. 
	inc bc			;a253	03 	. 
	inc bc			;a254	03 	. 
	ld bc,06a55h		;a255	01 55 6a 	. U j 
	push af			;a258	f5 	. 
	cp 0ffh		;a259	fe ff 	. . 
	rst 38h			;a25b	ff 	. 
	rst 38h			;a25c	ff 	. 
	rst 38h			;a25d	ff 	. 
	ld d,b			;a25e	50 	P 
	xor b			;a25f	a8 	. 
	ld d,l			;a260	55 	U 
	xor d			;a261	aa 	. 
	ld d,l			;a262	55 	U 
	jp pe,0faf5h		;a263	ea f5 fa 	. . . 
	nop			;a266	00 	. 
	nop			;a267	00 	. 
	nop			;a268	00 	. 
	add a,b			;a269	80 	. 
	ld b,b			;a26a	40 	@ 
	add a,b			;a26b	80 	. 
	ld b,b			;a26c	40 	@ 
	and b			;a26d	a0 	. 
	ld bc,00201h		;a26e	01 01 02 	. . . 
	ld (bc),a			;a271	02 	. 
	dec b			;a272	05 	. 
	dec bc			;a273	0b 	. 
	add hl,bc			;a274	09 	. 
	scf			;a275	37 	7 
	ld hl,(0a456h)		;a276	2a 56 a4 	* V . 
	call z,0d868h		;a279	cc 68 d8 	. h . 
	ret nc			;a27c	d0 	. 
	jr nc,la27fh		;a27d	30 00 	0 . 
la27fh:
	nop			;a27f	00 	. 
	nop			;a280	00 	. 
	nop			;a281	00 	. 
	nop			;a282	00 	. 
	nop			;a283	00 	. 
	nop			;a284	00 	. 
	nop			;a285	00 	. 
	nop			;a286	00 	. 
	nop			;a287	00 	. 
	nop			;a288	00 	. 
	nop			;a289	00 	. 
	inc b			;a28a	04 	. 
	ld b,003h		;a28b	06 03 	. . 
	nop			;a28d	00 	. 
la28eh:
	ld a,a			;a28e	7f 	 
	ccf			;a28f	3f 	? 
	rrca			;a290	0f 	. 
	inc bc			;a291	03 	. 
	nop			;a292	00 	. 
	dec b			;a293	05 	. 
	add a,d			;a294	82 	. 
	push hl			;a295	e5 	. 
	defb 0fdh,0fah,0fdh	;illegal sequence		;a296	fd fa fd 	. . . 
	rst 38h			;a299	ff 	. 
	ccf			;a29a	3f 	? 
	ld b,b			;a29b	40 	@ 
	xor l			;a29c	ad 	. 
	ld d,a			;a29d	57 	W 
	ld d,b			;a29e	50 	P 
	xor c			;a29f	a9 	. 
	ld e,(hl)			;a2a0	5e 	^ 
	ld sp,hl			;a2a1	f9 	. 
	add a,a			;a2a2	87 	. 
	ld e,a			;a2a3	5f 	_ 
	rst 30h			;a2a4	f7 	. 
	ld a,h			;a2a5	7c 	| 
	ld c,e			;a2a6	4b 	K 
	cp (hl)			;a2a7	be 	. 
	ld a,l			;a2a8	7d 	} 
	ei			;a2a9	fb 	. 
	or 0cch		;a2aa	f6 cc 	. . 
	jr c,la28eh		;a2ac	38 e0 	8 . 
	ld h,b			;a2ae	60 	` 
	ret nz			;a2af	c0 	. 
	add a,b			;a2b0	80 	. 
	nop			;a2b1	00 	. 
	nop			;a2b2	00 	. 
	nop			;a2b3	00 	. 
	nop			;a2b4	00 	. 
	nop			;a2b5	00 	. 
	nop			;a2b6	00 	. 
	nop			;a2b7	00 	. 
	nop			;a2b8	00 	. 
	nop			;a2b9	00 	. 
	nop			;a2ba	00 	. 
	nop			;a2bb	00 	. 
	nop			;a2bc	00 	. 
	nop			;a2bd	00 	. 
	nop			;a2be	00 	. 
	nop			;a2bf	00 	. 
	nop			;a2c0	00 	. 
	nop			;a2c1	00 	. 
	nop			;a2c2	00 	. 
	nop			;a2c3	00 	. 
	nop			;a2c4	00 	. 
	nop			;a2c5	00 	. 
	jr c,$+17		;a2c6	38 0f 	8 . 
	nop			;a2c8	00 	. 
	nop			;a2c9	00 	. 
	nop			;a2ca	00 	. 
	nop			;a2cb	00 	. 
	nop			;a2cc	00 	. 
	nop			;a2cd	00 	. 
	dec l			;a2ce	2d 	- 
	add a,b			;a2cf	80 	. 
	rst 38h			;a2d0	ff 	. 
	nop			;a2d1	00 	. 
	nop			;a2d2	00 	. 
	nop			;a2d3	00 	. 
	nop			;a2d4	00 	. 
	nop			;a2d5	00 	. 
	jp 0e03eh		;a2d6	c3 3e e0 	. > . 
	nop			;a2d9	00 	. 
	nop			;a2da	00 	. 
	nop			;a2db	00 	. 
	nop			;a2dc	00 	. 
	nop			;a2dd	00 	. 
	add a,b			;a2de	80 	. 
	nop			;a2df	00 	. 
	nop			;a2e0	00 	. 
	nop			;a2e1	00 	. 
	nop			;a2e2	00 	. 
	nop			;a2e3	00 	. 
	nop			;a2e4	00 	. 
	nop			;a2e5	00 	. 
	nop			;a2e6	00 	. 
	nop			;a2e7	00 	. 
	nop			;a2e8	00 	. 
	nop			;a2e9	00 	. 
	nop			;a2ea	00 	. 
	nop			;a2eb	00 	. 
	nop			;a2ec	00 	. 
	nop			;a2ed	00 	. 
	nop			;a2ee	00 	. 
	nop			;a2ef	00 	. 
	nop			;a2f0	00 	. 
	nop			;a2f1	00 	. 
	nop			;a2f2	00 	. 
	nop			;a2f3	00 	. 
	nop			;a2f4	00 	. 
	nop			;a2f5	00 	. 
	nop			;a2f6	00 	. 
	nop			;a2f7	00 	. 
	nop			;a2f8	00 	. 
	nop			;a2f9	00 	. 
	nop			;a2fa	00 	. 
	nop			;a2fb	00 	. 
	nop			;a2fc	00 	. 
	nop			;a2fd	00 	. 
	nop			;a2fe	00 	. 
	nop			;a2ff	00 	. 
	nop			;a300	00 	. 
	nop			;a301	00 	. 
	nop			;a302	00 	. 
	nop			;a303	00 	. 
	nop			;a304	00 	. 
	nop			;a305	00 	. 
	nop			;a306	00 	. 
	nop			;a307	00 	. 
	nop			;a308	00 	. 
	nop			;a309	00 	. 
	nop			;a30a	00 	. 
	nop			;a30b	00 	. 
	nop			;a30c	00 	. 
	nop			;a30d	00 	. 
	nop			;a30e	00 	. 
	nop			;a30f	00 	. 
	nop			;a310	00 	. 
	nop			;a311	00 	. 
	nop			;a312	00 	. 
	nop			;a313	00 	. 
	nop			;a314	00 	. 
	nop			;a315	00 	. 
	nop			;a316	00 	. 
	nop			;a317	00 	. 
	nop			;a318	00 	. 
	nop			;a319	00 	. 
	nop			;a31a	00 	. 
	nop			;a31b	00 	. 
	nop			;a31c	00 	. 
	nop			;a31d	00 	. 
	nop			;a31e	00 	. 
	nop			;a31f	00 	. 
	nop			;a320	00 	. 
	nop			;a321	00 	. 
	nop			;a322	00 	. 
	nop			;a323	00 	. 
	nop			;a324	00 	. 
	nop			;a325	00 	. 
	jr la32dh		;a326	18 05 	. . 
	nop			;a328	00 	. 
	nop			;a329	00 	. 
	nop			;a32a	00 	. 
	nop			;a32b	00 	. 
	nop			;a32c	00 	. 
la32dh:
	nop			;a32d	00 	. 
	nop			;a32e	00 	. 
	nop			;a32f	00 	. 
	nop			;a330	00 	. 
	nop			;a331	00 	. 
	nop			;a332	00 	. 
	nop			;a333	00 	. 
	nop			;a334	00 	. 
	nop			;a335	00 	. 
	nop			;a336	00 	. 
	nop			;a337	00 	. 
	nop			;a338	00 	. 
	nop			;a339	00 	. 
	nop			;a33a	00 	. 
	nop			;a33b	00 	. 
	nop			;a33c	00 	. 
	nop			;a33d	00 	. 
	nop			;a33e	00 	. 
	nop			;a33f	00 	. 
	nop			;a340	00 	. 
	nop			;a341	00 	. 
	nop			;a342	00 	. 
	nop			;a343	00 	. 
	nop			;a344	00 	. 
	nop			;a345	00 	. 
	nop			;a346	00 	. 
	ld bc,00000h		;a347	01 00 00 	. . . 
	nop			;a34a	00 	. 
	nop			;a34b	00 	. 
	nop			;a34c	00 	. 
	nop			;a34d	00 	. 
	nop			;a34e	00 	. 
	nop			;a34f	00 	. 
	nop			;a350	00 	. 
	nop			;a351	00 	. 
	nop			;a352	00 	. 
	nop			;a353	00 	. 
	nop			;a354	00 	. 
	nop			;a355	00 	. 
	nop			;a356	00 	. 
	nop			;a357	00 	. 
	ld bc,00001h		;a358	01 01 00 	. . . 
	cp 0feh		;a35b	fe fe 	. . 
	jp nz,0c1c0h		;a35d	c2 c0 c1 	. . . 
	nop			;a360	00 	. 
	nop			;a361	00 	. 
	nop			;a362	00 	. 
	nop			;a363	00 	. 
	ret p			;a364	f0 	. 
	nop			;a365	00 	. 
	nop			;a366	00 	. 
	nop			;a367	00 	. 
	nop			;a368	00 	. 
	nop			;a369	00 	. 
	nop			;a36a	00 	. 
	nop			;a36b	00 	. 
	nop			;a36c	00 	. 
	nop			;a36d	00 	. 
	nop			;a36e	00 	. 
	nop			;a36f	00 	. 
	pop bc			;a370	c1 	. 
	ret nz			;a371	c0 	. 
	ret nz			;a372	c0 	. 
	ret nz			;a373	c0 	. 
	ret nz			;a374	c0 	. 
	nop			;a375	00 	. 
	nop			;a376	00 	. 
	nop			;a377	00 	. 
	nop			;a378	00 	. 
	nop			;a379	00 	. 
	nop			;a37a	00 	. 
	nop			;a37b	00 	. 
	nop			;a37c	00 	. 
	nop			;a37d	00 	. 
	nop			;a37e	00 	. 
	nop			;a37f	00 	. 
	nop			;a380	00 	. 
	nop			;a381	00 	. 
	nop			;a382	00 	. 
	nop			;a383	00 	. 
	nop			;a384	00 	. 
	nop			;a385	00 	. 
	nop			;a386	00 	. 
	nop			;a387	00 	. 
	nop			;a388	00 	. 
	nop			;a389	00 	. 
	nop			;a38a	00 	. 
	nop			;a38b	00 	. 
	nop			;a38c	00 	. 
	nop			;a38d	00 	. 
	nop			;a38e	00 	. 
	nop			;a38f	00 	. 
	nop			;a390	00 	. 
	nop			;a391	00 	. 
	nop			;a392	00 	. 
	nop			;a393	00 	. 
	nop			;a394	00 	. 
	nop			;a395	00 	. 
	nop			;a396	00 	. 
	nop			;a397	00 	. 
	nop			;a398	00 	. 
	nop			;a399	00 	. 
	nop			;a39a	00 	. 
	nop			;a39b	00 	. 
	nop			;a39c	00 	. 
	nop			;a39d	00 	. 
	nop			;a39e	00 	. 
	nop			;a39f	00 	. 
	jr la3a7h		;a3a0	18 05 	. . 
	nop			;a3a2	00 	. 
	nop			;a3a3	00 	. 
	nop			;a3a4	00 	. 
	nop			;a3a5	00 	. 
	nop			;a3a6	00 	. 
la3a7h:
	nop			;a3a7	00 	. 
	nop			;a3a8	00 	. 
	nop			;a3a9	00 	. 
	nop			;a3aa	00 	. 
	nop			;a3ab	00 	. 
	nop			;a3ac	00 	. 
	nop			;a3ad	00 	. 
	nop			;a3ae	00 	. 
	nop			;a3af	00 	. 
	nop			;a3b0	00 	. 
	nop			;a3b1	00 	. 
	nop			;a3b2	00 	. 
	nop			;a3b3	00 	. 
	nop			;a3b4	00 	. 
	nop			;a3b5	00 	. 
	nop			;a3b6	00 	. 
	nop			;a3b7	00 	. 
	nop			;a3b8	00 	. 
	nop			;a3b9	00 	. 
	nop			;a3ba	00 	. 
	nop			;a3bb	00 	. 
	nop			;a3bc	00 	. 
	nop			;a3bd	00 	. 
	ld bc,00101h		;a3be	01 01 01 	. . . 
	ld bc,00000h		;a3c1	01 00 00 	. . . 
	nop			;a3c4	00 	. 
	nop			;a3c5	00 	. 
	nop			;a3c6	00 	. 
	nop			;a3c7	00 	. 
	nop			;a3c8	00 	. 
	nop			;a3c9	00 	. 
	nop			;a3ca	00 	. 
	nop			;a3cb	00 	. 
	nop			;a3cc	00 	. 
	nop			;a3cd	00 	. 
	nop			;a3ce	00 	. 
	nop			;a3cf	00 	. 
	nop			;a3d0	00 	. 
	nop			;a3d1	00 	. 
	ld bc,00001h		;a3d2	01 01 00 	. . . 
	cp 0feh		;a3d5	fe fe 	. . 
	jp nz,0c1c0h		;a3d7	c2 c0 c1 	. . . 
	nop			;a3da	00 	. 
	nop			;a3db	00 	. 
	ld b,b			;a3dc	40 	@ 
	nop			;a3dd	00 	. 
	cp 000h		;a3de	fe 00 	. . 
	nop			;a3e0	00 	. 
	ld b,b			;a3e1	40 	@ 
	nop			;a3e2	00 	. 
	nop			;a3e3	00 	. 
	nop			;a3e4	00 	. 
	nop			;a3e5	00 	. 
	nop			;a3e6	00 	. 
	nop			;a3e7	00 	. 
	nop			;a3e8	00 	. 
	nop			;a3e9	00 	. 
	pop bc			;a3ea	c1 	. 
	pop bc			;a3eb	c1 	. 
	pop bc			;a3ec	c1 	. 
	pop bc			;a3ed	c1 	. 
	ret nz			;a3ee	c0 	. 
	nop			;a3ef	00 	. 
	nop			;a3f0	00 	. 
	nop			;a3f1	00 	. 
	nop			;a3f2	00 	. 
	nop			;a3f3	00 	. 
	nop			;a3f4	00 	. 
	nop			;a3f5	00 	. 
	nop			;a3f6	00 	. 
	nop			;a3f7	00 	. 
	nop			;a3f8	00 	. 
	nop			;a3f9	00 	. 
	nop			;a3fa	00 	. 
	nop			;a3fb	00 	. 
	nop			;a3fc	00 	. 
	nop			;a3fd	00 	. 
	nop			;a3fe	00 	. 
	nop			;a3ff	00 	. 
	nop			;a400	00 	. 
	nop			;a401	00 	. 
	nop			;a402	00 	. 
	nop			;a403	00 	. 
	nop			;a404	00 	. 
	nop			;a405	00 	. 
	nop			;a406	00 	. 
	nop			;a407	00 	. 
	nop			;a408	00 	. 
	nop			;a409	00 	. 
	nop			;a40a	00 	. 
	nop			;a40b	00 	. 
	nop			;a40c	00 	. 
	nop			;a40d	00 	. 
	nop			;a40e	00 	. 
	nop			;a40f	00 	. 
	nop			;a410	00 	. 
	nop			;a411	00 	. 
	nop			;a412	00 	. 
	nop			;a413	00 	. 
	nop			;a414	00 	. 
	nop			;a415	00 	. 
	nop			;a416	00 	. 
	nop			;a417	00 	. 
	nop			;a418	00 	. 
	nop			;a419	00 	. 
	jr la421h		;a41a	18 05 	. . 
	nop			;a41c	00 	. 
	nop			;a41d	00 	. 
	nop			;a41e	00 	. 
	nop			;a41f	00 	. 
	nop			;a420	00 	. 
la421h:
	nop			;a421	00 	. 
	nop			;a422	00 	. 
	nop			;a423	00 	. 
	nop			;a424	00 	. 
	nop			;a425	00 	. 
	nop			;a426	00 	. 
	nop			;a427	00 	. 
	nop			;a428	00 	. 
	nop			;a429	00 	. 
	nop			;a42a	00 	. 
	nop			;a42b	00 	. 
	nop			;a42c	00 	. 
	nop			;a42d	00 	. 
	nop			;a42e	00 	. 
	nop			;a42f	00 	. 
	nop			;a430	00 	. 
	nop			;a431	00 	. 
	nop			;a432	00 	. 
	nop			;a433	00 	. 
	nop			;a434	00 	. 
	nop			;a435	00 	. 
	ld bc,00101h		;a436	01 01 01 	. . . 
	ld bc,00101h		;a439	01 01 01 	. . . 
	nop			;a43c	00 	. 
	nop			;a43d	00 	. 
	nop			;a43e	00 	. 
	nop			;a43f	00 	. 
	nop			;a440	00 	. 
	nop			;a441	00 	. 
	nop			;a442	00 	. 
	nop			;a443	00 	. 
	nop			;a444	00 	. 
	nop			;a445	00 	. 
	nop			;a446	00 	. 
	nop			;a447	00 	. 
	nop			;a448	00 	. 
	nop			;a449	00 	. 
	nop			;a44a	00 	. 
	nop			;a44b	00 	. 
	ld bc,00011h		;a44c	01 11 00 	. . . 
	cp 0feh		;a44f	fe fe 	. . 
	jp nz,0c9c0h		;a451	c2 c0 c9 	. . . 
	nop			;a454	00 	. 
	jr nz,la497h		;a455	20 40 	  @ 
	nop			;a457	00 	. 
	rst 38h			;a458	ff 	. 
	nop			;a459	00 	. 
	nop			;a45a	00 	. 
	ld b,b			;a45b	40 	@ 
	nop			;a45c	00 	. 
	nop			;a45d	00 	. 
	nop			;a45e	00 	. 
	nop			;a45f	00 	. 
	add a,b			;a460	80 	. 
	nop			;a461	00 	. 
	nop			;a462	00 	. 
	nop			;a463	00 	. 
	pop bc			;a464	c1 	. 
	pop bc			;a465	c1 	. 
	pop bc			;a466	c1 	. 
	pop bc			;a467	c1 	. 
	pop bc			;a468	c1 	. 
	ld bc,00000h		;a469	01 00 00 	. . . 
	jr nz,la46eh		;a46c	20 00 	  . 
la46eh:
	nop			;a46e	00 	. 
	nop			;a46f	00 	. 
	nop			;a470	00 	. 
	nop			;a471	00 	. 
	nop			;a472	00 	. 
	nop			;a473	00 	. 
	nop			;a474	00 	. 
	nop			;a475	00 	. 
	nop			;a476	00 	. 
	nop			;a477	00 	. 
	nop			;a478	00 	. 
	nop			;a479	00 	. 
	nop			;a47a	00 	. 
	nop			;a47b	00 	. 
	nop			;a47c	00 	. 
	nop			;a47d	00 	. 
	nop			;a47e	00 	. 
	nop			;a47f	00 	. 
	nop			;a480	00 	. 
	nop			;a481	00 	. 
	nop			;a482	00 	. 
	nop			;a483	00 	. 
	nop			;a484	00 	. 
	nop			;a485	00 	. 
	nop			;a486	00 	. 
	nop			;a487	00 	. 
	nop			;a488	00 	. 
	nop			;a489	00 	. 
	nop			;a48a	00 	. 
	nop			;a48b	00 	. 
	nop			;a48c	00 	. 
	nop			;a48d	00 	. 
	nop			;a48e	00 	. 
	nop			;a48f	00 	. 
	nop			;a490	00 	. 
	nop			;a491	00 	. 
	nop			;a492	00 	. 
	nop			;a493	00 	. 
	jr la49bh		;a494	18 05 	. . 
	nop			;a496	00 	. 
la497h:
	nop			;a497	00 	. 
	nop			;a498	00 	. 
	nop			;a499	00 	. 
	nop			;a49a	00 	. 
la49bh:
	nop			;a49b	00 	. 
	ld bc,00001h		;a49c	01 01 00 	. . . 
	nop			;a49f	00 	. 
	nop			;a4a0	00 	. 
	nop			;a4a1	00 	. 
	nop			;a4a2	00 	. 
	nop			;a4a3	00 	. 
	nop			;a4a4	00 	. 
	nop			;a4a5	00 	. 
	nop			;a4a6	00 	. 
	nop			;a4a7	00 	. 
	nop			;a4a8	00 	. 
	nop			;a4a9	00 	. 
	nop			;a4aa	00 	. 
	nop			;a4ab	00 	. 
	nop			;a4ac	00 	. 
	nop			;a4ad	00 	. 
	ld bc,00101h		;a4ae	01 01 01 	. . . 
	ld bc,00101h		;a4b1	01 01 01 	. . . 
	ld bc,00001h		;a4b4	01 01 00 	. . . 
	nop			;a4b7	00 	. 
	nop			;a4b8	00 	. 
	nop			;a4b9	00 	. 
	nop			;a4ba	00 	. 
	nop			;a4bb	00 	. 
	nop			;a4bc	00 	. 
	nop			;a4bd	00 	. 
	nop			;a4be	00 	. 
	nop			;a4bf	00 	. 
	nop			;a4c0	00 	. 
	nop			;a4c1	00 	. 
	nop			;a4c2	00 	. 
	nop			;a4c3	00 	. 
	nop			;a4c4	00 	. 
	nop			;a4c5	00 	. 
	ld hl,00011h		;a4c6	21 11 00 	! . . 
	cp 0feh		;a4c9	fe fe 	. . 
	jp nz,0c9c0h		;a4cb	c2 c0 c9 	. . . 
	djnz la4f0h		;a4ce	10 20 	.   
	ld b,b			;a4d0	40 	@ 
	nop			;a4d1	00 	. 
	rst 38h			;a4d2	ff 	. 
	nop			;a4d3	00 	. 
	nop			;a4d4	00 	. 
	ld b,b			;a4d5	40 	@ 
	nop			;a4d6	00 	. 
	nop			;a4d7	00 	. 
	nop			;a4d8	00 	. 
	nop			;a4d9	00 	. 
	ret p			;a4da	f0 	. 
	nop			;a4db	00 	. 
	nop			;a4dc	00 	. 
	nop			;a4dd	00 	. 
	pop de			;a4de	d1 	. 
	pop bc			;a4df	c1 	. 
	pop bc			;a4e0	c1 	. 
	pop bc			;a4e1	c1 	. 
	pop bc			;a4e2	c1 	. 
	ld bc,00101h		;a4e3	01 01 01 	. . . 
	jr nz,$+18		;a4e6	20 10 	  . 
	nop			;a4e8	00 	. 
	nop			;a4e9	00 	. 
	nop			;a4ea	00 	. 
	nop			;a4eb	00 	. 
	nop			;a4ec	00 	. 
	nop			;a4ed	00 	. 
	nop			;a4ee	00 	. 
	nop			;a4ef	00 	. 
la4f0h:
	nop			;a4f0	00 	. 
	nop			;a4f1	00 	. 
	nop			;a4f2	00 	. 
	nop			;a4f3	00 	. 
	nop			;a4f4	00 	. 
	nop			;a4f5	00 	. 
	ld bc,00000h		;a4f6	01 00 00 	. . . 
	nop			;a4f9	00 	. 
	nop			;a4fa	00 	. 
	nop			;a4fb	00 	. 
	nop			;a4fc	00 	. 
	nop			;a4fd	00 	. 
	nop			;a4fe	00 	. 
	nop			;a4ff	00 	. 
	nop			;a500	00 	. 
	nop			;a501	00 	. 
	nop			;a502	00 	. 
	nop			;a503	00 	. 
	nop			;a504	00 	. 
	nop			;a505	00 	. 
	nop			;a506	00 	. 
	nop			;a507	00 	. 
	nop			;a508	00 	. 
	nop			;a509	00 	. 
	nop			;a50a	00 	. 
	nop			;a50b	00 	. 
	nop			;a50c	00 	. 
	nop			;a50d	00 	. 
