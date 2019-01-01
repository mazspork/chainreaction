; *****************************************************************************
; *									      *
; * CHAIN REACTION, 48K/128K ZX Spectrum remix (started August 1st 1986)      *
; * Includes Integrated Machine-language Algorithms for Graphic Exhibition    *
; * Copyright (c) 1986 Durell Software Ltd. Written by Maz Spork, TDB.	      *
; * "Without love and beauty and danger, life would be almost easy to live"   *
; * (Incredibili celeritate, magno spatio paucis diebus confecto)	      *
; * This program was bug hunted by the XORcist !			      *
; *									      *
; * There is a bug which appears when a dynamic object is in collision	      *
; * exception state and moves to collide with another static object	      *
; * (not ExtObj), most commonly a doorpole. Then the dynamic object will NOT  *
; * collide properly The program will, however, not crash. Refer to fig. 1.   *
; *									      *
; *****************************************************************************
;
; The IMAGE Spectrum memory map is as follows:-
;
;	    0000-3FFF  :   ROM
;	    4000-5AFF  :   Display file
;	    5B00-5FFF  :   Background storage buffers 
;	    6000-77FF  :   Shadow display file 
;	    7800-7DFF  :   Not used 
;	    7E00-7EFF  :   Stack
;	    7F00-7FFF  :   Workpage
;	    8000-C7FF  :   Main core
;	    D000-FFFF  :   Foreground graphics
;
; -----------------------------------------------------------------------------
;
; The source files are laid out as follows (22:apr:87) :-
;
;	    IMAGE1     :   Menus.
;	    IMAGE1a    :   Sprite drawing routines.
;	    IMAGE2     :   Hidden lines.
;	    IMAGE2a    :   Keyboard decoding, floor and wall segments.
;	    IMAGE3     :   Table updating.
;	    IMAGE3a    :   Table initialising.
;	    IMAGE4     :   Backgrounds and Collision detection.
;	    IMAGE4a    :   VDU handler.
;	    IMAGE5     :   Sprite controllers: ACTION servers.
;	    IMAGE5a    :   Sprite controllers: GROL servers.
;	    IMAGE6     :   Interrupt server: Bullets.
;	    IMAGE6a    :   Interrupt server: Multicolour display.
;	    IMAGE7     :   Tables (reorigin).
;	    IMAGE7a    :   More tables.
;	    IMAGE8     :   Variables.
;	    IMAGE8a    :   More variables.
;	    IMAGE9     :   Room tables (reorigin).
;	    IMAGE9a    :   Doesn't exist (so why bother?) (refer to fig. 1).
;
; -----------------------------------------------------------------------------
;
; The 3D to 2D algorithm is:-  X=Cx+2*(x-y), Y=Cy-x-y-z
;
; The Box/Line assignments are:-
;
;	 ----	       |\	     ---	 /|
;   1.	 |  |	   2.  | \	3.   \ |    4.	/ |
;	 ----	       ---	      \|	---
;
;	 ---		---	     ---	  /
;   5.	 | /	   7.  / /	6.    \ \   8.	 /
;	 |/	      ---	       ---	/
;
;	 \			      |
;   9.	  \	  10. ---      11.    |
;	   \			      |
;
; -----------------------------------------------------------------------------
;
; The Main C-ish Algorithm is:-
;
;	setup_room(current_room);
;	print_dynamic(all);
;	while room_change=FALSE {
;	   move_pseudo_to_main()
;	   erase_dynamic(all)
;	   update_pointers(all)
;	   print_dynamic(all) }
;	}
;
; -----------------------------------------------------------------------------
;
; Assembler directives for screen and printer control:
;
PROC	     'Z80'
PAGE	     46
WIDTH	     111	      ; Facing broad printers...
;
DEBUG	     EQU  %00	      ; Bit 0 = Register print patch module included
			 ; Bit 1 = X/Y coord printup module included
;
; -----------------------------------------------------------------------------
;
; Main equate block:
;
ONE28	     EQU  0	      ; 1 if 128K version, 0 if 48K
CHEAT	     EQU  0	      ; Cheat mode on
RODOWN	     EQU  32	      ; How much rod affects man. 0=A LOT, 255=NOT A LOT (!)
PIX	     EQU  &DD	      ; IX register prefix.
PIY	     EQU  &FD	      ; IY register prefix.
XPLUS	     EQU  &70
YPLUS	     EQU  &D0	      ; Middle of screen (X,Y) in 2D.
MAXBUL	     EQU  10	      ; Maximum no of bullets on screen (only for speed+
KBPORT	     EQU  &FE	      ; Keyboard I/O port			   purposes)
JOYPOR	     EQU  &1F	      ; KEMPSTON joystick
CHFROM	     EQU  &07	      ; Border/screen colour to change from/to in the
CHTO	     EQU  &00	      ; smooth clear screen/border (not used)
BACKGR	     EQU  &5B00       ; Background storage buffer
MATRIX	     EQU  &5F00       ; Matrix expansion workspace for macroholes
PSEUDO	     EQU  &6000       ; Pseudo screen, 6K in size
INIT	     EQU  25000
OPHAV	     EQU  &5B00
COLMAP	     EQU  &5C00       ; 'Shadow' attribute colour map
STACK	     EQU  &7F00       ; Stack - may be changed. 256+ bytes in size
WORKPG	     EQU  &5F00       ; Work Page, buffer for treating sprites before print
PROGRM	     EQU  &8000       ; Main entry point
INTRPT	     EQU  &8080       ; Address of interrupt server
WALLS	     EQU  &B800       ; Background graphics (not used)
IMAGES	     EQU  &D200       ; Foreground graphics
WH	     EQU  7+64	      ; Colours: WHITE,YELLOW,CYAN,GREEN,MAGENTA,RED,BLUE
YE	     EQU  6+64	      ; all with brightness ON.
CY	     EQU  5+64
GR	     EQU  4+64
MA	     EQU  3+64
RE	     EQU  2+64
BL	     EQU  1+64
GUNFIRE_1    EQU   1	 ; Sound effects
JET_BURNER   EQU   2
DOOR_OPENING EQU   3
DOOR_CLOSING EQU   4
EXPLODE_1    EQU   5
GUNFIRE_2    EQU   6
GUNFIRE_3    EQU   7
GUNFIRE_4    EQU   8
EXPLODE_2    EQU   9
OVERRULE     EQU   &80	 ; Means "overrule sound queue priorities"
;
TITLE	'IMAGE1: Menu screens and demonstration sequences'
EJECT
;
; *****************************************************************************
;
; MENU SCREENS AND OPTION SELECTIONS
;
; This section includes interrupt low-level servers and sound routines. It also
; carries the keyboard redefinition, High-score table printup and the name
; entering, and (for the speccy 128) the memory paging supervisors.
;
; *****************************************************************************
;
	ORG  INIT
;
	LD   A,71
	CALL CLRALL
	LD   HL,MADEB2-(INIT-OPHAV)
	CALL SEQS
	LD   HL,MADEBY-(INIT-OPHAV)
	CALL PRINT
	CALL MOVESC
	CALL GETKEY
	RET
MADEBY: DW   PSEUDO+&0184
	DB   5,%110,'CHAIN REACTION',&FF
	DW   PSEUDO+&060C
	DB   4,%1,'Written by',&FF
	DW   PSEUDO+&080C
	DB   0,%10001,'Maz Spork',&FF
	DW   PSEUDO+&0B0D
	DB   0,%1001,'Music by',&FF
	DW   PSEUDO+&0C09
	DB   4,%11001,'Jean-Michel Jarre',&FF
	DW   PSEUDO+&0E06
	DB   0,%1001,'Arranged for the Spectrum by',&FF
	DW   PSEUDO+&0F0C
	DB   0,%11001,'Rob Hubbard',&FF
	DW   PSEUDO+&1306
	DB   0,%1001,'(C) 1987 Durell Software Ltd.',&FF
	DW   PSEUDO+&14EB
	DB   0,%11,'PRESS ANY KEY',0
MADEB2: DB   1
	DW   &5803
	DB   &18,&17,%01001111
	DB   1
	DW   &5820
	DB   &1E,&03,%01010110
	DB   2
	DW   PSEUDO+&0003
	DB   &18,&B9
	DB   2
	DW   PSEUDO+&00E0
	DB   &1E,&1A
	DB   0
OPHAV2: DB   0
;
	ORG  PROGRM	       ; Start here!
;	
CHAIN:	DI
	LD   SP,STACK+&FF
	CALL SETINT	       ; Set up interrupt register and mode
	CALL INITMU
	CALL CHKEMP	       ; Check if a Kempston joystick is fitted.
	LD   IX,PSKEY
	LD   HL,KEYDAT
	JR   NZ,GETSET	       ; If not, initialise to standard keys -
	LD   IX,PJOYS	       ; Else start up in Kempston joystick mode.
	LD   HL,JOYDAT
	CALL OOPS1
GETSET: CALL SETOPT
	CALL OPHAV	       ; Tell ppl that they die if they pirate the game
START:	LD   SP,STACK+&FF      ; Reserve a page for the stack (generous,i KNOW)
GO:	CALL DISINT
	XOR  A
	CALL CLRALL
	CALL MAKET1
	LD   HL,FARVER
	LD   DE,0
	LD   A,8
	LD   BC,&0300
	CALL MSETUP	       ; Multicolour mode
	CALL DISINT
	LD   HL,PSEUDO	       ; Clear pseudo screen
	CALL CLRSCR
	LD   HL,SEQS1	       ; Print up main menu sequence
	CALL SEQS
	LD   HL,SEQS2	       ; Print up colours
	CALL SEQS
	LD   HL,&6048
	LD   A,4
	CALL OLOMA
	LD   HL,&6056
	LD   A,4
	CALL OLOMA
	CALL MOVESC	       ; prior to moving down the text
	LD   A,71	       ; Bright white/black
	LD   (COLOUR),A
	LD   HL,&5800
	LD   DE,&5801
	LD   (HL),A
	LD   BC,&02FF
	LDIR
	CALL ENAINT
	JP   AWAIT
UDTRPT: NOP		       ; End of first chunk
;
	ORG  INTRPT	       ; Address &8080
	DB   &C3	       ; Opcode JP
VECTOR: DW   GOBACK	       ; Interrupt vector
	DW   INITMU
GOBACK: PUSH HL
	PUSH DE
	PUSH BC
	PUSH AF
	PUSH IX
	CALL MUSIC
	POP  IX
	POP  AF
	POP  BC
	POP  DE
	POP  HL
	EI
	RETI		       ; loads PCh with (SP) and PCl with (SP+1),
;			       ; then increases SP with two. (good, isn't it)
AWAIT:	LD   BC,&1000	       ; 48 'wait' states
AWAIT0: LD   HL,FORTY2	       ; This is the Maztercontrol pass text
AWAIT1: HALT		       ; Give an interrupt
	PUSH HL
	PUSH BC
	CALL INKEY	       ; Grab a key (from the keyboard(!))
	POP  BC
	POP  HL
	JP   NC,AWAIT3	       ; Was it a successful grab?
	CP   '6'               ; (change this to whatever if more is req.)
	JR   C,AWAIT4	       ; Was it a digit?
	CP   (HL)
	JR   NZ,AWAIT0	       ; Was it a superwoman (superwoman don't know...)
	INC  HL
	CALL GETOFF	       ; Release key
	LD   A,(HL)
	OR   A		       ; Was that the last key in the sequence?
	JR   NZ,AWAIT1	       ; Nope
	JP   PASSED	       ; Yep. Someone's just passed the password!
AWAIT4: SUB  '0'               ; Get digit number
	JR   C,AWAIT0	       ; But don't if someone just made a patch here
	ADD  A,A	       ; Two per address
	LD   L,A
	LD   H,0
	LD   DE,MENUJB	       ; Find job addresses
	ADD  HL,DE
	LD   A,(HL)
	INC  HL
	LD   H,(HL)	       ; Get appropriate job address
	LD   L,A
	PUSH HL
	CALL DISINT	       ; No colours
	XOR  A
	JP   CLRALL	       ; Call job through clearing screens
AWAIT3: LD   DE,100	       ; (no keys pressed at all)
	LD   A,(PJOYS)
	AND  %1000	       ; Kempston selected?
	JR   Z,AWAIT2	       ; No
	IN   A,(31)
	AND  %00010000
	JP   NZ,STAGAM
AWAIT2: DEC  DE 	       ; Wait for 1 ms
	LD   A,D
	OR   E
	JP   NZ,AWAIT2
	DEC  BC 	       ; Wait the thirty loops -
	LD   A,B
	OR   C
	JP   NZ,AWAIT1
	CALL DISINT
	CALL SCORES	       ; and then show the high scores
	JP   GO 	       ; and call it a day.
PASSED: 
JP WONGAM

IF CHEAT
	CALL GETOFF	       ; Enter here when someone typed 'FORTY2'
	CALL DISINT
	LD   A,71
	CALL CLRALL
	LD   HL,FORTY3
	CALL PRINT
	LD   IX,&4800
	XOR  A
	LD   BC,&01FF
	CALL KINPUT
	LD   HL,WORKPG+&80
MOREC1: LD   A,(HL)
	INC  HL
	LD   C,'0'
	CP   'L'      ; STARTING LEVEL IN BUILDING
	JR   Z,CHLEV
	CP   'E'      ; INFINIT JETPACK POWER
	JR   Z,INFJET
	CP   'B'      ; BULLETS DON'T HIDE
	JR   Z,BULOFF
	CP   'D'      ; DDO ROD OR DON'T DDO RODS!
	JR   Z,DDOTOG
	CP   'R'      ; NO. OF RODS IN GAME
	JR   Z,DDORRO
	JP   GO
DDORRO: LD   A,(HL)
	SUB  C
	LD   E,A
	INC  HL
	LD   A,(HL)
	SUB  C
	JR   C,GRAXXX
	CP   9
	JR   NC,GRAXXX
	ADD  A,A
	LD   D,A
	ADD  A,A
	ADD  A,A
	ADD  A,D
	ADD  A,E
	LD   E,A
	INC  HL
GRAXXX: DEC  HL
	LD   D,0
	PUSH HL
	EX   DE,HL
	ADD  HL,HL
	ADD  HL,HL
	LD   DE,AUXORG
	ADD  HL,DE
	LD   (HL),0
	POP  HL
	JP   MOREC
DDOTOG: LD   A,(HL)
	SUB  C
	JR   Z,DDOTO1
	LD   A,2
DDOTO1: LD   (UB0VAL),A
	JP   GO
BULOFF: LD   A,(HL)
	SUB  C
	LD   A,201
	JR   Z,NOJOKE	    ; It's no joke!
	XOR  A
NOJOKE: LD   (BHIDD),A
	JP   MOREC
CHLEV:	LD   A,(HL)
	SUB  C
	LD   (SFLOOR),A
	JP   MOREC
INFJET: LD   A,(HL)
	SUB  C
	RLCA
	RLCA
	PUSH HL
	LD   HL,JETFLG
	XOR  (HL)
	LD   (HL),A
	POP  HL
MOREC:	INC  HL
	JP   MOREC1
ELSE
	JP   GO
ENDIF
;
SCORES: CALL HSTABL
SCLRAT: LD   B,30
	CALL PAUSE	       ; Wait until timeout or keypress
	RET
HSTABL: XOR  A
	CALL CLRALL
	LD   HL,SEQS3
	CALL SEQS
	LD   HL,HERO
	LD   (HL),5	       ; DF LSB
	INC  HL
	LD   (HL),HIGH(PSEUDO+&400)  ; DF MSB
	INC  HL
	LD   (HL),0	       ; Pixel no.
	INC  HL
	LD   (HL),%11001       ; Pflags
	LD   B,8	       ; 8 heroes
	LD   DE,HEROES
	EXX
	LD   BC,HSCTAB
	EXX
SLARTY: EXX
	LD   DE,HERO+4
	LD   A,(BC)
	LD   L,A
	INC  BC
	LD   A,(BC)
	LD   H,A
	INC  BC
	PUSH BC
	CALL DSCORE
	POP  BC
	EXX
	PUSH BC
	LD   HL,HERO+12        ; This is the target
	EX   DE,HL	       ; HL is a hero, BC is &10, DE is HERO+4
	LD   BC,&10	       ; Length
	LDIR		       ; Move that to temprary area
	XOR  A
	LD   (DE),A	       ; End-marker
	PUSH HL
	LD   HL,HERO+1	       ; and print it
	INC  (HL)
	INC  (HL)
	DEC  HL
	CALL PRINT
	LD   HL,HERO	       ; Display-file MSB
	POP  DE 	       ; Get pointer to next name
	POP  BC 	       ; Counter
	DJNZ SLARTY
	LD   HL,&5800
	LD   DE,&5801
	LD   BC,&02FF
	LD   (HL),71	       ; Clear the attributes
	LDIR
	LD   HL,SEQS4	       ; Colours
	CALL SEQS
	CALL MOVESC	       ; Move the screen
	RET
;
PAUSE:	XOR  A		       ; Returns NZ if key was pressed during pause
	IN   A,(&FE)
	CPL
	AND  &1F
	RET  NZ
	DEC  DE
	LD   A,D
	OR   E
	JR   NZ,PAUSE
	DJNZ PAUSE
	RET
;
; Enter with A=attr value
;
CLRALL: PUSH AF
	LD   HL,&5800	       ; Make screen black whilst clearing it.
	LD   DE,&5801	       ; (amstrad: turn it off)
	LD   BC,&02FF
	LD   (HL),L
	LDIR
	LD   HL,&4000
	CALL CLRSCR	       ; Clear main d.f.
	LD   HL,PSEUDO
	CALL CLRSCR	       ; Clear pseudo d.f.
	POP  AF
	LD   HL,&5800	       ; Florg the attributes
	LD   DE,&5801
	LD   BC,&02FF
	LD   (HL),A	       ; with the given value
	LDIR
	XOR  A
	OUT  (KBPORT),A        ; Border...
	RET
;
SETINT: DI

IF ONE28     ; ----------------------------------------------------------------

	LD   A,4+16
	CALL RAMSEL
	LD   HL,PALETE	       ; Move some colours up!
	LD   DE,&C000
	LD   BC,&600
	LDIR
	LD   A,3+16
	CALL RAMSEL
	LD   HL,&4000
	LD   DE,&C000
	LD   BC,&1000	       ; Move the music up!
	LDIR
	LD   A,0+16
	CALL RAMSEL

ENDIF	     ; ----------------------------------------------------------------

	LD   HL,INIT
	LD   DE,OPHAV
	LD   BC,OPHAV2-INIT
	LDIR
	LD   A,HIGH (JMPBLK)
	LD   I,A
	IM   2
	EI
	RET
;
JOYSTK: LD   HL,JOYDAT
	LD   IX,PJOYS
	JR   OOPS
;
CURSOR: LD   HL,CSRDAT
	LD   IX,PCURSR
	JR   OOPS
;
PLUS2:	LD   HL,PL2DAT
	LD   IX,PPLUS2
	JR   OOPS
;
STDKEY: LD   HL,KEYDAT
	LD   IX,PSKEY
;
OPTION: CALL OPTX1
OOPS2:	CALL SETOPT
	JP   GO
OOPS:	CALL OOPS1
	JR   OOPS2
OPTX1:	LD   DE,FORZER	  ; Word "0" ad three E0s (character NOPs)
	JR   OPTI1
OOPS1:	LD   DE,FIRETX+2  ; Word "FIRE"
OPTI1:	PUSH HL
	EX   DE,HL
	LD   DE,SEQSS1+1
	LD   BC,4
	LDIR
	POP  HL
	RET
;
SETOPT: LD   BC,6*3
	LD   DE,CTRLTL
	PUSH IX
	LDIR
	POP  HL
OPT1:	XOR  A
	LD   (PDKEY),A
	LD   (PSKEY),A
	LD   (PJOYS),A
	LD   (PPLUS2),A
	LD   (PCURSR),A
	LD   (HL),%1000
	RET
;
DEFKEY: LD   HL,FARVE2
	LD   DE,0
	LD   A,8
	LD   BC,&0300
	CALL MSETUP	       ; Multicolour mode
	CALL DISINT
	LD   HL,TEXT2
	CALL SEQS
	AND  A
	CALL MOVESC
	LD   HL,&5800
	LD   DE,&5801
	LD   BC,767
	LD   (HL),71
	LDIR
	CALL ENAINT
	LD   HL,&40B3
	LD   (TEXT3),HL
	LD   HL,CTRLTL
	LD   IX,FLASH
	LD   B,6
GO6KEY: LD   (IX+0),%11001110
	PUSH BC
	PUSH HL
GRABAK: HALT
	CALL INKEY
	JR   NC,GRABAK
	POP  HL
	LD   (HL),&FE
	INC  HL
	LD   (HL),C
	INC  HL
	LD   (HL),B
	INC  HL
	PUSH HL
	PUSH IX
	LD   HL,(TEXT3)
	LD   C,A
	LD   A,L
	ADD  A,&40
	LD   L,A
	JR   NC,BFRE
	LD   A,H
	ADD  A,8
	LD   H,A
BFRE:	LD   (TEXT3),HL
	LD   HL,TEXT3+4
	LD   A,C
	LD   (HL),A
	CP   &20
	JR   Z,ASPC
	JR   NC,LESSTS
	EX   DE,HL
	LD   BC,5
	LD   HL,TENTER
	CP   13
	JR   Z,ISENT
	ADD  HL,BC
	JR   ISENT
ASPC:	LD   DE,TSPACE
	LD   BC,5
	EX   DE,HL
ISENT:	LDIR
	XOR  A
	LD   (DE),A
	JR   STYWUR
LESSTS: INC  HL
	LD   (HL),0
STYWUR: LD   HL,TEXT3
	CALL PRINT
	CALL GETOFF
	POP  IX
	POP  HL
	LD   (IX+0),0
	LD   BC,16
	ADD  IX,BC
	POP  BC
	DJNZ GO6KEY
	LD   HL,PSKEY
	CALL OPT1	       ; Set option
	XOR  A
	CALL DISINT
	CALL CLRALL
	LD   HL,CTRLTL
	LD   DE,KEYDAT
	LD   BC,3*6
	LDIR		      ; Move to keyboard
	JP   GO
;
STAGAM: CALL DISINT
	CALL TOFFMU
	LD   A,71
	LD   (COLOUR),A
	CALL CLRALL
	LD   A,3
	LD   (MANFLG),A
	XOR  A
	LD   (FLAGS3),A        ; Do some initialisation
	LD   (ELESTA),A
	LD   (MOVED),A
	LD   (FUEL),A
	LD   (UREM),A
	LD   (UREM3),A
	LD   (SCORE),A
	LD   (SCORE+1),A
	DEC  A
	LD   (BCKRAD),A        ; Background radiation
	LD   (BCKDEL),A
	LD   A,(SFLOOR)        ; Start on floor (norm. 1)
	LD   (CFLOOR),A        ; Current Floor
	LD   HL,1800
	LD   (TIMER),HL        ; 1/2 hour to solve the game...
	LD   HL,ROBOTP
	LD   DE,ROBOTP+1
	LD   BC,&0012
	LD   (HL),&FF	       ; Allow robots in all rooms.
	LDIR
	LD   HL,AUXORG
	LD   DE,AUXTAB
	XOR  A
RSAUX0: LD   B,A
	LD   A,(HL)
	OR   A
	JR   Z,RSAUX1
	LD   A,B
	INC  A
	LDI
	INC  DE
	INC  DE
	LDI
	LDI
	LDI
	JR   RSAUX0
RSAUX1: LD   A,B
	LD   (NORODS),A
	CP   10 	       ; Split line if >10 rods...
	JR   C,RSAUX2
	SUB  9
	LD   DE,PSEUDO+&1617
	CALL DRWXRD
	LD   A,9
RSAUX2: LD   DE,PSEUDO+&1717
	CALL DRWXRD
	CALL BSETUP	       ; Bullet interrupts from now on
	XOR  A
	LD   B,1
	JP   GO4IT	       ; no comment
;
DRWXRD: LD   C,A	       ; Draw a string of rods (only on PDF!)
DRWXR3: CALL DRWROD
	INC  E
	DEC  C
	RET  Z
	JR   DRWXR3
DRWROD: LD   HL,RODCHR	       ; Draw one rod
	LD   B,8
DRWRO1: LD   A,(HL)
	LD   (DE),A
	INC  HL
	LD   A,E
	ADD  A,&20	       ; down a line...
	LD   E,A
	DJNZ DRWRO1
	RET
;
ENDGAM: LD   HL,JETFLG
	BIT  1,(HL)
	CALL NZ,PPUTBK
	CALL DISINT
	XOR  A
	LD   (NOBULL),A
	LD   (B2REMO),A        ; Reset bullet vars
	LD   A,7
ENDG2:	LD   HL,&5800
	LD   DE,&5801
	LD   (HL),A
	SET  6,(HL)
	LD   BC,767
	HALT
	LDIR
	DEC  A
	JP   P,ENDG2
	CALL DISINT
	XOR  A
	CALL CLRALL
	CALL MAKET2
	LD   HL,FARVER
	LD   DE,1
	LD   A,8
	LD   BC,&0202
	CALL MSETUP
	CALL DISINT
	LD   HL,ENDTXT
	CALL PRINT
	CALL ENAINT
	CALL GETOJK
ENTNAM: CALL DISINT
	XOR  A
	CALL CLRALL
	CALL INITMU
	LD   HL,HSCTAB
	LD   A,8
NAM1:	DEC  A
	JP   M,DISPTA
	LD   DE,(SCORE)
	LD   C,(HL)
	INC  HL
	LD   B,(HL)
	INC  HL
	EX   DE,HL
	AND  A
	SBC  HL,BC
	EX   DE,HL
	JR   C,NAM1
	LD   (LISTNO),A
	LD   HL,HSCTAB+7*2-1
	ADD  A,A
	JR   Z,NAM2
	LD   C,A
	LD   B,0
	LD   DE,HSCTAB+8*2-1
	LDDR
NAM2:	INC  HL
	LD   DE,(SCORE)
	LD   (HL),E
	INC  HL
	LD   (HL),D
	LD   A,(LISTNO)
	LD   HL,HEROES+16*7-1
	ADD  A,A
	JR   Z,NAM3
	LD   DE,HEROES+16*8-1
	ADD  A,A
	ADD  A,A
	ADD  A,A
	LD   C,A
	LD   B,0
	LDDR
NAM3:	INC  HL
	PUSH HL
	LD   BC,&1020
NAM4:	LD   (HL),C
	INC  HL
	DJNZ NAM4
	CALL HSTABL
	LD   A,(LISTNO)
	ADD  A,A
	LD   B,A
	LD   A,20
	SUB  B
	LD   B,A
	LD   HL,&400C
NAM5:	LD   A,L
	ADD  A,&20
	LD   L,A
	JR   NC,NAM6
	LD   A,H
	ADD  A,8
	LD   H,A
NAM6:	DJNZ NAM5
	XOR  A
	LD   BC,&190F
	PUSH HL
	POP  IX
	CALL KINPUT
	LD   HL,WORKPG+&80
	POP  DE
	LD   BC,16
NAM7:	LD   A,(HL)
	CP   13
	JR   Z,DISPTA
	LDI	
	JP   PE,NAM7
DISPTA: CALL SCORES
	JP   GO
;
GETOJK: CALL GETOFF
	LD   A,(PJOYS)
	OR   A
	JR   NZ,WAIKMP
WAITK9: HALT
	CALL INKEY
	RET  C
	JR   WAITK9
WAIKMP: IN   A,(31)
	AND  16
	RET  NZ
	HALT
	CALL INKEY
	RET  C
	JR   WAIKMP
;
; Memory paging at low level. Call with A=16K page # to insert into address
; space from C000 to FFFF (49152 to 65535). Preserves all.
;
RAMSEL: 

IF ONE28     ; ----------------------------------------------------------------

	PUSH BC
	LD   BC,&7FFD
	OUT  (C),A
	POP  BC

ENDIF	     ; ----------------------------------------------------------------

	RET
;
; *****************************************************************************
;
; MUSIC routines:
;
; INITMU initialises the Zoolok soundtrack.
; TOFFMU kills the music (and fx's as well)
; MUSIC  must be called at 20 millisecond intervals to keep the sound going.
;
; *****************************************************************************
;
INITMU: LD   HL,AUXFLG	  ; Tell that music is playing
	SET  0,(HL)
	XOR  A		  ; call C000+0
DOMUS:	

IF ONE28     ; ----------------------------------------------------------------

	LD   (SMC42+1),A  ; patch the code!
	LD   A,3+16
	CALL RAMSEL	  ; get page 3 and rom 1
	XOR  A		  ; allways call with a=0
SMC42:	CALL &C000	  ; call C000,C003,C006 or C009
	LD   A,0+16
	CALL RAMSEL	  ; back to page zero (and rom 1) (with raisins)
	LD   A,(MUSOC1)   ; get Music Occupation in queue
	DEC  A
	JP   P,STILG1	  ; still going? then jump
	XOR  A
	LD   (PLAY1),A	  ; else tell play1 that is's not playing anything!
STILG1: LD   (MUSOC1),A   ; Tell musocc where it is.
	LD   A,(MUSOC2)
	DEC  A
	JP   P,STILG2	  ; Do all of the above for channel 2
	XOR  A
	LD   (PLAY2),A
STILG2: LD   (MUSOC2),A

ENDIF	     ; ----------------------------------------------------------------

	RET
MUSIC:	LD   HL,AUXFLG
	BIT  0,(HL)
	RET  Z
	LD   A,3	  ; Entry point #2 - call at 20ms intervals
	JR   DOMUS
TOFFMU: LD   B,DOOR_OPENING
	JP   SOUND
;
DISINT: LD   HL,(VECTOR)
	LD   (OLDVEC),HL
	LD   HL,GOBACK
	LD   (VECTOR),HL
	RET
ENAINT: LD   HL,(OLDVEC)
	LD   (VECTOR),HL
	RET
;
; STRANGE SOUNDS STUDIOS - Interface to Rob Hubbards music routines
; -----------------------------------------------------------------
;
; Call with B=fx routine. If B has bit 7 set, it means "OVERRULE QUEUE", and
; the sound gets out immediately, killing the current playing one.
;
;   1. Bullet for main char
;   2. Jetpack burner
;   3. Open door
;   4. Close door
;   5. Explode robot
;   6. Bullet v.2
;   7. Bullet v.3
;   8. Bullet v.4
;   9. Explode robot v.2
;
; The sounds 1 to 4 are on channel 1 only, the sounds from 5 to 9 are on
; channels 2 to 3.
;
SOUND:	PUSH HL 	 ; Save everything (especially index regs!)
	PUSH DE
	PUSH BC
	PUSH IX
	PUSH IY
	DI		 ; No interrupts while messing with the ram banks

IF ONE28     ; 128k version ---------------------------------------------------

	LD   A,B
	AND  &7F
	CP   5		 ; Is it a dual-channel sound?
	JR   NC,CHAN2	 ; yup, jump
	LD   HL,PLAY1	 ; nope, use this value
	JR   CHAN1
CHAN2:	LD   HL,PLAY2	 ; use channel 2 values
CHAN1:	LD   A,(HL)	 ; Get current playing sound
	DEC  HL 	 ; Decrement to point to occ state of the channel
	BIT  7,B	 ; Check for sound queue overrule bit set (SQO)
	RES  7,B
	JR   NZ,OVERRU	 ; Jump if so.
	CP   B		 ; Same one?
	JR   Z,SOUND1	 ; yes, then no need to repeat it!
	LD   A,(HL)	 ; Get occ state
	OR   A
	JR   NZ,SOUND1	 ; It must be zero!
OVERRU: PUSH HL 	 ; Save the pointer for a little while
	LD   L,B
	LD   H,0	 ; Index into sound queue
	LD   DE,SQUEUE
	ADD  HL,DE
	LD   A,(HL)	 ; Get occupation count for the chosen sound
	POP  HL
	LD   (HL),A	 ; Insert into occupation state
	INC  HL 	 ; Point to "playing sound" again
	LD   (HL),B	 ; This is the playing sound
	LD   A,3+16
	CALL RAMSEL	 ; Select page 3 @ C000
	LD   A,B
	SUB  5
	JR   NC,CHAN3	 ; If 0-4, call C000 with B
	LD   A,B	 ; else call C009 with B-5
	CALL &C000
	JR   CHAN4
CHAN3:	CALL NC,&C009
CHAN4:	LD   A,0+16	 ; Back to page 0 again
	CALL RAMSEL
	LD   HL,AUXFLG
	SET  0,(HL)

ELSE	     ; 48k version ----------------------------------------------------

	LD   L,B
	LD   H,0
	LD   C,H
	LD   DE,SQUEUE
	ADD  HL,DE
	LD   B,(HL)
CRAPUP: LD   A,C
	XOR  &10
	LD   C,A
	OUT  (&FE),A
	CALL RND
	AND  &3F
FUCKUP: DEC  A
	JP   NZ,FUCKUP
	DJNZ CRAPUP

ENDIF	     ; ----------------------------------------------------------------

SOUND1: POP  IY
	POP  IX
	POP  BC
	POP  DE
	POP  HL 	 ; Get back everything
	EI		 ; Ok, ints can occur again
	RET		 ; Yabba-dabba-doo. Bingo, Finished, Exit.
;
TITLE	'IMAGE1a: Sprite handling and drawing'
EJECT
;
; Module name:	Static Graphics Main Entry Point
;
; Label name:	STATIC
;
; Entry params: A=IMAGE number to print
;		B,C,D = X,Y,Z coordinates
;
;		SPECIAL CASES if
;
;		     A=0, the routine only returns with HL=DF ADDR for XYZ
;		     A>87, the object is a Special Computed Object
;
;		Does not consider masking of objects in front. No fixed timing.
;		(possible future ammendment where a flag denotes masking enab.)
;
STATIC: LD   (INPRNT),A
	OR   A
	JP   Z,CALCDF
	CP   88
	JP   C,REALST		; Go on if Real Bit-mapped Object
	LD   A,YPLUS
	SUB  B
	SUB  C
	SUB  D
	LD   L,A
	LD   A,B
	SUB  C
	ADD  A,A
	ADD  A,XPLUS
	RRCA
	RRCA
	RRCA
	AND  &1F
	LD   C,A		; Find 2D coords
	LD   B,0
	LD   H,B
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,BC
	LD   BC,PSEUDO-&0861
	ADD  HL,BC
	LD   A,(INPRNT)
	SUB  88
	ADD  A,A
	EX   DE,HL
	LD   C,A
	LD   B,0
	LD   HL,SADTAB
	ADD  HL,BC
	LD   C,(HL)
	INC  HL
	LD   B,(HL)
	EX   DE,HL
	PUSH BC
	RET
CALCDF: LD   A,YPLUS
	SUB  B
	SUB  C
	SUB  D
	LD   L,A
	LD   H,0
	ADD  HL,HL
	LD   A,B
	SUB  C
	ADD  A,A
	ADD  A,XPLUS
	RRCA
	RRCA
	RRCA
	AND  &1F
	LD   C,A
	LD   B,0
	LD   DE,SCRTAB
	ADD  HL,DE
	LD   E,(HL)
	INC  HL
	LD   D,(HL)
	EX   DE,HL
	ADD  HL,BC
	RET
REALST: CP   &40
	JP   C,REALS1
	LD   A,1+(GDOOR2-DIMTAB)/4 ; Use Doorpole if >64
REALS1: ADD  A,IMAGES/&100-1	; Static object printing (A,B,C,D)
	LD   H,A		
	LD   L,0		; Find address of graphic/mask
	LD   A,YPLUS
STATC1: SUB  B
	SUB  C
	SUB  D
	LD   E,A		; Calc Y(2D) from X,Y,Z
	LD   A,B
	SUB  C
	ADD  A,A
	ADD  A,XPLUS
	LD   C,A		; Calc X(2D) from X,Y,Z
	LD   B,E
	SCF
	LD   A,&FF
	LD   (INPRNT),A
	LD   IX,BACKGR		; Just somewhere where the backgrounds won't
	PUSH IY
	CALL OBJECT		; hurt. Call screen access "main printing"
	POP  IY
	RET
;
; Module name:	Dynamic Graphics (Sprites) Main Entry Point
;
; Label name:	DYNAMC
;
; Entry params: A=sprite number to print
;		carry flag set (1) if plot, reset (0) if unplot.
;
;		Considers masking of overlapping objects. No fixed timing.
;
; A NOTE ON DORMANT DYNAMIC OBJECTS (DDO's)
; -----------------------------------------
;
; A Dormant Dynamic Object will not be updated on screen. In fact, the only
; difference to the Static object is the fact that the background (the data
; "behind" the object) is saved elsewhere in memory, thus enabling the object
; to be "removed" from the screen. It is very important that a Dormant Dynamic
; Object does not "collide" on screen with another dynamic object, that being
; either active or dormant. If this is the case, a proper removal of the object
; will NOT be guaranteed. It has a fair chance of resulting in the wrong
; background data to be put back on the screen, as the Dormant Object will 
; not know if anything goes behind it. In other words, the background storage
; is NEVER updated. "Collosion" between a Dormant Object and a normal static
; one works perfectly well, as the static one is always put on first.
; Therefore, the background buffer will put any static data back properly.
; These objects are a bit wierd and should be used very carefully.
;
DYNAMC: LD   L,A	       ; Dynamic object printing (A=sprite no.)
	LD   H,0
	EX   AF,AF"            ; Save carry which defines plot mode.
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL	       ; 16 bytes per sprite block
	LD   DE,SPRTAB-&10
	ADD  HL,DE	       ; Index.
	LD   A,(HL)
	AND  7		       ; find no. of items in animation sequence list.
	ADD  A,7	       ; Plus 7 for User Byte 0 (1+1+n+3+2)
	LD   E,A
	LD   D,0
	ADD  HL,DE	       ; Which lead us to the User Byte no. 2
	BIT  1,(HL)	       ; Dormant?
	JR   Z,ACTIVE	       ; Jump if not.
	LD   A,(FLAGS2)
	RLA		       ; Check override-flag (if 1st time cycle)
	RET  NC 	       ; OK, if 1st time around do print it (obviously)
ACTIVE: DEC  HL 	       ; Go back to address of IXYZ
	LD   A,(HL)
	DEC  HL
	LD   L,(HL)		
	LD   H,A	       ; HL=address of physical image and X-Y-Z coords
	LD   A,(HL)
	LD   (INPRNT),A
	CP   &10
	JR   NC,DYNAMO
	LD   A,(ELESTA)        ; Ret carry=0 if printing man and in elevator
	CP   3
	RET  Z
	CP   4
	RET  Z
	LD   A,(INPRNT)
DYNAMO: ADD  A,IMAGES/&100-1
	LD   D,A
	LD   E,0	       ; Find address of graphic/mask
	PUSH HL 		
	LD   A,(HL)		
	LD   L,A
	LD   H,E	       ; Get physical image no. again.
	LD   BC,DIMTAB-4
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,BC	       ; Index into Dimensions Table.
	LD   C,(HL)
	INC  HL 	       ; Forward to Y
	LD   B,(HL)	       ; Get X and Y dimension.
	LD   (CURDX),BC        ; And save them for later.
	INC  HL 	       ; Forward to Z
	LD   A,(HL)
	LD   (CURDZ),A	       ; Also save Z.
	POP  HL 	       ; Address of physimage again.
	SET  7,(HL)	       ; Denotes "current object" as to deconfuse 
	PUSH HL 	       ; double overlap check, collision check etc.
	INC  HL
	LD   B,(HL)
	INC  HL
	LD   C,(HL)
	INC  HL
	LD   A,(HL)	       ; Fetch object coordinates,
	PUSH AF 	       ; and save Z on stack.
	LD   A,YPLUS
	SUB  B
	SUB  C
	SUB  (HL)
	JR   C,NVALID
	CP   &20
	JR   NC,VALID1	       ; QDcheck on valid screen coords
NVALID: POP  AF
	POP  HL
	RET
VALID1: LD   H,A	       ; Else store the 2D Y coordinate,
	LD   A,B
	SUB  C
	ADD  A,A
	ADD  A,XPLUS
	LD   L,A	       ; Calc and store 2D X coordinate,
	EX   (SP),HL	       ; and save them on the stack, getting back Z(3D)
	EX   AF,AF"            ; in H. Check alternate carry to consider
	JP   NC,DNOHID	       ; plot/unplot. If unplotting, no "hidden" is
	EX   AF,AF"            ; nescessary. Put carry back.
	PUSH DE 	       ; Save address of physical image/mask.
	LD   D,H	       ; Insert Z at D and call HIDDEN with BCD=XYZ
	CALL HIDDEN
	POP  HL 	       ; get back address of physical image.
	LD   DE,WORKPG+&80     ; this is where the Hide Mask is stored.
DLOOP1: LD   A,(DE)	       ; Operation:
	AND  (HL)	       ; Remove bits in graphic which is reset in the
	RES  7,E	       ; hide mask ie. overlapped by other objects.
	LD   (DE),A	       ; Insert result into low part of workpage.
	SET  7,E	       ; Get hide byte again and complement it
	LD   A,(DE)	       ; before telling mask data that the
	SET  7,L	       ; complemented data must be left alone
	CPL		       ; on the screen.
	OR   (HL)
	LD   (DE),A
	RES  7,L	       ; tidy the pointers.
	INC  L
	INC  E		       ; no Hi-byte considerations.
	JR   NZ,DLOOP1	       ; - the loop check is also done on the index
	EX   AF,AF"            ; registers. get back carry.
DNOHID: LD   L,A	       ; get sprite number. (enter here when unplot)
	EX   AF,AF"
	LD   H,0
	LD   DE,BCKTAB-2       ; Index into Background Storage table, which is
	ADD  HL,HL	       ; a fixed table (for multiplying with A0).
	ADD  HL,DE
	LD   E,(HL)
	INC  HL
	LD   D,(HL)	       ; First DE, and then IX...
	DB   PIX
	LD   H,D
	DB   PIX
	LD   L,E	       ; ... equals the background storage address.
	LD   HL,WORKPG	       ; The accumulated graphic and mask shape is
	POP  BC 	       ; located at WorkPage. Restore X and Y (2D).
	EX   AF,AF"            ; Get carry, identifying PLOT/UNPLOT.
	CALL OBJECT	       ; Print the thing,
	JP   NC,NOUPDA	       ; Skip section if unsuccessful.
	LD   HL,(UPDPTR)       ; Address of screen update addresses.
	LD   (HL),E
	INC  HL
	LD   (HL),D	       ; Insert PDF address
	INC  HL
	LD   (UPDPTR),HL       ; New pointer.
	LD   HL,NO2UPD
	INC  (HL)
NOUPDA: POP  HL 	       ; Pull back pointer into Current Room list,
	LD   A,(LAPPED)
	RES  7,(HL)	       ; and remove the "current" denotation in
	RET		       ; the list. Return with A=overlaps.
;
; Module name:	Main Display File Accessing
;
; Label name:	OBJECT
;
; Entry params: HL=Address of sprite and mask data in memory,
;		IX=Address of background area (where screen data is kept)
;		   (if not required, use WORKPG instead)
;		C =X coordinate on screen (from top-right-hand corner)
;		B =Y coordinate on screen 
;		carry flag set (1) if plot, reset (0) if unplot.
;
;
;	   THIS IS HOW THINGS ARE PRINTED
;    Y	   ==============================
;   ---
;   &00 +----------------------------------+
;	| Bank FF			   |	Invisible "working" area.
;	|				   |	Used to simplify drawing. 
;   &40 +----------------------------------+	(Doesn't really exist as mem)
;	| Bank 00			   |  \
;	|				   |   |
;   &80 +----------------------------------+   | 
;	| Bank 01			   |   | 
;	|				   |	> "Visible" screen area
;   &C0 +----------------------------------+   |
;	| Bank 02			   |   |
;	|				   |   |
;  (&FF)+----------------------------------+  /
;
;	  This system is quite clever, it means that there are 256 Y coords
;	  as well as 256 X ones (virtually). So we have a &100x&100 grid.
;
OBJECT: EX   AF,AF"            ; Main screen access module.
	LD   E,&20	       ; Assume &20 lines to print
	LD   A,(FLAGS2)
	RRCA		       ; This bit says if something is falling 
	JR   NC,NOFALL
	LD   A,(AUXFLG)
	BIT  2,A
	LD   A,(INPRNT)
	JR   Z,MANFAL
RODFAL: CP   20 ; THE UPPER ROD
	JR   NC,NOFALL
	CP   &10 ; THE UPPER MAN
	JR   C,NOFALL
	JR   ROMFAL
MANFAL: CP   &10
	JR   NC,NOFALL
	LD   A,(FLAGS3)
	OR   1
	LD   (FLAGS3),A
ROMFAL: LD   A,(FALLRW)        ; allowed through a hole!
	LD   E,A	       ; E=no. of rows to print.
	LD   A,&20
	SUB  E
	ADD  A,B
	LD   B,A
NOFALL: LD   A,C
	ADD  A,&10	       ; Find middle of object. (32 pixels wide)
	LD   C,A
	LD   A,B
	CP   &E0
	RET  NC 	       ; Check for possible bottom edge problems.
	LD   B,E	       ; No. of lines to print if whole image is on
	SUB  &40	       ; screen. 
	JR   NC,INSIDE	       ; Check for possible top edge problems.
	ADD  A,&20	       ; Check for sprite being totally lost.
	RET  NC 	       ; Sprite is lost off the top - don't bother.
	LD   B,A
	ADD  A,A	       ; Find first pixel row to be printed.
	ADD  A,A
	NEG		       ; This is the new number of bytes to print.
	LD   L,A	       ; This is the new start position of the sprite.
	XOR  A		       ; Start from top of screen.
INSIDE: EXX
	LD   L,A	       ; The Y coordinate (0-191)
	LD   H,0
	LD   DE,PSEUDO	       ; Offset.
	ADD  HL,HL	       ; *2
	ADD  HL,HL	       ; *4
	ADD  HL,HL	       ; *8
	ADD  HL,HL	       ; *16
	ADD  HL,HL	       ; *32 
	LD   A,H
	CP   &18	       ; AMSTRAD!!
	RET  NC
	ADD  HL,DE	       ; Address of screen addresses for 1st row.
	EXX
	SET  7,L	       ; Make sure HL points to the mask area.
	PUSH HL 	       ; 'cause 2's complement allows only for 
	POP  IY 	       ; substraction of &80 - not addition.
	LD   A,C
	AND  7		       ; Pixel number.
	ADD  A,ROTTAB/&100-1   ; High byte of rotation tables.
	LD   H,A
	PUSH BC
	LD   C,0
	SUB  ROTTAB/&100-1     ; Get pixel number again.
	JP   Z,NOEDGE	       ; No right edge.
ISEDGE: SCF
	RR   C		       ; Prepare an 'edge' byte for the mask.
	DEC  A
	JP   NZ,ISEDGE
NOEDGE: EX   AF,AF"
	LD   A,C	       ; - kept here.
	EX   AF,AF"
	EXX
	POP  BC 	       ; Rows to print / X coordinate.
	LD   A,C
	RRCA
	RRCA
	RRCA		       ; Byte pos. at X axis.
	AND  &1F	       ; 32 positions.
	LD   E,A
	LD   D,0
	ADD  HL,DE
	LD   DE,28	       ; No. of bytes per row in PDF
	PUSH HL 	       ; Save screen address
	DEC  HL
	DEC  HL
	SBC  HL,DE	       ; Start off at row -1
	EX   AF,AF"
	JP   NC,UNPLOT	       ; Break now if in unplot mode.
	EX   AF,AF"
	LD   A,C
	AND  7
	JP   Z,NOROT	       ; Jump if bitpos is zero.
;
;
; Module name:	Main Printing Loop
;
; Throughout the routine,
;
;	IY points to the graphic/mask area,
;	IX points to the background buffer,
;	SP points to the screen addresses,
;	DE is the X offset on screen,
;	B' is the pixel row counter,
;	A' is the edge mask.
;
OLOOP1: ADD  HL,DE	       ; Adjust X.
	EXX
	EX   AF,AF"
	LD   C,A	       ; Right edge mask byte.
	EX   AF,AF"
	LD   E,(IY+0)	       ; Mask byte.
	LD   D,(IY+-&80)       ; Graphic byte.
	LD   L,E
	SRL  L
	LD   A,(HL)	       ; Get rotated left mask.
	OR   C		       ; insert edge mask.
	EXX
	LD   C,(HL)
	AND  C		       ; Remove area from background.
	LD   (IX+0),C	       ; Save background.
	EXX
	LD   L,E
	SET  7,L
	LD   C,(HL)	       ; Get rotated right mask.
	LD   L,D
	SRL  L		
	OR   (HL)	       ; Insert rotated left graphic.
	LD   L,D
	SET  7,L
	LD   B,(HL)	       ; Get rotated right graphic.
	EXX
	LD   (HL),A	       ; Put back on screen.
	INC  HL
	EXX
	LD   E,(IY+1)	       ; - Repeat 4 times...
	LD   D,(IY+-&7F)
	LD   L,E
	SRL  L
	LD   A,(HL)
	OR   C
	EXX
	LD   C,(HL)
	AND  C
	LD   (IX+1),C
	EXX
	LD   L,E
	SET  7,L
	LD   C,(HL)
	LD   L,D
	SRL  L
	OR   (HL)
	OR   B
	LD   L,D
	SET  7,L
	LD   B,(HL)
	EXX
	LD   (HL),A
	INC  HL
	EXX
	LD   E,(IY+2)
	LD   D,(IY+-&7E)
	LD   L,E
	SRL  L
	LD   A,(HL)
	OR   C
	EXX
	LD   C,(HL)
	AND  C
	LD   (IX+2),C
	EXX
	LD   L,E
	SET  7,L
	LD   C,(HL)
	LD   L,D
	SRL  L
	OR   (HL)
	OR   B
	LD   L,D
	SET  7,L
	LD   B,(HL)
	EXX
	LD   (HL),A
	INC  HL
	EXX
	LD   E,(IY+3)
	LD   D,(IY+-&7D)
	LD   L,E
	SRL  L
	LD   A,(HL)
	OR   C
	EXX
	LD   C,(HL)
	AND  C
	LD   (IX+3),C
	EXX
	LD   L,E
	SET  7,L
	LD   C,(HL)
	LD   L,D
	SRL  L
	OR   (HL)
	OR   B
	LD   L,D
	SET  7,L
	LD   B,(HL)
	EXX
	LD   (HL),A
	INC  HL
	EXX
	EX   AF,AF"
	LD   E,A
	EX   AF,AF"
	LD   A,E
	CPL		       ; Left-hand edge mask. (RH,complemented)
	OR   C		       ; Insert rest of the rotated 4th byte
	EXX
	LD   C,(HL)
	AND  C		       ; Remove from background
	LD   (IX+4),C	       ; (grab it first)
	EXX
	OR   B		       ; Insert graphics
	LD   BC,5
	ADD  IX,BC
	DEC  C
	ADD  IY,BC
	EXX
	LD   (HL),A	       ; Onto screen again.
	DEC  B
	JP   NZ,OLOOP1
	POP  DE 	       ; DFADDR
	SCF		       ; "Successful print"
	RET
;
;	This is the main sprite printing loop for sprites positioned at pixel
;	zero only. Parameters are like the printing loop above. 
;
NOROT:	ADD  HL,DE	       ; X offset
	LD   A,(HL)	       ; Get screen contents,
	LD   (IX+0),A	       ; Save them,
	AND  (IY+0)	       ; Remove unwanted bits,
	OR   (IY+-&80)	       ; Insert wanted bits,
	LD   (HL),A	       ; Put back on screen
	INC  HL 	       ; And repeat 4 times...
	LD   A,(HL)
	LD   (IX+1),A
	AND  (IY+1)
	OR   (IY+-&7F)
	LD   (HL),A
	INC  HL
	LD   A,(HL)
	LD   (IX+2),A
	AND  (IY+2)
	OR   (IY+-&7E)
	LD   (HL),A
	INC  HL
	LD   A,(HL)
	LD   (IX+3),A
	AND  (IY+3)
	OR   (IY+-&7D)
	LD   (HL),A
	INC  HL
	LD   A,(HL)	       ; ... But do the 5th background anyway to make
	LD   (IX+4),A	       ; it compatible.
	EXX
	LD   DE,5
	ADD  IX,DE	       ; Next background row
	DEC  E
	ADD  IY,DE	       ; Next image row
	EXX
	DJNZ NOROT	       ; Next loop
	POP  DE
	SCF		       ; signal "Successful print"
	RET
;
;	This unplots the sprite.
;
UNPLOT: LD   B,D	       ; Transfer X offset to BC
	LD   C,E
	DB   PIX	       ; DE becomes background buffer,
	LD   D,H
	DB   PIX
	LD   E,L	       ; Background address into DE
	EXX
ULOOP1: EXX
	ADD  HL,BC	       ; X offset
	LD   A,(DE)	       ; Grab background
	LD   (HL),A	       ; Put it on screen
	INC  HL
	INC  DE 	       ; next
	LD   A,(DE)
	LD   (HL),A
	INC  HL
	INC  DE
	LD   A,(DE)
	LD   (HL),A
	INC  HL
	INC  DE
	LD   A,(DE)
	LD   (HL),A
	INC  HL
	INC  DE
	LD   A,(DE)
	LD   (HL),A
	INC  DE
	EXX
	DJNZ ULOOP1
	POP  DE
	SCF
	RET
;
TITLE	'IMAGE2: Hidden lines masking'
EJECT
;
; Module name:	 Hidden Lines off-masking
;
; Label name:	 HIDDEN
;
; Entry params:  B,C,D = X,Y,Z coordinates of square of which the mask
;			 is required
;
; Exit params:	 Only A' preserved, Z=1, C=0
;		 In addition, the high part (&80-&FF) of WORKPG contains
;		 the mask (if any).
;
;		 When an entry is fetched from CURENT, and it is discovered
;		 that it is a Dynamic Object, masking should not be considered
;		 due to the fact that dynamic objects are sorted before print,
;		 and therefore will mask each other out automatically. However,
;		 if it turns out to be a Dormant Dynamic Object, masking must
;		 be done, because the object in fact is not updated at the
;		 time. It is assumed that entries in CURENT and SPRTAB are
;		 placed analogus.
;
HIDDEN: LD   A,YPLUS	       ; Call with BCD=XYZ
	SUB  B		       ; Calc 2D Y (Should be a macro !)
	SUB  C
	SUB  D
	DB   PIX
	LD   H,A	       ; Y=(const-X-Y-Z)
	LD   A,B	       ; Calc 2D X
	SUB  C
	ADD  A,A
	ADD  A,XPLUS
	DB   PIX
	LD   L,A	       ; X=(Y-X)*2+const
	XOR  A			
	LD   (LAPPED),A        ; Counts number of overlapped object
	LD   HL,FLAGS1
	RES  5,(HL)	       ; Used in boundary checks
	LD   HL,WORKPG+&80
	DEC  A
HLOOP0: LD   (HL),A	       ; Set all bits in the result matrix - 
	INC  L		       ; if no objects are detected "in front",
	JP   NZ,HLOOP0	       ; the AND mask must be set.
	LD   A,(NOSTAT)
	LD   (HIDCNT),A
	LD   HL,CURENT
HLOOP1: LD   A,(FLAGS1)
	AND  %11011111	       ; "No inversion whatsoever"
	LD   (FLAGS1),A
	LD   A,(HIDCNT)
	DEC  A		       ; Next object
	LD   (HIDCNT),A
HSTAT:	LD   A,(HL)	       ; A byte from the objects table...
	OR   A
	RET  Z		       ; Last - go back.
	JP   M,NOLAP2	       ; Don't bother with the current object,
	CP   &60
	JP   NC,NOLAP3	       ; Walls and extended objs cannot hide anything.
	CP   1+(GBGHLL-DIMTAB)/4
	JR   Z,SKPHLE
	CP   1+(GBGHLR-DIMTAB)/4
	JR   Z,SKPHLE
	CP   1+(GHOLE1-DIMTAB)/4
	JP   NZ,FGTHLE
SKPHLE: INC  HL 	       ; Make hole not hide anything
	INC  HL
	INC  HL
	INC  HL
	JR   HLOOP1
FGTHLE: CP   &40
	JR   C,NOTDOR	       ; All above the 64 mark are door poles
	LD   A,23
NOTDOR: PUSH HL 	       ; Save the pointer
	DB   PIY
	LD   L,A	       ; Get object into 16 bit IY (unofficial)
	DB   PIY
	LD   H,0
	LD   A,D	       ; Tempsave Z coordinate in A
	LD   DE,DIMTAB-4       ; The offset
	ADD  IY,IY		
	ADD  IY,IY	       ; Each entry is 4 bytes in size
	ADD  IY,DE	       ; Index
	LD   D,A	       ; Place Z back
	INC  HL 	       ; Proceed to the X coordinate
	LD   A,(CURDX)	       ; Find right edge by adding Dimension X
	ADD  A,B	
	SUB  (HL)	       ; Check to see if right edge is smaller than
	INC  HL 	       ; other objects left edge.
	JP   C,NOLAP1	       ; if so, there's no cause for concern.
	LD   A,(CURDY)	       ; Do the same trick with Y -
	ADD  A,C
	SUB  (HL)
	INC  HL
	JP   C,NOLAP1
	LD   A,(HL)	       ; and Z.
	ADD  A,(IY+2)	       ; But Z is different as the dimension for the
	SUB  D		       ; new 'test' object must be considered.
	JP   C,NOLAP1
HID1:	POP  HL 	       ; Now is the time for 2D check
	PUSH BC
	PUSH DE
	PUSH HL 	       ; save everything: 3D xyz and pointer.
	INC  HL
	LD   B,(HL)
	INC  HL
	LD   C,(HL)	       ; Get X and Y
	INC  HL
	LD   A,YPLUS
	SUB  B
	SUB  C
	SUB  (HL)	       ; Calculate X as above (2D)		
	LD   E,A
	LD   A,B
	SUB  C
	ADD  A,A
	ADD  A,XPLUS 
	LD   C,A	       ; Calculate Y as above (2D)
	LD   B,E
	INC  HL
	DB   PIX
	LD   A,H	       ; Jump if the edges are too far from eachother
	ADD  A,&1F	       ; First the upper edges,
	SUB  B
	JP   C,NOLAP		
	LD   A,B		
	ADD  A,&1F	       ; Then the lower edges,
	DB   PIX
	SUB  H
	JP   C,NOLAP	       ; - it's another story with the left and right
	LD   A,C	       ; edges, they can exceed the screen boundary.
	CP   &F0
	JP   NC,HIDE5	       ; jump to set flag if an object is far right,
	DB   PIX
	LD   A,L
	ADD  A,&1F	       ; else find right edge of the other object
	JP   NC,HIDE2	       ; jump if inside "normal" screen
	SUB  &10
	JP   NC,HIDE1	       ; else find out which edge it is on.
	LD   A,&EF	       ; if on right edge make it -1. 
HIDE1:	ADD  A,&10	       ; Calc back to normal.
HIDE2:	SUB  C		       ; and finally check with the left edge of the
	JP   C,NOLAP	       ; other object - jump if no overlap.
	JP   HIDE7
HIDE5:	LD   A,(FLAGS1)        ; enter here if at a very right spot	
	OR   &20	       ; to set the flag
	LD   (FLAGS1),A
HIDE7:	DB   PIX
	LD   A,L
	CP   &F0
	JP   NC,HIDE6	       ; do the same trick the other way around.
	LD   A,C
	ADD  A,&1F
	JP   NC,HIDE4
	SUB  &10
	JP   NC,HIDE3
	LD   A,&EF
HIDE3:	ADD  A,&10
HIDE4:	DB   PIX
	SUB  L
	JP   C,NOLAP
	JP   HIDE8
HIDE6:	LD   A,(FLAGS1)        ; Note:
	XOR   &20	       ;   if both edges are behind the normal screen
	LD   (FLAGS1),A        ;   boundary, they are both treated normally.
HIDE8:	EX   (SP),HL	       ; Get CURENT pointer, save updated pointer.
	LD   A,(HL)
	CP   &40
	JP   C,HIDEME
	LD   A,23
HIDEME: LD   HL,LAPPED
	INC  (HL)	       ; One more lapped.
	ADD  A,IMAGES/&100-1   ; Find address of shape in question
	LD   E,&80	       ; Start off with the Mask.
	LD   D,A
	PUSH BC 	       ; Save the 2D X and Y coords.
	LD   HL,WORKPG
	EX   DE,HL
	LD   C,L	       ; L is &00
	LD   B,E	       ; E is &80 - move 128 bytes.
	LDIR		       ; Copy mask to workpage's low half.
	POP  BC 	       ; Get back X,Y.
	DB   PIX
	LD   A,H
	SUB  B		       ; Find difference in Y coordinates.
	JP   C,DOWN
UP:	ADD  A,A
	ADD  A,A	       ; 4 bytes/line
	LD   E,A	       ; From here in source mask
	LD   L,&80	       ; From start of result mask
	LD   A,L
	SUB  E		       ; This is the number of bytes to consider.
	JP   VOYAGE
DOWN:	NEG
	ADD  A,A
	ADD  A,A
	OR   &80
	LD   L,A	       ; From here in result mask
	XOR  A
	LD   E,A	       ; From start of source mask
	SUB  L		       ; This is no. of bytes.
VOYAGE: LD   H,D	       ; D is WORKPG/&100 already.
	PUSH HL 	       ; Save DEST
	PUSH DE 	       ; Save SRCE
	PUSH AF 	       ; Save BYTS
	EX   DE,HL
	SRL  A
	SRL  A
	LD   B,A	       ; No. of lines to scroll
	DB   PIX
	LD   A,L	       ; Find difference between main and test object's
	SUB  C		       ; 2D X coordinates.
	LD   C,A
	JP   Z,SHARE	       ; Jump if none - The objects are on the same
	LD   A,(FLAGS1)        ; X position.
	BIT  5,A	       ; If object was beginning at the other side of
	RES  5,A	       ; the screen and automatically moved into the
	LD   (FLAGS1),A
	JP   Z,WITHIN	       ; right side, this bit is set. Jump if no.
	CCF		       ; Otherwise it must be rotated the other way.
WITHIN: JP   C,RIGHT	       ; Jump if right rotation is required.
LEFT:	LD   A,B
	ADD  A,A
	ADD  A,A
	DEC  A
	ADD  A,L	       ; Must go from end of section.
	LD   L,A
	LD   A,C
	EXX
	RRCA
	RRCA
	RRCA
	AND  3
	LD   C,A	       ; This is the no. of bytes to rotate (if any).
	EXX
	LD   A,C
	AND  7
	LD   C,A	       ; This is the no. of pixels to rotate (if any).
SCRLL1: PUSH BC
	LD   A,C
	LD   D,(HL)
	DEC  L
	LD   E,(HL)
	DEC  L
	LD   B,(HL)
	DEC  L
	LD   C,(HL)
	INC  L
	INC  L		       ; Grab all four bytes in a row,
	INC  L
	OR   A
	JP   Z,NOPIXL	       ; Jump if on even pixel,
SCRLL2: DB   &CB,&32	       ; (Logical Shift Left D)
	RL   E
	RL   B
	RL   C
	DEC  A
	JP   NZ,SCRLL2	       ; Rotate to pixel pos. required.
NOPIXL: EXX
	LD   A,C
	EXX
	OR   A
	JP   Z,SCRLL4	       ; Jump if on byte pos. zero.
SCRLL3: LD   C,B
	LD   B,E
	LD   E,D
	LD   D,&FF
	DEC  A
	JP   NZ,SCRLL3	       ; Rotate to byte pos. required.
SCRLL4: LD   (HL),D
	DEC  L
	LD   (HL),E
	DEC  L
	LD   (HL),B
	DEC  L
	LD   (HL),C
	DEC  L		       ; Insert the four bytes again.
	POP  BC
	DJNZ SCRLL1
	JP   SHARE	       ; and go on.
RIGHT:	LD   A,C	       ; For right rotation, make scrolls positive and
	NEG		       ; enter loop:
	LD   C,A
	EXX
	RRCA
	RRCA
	RRCA
	AND  3
	LD   C,A	       ; No. of bytes to rotate,
	EXX
	LD   A,C
	AND  7
	LD   C,A	       ; No. of pixels,
SCRLR1: PUSH BC
	LD   A,C
	LD   C,(HL)
	INC  L
	LD   B,(HL)
	INC  L
	LD   E,(HL)
	INC  L
	LD   D,(HL)
	DEC  L
	DEC  L
	DEC  L		       ; Grab contents,
	OR   A
	JP   Z,NOPIXR	       ; No. pixel scroll,
SCRLR2: SCF
	RR   C
	RR   B
	RR   E
	RR   D
	DEC  A
	JP   NZ,SCRLR2	       ; Scroll'em.
NOPIXR: EXX
	LD   A,C
	EXX
	OR   A
	JP   Z,SCRLR4	       ; No. byte scroll,
SCRLR3: LD   D,E
	LD   E,B
	LD   B,C
	LD   C,&FF
	DEC  A
	JP   NZ,SCRLR3	       ; Scroll'em.
SCRLR4: LD   (HL),C
	INC  L
	LD   (HL),B
	INC  L
	LD   (HL),E
	INC  L
	LD   (HL),D
	INC  L		       ; Back in there.
	POP  BC
	DJNZ SCRLR1
SHARE:	POP  BC 	       ; B=No. of bytes to do.
	POP  DE 	       ; DE=start position in target mask,
	POP  HL 	       ; HL=start position in result mask,
HLOOP2: LD   A,(DE)	       ; get result after rotation,
	AND  (HL)	       ; remove from result mask mask.
	LD   (HL),A
	INC  E
	INC  L		       ; Next byte,
	DJNZ HLOOP2
	POP  HL 	       ; HL=pointer to CURENT
	POP  DE 	       ; B=X,C=Y,D=Z coords of test.
	POP  BC 	       ; Get back pointer and 3D xyz.
	JP   HLOOP1
NOLAP:	POP  DE 	       ; Get back 3D coords.
	POP  DE
	POP  BC
	JP   HLOOP1
NOLAP1: POP  HL 	       ; Get back pointer - XYZ are unaffected.
NOLAP3: INC  HL
	INC  HL
	INC  HL
	INC  HL 	       ; next entry.
	JP   HLOOP1
NOLAP2: AND  &7F	       ; Remove bit identifying current object.
	CP   &7F
	JP   Z,NOLAP3	       ; But if "inactive", don't reset the bit.
	LD   (HL),A	       ; Only if it is a control "current" bit.
	JP   NOLAP3
;
TITLE	'IMAGE2a: Keyboard decoding'
EJECT
;
; KEYBOARD READ ROUTINES - IX is DF address, B is PFLAGS, A is PIXEL
;					     C is limit on char input
;
; Returns E=chars in buffer EXCLUSIVE the <cr>
; 
KINPUT: LD   (PIXEL),A	       ; A is pixel
	LD   A,B
	LD   (PFLAGS),A        ; B is flags
	LD   A,C
	LD   (CLIM),A
	LD   HL,WORKPG+&80     ; Insert characters here
	LD   D,H	       ; Insert widths here
NOGOOD: LD   E,0
GETONE: EXX
	CALL GETKEY	       ; Get a character
	EXX
	LD   C,A	       ; Insert the character
	LD   A,&FE
	IN   A,(KBPORT)
	RRA
	LD   A,C
	JR   C,NOTA1	       ; nope, use normal lower-case
	CP   &30	       ; Perhaps a delete? ('0')
	JR   NZ,NOTAA	       ; nope, use Upper-case!
	DEC  E		       ; Previous character
	JP   M,NOGOOD	       ; No characters,
	LD   A,(DE)	       ; Right then, Delete the last character
	LD   C,A
	LD   A,(PIXEL)
	SUB  C		       ; Last pixel pos.
	JR   NC,SAMBYT
	DB   PIX	       ; Dec. byte if overflow
	DEC  L
SAMBYT: AND  7
	LD   (PIXEL),A	       ; Back
	PUSH IX
	PUSH AF 	       ; Save
	DEC  HL
	LD   A,(HL)
	EXX
	CALL VDU	       ; Remove it (xor)
	EXX
	POP  AF
	POP  IX 	       ; Reget
	LD   (PIXEL),A
	CALL GETOFF
	JR   GETONE
NOTA1:	CP   &41	       ; An 'a'
	JR   C,NOTAA
	ADD  A,&20
NOTAA:	EX   AF,AF"
	LD   A,(CLIM)
	CP   E
	JR   C,BUFULL
	EX   AF,AF"
	LD   (HL),A	       ; Store char in memory.
	CP   32
	EXX		       ; Proper character...
	CALL NC,VDU	       ; Print it if not a CR or shift.
	EXX
	LD   (DE),A	       ; Width in pixels.
	LD   A,(HL)	       ; Get char again
	INC  HL
	CP   13 	       ; Back if CR
	RET  Z
	INC  E
CTRCHR: CALL GETOFF
	JR   GETONE
BUFULL: EX   AF,AF"
	CP   13
	RET  Z
	JR   GETONE
GETOFF: HALT
	EXX
	CALL INKEY
	EXX
	RET  NC
	JR   GETOFF
;
; Get a key from keyboard in Mazcii format. GETKEY waits and INKEY doesn't.
;
GETKEY: CALL INKEY	       ; REPEAT  (OSRDCH)
	JR   NC,GETKEY
	RET
;
INKEY:	LD   HL,KEYTAB-1       ; Mascii values
	LD   DE,5
	LD   C,&7F	       ; 1st half row
GLOOP1: LD   A,C
	IN   A,(KBPORT)        ; Get half row
	CPL
	AND  &1F
	LD   B,A
	JR   Z,NOKEY	       ; No keys pressed
	PUSH HL
GLOOP2: INC  HL 	       ; Find key within row
	RRA
	JR   NC,GLOOP2	       ; Wow - No key if carry
	LD   A,(HL)	       ; Get mazcii value from table
	POP  HL
	CP   31
	JR   Z,NOKEY
	CP   30
	SCF
	RET  NZ 	       ; Back with CARRY if valid key
NOKEY:	ADD  HL,DE
	RRC  C		       ; Next row on keyboard (carry=1)
	JR   C,GLOOP1
	RET		       ; Go back with NO CARRY if no key pressed
;
CHKEMP: LD  BC,0	       ; Check for kempston joystick present
CHKEM1: IN  A,(31)
	OR  C
	LD  C,A
	DJNZ CHKEM1
	AND  &1F
	RET		       ; Returns zero flag set if kempston	
;
FLOOR:	PUSH HL
	POP  IX
	CALL BXLOOP
	PUSH IX
	POP  HL
	INC  HL
	RET
BXLOOP: LD   A,(IX+0)
	OR   A
	RET  Z
	LD   L,A
	LD   H,0
	LD   DE,BADTAB-2       ; Box address
	ADD  HL,HL
	ADD  HL,DE
	LD   E,(HL)
	INC  HL
	LD   D,(HL)	       ; Address of actual routine to call
	LD   HL,ANOTER	       ; Address of return routine
	PUSH HL
	PUSH DE 	       ; Save'em so they can be RETurned to
	LD   B,A
	LD   E,(IX+1)	       ; X and Y
	LD   D,(IX+2)
	LD   L,D
	LD   H,0
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL	       ; HL is line down display
	LD   A,H
	ADD  A,PSEUDO/&100     ; And is now real left-hand address
	LD   H,A
	LD   A,E
	RRCA
	RRCA
	RRCA
	AND  &1F	       ; Rest of line
	ADD  A,L	       ; Now HL is actual pseudo display file address
	LD   L,A
	LD   A,E	       ; Pixel number here
	AND  7
	LD   C,A	       ; C=PIXEL
	LD   D,(IX+5)	       ; D=MASK
	LD   E,(IX+4)	       ; E=Y DIMENSION
	LD   B,(IX+3)	       ; B=X DIMENSION
	RET		       ; "Call" desired box-drawing routine
;
ANOTER: LD   DE,6	       ; (return here) - Go to next entry in 
	ADD  IX,DE	       ; the BOX table and go to main loop.
	JR   BXLOOP
;
BOXA:	CALL LINEX
	DEC  E
	JR   NZ,BOXA
	RET
;
BOXB:	LD   B,1	       ; Start off from 2 pixels
BOXB1:	CALL LINEX
	INC  B
	INC  B
	DEC  E
	JR   NZ,BOXB1
	RET
;
BOXC:	CALL LINEX
	DEC  B
	DEC  B
	LD   A,C
	ADD  A,2
	CP   8
	JR   C,BOXC1
	AND  7
	INC  L
BOXC1:	LD   C,A
	DEC  E
	JR   NZ,BOXC
	RET
;
BOXD:	LD   A,B
	RRCA
	RRCA
	RRCA
	AND  &1F
	ADD  A,L		  ; START FROM OTHER END
	LD   L,A
	LD   B,2
BOXD1:	CALL LINEX
	INC  B
	INC  B
	LD   A,C
	SUB  2
	JR   NC,BOXD2
	AND  7
	DEC  L			  ; BACKWARDS !
BOXD2:	LD   C,A
	DEC  E
	JR   NZ,BOXD1
	RET
;
BOXE:	CALL LINEX
	DEC  B
	DEC  B
	DEC  E
	JR   NZ,BOXE
	RET
;
BOXF:	CALL LINEX
	LD   A,C
	ADD  A,2
	CP   8
	JR   C,BOXF1
	AND  7
	INC  L
BOXF1:	LD   C,A
	DEC  E
	JR   NZ,BOXF
	RET
;
BOXG:	CALL LINEX
	LD   A,C
	SUB  2
	JR   NC,BOXG1
	AND  7
	DEC  L
BOXG1:	LD   C,A
	DEC  E
	JR   NZ,BOXG
	RET
;
LINEX:	PUSH HL
	PUSH BC
	CALL LINE
	POP  BC
	POP  HL
	LD   A,D
	CP   &FF
	JR   Z,LINEX1
	CPL
	LD   D,A
LINEX1: JP   NXTROW
;
LINE:	LD   A,B		  ; B is width of line, C is current pixel
	ADD  A,C
	CP   8
	JR   C,SINGLE
	CALL FILL		  ; all within same byte
	AND  D
	OR   (HL)
	LD   (HL),A
	INC  L
	LD   A,B
	LD   C,A
	RRCA
	RRCA
	RRCA
	AND  &1F
	JR   Z,LINE2
	LD   B,A
	LD   A,D
LINE1:	LD   (HL),A
	INC  L
	DJNZ LINE1
LINE2:	LD   A,C
	AND  7
	RET  Z
	LD   B,A
	XOR  A
LINE3:	SCF
	RRA
	DJNZ LINE3
	AND  D
	OR   (HL)
	LD   (HL),A
	RET
SINGLE: INC  B
	CALL FILL
	JR   Z,SNG2
SNG1:	RLCA
	DEC  C
	JR   NZ,SNG1
SNG2:	AND  D
	OR   (HL)
	LD   (HL),A
	RET
FILL:	LD   A,8
	SUB  C
	LD   C,A
	XOR  A
FILL1:	SCF
	RLA
	DEC  C
	RET  Z
	DJNZ FILL1
	RET
;
LINER:	       ; Line Rise ( / )
LINEF:	       ; Line Fall ( \ )
LINEH:	       ; Horisontal line ( - )
LINEV:	       ; Vertical line ( | )
;
;	SPECIAL COMPUTED OBJECTS
;
; These routines are called from STATIC and have on entry HL=DF ADDR of the
; three XYZ coords associated with the special object.
;
OUTLNE: LD   B,&20	       ; Door outlining.
	LD   D,&00	       ; This is the value. Can only be zero!
	LD   E,32-3	       ; This is what is left after two increments
OLINE2: LD   (HL),D	       ; Mask out a 4x4 character square on pDF
	INC  L		       ; Next byte
	LD   (HL),D	       ; second byte
	INC  L		       ; Next byte
	LD   (HL),D	       ; third byte
	INC  L		       ; Next byte
	LD   (HL),D	       ; fourth byte. This is actually redundant.
	ADD  HL,DE	       ; go to next pixel row. D is 0
	DJNZ OLINE2	       ; Do that 32 times
	RET		       ; and back
;
OUTLFT: LD   DE,-&005C	       ; Mask out as behind doorpole to the left
	ADD  HL,DE	       ; This is the offset to make 0,0,0 easy to use
	LD   C,3	       ; three columns
OUTLF1: PUSH HL 	       ; Save Hl
	LD   DE,OLDATA	       ; This is the left-hand side masks
	CALL MSKROW	       ; mask out one column
	POP  HL 	       ; Rehi HL
	LD   DE,-&7F	       ; Go up 4 pixel lines and right one byte
	ADD  HL,DE	       ; But, says man, the babelfish is a dead give-
	DEC  C		       ; away, isn't it? It proves you exist, and
	JR   NZ,OUTLF1	       ; therefore, by your own arguments, you don't.
	RET		       ; QED.
;
OUTRGT: LD   DE,-&0180	       ; Right-hand side. This is the offset
	ADD  HL,DE	       ; do offset
	LD   C,3	       ; three columns to mask out
OUTRT1: PUSH HL 	       ; Save hl to save decrementing it
	LD   DE,ORDATA	       ; mask from here
	CALL MSKROW	       ; do a column
	POP  HL 	       ; back again. Hi hl.
	LD   DE,&81	       ; Go down 4 pixel lines and right one byte
	ADD  HL,DE	       ; This really is a nice display file
	DEC  C		       ; three cols
	JR   NZ,OUTRT1	       ; Jump if Z flag is not set.
	RET		       ; and back
;
ELEVU:	LD   HL,ELEVAT	       ; Use data from elevator graphics
	LD   DE,PSEUDO+&207
	JR   ELEV2	       ; Bypass the index initialisation for ELEVD
;
ELEVD:	LD   HL,ELEVAT+(3*28)  ; Door down
	LD   DE,PSEUDO+&217 
ELEV2:	LD   (TEMPHL),DE       ; Store this for the outside
	LD   A,(ELECNT)        ; This is the number of bytes to show off
	LD   C,A	       ; Store here for counter
	NEG		       ; This is what's left to print in zeros
	JR   Z,ELEV7	       ; But dont't draw anything if ELECNT was zero
	PUSH AF 	       ; else save that number (-ELECNT)
	ADD  A,3
	JR   Z,GELEV5
	PUSH BC
	LD   BC,28
GLSNST: ADD  HL,BC	       ; Glasnost
	DEC  A
	JR   NZ,GLSNST
	POP  BC
GELEV5: CALL ELEV5	       ; and draw a column
	POP  AF 	       ; retrieve -ELECNT
ELEV7:	ADD  A,3	       ; find remainder of space columns to draw
	RET  Z		       ; return now if that was zero (ie. ELECNT=3)
	LD   H,C	       ; Make H=0 so below code understands "no data"
	LD   C,A	       ; This is then the remaining columns
ELEV5:	PUSH DE 	       ; Save screen address
	LD   B,28	       ; twenty-eight rows to fill in
ELEV3:	LD   A,H	       ; Get high byte of data
	OR   A		       ; if that was zero, there IS no data, therefore
	JR   Z,ELEV6	       ; keep the zero and use that as data.
	LD   A,(HL)	       ; Else get data from the data address
	INC  HL 	       ; and forward to next piece of data (if H<>0)
ELEV6:	LD   (DE),A	       ; Insert into (pseudo) screen memory
	LD   A,E	       ; Calculate next row in (pseudo) display file
	ADD  A,&20	       ; by adding one line (32 bytes) to the address
	LD   E,A	       ; and incrementing the high-byte only if
	JR   NC,ELEV4	       ; an overflow occured here.
	INC  D		       ; Like this.
ELEV4:	DJNZ ELEV3	       ; Do this 28 times
	POP  DE 	       ; Get back display file address
	INC  E		       ; One Byte Right
	DEC  C		       ; decrement counter
	JR   NZ,ELEV5	       ; and loop the loop if NZ (here we go again)
	RET		       ; else back (to ELEV first time round!)
;
; Draw a 2x2 char object static on background. Very bad, but it worx!
;
FLAT:	LD   A,(INPRNT)        ; Grab actual fake image to print
	SUB  93 	       ; Calc real binary offset
	CP   2
	JR   C,TALF1
	LD   (CAMDF2),HL
	JR   OLOMA
TALF1:	LD   (CAMDF1),HL
OLOMA:	EX   DE,HL	       ; Call OLOMA with HL=pdf addr, A=object
OLOMAX: ADD  A,A	       ; Mul. 32 ('cos each 'flat' obj is 32 bytes)
	ADD  A,A	       ; Do that
	ADD  A,A	       ; com'on get finished
	ADD  A,A	       ; thankyou
	ADD  A,A	       ; Any further adding must be done in 16 bit
	LD   L,A	       ; Make that 16 bit
	LD   H,0	       ; Zero high byte
	LD   BC,CAMERA	       ; Offset to camera character data (or what
	ADD  HL,BC	       ; it might be). Add offset to calc'd constant
	LD   C,2
FLAT1:	LD   B,16	       ; Draw a 16x16 pixel square from a downwards
	PUSH DE 	       ; linear buffer in memory (ie, 16 bytes for
FLAT2:	LD   A,(HL)	       ; first column, 16 bytes for next)
	LD   (DE),A
	INC  HL 	       ; Do one, inc counter
	LD   A,E
	ADD  A,&20	       ; Go one line down
	LD   E,A
	JR   NC,FLAT3
	INC  D		       ; Inc hibyte only if nescessary
FLAT3:	DJNZ FLAT2	       ; 16 bytes
	POP  DE
	INC  E		       ; Get dfaddr back, assume no wraparound so
	DEC  C		       ; inc to next row and check C for last row
	JR   NZ,FLAT1	       ; and do that if not so.
	RET		       ; That about wraps it up.
;
MSKROW: CALL MSKRW2	       ; Mask off top (used by outlining SpcObj's)
	LD   B,26	       ; Go 4 26 lines, that is 32-3*2 (for top & bot)
MSKRW1: LD   (HL),0	       ; zro out
	CALL NXTROW	       ; a pxl-lne dwn
	DJNZ MSKRW1	       ; zro all 26 out
MSKRW2: LD   B,3	       ; cont into mskrow 2 msk out bottom. The
MSKRW3: LD   A,(DE)	       ; pntr 2 the msk dta has alrdy been
	AND  (HL)	       ; incrmntd last time the rtne was cld.
	LD   (HL),A	       ; Get from msk dta, msk out frm scrn and
	INC  DE 	       ; sto bck on scrn. Then inc 2 nxt piece
	CALL NXTROW	       ; of msk dta and nxt row in dspfle
	DJNZ MSKRW3	       ; do three of those.
	RET		       ; and back. This is a locally used sbrtne.
;
DSCORE: LD   BC,-10000	       ; and DE = address in memory where ascii is to
	CALL SCORE1	       ; be put.
	LD   BC,-1000
	CALL SCORE1	       ; DSCORE with HL=no, DE=add to put it in mem
	LD   BC,-100	       ; as 5*ascii
	CALL SCORE1
	LD   BC,-10
	CALL SCORE1
	LD   A,L
	ADD  A,&30
	LD   (DE),A
	RET
CSCORE: LD   (SCTXT),DE       ; Call with DE=df addr to print
	LD   DE,SCTXT+4
	CALL DSCORE
PSCORE: LD   HL,(SCTXT)
	XOR  A
	LD   C,8
	LD   D,0
SCORL1: LD   B,5
	LD   A,L
SCORL2: LD   (HL),D
	INC  L
	DJNZ SCORL2
	LD   L,A
	CALL NXTROW
	DEC  C
	JR   NZ,SCORL1
	LD   HL,SCTXT
	JP   PRINT
SCORE1: LD   A,&FF
SCORE2: INC  A
	ADD  HL,BC
	JR   C,SCORE2
	SBC  HL,BC
	ADD  A,&30
	LD   (DE),A
	INC  DE
	RET
;
TITLE	'IMAGE3: Update sequences'
EJECT
;
;	MAIN UPDATE INITIALIZING AND UPDATE WRAP LOOP:
;	Update pointers, print all on pseudo screen and move it down before
;	entering the main loop. The dynamic objects are printed BEFORE
;	moving down so they are present upon display, thereby a flicker
;	is avoided.
;
UDINIT: CALL UDPTRS	       ; Initial update of all sprites
	CALL UDSETP	       ; Print them all before displaying the screen
	LD   A,(MOVED)
	OR   A
	JR   NZ,UPXYZ2
	CALL MOVESC	       ; THE MAIN LOOP:
	JR   UPXYZ1
UPXYZ2: CALL MOVEB0
UPXYZ1: LD   (HIWATR),SP       ; Bllrrpp!
	LD   A,&FF
	LD   (MOVED),A
UPLOOP: CALL UPDATE	       ; Update the sprites.
	LD   HL,LOOP
	INC  (HL)
	CALL BRK
	JR   C,UPLOOP
	JP   ABANDN
;
UDSETP: LD   HL,UPDTAB	       ; Prevent any accidents first time.
	LD   (UPDPTR),HL       ; These are in fact not used first time.
	XOR  A
	LD   (NO2UPD),A
	LD   A,(NODYNA)        ; Initially draw dynamic objects on the screen
	OR   A		       ; BEFORE display, so they are already there.
	RET  Z		       ; No dynamic objects This is very unlikely.
	LD   B,A
	LD   HL,FLAGS2
	SET  7,(HL)	       ; "Second (or more) time calling cycle"
	LD   C,0
TLOOP1: INC  C
	LD   A,C
	SCF		       ; Set for PLOT
	PUSH BC
	CALL DYNAMC	       ; and print it
	POP  BC
	DJNZ TLOOP1	       ; do them all.
	LD   HL,FLAGS2
	RES  7,(HL)
	RET
;
UPDATE: LD   A,(NODYNA)        ; No sprites - forget it
	OR   A
	RET  Z
	LD   B,A	       ; A is still NODYNA
PLOOP0: LD   A,B	       ; get sprite number,
	AND  A		       ; signal "Unplot mode"
	PUSH BC
	CALL DYNAMC	       ; Erase it
	POP  BC 	       ; get'em back.
	DJNZ PLOOP0	       ; repeat.
	CALL UDPTRS	       ; Update pointers while screen is blank.
	CALL EDOORS	       ; Elevator doors
	CALL RODS
	LD   A,R
	AND  &1F
	CP   2
	JR   C,DOC1
	CP   4
	JR   NC,NDOC1
DOC2:	LD   DE,(CAMDF2)
	JR   UTWAT
DOC1:	LD   DE,(CAMDF1)
UTWAT:	LD   B,A
	LD   A,E
	OR   D
	JR   Z,NDOC1
	LD   HL,(UPDPTR)
	LD   (HL),E
	INC  HL
	LD   (HL),D
	INC  HL
	LD   (UPDPTR),HL
	LD   HL,NO2UPD
	INC  (HL)
	LD   A,B
	CALL OLOMAX
NDOC1:	LD   A,(NODYNA)
	LD   B,A
	LD   C,0
PLOOP5: INC  C
	LD   A,C	       ; Get sprite number
	SCF		       ; signal "Plot mode"
	PUSH BC
	CALL DYNAMC	       ; Print it up.
	POP  BC
	DJNZ PLOOP5	       ; Do it.
	LD   HL,FLAGS2
	AND  A		       ; Reset carry (no attr clear if screen is moved)
	BIT  1,(HL)
	RES  1,(HL)	       ; Move screen down if something had disturbed
	CALL NZ,MOVESC	       ; it. (fx. pause bar)
	LD   HL,NO2UPD	       ; No of objs to update!
	LD   A,(HL)
	LD   (HL),0
	LD   HL,UPDTAB	       ; Now when all has been updated on the shadow
	LD   (UPDPTR),HL       ; screen, the display file must be updated.
	OR   A
	JP   Z,ANDREA
	LD   B,A
PLOOP1: PUSH BC 	       ; Save counter
	LD   E,(HL)
	INC  HL
	LD   D,(HL)	       ; Fetch an address in DE from UPDTAB.
	INC  HL
	PUSH HL 	       ; Get off
	LD   L,E
	LD   H,D
	ADD  HL,HL	       ; This is actually the Y coordinate * &20
	ADD  HL,HL	       ; So.. *8 (sum.256) makes H = Y.
	ADD  HL,HL	       ; Fetch Y coordinate (tricky)
	LD   L,H
	LD   H,0	       ; Now..
	ADD  HL,HL
	LD   BC,SCRTAB
	ADD  HL,BC	       ; Point to display file address of 1st row
	LD   A,E
	AND  &1F
	LD   C,A
	LD   B,&20
	DEC  DE
	DEC  DE
	CP   2		       ; LEFT HAND? (A IS X COORDINATE/8)
	JR   C,LHEDGE
	CP   &1E
	JR   C,PLOOP2
RHEDGE: JR   Z,RHEDG2
	DEC  DE
	DEC  C
RHEDG2: DEC  DE
	DEC  C
	JR   PLOOP2
LHEDGE: INC  DE
	INC  C
	OR   A
	JR   NZ,PLOOP2
	INC  C
PLOOP2: PUSH BC 	       ; Move a row
	LD   A,(HL)
	ADD  A,C
	LD   C,A
	INC  HL
	LD   B,(HL)
	DEC  BC
	DEC  BC
	INC  HL
	LD   A,(DE)
	LD   (BC),A
	INC  DE
	INC  BC
	LD   A,(DE)
	LD   (BC),A
	INC  DE
	INC  BC
	LD   A,(DE)
	LD   (BC),A
	INC  DE
	INC  BC
	LD   A,(DE)
	LD   (BC),A
	INC  DE
	INC  BC
	LD   A,(DE)
	LD   (BC),A
	LD   A,E
	ADD  A,28	       ; Rest of line
	JP   NC,PLUCK2
	INC  D
PLUCK2: LD   E,A
	EX   AF,AF"            ; Do All 32 rows, then
	POP  BC
	DJNZ PLOOP2
	POP  HL 	       ; Get Updates Table pointer and
	POP  BC 	       ; Counters back and
	DJNZ PLOOP1	       ; Do All sprites.
ANDREA: LD   A,(FUEL)
	LD   B,&E4
	LD   H,&54-1
	CALL BAR
	LD   A,(UREM)
	LD   B,&C4
	LD   H,&54-1
	CALL BAR
	LD   HL,FLAGS3	      ; Does rod/jet need rising because of early in 
	BIT  5,(HL)	      ; game?
	JR   NZ,FULSWI
	LD   HL,UREM3
	INC  (HL)
	LD   A,(HL)
	LD   (UREM),A
	LD   (FUEL),A
	CP   &3F
	JR   C,FULSWI
	LD   HL,FLAGS3
	SET  5,(HL)
FULSWI: LD   A,(UREM)
	OR   A
	JP   Z,GAMOVR	      ; BLE.S GAMOVR !
	JP   M,GAMOVR
	LD   HL,FLAGS4
	BIT  2,(HL)
	JR   Z,IKKR7
	LD   HL,IMAGES+(GRAIN-DIMTAB)*&40+&FF
	LD   BC,8
	LD   DE,WORKPG+7
	LDDR
	LD   D,H
	LD   E,&FF
	LD   L,&F7
	LD   C,&78
	LDDR
	LD   HL,WORKPG
	LD   C,8
	LD   E,&80
	LDIR
IKKR7:	LD   A,(UREM)
	CP   8
	JR   NC,IKKR8
	LD   A,(LOOP)
	RRA
	RRA
	LD   A,66	       ; br.red
	JR   C,IKKR9
IKKR8:	LD   A,71	       ; br.white
IKKR9:	LD   (&5AC0),A
	LD   (&5AC1),A
	LD   (&5AC2),A
	LD   A,(TIMER+1)
	DEC  A
	JP   M,TIMCR1
	JR   Z,TIMYEL
	DEC  A
	JR   NZ,TIMNCR
	LD   A,GR
	JR   TIMCR2
TIMYEL: LD   A,YE
	JR   TIMCR2
TIMCR1: LD   A,(LOOP)
	RRA
	RRA
	LD   A,66
	JR   C,TIMCR2
	LD   A,WH
TIMCR2: LD   (&5AEC),A
	LD   (&5AED),A
	LD   (&5AEE),A
	LD   (&5AEF),A
	LD   (&5AF0),A
TIMNCR: LD   HL,FLAGS4
	BIT  3,(HL)
	JR   NZ,ENEMYP
	LD   A,R
	OR   A
	JR   NZ,ENEMYP
	SET  4,(HL)
ENEMYP: 

IF DEBUG=1     ; THIS WILL PRINT UP CO-ORDS OF LAST PROCESSED SPRITE!
 LD HL,&4000
 LD C,24
 MZ1: LD B,4
 MZ2: LD (HL),0
 INC L
 DJNZ MZ2
 DEC L
 DEC L
 DEC L
 DEC L
 CALL NXTROW
 DEC C
 JR NZ,MZ1
 LD A,(OLDX)
 LD HL,MZX
 CALL HEXA
 LD A,(OLDY)
 LD HL,MZY
 CALL HEXA
 LD HL,MZP
 CALL PRINT
 JP QRT77OF
 HEXA: LD B,A
 RRA
 RRA
 RRA
 RRA
 CALL HEXA2
 LD A,B
 HEXA2: AND &F
 CP 10
 JR C,HEXA3
 ADD A,7
 HEXA3: ADD A,48
 LD (HL),A
 INC HL
 RET
 MZP: DW &4000
 DB 0,0
 MZX: DB 0,0,255
 DW &4120
 DB 0,0
 MZY: DB 0,0,0
 QRT77OF:
ENDIF

	RET
;
BAR:	LD   C,A
	RRA
	RRA
	RRA
	AND  7
	ADD  A,B
	LD   L,A
	LD   (HL),0
	INC  H
	LD   (HL),0
	INC  H
	LD   (HL),0
	DEC  L
	LD   A,C
	AND  7
	INC  A
	LD   B,A
FUELOP: SCF
	RRA
	DJNZ FUELOP
	AND  254
	LD   (HL),A
	DEC  H
	LD   (HL),A
	DEC  H
	LD   (HL),A
	RET
;
EDOORS: LD   A,(ELESTA)        ; Elevator status
	DEC  A
	RET  M		       ; No elevator action if negative
	JP   Z,ELOPXX	       ; 1=opening door, but check for pflame!
	DEC  A
	RET  Z		       ; 2=walking-in man
	DEC  A
	JR   Z,ECLOSE	       ; 3=Closing door
	DEC  A
	JR   Z,ELOPEN	       ; 4=opening door
	DEC  A
	RET  Z		       ; 5=Walking-out man
ECLOSE: LD   A,(ELECNT)        ; 6 (3) =closing door
	INC  A
	CP   4		       ; check size of door, 4=too big, change
	JR   NZ,EDOOR3	       ; elevator action (wasn't there a game called..)
	LD   HL,ELESTA
	INC  (HL)	       ; OK, if that was closed, shut off elevators.
	CP   (HL)	       ; A is 4.
	JR   NZ,EOFF	       ; Jump if coming out.
	LD   HL,CFLOOR
	LD   A,(ELEVUD)
	NEG
	ADD  A,3
	LD   (ELEVUD),A        ; Rev elev type
	DEC  A
	JR   NZ,GOINUP
	DEC  (HL)
	JR   GOINDN
GOINUP: INC  (HL)
GOINDN: LD   A,3
	LD   (ELECNT),A
	LD   A,6
	LD   (ELECN2),A
	LD   HL,(WASXYZ)
	LD   A,(HL)
	CP   65
	JR   C,GOINSN
	LD   B,50+3
	LD   C,80+3
	JR   GOINXY
GOINSN: LD   B,82+3
	LD   C,48+3
GOINXY: LD   (HL),B
	INC  HL
	LD   (HL),C
	INC  HL
	LD   (HL),0
	LD   A,5
	LD   (CURGRL),A
	LD   A,(CURROM)
	LD   (NEWROM),A
	LD   HL,FLAGS3
	SET  0,(HL)
	SET  1,(HL)	       ; Floor change AND elevator action
	LD   HL,FLAGS4
	SET  0,(HL)
	RET
EOFF:	LD   (HL),0	       ; Stop whole thing
	RET
ELOPEN: LD   A,(ELECNT)        ; Opening door (1)
	DEC  A		       ; One less in door size
	JP   P,EDOOR3	       ; Positive means still opening, jump
EDOOR4: LD   HL,ELESTA	       ; else go to next elevator status
	INC  (HL)
	RET		       ; and back
EDOOR3: LD   (ELECNT),A
	LD   A,(ELEVUD)        ; Get which thingy is in action
	DEC  A
	JR   Z,DELEVU	       ; Well, it was the UP thingy
	CALL ELEVD	       ; or the DOWN thingy
	JR   DELEVD
DELEVU: CALL ELEVU	       ; Draw the UP arrow a bit
DELEVD: LD   HL,(UPDPTR)       ; Update it on screen
	LD   DE,(TEMPHL)
	LD   (HL),E
	INC  HL
	LD   (HL),D
	INC  HL
	LD   (UPDPTR),HL       ; Store new ptr
	LD   HL,NO2UPD
	INC  (HL)
ELWALK: RET
;
ELOPXX: LD   HL,JETFLG	       ; Make sure to put back his flame thingy 
	BIT  1,(HL)	       ; properly!
	CALL NZ,PPUTBK
	JR   ELOPEN
;
; Handle all rods which have fallen through a hole. This decrements the table
; of exited rods and does all related checking.
;
RODS:	LD   A,(EXRODS)        ; Any rods in room below?
	OR   A
	RET  Z		       ; Ret if not
	LD   B,A
	LD   HL,RODTAB	       ; Else start refreshing
RODS1:	LD   E,(HL)
	INC  HL
	LD   D,(HL)	       ; Address of the Z coordinate
	INC  HL
	LD   A,(DE)
	SUB  6
	JR   NC,RODS2	       ; Make it fall 6 and jump if OK,
	XOR  A		       ; but Zero it if it hit the floor
RODS2:	LD   (DE),A
	DJNZ RODS1
	RET
;
;	DYNAMIC OBJECTS UPDATE POINTERS AND VARIABLES
;
;	No entry or exit parameters.
;
;	The coordinates are refreshed (ie. added to the Velocity Vectors)
;	and a collision check is performed for all sprites. Then a GROL
;	server ( the motion controller ) is called to control the further
;	movement, or, if collision was detected, to take action upon that.
;	A GROL is entered with the zero flag reset if a collision occured.
;	Also, (LAPPED) contains the number of overlapped objects from HIDDEN.
;	IY points to the first User Byte of the object, IX points to the
;	Velocity Vector X. A GROL should only affect velocity vectors and 
;	User Bytes and should return with RET.
;
;	HOW TO REMOVE A DYNAMIC OBJECT IN A GROL ROUTINE
;	------------------------------------------------
;	Remove the entry in SPRTAB, and if it's the last in the list, ie.
;	if. (SPRCNT)=(NODYNA), stick a &00 on the 0th byte. 
;	The entry in CURENT should NOT be removed, 'cause that would really
;	fuck the sorting up. Rename the image (0th byte) to &FF, which 
;	reduces it to a Dormant Dynamic Object. Naturally, it will never
;	come back again, as its GROL will never more be executed, as it has
;	no sprite data.
;	(SPRCNT) must be decremented.
;	(NODYNA) must be decremented.
;	Also, always stick a &00 after SPRTAB as some routines may still use
;	this as a delimiter instead of using a counter.
;	This is all severe pestillence.
;
UDPTRS: LD   HL,SPRCNT	       ; Update Pointers. This is where all GROLs and
	LD   (HL),1	       ; ACTNs are called. Here is also taken care of
	LD   HL,SPRTAB	       ; the vectors and coordinates.
PLOOP4: LD   A,(HL)	       ; MAIN LOOP:
	AND  7		       ; Get byte from sprites table
	RET  Z		       ; but return if at the end.
	LD   (CURSPR),HL       ; Save address here for GROLs etc.
	ADD  A,7	       ; Test for DDO being removed from the table
	LD   B,0
	LD   C,A
	ADD  HL,BC
	BIT  1,(HL)	       ; Is it a DDO anyway?
	JP   NZ,DDOOBJ
	SBC  HL,BC
	SUB  7		       ; BC is no. of entries in the Animation
	LD   C,A	       ; Sequence Table.
	LD   A,(HL)
	RLCA
	RLCA
	AND  3		       ; Get the Direction Pointer to bit 0 and 1.
	LD   E,A	       ; Note: this may be kludged from GROLs.
	LD   A,(HL)
	RRA
	RRA
	RRA
	AND  7		       ; Get no. of physical images per direction
	JR   Z,PEXIT6	       ; incl. animations. Jump if none (not mirrored).
	LD   D,A
	XOR  A
PLOOP9: ADD  A,E	       ; Else multiply direction with no. of images
	DEC  D		       ; so as to find the logical image in question.
	JR   NZ,PLOOP9
PEXIT6: PUSH HL 	       ; Save pointer.
	INC  HL
	LD   E,(HL)	       ; Get index into Animation Seqence Table.
	LD   D,B	       ; B is 0
	PUSH HL
	ADD  HL,DE	       ; Index into table,
	ADD  A,(HL)	       ; and add logical image to base image to find
	LD   (PHYSIC),A        ; Current Physical Image
	LD   D,A	       ; a new logical image which goes into D.
	POP  HL 	       ; Get back pointer.
	LD   A,E	       ; get current index,
	CP   C		       ; and see if it exceeds the total.
	JR   C,PEXIT2	       ; Jump if all is OK.
	LD   (HL),B	       ; else re-initialise pointer to 1ST (B=0)
PEXIT2: INC  (HL)
	ADD  HL,BC	       ; Go past the animation sequence table.
	INC  HL
	PUSH HL 	       ; IX must point to XYZ Velocity Vectors when
	POP  IX 	       ; entering a GROL server.
	LD   C,(HL)
	INC  HL
	LD   B,(HL)
	INC  HL
	LD   E,(HL)	       ; CBE is now the XYZ Velocity Vectors
	INC  HL
	LD   A,(HL)	       ; Get low byte of entry in Current Room Table.
	INC  HL
	PUSH HL
	POP  IY 	       ; Transfer to IY to make it point to the
	INC  IY 	       ; first "User Byte" of that object. (for GROLs)
	LD   H,(HL)	       ; Get high byte of Current Room addr.
	LD   L,A	       ; Low was in A.
	LD   (HL),D	       ; Insert current logical image to print.
	INC  HL 	       ; Points to X,
	LD   (XYZADR),HL       ; save this for GROL servers.
	LD   A,(HL)
	LD   (OLDX),A	       ; Save old x,
	ADD  A,C	       ; add to X velocity vector,
	LD   (HL),A	       ; and put back.
	INC  HL
	LD   A,(HL)
	LD   (OLDY),A	       ; Save old y,
	ADD  A,B	       ; add to Y velocity vector,
	LD   (HL),A	       ; and put back.
	INC  HL
	LD   A,(HL)
	LD   (OLDZ),A	       ; Save old z,
	ADD  A,E	       ; add to Z velocity vector,
	LD   (HL),A	       ; and put back.
	POP  HL 	       ; Get address in sprites table back
	LD   DE,&F	       ; Advance to last byte in table,
	ADD  HL,DE
	LD   A,(HL)	       ; which is the GROL server.
	LD   (CURGRL),A        ; Save here so ACTNs know what to except!
	INC  HL 	       ; Next sprite address.
	OR   A		       ; See if GROL is unset,
	PUSH HL 	       ; Save this 'till the end.
	JR   Z,PBACK1	       ; Don't do antything if the GROL was zero.
	LD   E,A	       ; (D is 0)
	EX   DE,HL
	LD   DE,GADTAB-2       ; The offset.
	ADD  HL,HL	       ; Two bytes per entry.
	ADD  HL,DE
	LD   E,(HL)
	INC  HL
	LD   D,(HL)	       ; DE is address of GROL server.
	LD   HL,PBACK1	       ; HL is return address after a GROL.
	PUSH HL 	       ; Put return address on stack, simulating CALL.
	PUSH DE 	       ; Put call address on stack simulating CALL.
	LD   A,(SPRCNT)        ; Get sprite counter
	PUSH IX
	PUSH IY
	CALL COLISN	       ; Set flags according to collision status,
	POP  IY
	POP  IX
	EX   AF,AF"            ; Tempsave zero flag (=0 if collision),
	LD   HL,(SPRCNT)
	LD   H,0
	LD   DE,LAPTAB-1
	ADD  HL,DE	       ; Index into Lapped Object's Table
	LD   A,(HL)	       ; and place the number in LAPPED
	LD   (LAPPED),A        ; - if a GROL would like to use this...
	RES  5,(IY+0)	       ; Signal "Coming from GROL" to action servers.
	EX   AF,AF"            ; Get Collision Zero back, and
	RET		       ; call relevant GROL and return at PBACK1
DDOOBJ: NEG		       ; Calc next sprite !
	ADD  A,&10
	LD   C,A	       ; This is it. B should be 0 here (hopefully!)
DJRMOV: ADD  HL,BC
	PUSH HL 	       ; Save so it fits! This is RAW CODE!!
PBACK1: LD   HL,SPRCNT
	INC  (HL)	       ; Next sprite
	POP  HL 	       ; address
	JP   PLOOP4	       ; again.
;
; Remove a Dynamic Object. Dynamic object number held in (SPRCNT), address
; of sprite is in (CURSPR).
;
RMVOBJ: LD   HL,(CURSPR)       ; This is used to "pseudo"-remove an object
	LD   A,(HL)	       ; from the tables. It is actually not REALLY
	AND  7
	ADD  A,7	       ; removed, but it is made Redundant. This is
	LD   C,A	       ; all due to the programmer's extreme lazyness,
	LD   B,0	       ; 'cause he couldn't be bothered restoring all
	ADD  HL,BC	       ; those tables in the middle of a loop actually
	RES  3,(HL)	       ; using them! That was too much hastle!
	SET  1,(HL)
	DEC  HL
	LD   A,(HL)
	DEC  HL
	LD   L,(HL)
	LD   H,A
	LD   (HL),&FF
	RET
;
; Insert a Dynamic Object. The object to be inserted is assumed to be a
; rod. Since last ammendments to this code it can only (and i mean ONLY)
; be called from GROL or ACTN server level. otherwise the coordinate
; structure and program heap gets irecoverable corrupted.
;
; Also, A = Physical object on entry
;
INSOBJ: LD   E,5	 ; Assume ROD
	CP   1+(GROD-DIMTAB)/4
	JR   Z,ISOKRD	 ; Good assumpsion
	LD   E,30
ISOKRD: LD   (INPRNT),A
	LD   D,A
	LD   A,E
	LD   (ZLEVEL),A
	LD   HL,(XYZADR) ; Grab pointer to main chr's X
	DEC  HL 	; back to image. The main char is used as a kind of
	LD   C,(HL)	; a stand-in for the new object to be born
	PUSH BC 	; Save original image on stack
	LD   (HL),D	; Physical image of a rod
	INC  HL
	LD   C,(HL)	; Grab X and Y. C is X and B is Y
	INC  HL
	LD   B,(HL)
	INC  HL
	LD   A,(HL)	; Z
	LD   (HOLDZ),A
	ADD  A,E	; + 5 OR 30
	LD   (HL),A
	DEC  HL
	PUSH BC 	; Save orginal X and Y on stack
	PUSH HL 	; Save pointer to Y
	INC  B
	INC  C
	LD   A,(MANFLG) ; Current direction
	AND  7
	ADD  A,A	; *2 for x/y vector
	INC  A		; +1, we want Y first
	LD   L,A	; form 16-bit index
	LD   H,0
	LD   DE,VECTAB	; This is the 16-bit offset to the vectors
	ADD  HL,DE	; HL is now address of Y vector in question
	POP  DE 	; get pointer to CURENT back. It points to Y
	LD   A,(HL)	; get Y vector
	ADD  A,A
	ADD  A,A	; multiply by 4
	ADD  A,B	; plus middle of man at Y
	LD   (DE),A	; into Y coord
	DEC  HL 	; get X autovector
	DEC  DE 	; decrease to point to X
	LD   A,(HL)	; Same for X
	ADD  A,A
	ADD  A,A
	ADD  A,C	; plus X coord
	LD   (DE),A	; store in X. Keep Z as it is...
	DEC  DE 	; points to image
	PUSH DE
	LD   A,(SPRCNT)
	CALL COLISN	; Check the collision
	POP  HL 	; points to img
	POP  BC 	; original x,y's
	POP  DE 	; E is original image
	LD   (HL),E	; old image back in
	INC  HL
	LD   E,(HL)	; old X back in, put new in e
	LD   (HL),C
	INC  HL
	LD   D,(HL)	; old Y back, put new in d
	LD   (HL),B
	INC  HL
	LD   A,(HOLDZ)
	LD   (HL),A
	EX   DE,HL	; now L=x, H=y, A=z coords if no collision.
	RET  NZ 	; if coll., just go back now.
	DI		  ; No interrupts when messing with the tables
	LD   (RODPOS),HL  ; store x,y
	LD   (RODPOS+2),A ; ,z
	LD   A,(ZLEVEL)
	CP   10
	JR   NC,USALAB
	LD   HL,FLAGS3
	RES  2,(HL)	; NOW we can seriously say, We Have No Rod.
USALAB: LD   HL,SPRTAB
	LD   C,1
INSL1:	LD   A,(HL)
	AND  7
	JR   Z,INSNDO
	ADD  A,5
	LD   E,A
	LD   D,0
	PUSH HL
	ADD  HL,DE
	LD   E,(HL)
	INC  HL
	LD   D,(HL)
	POP  HL
	LD   A,(DE)
	INC  A
	JR   Z,INSDDO
	LD   DE,&10
	ADD  HL,DE
	INC  C
	JR   INSL1
INSNDO: PUSH HL
	LD   HL,NODYNA
	INC  (HL)
	LD   A,(NOSTAT)
	ADD  A,C
	LD   L,A
	LD   H,0
	ADD  HL,HL
	ADD  HL,HL
	LD   DE,CURENT-4
	ADD  HL,DE
	EX   DE,HL
	POP  HL
	LD   B,&00
	JR   INSL2
INSDDO: LD   B,&FF
INSL2:	PUSH HL
	LD   L,4
	LD   A,(INPRNT)
	CP   1+(GROD-DIMTAB)/4
	JR   NZ,UKLAB
	LD   HL,(RODADR)  ; Poke address of rod in AUXTAB into sprite data
	LD   (CELERA),HL
	LD   L,7
UKLAB:	LD   (DE),A
	LD   (ELEMNT+2),A
	LD   (ELEMNT+3),A
	LD   (ELEMNT+4),A
	LD   (ELEMNT+5),A
	LD   A,L
	LD   (CELEGR),A   ; GROL server
	LD   HL,RODPOS	  ; Address of main char's XYZs on last update
	LD   (CELEMT),DE  ; Save that here
	INC  DE 	  ; Go on to X
	PUSH BC
	LDI		  ; Move X and Y to rod.
	LDI
	LD   A,(ZLEVEL)
	ADD  A,(HL)	  ; Make Z a little higher so you actually "drop" it!
	LD   (DE),A
	INC  DE
	POP  BC
	POP  HL
	BIT  0,B	  ; Last in list?
	JR   NZ,INSL3
	XOR  A
	LD   (DE),A
INSL3:	LD   DE,ELEMNT	  ; DE => data block which defines new sprite
	EX   DE,HL	  ; HL points to new data, DE points to sprite area
	PUSH BC
	LD   BC,&10	  ; Again, 16 bytes to move (17 with the delim.)
	LDIR
	POP  BC
	BIT  0,B
	JR   NZ,INSL4
	XOR  A
	LD   (DE),A
INSL4:	LD   A,(NOSTAT)
	ADD  A,C
	LD   E,A
	LD   D,0
	LD   HL,ACTTAB-1  ; Find action server for last object
	ADD  HL,DE
	LD   (HL),2	  ; Move the last pointer one up and zero out the one
	EI
	LD   A,(INPRNT)
	CP   1+(GROD-DIMTAB)/4	; Arg! Use action server 1 or 4 according to
	RET  NZ 		; taste
	LD   (HL),4
	LD   A,(RODCOL)
	LD   E,A
	LD   A,(COLOUR)
	LD   D,A	  ; Change colour down on the panel on the rods
	JP   CHNGRD
;
TITLE	'IMAGE3a: Table initialising modules'
EJECT
;
; ****************************************************************************
;
; Module Name:	 Room Table Setup
;
; Label:	 ROOM
;
; Entry params:  A=room number.
;
; Exit params:	 DE=1st free entry in Sprites Table - the address where the 
;		    next piece of sprite data can be initialised.
;		 Everything else corrupt, Interrupt status preserved.
;
; The room table format is as follows:-
;
;  1. The values for the room's Diagonal Walls in the order:-
;      a) Up (distant wall)
;      b) Right
;      c) Left
;      d) Down (near wall)
;
;  2. The values for the room's Axis Walls in the order
;      a) X axis low
;      b) Y axis low
;      c) X axis high
;      d) Y axis high
;
;  3. A Sequence of either:-
;      a) 1,x,y,w,h,mask for a square, or
;      b) 2,x,y,w,h,mask for a descending triangle (corner in bottom left), or
;      c) 3,x,y,w,h,mask for a descending triangle (corner in top right), or
;      d) 4,x,y,w,h,mask for an ascending triangle (corner in bottom right), or
;      e) 5,x,y,w,h,mask for an ascending triangle (corner in top left), or
;      f) 6,x,y,w,h,mask for an ascending parallelogram, or
;      g) 7,x,y,w,h,mask for a descending parallelogram, or
;      h) &00		       (terminates sequence)
;
;  4. A Sequence of either:-
;      a) &01-&57,X,Y,Z        (for Static Objects), or
;      b) &58-&6F,X,Y,Z        (for Computed Objects), or
;      c) &7F,X,Y,Z,A,B,C,ACTN (for Extended Objects eg. exits, holes), or
;      d) &81-&FF,X,Y,Z        (for Relative Tokens), or
;      e) &00		       (terminates sequence)
;
;  5. A Sequence of either:-
;      a) Frames,Animation Sequence,X,Y,Z,VvX,VvY,VvZ,GROL+&80,ACTN, or
;      b) &00		       (terminates sequence)
;
; The Token Colour types follow these rules:-
;
;      Type 0 = No More or Nothing At All
;      Type 1 = Any ink (exceot black) and black paper
;      Type 2 = Room foreground, any background (except black)
;      Type 3 = A colour close to current (to avoid attribute colour clash)
;
;
; X,Y,Z are coordinates,
; VvX,VvY,VvZ are Velocity vectors,
; GROL/ACTN are grol and action servers,
; R is Reflect state (0/1), W is Width (in blocks), H is Height (in blocks)
; w is width in pixels, h is height in pixels, mask is the wall mask,
; x and y are screen coordinates. A,B and C are XYZ dimensions.
;							
; There is never black ink anywhere by the way.
;
; Refer to DIMTAB (M:7A) for object explanations and computed objects info.
;
ROOM:	LD   HL,SPRTAB	       ; Print and set up room graphics and tables.
	LD   DE,SPRTAB+1
	LD   BC,&BF
	LD   (HL),B
	LDIR		       ; initialise dynamic object's table
	LD   (CURROM),A        ; Insert room number here
	LD   HL,CURENT
	LD   DE,CURENT+1
	LD   BC,&120
	LD   (HL),0
	LDIR
	XOR  A
	LD   (EXTNUM),A        ; Zero number of static and extended objects.
	LD   (NOSTAT),A
	LD   (FLAGS4),A
	LD   (BABIES),A
	LD   H,A
	LD   L,A
	LD   (CAMDF1),HL
	LD   (CAMDF2),HL
	LD   A,3
	LD   (ELECNT),A        ; Any elevator doors should be printed in full.
	LD   HL,ACTTAB
	LD   (ACTPTR),HL       ; Initialise action server table pointer.
	LD   HL,EXTDIR
	LD   (XTDPTR),HL       ; and Extended Direction pointer
	LD   HL,RODTAB
	LD   (RTDPTR),HL       ; and Rod Table pointer
	LD   A,(MOVED)
	OR   A
	JR   NZ,ALRTHE
	LD   HL,SCRTXT
	CALL PRINT
	LD   HL,PATTRS
	LD   DE,&5EC0
	LD   BC,&40
	LDIR		       ; Set up panel colours (yeuch!)
	JR   ISDNE
ALRTHE: LD   HL,PSEUDO	       ; Clear pseudo screen. Note that the main screen
	CALL CLRSCR	       ; is wiped out when the pseudo screen is moved.
ISDNE:	LD   A,(CURROM)
	LD   L,A
	LD   H,0
	ADD  HL,HL
	LD   DE,RADTAB-2       ; Find room in memory
	ADD  HL,DE
	LD   E,(HL)
	INC  HL
	LD   D,(HL)
	EX   DE,HL
	LD   DE,DLIMIT	       ; Move the 8 diagonal values 
	LD   BC,8
	LDIR
	PUSH HL
	LD   HL,DLIMIT
	LD   DE,XLIMIT
	LD   BC,8
	LDIR
	LD   DE,XLIMIT
	LD   HL,XADJST
	LD   B,8
ADJUST: LD   A,(DE)	       ; Make a copy of boundary limits with
	SUB  (HL)	       ; adjustment for hole printup use.
	LD   (DE),A
	INC  DE
	INC  HL
	DJNZ ADJUST
	POP  HL
	CALL FLOOR	       ; Draw up room skeleton
	LD   DE,(COLOUR)
	LD   D,E
	LD   A,(CFLOOR)
	XOR  7		       ; Fix colours (cannot be >7)
	JR   NZ,VISBLE
	LD   A,7	       ; Top floor is white.
VISBLE: LD   E,A
	LD   (COLOUR),DE
	EXX
	LD   HL,COLMAP
	LD   DE,COLMAP+1
	LD   BC,&02BF	       ; Clear area in buffer to be used for colur
	LD   (HL),A	       ; pseudo-attribute-file
	LDIR
	EXX
	LD   IY,CURENT
	PUSH HL
	CALL HOLES	       ; Put up holes
	POP  HL
	LD   A,(CFLOOR)
	CALL MUL21
	LD   C,A
	LD   A,(CURROM)
	ADD  A,C
	CALL SETSED
RLOOP1: LD   A,(HL)	       ; Byte from room table
	OR   A
	JP   M,RTOKEN	       ; >127 is a token, jump to decode it.
	JP   Z,REXIT1	       ; Zero is the terminator, break loose.
	CP   1+(GBGHLR-DIMTAB)/4
	JR   Z,RNOBGV
	CP   1+(GBGHLL-DIMTAB)/4
	JR   NZ,RNOBGH
RNOBGV: LD   E,A
	LD   A,(CFLOOR)
	DEC  A
	JP   NZ,VIRKXX
	LD   A,E
RNOBGH: CP   &70
	JP   C,STATOB	       ; 01-6F is a physical image
	LD   A,(EXTNUM)        ; 127 is an "Extended Object" -
	LD   E,A	       ; it has no graphic shape, but will merely be
	LD   D,0	       ; represented as an object with a universal
	INC  A		       ; X/Y/Z coordinate set and an A/B/C dimension
	LD   (EXTNUM),A        ; set.
	ADD  A,&5F
	LD   (IY+0),A
	LD   A,E
	ADD  A,A
	ADD  A,A
	LD   E,A
	LD   BC,EXTTAB
	EX   DE,HL
	ADD  HL,BC
	EX   DE,HL
	INC  HL
	CALL PUTXYZ
	LDI
	LDI
	LDI
	LD   A,(HL)
	LDI
	CALL UPDACT	       ; Complete with action servers.
	LD   DE,(XTDPTR)
	LD   A,(HL)
	LD   (DE),A
	INC  HL 	       ; Insert "direction"
	INC  DE
	LD   (XTDPTR),DE
	LD   A,(NOSTAT)
	INC  A
	LD   (NOSTAT),A
	JP   RCONT1	       ; Go4another
CALCAT: INC  HL 	       ; Call with HL => CURENT entry.
	LD   B,(HL)	       ; Ret's ATADDR = attribute address of object
	INC  HL 	       ; Note: IT'S ATTR_ADDRESS+&400
	LD   C,(HL)
	INC  HL
	LD   A,(HL)
	ADD  A,&10
	LD   D,A
	XOR  A		       ; If STATIC is called with A=0, it means 
	CALL STATIC	       ; return with HL=df address of BCD=XYZ
	LD   A,H
	RRCA
	RRCA
	RRCA		       ; Convert to attribute address
	AND  &03
	ADD  A,HIGH (COLMAP)   ; Put it into the background storage space
	LD   H,A
	LD   DE,&A0
	LD   A,L
	AND  &1F
	CP   &1E
	JR   NZ,VIRKER
	LD   E,&C0
VIRKER: AND  A
	SBC  HL,DE
	LD   (ATADDR),HL
	RET
RTOKEN: PUSH HL
	CALL CALCAT
	POP  HL
	LD   A,(HL)
	AND  &7F
	CP   &0B
	JR   C,VIRK1
	CP   &0D
	JR   C,NVIRK1
	JR   NZ,VIRK1
	LD   A,(FLAGS4)
	OR   %100
	LD   (FLAGS4),A
	LD   A,&D
	JR   VIRK1
NVIRK1: SUB  &0A
	LD   C,A
	LD   A,(CFLOOR)
	CP   &07
	JR   Z,VIRKT	       ; This sets up the elevators. Too lazy to 
	CP   &01	       ; doc it.
	JR   Z,VIRKB
VIRKN:	LD   A,C
	ADD  A,&0A
	JR   VIRK1
VIRKT:	BIT  0,C
	JR   Z,VIRKN
VIRKXX: INC  HL
	INC  HL
	INC  HL
	INC  HL
	JP   RLOOP1
VIRKB:	BIT  1,C
	JR   Z,VIRKN
	JR   VIRKXX
VIRK1:	LD   DE,TADTAB-2
	ADD  A,A
	LD   C,A
	LD   B,0
	EX   DE,HL
	ADD  HL,BC
	LD   A,(HL)
	INC  HL
	LD   H,(HL)
	LD   L,A
	INC  DE
RTLOOP: LD   A,(HL)
	OR   A
	JR   Z,RTEXIT
	LD   (IY+0),A
	INC  HL
	LD   A,(DE)
	ADD  A,(HL)
	LD   (IY+1),A
	LD   B,A
	INC  HL
	INC  DE
	LD   A,(DE)
	ADD  A,(HL)
	LD   (IY+2),A
	LD   C,A
	INC  HL
	INC  DE
	LD   A,(DE)
	ADD  A,(HL)
	LD   (IY+3),A
	DEC  DE
	DEC  DE
	INC  HL
	PUSH DE
	LD   D,A
	LD   A,(IY+0)
	CALL STATP2
	JR   NC,RTDONT	       ; Not to be updated (computed object)
	LD   DE,4
	ADD  IY,DE
	LD   A,(NOSTAT)
	INC  A		       ; HL is used...	ADDQ #1,NOSTAT
	LD   (NOSTAT),A
RTDONT: POP  DE
	JR   RTLOOP
;
; Colour in tokens (in attributes - tricky to translate)
;
RTEXIT: PUSH IX
	LD   IX,(ATADDR)
	INC  HL
	LD   A,(HL)	       ; Colour type 1-3 or Zero for nothing
	OR   A
	JR   Z,RTSLUT	       ; No colours - end token expansion
	DEC  A
	JR   Z,TYPE1	       ; Type 1 colour token
	DEC  A
	JR   Z,TYPE2	       ; Type 2 colour token
TYPE3:	CALL RND	       ; Fetch a colour
	AND  3		       ; Make 0,1 or 2
	DEC  A		       ; Make -1,0 or 1
	LD   B,A
	LD   A,(COLOUR)
	AND  7		       ; Get native Room colour
	ADD  A,B	       ; Find colour close to that
	JR   Z,TYPE3	       ; Musn't be zero
	CP   8
	JR   NC,TYPE3	       ; Must be less than 8
	JR   GOTYPL	       ; That's it
TYPE2:	CALL RND
	AND  7
	JR   Z,TYPE2
	LD   B,A
	LD   A,(COLOUR)
	AND  7
	CP   B
	JR   Z,TYPE2
	RLC  B
	RLC  B
	RLC  B
	OR   B
	JR   GOTYPE
TYPE1:	CALL RND	       ; Get colour
	AND  7		       ; Only ink, black paper
	JR   Z,TYPE1	       ; But get another if black ink
	LD   B,A
	LD   A,(COLOUR)
	CP   B
	JR   Z,TYPE1
	LD   A,B
GOTYPL: OR   &40	       ; Luminance on
GOTYPE: LD   C,A
RTGRAB: INC  HL 	       ; 1st offset
	LD   A,(HL)
	CP   &10
	JR   Z,RTSLUT
	LD   (RTSMC+2),A       ; Patch in offset value
RTSMC:	LD   (IX+0),C	       ; RoomTokenSelfModifyingCode!
	JR   RTGRAB
RTSLUT: POP  IX
	INC  DE
	INC  DE
	INC  DE
	EX   DE,HL
	JP   RLOOP1
UPDACT: PUSH HL
	LD   HL,(ACTPTR)
	LD   (HL),A
	INC  HL
	LD   (ACTPTR),HL
	POP  HL
	RET
STATP1: LD   (IY+0),A	       ; Get Image, x, y, z coordinates,
	INC  HL
	LD   B,(HL)
	LD   (IY+1),B	       ; and insert them into CURENT
	INC  HL
	LD   C,(HL)
	LD   (IY+2),C
	INC  HL
	LD   D,(HL)
	LD   (IY+3),D
	INC  HL
STATP2: PUSH HL 	       ; Save pointer to ROOMxx
	CALL STATIC	       ; Print the object too.
	LD   A,(IY+0)
	CP   88
	POP  HL
	RET  NC 	       ; Don't bitmap if 88-95
	CP   &40
	JR   C,STATP8
	LD   A,1+(GDOOR2-DIMTAB)/4
STATP8: PUSH HL
	LD   L,A
	LD   H,0
	LD   DE,DIMTAB-1       ; Entry 4 (or 3 @ 0-3)
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,DE	       ; This is the ACTN server.
	LD   A,(HL)
	CALL UPDACT	       ; Update Action Server Table
	POP  HL
	SCF		       ; Do bitmap
	RET
STATOB: CALL STATP1
	JP   NC,RLOOP1	       ; No bitmap
	EX   DE,HL
	LD   HL,NOSTAT
	INC  (HL)
	EX   DE,HL
RCONT1: LD   DE,4
	ADD  IY,DE
	JP   RLOOP1
REXIT1: PUSH HL
	LD   HL,COLMAP		; Move the attrs down
	LD   DE,&5800
	LD   BC,&0300
	LD   A,(MOVED)
	OR   A
	JR   Z,FIRTST	       ; Move only &2D0 bytes if the rest is already
	LD   BC,&02D0	       ; there!
FIRTST: LDIR
	POP  HL
	LD   DE,SPRTAB	       ; Now deal with the dynamic objects (sprites)
	INC  HL
	LD   B,0
	LD   IX,COUNTR
	LD   (IX+0),B
RLOOP2: PUSH HL
	PUSH DE
	LD   A,(CURROM)
	LD   D,A
	LD   A,(CFLOOR)
	DEC  A
	DEC  D
	CALL INDX21
	LD   DE,ROBOTP
	ADD  HL,DE
	AND  (HL)
	POP  DE
	POP  HL
	EX   AF,AF"
	LD   A,(HL)	       ; Grab a byte from the sprites section of
	OR   A		       ; the room data.
	JP   Z,RNEXT4	       ; However, jump along if already at the end,
	INC  (IX+0)	       ; else increase sprite counter.
	LD   (DE),A	       ; Insert first byte
	AND  7
	LD   C,A
	PUSH DE 	       ; No of animations to put into the frame buffer
	INC  DE
	LD   A,1	       ; Insert a 1 as to start off from the 1st!
	LD   (DE),A
	INC  DE
	INC  HL
	LD   A,(HL)	       ; Get actual 1st animation physical image
	CP   1+(GRBOT1-DIMTAB)/4
	JR   Z,FLAP44
	CP   1+(GRBOT2-DIMTAB)/4
	JR   Z,FLAP44
	CP   1+(SPARE1-DIMTAB)/4
	JR   NZ,WHAP2
	LD   A,(CFLOOR)        ; Check for floor=3 and a small flat square...
	CP   3
	JR   Z,WHAP2
	LD   DE,9
	JR   FLAP45
FLAP44: EX   AF,AF"
	JR   NZ,SHOW1
	LD   DE,11
FLAP45: ADD  HL,DE
	DEC  (IX+0)
	POP  DE
	JP   RLOOP2
SHOW1:	PUSH HL
	LD   HL,FLAGS4
	SET  3,(HL)
	POP  HL
SHOW2:	EX   AF,AF"
WHAP2:	LD   (IY+0),A	       ; Insert the image into CURENT as well!
	LDIR
	CALL PUTXYZ	       ; Insert the coords
	LDI		       ; Insert the vectors
	LDI
	LDI
	CALL PUTWRD
	LD   A,(HL)
	LD   (DE),A
	INC  DE
	XOR  A
	LD   (DE),A	       ; Suspend User Byte 0 for all good reasons in
RLOOP3: LD   A,(HL)	       ; the world!
	OR   A
	JP   M,REXIT3
	LDI
	JR   RLOOP3
REXIT3: EX   DE,HL
	INC  HL
	LD   (HL),0
	EX   DE,HL
	AND  &7F
	POP  DE
	LD   BC,&F
	EX   DE,HL
	ADD  HL,BC
	EX   DE,HL
	LD   C,4
	ADD  IY,BC
	LD   (DE),A
	INC  DE
	INC  HL
	LD   A,(HL)
	CALL UPDACT
	INC  HL
	JP   RLOOP2
RNEXT4: LD   HL,AUXTAB
RNEXT5: LD   A,(IX+0)
	CP   4		       ; Make sure game doesn't crash due to too many
	JR   Z,RFINIS	       ; sprites! - 6 DDO'S, 1 ROBOT, 1 MAIN. (+1 AUX?)
	LD   A,(HL)	       ; Dormant Dynamic Objects. Get room no.
	OR   A		       ; Test for zero (=end of table)
	JR   Z,RFINIS	       ; jump if so
	CP   &FF
	JP   Z,NOTHRE	       ; Disabled.
	AND  &1F
	LD   B,A	       ; Room no. extracted
	LD   A,(HL)
	RLCA
	RLCA		       ; This is the same as 5*RRCA.
	RLCA
	AND  &7
	LD   C,A	       ; Floor no.
	INC  C
	LD   A,(CFLOOR)
	CP   C
	JP   NZ,NOTHRE
	LD   A,(CURROM)
	CP   B		       ; Test if room number matches.
	JR   NZ,NOTHRE
	LD   (TEMPHL),HL
	INC  (IX+0)	       ; If so, mark another dynamic object
	LD   A,1
	LD   (DE),A	       ; One frame into SPRTAB
	INC  DE
	LD   (DE),A	       ; Current = 1 into SPRTAB
	INC  DE
	INC  HL
	LD   A,(HL)	       ; Get image
	LD   (DE),A
	LD   (IY+0),A	       ; Insert into SPRTAB,CURENT
	INC  HL
	INC  DE
	LD   A,(HL)
	CALL UPDACT	       ; Update actions table
	INC  HL
	XOR  A
	LD   (DE),A	       ; Zero all velocity vectors
	INC  DE
	LD   (DE),A
	INC  DE
	LD   (DE),A
	INC  DE
	CALL PUTWRD	       ; Put address in CURENT
	CALL PUTXYZ	       ; put the coordinates into CURENT
	EX   DE,HL
	LD   A,(UB0VAL)
	LD   (HL),A	       ; DDO!
	INC  HL
	LD   (HL),0	       ; No Z acc.
	INC  HL
	LD   A,(TEMPHL)
	LD   (HL),A
	INC  HL
	LD   A,(TEMPHL+1)      ; Address of 0th byte in AUXillary table
	LD   (HL),A	       ; is inserted into the sprite's data.
	LD   BC,4	       ; This completely fills the sprite's area
	ADD  HL,BC	       ; if it has got 4 animations, ie 2+4+3+2+1+1+2+1
	LD   (HL),0	       ; No GROL
	INC  HL
	EX   DE,HL
	LD   C,4
	ADD  IY,BC
	JR   RNEXT5
NOTHRE: LD   BC,6
	ADD  HL,BC
	JR   RNEXT5
RFINIS: XOR  A
	LD   (FLAGS1),A        ; Start initialising (A=0)
	LD   (FLAGS2),A
	LD   (EXRODS),A
	LD   (IY+0),A	       ; End marker at end of "CURENT"
	LD   A,(AUXFLG)
	AND  %11
	LD   (AUXFLG),A
	LD   HL,UPDTAB
	LD   (UPDPTR),HL       ; Screen addresses pointer at start
	LD   A,(IX+0)	       ; Add sprite counter to previous number
	INC  A		       ; of dynamic objects. This is because the
	LD   (NODYNA),A        ; previous number may be zero during, for
	CALL RND
	AND  7
	ADD  A,2
	LD   (ENERGY),A        ; Give a robot some energy
	RET		       ; example, demo screens.
;
PUTXYZ: LD   A,(HL)	       ; This inserts three bytes addressed by
	LD   (IY+1),A	       ; HL into the three addresses IY+1,2 and 3.
	INC  HL 	       ; HL is incremented by 3. A is Z coordinate
	LD   A,(HL)	       ; all flags and other registers are preserved
	LD   (IY+2),A
	INC  HL
	LD   A,(HL)
	LD   (IY+3),A
	INC  HL
	RET
;
; Module name:	 Hole Calculation and printing
;
; Label name:	 HOLES
; 
CHOLES: LD   (TMPFLR),A
	LD   HL,MATRIX	       ; Clear matrix
	LD   DE,MATRIX+1
	LD   BC,&003F	       ; 8*8 it is
	LD   (HL),B
	LDIR
	LD   A,(TMPFLR)        ; Make a copy of current floor number
	DEC  A
	RET  Z		       ; Don't bother if bottom floor.
HOLES1: LD   A,(TMPFLR)
	INC  A
	CALL MUL21
	LD   C,A
	LD   A,(CURROM)        ; Find actual room number (floor*21+room)
	ADD  A,C
	CALL SETSED	       ; Set random seed according to room number
	CALL RND
	AND  3
	INC  A
	LD   B,A	       ; This is no. of expansions
HOLES2: CALL RND	       ; Get which macro to expand
	AND  &38
	LD   L,A
	LD   H,&00
	LD   DE,MACROS	       ; Holemacros
	ADD  HL,DE
	CALL RND
	AND  &3F	       ; Get offset (position in matrix)
	LD   E,A
	LD   D,HIGH(MATRIX)
HOLES3: LD   A,1+(GHOLE1-DIMTAB)/4
	LD   (DE),A	       ; Insert hole in matrix
	LD   A,E	       ; Get current position
	ADD  A,(HL)	       ; New relative offset (which it always is!)
	INC  HL 	       ; Next offset pointer
	CP   &40
	JR   NC,HOLES4	       ; Out of matrix range -so skip.
	CP   E		       ; Check if it moved,
	LD   E,A	       ; 'cause if it didn't
	JR   NZ,HOLES3	       ; Now the end is reached. This is the pits.
HOLES4: DJNZ HOLES2
NXTHLE: LD   HL,TMPFLR	       ; That was one floor done - descent one level
	DEC  (HL)	       ; Find out if we're on bottom floor -
	JR   NZ,HOLES1	       ; 'cause if we are, we shouldn't.
	LD   A,(CURROM)
	LD   HL,EXCEPT-1       ; Exceptions table - remove any holes which
	LD   B,A	       ; should'n have been there. This is a patch.
EXCEP1: LD   A,(HL)	       ; Get from table
	INC  HL
	OR   A		       ; End-marker
	JP   P,EXCEP1	       ; No, grab next
	DJNZ EXCEP1	       ; else go on only if current room is not reached
EXCEP2: LD   A,(HL)	       ; Get matrix index
	OR   A
	RET  M
	LD   E,A
	INC  HL
	LD   A,(HL)
	ADD  A,A
	ADD  A,A
	ADD  A,A
	ADD  A,E
	NEG
	ADD  A,&48
	LD   E,A
	XOR  A
	LD   (DE),A	       ; and zero any hole there out.
	INC  HL
	JR   EXCEP2
HOLES:	LD   A,(CFLOOR)
	CALL CHOLES
PHOLES: LD   DE,MATRIX	       ; DE now equals MATRIX
	LD   C,8
PHOLE1: LD   B,8	       ; 8x8 matrix
PHOLE2: LD   A,(DE)
	OR   A
	JR   Z,PNOHO1	       ; Jump if no hole there
	PUSH BC 	       ; Else check boundaries
	PUSH DE
	LD   A,B
	ADD  A,A
	ADD  A,A
	ADD  A,A
	LD   B,A	       ; Real X coord.
	LD   A,C
	ADD  A,A
	ADD  A,A
	ADD  A,A
	LD   C,A	       ; Real Y coord. (perhaps add to XLOW/YLOW?)
	LD   HL,XLIMIT
	CALL CHKBND
	JR   NC,PNOHO2	       ; Jump if it was far out.
	LD   A,(DE)
	LD   D,0
	LD   (IY+0),A	       ; Else prepare for a New Object to enter
	LD   (IY+1),B	       ; The Universe. Gosh, it's such a thrill being
	LD   (IY+2),C	       ; A Creator!
	LD   (IY+3),D
	CALL STATP2	       ; Dig the baby!
	LD   BC,4
	ADD  IY,BC	       ; Next entry in current room table
	PUSH HL
	LD   HL,NOSTAT	       ; One xtra static object!
	INC  (HL)
	POP  HL
PNOHO2: POP  DE
	POP  BC
PNOHO1: INC  DE 	       ; Next matrix pos.
	DJNZ PHOLE2
	DEC  C		       ; Go 8x8
	JR   NZ,PHOLE1
	RET		       ; Bingo
;
MUL21:	LD   C,A	       ; Slow mul. 21d (&17). A=A*21
	ADD  A,A
	ADD  A,A
	LD   B,A
	ADD  A,A	       ; MULU.W D0,D1  (hehe)
	ADD  A,A
	ADD  A,B
	ADD  A,C
	RET
;
; Module name:	 Check Room Boundaries, both axis and diagonal.
;
; Label name:	 CHKBND
;
; Entry params:  C=X coord, B=Y coord, Z coord is redundant.
;		 HL=address of wall coordinates in the order
;
;		     1. Down Wall Limit
;		     2. Upper Wall Limit
;		     3. Right Wall Limit
;		     4. Left Wall Limit
;		     5. Xwall Low
;		     6. Ywall Low
;		     7. Xwall High
;		     8. Ywall High
;
; Exit params:	 Carry is clear if outside boundaries, else set.
;		 HL=(HL)+7
;		 DE and all alternate registers preserved
;		 All other registers and flags undefined
;
; Note: When X axis wall is used, it means the wall running parallel with
;	the X axis in the universe, and it is actually the Y coordinate
;	which should be used in a check.
;
CHKBND: LD   A,C	       ; First check the Modified Axis Walls, or, as
	ADD  A,B	       ; I prefer to call them, Diagonal Walls. Grab
	CP   (HL)	       ; X+Y and check against the Down Wall Limit.
	JR   C,BNDHIT	       ; Jump if embedded in or outside the wall.
	INC  HL 	       ; Else go on. We're now at Upper Wall Limit.
	CP   (HL)	       ; Check that. 
	JR   NC,BNDHIT	       ; Again jump if embedded or away.
	INC  HL 	       ; Else go on to Right Wall Limit. 
	LD   A,B	       ; But here we need another reference.
	SUB  C		       ; This is Y-X. If this results in an overflow,
	JR   C,BOUND1	       ; it means that this wall is on the other half
	CP   (HL)	       ; of the screen. This means that walls cannot
	JR   NC,BNDHIT	       ; be beyond the middle of their respective 
BOUND1: LD   A,C	       ; screen side. If it was outside, jump.
	SUB  B		       ; Else go on to grab X-Y and to the trick the
	INC  HL 	       ; other way around. Forward to Left Wall Limit,
	JR   C,BOUND2	       ; but if the wall is too far away, skip the
	CP   (HL)	       ; section. Else do the check.
	JR   NC,BNDHIT	       ; Jump if wall crash here.
BOUND2: INC  HL 	       ; Forward to X axis wall Low limit.
	LD   A,B	       ; Check the Y
	CP   (HL)	       ; against that.
	JR   C,BNDHIT	       ; Jump if wall hit
	INC  HL 	       ; else go on to Y axis wall low limit.
	LD   A,C	       ; Get X
	CP   (HL)	       ; Check against wall
	JR   C,BNDHIT	       ; Again jump if wall hit
	INC  HL 	       ; else forward to X axis high limit
	LD   A,B	       ; Grab Y again
	CP   (HL)	       ; Check against limit
	JR   NC,BNDHIT	       ; jump if embedded or outside
	INC  HL 	       ; Go on to last value, the Y axis wall hi limit
	LD   A,C	       ; Get X
	CP   (HL)	       ; Do the check
	RET  C		       ; and return carry flag if no wall hit at all.
BNDHIT: AND  A		       ; Enter here if there was a wall collision.
	RET		       ; Reset carry flag and return it
;
TITLE	'IMAGE4: Collision detection'
EJECT
;
; COLLISION DETECTION MODULE:
; Call with A=Sprite no. Returns Z=0 if collision was detected. The following
; variables are then set:
;	CURLOG = Logical object in collision (The entry in CURENT)
;	CURPHY = Physical object in collision (The IMAGE number)
;	EXTZ = The Z coordinate + Z dimension of the object in collision
;	EXTY = Same for Y
;	EXTX = Same for X (X and Y not really useful)
; Note that bit 7 of the sprite's entry in DIMTAB is set during the test
; to avoid collision between a sprite and the sprite itself
;
; For anti-crash purposes, a special "Collision Exception" is implemented
; and activated when a collision occurs in the very first call (ie. on
; print-up) of COLISN. In that case, the collision is not reported as a real
; collision, but other actions can be taken, eg. limited rotation of the
; main character.
; There is also an even more special "Collision Exception Exception" which
; covers the Holes, which are only penetrateable within a certain offset.
; This exception exception will delete the first exception and assume a
; collision.
;
COLISN: LD   HL,FLAGS1
	RES  0,(HL)	     ; Assume no propspective collsion.
	LD   L,A
	XOR  A
	LD   H,A	     ; HL=sprite no
	DB   &DD
	LD   H,A	     ; Initialize:
	LD   (CURLOG),A      ; Collided object,
	LD   (CURPHY),A
	LD   (DORWAY),A      ; Collided shape, doorway (if any)
	DEC  A
	LD   (EXTZ),A	     ; Z+Z dimension to -1
	LD   A,1
	LD   (COUNTR),A      ; No. of objects
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL	     ; 16 per entry
	LD   DE,SPRTAB-&10
	ADD  HL,DE	     ; Into sprites table,
	LD   A,(HL)
	AND  7
	ADD  A,5	     ; Go past first two bytes and the velocity vectors
	LD   E,A
	LD   D,0
	ADD  HL,DE	     ; Find address of I,XYZ
	LD   A,(HL)
	INC  HL
	LD   H,(HL)
	LD   L,A	     ; HL points to I,XYZ
	LD   A,(HL)	     ; Get image
	DB   &DD
	LD   L,A
	LD   DE,DIMTAB-4     ; Index into Dimension Table,
	ADD  IX,IX
	ADD  IX,IX
	ADD  IX,DE	     ; Points to X Dimension
	SET  7,(HL)	     ; "This is the current object"
	INC  HL 	     ; Points to X coord
	LD   A,(HL)	     ; Now check diagonal walls
	LD   B,A
	INC  HL
	LD   C,(HL)	     ; Grab X and Y
	DEC  HL
	PUSH HL
	LD   HL,DLIMIT
	CALL CHKBND	     ; Check boundaries
	JR   C,NOTATA
HITWAL: LD   HL,FLAGS1	     ; Enter here if "Outside Diagonal or Axis Wall"
	SET  0,(HL)	     ; "Collision detected"
	XOR  A
	LD   (CURPHY),A
	INC  A		     ; A's an object with an inactive action server (1)
	LD   (CURLOG),A      ; But check collision anyway.
NOTATA: POP  HL
	LD   DE,CURENT	     ; Start of table,
CLOOP1: LD   A,(DE)
	OR   A
	JR   NZ,CMORE	     ; Not finished yet,
	DEC  HL
	RES  7,(HL)	     ; Remove current object indentifier
	INC  HL
	INC  HL
	INC  HL
	LD   A,(HL)	     ; Z coordinate
	OR   A
	RET  M		     ; No collision state if collided with floor,
	LD   HL,FLAGS1	     ; (returns NZ)
	LD   A,(CURGRL)
	CP   5
	JR   NZ,NOTAG5	     ; Ret NZ if collision AND not GROL 5 character
	BIT  0,(HL)	     ; Was there a collision?
	LD   HL,FLAGS2
	JR   NZ,CHITY	     ; Jump if so.
EXCPT9: SET  2,(HL)	     ; Else set "processed a loop without collision"
	RET		     ; Return Z (no collision)
CHITY:	BIT  2,(HL)	     ; Collision detected. Allowed?
	RET  NZ 	     ; Yes, return NZ (collision)
	LD   A,(CURPHY)      ; Exception condition. Get collided object
	CP   1+(GHOLE1-DIMTAB)/4  ; Hole?
	JR   Z,MYHOLE	     ; Yes. Collision is allowed again!
	XOR  A
	RET		     ; No collision! Ret Z.
MYHOLE: OR   &FF	     ; But do not set "processed a loop without a
	JR   EXCPT9	     ; collision" flag. Then return with no tea (NZ)
NOTAG5: BIT  0,(HL)
	RET
CMORE:	JP   M,NCLSN2
	CP   &40
	JR   C,CEVENM
	CP   &50
	JR   NC,CEVENM
	LD   (COBJC2),A
	LD   A,1+(GDOOR2-DIMTAB)/4
CEVENM: LD   (COBJCT),A
	DB   PIY
	LD   L,A
	DB   PIY
	LD   H,0
	LD   BC,DIMTAB-4
	ADD  IY,IY
	ADD  IY,IY
	ADD  IY,BC
	PUSH HL
	PUSH DE
	INC  DE
	LD   A,(DE)
	LD   B,A
	LD   A,(HL)
	ADD  A,(IX+0)
	SUB  B
	JR   C,NOCLSN
	LD   A,B
	ADD  A,(IY+0)
	SUB  (HL)
	JR   C,NOCLSN
	INC  DE
	INC  HL
	LD   A,(DE)
	LD   B,A
	LD   A,(HL)
	ADD  A,(IX+1)
	SUB  B
	JR   C,NOCLSN
	LD   A,B
	ADD  A,(IY+1)
	SUB  (HL)
	JR   C,NOCLSN
	INC  DE
	INC  HL
	LD   A,(DE)
	LD   B,A
	LD   A,(HL)
	ADD  A,(IX+2)
	SUB  B
	JR   C,NOCLSN
	LD   A,B
	ADD  A,(IY+2)
	LD   E,A
	SUB  (HL)
	JR   C,NOCLSN
	LD   HL,FLAGS1
	SET  0,(HL)
	LD   A,E
	LD   (EXTZ),A
	LD   A,(COUNTR)
	LD   (CURLOG),A
	LD   A,(COBJCT)
	LD   (CURPHY),A
	LD   A,(COBJC2)
	LD   (DORWAY),A
	POP  DE
	POP  HL
	INC  DE
	LD   (CURXYZ),DE
	INC  DE
	INC  DE
	INC  DE
	JR   NCLSNX
NOCLSN: POP  DE
NCLSN3: POP  HL
NCLSN2: INC  DE
	INC  DE
	INC  DE
	INC  DE
NCLSNX: LD   A,(COUNTR)
	INC  A
	LD   (COUNTR),A
	JP   CLOOP1
;
; Module Name:	 Move Pseudo Display File to main Display File	
;
; Label:	 MOVESC
;
; Entry params:  carry is set if attributes are to be cleared 
;		 in "current colour"
;		 (MOVEA with A=No. of horizontal lines to move)
;		 (MOVEB0 to move &B0 lines)
;
; Exit params:	 Z=1, A=0, BC=0, DE,DE,IX corrupt.
;
MOVEB0: LD   A,&B0	     ; Enter here if only &B0 lines to move
	JR   MOVEA
MOVESC: LD   A,192
MOVEA:	JR   NC,NOATTR	     ; Skip if attributes should stay.
	LD   HL,&5800	     ; Otherwise clear them
	LD   BC,&0300	     ; 768 bytes
	LD   DE,(COLOUR)     ; in current colour
	PUSH AF
CLRATT: LD   A,(HL)
	CP   D
	JR   NZ,MOVCHA
	LD   (HL),E
MOVCHA: INC  HL
	DEC  BC
	LD   A,B
	OR   C
	JR   NZ,CLRATT
	POP  AF
NOATTR: LD   IX,SCRTAB	     ; Normal display file left-hand addresses
	LD   HL,PSEUDO	     ; start of pseudo display file
MOVES1: LD   E,(IX+0)
	LD   D,(IX+1)	     ; Get a DF address
	INC  IX
	INC  IX
	LD   BC,32	     ; move a line
	LDIR
	DEC  A		     ; 192 times
	JP   NZ,MOVES1
	RET
;
CLRSCR: LD   BC,&1800	     ; Enter this with HL pointing to the 6K block
	LD   D,C	     ; to be cleared. Initialise D to Zero
SLOOP1: LD   (HL),D	     ; Nop out a byte
	INC  HL 	     ; Next byte
	DEC  BC 	     ; That's one byte done
	LD   A,B	     ; Check for end reached
	OR   C		     ; Line this
	JR   NZ,SLOOP1	     ; jump if end not reached
	RET		     ; and back
;
; Routine for indexing into a bitmap where 1 bit is a flag. A=floor number
; and D=room number to check. On exit, HL is offset and A is pixel mask.
; Corrupts also C,B,E and all flags.
;
INDX21: CALL MUL21	     ; A=A*21
	ADD  A,D	     ; plus room number
	LD   D,A	     ; into D
	RRCA
	RRCA
	RRCA		     ; Get byte offset
	AND  &1F	     ; but lose bit 5-6-7
	LD   L,A
	LD   H,0	     ; HL is real offset now
	LD   A,D
	AND  7		     ; This is the pixel number
	LD   B,A	     ; 0-7
	INC  B		     ; 1-8
	XOR  A		     ; Zero the mask
	SCF		     ; Set the bit
INDX22: RLA		     ; and rotate it in
	DJNZ INDX22	     ; B times.
	RET		     ; Return HL,A
;
GAMOVR: LD   B,64
GAMO1:	PUSH BC
	LD   B,EXPLODE_2
	CALL SOUND
	POP  BC
	LD   HL,&5800
	LD   DE,&5801
	LD   A,B
	AND  &7
	LD   (HL),A
	PUSH BC
	LD   BC,&300-&41
	HALT
	LDIR
	POP  BC
	DJNZ GAMO1
	JP   ENDGAM
;
TITLE	'IMAGE4a: Display handlers'
EJECT
;
; Draw a box in display file addressed by HL with width D (in bytes) and
; height E (in pixels)
;
BOX:	PUSH HL
	PUSH DE
	LD   C,&FF	     ; top line
	LD   B,D
	CALL POKE
BOX1:	SET  0,(HL)	     ; That bit
	LD   C,L	     ; Save l here
	LD   A,L
	ADD  A,D	     ; + width
	INC  A
	LD   L,A
	SET  7,(HL)	     ; That bit
	LD   L,C	     ; Put back 
	LD   C,H
	CALL NXTROW
	DEC  E
	JR   NZ,BOX1
	CALL PRVROW
	LD   C,&FF
	LD   B,D
	CALL POKE
	POP  DE
	DEC  E
	DEC  E
	POP  HL
	CALL NXTROW
BOX2:	LD   C,0
	LD   B,D
	CALL POKE
	CALL NXTROW
	DEC  E
	RET  Z
	JR   BOX2
;
; Fill a box in the attribute file with A, rhc HL, XY size D,E;
;
COLBOX: LD   C,A
	LD   B,D
	PUSH AF
	CALL POKE
	LD   BC,&20
	ADD  HL,BC
	POP  AF
	DEC  E
	RET  Z
	JR   COLBOX
;
; Fill memory starting from HL, B bytes on, with C. HL is unaltered upon exit
;
POKE:	LD   A,L
POKE1:	INC  L
	LD   (HL),C
	DJNZ POKE1
	INC  L
	LD   C,L
	LD   L,A
	RET
;
; Write string of characters to screen - ascii characters addressed by HL
; NOTE: Format MUST be:
;
;	1. DW DF-ADDR
;	2. DB PIXEL (reverse bit pos.)
;	3. DB FLAGS
;	4. DB 'Text'
;	5. One of:-
;	   a) &00 Terminator
;	   b) &FF Repeat step 1
;	   c) &FE Repeat step 3
;	   d) &FD Next pixel=0 match
;	   e) &E0 NOP
;
; The flag byte should contain:-
;
;	Bit  When set,
;	 0    Font in ROM, else font in RAM (assumes RAM)
;	 1    Double Height
;	 2    Double Width
;	 3    Italics, right slashed
;	 4    Boldface
;	 5    Shading
;	 6    Inverse
;	 7    Plot mode, 0=XOR, 1=OR
; 
; If bit 5 is 0 (normal spacing), the font address is in (NSFONT).
; No attribute considerations are taken.
;
PRINT:	LD   E,(HL)
	INC  HL
	LD   D,(HL)
	INC  HL
	PUSH DE
	POP  IX
	LD   A,(HL)
	LD   (PIXEL),A
	INC  HL
PENTRY: LD   A,(HL)
	LD   (PFLAGS),A
	INC  HL
PRINT1: LD   A,(HL)
	INC  HL
	OR   A
	RET  Z
	CP   &FF
	JR   Z,PRINT
	CP   &FE
	JR   Z,PENTRY
	CP   &FD
	JR   Z,NEXTP0
	CP   &E0
	JR   Z,PRINT1
	PUSH HL
	CALL VDU
	POP  HL
	JP   PRINT1
NEXTP0: XOR  A
	DB   &DD
	INC  L
	LD   (PIXEL),A
	JR   PRINT1
;
; Write a character to the screen (no control characters)
; IX=DF address, A=ASCII character to be printed
; (PIXEL)=Pixel number and (PFLAGS)=Print options must also be initialized.
;
VDU:	LD   IY,PFLAGS	     ; Printer flags
	BIT  0,(IY+0)
	JR   NZ,ALLIGN
	CP   ' '
	JR   Z,ALLIGN
	CP   ';'
	JR   C,ALIGN2
	SUB  6
ALIGN2: SUB  15
ALLIGN: PUSH IX
	INC  A		     ; Start from 7th byte so go for next character
	LD   L,A
	LD   H,0
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL
	LD   A,H
	LD   BC,&3C00	     ; Rom font
	BIT  0,(IY+0)
	JR   NZ,ROMFNT
	LD   BC,FONT-&100    ; Ram font
ROMFNT: ADD  HL,BC
	XOR  A
	LD   B,8
PRINT2: DEC  HL
	OR   (HL)
	DJNZ PRINT2
	JR   NZ,PRINT3	     ; Jump if not a space
	LD   A,&1F	     ; Set <space> to 5 pixels
PRINT3: SLA  A
	INC  B
	JR   NC,PRINT3
	DEC  B
	LD   C,&B
PRINT4: RRA
	DEC  C
	JR   NC,PRINT4
	LD   A,C
	BIT  2,(IY+0)	     ; Double width?
	JR   Z,NWIDTH
	ADD  A,A	     ; Width*2 for double width
NWIDTH: LD   (PIXADD),A
	LD   A,(PIXEL)
	LD   (LOOPIX),A
	LD   C,8
	LD   A,&55
	LD   (CURSHA),A
PRINT5: PUSH BC
	LD   D,(HL)	     ; Grab a line of the character
	BIT  4,(IY+0)	     ; Boldface?
	JR   Z,NOBOLD
	LD   A,D
	SRL  A
	OR   D		     ; Then paint.
	LD   D,A
NOBOLD: BIT  6,(IY+0)
	JR   Z,NOINVS	     ; Inverse video?
	LD   A,D
	CPL
	LD   D,A
NOINVS: LD   E,0
	LD   C,E
	BIT  2,(IY+0)	     ; Double width?
	JR   Z,NRMWDT	     ; No.
	PUSH BC
	LD   B,8
	LD   A,D
DWLOOP: RRCA		     ; Perform d.width
	RLCA
	RR   D
	RR   E
	RRCA
	RR   D
	RR   E
	DJNZ DWLOOP
	POP  BC
NRMWDT: LD   A,B
	OR   A
	JR   Z,PRINT7	     ; already there
PRINT6: SLA  E		     ; To left edge
	RL   D
	DEC  A
	JR   NZ,PRINT6
PRINT7: LD   A,(LOOPIX)
	OR   A
	JR   Z,PRINT9	     ; already
PRINT8: SRL  D		     ; Into pixel no.
	RR   E
	RR   C
	DEC  A
	JR   NZ,PRINT8
PRINT9: PUSH HL
	PUSH IX
	POP  HL
	CALL ONLINE	     ; Print a line
	BIT  1,(IY+0)	     ; Double height?
	CALL NZ,ONLINE	     ; Yes.
	PUSH HL
	POP  IX
	POP  HL
	INC  HL
	POP  BC
	BIT  3,(IY+0)	     ; Italics?
	JR   Z,NOITAL	     ; No.
	LD   A,C
	ADD  A,2
ITALIC: SUB  3		     ; Time for an tilt? (each 3 rows)
	JR   C,NOITAL	     ; No.
	JR   NZ,ITALIC
	LD   A,(LOOPIX)      ; Left tilt italics
	DEC  A
	JP   P,ITALSM
	DB   PIX	     ; Decrement byte if overflow
	DEC  L
	AND  7
ITALSM: LD   (LOOPIX),A
NOITAL: DEC  C
	JP   NZ,PRINT5	     ; 8 pixel lines
	POP  IX 	     ; DF Address again
	LD   DE,(PIXEL)
	LD   A,E	     ; E is current pixel
	BIT  4,(IY+0)
	JR   Z,OHFUCK	     ; 1 more for boldface
	INC  D
OHFUCK: BIT  2,(IY+0)	     ; 1 less for double width (looks better)
	JR   Z,OHSHIT
	DEC  D
OHSHIT: ADD  A,D	     ; D is pixels in character just printed
	CP   8
	JR   C,PRINTA	     ; 1 Byte overflow
	CP   16
	JR   C,PRINTB	     ; 2 Bytes overflow
	DB   PIX	     ; Next byte
	INC  L
PRINTB: DB   PIX	     ; Next again
	INC  L
PRINTA: AND  7		     ; Inside
	LD   (PIXEL),A
	LD   A,D
	RET
ONLINE: LD   B,&FF
	LD   A,(CURSHA)
	CPL		     ; Invert shade mask
	LD   (CURSHA),A
	BIT  5,(IY+0)	     ; Shading?
	JR   Z,NOSHAD
	LD   B,A
NOSHAD: BIT  7,(IY+0)	     ; Plot mode, OR or XOR?
	JR   NZ,PMOR
PMXOR:	LD   A,D	     ; First data byte
	AND  B		     ; Shade
	XOR  (HL)	     ; Screen contents
	LD   (HL),A	     ; on screen
	INC  L
	LD   A,E	     ; Second...
	AND  B
	XOR  (HL)
	LD   (HL),A
	INC  L
	LD   A,C	     ; Third...
	AND  B
	XOR  (HL)
	JR   PMXOR1
PMOR:	LD   A,D
	AND  B
	OR   (HL)
	LD   (HL),A
	INC  L
	LD   A,E
	AND  B
	OR   (HL)
	LD   (HL),A
	INC  L
	LD   A,C
	AND  B
	OR   (HL)
PMXOR1: LD   (HL),A
	DEC  L
	DEC  L
NXTROW: BIT  5,H	     ; Next row addressed by HL. Uses only A (and HL)
	JR   Z,NORMDF	     ; Jump if in normal display file format
	LD   A,L
	ADD  A,&20	     ; Find next line
	LD   L,A
	RET  NC
	INC  H		     ; Hi byte if required
	RET
NORMDF: INC  H		     ; Next row
	LD   A,H
	AND  7
	RET  NZ 	     ; That's all
	LD   A,L
	ADD  A,&20	     ; A character row
	LD   L,A
	RET  C		     ; Back if also overlap of a 'third'
	LD   A,H
	SUB  8		     ; But if not, go a 'third' back.
	LD   H,A
	RET
;
PRVROW: BIT  5,H	     ; Previous row addressed by HL
	JR   Z,PRVRW1
	LD   A,L
	SUB  &20
	LD   L,A
	RET  NC
	DEC  H
	RET
PRVRW1: DEC  H
	LD   A,H
	CPL
	AND  7
	RET  NZ
	LD   A,L
	SUB  &20
	LD   L,A
	RET  C
	LD   A,H
	ADD  A,8
	LD   H,A
	RET
;
; Sequence of graphics and text:-
; HL points to table with the format:-
;    &00	     Terminator
;    &01,AFA,W,H,V   Attribute file box (in characters)
;    &02,DFA,W,H     Display file box (W=bytes,H=pixels)
;    &03,PIX,FLG,TXT Text
;    &04,IMG,X,Y,Z   Image
; AFA is Attribute File Address, DFA=Display file address, V=Attribute
; W=Width, H=Height, PIX=pixel
;
SEQS:	LD   A,(HL)
	INC  HL
	OR   A
	RET  Z
	DEC  A
	JR   Z,SABOX
	DEC  A
	JR   Z,SSBOX
	DEC  A
	JR   Z,STEXT
	DEC  A
	JR   Z,SIMAGE
	RET		       ; Error anyway.
SABOX:	CALL SGRAB	       ; Attribute box
	LD   A,(HL)
	INC  HL
	PUSH HL
	LD   L,C
	LD   H,B
	CALL COLBOX
	POP  HL
	JR   SEQS
SSBOX:	CALL SGRAB	       ; DF box
	PUSH HL
	LD   L,C
	LD   H,B
	CALL BOX
	POP  HL
	JR   SEQS
STEXT:	CALL PRINT	       ; Text
	JR   SEQS
SIMAGE: CALL SGRAB	       ; Image
	LD   A,C
	LD   C,D
	LD   D,E
	PUSH HL
	CALL STATIC
	POP  HL
	JR   SEQS
SGRAB:	LD   C,(HL)
	INC  HL
	LD   B,(HL)
	INC  HL
	LD   D,(HL)
	INC  HL
	LD   E,(HL)
	INC  HL
	RET
;
PUTWRD: DB   PIY	       ; LD (DE),IY
	LD   A,L
	LD   (DE),A
	INC  DE
	DB   PIY
	LD   A,H
	LD   (DE),A
	INC  DE
	RET
;
INVERT: PUSH AF 	       ; Call with A=image no.
	LD   C,A	       ; and CARRY=1 for mirror, 0 for normal
	RRCA		       ; (ret's C if mirror, NC if already mir'd)
	RRCA
	RRCA		       ; Divide w. 8
	AND  7
	LD   E,A
	LD   D,0
	LD   HL,MIRFLG	       ; and index into flags
	ADD  HL,DE
	LD   A,C
	AND  7		       ; bit no. 0-7
	INC  A
	LD   C,(HL)	       ; C is source
	LD   B,A
	LD   A,&80	       ; A is abs. bit no. 
INV0:	RLCA		       ; Rot. a IN
	RR   C		       ; Rot. c OUT
	DJNZ INV0
	PUSH AF
	POP  BC
	LD   A,C	       ; get status
	AND  &01
	LD   E,B
	LD   D,A
	POP  BC
	LD   A,C
	AND  &01
	XOR  D		       ; check with requirement
	RRA
	RET  NC 	       ; and return if it's already there (with NC)
	LD   A,E	       ; .. is abs. bit
	XOR  (HL)	       ; swop the bit in the source byte
	LD   (HL),A	
	LD   A,B	       ; ..is the image's physcal no. or in other
	ADD  A,IMAGES/&100-1   ; words, the page offset from "IMAGES"
	LD   H,A
	LD   L,0
	LD   (TEMPSP),SP
	DI		       ; REDUNDANT
	LD   SP,HL
	LD   B,&40
	EXX
	LD   HL,MIRTAB
	EXX
INV1:	EXX
	POP  BC 	       ;    FORMAT: CBED
	POP  DE
	LD   A,D
	LD   L,C
	LD   D,(HL)
	LD   L,A
	LD   C,(HL)
	LD   A,E
	LD   L,B
	LD   E,(HL)
	LD   L,A
	LD   B,(HL)	       ;    FORMAT: DECB
	PUSH DE
	PUSH BC
	INC  SP
	INC  SP
	INC  SP
	INC  SP
	EXX
	DJNZ INV1
	LD   SP,(TEMPSP)
	EI
	SCF
	RET
;
RND:	PUSH HL
	PUSH DE
	PUSH BC
	LD   HL,(SEED)
	LD   DE,(SEED+2)
	LD   B,7
RND1:	LD   A,L
	AND  &48
	ADC  A,&38
	SLA  A
	SLA  A
	RL   D
	RL   E
	RL   H
	RL   L
	DJNZ RND1
	LD   (SEED),HL
	LD   (SEED+2),DE
	LD   A,L
	POP  BC
	POP  DE
	POP  HL
	RET
;
; Initialize SEED (32-bit) to the contents of A
;
SETSED: PUSH HL
	RLCA
	RLCA
	NEG
	LD   H,A
	LD   L,A
	LD   (SEED+2),HL
	LD   L,A
	LD   (SEED),HL	       ; Initialise variable
	CALL RND	       ; Call a couple of times to change it
	POP  HL
	RET
;
; Setup main character data when interchanging between rooms
;
SETUP:	PUSH AF 	     ; Save room to change to here
	LD   A,(CURGRL)
	CP   5		     ; Not if not main character
	POP  BC
	RET  NZ
	PUSH BC
	LD   A,(EXRODS)
	OR   A
	JP   Z,NOBOTH	     ; No rods below, phew and don't bother
	LD   A,(FLAGS3)
	RRA
	JR   C,BOTHER	     ; Jump if exit was a door.
	RRA
	JR   C,BOTHER	     ; Jump if exit was an elevator.
	LD   C,&FF
BOTHER: LD   A,(CFLOOR)      ; This is the floor which we just exited, or
	LD   (TMPRM2),A
RODC3:	LD   A,(TMPRM2)      ; if illegal (ie. zero). Else store here
	EXX
	CALL CHOLES	     ; Build up hole matrix
	EXX
	LD   HL,RODTAB	     ; This is the table of rod addresses in auxtab
	LD   A,(EXRODS)      ; and this is how many entries there are
	LD   B,A	     ; goes here
RODC1:	PUSH BC 	     ; save the counter
	LD   E,(HL)
	INC  HL
	LD   D,(HL)	     ; Get rods Z coord
	INC  HL 	     ; HL now points to new entry in RODTAB
	LD   A,D	     ; Check for entry being zero, which does not mean
	OR   E		     ; end of list, it means "Processed entry"
	JR   Z,RODC2	     ; Skip if rod is not a rod (tea and no tea)
	DEC  DE 	     ; Backwards to Y
	LD   A,(DE)	     ; Get Y
	AND  &38	     ; Process a fake Y div 8, so C becomes
	SUB  8
	LD   C,A	     ; Y coord div 8 mul 8!
	DEC  DE 	     ; Point to X
	LD   A,(DE)	     ; Get X
	AND  &38
	SUB  8
	RRCA
	RRCA
	RRCA		     ; Divide x by 8
	ADD  A,C	     ; So we now have X+(Y*8), or sfaw(x/8)+y
	LD   C,A
	LD   A,&3F
	SUB  C
	LD   C,A
	LD   B,HIGH(MATRIX)  ; Form 16-bit address
	LD   A,(BC)	     ; Get contents of that entry for x,y from matrix
	OR   A		     ; Hole there?
	JR   NZ,RODC2	     ; Jump if yes.
	DEC  HL
	DEC  HL 	     ; If no, the rod will stay there. Make HL point
	LD   (HL),A	     ; to same address in RODTAB
	INC  HL
	LD   (HL),A	     ; Make them both zero, ie. "processed entry"
	INC  HL 	     ; point to new entry again
	INC  DE
	INC  DE
	LD   (DE),A	     ; Nop out the Z (ie stand on ground)
	DEC  DE 	     ; Y
	DEC  DE 	     ; X
	DEC  DE 	     ; ACTN
	DEC  DE 	     ; IMAGE
	DEC  DE 	     ; ROOM/FLOOR
	LD   A,(TMPRM2)      ; This is where we came to ..
	DEC  A
	RRCA
	RRCA
	RRCA		     ; Make this the top 3 bits
	LD   C,A
	LD   A,(CURROM)      ; Always this room
	ADD  A,C
	LD   (DE),A	     ; insert!
	PUSH HL
	LD   A,(COLOUR)
	LD   E,A
	LD   A,(TMPRM2)
	LD   D,A
	CALL MIDWAY	     ; New rod on bottom of screen
	POP  HL
RODC2:	POP  BC
	DJNZ RODC1	     ; Do this for all the rods
	LD   HL,TMPRM2
	DEC  (HL)
	JR   NZ,RODC3	     ; ... and for all the levels!
NOBOTH: LD   HL,JETFLG
	BIT  1,(HL)	     ; Is the flame on?
	RES  1,(HL)
	CALL NZ,PPUTBK	     ; If it is, remove it and put old data back
	XOR  A
	LD   (NOBULL),A
	LD   (B2REMO),A
	POP  BC 	     ; B is new room
	LD   SP,(HIWATR)     ; Aaargh!
	RES  7,B
	PUSH BC
	LD   HL,(XYZADR)
	LD   DE,MANXYZ
	LDI
	LDI
	LDI
	LD   HL,(CURSPR)
	LD   DE,MANINF
	LD   BC,&10
	LDIR
	LD   A,(CURLOG)
	LD   L,A
	LD   H,B
	LD   DE,CURENT-4
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,DE
	LD   A,(HL)
	SUB  &60
	JR   C,NODOOR	     ; A hole, not a door
	PUSH HL
	LD   L,A
	LD   H,0
	LD   DE,EXTDIR
	ADD  HL,DE
	LD   A,(HL)	     ; True direction.
	LD   (MANFLG),A
	POP  HL
NODOOR: INC  HL 	     ; To X
	LD   IX,OLDX
	LD   DE,(XYZADR)
	LD   B,3
SETUPD: LD   A,(DE)
	SUB  (HL)	     ; This should be the relative difference
	LD   (IX+0),A
	INC  HL
	INC  DE
	INC  IX
	DJNZ SETUPD
	POP  BC
	LD   A,(CURROM)
	AND  &1F
GO4IT:	PUSH AF
	LD   A,1	     ; MAIN CHAR
	LD   (NODYNA),A
	LD   A,B	     ; ROOM NO
	CALL ROOM	     ; DRAW UP ROOM, RETS DE=1ST FREE ENTRY IN SPRTAB
	POP  IX 	     ; IXH=PREVIOUS ROOM NO.
	LD   HL,COUNTR
	XOR  A
	LD   (HL),A	     ; INIT
	LD   HL,CURENT	     ; LIST OF OBJECTS
	DB   PIX		
	OR   H
	JP   Z,SETUPC	     ; SKIP IF 1ST ROOM (START OF GAME)
SETUPA: LD   A,(HL)
	OR   A
	JP   Z,SETUPC	     ; NO EXIT BACKWARDS TO PREVIOUS ROOM
	PUSH HL
	LD   HL,COUNTR
	INC  (HL)
	LD   L,(HL)	     ; GET IMAGE
	LD   H,0
	LD   BC,ACTTAB-1
	ADD  HL,BC
	LD   A,(HL)	     ; GET ACTION SERVER FOR THE ENTRY
	OR   A
	POP  HL
	INC  HL
	JP   P,SETUPB	     ; NORMAL ACTION SERVER - NOT A ROOM CHANGE INDICTR
	AND  &7F	     ; GET ROOM
	DB   PIX
	CP   H		     ; LAST ROOM? (THE ONE WE JUST EXITED?)
	JP   NZ,SETUPB	     ; NOPE - TAKE ANOTHER
	PUSH HL 	     ; YES, THEN FIND ENTRY
	XOR  A
	LD   (OLDZ),A	     ; STAND ON GROUND WHEN ENTERING ROOM!
SETUPE: LD   HL,MANINF	     ; INSTALL MAIN SPRITE
	LDI		     ; NO. OF FRAMES
	LDI		     ; CURRENT FRAME
	LDI		     ; 4 FRAMES
	LDI
	LDI
	LDI
	LDI		     ; 3 VELOCITY VECTORS
	LDI
	LDI
	CALL PUTWRD	     ; ADDRESS OF IXYZ (IN IY)
	INC  HL 	     ; GO PAST ADDRESS
	INC  HL
	LDI		     ; FLAGS
	LDI
	LDI
	LDI		     ; GROL SERVER
	LDI		     ; ACTN SERVER
	EX   DE,HL
	LD   (HL),0	     ; END OF SPRITE DEF MARKER
	POP  HL
	LD   A,(MANINF+02)   ; 1ST FRAME (FREEZE FRAME)
	LD   (IY+0),A
	LD   DE,OLDX
	LD   A,(MANINF+06)   ; X VV
	ADD  A,(HL)	     ; ENTRY X POS. IN NEW ROOM
	LD   (IY+1),A	     ; NEW X
	LD   (DE),A
	INC  HL
	INC  DE
	LD   A,(MANINF+07)
	ADD  A,(HL)
	LD   (IY+2),A
	LD   (DE),A
	INC  HL
	INC  DE
	LD   A,(DE)
	ADD  A,(HL)
	LD   (IY+3),A
	LD   (IY+4),0
	LD   (DE),A
	LD   A,5
	CALL UPDACT
	LD   HL,LAPTAB
	LD   DE,LAPTAB+1
	LD   BC,&0F
	LD   (HL),B
	LDIR
	LD   A,(MANFLG)
	LD   (INITRN),A
	LD   HL,JETFLG
	RES  7,(HL)	     ; Flag disallowing turning
	JP   UDINIT	     ; Restart game
SETUPB: INC  HL
	INC  HL
	INC  HL
	JP   SETUPA
SETUPC: LD   HL,FLAGS3	     ; OK, no door leading back was found. This can
	BIT  0,(HL)	     ; mean only two things. EITHER the game has just
	RES  0,(HL)	     ; started and there will of course be no door, in
	JR   Z,SETUPF	     ; which case the object is placed in the middle of
	BIT  1,(HL)	     ; the room. OR the man has bumped himself down a
	RES  1,(HL)	     ; hole
	JR   Z,ISHOLE
	XOR  A		     ; If elevator, stand on ground and
	LD   C,A	     ; have no Z vv
	EXX
	LD   HL,SHTFCE
	LD   DE,MANINF+2     ; Also make him face out.
	LD   BC,4
	LDIR
	EXX
	PUSH AF
	LD   B,DOOR_OPENING
	CALL SOUND	     ; Sounds like a door opening...
	POP  AF
	JR   ISHOL2
ISHOLE: EXX
	LD   HL,MANDAT
	LD   DE,MANINF
	LD   BC,2
	LDIR
	LD   C,4
	ADD  HL,BC
	EX   DE,HL
	ADD  HL,BC
	EX   DE,HL
	LD   C,10
	LDIR
	EXX
	LD   C,&F8
	LD   HL,MANINF+11
	SET  7,(HL)	     ; "In The Air"
	LD   A,64
ISHOL2: LD   HL,MANXYZ
	LD   (MANXYZ+2),A
	LD   A,C
	LD   (MANINF+12),A   ; Make his Z velocity vector go mad
	JR   SETUPS
SETUPF: EXX
	LD   HL,MANDAT
	LD   DE,MANINF
	LD   BC,&10
	LDIR
	EXX
	LD   HL,MIDSCR
SETUPS: PUSH HL 	     ; in the middle. Save on stack - used later
	LD   HL,OLDX
	XOR  A
	LD   (HL),A	     ; No relative offset
	INC  HL
	LD   (HL),A
	INC  HL
	LD   (HL),A
	LD   (MANINF+6),A    ; No velocity vectors either (redundant anyway)
	LD   (MANINF+7),A
	JP   SETUPE
;
TITLE	'IMAGE5: Sprite controllers, gravity, GROLs and ACTNs'
EJECT
;
COLIS2: PUSH IX 	       ; Same as COLISN but with the index registers 
	PUSH IY 	       ; saved.
	CALL COLISN
	POP  IY
	POP  IX
	RET
;
BRK:	LD   A,&7F	       ; Check for break (caps shift and space) pressed
	IN   A,(&FE)	       ; Caps shift is at port &7FFE, Bit 0
	RRA			
	RET  C		       ; Return with carry set if no break
	LD   A,&FE	       ; Space is at port &FEFE, Bit 0
	IN   A,(&FE)	       ; (this is not buffered as it may be called
	RRA		       ; from anywhere!)
	RET		       ; Back with carry clear if break.
;
RESREG: LD   DE,(XYZADR)       ; Restore X, Y and Z to their previous values.
	LD   HL,OLDX
	LDI
	LDI
	LDI		       ; PS: This does NOT affect neither the Z or C
	RET		       ; flag and should be called from an init GROL.
;
; MAIN ACTION SERVER ENTRY POINT:
; Entered when an action is required after a collision between a dynamic
; or static sprite and a dynamic sprite. Uses 'passive' object's ACTION server
; number. Bit 5 of (IY) is SET when coming from GRAVITY - RESET when coming
; from a GROL server.
;
ACTION: LD   A,(CURLOG)        ; Action server selected index and jump.
	LD   L,A
	LD   H,0
	LD   DE,ACTTAB-1
	ADD  HL,DE	       ; Index action number into Actions Table.
	LD   A,(HL)	       ; Get action server associated to the current
	LD   (SERVNO),A
	OR   A		       ; collided logic object.
	RET  Z		       ; No action.
	JP   M,ACTN3	       ; over 127 is room change (bits 0-6 is new room)
	LD   L,A	       ; 1-127 are normal actions.
	LD   H,0	       ; Now index into the address table:
	LD   DE,AADTAB-2       ; Offset is Actions Address Table,
	ADD  HL,HL	       ; (wordsize entries)
	ADD  HL,DE		
	LD   A,(HL)	       ; Get address
	INC  HL
	LD   H,(HL)
	LD   L,A
	LD   DE,ACTBCK	 
	PUSH DE 	       ; Return there afterwards.
	JP   (HL)	       ; and swop their places!
ACTBCK: RET
;
; DIRECTION ALTERING SEQUENCES:
; For 'random left-right' turn, left turn, right turn and bouncing.
;
TRNRND: CALL RND	       ; Get a random number...
	AND  3
	JR   Z,BOUNCE
	DEC  A
	JR   Z,TRNRGT
	DEC  A
	JR   NZ,TRNRND
;
TRNLFT: CALL RESREG	       ; Turn left by setting X=Y and Y=-X
	CALL ACTION	       ; Restore X,Y,Z to previous and perform action
	LD   A,(IX+0)	       ; upon the collided object.
	LD   B,(IX+1)
	NEG		       ; A=-(B)
	LD   (IX+1),A
	LD   (IX+0),B
	LD   HL,(CURSPR)       ; Tell sprite data that another image may now
	LD   A,(HL)	       ; be required when printing.
	SUB  &40
	LD   (HL),A
	RET
;
TRNRGT: CALL RESREG	       ; Turn right by setting X=-Y, Y=X
	CALL ACTION	       ; Restore X,Y,Z and perform action.
	LD   A,(IX+1)
	LD   B,(IX+0)
	NEG
	LD   (IX+0),A
	LD   (IX+1),B
	LD   HL,(CURSPR)
	LD   A,(HL)	       ; Tell sprite data that the sprite has turned
	ADD  A,&40	       ; 90 degrees.
	LD   (HL),A
	RET
;
BOUNCE: CALL RESREG	       ; Bounce back by setting X=-X and Y=-Y
	CALL ACTION
	LD   A,(IX+0)
	NEG
	LD   (IX+0),A
	LD   A,(IX+1)
	NEG
	LD   (IX+1),A
	LD   A,(HL)
	LD   HL,(CURSPR)
	LD   A,(HL)
	ADD  A,&80	       ; Tell sprite data that another image might be
	LD   (HL),A	       ; considered after turning 180 degrees.
	RET
;
; GRAVITY AFFECTIONS:
; Call constantly to make a dynamic object falling. True acceleration is
; performed to make the fall realistic. Bit 6 of User Byte 0 is used internally
; as a flag.
;
GRVITY: LD   HL,FLAGS2	       ; Check if mutual collision is present,
	BIT  2,(HL)	       ; and if so, don't bother.
	RET  Z
	DEC  (IY+1)	       ; Z-vector distutbances
	LD   A,(IY+1)
	JP   P,GRDST	       ; If falling up (!), all is OK
	CP   &F8
	JR   NC,GRDST	       ; Nothing can fall faster than this!
	LD   A,&F8
	LD   (IY+1),A
GRDST:	LD   (OLDACC),A
	RES  6,(IY+0)	       ; No floor or roof detection assumed.
	SET  5,(IY+0)	       ; Coming from GRVITY, not from GROL. (4 ACTN)
	LD   HL,(XYZADR)
	INC  HL
	INC  HL
	LD   A,(HL)	       ; Get Z
	PUSH AF
	ADD  A,(IY+1)	       ; add acceleration from sprite data
	JP   P,GRAV03
	SUB  (HL)	       ; If it hit the floor, get accelleration again
	LD   A,&7F	       ; make it top position if jumping up,
	JP   P,GRAV04
	XOR  A		       ; bottom position if falling down.
GRAV04: RES  7,(IY+0)
	SET  6,(IY+0)	       ; Floor/roof detection flag.
GRAV03: LD   (HL),A	       ; Insert new Z
	LD   A,(SPRCNT)
	PUSH HL
	CALL COLIS2	       ; Consider collision with other objects
	POP  HL
	POP  BC 	       ; B is old Z
	EX   AF,AF"            ; Save collision flag
	BIT  6,(IY+0)
	JP   NZ,GRAV05	       ; Keep the Z if just set by floor/roof collision
	EX   AF,AF"
	JP   Z,GRAV01	       ; Go back if all was ok.
	LD   (HL),B	       ; Put back old Z
GRAV05: LD   A,(IY+1)	       ; Get acceleration
	LD   (IY+1),0	       ; then make it zero - Object has collided.
	OR   A		       ; Still, check whether it was head or tails
	JP   P,GRAV01	       ; that became squashed. Jump if head.
GRAV06: LD   A,(EXTZ)	       ; Look at the top of collided object.
	INC  A
	LD   (HL),A	       ; add 1 and make it current Z
	PUSH HL 	       ; now enter a loop - if more objects are on
	LD   A,(CURLOG)        ; the top of the first. Save present CURLOG.
	PUSH AF
	LD   A,(SPRCNT)        ; Always same sprite.
	CALL COLIS2	       ; Check new position,
	POP  BC 	       ; get logical collided object back in B
	POP  HL
	JP   NZ,GRAV06	       ; If collision was detected, we go again.
	RES  7,(IY+0)	       ; Not falling anymore.
	LD   A,B
	LD   (CURLOG),A        ; Tell Gravity Collision Table which one 
	CALL GRVCOL	       ; has collided.
	LD   A,(HL)
	DEC  A
	LD   (EXTZ),A	       ; Finally, this must be the new EXTZ
	JP   ACTION	       ; Consider further actions upon collision.
GRAV01: XOR  A		       ; Insert a zero in Gravity Collision Table
	CALL GRVCOL
	SET  7,(IY+0)	       ; Return with bit 7 set if still falling, ie.
	BIT  6,(IY+0)	       ; if there was "roof collision"
	RET  Z
	RES  7,(IY+0)	       ; but stop falling if standing on the floor.
	RET
;
; Inform sprite data that retrospective collision might occur. This is a
; pretty bad state.
;
GRVCOL: LD   B,A	       ; Read/write collided object - if it was to
	LD   A,(SPRCNT)        ; be taken.
	LD   E,A
	LD   D,0
	PUSH HL
	LD   HL,GRCTAB
	ADD  HL,DE	       ; Index into Gravity Collision Table
	LD   A,(HL)
	LD   (HL),B	       ; Insert new, return with old.
	POP  HL
	RET
;
ACTN1:	RET
;
ACTN2:	BIT  5,(IY+0)
	RET  Z
	CALL CLCSPR
	LD   A,(HL)
	AND  7
	ADD  A,2
	LD   E,A
	LD   D,0
	ADD  HL,DE
	LD   A,(HL)	; XVV
	ADD  A,(IX+0)
	LD   (IX+0),A	; MAKE MAIN XVV+COL XVV
	INC  HL
	LD   A,(HL)
	ADD  A,(IX+1)
	LD   (IX+1),A
	INC  HL
	LD   A,(HL)
	ADD  A,(IX+2)
	LD   (IX+2),A
	RET
;
ACTN3:	AND  &7F	       ; Room change. Remove "room change" identifier
	JP   SETUP	       ; so b0-6 holds the actual room.
;
CLCSPR: LD   A,(CURLOG)        ; Find collided object's address.
	LD   HL,NOSTAT	       ; Exit, HL is sprites address, A is sprites 
	SUB  (HL)	       ; physical number.
	LD   L,A
	LD   H,0
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL
	LD   DE,SPRTAB-&10
	ADD  HL,DE
	RET
;
ACTN4:	BIT  5,(IY+0)	       ; Back if NOT coming from GRVITY.
	RET  NZ
	CALL CLCSPR
	LD   A,(HL)
	AND  7
	ADD  A,2
	LD   E,A
	LD   D,0
	ADD  HL,DE
	LD   A,(IX+0)
	LD   (HL),A
	INC  HL
	LD   A,(IX+1)
	LD   (HL),A
	INC  HL
	LD   A,(IX+2)
	LD   (HL),A
	CALL CHKGR5
	LD   HL,UREM	       ; If main chr pushed, make him radiate!
	DEC  (HL)
	RET
;
ACTN5:	CALL CHKGR5
	LD   A,(DORWAY)        ; Doorway pole type hit
	LD   B,A	       ; Bit 0 is left/right pole
	RRC  A		       ; RRCA/RRA don't affect zero flag, RRC A does.
	RET  Z		       ; but the zero flags is set if no doorway.
	AND  7		       ; Rest (0 to 7) is direction
	LD   C,A
	LD   A,(MANFLG)        ; Get direction
	CP   C
	JR   Z,ACTN52	       ; Same direction: OK
	INC  A
	AND  7
	CP   C
	JR   Z,ACTN51	       ; Almost the same?
	DEC  A
	DEC  A
	AND  7
	CP   C
	RET  NZ 	       ; Other near direction?
ACTN51: LD   A,C
	LD   (MANFLG),A        ; Insert new direction
	RET
ACTN52: ADD  A,A	       ; A is direction (*2)
	LD   E,A
	LD   D,0
	LD   HL,VECTAB	       ; Vectors table offset
	ADD  HL,DE	       ; Index
	LD   E,(HL)	       ; Grab X and Y vectors
	INC  HL
	LD   D,(HL)
	LD   HL,(XYZADR)       ; Grab address of X and Y
	RR   B		       ; Bit zero denotes the left or right pole.
	JP   NC,ACTN53	       ; Right - jump.
	LD   A,(HL)
	LD   C,A	       ; Save X in C
	ADD  A,D	       ; Add Yv
	LD   (HL),A
	INC  HL
	LD   A,(HL)
	LD   B,A	       ; Save Y in B
	SUB  E		       ; Substract Xv, ie. X=X+Yv; Y=Y-Xv
	LD   (HL),A
	JR   ACTN54
ACTN53: LD   A,(HL)
	LD   C,A	       ; Save X in C
	SUB  D		       ; Substract Yv
	LD   (HL),A
	INC  HL
	LD   A,(HL)
	LD   B,A	       ; Save Y in B
	ADD  A,E	       ; Add Xv
	LD   (HL),A
ACTN54: PUSH HL
	PUSH BC
	LD   A,(SPRCNT)
	CALL COLIS2
	POP  BC
	POP  HL
	RET  Z		       ; Z=No collision
	LD   (HL),B
	DEC  HL
	LD   (HL),C
	RET
;
ACTN6:	LD   HL,CFLOOR	       ; One Floor Up
	INC  (HL)
	LD   A,(CURROM)
	JP   SETUP
;
ACTN7:	LD   HL,CFLOOR	       ; One Floor Down or die if zero (ie. bighole)
	DEC  (HL)
	JP   Z,GAMOVR
	LD   B,GUNFIRE_4
	CALL SOUND
	LD   A,(CURROM)
	JP   SETUP
;
ACTN8:	BIT  5,(IY+0)	       ; Add Z ACTION server:
	RET  Z		       ; Don't bother if not coming from GRAVITY,
	LD   HL,(CURXYZ)
	INC  HL
	INC  HL
	INC  (HL)	       ; Increase Z and in effect make the object
	LD   A,(HL)
	CP   44
	JR   NC,ACTN8B
	PUSH HL 	       ; 'rise'
	LD   A,(SPRCNT)        ; get current sprite number,
	LD   HL,(XYZADR)
	INC  HL
	INC  HL
	INC  (HL)	       ; Also increase that Z,
	PUSH HL
	CALL COLIS2	       ; Did it collide?
	POP  HL
	JP   Z,ACTN8A	       ; No, then we're a step up -
	DEC  (HL)	       ; Yes, give back both Zs
	POP  HL
ACTN8B: DEC  (HL)		
	RET
ACTN8A: POP  HL
	RET
;
ACTN9:	LD   A,(CURGRL)
	CP   4
	RET  Z
	DEC  A
	RET  Z
	LD   HL,FLAGS2
	BIT  0,(HL)
	RET  NZ
	LD   A,(CURPHY)
	CP   &10
	RET  NC
	LD   B,5
;
; Enter AARRGH with B=edge tolerance for size of the hole. AARRGH is the fall-
; through code.
;
AARRGH: LD   A,(SERVNO)
	LD   (SOURCE),A
	LD   A,(CURLOG)        ; Fall-through-a-hole ACTION server:
	LD   L,A
	LD   H,0	       ; Collided object
	ADD  HL,HL
	ADD  HL,HL
	LD   DE,CURENT-3       ; X-coord offset
	ADD  HL,DE
	LD   DE,(XYZADR)       ; Get main object's coords
	LD   A,(DE)
	SUB  (HL)	       ; Check difference
	RET  C		       ; Must be within range 0-4 for a 'fall' to
	CP   B		       ; occur, ie. pretty close to the edge.
	RET  NC
	INC  HL 	       ; Forward to Y
	INC  DE
	LD   A,(DE)
	SUB  (HL)	       ; Y difference must also be between 0 and 4
	RET  C
	CP   B
	RET  NC
	INC  HL
	INC  DE
	LD   A,(HL)
	ADD  A,4
	LD   (DE),A	       ; Could use B/2 here, but who cares?
	DEC  HL
	DEC  DE
	LD   A,(HL)
	LD   (DE),A
	DEC  HL
	DEC  DE
	LD   A,(HL)
	LD   (DE),A
	SET  4,(IY+0)	       ; Falling
	LD   A,&20
	LD   (FALLRW),A
	LD   A,(OLDACC)        ; Put Old Acceleration
	NEG
	BIT  5,(IY+0)	       ; Assume coming from GRVITY, use the fall factor
	JR   NZ,FFROMG	       ; Good assumption
	XOR  A		       ; Else start off from zero.
FFROMG: LD   (FACCEL),A        ; into Fall acceleration
	LD   (IX+0),0
	LD   (IX+1),0	       ; Zero X/Y Velocity vectors.
	LD   HL,FLAGS2
	SET  0,(HL)	       ; Something is falling
	LD   HL,AUXFLG
	RES  2,(HL)
	LD   A,(CURGRL)
	CP   5		       ; 2,AUXFLG=1, MEANS ROD FALLING
	JR   Z,FALLIN	       ;	 =0, MEANS MAN FALLING
	SET  2,(HL)
FALLIN: LD   HL,FACCEL
	INC  (HL)	       ; Increase acceleration
	LD   A,(FALLRW)
	SUB  (HL)
	LD   (FALLRW),A
	RET  P
	LD   HL,FLAGS2
	RES  0,(HL)
	LD   HL,AUXFLG	       ; Test origin (nature of object)
	BIT  2,(HL)
	JP   Z,ACTN7	       ; ACTN7 is One Floor Down - jump if main char
	CALL FJERN1	       ; else remove the object
	LD   A,(SOURCE)
	CP   &C 	       ; Big hole?
	JR   NZ,NMHLE	       ; No, then assume normal hole and go down
	LD   (HL),&FF	       ; "Saved"
	LD   HL,NORODS
	DEC  (HL)	       ; =0 if last
	JR   NZ,MOREWK
	LD   HL,(TIMER)
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL
	EX   DE,HL	      ; Give bonus according to time left
	CALL ADDSC
	JP   WONGAM
MOREWK: LD   A,(COLOUR)
	LD   E,A
	LD   D,0
	CALL CHNGRD
	LD   A,(NORODS)
	NEG
	ADD  A,31
	ADD  A,A
	ADD  A,A
	ADD  A,A
	LD   L,A
	LD   H,0
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL
	EX   DE,HL
	CALL ADDSC
	RET
NMHLE:	LD   B,GUNFIRE_4
	CALL SOUND
	LD   A,(CURROM)
	SUB  &20	       ; Down a floor - C here if last !
	LD   C,A
	LD   A,(CFLOOR)
	RRCA
	RRCA
	RRCA
	ADD  A,C
	LD   (HL),A	       ; Insert room/floor
	LD   DE,5	       ; +5 past image,actn,x,y to Z
	ADD  HL,DE
	EX   DE,HL	       ; DE points to Z coord in auxtab entry
	LD   HL,(RTDPTR)       ; Insert Z address here!
	LD   (HL),E
	INC  HL
	LD   (HL),D	       ; This is the address of Z in auxtab
	INC  HL
	LD   (RTDPTR),HL       ; pointer back
	LD   HL,EXRODS
	INC  (HL)	       ; One more!
	RET
;
; Call CHNGRD with E=rod colour to find, D=rod colour to change to
; Call CHNGRF with D=floor no. to find, D=floor no. to change to
;
; Uses A,DE,HL,B
;
CHNGRF: LD   A,E
	XOR  7
	JR   NZ,CHCH1
	LD   A,7
CHCH1:	LD   E,A
MIDWAY: LD   A,D
	XOR  7
	JR   NZ,CHCH2
	LD   A,7
CHCH2:	LD   D,A
CHNGRD: LD   HL,&5AD7
	LD   A,E
	OR   &40
	LD   B,9
WITS1:	CP   (HL)
	JR   Z,WITS2
	INC  L
	DJNZ WITS1
	LD   L,&F7
	JR   WITS1
WITS2:	SET  6,D       ; BRIGHT
	LD   (HL),D
	RET
;
ACTNA:	CALL CHKGR5
	LD   A,(CFLOOR)
	CP   7
	RET  Z
	LD   A,1
ACTNAA: LD   (ELEVUD),A        ; Insert up/down elevator indicator so we
	LD   A,1	       ; later know which arrow to print (!)
	LD   (ELESTA),A        ; Elevator status = 1
	LD   A,6
	LD   (ELECN2),A        ; No. of steps to take
	LD   A,3
	LD   (ELECNT),A
	LD   HL,(XYZADR)
	INC  HL
	INC  HL
	LD   (HL),0
	LD   B,DOOR_OPENING
	JP   SOUND
;
ACTNB:	CALL CHKGR5
	LD   A,(CFLOOR)
	DEC  A
	RET  Z
	LD   A,2
	JR   ACTNAA
;
ACTNC:	LD   A,(CURGRL)
	CP   4
	RET  Z
	DEC  A
	RET  Z
	LD   A,(CFLOOR)
	DEC  A
	RET  NZ
	LD   HL,FLAGS2
	BIT  0,(HL)
	RET  NZ
	LD   B,15	  ; Fall-through action-server for the BIG holie.
	JP   AARRGH	  ; This has a tolerance level of 15.
;
ACTND:	CALL CHKGR5	  ; Filter through only GROL 5's
	LD   HL,FLAGS3
	BIT  2,(HL)
	RET  NZ 	  ; Already held
	LD   HL,FLAGS3
	SET  2,(HL)	  ; OK, flag "got a rod, start giving him cancer!"
	CALL FJERN
	LD   (HL),&FF	  ; Tell AUXTAB that rod is held.
	LD   (RODADR),HL
	LD   A,(COLOUR)
	LD   (RODCOL),A
	LD   B,GUNFIRE_3
	JP   SOUND
;
; Remove any AUXTAB object from the screen and from the tables.
; FJERN removes current object in collision (when called from ACTN)
; FJERN1 removes current object (when called from a GROL)
; HL points to AUXTAB entry on exit
;
FJERN:	LD   A,(CURLOG)
	LD   HL,NOSTAT
	SUB  (HL)
	LD   B,&FF
	JR   FJERN2
FJERN1: LD   A,(SPRCNT)
	LD   B,0
FJERN2: LD   L,A
	LD   H,0
	ADD  HL,HL	  ; x16 for each entry
	ADD  HL,HL
	ADD  HL,HL
	ADD  HL,HL
	LD   DE,SPRTAB-&10
	ADD  HL,DE
	LD   C,A
	LD   A,(HL)	  ; Sprite's 1st byte
	AND  7		  ; No. of entries in Frame Buffer (animation sequence)
	ADD  A,7	  ; +offset to reach User Byte 0
	LD   E,A	  ; D is already 0
	LD   D,0
	ADD  HL,DE
	INC  B
	JR   NZ,FJERN3	  ; Skip if removal of the screen contents is not req.
	RES  1,(HL)	  ; Allow for dormant dynamic object to be cleared
	LD   A,C	  ; Get sprite number back
	PUSH HL 	  ; Save address of user byte 0
	PUSH IX 	  ; IMPORTANT saving of these pointers
	PUSH IY
	AND  A		  ; Unplot (it's already cleared, but i wanna be sure)
	CALL DYNAMC	  ; Remove it from screen
	POP  IY
	POP  IX 	  ; Get pointers to velocity vectors back again
	POP  HL
FJERN3: SET  1,(HL)	  ; Dormant Dynamic Object again
	PUSH HL 	  ; Save that for a while
	DEC  HL
	LD   A,(HL)	  ; This is the high-byte of the I,XYZ entry in CURENT
	DEC  HL
	LD   L,(HL)	  ; This is the low byte
	LD   H,A	  ; form 16-bit address
	LD   (HL),&FF	  ; Dormant in CURENT
	POP  HL 	  ; Reget the address of User Byte 0
	INC  HL 	  ; Now the Z acceleration
	INC  HL 	  ; Now LSB of address pointing to AUXTAB
	LD   A,(HL)
	INC  HL 	  ; (i have just realised that this will only work
	LD   H,(HL)	  ;  with objects which are in AUXTAB - oops!)
	LD   L,A
	RET
;
ACTNE:	LD   A,(UREM)	  ; Decrease radiation level
	INC  A
	CP   &40
	RET  NC
	LD   (UREM),A
	RET
;
ACTNF:	CALL CLCSPR
	LD   DE,5
	ADD  HL,DE
	LD   (HL),1
	LD   DE,(CURXYZ)
	INC  DE
	INC  DE
	LD   A,(DE)
	CP   40
	JR   NC,ACTNF2
	LD   (IX+2),2
	RET
ACTNF2: LD   (HL),0
	LD   (IX+2),0
	RET
;
ACTNG:
ACTNH:	RET
;
; This is a pretty vital routine, checking for the current GROL being 
; something. If it is, the routine returns. If it isn't, the routine
; returns to the address above the current stack entry, ie. one level
; above. Call CHKGRL with B=grol number, or alternatively CHKGR5 to check for
; GROL 5 (main char GROL)
;
CHKGR5: LD   B,5
CHKGRL: LD   A,(CURGRL)
	CP   B
	RET  Z
	INC  SP 	
	INC  SP
	RET
;
ABANDN: JP   ENDGAM
;
WONGAM: DI
	XOR  A
	CALL CLRALL
	CALL TOFFMU
	LD   HL,WONTXT
	CALL PRINT
	LD   A,4+16
	CALL RAMSEL

IF ONE28

	LD   HL,&C000
	LD   A,8
	LD   BC,&0301
	LD   DE,192

ELSE

	CALL MAKET2
	LD   HL,FARVER
	LD   A,8
	LD   BC,&0303
	LD   DE,1

ENDIF

	CALL MSETUP
	LD   HL,AUXFLG
	RES  0,(HL)
	EI
	CALL GETOFF
	CALL GETKEY
	LD   A,0+16
	CALL RAMSEL
	JP   ENDGAM
;
GROL1:	CALL NZ,TRNRND	   ; Turn Random if there was a collision
	LD   A,18
GROL1A: LD   (HOLDZ),A
	LD   HL,FLAGS2	   ; Main flags for the room 
	BIT  6,(HL)	   ; Is robot exploding?
	JP   NZ,REXPLO	   ; Jump if so
	BIT  4,(HL)	   ; Has it just been hit?
	JR   Z,SALIVE
	RES  4,(HL)
	PUSH HL
	LD   HL,ENERGY
	DEC  (HL)
	POP  HL
	JP   M,ROBHIT
	LD   A,(IX+0)
	OR   (IX+1)
	CALL Z,RSTKNX	   ; If hit and standing, make him walk another way
SALIVE: LD   DE,HEROXY
	LD   HL,(XYZADR)
	LD   A,(DE)	   ; Robot's X
	SUB  (HL)	   ; Minus man's X
	JR   Z,SAMPX	   ; jump if same!
	LD   A,1	   ; assume man is furthest away.
	JR   NC,SAMPX
	NEG
SAMPX:	LD   C,A
	INC  HL
	INC  DE
	LD   A,(DE)
	SUB  (HL)	   ; Check Ys
	JR   Z,SAMPY
	LD   A,1
	JR   NC,SAMPY
	NEG
SAMPY:	LD   B,A
	LD   HL,VECTAB
	LD   D,&FF
SRCHV:	INC  D
	LD   A,(HL)
	INC  HL
	LD   E,(HL)
	INC  HL
	CP   C
	JR   NZ,SRCHV
	LD   A,E
	CP   B
	JR   NZ,SRCHV
	LD   A,D
	AND  7
	LD   (TMPDIR),A
	RR   D
	JR   NC,ZAO9
	XOR  6
ZAO9:	LD   (RANGLE),A
	PUSH BC
	CALL PHEAD
	POP  BC
	LD   A,(RANGLE)
	RRA		   ; Do all compares for killing here
	JR   C,KILLL	   ; This is for real "axis" coordinate clash
	LD   HL,(XYZADR)
	LD   A,(HEROXY)
	LD   D,A
	SUB  (HL)
	LD   E,A
	INC  HL
	LD   A,(HEROXY+1)  ; Check only x and y coords - z is made redundant
	SUB  (HL)
	CP   E
	JR   Z,KILLL	   ; This is for straight coordinate clash
	ADD  A,(HL)
	ADD  A,D
	LD   D,A
	LD   A,(HL)
	DEC  HL
	ADD  A,(HL)
	CP   D
	JR   Z,KILLL
	JR   RSTKEP
KILLL:	LD   HL,(XYZADR)
	INC  HL
	INC  HL
	LD   A,(HL)
	CP   48
	JR   Z,RSTKNX
	LD   HL,RDELAY
	DEC  (HL)
	JR   Z,RSTKNX	   ; Don't fire anymore!
	JP   P,KILCO
KILJF:	LD   A,R
	AND  7
	INC  A
	LD   (HL),A
KILCO:	LD   A,C
	ADD  A,A
	ADD  A,8
	LD   C,A
	RL   B
	LD   A,(HOLDZ)	   ; Z allignment
	CALL ROBULL
	LD   B,GUNFIRE_2
	CALL SOUND
RSTSHT: LD   E,(IX+0)
	LD   D,(IX+1)
	LD   A,E
	OR   D
	JP   Z,GRVITY
STSTIL: LD   (ROBVEC),DE
	LD   (IX+0),0
	LD   (IX+1),0
	JP   Z,GRVITY
RSTKNY: LD   A,R
	AND  3
	DEC  A
	CP   2
	RET  NZ
	JR   RSTKNY
RSTKNX: CALL RSTKNY
	LD   (IX+0),A
	CALL RSTKNY
	LD   (IX+1),A
	JP   GRVITY
RSTKEP: LD   A,(IX+0)
	OR   (IX+1)
	JP   NZ,GRVITY
GIVITB: LD   DE,(ROBVEC)
	LD   (IX+0),E
	LD   (IX+1),D
	JP   GRVITY
ROBHIT: CALL RND
	AND  4		   ; Get bit 4 only (difference from expl#1 to expl#2)
	ADD  A,EXPLODE_1+OVERRULE
	LD   B,A
	CALL SOUND
	SET  6,(HL)	   ; Else signal "robot is exploding"
	LD   A,15
	LD   (RSTATE),A    ; Initialise explosion counter
	LD   DE,100	   ; this must be <256!
	LD   (IX+0),D	   ; Zero Velocity vectors
	LD   (IX+1),D
	CALL ADDSC	   ; Give a BOOST 100 SCORE !! (decimal)
REXPLO: LD   A,(CURROM)
	LD   D,A
	LD   A,(CFLOOR)
	DEC  A
	DEC  D
	CALL INDX21
	LD   DE,ROBOTP
	ADD  HL,DE
	CPL
	AND  (HL)
	LD   (HL),A
	LD   A,(RSTATE)    ; Get current state of explosion
	LD   B,A	   ; Save it here
	SRL  A		   ; Div. 2 to make it stay longer on screen
	SUB  4		   ; Make it 3 to -3, so it goes IMPLODE-EXPLODE
	JR   NC,REXPL1	   ; Jump if implosion
	NEG		   ; Invert if explosion
REXPL1: ADD  A,1+(GEXPLO-DIMTAB)/4
	LD   HL,(XYZADR)
	DEC  HL
	LD   (HL),A
	LD   A,B	   ; Get RSTATE back
	DEC  A		   ; 1 less
	LD   (RSTATE),A    ; save that
	DEC  A		   ; check if end reached - 1 is end, thus 0 here
	RET  NZ 	   ; Return if not.
	LD   (HL),&FF	   ; Remove entry in CURENT
	LD   HL,(CURSPR)
	LD   A,(HL)
	INC  HL
	INC  HL
	LD   D,(HL)
	AND  7
	ADD  A,5
	LD   C,A
	LD   B,0
	ADD  HL,BC
	RES  3,(HL)
	RES  2,(HL)	   ; (D)DO it! - make it dormant.
	SET  1,(HL)	   ; Remove entry here also
	LD   HL,FLAGS2
	RES  6,(HL)	   ; Not exploding anymore
	LD   HL,FLAGS4
	RES  3,(HL)
	LD   A,D
	CP   1+(GRBOT3-DIMTAB)/4
	RET  NZ
	LD   HL,BABIES
	DEC  (HL)
	RET
;
PHEAD:	LD   A,(RANGLE)
	RRCA
	RRCA
	RRCA
	LD   E,A
	LD   D,0	   ; Form 16-bit index
	LD   HL,ROBOTH	   ; This is the offset
	ADD  HL,DE
	LD   DE,IMAGES+((GRBOT1-DIMTAB)/4)*&100+1 ; Address of graphic 1. byte,
	CALL PHEAD0	   ; (not 0th) - call insertion for all three images
	LD   E,&81	   ; This is for the masks
PHEAD0: LD   B,8	   ; Eight lines to handle
PHEAD1: CALL MOVE3B	   ; Move one byte to all three images
	CALL MOVE3B	   ; Move another byte to the three images
	INC  E
	INC  E		   ; Forward to next byte, ie. 1st byte on next line
	DJNZ PHEAD1	   ; Do all that 8 times
	RET		   ; and finish
MOVE3B: LD   C,D
	LD   A,(HL)	   ; Get graphic byte
	LD   (DE),A	   ; Store in all three graphic images
	INC  D
	LD   (DE),A
	INC  D
	LD   (DE),A
	LD   D,C
	INC  HL
	INC  E		   ; Next byte on the line
	RET		   ; Inc this here for ease of use and return
;
GROL2:	RET
;
GROL3:	RET  Z
	JP   TRNLFT
;
GROL4:	JP   Z,GROL4A	     ; BOUNCING...
	CALL TRNRND
GROL4A: LD   HL,(XYZADR)
	INC  HL
	INC  HL
	LD   A,(HL)
	CP   32
	LD   A,8
	JR   NC,GROL4B
	BIT  7,(IY+0)
	JP   NZ,GROL1A
	SET  7,(IY+0)
	LD   (IY+1),8
	JP   GROL1A
GROL4B: LD   (HL),32
	JP   GROL1A
;
ADDSC:	LD   HL,(SCORE)
	ADD  HL,DE
	LD   (SCORE),HL
	LD   DE,&50D1
	JP   CSCORE
;
TITLE	'IMAGE5a: Sprite controllers, GROLs and ACTNs and specials'
;
GROL5:	EX   AF,AF"            ; Save zero
	LD   HL,(XYZADR)
	LD   DE,HEROXY
	LDI
	LDI
	LD   A,(CURGRL)
	LD   (MANGRL),A
	LD   A,(NEWROM)
	LD   HL,FLAGS4
	BIT  0,(HL)
	JP   NZ,SETUP
	LD   A,(ELESTA)        ; Elevator status
	OR   A		       ; Check for entering elevator
	JR   Z,NOELEV
	LD   B,&01
	CP   2		       ; Man walking in?
	JR   Z,GONOK	       ; Return if standing still
	CP   5
	RET  NZ
	LD   B,&FF	       ; So this is walking out!
GONOK:	LD   (IX+0),B
	LD   (IX+1),B	       ; Make him walk
	XOR  A
	LD   (IX+2),A
	LD   HL,(CURSPR)
	INC  HL
	INC  HL
	LD   (HL),14
	INC  HL
	LD   (HL),15
	INC  HL
	LD   (HL),14
	INC  HL
	LD   (HL),13
	LD   A,4
	LD   (MANFLG),A
	LD   HL,ELECN2
	DEC  (HL)	       ; One step taken
	RET  NZ
	LD   HL,ELESTA
	INC  (HL)	       ; Door closing, man not drawn
	XOR  A
	LD   (ELECNT),A
	LD   HL,(XYZADR)
	LD   (WASXYZ),HL
	LD   B,DOOR_CLOSING
	JP   SOUND	       ; Sounds like a door closing
NOELEV: BIT  4,(IY+0)
	JP   NZ,FALLIN
	EX   AF,AF"
	JR   Z,GROL5V	       ; No collision
	RES  4,(IY+0)
	CALL SPCIAL
GROL5V: LD   HL,(XYZADR)
	LD   A,YPLUS
	SUB  (HL)
	INC  HL
	SUB  (HL)
	INC  HL
	SUB  (HL)
	JP   C,GROL5E
	CP   &30
	JP   C,GROL5E
	LD   A,4
	CALL CTRL	       ; Jump?
	LD   HL,JETFLG
	JP   NZ,GROL5E	       ; Jump if no jump...
	LD   A,(FUEL)
	BIT  7,(IY+0)	       ; On the ground?
	JR   Z,TAKEOF	       ; then take off
	BIT  2,(HL)
	JR   NZ,NODFUE
	DEC  A
	JR   Z,NOFUEL	       ; Else check for any fuel left
NODFUE: BIT  7,(IY+1)	       ; This bit is set if the object is falling
	JP   Z,GOGRSF
	LD   (IY+1),2	       ; if falling, make it stand still.
	SET  0,(HL)	       ; "fuel burn"
	JR   GOGRSF
NOFUEL: LD   (IY+1),A	       ; A is 0
	SET  0,(HL)	       ; still burning
	JR   GOGRAV
TAKEOF: CP   7
	JR   C,LESST7
	LD   A,7
LESST7: AND  7
	LD   (IY+1),A	       ; Init
	LD   B,A
	LD   A,(FUEL)
	SUB  B
	INC  A
	BIT  2,(HL)
	JR   NZ,NODFU2
NODFU2: SET  7,(IY+0)	       ; off. Signal "in the air"
	SET  0,(HL)	       ; Signal "jetpack flame on"
	JP   GOGRSF
GROL5E: BIT  1,(HL)
	CALL NZ,PPUTBK
	RES  0,(HL)
	BIT  3,(HL)
	JR   Z,GROL5Y
	RES  3,(HL)
	PUSH IX
	PUSH IY
	DI
	CALL TOFFMU
	EI
	POP  IY
	POP  IX
	JR   GROL5Y
GOGRSF: LD   HL,FLAGS3
	BIT  5,(HL)
	LD   HL,JETFLG
	JR   Z,GOGRAV	    ; Only affect FUEL if allowed.
	LD   (FUEL),A
GOGRAV: BIT  0,(HL)	    ; Check for flame on and go put it on if so
	JR   Z,GROL5Y
	SET  3,(HL)
	CALL PFLAME
	LD   A,(PLAY1)
	OR   A
	JR   NZ,GROL5Y
	LD   B,JET_BURNER
	CALL SOUND	    ; Do sound!
GROL5Y: LD   A,6
	CALL CTRL
	JP   Z,HLTGAM
	LD   A,(MANFLG)
	OR   &80	    ; Before (maybe) calling new bullet insertion
	LD   (TMPDIR),A     ; routine, make sure that the origin was the
	LD   HL,FLAGS4
	BIT  4,(HL)
	RES  4,(HL)
	JR   Z,NONEWR
	LD   A,(NODYNA)
	CP   6
	JR   NC,NONEWR
	LD   HL,BABIES
	LD   A,(HL)
	CP   2
	JR   NC,NONEWR
	INC  (HL)
	PUSH IX
	PUSH IY
	LD   A,1+(GRBOT3-DIMTAB)/4
	CALL INSOBJ
	POP  IY
	POP  IX
NONEWR: LD   A,5	    ; little cute man. Otherwise he may DIE in the
	CALL CTRL	    ; process!!
	JR   NZ,NOFIR
	LD   HL,FLAGS3
	BIT  2,(HL)	    ; Drop rod, or do firing?
	JR   Z,NOHELD	    ; Jump - nothing held - to bullet
	LD   A,(NODYNA)
	CP   7
	JR   Z,NOFIR	    ; But don't if too many rods is in the room
	LD   A,1+(GROD-DIMTAB)/4
	PUSH IX
	PUSH IY
	CALL INSOBJ	    ; Else insert the rod.
	POP  IY
	POP  IX
	JR   NOFIR
NOHELD: CALL BULLET
	LD   B,GUNFIRE_1
	CALL SOUND
NOFIR:	LD   HL,(CURSPR)
	LD   DE,TRNCNT
	BIT  7,(IY+0)
	JR   Z,GROL5G
	INC  HL
	LD   (HL),1	    ; no animation when flying
	DEC  HL
	JR   NOGAIN
GROL5G: LD   A,(FUEL0)
	DEC  A
	AND  3
	LD   (FUEL0),A
	JR   NZ,NOGAIN
	LD   A,(FUEL)
	INC  A
	CP   &40
	JR   NC,NOGAIN
	LD   (FUEL),A
NOGAIN: PUSH HL
	LD   A,(DE)
	DEC  A
	LD   (DE),A
	JP   P,GROL5A	       ; Turning not allowed yet.
	INC  A
	LD   (DE),A
	LD   A,(MANFLG)
	LD   E,A
	LD   A,1
	CALL CTRL
	JP   NZ,GROL5B	       ; No turning left
	INC  E
	JP   GROL5F
GROL5B: LD   A,2
	CALL CTRL
	JP   Z,GROL5D
	JP   GROL5A	       ; No turning right
GROL5D: DEC  E
GROL5F: LD   HL,FLAGS2
	LD   A,E
	AND  7
	LD   E,A
	LD   A,(INITRN)
	SUB  E
	JR   NC,DOANEG
	NEG
	CP   3	       ; ?????????
DOANEG: CP   6
	JR   C,DOANE2
	NEG
	ADD  A,&08
DOANE2: CP   2
	JP   C,MSTAKE
	BIT  2,(HL)
	JP   Z,GROL5A
MSTAKE: LD   A,E
	LD   (MANFLG),A
	LD   A,(NODYNA)
	DEC  A
	JR   NZ,GROL5Q	       ; Alone?
	LD   A,3	       ; If alone, make turning a bit slower.
GROL5Q: LD   (TRNCNT),A
GROL5A: LD   A,(MANFLG)
	LD   E,A
	LD   B,A
	CP   5
	JP   C,GROL5J
	LD   A,8
	SUB  E
	LD   E,A
GROL5J: ADD  A,A
	ADD  A,E	       ; *3 (each view has 3 animation stages)
	INC  A
	LD   E,A
	LD   A,B
	CP   5
	CCF
	LD   A,E
	PUSH DE
	PUSH BC
	PUSH AF
	CALL INVERT
	POP  AF
	INC  A
	PUSH AF
	CALL INVERT
	POP  AF
	INC  A
	PUSH AF
	CALL INVERT
	POP  AF
	POP  BC
	POP  DE
	POP  HL
	INC  E
	INC  HL
	INC  HL
	LD   (HL),E
	DEC  E
	INC  HL
	LD   (HL),E
	INC  E
	INC  HL
	LD   (HL),E
	INC  E
	INC  HL
	LD   (HL),E
	DEC  HL
	DEC  HL 		
	DEC  HL
	DEC  HL
WALKOK: LD   A,(JETFLG)
	RRA
	JR   NC,NCKWNK	       ; jump if not pressed jetpack
	LD   A,(PJOYS)
	OR   A		       ; if jetpack and joystick, jump
	JR   NZ,GROL5H	       ; to walk anyway - else check for key
NCKWNK: LD   A,3	       ; Forward?
	CALL CTRL
	JP   Z,GROL5H	       ; If no walking is in progress, make the image
WALKKO: LD   (HL),1	       ; be the first one in the list as if the man was
	LD   (IX+0),0
	LD   (IX+1),0
	JP   GROL5C	       ; standing with his two feet on the ground.
GROL5H: LD   A,(MANFLG)
	AND  7
	ADD  A,A
	LD   E,A
	LD   D,0
	LD   HL,VECTAB
	ADD  HL,DE
	LD   A,(HL)
	LD   (IX+0),A
	INC  HL
	LD   A,(HL)
	LD   (IX+1),A
GROL5C: LD   HL,FLAGS3
	BIT  2,(HL)	    ; Rod held?
	JR   Z,NOTHLD	    ; Jump if not
	LD   HL,UREM2
	DEC  (HL)	    ; Else test counterflag
	JP   P,NOTHLD
	LD   (HL),RODOWN
	LD   HL,UREM
	DEC  (HL)
NOTHLD: LD   HL,FLAGS2
	BIT  2,(HL)
	JR   Z,FGETIT
	BIT  3,(HL)
	RES  3,(HL)
	LD   A,(BULDIM)        ; Bullet direction for MAN
	CALL NZ,PANMAN
FGETIT: JP   GRVITY
;
PPUTBK: RES  1,(HL)
	LD   HL,FIRBUF
	LD   DE,(LOFFST)
	LD   C,E
	LD   B,11
PBACKG: LD   A,(HL)
	LD   (DE),A
	INC  HL
	INC  E
	LD   A,(HL)
	LD   (DE),A
	INC  HL
	INC  E
	INC  E
	INC  E
	DJNZ PBACKG
	LD   E,C
	SET  7,E
	LD   B,11
PBACKM: LD   A,(HL)
	LD   (DE),A
	INC  HL
	INC  E
	LD   A,(HL)
	LD   (DE),A
	INC  HL
	INC  E
	INC  E
	INC  E
	DJNZ PBACKM
	RET
;	 
PFLAME: BIT  1,(HL)	       ; First time?
	CALL NZ,PPUTBK	       ; Put pack if more than first time
	LD   A,(MANFLG)
	LD   L,A
	LD   H,0	       ; Direction
	LD   DE,POFFST
	ADD  HL,DE	       ; Find offset from where to start the flame
	LD   A,(HL)
	OR   A		       ; Zero means the man is facing out of the screen
	RET  Z		       ; - thus no flame is required
	LD   HL,JETFLG
	SET  1,(HL)
	LD   E,A	       ; E is offset
	LD   A,(PHYSIC)
	ADD  A,IMAGES/&100-1   ; Find hi-byte of physical image to be printed
	LD   D,A	       ; This is the picture (exellent birds)
	LD   (LOFFST),DE       ; Save this
	LD   A,(CURFIR)        ; Current Flame Frame
	INC  A
	CP   3
	JR   C,STILL
	XOR  A
STILL:	LD   (CURFIR),A        ; Save it again
	LD   B,A	       ; Also here (saves hastle :-))
	PUSH IX
	PUSH IY
	LD   A,(MANFLG)
	LD   IX,FLAME1
	OR   A
	JR   Z,STILL1
	LD   IX,FLAME3
	DEC  A
	JR   Z,STILL1
	LD   IX,FLAME4
	CP   6
	JR   Z,STILL1
	LD   IX,FLAME2
STILL1: LD   A,B
	OR   A
	JR   Z,GOTIT	       ; First
	LD   BC,44
	ADD  IX,BC
	DEC  A
	JR   Z,GOTIT	       ; Second
	ADD  IX,BC
GOTIT:	LD   IY,FIRBUF
	LD   B,11	       ; 11 bytes
SKYLAB: LD   A,(DE)
	LD   (IY+0),A
	AND  (IX+22)
	OR   (IX+0)
	LD   (DE),A
	SET  7,E
	LD   A,(DE)
	LD   (IY+22),A
	AND  (IX+22)
	LD   (DE),A
	INC  E
	INC  IY
	INC  IX
	LD   A,(DE)
	LD   (IY+22),A
	AND  (IX+22)
	LD   (DE),A
	RES  7,E
	LD   A,(DE)
	LD   (IY+0),A
	AND  (IX+22)
	OR   (IX+0)
	LD   (DE),A
	INC  E
	INC  E
	INC  E
	INC  IY
	INC  IX
	DJNZ SKYLAB
	POP  IY
	POP  IX
	RET
;
; Call CTRL with A=1 testing for right
;		 A=2 for left
;		 A=3 for up (forward)
;		 A=4 for down (jetpack)
;		 A=5 for fire
;		 A=6 for PAUSE (not a joystick control)
;
; Returns NZ if key not pressed, Z if key pressed
;
; Corrupts BC', DE', HL', A and all flags
;
; Special case:
;
; When a joystick is selected and the call is "FORWARD?" the routine returns
; TRUE (z) if EITHER forward OR jetpack is pressed. So if player uses a joy-
; stick and pulls it back, the FORWARD routines call will detect that as well
; as the JETPACK call!
;
CTRL:	EXX		       ; Returns zero if key/joything was pressed.
	LD   B,A
	CP   3		       ; Is it a FORWARD call?
	JR   NZ,NOTFWD	       ; No, jump
	LD   A,(PSKEY)	       ; Yes, get keyboard_true
	OR   A		       ; Joystick selected (any type)?
	JR   NZ,NOTFWD	       ; No, jump
	CALL NOTFWD	       ; Yes, check for forward being pressed anyway.
	RET  Z		       ; Return zero = pressed if it was
	LD   A,(JETFLG)
	AND  1
	XOR  1
	RET
;	 EXX			; Exchange back again (3rd time)
;	 LD   B,4		; Now check for jetpack!
NOTFWD: LD   A,B	       ; Get check !
	ADD  A,A
	ADD  A,B
	LD   L,A
	LD   H,0
	LD   DE,CTRLTL-3       ; Start here
	ADD  HL,DE
	LD   C,(HL)
	INC  HL
	LD   B,(HL)
	INC  HL
	LD   D,(HL)
	IN   A,(C)
	BIT  7,C
	JR   NZ,WASKEY
	CPL		       ; Invert result if kempston!
WASKEY: AND  D
	EXX
	RET
;
BULLET: LD   A,(NOBULL)        ; Bullet initialize
	INC  A
	CP   MAXBUL+1	       ; Is too many bullets already on screen?
	RET  Z		       ; Ret if so.
	XOR  A
	LD   (TRNCNT),A
	LD   A,(TMPDIR)        ; Direction
	AND  7
	ADD  A,A
	LD   E,A
	LD   D,0
	LD   HL,VECTAB	       ; Index to get those direction vectors
	ADD  HL,DE
	LD   A,(HL)	       ; get X vector
	ADD  A,A
	ADD  A,A
	ADD  A,8	       ; Allign
	LD   C,A
	INC  HL
	LD   A,(HL)	       ; Y vector
	ADD  A,A
	ADD  A,A
	LD   B,A	       ; Allign
	LD   A,9
ROBULL: DI
	LD   (BPATCH+1),A
	LD   HL,NOBULL
	INC  (HL)
	LD   A,(HL)
	LD   H,0
	ADD  A,A
	ADD  A,A
	ADD  A,A
	LD   L,A
	LD   DE,BULTAB-8       ; Index
	ADD  HL,DE
	PUSH IY
	PUSH IX
	PUSH HL
	LD   DE,(XYZADR)       ; Get coords of main sprite
	LD   A,(DE)
	ADD  A,C	       ; allign bullet X so it appears to come out
	LD   (HL),A	       ; of the mouth of the gun. The UZI gun, that is.
	INC  HL
	INC  DE
	LD   A,(DE)
	ADD  A,B	       ; allign bullet Y
	LD   (HL),A
	INC  HL
	INC  DE
	LD   A,R
	AND  3		       ; A is 0,1,2,3
	SUB  2		       ; A is -2,-1,0,1
	ADC  A,0	       ; A is -1,0,1   This is the "distortion" factor!
	EX   DE,HL
	ADD  A,(HL)	       ; plus Z coord
	EX   DE,HL
BPATCH: ADD  A,9	       ; plus allignment (this is self-modified)
	LD   (HL),A
	INC  HL
	LD   A,(TMPDIR)        ; Direction again
	LD   (HL),A
	POP  IX 	       ; Get pointer to the actual bullet info
	CALL BCALC1	       ; Get It On !
	POP  IX
	POP  IY
	EI		       ; You can start again now!
	RET
;
GROL6:	JP   Z,GRVITY	       ; Repeat movement until collision.
;
GROL7:	EX   AF,AF"
	BIT  4,(IY+0)
	JP   NZ,FALLIN
	RES  4,(IY+0)
	EX   AF,AF"
	CALL NZ,SPCIAL
	XOR  A
	LD   (IX+0),A
	LD   (IX+1),A
	LD   (IX+2),A
	LD   HL,(CURSPR)
	LD   A,(HL)
	AND  7
	ADD  A,5
	LD   E,A
	LD   D,0
	ADD  HL,DE
	LD   E,(HL)
	INC  HL
	LD   D,(HL)
	INC  HL
	INC  HL
	INC  HL
	LD   A,(HL)
	INC  HL
	LD   H,(HL)
	LD   L,A
	INC  DE
	LD   A,(CURROM)
	AND  &1F
	LD   B,A
	LD   A,(CFLOOR)
	DEC  A
	RRCA
	RRCA
	RRCA
	AND  &E0
	ADD  A,B
	LD   (HL),A
	INC  HL
	INC  HL
	INC  HL
	EX   DE,HL
	LDI
	LDI
	LDI
	JP   GRVITY
;
; Special "Restore Registers" and "Call Action Server" which returns without
; calling any action servers if the object with which the current object has
; collided is any of the following:-
;
;  a) A "Common" hole
;  b) A "Big" hole
;  c) A "Decontamination shower" (EXTOBJ, not the graphic image!)
;
; This routine (SPCIAL) should be called from a GROL if a collision is detected
; (Z=0 on entry) only if the above exceptions apply.
;
; Note: any hole collision and logic for falling through and detecting that
; is dealt with through gravitational affections on the Z vector. This is only
; to avoid an actually 'collision' with a hole, which of course would be silly.
; It is nescessary, however, to know that the hole is there, so to make robots
; avoid them, for example.
;
SPCIAL: LD   A,(CURPHY)
	CP   1+(GBGHLR-DIMTAB)/4   ; Exceptions for collisions:-
	RET  Z
	CP   1+(GBGHLL-DIMTAB)/4   ; The Holes; two sides of the Big Hole
	RET  Z
	CP   1+(GHOLE1-DIMTAB)/4   ; Small "common" hole
	RET  Z
	LD   A,(CURLOG)
	LD   L,A
	LD   H,0
	LD   DE,ACTTAB-1
	ADD  HL,DE
	LD   A,(HL)
	CP   &C 	       ; Check for "big hole" ACTION server!
	RET  Z
	CP   &E 	       ; Check for "decontamination shower" server
	RET  Z
	CALL RESREG
	JP   ACTION
;
HLTGAM: CALL GETOFF
HLT1:	XOR  A
	IN   A,(&FE)
	CPL
	AND  &F
	RET  NZ
	JR   HLT1
;
GROL8:	RET
;
GROL9:	RET
;
GROLA:	RET

(*	CALL NZ,BOUNCE
	BIT  7,(IY+0)
	JP   NZ,GRVITY
	SET  7,(IY+0)
	LD   (IY+1),6
	CALL TRNRND
	LD   HL,(XYZADR)
	INC  HL
	INC  HL
	LD   A,(HL)
	OR   A
	JP   NZ,GRVITY
	LD   (HL),84
	DEC  HL
	LD   (HL),8
	DEC  HL
	LD   (HL),8
*)
GROLB:	JP   GRVITY
;
; Use PANMAN to affect an object which has been hit by a bullet to 'pan' or
; 'slide' in that direction.
;
; Call with A=direction of bullet (cannot be 1 const, 'cause robot and man
; may both be hit simultaneous)
;
PANMAN: ADD  A,A	      ; Index by two bytes
	LD   L,A
	LD   H,0	      ; form 16-bit address
	LD   DE,VECTAB	      ; ok, this is the base
	ADD  HL,DE	      ; index
	LD   A,(HL)
	ADD  A,(IX+0)
	LD   (IX+0),A
	INC  HL
	LD   A,(HL)
	ADD  A,(IX+1)
	LD   (IX+1),A
	RET
;
;IF DEBUG=1   ; REGISTER PRINT PATCH FOR DEBUGGING
 PATCH:
 DI
 LD (XHL),HL
 LD (XDE),DE
 LD (XBC),BC
 PUSH AF
 POP HL
 LD (XAF),HL
 LD (XIX),IX
 LD (XIY),IY
 LD A,71
 CALL CLRALL
 LD DE,PAF
 LD HL,(XAF)
 CALL DHEX
 LD DE,PBC
 LD HL,(XBC)
 CALL DHEX
 LD DE,PDE
 LD HL,(XDE)
 CALL DHEX
 LD DE,PHL
 LD HL,(XHL)
 CALL DHEX
 LD DE,PPIX
 LD HL,(XIX)
 CALL DHEX
 LD DE,PPIY
 LD HL,(XIY)
 CALL DHEX
 LD HL,PST
 CALL PRINT
 WAITXXX: LD A,&FE
 IN A,(&FE)
 RRA
 JP C,WAITXXX
 LD HL,(XAF)
 PUSH HL
 POP AF
 LD DE,(XDE)
 LD BC,(XBC)
 LD IX,(XIX)
 LD IY,(XIY)
 LD HL,(XHL)
 EI
 RET
 DHEX: LD A,H
 CALL HEX
 LD A,L
 HEX:
 PUSH AF
 RRCA
 RRCA
 RRCA
 RRCA
 CALL HEX1
 POP AF
 HEX1:
 AND 15
 CP 10
 JR C,HEX2
 ADD A,7
 HEX2:
 ADD A,48
 LD (DE),A
 INC DE
 RET
 PST:
 DW &4000
 DB 0,1,'AF:'
 PAF: 
 DB '    ',&FF
 DW &4020
 DB 0,1,'BC:'
 PBC: 
 DB '    ',&FF
 DW &4040
 DB 0,1,'DE:'
 PDE: 
 DB '    ',&FF
 DW &4060
 DB 0,1,'HL:'
 PHL: 
 DB '    ',&FF
 DW &4080
 DB 0,1,'IX:'
 PPIX: 
 DB '    ',&FF
 DW &40A0
 DB 0,1,'IY:'
 PPIY: 
 DB '    ',0
 XAF: DW 0
 XBC: DW 0
 XDE: DW 0
 XHL: DW 0
 XIX: DW 0
 XIY: DW 0
;ENDIF
;
TITLE	'IMAGE6: Interrupt handlers (bullets)'
EJECT
;
; 0. X
; 1. Y
; 2. Z
; 3. Direction (as MANFLG) + Bit 7 is ORIGIN (1=ROBOT, 0=MAN)
; 4. Background (excl. bullet bit)
; 5. Mask (or abs. bullet)
; 6. DF LSB
; 7. DF MSB
;
COAL	EQU  6		       ; This is a 3rd level patch
;
BSETUP: XOR  A
	LD   (NOBULL),A
	LD   HL,HANDLR
	LD   (VECTOR),HL
	RET
;
HANDLR: EI
	PUSH AF 	       ; Interrupt entry point - Save all registers
	PUSH BC
	PUSH DE
	PUSH HL
	EXX
	PUSH BC
	PUSH DE
	PUSH HL
	PUSH IX
	PUSH IY
	CALL MUSIC
	LD   HL,AUXFLG
	BIT  1,(HL)
	JR   NZ,SKPOVR
	SET  1,(HL)
	CALL BUNPL	       ; Unplot
	CALL BPLOT	       ; Plot
	CALL BMOVE	       ; Move
	CALL BCOLL	       ; Collision
	CALL BCALC	       ; New addresses
	LD   HL,AUXFLG
	RES  1,(HL)
SKPOVR: LD   HL,TIMER1
	DEC  (HL)
	JP   P,NOT1SY
	LD   (HL),50
	LD   HL,(TIMER)
	DEC  HL
	LD   (TIMER),HL
	LD   A,L
	OR   H
	JP   Z,GAMOVR
	LD   DE,&50F1
	CALL CSCORE
NOT1SY: POP  IY
	POP  IX
	POP  HL
	POP  DE
	POP  BC
	EXX
	POP  HL
	POP  DE
	POP  BC
	POP  AF
	EI
	RETI		       ; Nest if nescessary
;
BCOLL:	LD   A,&FF
	LD   (COUNTX),A
	LD   A,(NOBULL)        ; Get number of bullets
	LD   B,A
	LD   IX,BULTAB-8       ; Start off from bottom 
BCOLL2: LD   DE,8	       ; "while bullet>0..."
	ADD  IX,DE
	DEC  B		       ; No more bullets?
	RET  M		       ; No more bullets.
	LD   HL,COUNTX
	INC  (HL)	       ; Current bullet to be reprocessed.
	LD   E,(IX+0)	       ; Grab the X and Y
	LD   D,(IX+1)
	LD   HL,XLOW
	LD   A,E	       ; Check the Axis Walls (4-byte table)
	CP   (HL)
	JP   C,BREMOV	       ; Check X low axis
	INC  HL
	LD   A,D
	CP   (HL)
	JP   C,BREMOV	       ; Y low axis
	INC  HL
	LD   A,E
	SUB  8
	CP   (HL)
	JP   NC,BREMOV	       ; X high axis
	INC  HL
	LD   A,D
	CP   (HL)	       ; Y high axis
	JP   NC,BREMOV
	LD   HL,DLIMIT	       ; Diagonal walls boundaries' 4-byte table
	LD   A,E
	ADD  A,D	       ; Find upper/lower absolute - X+Y
	CP   (HL)	       ; Test against Lower limit - (bottom of screen)
	JP   C,BREMOV	       ; But the lower limit was above - remove bullet
	INC  HL 	       ; Advance to Upper limit
	SUB  8		       ; Adjust to suit limit
	CP   (HL)	       ; Upper limit
	JP   NC,BREMOV	       ; If it was not above - remove bullet
	INC  HL 	       ; Advance to Right limit
	LD   A,E	       ; Find right absolute - X-Y
	SUB  D		       ; but if result is negative, the bullet is
	JR   C,BNOK1	       ; on the wrong side of the scren - thus skip.
	LD   C,A
	LD   A,(HL)	       ; Make right wall extended 16 pixels, to
	ADD  A,16	       ; make up for the sprite's odd sizes.
	CP   C
	JP   C,BREMOV
BNOK1:	INC  HL 	       ; Advance to Left limit
	LD   A,D	       ; This is Y-X
	SUB  E
	JP   C,BNOK2	       ; But carry indicates wrong side of screen
	CP   (HL)
	JP   NC,BREMOV
BNOK2:	LD   HL,CURENT
BCOLL1: LD   A,(HL)	       ; Get an object from the room
	OR   A
	JP   Z,BCOLL2	       ; End of table - go round again.
	INC  HL
	CP   15 	       ; Check for hit object only being nasties!
	JP   C,BDOIT1 
	CP   1+(GRBOT1-DIMTAB)/4
	JR   Z,BDOIT2	       ; Insert all robot compares here!
	CP   2+(GRBOT1-DIMTAB)/4
	JR   Z,BDOIT2
	CP   3+(GRBOT1-DIMTAB)/4
	JR   Z,BDOIT2
	CP   1+(GRBOT2-DIMTAB)/4
	JR   Z,BDOIT2
	CP   1+(GRBOT3-DIMTAB)/4
	JR   Z,BDOIT2
	CP   1+(GCUBE-DIMTAB)/4
	JR   Z,BDOIT
	CP   1+(GSHOWL-DIMTAB)/4
	JR   Z,BDOIT
	JP   NOPC1
BDOIT1: BIT  7,(IX+3)	       ; OK, the object is the main character.
	JP   NZ,NOPC1	       ; so check for bullet being his own and skip
	JR   BDOIT	       ; if so!
BDOIT2: BIT  7,(IX+3)	       ; Do the same if a robot.
	JP   Z,NOPC1
BDOIT:	EXX
	LD   L,A	       ; Find the dimensions ...
	LD   H,0
	ADD  HL,HL
	ADD  HL,HL	       ; 4 per entry
	LD   DE,DIMTAB-4       ; Offset
	ADD  HL,DE	       ; Index
	EXX		       ; The following conditions must all be true for
	LD   A,(IX+0)	       ; a collision to be acceptable:-  Cond. Xp-Xo=>0
	SUB  COAL
	SUB  (HL)	       ; Xp-Xo
	JP   C,NOPC1	       ; Xp was smaller, ie. not collided
	LD   A,(HL)	       ; Cond. (Xo+Xd)-Xp=>0
	EXX
	ADD  A,(HL)	       ; Other edge of object's X (add dimension)
	EXX
	ADD  A,COAL
	SUB  (IX+0)	       ; Check against point X
	JP   C,NOPC1	       ; Xp was bigger, ie. not collided
	INC  HL 	       ; Advance to Yo 
	LD   A,(IX+1)	       ; Cond. Yp-Yo=>0
	SUB  (HL)	       ; Yp-Yo
	JP   C,NOPC2	       ; Yp was smaller, ie. not collided
	LD   A,(HL)	       ; Cond. (Yo+Yd)-Yp=>0
	EXX
	INC  HL 	       ; Forward to Yd
	ADD  A,(HL)	       ; Other edge of object's Y (add dimension)
	EXX
	SUB  (IX+1)	       ; Check against point Y again,
	JP   C,NOPC2	       ; and jump if Yp was bigger, ie. not collided
	INC  HL 	       ; Forward to Zo
	LD   A,(IX+2)	       ; Cond. Zp-Zo=>0
	SUB  (HL)	       ; Zp-Zo
	JP   C,NOPC3	       ; Zp was bigger, so no collision
	LD   A,(HL)	       ; Cond. (Zo+Zd)-Zp=>0
	EXX
	INC  HL 	       ; Advance to Zo
	ADD  A,(HL)	       ; Add Zd to find top edge of object
	EXX
	SUB  (IX+2)	       ; Check against point Z again
	JP   C,NOPC3	       ; But jump if above.
	DEC  HL
	DEC  HL
	DEC  HL
	LD   A,(HL)	       ; Get physical image which was hit
	LD   HL,FLAGS2	       ; Prepare to set flags according
	CP   1+(GCUBE-DIMTAB)/4
	JR   Z,BREMOV
	CP   1+(GSHOWL-DIMTAB)/4
	JR   Z,BREMOV
	CP   15 	       ; Main chr?
	LD   A,(IX+3)
	JR   C,MANHIT	       ; Yes, jump to Man Hit
	AND  7
	LD   (BULDIR),A        ; Store for ROBOT
	BIT  6,(HL)	       ; Else (assuming it must be a robot) test
	JR   NZ,BREMOV	       ; for a robot already in explosion.
	SET  4,(HL)
	JR   BREMOV
MANHIT: AND  7
	LD   (BULDIM),A        ; Store for MAN
	BIT  5,(HL)
	JR   NZ,BREMOV
	SET  3,(HL)
	LD   HL,UREM
	DEC  (HL)
	JR   BREMOV
NOPC1:	INC  HL 	       ; Advance to next entry in CURENT
NOPC2:	INC  HL
NOPC3:	INC  HL
	JP   BCOLL1	       ; and go again.
;
BREMOV: LD   A,(NOBULL)        ; A bullet must be removed
	DEC  A
	LD   (NOBULL),A        ; So tell No Of Bullets.
	RET  Z		       ; But go back if this was the only bullet.
	LD   HL,COUNTX
	SUB  (HL)	       ; Calculate nos. left "above" current bullet.
	DEC  (HL)	       ; This is altered back in BCOLL2 so the effect
	OR   A		       ; is that the bullet number is unaffected.
	JP   Z,BCOLL2	       ; Exit now if it was the last physical bullet.
	PUSH IX 	       ; Get BULTAB pointer into HL
	POP  HL
	LD   E,L
	LD   D,H	       ; DE points to the bullet data to be removed.
	PUSH DE
	LD   DE,8
	ADD  HL,DE	       ; HL points to next entry in bullets table
	ADD  A,A	       ; 8 bytes per entry in BULTAB thus multiply
	ADD  A,A
	ADD  A,A
	LD   C,A	       ; C is no. of bytes to move
	LD   A,B	       ; Put B away. (D=0)
	LD   B,D	       ; BC is the number of "bullets" to move * 8
	POP  DE
	LDIR		       ; Move remaining bullets down over the lost one
	LD   B,A	       ; get B back.
	JP   BCOLL2	       ; Go get another bullet.
;
BMOVE:	LD   A,(NOBULL)
	OR   A
	RET  Z		       ; No bullets flying around - drop it
	LD   IX,BULTAB
	LD   B,A		
BMOVE1: LD   A,(IX+3)	       ; Direction
	AND  7		       ; Only direction
	ADD  A,A	       ; *2 for X and Y
	LD   L,A
	LD   H,0
	LD   DE,VECTAB	       ; Table of direction vectors
	ADD  HL,DE
	LD   A,(HL)
	ADD  A,A	       ; Speed it up
	ADD  A,(IX+0)
	LD   (IX+0),A
	INC  HL
	LD   A,(HL)
	ADD  A,A	       ; Speed Y up too
	ADD  A,(IX+1)
	LD   (IX+1),A
	LD   DE,8	       ; Next entry
	ADD  IX,DE
	DJNZ BMOVE1
	RET
;
BCALC:	LD   A,(NOBULL)        ; No. of bullets
	OR   A
	RET  Z		       ; none.
	LD   IX,BULTAB
	LD   C,A
BCALCL: CALL BCALC1	       ; Spray one on screen
	DEC  C
	JP   NZ,BCALCL	       ; Spray them all on screen
	RET
;
BCALC1: LD   A,YPLUS-&20       ; This is a compromise...
	SUB  (IX+0)
	SUB  (IX+1)
	SUB  (IX+2)	       ; Y
	LD   L,A
	LD   H,0
	LD   A,(IX+0)
	SUB  (IX+1)
	ADD  A,A
	ADD  A,XPLUS
	LD   B,A	       ; X
	RRCA
	RRCA
	RRCA
	AND  &1F
	ADD  HL,HL
	LD   DE,SCRTAB
	ADD  HL,DE
	ADD  A,(HL)	       ; Offset
	INC  HL
	LD   H,(HL)
	LD   L,A
	LD   A,B
	AND  7		       ; Pixel
	LD   A,&80
	JP   Z,BCALC3
BCALC2: RRCA		       ; Do absolute pixel/mask
	DJNZ BCALC2
BCALC3: LD   (IX+5),A
	LD   (IX+6),L	       ; Save pixel mask and DF address
	LD   (IX+7),H
	LD   B,A
	PUSH BC
	PUSH HL
	PUSH IX
	LD   B,(IX+0)	       ; Get the coordinates again
	LD   C,(IX+1)
	LD   D,(IX+2)
	CALL BHIDD	       ; Test for hidden pixel, returns CARRY if 
	POP  IX 	       ; the pixel is hidden.
	POP  HL
	POP  BC
	JP   NC,LEAVE	       ; If not hidden, leave the plotting to BPLOT
	LD   (IX+5),0	       ; Else make bullet mask zero (ie. no bullets)
LEAVE:	LD   DE,8	       ; Next entry
	ADD  IX,DE
	RET
;
; BULLET PLOT - add bullets on screen as stated in BULTAB
;
BPLOT:	LD   IX,UNPTAB	       ; Insert values to be deleted later here
	LD   HL,BULTAB+5       ; First byte of useful information in this table
	LD   A,(NOBULL)        ; No. of bullets currently on screen
	OR   A
	RET  Z
	LD   B,A
	DB   PIY
	LD   L,0
BPLOT1: LD   A,(HL)	       ; Bullet mask
	INC  HL
	LD   E,(HL)	       ; Displayfile address LSB
	INC  HL
	LD   D,(HL)	       ; Displayfile address MSB
	OR   A
	JR   Z,BPLOT2	       ; Jump if the bullet is hidden (mask=0)
	LD   C,A
	LD   A,(DE)	       ; Screen contents
	LD   (IX+0),E
	LD   (IX+1),D	       ; Save DF addr, bullet mask and background.
	LD   (IX+2),C
	LD   (IX+3),A
	XOR  C		       ; Insert bullet
	LD   (DE),A	       ; Onto screen
	LD   DE,4
	ADD  IX,DE
	DB   PIY
	INC  L
BPLOT2: LD   DE,6
	ADD  HL,DE	       ; Next entry
	DJNZ BPLOT1
	LD   (TAB2IN),IX
	DB   PIY
	LD   A,L
	LD   (B2REMO),A        ; Tell Bullets_To_Remove
	RET
;
; BULLET UNPLOT - Remove bullets as stated in TABLE2 backwards.
;
BUNPL:	LD   HL,(TAB2IN)       ; Get indirection pointer to TABLE2 - the
	LD   A,(B2REMO)        ; Bullets to remove...
	OR   A
	RET  Z		       ; But return if none
	LD   B,A
BUNPL1: DEC  HL
	LD   A,(HL)	       ; Old screen contents
	DEC  HL
	LD   C,(HL)	       ; Pixel mask
	DEC  HL
	LD   D,(HL)	       ; DF_MSB (first 'cause it's reverse order)
	DEC  HL
	LD   E,(HL)	       ; DF_LSB
	XOR  C		       ; Temp. insert bullet
	EX   DE,HL
	CP   (HL)	       ; Check whether screen contents have changed
	EX   DE,HL
	JP   NZ,CHANGD	       ; They had.
	XOR  C		       ; Remove the bullet
	LD   (DE),A	       ; They hadn't changed - erase bullet from screen
CHANGD: DJNZ BUNPL1	       ; Do them all 
	RET
;
; Detect if a single pixel on screen is hidden behind something.
; Returns carry set if pixel is hidden.
; Enter with BCD=xyz of pixel position. 
;
BHIDD:	NOP
	LD   A,YPLUS+&20      ; Y(2D)=OFFSET(Y)-X(3D)-Y(3D)-Z(3D)
	SUB  B
	SUB  C
	SUB  D
	DB   PIX	      ; Goes into XH
	LD   H,A	      ; Calculate 2D x coordinate of pixel
	LD   A,B
	SUB  C		      ; X(2D)=2*(Y(3D)-X(3D))+OFFSET(X)
	ADD  A,A
	ADD  A,XPLUS
	DB   PIX	      ; IX=2D coords
	LD   L,A	      ; Goes into XH - so XH=Y, XL=X
	LD   HL,CURENT
DMAIN1: LD   A,(HL)	      ; Grab an image.		
	OR   A		      ; Reset carry, Check for zero
	RET  Z		      ; No more - return carry CLEAR = no overlap
	CP   &40
	JR   C,DOK1
	CP   &60
	JP   NC,DEXIT5
	LD   A,1+(GDOOR2-DIMTAB)/4 ; If suspected doorpole, assume this object
DOK1:	INC  HL
	EXX
	LD   C,A	      ; C'=image (for later use)
	LD   DE,DIMTAB-2      ; Find dimension entry. Points to Z coord.
	LD   L,A
	LD   H,0	      ; Index,
	ADD  HL,HL
	ADD  HL,HL	      ; Four per entry
	ADD  HL,DE	      ; HL' now points to Z dimension of object
	EXX
	LD   A,B	      ; Grab Xp
	SUB  (HL)	      ; Test against other Xo
	JP   C,DEXIT3	      ; If in front, don't bother, go for next
	INC  HL 	      ; Forward to Yo
	LD   A,C	      ; Grab Yp
	SUB  (HL)	      ; Test against Yo
	JP   C,DEXIT2	      ; Jump if in front
	INC  HL 	      ; Else forward to Zo
	LD   A,(HL)	      ; Grab Zo
	EXX
	ADD  A,(HL)	      ; Add Zo Dimension to find top edge of object
	EXX
	SUB  D		      ; Test against Zp
	JP   C,DEXIT1	      ; Jump if Zp was above
	DEC  HL 	      ; Now the pixel was "behind" in 3D -
	DEC  HL 	      ; Go back to point to Xo
	PUSH BC 	      ; save XYZ
	PUSH DE
	LD   B,(HL)	      ; Other object's X
	INC  HL
	LD   C,(HL)	      ; and Y
	INC  HL 	      ; Point to Z
	LD   A,YPLUS	      ; 2D Y for test obj
	SUB  B
	SUB  C
	SUB  (HL)	      ; (HL) is Z
	LD   E,A	      ; E=Y
	DEC  HL
	DEC  HL
	LD   A,B
	SUB  C
	ADD  A,A
	ADD  A,XPLUS	      ; 2D X for test obj
	LD   C,A	      ; C=X
	LD   B,E	      ; BC is now 2D YX (test)
	DB   PIX	      ; This is Xp
	LD   A,L
	SUB  C		      ; Test left edge
	JP   C,DEXIT4	      ; No overlap - Xp was to the left of Xo(2D)
	LD   E,A	      ; Otherwise is this the difference (in pixels).
	LD   A,C
	ADD  A,&1F	      ; This is the right edge - Xo(2D)+WIDTH
	DB   PIX
	SUB  L		      ; Test against Xp
	JP   C,DEXIT4	      ; Forget it
	DB   PIX	      ; This is Yp
	LD   A,H
	SUB  B		      ; Test against Yo(2D)
	JP   C,DEXIT4	      ; Test top edge
	LD   D,A	      ; This is the difference
	LD   A,B
	ADD  A,&1F
	DB   PIX
	SUB  H
	JP   C,DEXIT4	      ; Check bottom edge
	PUSH HL 	      ; Save CURENT pointer
	SLA  D
	SLA  D		      ; 4 bytes per pixel row - D=Byte/4 within image
	LD   A,E
	RRCA
	RRCA
	RRCA
	AND  3		      ; Byte position on row
	ADD  A,D
	OR   &80
	LD   L,A	      ; D=absolute byte 
	LD   A,E
	AND  7		      ; Bit pos.
	INC  A
	LD   B,A
	EXX
	LD   A,C	      ; C was image
	EXX
	ADD  A,IMAGES/&100-1  ; Hi byte
	LD   H,A	      ; goes here
	LD   A,(HL)
FINDIT: RLA
	DJNZ FINDIT
	JP   NC,BEHIND	      ; Wow! We've got an OverLap
	POP  HL
DEXIT4: POP  DE
	POP  BC 	      ; Get back registers - XYZ and CURENT address
DEXIT3: INC  HL
DEXIT2: INC  HL
DEXIT1: INC  HL 	      ; Next entry
	JP   DMAIN1
DEXIT5: INC  HL
	JP   DEXIT3
BEHIND: POP  HL
	POP  DE
	POP  BC
	SCF		      ; Was hidden - signal so and return
	RET
;
TITLE	'IMAGE6a: Interrupt handlers (multicolour)'
EJECT
;
; *****************************************************************************
;
; MULTICOLOUR ATTRIBUTE interrupt server.
; 
; Note that this interrupt takes up appriximately 18600 microseconds, thus 
; leaving only about a thousand microseconds (4000 cycles) before it gets
; invoked again! There is no way around this!
;
; *****************************************************************************
;
MCOLOR: DI		      ; Square frame grab
	PUSH AF
	PUSH BC
	PUSH DE
	PUSH HL
	PUSH IX
	LD   IX,&FFFF

IF ONE28 ; --------------------------------------------------------------------
	LD   B,13
ELSE 
	LD   B,9
ENDIF ; -----------------------------------------------------------------------
MWAIT:	DJNZ MWAIT
	LD   B,61
	CALL WROWS
	LD   (HOLDSP),SP
	LD   BC,(FPOINT)
	LD   HL,&5818
HAND2:	LD   E,8       ; The numbers given here are machine cycles.
HAND1:	LD   SP,HL     ; 6     set screen pointer
	LD   A,(BC)    ; 7     grab a colour
	LD   D,A       ; 4     into d
	LD   A,E       ; 4     save counter in a
	LD   E,D       ; 4     also e
	PUSH DE        ;11     put 16 bytes onto screen...
	PUSH DE        ;11     - the real problem here is that when
	PUSH DE        ;11     The Raster comes along, it doesn't actually
	PUSH DE        ;11     overtake the fill routine - they just pass
	PUSH DE        ;11     eachother, and thus there is very little
	PUSH DE        ;11     time to get things done.
	PUSH DE        ;11
	PUSH DE        ;11
	INC  BC        ; 6     next colour
	LD   E,A       ; 4
	DEC  A	       ; 4     was this last row of a character line?
	JP   NZ,VIDERE ;10     no
	LD   DE,32     ;10     yes, go to next line
	ADD  HL,DE     ;11
	LD   E,A       ;4      tidy counter
	INC  E	       ;4
	LD   A,H       ;4      if this is outside screen,
UNTIL:	CP   &5B       ;7
	JP   NC,SLUT   ;10     go back
MERE:	NOP	       ; 4 *
	NOP	       ; 4 *
IF ONE28    ; ----------------------------------------------------------------
	NOP	       ; 4 *
	NOP	       ; 4 *
	NOP	       ; 4 *
ENDIF ; ----------------------------------------------------------------------
	LD   A,0       ; 7 *
	NOP	       ; 4 *   Wait 8.8571 usecs
	NOP	       ; 4 *
	DEC  E	       ; 4 *
	JP   NZ,HAND1  ;10     next pixel row within character row
	JP   HAND2     ;10     these 10 cycles are not regarded significant!
VIDERE: NOP	       ; 4 *   Wait 6.8571 usecs
	NOP	       ; 4 *
	NOP	       ; 4 *
	NOP	       ; 4 *
	NOP	       ; 4 *
	NOP	       ; 4 *
	JP   MERE      ;10     next pixel row
SLUT:	LD   SP,(HOLDSP)      ; get stack back
	LD   HL,MDELAY
	DEC  (HL)
	JP   P,TILBAE
	LD   A,(MDELOR)
	LD   (HL),A
	LD   HL,FCOUNT	      ; get counter
	DEC  (HL)
	JP   NZ,ONTHER	      ; still scroll
	LD   A,(FINITP)       ; if finished, initialise again
	LD   (HL),A
	LD   HL,(FINITW)
	LD   (FPOINT),HL
	JP   TILBAE
ONTHER: LD   HL,(FPOINT)
	LD   DE,(FOFFST)
	ADD  HL,DE
	LD   (FPOINT),HL
TILBAE: LD   HL,AUXFLG
	BIT  0,(HL)
	CALL NZ,MUSIC
	POP  IX
	POP  HL
	POP  DE
	POP  BC
	POP  AF
	EI
	RET		      ; Return to sender - but not for very long...
;
; MULTICOLOUR SETUP:
; Call with HL=address of colours, DE=offset to change, A=no. of frames
; B=No. of screen "thirds" to affect (ie. blocks of &100 bytes)
;
MSETUP: LD   (FINITW),HL      ; Do that trivial initialisation
	LD   (FINITP),A
	LD   (FOFFST),DE
	LD   (FPOINT),HL
	LD   (FCOUNT),A
	LD   A,C
	LD   (MDELAY),A
	LD   (MDELOR),A
	LD   A,B
	ADD  A,&58
	LD   (UNTIL+1),A
	LD   HL,MCOLOR
	LD   (VECTOR),HL      ; Grab new vector $Interrupts must be disabled$
	RET
;
; WROWS waits for an exact n*228 T states, as held in B on entry.
; The timing includes the LD B,n , CALL WROWS and RET statements.
; Therefore, to wait for, say, 5 horizontal scan lines, use
;   LD	 B,5
;   CALL WROWS
; This will wait exactly 5 rows from the T state at the beginning of the
; LD instruction until and inclusive the last T state of the RET statement
; in the subroutine.
; Uses only A and B registers. Returns c=1, Z=1 and both A and B=0
;
WROWS:	DEC  B		; 4   (7+17 FROM ABOVE)
	JP   Z,WC	;10
WA:	LD   A,11
WB:	SUB  1
	JP   NZ,WB
	NOP
	NOP
	NOP
	NOP
	NOP
	DEC  B
	JP   NZ,WA
WC:	LD   B,12	; 7
WD:	DJNZ WD 	; 151
	LD   A,0	; 7
	LD   B,0	; 7
	NOP		; 4
	NOP		; 4
	RET		; 10
;
MAKET1: LD   HL,FARVER
	LD   (HL),71
	INC  HL
	LD   B,18
MAKETA: LD   (HL),17
	INC  HL
	LD   (HL),49
	INC  HL
	DJNZ MAKETA
	LD   (HL),17
	INC  HL
	LD   (HL),103
	INC  HL
	LD   B,18*2
MAKETB: LD   (HL),&45
	INC  HL
	LD   (HL),&46
	INC  HL
	LD   (HL),&47
	INC  HL
	LD   (HL),&44
	INC  HL
	DJNZ MAKETB
	LD   (HL),71
	INC  HL
	LD   B,8
MAKETC: LD   (HL),0
	INC  HL
	DJNZ MAKETC
	RET
;
MAKET2: LD   HL,FARVER
	LD   B,24
MAKETD: LD   (HL),&46
	INC  HL
	LD   (HL),&46
	INC  HL
	LD   (HL),&42
	INC  HL
	LD   (HL),&42
	INC  HL
	LD   (HL),&44
	INC  HL
	LD   (HL),&44
	INC  HL
	LD   (HL),&41
	INC  HL
	LD   (HL),&41
	INC  HL
	DJNZ MAKETD
	LD   B,4
	JR   MAKETC
;
HOLDSP: DW   0
FCOUNT: DB   0
FINITP: DB   0
FINITW: DW   0
FPOINT: DW   0
FOFFST: DW   0

FARVE2:

DB 71
DB %01010110,%01101110
DB %01001110,%01101110,%01010110,%01101110
DB %01001110,%01101110,%01010110,%01101110
DB %01001110,%01101110,%01010110,%01101110
DB %01001110,%01101110,%01010110,%01101110
DB %01001110,%01101110,%01010110,%01101110
DB %01001110,%01101110,%01010110,%01101110
DB %01001110,%01101110,%01010110,%01101110
DB %01001110,%01101110,%01010110,%01101110
DB %01001110,%01101110,%01010110
DB 71

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

DB 7,2,7,2,7,2,7,2,7
FLASH: DB 2,7,2,0,0,0,0
DB 5,1,5,1,5,1,5,1,5,1,5,1,0,0,0,0
DB 3,6,3,6,3,6,3,6,3,6,3,6,0,0,0,0
DB 6,2,6,2,6,2,6,2,6,2,6,2,0,0,0,0
DB 4,1,4,1,4,1,4,1,4,1,4,1,0,0,0,0
DB 3,6,3,6,3,6,3,6,3,6,3,6,0,0,0,0

DB 6,5,4,3
DB 6,5,4,3
DB 6,5,4,3
DB 6,5,4,3
DB 6,5,4,3
DB 6,5,4,3
DB 6,5,4,3
DB 6,5,4,71
;
TITLE	'IMAGE7: Tables and'
EJECT
;
	ORG  ($/&100)*&100
;
; Table for mirroring bytes - index with low byte=value to be mirrored.
;
MIRTAB: DB   &00,&80,&40,&C0,&20,&A0,&60,&E0,&10,&90,&50,&D0,&30,&B0,&70,&F0
	DB   &08,&88,&48,&C8,&28,&A8,&68,&E8,&18,&98,&58,&D8,&38,&B8,&78,&F8
	DB   &04,&84,&44,&C4,&24,&A4,&64,&E4,&14,&94,&54,&D4,&34,&B4,&74,&F4
	DB   &0C,&8C,&4C,&CC,&2C,&AC,&6C,&EC,&1C,&9C,&5C,&DC,&3C,&BC,&7C,&FC
	DB   &02,&82,&42,&C2,&22,&A2,&62,&E2,&12,&92,&52,&D2,&32,&B2,&72,&F2
	DB   &0A,&8A,&4A,&CA,&2A,&AA,&6A,&EA,&1A,&9A,&5A,&DA,&3A,&BA,&7A,&FA
	DB   &06,&86,&46,&C6,&26,&A6,&66,&E6,&16,&96,&56,&D6,&36,&B6,&76,&F6
	DB   &0E,&8E,&4E,&CE,&2E,&AE,&6E,&EE,&1E,&9E,&5E,&DE,&3E,&BE,&7E,&FE
	DB   &01,&81,&41,&C1,&21,&A1,&61,&E1,&11,&91,&51,&D1,&31,&B1,&71,&F1
	DB   &09,&89,&49,&C9,&29,&A9,&69,&E9,&19,&99,&59,&D9,&39,&B9,&79,&F9
	DB   &05,&85,&45,&C5,&25,&A5,&65,&E5,&15,&95,&55,&D5,&35,&B5,&75,&F5
	DB   &0D,&8D,&4D,&CD,&2D,&AD,&6D,&ED,&1D,&9D,&5D,&DD,&3D,&BD,&7D,&FD
	DB   &03,&83,&43,&C3,&23,&A3,&63,&E3,&13,&93,&53,&D3,&33,&B3,&73,&F3
	DB   &0B,&8B,&4B,&CB,&2B,&AB,&6B,&EB,&1B,&9B,&5B,&DB,&3B,&BB,&7B,&FB
	DB   &07,&87,&47,&C7,&27,&A7,&67,&E7,&17,&97,&57,&D7,&37,&B7,&77,&F7
	DB   &0F,&8F,&4F,&CF,&2F,&AF,&6F,&EF,&1F,&9F,&5F,&DF,&3F,&BF,&7F,&FF
;
HUSKPC	EQU  $
	ORG  &7800
;
; Tables for rotating bytes - index with low byte=value to be rotated,
; and high byte=no. of rotations + (ROTTAB/&100)-1. cannot rotate 0/8.
;
ROTTAB: DB   &00,&01,&02,&03,&04,&05,&06,&07,&08,&09,&0A,&0B,&0C,&0D,&0E,&0F
	DB   &10,&11,&12,&13,&14,&15,&16,&17,&18,&19,&1A,&1B,&1C,&1D,&1E,&1F
	DB   &20,&21,&22,&23,&24,&25,&26,&27,&28,&29,&2A,&2B,&2C,&2D,&2E,&2F
	DB   &30,&31,&32,&33,&34,&35,&36,&37,&38,&39,&3A,&3B,&3C,&3D,&3E,&3F
	DB   &40,&41,&42,&43,&44,&45,&46,&47,&48,&49,&4A,&4B,&4C,&4D,&4E,&4F
	DB   &50,&51,&52,&53,&54,&55,&56,&57,&58,&59,&5A,&5B,&5C,&5D,&5E,&5F
	DB   &60,&61,&62,&63,&64,&65,&66,&67,&68,&69,&6A,&6B,&6C,&6D,&6E,&6F
	DB   &70,&71,&72,&73,&74,&75,&76,&77,&78,&79,&7A,&7B,&7C,&7D,&7E,&7F
	DB   &00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80
	DB   &00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80
	DB   &00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80
	DB   &00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80
	DB   &00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80
	DB   &00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80
	DB   &00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80
	DB   &00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80,&00,&80
	DB   &00,&00,&01,&01,&02,&02,&03,&03,&04,&04,&05,&05,&06,&06,&07,&07
	DB   &08,&08,&09,&09,&0A,&0A,&0B,&0B,&0C,&0C,&0D,&0D,&0E,&0E,&0F,&0F
	DB   &10,&10,&11,&11,&12,&12,&13,&13,&14,&14,&15,&15,&16,&16,&17,&17
	DB   &18,&18,&19,&19,&1A,&1A,&1B,&1B,&1C,&1C,&1D,&1D,&1E,&1E,&1F,&1F
	DB   &20,&20,&21,&21,&22,&22,&23,&23,&24,&24,&25,&25,&26,&26,&27,&27
	DB   &28,&28,&29,&29,&2A,&2A,&2B,&2B,&2C,&2C,&2D,&2D,&2E,&2E,&2F,&2F
	DB   &30,&30,&31,&31,&32,&32,&33,&33,&34,&34,&35,&35,&36,&36,&37,&37
	DB   &38,&38,&39,&39,&3A,&3A,&3B,&3B,&3C,&3C,&3D,&3D,&3E,&3E,&3F,&3F
	DB   &00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0
	DB   &00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0
	DB   &00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0
	DB   &00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0
	DB   &00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0
	DB   &00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0
	DB   &00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0
	DB   &00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0,&00,&40,&80,&C0
	DB   &00,&00,&00,&00,&01,&01,&01,&01,&02,&02,&02,&02,&03,&03,&03,&03
	DB   &04,&04,&04,&04,&05,&05,&05,&05,&06,&06,&06,&06,&07,&07,&07,&07
	DB   &08,&08,&08,&08,&09,&09,&09,&09,&0A,&0A,&0A,&0A,&0B,&0B,&0B,&0B
	DB   &0C,&0C,&0C,&0C,&0D,&0D,&0D,&0D,&0E,&0E,&0E,&0E,&0F,&0F,&0F,&0F
	DB   &10,&10,&10,&10,&11,&11,&11,&11,&12,&12,&12,&12,&13,&13,&13,&13
	DB   &14,&14,&14,&14,&15,&15,&15,&15,&16,&16,&16,&16,&17,&17,&17,&17
	DB   &18,&18,&18,&18,&19,&19,&19,&19,&1A,&1A,&1A,&1A,&1B,&1B,&1B,&1B
	DB   &1C,&1C,&1C,&1C,&1D,&1D,&1D,&1D,&1E,&1E,&1E,&1E,&1F,&1F,&1F,&1F
	DB   &00,&20,&40,&60,&80,&A0,&C0,&E0,&00,&20,&40,&60,&80,&A0,&C0,&E0
	DB   &00,&20,&40,&60,&80,&A0,&C0,&E0,&00,&20,&40,&60,&80,&A0,&C0,&E0
	DB   &00,&20,&40,&60,&80,&A0,&C0,&E0,&00,&20,&40,&60,&80,&A0,&C0,&E0
	DB   &00,&20,&40,&60,&80,&A0,&C0,&E0,&00,&20,&40,&60,&80,&A0,&C0,&E0
	DB   &00,&20,&40,&60,&80,&A0,&C0,&E0,&00,&20,&40,&60,&80,&A0,&C0,&E0
	DB   &00,&20,&40,&60,&80,&A0,&C0,&E0,&00,&20,&40,&60,&80,&A0,&C0,&E0
	DB   &00,&20,&40,&60,&80,&A0,&C0,&E0,&00,&20,&40,&60,&80,&A0,&C0,&E0
	DB   &00,&20,&40,&60,&80,&A0,&C0,&E0,&00,&20,&40,&60,&80,&A0,&C0,&E0
	DB   &00,&00,&00,&00,&00,&00,&00,&00,&01,&01,&01,&01,&01,&01,&01,&01
	DB   &02,&02,&02,&02,&02,&02,&02,&02,&03,&03,&03,&03,&03,&03,&03,&03
	DB   &04,&04,&04,&04,&04,&04,&04,&04,&05,&05,&05,&05,&05,&05,&05,&05
	DB   &06,&06,&06,&06,&06,&06,&06,&06,&07,&07,&07,&07,&07,&07,&07,&07
	DB   &08,&08,&08,&08,&08,&08,&08,&08,&09,&09,&09,&09,&09,&09,&09,&09
	DB   &0A,&0A,&0A,&0A,&0A,&0A,&0A,&0A,&0B,&0B,&0B,&0B,&0B,&0B,&0B,&0B
	DB   &0C,&0C,&0C,&0C,&0C,&0C,&0C,&0C,&0D,&0D,&0D,&0D,&0D,&0D,&0D,&0D
	DB   &0E,&0E,&0E,&0E,&0E,&0E,&0E,&0E,&0F,&0F,&0F,&0F,&0F,&0F,&0F,&0F
	DB   &00,&10,&20,&30,&40,&50,&60,&70,&80,&90,&A0,&B0,&C0,&D0,&E0,&F0
	DB   &00,&10,&20,&30,&40,&50,&60,&70,&80,&90,&A0,&B0,&C0,&D0,&E0,&F0
	DB   &00,&10,&20,&30,&40,&50,&60,&70,&80,&90,&A0,&B0,&C0,&D0,&E0,&F0
	DB   &00,&10,&20,&30,&40,&50,&60,&70,&80,&90,&A0,&B0,&C0,&D0,&E0,&F0
	DB   &00,&10,&20,&30,&40,&50,&60,&70,&80,&90,&A0,&B0,&C0,&D0,&E0,&F0
	DB   &00,&10,&20,&30,&40,&50,&60,&70,&80,&90,&A0,&B0,&C0,&D0,&E0,&F0
	DB   &00,&10,&20,&30,&40,&50,&60,&70,&80,&90,&A0,&B0,&C0,&D0,&E0,&F0
	DB   &00,&10,&20,&30,&40,&50,&60,&70,&80,&90,&A0,&B0,&C0,&D0,&E0,&F0
	DB   &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00
	DB   &01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01
	DB   &02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02
	DB   &03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03
	DB   &04,&04,&04,&04,&04,&04,&04,&04,&04,&04,&04,&04,&04,&04,&04,&04
	DB   &05,&05,&05,&05,&05,&05,&05,&05,&05,&05,&05,&05,&05,&05,&05,&05
	DB   &06,&06,&06,&06,&06,&06,&06,&06,&06,&06,&06,&06,&06,&06,&06,&06
	DB   &07,&07,&07,&07,&07,&07,&07,&07,&07,&07,&07,&07,&07,&07,&07,&07
	DB   &00,&08,&10,&18,&20,&28,&30,&38,&40,&48,&50,&58,&60,&68,&70,&78
	DB   &80,&88,&90,&98,&A0,&A8,&B0,&B8,&C0,&C8,&D0,&D8,&E0,&E8,&F0,&F8
	DB   &00,&08,&10,&18,&20,&28,&30,&38,&40,&48,&50,&58,&60,&68,&70,&78
	DB   &80,&88,&90,&98,&A0,&A8,&B0,&B8,&C0,&C8,&D0,&D8,&E0,&E8,&F0,&F8
	DB   &00,&08,&10,&18,&20,&28,&30,&38,&40,&48,&50,&58,&60,&68,&70,&78
	DB   &80,&88,&90,&98,&A0,&A8,&B0,&B8,&C0,&C8,&D0,&D8,&E0,&E8,&F0,&F8
	DB   &00,&08,&10,&18,&20,&28,&30,&38,&40,&48,&50,&58,&60,&68,&70,&78
	DB   &80,&88,&90,&98,&A0,&A8,&B0,&B8,&C0,&C8,&D0,&D8,&E0,&E8,&F0,&F8
	DB   &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00
	DB   &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00
	DB   &01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01
	DB   &01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01
	DB   &02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02
	DB   &02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02,&02
	DB   &03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03
	DB   &03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03,&03
	DB   &00,&04,&08,&0C,&10,&14,&18,&1C,&20,&24,&28,&2C,&30,&34,&38,&3C
	DB   &40,&44,&48,&4C,&50,&54,&58,&5C,&60,&64,&68,&6C,&70,&74,&78,&7C
	DB   &80,&84,&88,&8C,&90,&94,&98,&9C,&A0,&A4,&A8,&AC,&B0,&B4,&B8,&BC
	DB   &C0,&C4,&C8,&CC,&D0,&D4,&D8,&DC,&E0,&E4,&E8,&EC,&F0,&F4,&F8,&FC
	DB   &00,&04,&08,&0C,&10,&14,&18,&1C,&20,&24,&28,&2C,&30,&34,&38,&3C
	DB   &40,&44,&48,&4C,&50,&54,&58,&5C,&60,&64,&68,&6C,&70,&74,&78,&7C
	DB   &80,&84,&88,&8C,&90,&94,&98,&9C,&A0,&A4,&A8,&AC,&B0,&B4,&B8,&BC
	DB   &C0,&C4,&C8,&CC,&D0,&D4,&D8,&DC,&E0,&E4,&E8,&EC,&F0,&F4,&F8,&FC
	DB   &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00
	DB   &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00
	DB   &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00
	DB   &00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00,&00
	DB   &01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01
	DB   &01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01
	DB   &01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01
	DB   &01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01,&01
	DB   &00,&02,&04,&06,&08,&0A,&0C,&0E,&10,&12,&14,&16,&18,&1A,&1C,&1E
	DB   &20,&22,&24,&26,&28,&2A,&2C,&2E,&30,&32,&34,&36,&38,&3A,&3C,&3E
	DB   &40,&42,&44,&46,&48,&4A,&4C,&4E,&50,&52,&54,&56,&58,&5A,&5C,&5E
	DB   &60,&62,&64,&66,&68,&6A,&6C,&6E,&70,&72,&74,&76,&78,&7A,&7C,&7E
	DB   &80,&82,&84,&86,&88,&8A,&8C,&8E,&90,&92,&94,&96,&98,&9A,&9C,&9E
	DB   &A0,&A2,&A4,&A6,&A8,&AA,&AC,&AE,&B0,&B2,&B4,&B6,&B8,&BA,&BC,&BE
	DB   &C0,&C2,&C4,&C6,&C8,&CA,&CC,&CE,&D0,&D2,&D4,&D6,&D8,&DA,&DC,&DE
	DB   &E0,&E2,&E4,&E6,&E8,&EA,&EC,&EE,&F0,&F2,&F4,&F6,&F8,&FA,&FC,&FE
;
IF ONE28
;
	ORG  &7000   ; Colours for won game goes here - &600 bytes
;
PALETE: DB    17,21,17,21,49,53,49,53,17,21,17,21,49,53,49,53
	DB    17,21,17,21,49,53,49,53,17,21,17,21,49,53,49,53
	DB    17,21,17,21,49,53,49,53,17,21,17,21,49,53,49,53
	DB    17,21,17,21,49,53,49,53,17,21,17,21,49,53,49,53
	DB    17,21,17,21,49,53,49,53,17,21,17,21,49,53,49,53
	DB    17,21,17,21,49,53,49,53,17,21,17,21,49,53,49,53
	DB    17,21,17,21,49,53,49,53,17,21,17,21,49,53,49,53
	DB    17,21,17,21,49,53,49,53,17,21,17,21,49,53,49,53
	DB    17,21,17,21,49,53,49,53,17,21,17,21,49,53,49,53
	DB    17,21,17,21,49,53,49,53,17,21,17,21,49,53,49,53
	DB    17,21,17,21,49,53,49,53,17,21,17,21,49,53,49,53
	DB    17,21,17,21,49,53,49,53,17,21,17,21,49,53,49,53
	DB    49,21,17,21,17,53,49,53,49,21,17,21,17,53,49,53
	DB    49,21,17,21,17,53,49,53,49,21,17,21,17,53,49,53
	DB    49,21,17,21,17,53,49,53,49,21,17,21,17,53,49,53
	DB    49,21,17,21,17,53,49,53,49,21,17,21,17,53,49,53
	DB    49,21,17,21,17,53,49,53,49,21,17,21,17,53,49,53
	DB    49,21,17,21,17,53,49,53,49,21,17,21,17,53,49,53
	DB    49,21,17,21,17,53,49,53,49,21,17,21,17,53,49,53
	DB    49,21,17,21,17,53,49,53,49,21,17,21,17,53,49,53
	DB    49,21,17,21,17,53,49,53,49,21,17,21,17,53,49,53
	DB    49,21,17,21,17,53,49,53,49,21,17,21,17,53,49,53
	DB    49,21,17,21,17,53,49,53,49,21,17,21,17,53,49,53
	DB    49,21,17,21,17,53,49,53,49,21,17,21,17,53,49,53
	DB    49,53,17,21,17,21,49,53,49,53,17,21,17,21,49,53
	DB    49,53,17,21,17,21,49,53,49,53,17,21,17,21,49,53
	DB    49,53,17,21,17,21,49,53,49,53,17,21,17,21,49,53
	DB    49,53,17,21,17,21,49,53,49,53,17,21,17,21,49,53
	DB    49,53,17,21,17,21,49,53,49,53,17,21,17,21,49,53
	DB    49,53,17,21,17,21,49,53,49,53,17,21,17,21,49,53
	DB    49,53,17,21,17,21,49,53,49,53,17,21,17,21,49,53
	DB    49,53,17,21,17,21,49,53,49,53,17,21,17,21,49,53
	DB    49,53,17,21,17,21,49,53,49,53,17,21,17,21,49,53
	DB    49,53,17,21,17,21,49,53,49,53,17,21,17,21,49,53
	DB    49,53,17,21,17,21,49,53,49,53,17,21,17,21,49,53
	DB    49,53,17,21,17,21,49,53,49,53,17,21,17,21,49,53
	DB    49,53,49,21,17,21,17,53,49,53,49,21,17,21,17,53
	DB    49,53,49,21,17,21,17,53,49,53,49,21,17,21,17,53
	DB    49,53,49,21,17,21,17,53,49,53,49,21,17,21,17,53
	DB    49,53,49,21,17,21,17,53,49,53,49,21,17,21,17,53
	DB    49,53,49,21,17,21,17,53,49,53,49,21,17,21,17,53
	DB    49,53,49,21,17,21,17,53,49,53,49,21,17,21,17,53
	DB    49,53,49,21,17,21,17,53,49,53,49,21,17,21,17,53
	DB    49,53,49,21,17,21,17,53,49,53,49,21,17,21,17,53
	DB    49,53,49,21,17,21,17,53,49,53,49,21,17,21,17,53
	DB    49,53,49,21,17,21,17,53,49,53,49,21,17,21,17,53
	DB    49,53,49,21,17,21,17,53,49,53,49,21,17,21,17,53
	DB    49,53,49,21,17,21,17,53,49,53,49,21,17,21,17,53
	DB    49,53,49,53,17,21,17,21,49,53,49,53,17,21,17,21
	DB    49,53,49,53,17,21,17,21,49,53,49,53,17,21,17,21
	DB    49,53,49,53,17,21,17,21,49,53,49,53,17,21,17,21
	DB    49,53,49,53,17,21,17,21,49,53,49,53,17,21,17,21
	DB    49,53,49,53,17,21,17,21,49,53,49,53,17,21,17,21
	DB    49,53,49,53,17,21,17,21,49,53,49,53,17,21,17,21
	DB    49,53,49,53,17,21,17,21,49,53,49,53,17,21,17,21
	DB    49,53,49,53,17,21,17,21,49,53,49,53,17,21,17,21
	DB    49,53,49,53,17,21,17,21,49,53,49,53,17,21,17,21
	DB    49,53,49,53,17,21,17,21,49,53,49,53,17,21,17,21
	DB    49,53,49,53,17,21,17,21,49,53,49,53,17,21,17,21
	DB    49,53,49,53,17,21,17,21,49,53,49,53,17,21,17,21
	DB    17,53,49,53,49,21,17,21,17,53,49,53,49,21,17,21
	DB    17,53,49,53,49,21,17,21,17,53,49,53,49,21,17,21
	DB    17,53,49,53,49,21,17,21,17,53,49,53,49,21,17,21
	DB    17,53,49,53,49,21,17,21,17,53,49,53,49,21,17,21
	DB    17,53,49,53,49,21,17,21,17,53,49,53,49,21,17,21
	DB    17,53,49,53,49,21,17,21,17,53,49,53,49,21,17,21
	DB    17,53,49,53,49,21,17,21,17,53,49,53,49,21,17,21
	DB    17,53,49,53,49,21,17,21,17,53,49,53,49,21,17,21
	DB    17,53,49,53,49,21,17,21,17,53,49,53,49,21,17,21
	DB    17,53,49,53,49,21,17,21,17,53,49,53,49,21,17,21
	DB    17,53,49,53,49,21,17,21,17,53,49,53,49,21,17,21
	DB    17,53,49,53,49,21,17,21,17,53,49,53,49,21,17,21
	DB    17,21,49,53,49,53,17,21,17,21,49,53,49,53,17,21
	DB    17,21,49,53,49,53,17,21,17,21,49,53,49,53,17,21
	DB    17,21,49,53,49,53,17,21,17,21,49,53,49,53,17,21
	DB    17,21,49,53,49,53,17,21,17,21,49,53,49,53,17,21
	DB    17,21,49,53,49,53,17,21,17,21,49,53,49,53,17,21
	DB    17,21,49,53,49,53,17,21,17,21,49,53,49,53,17,21
	DB    17,21,49,53,49,53,17,21,17,21,49,53,49,53,17,21
	DB    17,21,49,53,49,53,17,21,17,21,49,53,49,53,17,21
	DB    17,21,49,53,49,53,17,21,17,21,49,53,49,53,17,21
	DB    17,21,49,53,49,53,17,21,17,21,49,53,49,53,17,21
	DB    17,21,49,53,49,53,17,21,17,21,49,53,49,53,17,21
	DB    17,21,49,53,49,53,17,21,17,21,49,53,49,53,17,21
	DB    17,21,17,53,49,53,49,21,17,21,17,53,49,53,49,21
	DB    17,21,17,53,49,53,49,21,17,21,17,53,49,53,49,21
	DB    17,21,17,53,49,53,49,21,17,21,17,53,49,53,49,21
	DB    17,21,17,53,49,53,49,21,17,21,17,53,49,53,49,21
	DB    17,21,17,53,49,53,49,21,17,21,17,53,49,53,49,21
	DB    17,21,17,53,49,53,49,21,17,21,17,53,49,53,49,21
	DB    17,21,17,53,49,53,49,21,17,21,17,53,49,53,49,21
	DB    17,21,17,53,49,53,49,21,17,21,17,53,49,53,49,21
	DB    17,21,17,53,49,53,49,21,17,21,17,53,49,53,49,21
	DB    17,21,17,53,49,53,49,21,17,21,17,53,49,53,49,21
	DB    17,21,17,53,49,53,49,21,17,21,17,53,49,53,49,21
	DB    17,21,17,53,49,53,49,21,17,21,17,53,49,53,49,21
;
ENDIF
;
	ORG  HUSKPC ; back again!
;
; Interrupt request jump block - 257 bytes as the answer from the peripheral
; device cannot be relied upon.
;
JMPBLK: DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT,INTRPT
	DW   INTRPT
;
; The Character set graphics - only 0-9, capitals A-Z and
; full stop + space are represented. Used by VDU handler.
;
FONT:	DB   &00,&00,&00,&00,&00,&00,&00,&00 ; <SPACE>
	DB   &78,&FC,&CC,&CC,&CC,&CC,&FC,&78 ; 0
	DB   &60,&E0,&E0,&60,&60,&60,&60,&60 ; 1
	DB   &F8,&FC,&0C,&7C,&F8,&00,&FC,&FC ; 2
	DB   &F8,&F0,&00,&30,&78,&18,&F8,&F0 ; 3
	DB   &1C,&38,&70,&FC,&FC,&00,&0C,&0C ; 4
	DB   &FC,&FC,&00,&F8,&FC,&0C,&FC,&F8 ; 5
	DB   &1C,&38,&70,&E0,&CC,&CC,&FC,&78 ; 6
	DB   &FC,&FC,&00,&0C,&18,&18,&30,&30 ; 7
	DB   &38,&7C,&44,&38,&7C,&C6,&FE,&7C ; 8
	DB   &78,&FC,&CC,&CC,&1C,&38,&70,&E0 ; 9
	DB   &00,&00,&00,&00,&00,&00,&18,&18 ; . (use ':')
	DB   &18,&18,&18,&0C,&6C,&6C,&FE,&FE ; A
	DB   &FC,&FE,&06,&FE,&FC,&06,&FE,&FC ; B
	DB   &3E,&7E,&E0,&C0,&C0,&E0,&7E,&3E ; C
	DB   &F8,&FC,&0E,&C6,&C6,&CE,&FC,&F8 ; D
	DB   &FE,&FE,&00,&FE,&FE,&C0,&FE,&FE ; E
	DB   &FE,&FE,&00,&FE,&FE,&C0,&C0,&C0 ; F
	DB   &3E,&7E,&E0,&CE,&CE,&E6,&7E,&3E ; G
	DB   &C6,&C6,&C6,&FE,&FE,&C6,&C6,&C6 ; H
	DB   &C0,&C0,&00,&C0,&C0,&C0,&C0,&C0 ; I
	DB   &18,&18,&00,&18,&18,&38,&F0,&E0 ; J
	DB   &1C,&38,&70,&E0,&E0,&70,&38,&1C ; K
	DB   &C0,&C0,&C0,&C0,&C0,&C0,&FC,&FC ; L
	DB   &FC,&FE,&D6,&D6,&D6,&D6,&D6,&D6 ; M
	DB   &FC,&FE,&C6,&C6,&C6,&C6,&C6,&C6 ; N
	DB   &78,&FC,&CC,&CC,&CC,&CC,&FC,&78 ; O (as 0)
	DB   &FC,&FE,&06,&FE,&FC,&C0,&C0,&C0 ; P
	DB   &7C,&FE,&EE,&C6,&C6,&EE,&F6,&78 ; Q
	DB   &FC,&FE,&06,&FE,&FC,&0E,&06,&06 ; R
	DB   &30,&30,&38,&1C,&0E,&06,&FE,&FC ; S
	DB   &FC,&FC,&00,&C0,&C0,&E0,&7C,&3C ; T
	DB   &C6,&C6,&C6,&C6,&C6,&E6,&FE,&7E ; U
	DB   &C6,&C6,&66,&66,&3C,&3C,&18,&18 ; V
	DB   &C6,&C6,&D6,&D6,&6C,&6C,&6C,&6C ; W
	DB   &C6,&6C,&7C,&38,&38,&7C,&6C,&C6 ; X
	DB   &C6,&C6,&EE,&7C,&38,&00,&38,&38 ; Y
	DB   &FE,&FE,&00,&1C,&38,&70,&FE,&FE ; Z
;
;	JETPACK GRAPHICS - Heavily encoded and compressed.
;
FLAME1: DB   0,0,12,8,18,44,42,52,20,42,41,84,54,110,26,52,20,44,8,24,0,0
	DB   225,227,193,193,128,129,128,129,128,0,128,0,128,0,128,1,193,129
	DB   195,129,227,195
FL1A:	DB   0,0,10,40,20,84,27,44,37,82,41,74,54,116,20,44,26,48,12,24,0,0
	DB   225,195,192,129,192,1,128,0,128,0,128,0,128,1,128,1,192,129,192
	DB   131,225,195
FL1B:	DB   0,0,14,56,21,84,14,40,9,84,22,52,21,72,26,48,9,40,6,16,0,0
	DB   225,195,192,131,192,1,192,1,192,1,192,1,192,1,192,3,224,3,224,3
	DB   240,199
;	
FLAME2: DB   0,0,2,128,5,64,7,64,10,128,10,160,5,64,2,160,5,192,2,128,0,0
	DB   248,63,240,31,240,31,224,31,224,15,224,15,224,15,240,15,240,31
	DB   240,31,248,63
FL2A:	DB   0,0,1,128,2,160,1,192,6,160,5,208,2,160,7,64,2,160,1,64,0,0
	DB   252,63,252,31,248,15,240,15,240,7,240,7,240,7,240,15,240,15,248
	DB   15,252,31
FL2B:	DB   0,0,3,64,5,224,14,160,5,80,11,160,12,64,3,96,5,64,2,128,0,0
	DB   252,63,240,15,224,15,224,7,224,7,224,7,224,15,224,15,240,15,240
	DB   31,248,63
;
FLAME3: DB   0,0,2,192,82,160,57,64,69,64,40,160,85,64,85,64,40,192,16,0,0,0
	DB   252,31,136,15,0,15,0,15,0,15,0,15,0,15,0,31,0,31,130,31,199,255
FL3A:	DB   0,0,1,128,58,64,85,192,52,128,105,64,41,64,85,0,36,128,24,0,0,0
	DB   252,63,128,31,0,31,0,31,0,31,0,31,0,31,0,31,0,31,128,63,195,255
FL3B:	DB   0,0,1,64,90,160,85,128,36,160,121,80,76,224,84,64,42,224,52,0,0,0
	DB   252,31,192,15,0,15,0,15,0,7,0,7,0,7,0,15,0,15,128,15,129,255
;
FLAME4: DB   0,0,3,64,5,74,2,156,2,162,5,20,2,170,2,170,3,20,0,8,0,0
	DB   248,63,240,17,240,0,240,0,240,0,240,0,240,0,248,0,248,0,248
	DB   65,255,227
FL4A:	DB   0,0,1,128,2,92,3,170,1,44,2,150,2,148,0,170,1,36,0,24,0,0
	DB   252,63,248,1,248,0,248,0,248,0,248,0,248,0,248,0,248,0,252
	DB   1,255,195
FL4B:	DB   0,0,2,128,5,90,1,170,5,36,10,158,7,50,2,42,7,84,0,44,0,0
	DB   248,63,240,3,240,0,240,0,224,0,224,0,224,0,240,0,240,0,240
	DB   1,255,129
;
;	GRAPHICS FOR ELEVATOR DOORS
;
ELEVAT: DB   170,85,128,66,133,74,148,64,191,85,170,85,170,85
	DB   171,87,174,95,170,85,170,85,170,85,170,85,170,85
	DB   170,85,0,16,32,64,128,0,255,85,178,113,232,212,170
	DB   85,170,212,232,212,232,212,232,212,232,192,170,85
	DB   170,85,2,5,6,5,6,5,254,85,170,85,170,85,42,21,138
	DB   5,170,85,170,85,170,85,170,85,170,85
	DB   170,85,128,66,133,74,148,64,191,85,170,85,170,85
	DB   170,85,170,85,170,85,175,87,171,85,170,85,170,85
	DB   170,85,0,16,32,64,128,0,255,85,254,212,232,212,232
	DB   212,232,212,232,212,234,85,170,212,232,113,186,85
	DB   170,85,2,5,6,5,6,5,254,85,170,85,170,85,170,85,170
	DB   85,170,85,10,21,42,85,170,85,170,85
;
;	GRAPHICS FOR SMALL CAMERAS
;
CAMERA: DB   170,84,169,70,152,94,159,95
	DB   135,97,98,18,172,81,170,85
	DB   170,85,138,101,24,6,154,236
	DB   154,146,76,1,170,85,170,85
	DB   170,85,170,84,171,86,167,83
	DB   139,101,105,18,172,81,170,85
	DB   170,85,170,5,242,9,10,5
	DB   132,130,218,165,165,90,160,85
	DB   170,84,169,71,159,127,95,54
	DB   104,73,178,69,170,85,170,85
	DB   170,85,138,225,250,233,138,25
	DB   96,134,70,73,178,69,170,85
	DB   170,85,170,80,175,95,159,63
	DB   191,102,90,37,37,90,128,85
	DB   170,85,170,21,202,165,170,85
	DB   72,150,166,73,50,69,170,85
;
;	GRAPHICS FOR RADIATION DANGER SIGN
;
DANGER: DB   0,7,15,7,7,3,2,1
	DB   125,126,126,60,60,56,24,0
	DB   0,224,240,224,224,192,64,128
	DB   190,126,126,60,60,28,24,0
;
;	A SMALL RADIOACTIVE ROD TO PRINT ON STATUS BAR.
;
RODCHR: DB   60,66,126,122,122,122,122,60
;
;	GRAPHICS FOR ROBOTS DIFFERENT HEAD POSITIONS
;	(TWO BYTES GRAPHIC DATA 8 LINES DOWN, THEN SAME FOR MASK)
;	... 32 BYTES PER POSITION!
;
ROBOTH: DB   &00,&00,&0F,&F0,&08,&10,&08,&10,&0C,&30,&0B,&D0,&24,&24,&53,&C2
	DB   &FF,&FF,&F0,&0F,&F0,&0F,&F0,&0F,&F0,&0F,&F0,&0F,&C0,&0B,&80,&01
	DB   &03,&80,&0C,&60,&10,&18,&10,&08,&18,&38,&17,&C8,&28,&38,&57,&C2
	DB   &FC,&7F,&F0,&1F,&E0,&07,&E0,&07,&E0,&07,&E0,&07,&C0,&03,&80,&01
	DB   &00,&00,&07,&F0,&08,&10,&08,&10,&0F,&F0,&08,&10,&27,&F4,&50,&02
	DB   &FF,&FF,&F8,&0F,&F0,&0F,&F0,&0F,&F0,&0F,&F0,&0F,&C0,&0B,&80,&01
	DB   &03,&C0,&0C,&30,&10,&18,&18,&68,&17,&A8,&11,&18,&19,&60,&47,&82
	DB   &FC,&3F,&F0,&0F,&E0,&07,&E0,&07,&E0,&07,&E0,&07,&C0,&03,&80,&01
	DB   &03,&C0,&04,&20,&08,&10,&08,&10,&0F,&F0,&09,&90,&2F,&F4,&50,&02
	DB   &FC,&3F,&F8,&1F,&F0,&0F,&F0,&0F,&F0,&0F,&F0,&0F,&C0,&03,&80,&01
	DB   &03,&C0,&0C,&30,&18,&08,&16,&18,&15,&E8,&18,&88,&06,&98,&41,&E2
	DB   &FC,&3F,&F0,&0F,&E0,&07,&E0,&07,&E0,&07,&E0,&07,&C0,&03,&80,&01
	DB   &00,&00,&0F,&E0,&08,&10,&08,&10,&0F,&F0,&08,&10,&2F,&E4,&40,&0A
	DB   &FF,&FF,&F0,&1F,&F0,&0F,&F0,&0F,&F0,&0F,&F0,&0F,&D0,&03,&80,&01
	DB   &01,&C0,&06,&30,&18,&08,&10,&08,&1C,&18,&13,&E8,&1C,&14,&43,&EA
	DB   &FE,&3F,&F8,&0F,&E0,&07,&E0,&07,&E0,&07,&E0,&07,&C0,&03,&80,&01
;
;	OUTLINE DATA - Used for outlining Axis doors.
;
OLDATA: DB   %11111100,%11110000,%11000000,%00000011,%00001111,%00111111 ;LEFT
ORDATA: DB   %00111111,%00001111,%00000011,%11000000,%11110000,%11111100 ;RIGHT
;
;	DISPLAY FILE LEFT-HAND ADDRESSES - Index with Y coordinate * 2.
;					
SCRTAB: DW   &4000,&4100,&4200,&4300,&4400,&4500,&4600,&4700 ; 1st half
	DW   &4020,&4120,&4220,&4320,&4420,&4520,&4620,&4720
	DW   &4040,&4140,&4240,&4340,&4440,&4540,&4640,&4740
	DW   &4060,&4160,&4260,&4360,&4460,&4560,&4660,&4760
	DW   &4080,&4180,&4280,&4380,&4480,&4580,&4680,&4780
	DW   &40A0,&41A0,&42A0,&43A0,&44A0,&45A0,&46A0,&47A0
	DW   &40C0,&41C0,&42C0,&43C0,&44C0,&45C0,&46C0,&47C0
	DW   &40E0,&41E0,&42E0,&43E0,&44E0,&45E0,&46E0,&47E0
	DW   &4800,&4900,&4A00,&4B00,&4C00,&4D00,&4E00,&4F00 ; 2nd half
	DW   &4820,&4920,&4A20,&4B20,&4C20,&4D20,&4E20,&4F20
	DW   &4840,&4940,&4A40,&4B40,&4C40,&4D40,&4E40,&4F40
	DW   &4860,&4960,&4A60,&4B60,&4C60,&4D60,&4E60,&4F60
	DW   &4880,&4980,&4A80,&4B80,&4C80,&4D80,&4E80,&4F80
	DW   &48A0,&49A0,&4AA0,&4BA0,&4CA0,&4DA0,&4EA0,&4FA0
	DW   &48C0,&49C0,&4AC0,&4BC0,&4CC0,&4DC0,&4EC0,&4FC0
	DW   &48E0,&49E0,&4AE0,&4BE0,&4CE0,&4DE0,&4EE0,&4FE0
	DW   &5000,&5100,&5200,&5300,&5400,&5500,&5600,&5700 ; 3rd half
	DW   &5020,&5120,&5220,&5320,&5420,&5520,&5620,&5720
	DW   &5040,&5140,&5240,&5340,&5440,&5540,&5640,&5740
	DW   &5060,&5160,&5260,&5360,&5460,&5560,&5660,&5760
	DW   &5080,&5180,&5280,&5380,&5480,&5580,&5680,&5780
	DW   &50A0,&51A0,&52A0,&53A0,&54A0,&55A0,&56A0,&57A0
	DW   &50C0,&51C0,&52C0,&53C0,&54C0,&55C0,&56C0,&57C0
	DW   &50E0,&51E0,&52E0,&53E0,&54E0,&55E0,&56E0,&57E0
;
TITLE	'IMAGE7a: Tables and text'
;
;	RELATIVE TOKENS - Can only contain true objects - no backgrounds.
;	Used for frequently appearing combinations of objects on screen.
;
;  Token     Contents
;  -----     --------
;    1	     Door direction 3
;    2	     Door direction 5
;    3	     Door direction 4
;    4	     Door direction 2
;    5	     Door direction 0
;    6	     Door direction 6
;    7	     Door direction 1
;    8	     Door direction 7
;    9	     Tall cube acc.
;    A	     Barrel
;    B	     Door direction 0 with elevator inline going UP
;    C	     Door direction 0 with elevator inline going DOWN
;    D	     Decontamination shower
;
;
TOKEN1: DB   70,0,12,0	; Door image token (door 1) \
	DB   71,0,0,0
	DB   26,0,6,26
	DB   0
	DB   0		; No colour
TOKEN2: DB   75,12,0,0	; Door image token (door 2) /
	DB   74,0,0,0
	DB   27,6,0,26
	DB   0
	DB   0		; No colour
TOKEN3: DB   73,0,0,0	; Door image token (door 3) -
	DB   72,-8,8,0
	DB   24,-8,8,31
	DB   25,0,0,31
	DB   0
	DB   1		; Any ink, black paper
	DB   &3E,&42,&1E,&22,2,-2,-&1E,-&22,-&42,-&41,-&40,-&3F,-&3E
	DB   &10
TOKEN4: DB   68,7,7,0
	DB   69,0,0,0
	DB   28,0,0,32
	DB   0
	DB   1
	DB   -&7E,-&5E,-&3E,-&1E,2,&22,&42
	DB   &10
TOKEN5: DB   88,0,0,0
	DB   64,0,0,0	; Door image token (door 3) -
	DB   65,-8,8,0
	DB   24,-8,8,31
	DB   25,0,0,31
	DB   0
	DB   3		; Similar ink
	DB   &3E,&42,&1E,&22,2,-2,-&1E,-&22,-&42,-&41,-&40,-&3F,-&3E
	DB   &10
TOKEN6: DB   77,8,8,0
	DB   76,0,0,0
	DB   28,0,0,32
	DB   0
	DB   1
	DB   -&7E,-&5E,-&3E,-&1E,2,&22,&42
	DB   &10
TOKEN7: DB   89,0,0,0
	DB   66,12,0,0	; Door image token (door 2) /
	DB   67,0,0,0
	DB   27,6,0,26
	DB   0
	DB   0
TOKEN8: DB   90,0,0,0
	DB   79,0,12,0	; Door image token (door 1) \
	DB   78,0,0,0
	DB   26,0,6,26
	DB   0
	DB   0
TOKEN9: DB   20,0,0,0	; Pole cylinder
	DB   20,0,0,8
	DB   20,0,0,16
	DB   20,0,0,24
	DB   20,0,0,32
	DB   0
	DB   0
TOKENA: DB   22,0,0,0
	DB   0
	DB   2
	DB   &21,&22,&41,&42
	DB   &10
TOKENB: DB   88,0,0,0
	DB   64,0,0,0	; Door image token (door 3) -
	DB   65,-8,8,0
	DB   24,-8,8,31
	DB   25,0,0,31
	DB   91,0,0,0
	DB   0
	DB   3		; Similar ink
	DB   &3E,&42,&1E,&22,2,-2,-&1E,-&22,-&42,-&41,-&40,-&3F,-&3E
	DB   &10
TOKENC: DB   88,0,0,0
	DB   64,0,0,0	; Door image token (door 3) -
	DB   65,-8,8,0
	DB   24,-8,8,31
	DB   25,0,0,31
	DB   92,0,0,0
	DB   0
	DB   3		; Similar ink
	DB   &3E,&42,&1E,&22,2,-2,-&1E,-&22,-&42,-&41,-&40,-&3F,-&3E
	DB   &10
TOKEND: DB   37,0,0,0
	DB   36,0,0,32
	DB   0
	DB   0
TOKENE:
TOKENF:
TOKENG:
TOKENH:
TOKENI: DB   0
;
; Holemacros:
;
MACROS: DB   1,7,1,0,0,0,0,0
	DB   7,2,7,0,0,0,0,0
	DB   1,1,0,0,0,0,0,0
	DB   7,1,1,7,0,0,0,0
	DB   1,7,1,0,0,0,0,0
	DB   7,7,7,7,0,0,0,0
	DB   9,9,9,9,0,0,0,0
	DB   8,6,1,1,0,0,0,0
	DB   1,1,6,8,0,0,0,0
;
; This is a (bad) table of hole locations which may be occupied through
; collision problems in front of doors. Format is X,Y ... ,255 for each
; room. X and Y are NOT Universe Coordinates, they are Matrix Coordinates.
; It's a kind of an EXEPTION TABLE.
;
EXCEPT: DB   255	     ; (00) (indicator only)
	DB   7,7,8,2,255
	DB   7,7,255
	DB   255
	DB   4,1,255
	DB   4,6,255
	DB   2,1,4,3,4,4,4,5,5,4,7,5,255
	DB   1,6,255
	DB   255
	DB   255
	DB   8,2,7,7,2,8,255 ; (10)
	DB   8,2,255
	DB   1,2,255
	DB   7,7,255
	DB   4,8,255
	DB   255
	DB   6,2,255
	DB   255
	DB   6,4,5,4,6,8,255
	DB   8,6,4,5,4,4,255
	DB   2,3,6,4,6,5,6,6,6,7,255 ; (20)
	DB   3,2,255	     ; That's half of forty-two, d'ya realize that?
;
; THE ADDRESS TABLES - used for indexing into various data tables:
;
; ACTION ADDRESS TABLE	   ; ACTN servers
; GROL ADDRESS TABLE	   : GROL servers
; TOKEN ADDRESS TABLE	   : Tokens 1..n
; ROOM ADDRESS TABLE	   : Room 1..n
; BACKGROUND ADDRESS TABLE : Background from behind dynamic objects
; BOX ADDRESS TABLE	   : Different box drawing routines
; WALL ADDRESS TABLE	   : For drawing walls
; SPECIAL OBJECTS ADDR TAB : For printing up special cases
;
AADTAB: DW   ACTN1,ACTN2,ACTN3,ACTN4,ACTN5,ACTN6,ACTN7,ACTN8,ACTN9,ACTNA,ACTNB
	DW   ACTNC,ACTND,ACTNE,ACTNF,ACTNG,ACTNH
GADTAB: DW   GROL1,GROL2,GROL3,GROL4,GROL5,GROL6,GROL7,GROL8,GROL9,GROLA,GROLB
TADTAB: DW   TOKEN1,TOKEN2,TOKEN3,TOKEN4,TOKEN5,TOKEN6,TOKEN7,TOKEN8,TOKEN9
	DW   TOKENA,TOKENB,TOKENC,TOKEND,TOKENE,TOKENF,TOKENG,TOKENH,TOKENI
BCKTAB: DW   BACKGR+&0000,BACKGR+&00A0,BACKGR+&0140,BACKGR+&01E0
	DW   BACKGR+&0280,BACKGR+&0320,BACKGR+&03C0,BACKGR+&0460
BADTAB: DW   BOXA,BOXB,BOXC,BOXD,BOXE,BOXF,BOXG,LINER,LINEF,LINEH,LINEV
SADTAB: DW   OUTLNE,OUTLFT,OUTRGT,ELEVU,ELEVD,FLAT,FLAT,FLAT,FLAT
;
MANDAT: DB   4,1,2,1,2,3,0,0,0,0,0,0,0,0,0,5  ; Initial main sprite info.
;
POFFST: DB   21*4+1,19*4+2,20*4+2,0,0,0,20*4,19*4
;
MIDSCR: DB   32,32,0 ; Middle of 3D screen. This is where the main sprite 
;		       appears when no matching entry door is found.
;
MANXYZ: DB   0,0,0   ; Old XYZ coords before a room change (used with holes)
;
SHTFCE: DB   14,15,14,13 ; Patch to man graphics when going through an elevator
;
XADJST: DB   255,8,8,8,255,255,8,8 ; Wall adjustments for hole printup
;
ELEMNT: DB   4,1,16,16,16,16,0,0,0
CELEMT: DB   0,0
	DB   0,0
CELERA: DB   0,0
CELEGR: DB   7
	DB   0
;
; MIRROR FLAG TABLE - &60 flags denoting the mirror state of physical
; objects in memory. These flags are not initialized.
;
MIRFLG: DB   0,0,0,0,0,0,0,0,0,0,0,0
;
; TABLE OF OBJECTS WHICH CAN BE CARRIED FROM ROOM TO ROOM.
; Format is Room number,Physical image,ACTION server,X,Y,Z (6 bytes)
;
; The table has "blocks" of 32 bytes in which 5 rod appears in the above 
; format. Then a delimiter &00 and a fill-up byte (0) for nice block size.
; Each block corresponds to one floor level in the game, and starts off
; at level 2; so there are no rods at level 1. In fact, level 1 is pretty
; boring.
;
; Room is defined as: &00-&93 : Rooms (mod 21=room, div 21=floor)
;			  &C0 : non-existent
;			  &FF : Held
;
DDOACT	EQU  &D
;
AUXTAB: DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0,1+(GROD-DIMTAB)/4,DDOACT,0,0,0
	DB   0
;
AUXORG: DB   32+3,&12,&23,0
	DB   32+16,&3F,&34,0
	DB   0+2,&33,&33,0
	DB   64+6,&3D,&29,40
	DB   64+20,&2B,&13,0
	DB   96+14,&D,&34,0
	DB   96+5,&17,&41,0
	DB   96+8,&19,&3D,0
	DB   128+11,&1D,&42,0
	DB   0+17,&1B,&1B,0
	DB   0+16,&19,&13,0
	DB   128+12,&2A,&22,0
	DB   160+11,&30,&2B,0
	DB   0+20,&13,&28,0
	DB   160+4,&12,&2A,0
	DB   192+14,&44,&1C,0
	DB   192+21,&12,&2A,0
	DB   192+4,&3D,&17,0
	DB   0
;
; ASCII values of keys as they are read from the keyboard. (40 keys)
; 30d is CAPS SHIFT, 31d is SYMBOL SHIFT and 13d is ENTER
;
KEYTAB: DB   ' ',31,'MNB',13,'LKJHPOIUY0987612345QWERTASDFG',30,'ZXCV'
;
; HIGH SCORE TABLE - ten entries @ 2 bytes
;
HSCTAB: DW   750,650,550,450,350,250,150,50
;
;LEVELS: DB   0,'YELLOW',0,'CYAN',0,'GREEN',0,'MAGENTA',0,'RED',0,'BLUE',0
;	 DB   'WHITE',0
;
;	SEQUENCES OF STATIC GRAPHICS ON SCREEN:- (ref. SEQS)
;
SEQS1:	DB   2
	DW   PSEUDO+&0304
	DB   &16,160
	DB   2
	DW   PSEUDO+&0007
	DB   &10,39
	DB   3
	DW   PSEUDO+&004B
	DB   7,%110,'CHAIN',&FF
	DW   PSEUDO+&02A9
	DB   3,%110,'REACTION',&FF
	DW   PSEUDO+&0808
	DB   2
SEQSS1: DB   0,'0',&E0,&E0,&E0,':',&FD,'START GAME',&FF
	DW   PSEUDO+&0A08
	DB   2
PDKEY:	DB   0,'1:',&FD,'DEFINE KEYS',&FF
	DW   PSEUDO+&0C08
	DB   2
PSKEY:	DB   0,'2:',&FD,'KEYBOARD',&FF
	DW   PSEUDO+&0E08
	DB   2
PJOYS:	DB   0,'3:',&FD,'KEMPSTON',&FF
	DW   PSEUDO+&1008
	DB   2
PPLUS2: DB   0,'4:',&FD,'SPECTRUM PLUS 2',&FF
	DW   PSEUDO+&1208
	DB   2 
PCURSR: DB   0,'5:',&FD,'PROTEK ',&FE,%10001,'/',&FE,0,' CURSOR',&FF
	DW   PSEUDO+&05AB
	DB   1,%100,'DURELL',&FF
	DW   PSEUDO+&14A9
	DB   0,0,'PRESS FOR ACTION',0
;
SEQS2:	DB   1
	DW   &5864
	DB   &16,20,%01000111  ; WHITE ON BLUE BRIGHT
	DB   0
SEQS3:	DB   2
	DW   PSEUDO+&0003
	DB   &18,&B9
	DB   2
	DW   PSEUDO+&00E0
	DB   &1E,&1A
	DB   3
	DW   PSEUDO+&0187
	DB   5,%10,'TOP DECONTAMINATORS',0
	DB   0
SEQS4:	DB   1
	DW   &5803
	DB   &18,&17,%01010111
	DB   1
	DW   &5820
	DB   &1E,&03,&4E
	DB   0
SEQS6:	DB   0
FORZER: DB   '0',&E0,&E0,&E0
;
; Table of characters to be entered for "Mazter-Control Mode"
;
FORTY2: 
	DB   'FORTYTWO',0
IF CHEAT
FORTY3: DW   &4044
	DB   1,%01111,'"Hi There!"',&FF
	DW   &4082
	DB   1,%10001,'What''ya want, Maz?',0
ENDIF
;
; Addresses of menu jobs - play, define keys, standard keys, kempston, +2,
; protek.
;
MENUJB: DW   STAGAM,DEFKEY,STDKEY,JOYSTK,PLUS2,CURSOR
;
; Vectors for the main character's 8-directional movement.
;
VECTAB: DB   1,1    ; Direction N
	DB   0,1    ; -NW
	DB   -1,1   ; -W
	DB   -1,0   ; -SW
	DB   -1,-1  ; -S
	DB   0,-1   ; -SE
	DB   1,-1   ; -E
	DB   1,0    ; -NE
;
; DIMENSIONS TABLE for all physical objects: (now max dec. 66)
; Format: X,Y,Z dimensions, ACTION server number.
;
; The number in brackets is the actual physical numbers for these images.
;
DIMTAB:
GMAN:	DB   4,4,24,0	; 16 dimension sets for main character (1-15)
	DB   4,4,24,0	; ie. 3 animation frames * 5 rotations (the last 3*3
	DB   4,4,24,0	; mirrored)
	DB   4,4,24,0
	DB   4,4,24,0
	DB   4,4,24,0
	DB   4,4,24,0
	DB   4,4,24,0
	DB   4,4,24,0
	DB   4,4,24,0
	DB   4,4,24,0
	DB   4,4,24,0
	DB   4,4,24,0
	DB   4,4,24,0
	DB   4,4,24,0
GROD:	DB   2,2,12,0	; Radioactive rod (16)
GTAPE:	DB   8,5,20,0	; Tape streamer (17)
GTUBES: DB   6,6,20,0	; Tubes (18)
GRBOT3: DB   5,5,12,0	; Robot mk III (19)
GCUBE:	DB   7,7,7,0	; Cube (20)
GHCUBE: DB   7,7,20,0	; Double-height cube panel thing (21)
GBIN:	DB   6,6,24,0	; The radioactive barrel (22)
GDOOR2: DB   4,3,37,5	; Vdoor (23,24,25,26,27,28)
	DB   4,4,8,0
	DB   6,6,8,0
	DB   2,4,8,0
	DB   4,2,8,0
	DB   3,3,8,0
GSACK:	DB   7,4,18,0	; Sack (29)
GTABLE: DB   7,7,18,0	; Table (30)
GTRANS: DB   7,7,8,0	; Transparant thing (31)
GHOLE1: DB   7,7,0,9	; "hole" - (32)
SPARE1: DB   7,7,4,0	; Small flat square (33)
GBGHLR: DB   0,0,0,0	; Big Hole right hand side (34)
GBGHLL: DB   0,0,0,0	; Big hole left hand side (35)
GSHOWU: DB   6,8,32,0	; Upper shower (36)
GSHOWL: DB   6,8,32,0	; Lower half of shower (37)
GRAIN:	DB   0,0,0,0	; Rain from shower (38)
GEXPLO: DB   0,0,0,0	; Explosion (39,40,41,42)
	DB   0,0,0,0
	DB   0,0,0,0
	DB   0,0,0,0
GRBOT1: DB   5,5,24,0	; Robot mk. 1 (43,44,45)
	DB   5,5,24,0
	DB   5,5,24,0
GRBOT2: DB   6,6,24,0	; Robot mk. 2 (46)
;
;	COMPUTED OBJECTS INFO: (These have no entries in the Dimensions Table)
;
;	  88 = square black hole.
;	  89 = Left hand (rising) hole
;	  90 = Right hand (falling) hole
;	  91 = Elevator Up
;	  92 = Elevator Down
;	  93 = Left-hand camera facing left
;	  94 = Left-hand camera facing right
;	  95 = Right-hand camera facing left
;	  96 = Right-hand camera facing right
;
;	REFERENCE OBJECTS:
;
	ORG  DIMTAB+(64-1)*4
;
	DB   0,0,0,0	; This is a reference object with zero dimensions.
;
;	EXTENDED OBJECTS TABLE - for special-purpose objects with no
;	graphical shape.
;	Defined in ROOMxx as &7F,X,Y,Z coordinate,X,Y,Z dimensions,ACTION.
;
	ORG  DIMTAB+(96-1)*4
;
EXTTAB: DS   &20	    ; Extend if nessecary.
;

;	Text for various purposes:
;
HERO:	DW   PSEUDO+&0600
	DB   0,%1000
	DB   '00000: ',&FD
	DB   '                ',0
;
HEROES: DB   'SQUAREJAW FETISH'
	DB   'OH MOOSE        '
	DB   'MY HOVERCRAFT IS'
	DB   'FULL OF EELS    '
	DB   'LIVETS P0LSE ER '
	DB   'SPEGET          '
	DB   'WE APOLOGISE FOR'
	DB   'THIS CLIVE      '
TEXT2:	DB   2
	DW   PSEUDO+&0304
	DB   &16,160
	DB   2
	DW   PSEUDO+&0007
	DB   &10,39
	DB   3
	DW   PSEUDO+&004B
	DB   0,%1110,'DEFINE',&FF
	DW   PSEUDO+&028C
	DB   4,%1110,'KEYS',&FF
	DW   PSEUDO+&070A
	DB   0,0,'LEFT',&FF
	DW   PSEUDO+&090A
	DB   0,0,'RIGHT',&FF
	DW   PSEUDO+&0B0A
	DB   0,0,'FORWARD',&FF
	DW   PSEUDO+&0D0A
	DB   0,0,'JETPACK',&FF
	DW   PSEUDO+&0F0A
FIRETX: DB   0,0,'FIRE',&FF
	DW   PSEUDO+&110A
	DB   0,0,'PAUSE',&FF
	DW   PSEUDO+&1409
	DB   2,%110010,'PRESS ANY KEY',0
TEXT3:	DW   0
	DB   0,%10001,'12345',0
TENTER: DB   'Enter'
TSHIFT: DB   'Shift' ; Used for both SS and CS keys. Saves hastle :-)
TSPACE: DB   'Space'
ENDTXT: DW   &4808
	DB   0,%1110,'GAME OVER',0
;
SCRTXT: DW   PSEUDO+&1600
	DB   0,%10001,'Rad',&FF
	DW   PSEUDO+&1700
	DB   0,%10001,'Jet',&FF
	DW   PSEUDO+&160C
	DB   4,%10001,'Score',&FD,'00000',&FF
	DW   PSEUDO+&170C
	DB   4,%10001,'Time ',&FD,'     ',0
;
WONTXT: DW   &4028
	DB   2,%1110,'WELL DONE',&FF
	DW   &40A9
	DB   0,%1110,'YOU HAVE',&FF
	DW   &4808
	DB   1,%1110,'COMPLETED',&FF
	DW   &4869
	DB   0,%1110,'THE GAME',&FF
	DW   &500C
	DB   0,%1110,'PRESS',&FF
	DW   &544E
	DB   0,%1110,'ANY',&FF
	DW   &50AE
	DB   0,%1110,'KEY',0
;
SCTXT:	DW   &4000
	DB   0,%10001,'00000',0
;
PATTRS: DB   WH,WH,WH,RE,RE,YE,YE,YE,YE,GR,GR,WH,WH,WH,WH,WH
	DB   WH,WH,WH,WH,WH,WH,WH
	DB   YE,YE,YE,YE,CY,CY,GR,GR,MA
	DB   WH,WH,WH,MA,MA,CY,CY,CY,CY,CY,CY,WH,WH,WH,WH,WH
	DB   WH,WH,WH,WH,WH,WH,WH
	DB   MA,MA,RE,RE,BL,BL,WH,WH,WH
;
IF ONE28 ; -----------------------------------------------------
;
SQUEUE: DB   0,3,0,4,4,16,4,4,4,16
;
ELSE	 ; -----------------------------------------------------
;
SQUEUE: DB   1,24,48,32,32,255,32,32,32,255
;
ENDIF
;
TITLE	'IMAGE8: Variables'
;
; MAIN VARIABLE BLOCK - for various uses. IMPORTANT NOTE: Some of
; the variables MUST be in a fixed order, so do NOT put any new
; in the middle - use the end.
;
; Any variable or word flagged "local" can be used by any routine as a
; scratchpad - therefore any routine can assume their contents as garbage.
; Unless, of course two routines are linked together in which case it's not
; my responsibillity (ie. initialisation before calling etc.)
;
;	WORD DEFINITIONS:
;
UPDPTR: DW   &0000  ; Address of next display file address to be updated
MIRPTR: DW   &0000  ; Address of 256 bytes to be reflected. NB: page boundary
ACTPTR: DW   &0000  ; Address of action server list
TEMPSP: DW   &0000  ; Temporary Stackpointer
TEMPIX: DW   &0000  ; Temp. IX	- during subroutine calls which might exit
TEMPIY: DW   &0000  ; Temp. IY	- suddenly and where stack in inconvenient
CURSPR: DW   &0000  ; Address of Current Sprite
XYZADR: DW   &0000  ; Address of sprite's (X,Y,Z) coordinates
TEMPHL: DW   &0000  ; Temporary HL (!) This is a local variable.
SEED:	DW   &0,&0  ; 32 bit random seed.
DYNAST: DW   &0000  ; Temporary storage in SORT.
LOFFST: DW   &0000  ; Last address of first byte in an image of a fire flame.
TAB2IN: DW   &0000  ; Indirection pointer into UNPTAB.
ATADDR: DW   &0000  ; Attribute base address.
HIWATR: DW   &0000  ; Operating System High-Water Marker for machine stack.
XTDPTR: DW   &0000  ; Pointer to extended directions table.
CURXYZ: DW   &0000  ; Address of collided object's x/y/z coords
OLDVEC: DW   &0000  ; Storage for int vector
WASXYZ: DW   &0000  ; Old xyz coords for external use.
RODADR: DW   &0000  ; Rods address in AUXTAB for currently held object
RTDPTR: DW   &0000  ; Next address in RODTAB of addresses in AUXTAB
ROBVEC: DW   &0000  ; Storage for robots old X and Y velocity vectors
HEROXY: DW   &0000  ; Main chrs x and y coordinate (updated each frame)
SCORE:	DW   &0000  ; Guess what!
CAMDF1: DW   &0000  ; Camera on screen
CAMDF2: DW   &0000  ; Urgh! THIS IS SOOOO BAAAAAAD (So BAT)
CURHER: DW   &0000  ; Current Hero!
TIMER:	DW   &0000  ; Time counter
;
;	BYTE DEFINITIONS:
;
;  VARIOUS FLAG DEFINITIONS
;  ========================
;
;  FLAGS1: Bit	When set (General-purpose local flags)
;	   ---	-------------------------------------------------------------
;	    0	Used as collision detect flag in COLISN (Local)
;	    5	Used for hidden boundary checks in HIDDEN (Local)
;
;  FLAGS2: Bit	When set (General-purpose global flags, reset on room change)
;	   ---	-------------------------------------------------------------
;	    0	Something falling through a hole (Global)
;	    1	Whole screen must be refreshed next frame (Global)
;	    2	Main Character GROL has been executed for the first time
;		without a collision (Global)
;	    3	Man has been hit by a bullet (set by interrupt)
;	    4	Robot has been hit by a bullet (set by interrupt)
;	    5	Man is exploding
;	    6	Robot is exploding
;	    7	First time DYNAMC calling cycle.
;
;  FLAGS3: Bit	When set (General-purpose golobal flags, reset on start game)
;	   ---	-------------------------------------------------------------
;	    0	Room change caused by a hole or elevator, else a door
;	    1	Room change definitely caused by an elevator, either up or down
;	    2	Radioactive rod carried.
;	    3	Under Shower.
;	    4	Object must be inserted after all refreshed have been done.
;	    5	Very start of game - raise RAD and JET
;
;  FLAGS4: Bit	When set (General-purpose global flags, reset on room change)
;	   ---	-------------------------------------------------------------
;	    0	Elevator Action (set by Elevator modules!)
;	    1	Action F active (!)
;	    2	A shower is present in the room.
;	    3	Set if there is a robot in the room
;
;  PFLAGS: Bit	When set (VDU driver flags)
;	   ---	-------------------------------------------------------------
;	    0	Use font in ROM (else RAM) (Local)
;	    1	Use Double Height (Local)
;	    2	Use Double Width (Local)
;	    3	Use Italics (Local)
;	    4	Use Boldface (Local)
;	    5	Use Shading Mode (Local)
;	    6	Use Inversion Mode (Local) (perhaps change to OUTLINE)
;	    7	Plot mode (0=XOR, 1=OR) (Local)
;
;  JETFLG: Bit	When set (Jetpack flame flags)
;	   ---	-------------------------------------------------------------
;	    0	Jetpack flame ON (Local)
;	    1	Data behind flame must be put back next frame (Global)
;	    2	Means no jetpack decrement!
;	    3	Jetpack sound hasn't been turned off yet (*1)
;	    7	Turning is now allowed even after mutual collision (Global)
;
;  ELESTA: Val	Meaning (Elevator Status)
;	   ---	-------------------------------------------------------------
;	    0	No Action
;	    1	Elevator door opening, keyboard controls disabled
;	    2	Sprite walking into elevator, keyboard disabled
;	    3	Sprite turning around in elevator, doors closing, room change
;	    4	Doors opening, Keyboard disabled
;	    5	Sprite waling out of elevator, keybard disabled
;	    6	Doors closing behind sprite, keyboard disabled.
;
;  AUXFLG  Bit	When set (global flags, never reset)
;	   ---	-------------------------------------------------------------
;	    0	Music on (else off!)
;	    1	Used in bullet interrupt and denotes "active"
;	    2	Not the main character which is falling through the hole!
;
; *1) This is logically the same as JETFLG(1), but as the flame doesn't show
;     when the man is facing out of the screen, the sound would be garbled
;     there.
;
CTRLTL: DB   &FE,&FE,&02  ; Initial control values - set to keyboard controls
CTRLTR: DB   &FE,&FE,&04
CTRLWA: DB   &FE,&BF,&08
CTRLJP: DB   &FE,&DF,&04
CTRLFI: DB   &FE,&DF,&02
CTRLPA: DB   &FE,&DF,&01
KEYDAT: DB   &FE,&FE,&02  ; Initial values for keyboard controls
	DB   &FE,&FE,&04
	DB   &FE,&BF,&08
	DB   &FE,&DF,&04
	DB   &FE,&DF,&02
	DB   &FE,&DF,&01
JOYDAT: DB   &1F,&00,&02  ; Initial Kempston joystick values
	DB   &1F,&00,&01
	DB   &1F,&00,&08
	DB   &1F,&00,&04
	DB   &1F,&00,&10
	DB   &FE,&DF,&01
PL2DAT: DB   &FE,&EF,&10  ; Spectrum Plus 2 joystick values
	DB   &FE,&EF,&08
	DB   &FE,&EF,&04
	DB   &FE,&EF,&02
	DB   &FE,&EF,&01
	DB   &FE,&DF,&01
CSRDAT: DB   &FE,&F7,&10  ; Cursor/Protek joystick values
	DB   &FE,&EF,&04
	DB   &FE,&EF,&08
	DB   &FE,&EF,&10
	DB   &FE,&EF,&01
	DB   &FE,&DF,&01
MANFLG: DB   &00    ; Flags associated to main character
NODYNA: DB   &00    ; Number of dynamic objects - global
NOSTAT: DB   &00    ; Number of static objects - global
NOBULL: DB   &00    ; Number of bullets
TRNCNT: DB   &00    ; Delay before clockwise/anticlockwise may be used
SPRCNT: DB   &00    ; Current sprite - in GROLs
OLDX:	DB   &00    ; X before move
OLDY:	DB   &00    ; Y before move
OLDZ:	DB   &00    ; Z before move
EXTX:	DB   &00    ; X+dimension (other edge of the image)
EXTY:	DB   &00    ; Y+dimension
EXTZ:	DB   &00    ; Z+dimension
CURDX:	DB   &00    ; X dimension for parameter passing
CURDY:	DB   &00    ; Y dimension for parameter passing
CURDZ:	DB   &00    ; Z dimension for parameter passing
EXTNUM: DB   &00    ; Number of 'special' defined objects
COUNTR: DB   &00    ; General purpose counter
CURLOG: DB   &00    ; Current logic object in collision
CURPHY: DB   &00    ; Current physical object in collision
CURROM: DB   &00    ; Current room
FLAGS1: DB   &00    ; Various local flags
FLAGS2: DB   &00    ; Global flags (reset every room setup)
FLAGS3: DB   &00    ; Global static flags
FLAGS4: DB   &00    ; Global flags reset on room setup
GAMMA:	DB   &00    ; 'wait' state in interrupt (mamma+)
COLOUR: DB   &00    ; Room colour
OLDCOL: DB   &00    ; Last room colour
PIXEL:	DB   &00    ; Current pixel within byte of character in print
PIXADD: DB   &00    ; Current offset from character
LAPPED: DB   &00    ; No. of objects 'in front'
SHIFT:	DB   &00    ; Shift key status
PFLAGS: DB   &00    ; VDU handler's flags
LOOPIX: DB   &00    ; Temporary pixel counter in VDU handler.
NSFONT: DB   &00    ; Normal Spacing Font Address.
INTDEL: DB   &00    ; Interrupt delay (for test purposes)
FALLRW: DB   &00    ; No. of rows to print when falling through a hole.
FACCEL: DB   &00    ; Acceleration when falling through a hole.
INPRNT: DB   &00    ; Physical object "in print"
OLDACC: DB   &00    ; Old Accelerattion.
HIDCNT: DB   &00    ; Counter in HIDDEN (local)
CURSHA: DB   &00    ; Current shading value (for text)
COUNTX: DB   &00    ; General-purpose counter
DLIMIT: DB   &00    ; The limits for walls that run diagonally on the angles
ULIMIT: DB   &00    ; Note: these are FIXED - only 45 degrees!
RLIMIT: DB   &00    ; and corresponds to the
LLIMIT: DB   &00    ; DOWN, UP, RIGHT AND LEFT diagonal wall values
XLOW:	DB   &00    ; AXIS walls for X (low)
YLOW:	DB   &00    ; For Y (low)
XHIGH:	DB   &00    ; For X (high)
YHIGH:	DB   &00    ; For Y (high)
WAIT4U: DB   &00    ; Interrupt wait flags
FUEL0:	DB   &00    ; Used for fuel scoop "delay"
FUEL:	DB   &00    ; Jetpack fuel level
UREM:	DB   &00    ; MilliRem, or Current Radiation Level.
UREM2:	DB   &00    ; This is a pseudo-wait dongle thingy thing. Don't worry.
UREM3:	DB   &00    ; This is the starting count-up value
CGROL:	DB   &00    ; Current GROL
JETFLG: DB   &00    ; Jetpack flags
CURFIR: DB   &00    ; Current fire frame
PHYSIC: DB   &00    ; Current physical object to be printed
B2REMO: DB   &00    ; Bullets to remove (in BREMO)
CFLOOR: DB   &00    ; Current floor number
SFLOOR: DB   &01    ; Start on floor this.
DORWAY: DB   &00    ; Doorway hit.
COBJCT: DB   &00    ; Current object in collision (ammended version)
COBJC2: DB   &00    ; Current door (if any) in collison test loop
INITRN: DB   &00    ; Initial direction
TMPFLR: DB   &00    ; Temporary floor actually!
TMPRM2: DB   &00    ; Temporary temporary floor.
ELECNT: DB   &00    ; How many byte to print horisontally of an elevator
ELECN2: DB   &00    ; Counts no. of steps taken when entering/exiting elevator
ELESTA: DB   &00    ; Elevator Status.
ELEVUD: DB   &00    ; 1=Going Up, 2=Going Down!
TMPDIR: DB   &00    ; Temporary Direction (used in BULLET)
MDELAY: DB   &00    ; Delay in multicolour slide
MDELOR: DB   &00    ; Original value of above.
CURGRL: DB   &00    ; Current GROL executed (useful in ACTION servers)
RSTATE: DB   &00    ; Explosion frame
NO2UPD: DB   &00    ; Number to update. Cannot use NODYNA cause of DDO's
AUXFLG: DB   &00    ; Auxillary flags
MANGRL: DB   &00    ; Man's GROL! (this is BAD)
NEWROM: DB   &00    ; Hold the new room here until needed
NORODS: DB   &00    ; No. of rods in game.
EXRODS: DB   &00    ; No. of rods thrown down in room below!
RANGLE: DB   &00    ; Similar to MANFLG, but for robots head
RDELAY: DB   &00    ; Delay from the above
SERVNO: DB   &00    ; Remember what the original acion server was!
SOURCE: DB   &00    ; Same 1 level down
MOVED:	DB   &00    ; Moved never mind
BCKRAD: DB   &00    ; Background radiation delay (original)
BCKDEL: DB   &00    ; Background radiation delay counter
LISTNO: DB   &00    ; Current selected list item
CLIM:	DB   &00    ; Character Input Buffer Limit (used in KINPUT), buffersize
BULDIM: DB   &00    ; Bullet direction if a bullet hit the man
BULDIR: DB   &00    ; Bullet direction if a bullet hit a robot (these are diff)
ENERGY: DB   &00    ; Robot's energy level
UB0VAL: DB   &00    ; User Byte 0 value (for patching!)
LOOP:	DB   &00    ; Just a dummy incremented every time main loop goes round
DRPDEL: DB   &00    ; Drop delay
MUSOC1: DB   &00    ; Music routine occupied!
PLAY1:	DB   &00    ; Playing sound @ moment...
MUSOC2: DB   &00    ; same for Chan 2
PLAY2:	DB   &00    ; same for Chan 2
TIMER1: DB   &00    ; 20ms timer
RODCOL: DB   &00    ; Origin colour of the rod just picked up.
HOLDZ:	DB   &00    ; Temp storage
ZLEVEL: DB   &00    ; Temp storage
BABIES: DB   &00    ; No. of babies!
;
;	TABLE DEFINITIONS:
;
; SPRITE TABLE - Table containing all information about dynamic object
; currently displayed on-screen.
; The table contents are as follows: each entry is &10 bytes long, and
; corresponds to the object of that number. The contents of the entry
; are:
;
;  1. bit 0-2: No. of objects in Animation Sequence Table
;     bit 5-7: Current mirror state of the actual images (no. to multiply with)
;  2. Current index in Animation Sequence Table
;  3. Animation Sequence Table, holding the sequence of images to be displayed
;  4. X,Y and Z velocity vectors (addressed by IX upon entrering a GROL)
;  5. Address of the Image and x,y and z coordinates in the CURENT room table
;  6. Gravity flags (always addressed by IY)
;  7. User flags
;  8. GROL server used
;
; The actual positions depend on the length of the Animation Sequence Table.
; The GROL server is on the last byte of the entry.
; The ACTN server used is in a different table, due to the fact that also
; static objects can have an action server attached. (ACTTAB)
; The User Bytes are in Chain Reaction used like this:-
;	
;    UB0 Bit When set
;	 --- ----------------------------------------------------------------
;	  1  Dormant Dynamic Object
;	  2  DDO must be cleared off screen next time around.
;	  3  DDO has been cleared off screen.
;	  4  Object is falling through a hole
;	  5  Origin was Gravity module (else GROL module)
;	  6  "Whatever"-flag. This is always garbage, just for the hell of it.
;	  7  Object is "In Mid Air"
;
; UPDATE ADDRESSES TABLE - Table of addresses in (pseudo) display file where
; the top right-hand corner of a sprite has been printed. Used by both PLOT and
; unplot.
;
; ACTION SERVER TABLE - one for each object, static or dynamic, in the current
; room. Analogus with CURENT.
;
; EXTDIR holds the values of directions to use when exiting rooms through
; Extended Objects. Applies mostly to doors.
;
; CURENT (Current Objects-in-the-universe List) holds all objects present
; in the room in the table as follows, 4 bytes each entry:-
;
;     0. Image number (index into DIMTAB to find it's dimensions)
;     1. X coord
;     2. Y coord
;     3. Z coord
;
; An image number of nn+&80 indicates "current" object in the linked lists,
; and a value of &FF is aN invalid, or "dead" object, and should be ignored.
;
; It seems that Chain Reaction can only handle eight sprites!
;
RODPOS: DS   &03
SPRTAB: DS   &80    ; Table of sprite data, suitable for 8 sprites. 
FIRBUF: DS   &40    ; What's behind the fire breethe! This is a graphics buffer
UPDTAB: DS   &24    ; Table of screen update addresses, the left upper corner
ACTTAB: DS   &80    ; Table of Action Servers
EXTDIR: DS   &10    ; Which direction to point to when exiting a room.
MANINF: DS   &10    ; Main Character Sprite Information.
GRCTAB: DS   &10    ; Gravity Collision Table - containing LOGICAL object nos.
LAPTAB: DS   &10    ; Table of lapped objects.
BULTAB: DS   &80    ; Bullet info (&1F entries).
UNPTAB: DS   &80    ; Table of bullets to delete from screen.
XLIMIT: DS   &08    ; Extended wall limits.
ROBOTP: DS   &13    ; Robot presence flags. Each bit is a room.
RODTAB: DS   &10    ; Table pointing to Z coords of rods dropped through holes
FARVER: DB   &00
CURENT: DS   &FF    ; Cur(r)ent room data : IMAGE,X,Y,Z
	DS   &30
;
TITLE	'IMAGE9: Room Data - Boring stuff, only there to please the public!'
;
RADTAB: DW   ROOM01,ROOM02,ROOM03,ROOM04,ROOM05,ROOM06,ROOM07,ROOM08,ROOM09
	DW   ROOM10,ROOM11,ROOM12,ROOM13,ROOM14,ROOM15,ROOM16,ROOM17,ROOM18
	DW   ROOM19,ROOM20,ROOM21
;
ROOM01: DB   &20,&7C,&3B,&3B
	DB   &00,&00,&4F,&4F

	DB   1,64,45,129,99,255       ; BIG MIDDLE
	DB   4,0,45,64,32,255	      ; TOPLEFT OF MIDDLE
	DB   2,194,45,64,32,255       ; TOPRIGHT OF MIDDLE
	DB   1,0,77,64,35,255	      ; LEFT OF MIDDLE
	DB   1,191,77,64,35,255       ; RIGHT OF MIDDLE
	DB   3,0,112,64,32,255	      ; BOTLEFT OF MIDDLE
	DB   5,191,112,64,32,255      ; BOTRIGHT OF MIDDLE
	DB   5,0,43,64,32,85	      ; BOTTOM OF LEFT WALL
	DB   4,61,0,1,32,170	      ; TOP OF LEFT WALL
	DB   1,65,0,127,44,170	      ; MIDDLE WALL
	DB   3,194,43,61,31,255       ; BOTTOM OF RIGHT WALL
	DB   2,194,0,60,31,255	      ; TOP OF RIGHT WALL
	DB   1,0,32,63,11,170	      ; MIDDLE OF LEFT WALL
	DB   1,194,31,61,12,255       ; MIDDLE OF RIGHT WALL

	DB   0

	DB   93,46,72,15	      ; Some cameras
	DB   96,76,42,19

	DB   &85,69,59,0	      ; Some doors
	DB   &88,80,26,0
	DB   &87,26,80,0
	DB   &86,67,5,0
	DB   &84,5,67,0

	DB   34,34,42,0 	      ; Big hole
	DB   35,42,34,0

	DB   &8A,29,05,0	      ; More doors
	DB   &81,0,40,0
	DB   &82,40,0,0
	DB   &83,21,11,0

	DB   127,66,64,0,0,0,32,2+&80,0  ; Doors ExtObjs
	DB   127,17,15,0,0,0,32,3+&80,4
	DB   127,10,72,0,0,0,32,4+&80,2
	DB   127,72,10,0,0,0,32,5+&80,6
	DB   127,33,82,0,0,0,32,6+&80,1
	DB   127,83,33,0,0,0,32,7+&80,7
	DB   127,00,47,0,0,0,32,8+&80,3
	DB   127,47,00,0,0,0,32,9+&80,5

	DB   127,38,38,0,16,16,0,&C,0	 ; Big hole!

	DB   0				 ; No dynamic objs
	DB   0
;
ROOM02: DB   &20,&7C,&20,&20
	DB   &00,&00,83,83

	DB   1,64,0,128,44,170	      ; BACK WALL
	DB   1,64,45,128,99,255       ; MID FLOOR
	DB   4,55,0,8,4,170	      ; RIGHT TOP
	DB   1,55,4,8,40,170	      ; RIGHT MID
	DB   5,55,43,8,4,85	      ; RIGHT BOT
	DB   4,55,45,8,4,255
	DB   1,55,49,8,91,255
	DB   3,55,140,8,4,255
	DB   2,193,0,8,4,255
	DB   1,193,4,8,40,255
	DB   3,193,43,8,4,255
	DB   2,193,45,8,4,255
	DB   1,193,49,8,91,255
	DB   5,193,140,8,4,255

	DB   0

	DB   5+&80,69,59,0

	DB   3+&80,21,11,0

	DB   127,17,15,0,0,0,32,1+&80,4
	DB   127,66,64,0,0,0,32,11+&80,0

	DB   0
	DB   3,46,46,46,5,&23,0,1,1,0,1+&80,2
	DB   0

ROOM03: DB   &20,&7C,&1D,&1D
	DB   &00,&00,83,83

	DB   1,64,0,128,44,170	      ; BACK WALL
	DB   1,64,45,128,99,255       ; MID FLOOR
	DB   4,55,0,8,4,170	      ; RIGHT TOP
	DB   1,55,4,8,40,170	      ; RIGHT MID
	DB   5,55,43,8,4,85	      ; RIGHT BOT
	DB   4,55,45,8,4,255
	DB   1,55,49,8,91,255
	DB   3,55,140,8,4,255
	DB   2,193,0,8,4,255
	DB   1,193,4,8,40,255
	DB   3,193,43,8,4,255
	DB   2,193,45,8,4,255
	DB   1,193,49,8,91,255
	DB   5,193,140,8,4,255

	DB   0

	DB   5+&80,69,59,0

	DB   22,&48,&2C,0
	DB   &8A,&1F,3,0

	DB   3+&80,21,11,0

	DB   127,66,64,0,0,0,32,1+&80,0
	DB   127,17,15,0,0,0,32,10+&80,4

	DB   0
	DB   3,46,46,46,5+20,&20+20,0,0,1,0,1+&80,2
	DB   0

ROOM04: DB   &20,&5C,&30,&30
	DB   &00,&00,83,83

	DB   1,32,32,192,44,170
	DB   1,32,77,192,67,255
	DB   0

	DB   5+&80,43,53,0

	DB   29,&18,&42,0
	DB   29,&18,&3C,0

	DB   6+&80,51,5,0
	DB   4+&80,5,51,0

	DB   3+&80,31,1,0

	DB   127,55,9,0,0,0,32,1+&80,6
	DB   127,39,57,0,0,0,32,6+&80,0
	DB   127,27,5,0,0,0,32,8+&80,4
	DB   127,9,55,0,0,0,32,12+&80,2

	DB   0
	DB   3,43,44,45,0,&24,0,0,1,0,1+&80,2
	DB   0

ROOM05: DB   &20,&5C,&30,&30
	DB   &00,&00,83,83

	DB   1,32,32,192,44,170
	DB   1,32,77,192,67,255
	DB   0

	DB   5+&80,43,53,0

	DB   6+&80,51,5,0
	DB   4+&80,5,51,0

	DB   3+&80,31,1,0

	DB   127,9,55,0,0,0,32,1+&80,2
	DB   127,39,57,0,0,0,32,7+&80,0
	DB   127,27,5,0,0,0,32,9+&80,4
	DB   127,55,9,0,0,0,32,13+&80,6

	DB   0
	DB   3,43,44,45,&12,&23,0,0,1,0,1+&80,2
	DB   0

ROOM06: DB   &10,&7E,&3B,&3B
	DB   &00,&00,&48,&35

	DB   2,170,0,84,42,255
	DB   3,171,42,83,42,255
	DB   4,86,0,84,42,170
	DB   7,82,42,84,36,170
	DB   5,12,78,82,42,170
	DB   2,169,43,86,42,255
	DB   4,86,44,85,43,255
	DB   1,84,85,170,28,255
	DB   4,12,81,85,41,255
	DB   3,12,121,88,44,255
	DB   1,83,86,54,62,255
	DB   5,138,112,116,53,255
	DB   1,100,132,40,33,255

	DB   0

	DB   9+&80,&3A,&26,0	   ; TOWER OF POWER

	DB   9+&80,&3A-24,&26,0

	DB   2+&80,40,0,0
	DB   3+&80,11,5,0

	DB   21,18,&30,0
	DB   17,10,&32,0
	DB   17,2,&32,0

	DB   127,47,0,0,0,0,32,1+&80,5
	DB   127,7,9,0,0,0,32,4+&80,4

	DB   0
	DB   3,43,44,45,&1C,&30,0,0,1,0,1+&80,2
	DB   1,33,34,28,0,0,0,0,0+&80,&8
	DB   0

ROOM07: DB   0,&7C,&3B,&3B
	DB   &00,&00,&33,&48

	DB   4,2,0,84,42,170
	DB   5,0,42,84,42,170
	DB   4,2,45,84,42,255
	DB   1,0,86,116,28,255
	DB   3,0,114,116,53,255
	DB   2,86,0,84,43,255
	DB   6,86,43,87,34,255
	DB   2,84,44,154,77,255
	DB   1,116,76,30,91,255
	DB   3,154,77,87,44,255
	DB   5,146,121,95,46,255

	DB   0

	DB   93,&2F,&40,18

	DB   &8D,40,40,0

	DB   1+&80,0,40,0
	DB   3+&80,11,5,0		

	DB   127,0,47,0,0,0,32,1+&80,3
	DB   127,7,9,0,0,0,32,5+&80,4

	DB   127,46,41,0,16,6,0,&E,0

	DB   0		 
	DB   3,43,44,45,&13,&44,0,0,1,0,1+&80,2
	DB   1,38,46,40,20,0,0,0,0+&80,0
	DB   0

ROOM08: DB   0,&80,&2B,&3B
	DB   &13,&07,&40,&4D

	DB   4,0,0,64,32,170
	DB   1,0,32,66,10,170
	DB   5,0,42,64,32,170
	DB   1,68,0,64,42,170
	DB   2,134,0,83,42,255
	DB   3,134,42,83,42,255
	DB   4,0,44,64,32,255
	DB   3,0,76,152,76,255
	DB   1,66,43,67,33,255
	DB   2,134,44,84,42,255
	DB   5,153,119,64,32,255
	DB   1,153,86,64,33,255

	DB   0
	DB   8+&80,65,36,0
	DB   5+&80,61,67,0

	DB   &8A,&29,&09,&00

	DB   127,68,43,0,0,0,32,1+&80,7
	DB   127,57,71,0,0,0,32,4+&80,0
	DB   0
	DB   3,43,44,45,&17,&43,0,0,2,0,1+&80,2
	DB   0

ROOM09: DB   0,&80,&3B,&3B
	DB   &00,&1E,&44,&4B

	DB   4,2,0,84,42,170
	DB   5,0,42,84,42,170
	DB   1,86,0,62,42,170
	DB   4,3,44,84,42,255
	DB   1,0,86,64,32,255
	DB   3,0,118,64,32,255
	DB   1,85,43,64,33,255
	DB   5,64,76,146,73,255
	DB   2,150,0,64,32,255
	DB   1,150,32,64,10,255
	DB   3,150,42,64,32,255
	DB   2,150,44,64,32,255

	DB   0

	DB   7+&80,26,75,0
	DB   5+&80,65,63,0

	DB   127,33,76,0,0,0,32,1+&80,1
	DB   127,62,68,0,0,0,32,5+&80,0
	DB   0
	DB   3,43,44,45,&00,&21,0,0,1,0,1+&80,2
	DB   0

ROOM10: DB   &3A,&80,&3B,&3B
	DB   &00,&00,&4D,&4D

	DB   4,0,0,64,32,170
	DB   1,0,32,66,10,170
	DB   5,0,42,64,32,170
	DB   4,0,44,64,32,255
	DB   1,68,0,121,42,170
	DB   2,191,0,64,32,255
	DB   1,191,32,64,10,255
	DB   3,191,42,64,32,255
	DB   1,66,43,124,33,255
	DB   2,191,44,64,32,255
	DB   1,0,76,255,42,255

	DB   0
	DB   5+&80,69,59,0
	DB   4+&80,5,67,0
	DB   6+&80,67,5,0
	DB   30,&28,&42,0
	DB   30,&20,&42,0
	DB   30,&18,&42,0

	DB   127,67,65,0,0,0,32,3+&80,0
	DB   127,10,72,0,0,0,32,16+&80,2
	DB   127,73,11,0,0,0,32,17+&80,6

	DB   0
	DB   3,43,44,45,&2A,&22,0,0,1,0,1+&80,2
	DB   0

ROOM11: DB   &20,&80,&3B,&3B
	DB   &00,&00,&60,&60

	DB   1,0,0,255,42,170
	DB   1,0,43,255,69,255
	DB   3,0,112,64,32,255
	DB   5,191,112,64,32,255
	DB   1,64,112,128,32,255

	DB   0
	DB   3+&80,21,11,0
	DB   6+&80,67,5,0
	DB   4+&80,5,67,0

	DB   &8A,&31,1,0
	DB   &8A,&21,1,0

	DB   &8B,53,75,0
	DB   &8C,85,43,0

	DB   127,17,15,0,0,0,32,2+&80,4
	DB   127,72,10,0,0,0,32,14+&80,6
	DB   127,10,72,0,0,0,32,15+&80,2

	DB   127,50,80,0,0,0,32,10,0
	DB   127,82,48,0,0,0,32,11,0

	DB   0
	DB   3,43,44,45,&0D,&33,0,1,0,0,1+&80,2
	DB   0

ROOM12: DB   0,&80,&1E,&1C
	DB   &00,&0B,&3D,83

	DB   1,63,0,63,42,170
	DB   1,63,43,64,112,255
	DB   2,128,0,64,32,255
	DB   1,128,32,64,10,255
	DB   3,128,42,64,32,255
	DB   2,128,44,64,32,255
	DB   1,128,76,64,48,255
	DB   5,128,123,64,32,255

	DB   0
	DB   6+&80,47,17,0
	DB   3+&80,9,15,0
	DB   5+&80,61,67,0

	DB   127,51,21,0,0,0,32,4+&80,6
	DB   127,7,18,0,0,0,32,18+&80,4
	DB   127,58,72,0,0,0,32,20+&80,0
	DB   0
	DB   3,43,44,45,&32,&2A,0,1,1,0,1+&80,2
	DB   0

ROOM13: DB   &15,&80,&0F,&28
	DB   &05,&00,83,&43

	DB   4,38,0,64,32,170
	DB   1,40,32,64,10,170
	DB   5,40,42,64,32,170
	DB   4,40,44,64,32,255
	DB   1,40,76,64,48,255
	DB   3,40,123,64,32,255
	DB   1,106,0,64,42,170
	DB   1,105,43,65,112,255

	DB   0
	DB   18,&1A,&22,0
	DB   4+&80,7,49,0
	DB   5+&80,71,57,0
	DB   3+&80,19,5,0
	DB   &8A,&07,&23,0

	DB   127,11,53,0,0,0,32,5+&80,2
	DB   127,67,61,0,0,0,32,21+&80,0
	DB   127,15,9,0,0,0,32,19+&80,4

	DB   0
	DB   3,43,44,45,&2B,&32,0,0,1,0,1+&80,2
	DB   0

ROOM14: DB   &00,&80,&3B,&2B
	DB   &0B,&1B,&46,&60
	
	DB   1,32,0,126,42,170
	DB   1,32,43,64,64,255
	DB   3,32,107,64,32,255
	DB   1,96,43,64,32,255
	DB   5,96,75,128,64,255
	DB   2,160,0,64,32,255
	DB   1,160,32,64,10,255
	DB   3,160,42,64,32,255
	DB   2,160,44,64,32,255

	DB   0

	DB   &8A,&0D,&2D,0

	DB   4+&80,21,67,0
	DB   2+&80,40,26,0

	DB   127,26,72,0,0,0,32,11+&80,2
	DB   127,47,26,0,0,0,32,21+&80,5
	DB   0

	DB   3,43,44,45,&41,&36,0,255,0,0,1+&80,2

	DB   0

ROOM15: DB   0,&80,&2B,&2B
	DB   &1A,&0B,&60,&46

	DB   4,29,0,64,32,170
	DB   1,31,32,64,10,170
	DB   5,31,42,64,32,170
	DB   4,31,44,64,32,255
	DB   3,31,76,128,64,255
	DB   1,97,0,127,42,170
	DB   1,97,43,127,35,255
	DB   1,160,75,64,32,255
	DB   5,160,107,64,32,255

	DB   0
	DB   6+&80,59,13,0
	DB   1+&80,26,34,0
	DB   &8A,&2F,&0B,0
	DB   &8A,&1F,&0B,0

	DB   127,65,19,0,0,0,32,11+&80,6
	DB   127,25,41,0,0,0,32,20+&80,3
	DB   0
	DB   3,43,44,45,&32,&32,0,1,0,0,1+&80,2
	DB   0

ROOM16: DB   &24,&80,&2D,&3B
	DB   &0A,&00,&46,&37

	DB   4,76,1,84,42,170
	DB   7,73,43,84,22,170
	DB   5,29,65,84,42,170
	DB   4,31,44,128,64,255
	DB   3,31,108,64,32,255
	DB   1,95,106,129,34,255
	DB   2,160,0,64,32,255
	DB   1,160,32,64,10,255
	DB   3,160,42,64,32,255
	DB   2,160,44,64,32,255
	DB   1,160,76,64,32,255

	DB   0
	DB   6+&80,51,5,0
	DB   7+&80,34,56,0
	DB   22,&45,&33,0
	DB   22,&45,&2C,0
	DB   22,&45,&2F,26

	DB   127,56,10,0,0,0,32,10+&80,6
	DB   127,41,59,0,0,0,32,18+&80,1
	DB   0
	DB   3,43,44,45,&12,&33,0,0,1,0,1+&80,2
	DB   0

ROOM17: DB   &24,&80,&3B,&2C
	DB   &00,&0A,&37,&48

	DB   4,030,000,064,32,170
	DB   1,032,032,064,10,170
	DB   5,032,042,064,32,170
	DB   2,098,000,084,42,255
	DB   6,098,042,085,22,255
	DB   3,142,064,084,42,255
	DB   4,032,044,064,32,255
	DB   1,032,076,065,32,255
	DB   1,032,108,128,32,255
	DB   5,160,108,064,32,255
	DB   2,098,044,128,64,255

	DB   0
	DB   &8D,&2C,&35,&00
	DB   4+&80,5,51,0
	DB   8+&80,56,32,0

	DB   127,10,56,0,0,0,32,10+&80,2
	DB   127,59,39,0,0,0,32,19+&80,7
	DB   127,&32,&36,0,16,6,0,&E,0


	DB   0
	DB   3,43,44,45,&1C,&14,0,0,1,0,1+&80,2
	DB   1,38,&32,&35,20,0,0,0,0+&80,0
	DB   0

ROOM18: DB   0,&80,&3B,&2D
	DB   &0B,&1B,&46,&60
	
	DB   1,32,0,126,42,170
	DB   1,32,43,64,64,255
	DB   3,32,107,64,32,255
	DB   1,96,43,64,32,255
	DB   5,96,75,128,64,255
	DB   2,160,0,64,32,255
	DB   1,160,32,64,10,255
	DB   3,160,42,64,32,255
	DB   2,160,44,64,32,255

	DB   0
	DB   5+&80,61,67,0
	DB   2+&80,39,26,0
	DB   18,&0C,&36,0
	DB   18,&0C,&2E,0
	DB   18,&0C,&26,0
	DB   31,&0C,&1D,0
	DB   31,&0C,&1D,12
	DB   31,&0C,&1D,24

	DB   127,57,71,0,0,0,32,12+&80,0
	DB   127,46,26,0,0,0,32,16+&80,5
	DB   0
	DB   3,43,44,45,&28,&52,0,0,255,0,1+&80,2
	DB   0

ROOM19: DB   0,&80,&2B,&2B
	DB   &1A,&0B,83,&48
	
	DB   4,29,0,64,32,170
	DB   1,31,32,64,10,170
	DB   5,31,42,64,32,170
	DB   4,31,44,64,32,255
	DB   3,31,76,128,64,255
	DB   1,97,0,127,42,170
	DB   1,97,43,127,35,255
	DB   1,160,75,64,32,255
	DB   5,160,107,64,32,255

	DB   0
	DB   29,&36,&45,0
	DB   29,&2E,&45,0
	DB   29,&2E,&3E,0
	DB   29,&20,&45,0
	DB   29,&23,&3E,0
	DB   5+&80,77,51,0
	DB   1+&80,26,34,0
	DB   18,&28,12,0
	DB   18,&20,12,0

	DB   127,74,56,0,0,0,32,13+&80,0
	DB   127,26,41,0,0,0,32,17+&80,3

	DB   0
	DB   3,43,44,45,&32,&22,0,0,1,0,1+&80,2
	DB   0

ROOM20: DB   &25,&80,&2A,&2B
	DB   &00,&0A,&39,&45
	
	DB   4,30,0,64,32,170
	DB   1,32,32,64,10,170
	DB   5,32,42,64,32,170
	DB   2,98,0,84,42,255
	DB   6,98,42,85,22,255
	DB   3,142,64,84,42,255
	DB   4,32,44,64,32,255
	DB   1,32,76,65,32,255
	DB   1,32,108,128,32,255
	DB   5,160,108,64,32,255
	DB   2,98,44,128,64,255

	DB   0
	DB   &8D,&2C,&39,1
	DB   3+&80,15,25,0
	DB   8+&80,56,32,0
	DB   &8A,1,&29,0

	DB   127,11,29,0,0,0,32,12+&80,4
	DB   127,57,39,0,0,0,32,15+&80,7
	DB   127,&32,&3A,0,16,6,0,&E,0

	DB   0
	DB   3,43,44,45,&2A,&31,0,0,1,0,1+&80,2
	DB   1,38,&32,&39,20,0,0,0,0+&80,0
	DB   0

ROOM21: DB   &24,&80,&2E,&3B
	DB   &0B,&00,&46,&37
	
	DB   4,77,1,84,42,170
	DB   7,74,43,84,22,170
	DB   5,30,65,84,42,170
	DB   4,32,44,128,64,255
	DB   3,32,108,64,32,255
	DB   1,96,106,129,34,255
	DB   2,161,0,64,32,255
	DB   1,161,32,64,10,255
	DB   3,161,42,64,32,255
	DB   2,161,44,64,32,255
	DB   1,161,76,64,32,255

	DB   0
	DB   7+&80,30,56,0
	DB   3+&80,31,9,0

	DB   127,37,59,0,0,0,32,14+&80,1
	DB   127,27,13,0,0,0,32,13+&80,4
	DB   0

	DB   3,43,44,45,&43,&31,0, 0,1,0, 1+&80,2

	DB   0
;
TITLE	'C H A I N   R E A C T I O N'
;
	END
