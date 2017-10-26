.include "m2560def.inc"
.def row = r16              ; current row number
.def col = r17              ; current column number
.def rmask = r18            ; mask for current row during scan
.def cmask = r19            ; mask for current column during scan
.def temp1 = r20 
.def temp2 = r21
.def flag=r22
.def display1=r24
.def STN = r25
.def temp3 =r23
.def input = r30			;change speed
.def SC = r31				
.equ PORTLDIR = 0xF0        ; PH7-4: output, PH3-0, input
.equ INITCOLMASK = 0xEF     ; scan from the rightmost column,
.equ INITROWMASK = 0x01     ; scan from the top row
.equ ROWMASK = 0x0F         ; for obtaining input from Port L
.equ PATTERN = 0x00000011

.macro clear
ldi YL, low(@0) ; load the memory address to Y pointer
ldi YH, high(@0)
clr temp3 ; set temp to 0
st Y+, temp3 ; clear the two bytes at @0 in SRAM
st Y, temp3
.endmacro
.macro clearX
ldi XL, low(@0) ; load the memory address to Y pointer
ldi XH, high(@0)
clr temp3 ; set temp to 0
st X+, temp3 ; clear the two bytes at @0 in SRAM
st X, temp3
.endmacro
.macro do_lcd_command
	ldi r16, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro
.macro do_lcd_data
	mov r16, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro
.macro do_lcd_letters
	ldi r16, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro
.macro do_lcd_char
	ldi r16, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.dseg
DebounceCounter:	.byte 2              
TC:					.byte 2		; Two-byte counter for counting seconds.   
TTime:				.byte 10	; travel time
Zer:				.byte 1
LEDCounter:			.byte 2

.cseg

.org 0x0000
	jmp RESET
.org INT0addr
	jmp PB
.org INT1addr
	jmp PB
.org OVF0addr
	jmp Timer0OVF	; Jump to the interrupt handler for
	jmp DEFAULT		; default service for all other interrupts.

DEFAULT: reti		; no service
					; continued
jmp reset

RESET:
	clear Ttime		;??Ttime?????????
	ldi XL, low(Ttime)
	ldi XH, high(Ttime)
    ldi temp1, low(RAMEND)  ; initialize the stack
    out SPL, temp1
    ldi temp1, high(RAMEND)
    out SPH, temp1
	
	ldi temp1, PORTLDIR     ; PB7:4/PB3:0, out/in
    sts DDRL, temp1         ; PORTB is input
	ser r16
	out DDRF, r16
	out DDRA, r16
	out DDRC, r16
	clr r16
	out PORTF, r16
	out PORTA, r16
	out PORTC, r16

	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_5ms
	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_1ms
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00001000 ; display off?
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001110 ; Cursor on, bar, no blink

	do_lcd_letters 'M'
	do_lcd_letters 'a'
	do_lcd_letters 'x'
	do_lcd_letters 'i'
	do_lcd_letters 'm'
	do_lcd_letters 'u'
	do_lcd_letters 'm'
	do_lcd_letters ' '
	do_lcd_letters 'n'
	do_lcd_letters 'u'
	do_lcd_letters 'm'
	do_lcd_letters 'b'
	do_lcd_letters 'e'
	do_lcd_letters 'r'
	do_lcd_command 0b10101000
	do_lcd_letters 'o'
	do_lcd_letters 'f'
	do_lcd_letters ' '
	do_lcd_letters 's'
	do_lcd_letters 't'
	do_lcd_letters 'a'
	do_lcd_letters 't'
	do_lcd_letters 'i'
	do_lcd_letters 'o'
	do_lcd_letters 'n'
	do_lcd_letters 's'
	do_lcd_letters ':'
	ldi temp3, 48
	clr display1
	ldi flag,  30

	rjmp read ; jump to main program

read:
    ldi cmask, INITCOLMASK  ; initial column mask
    clr col                 ; initial column
colloop:
    cpi col, 4
    breq read               ; If all keys are scanned, repeat.
    sts PORTL, cmask        ; Otherwise, scan a column.
    ldi temp1, 0xFF         ; Slow down the scan operation.
delay:
    dec temp1
    brne delay              ; until temp1 is zero? - delay
    lds temp1, PINL          ; Read PORTL
    andi temp1, ROWMASK     ; Get the keypad output value
    cpi temp1, 0xF          ; Check if any row is low
    breq nextcol            ; if not - switch to next column
                            ; If yes, find which row is low
    ldi rmask, INITROWMASK  ; initialize for row check
    clr row
rowloop:
    cpi row, 4              ; is row already 4?
    breq nextcol            ; the row scan is over - next column
    mov temp2, temp1
    and temp2, rmask        ; check un-masked bit
    breq convert            ; if bit is clear, the key is pressed
    inc row                 ; else move to the next row
    lsl rmask
    jmp rowloop
nextcol:                    ; if row scan is over
     lsl cmask
     inc col                ; increase col value
     jmp colloop            ; go to the next column
convert:
    cpi col, 3              ; If the pressed key is in col 3
    breq letters            ; we have letter
	jmp NT1
	letters:
    cpi row, 1
	breq next1
	cpi row, 2
	breq spac1
	jmp NameFun
	spac1:
	do_lcd_letters ' '
	jmp debounce
	next1:
	ldi display1, 65
	do_lcd_data display1 ; Cursor on, bar, no blink
	jmp debounce
    
	
	NT1:                    ; If the key is not in col 3 and
    cpi row, 3              ; if the key is in row 3,
    breq symbols            ; we have a symbol or 0
	jmp NT2
	symbols:
		cpi col, 0              ; Check if we have a star
		breq star
		jmp NT3
		star:
		ldi temp1, 42          ; set to star
		jmp convert_end
		NT3:
		cpi col, 1              ; or if we have zero
		breq zero
		jmp NT4
		zero:
		ldi temp1, 0
		subi temp1, -'0' 
		jmp convert_end
		NT4:
		ldi temp1, 35          ; if not we have hash
		jmp convert_end
	NT2:
    mov temp1, row          ; otherwise we have a number 1-9
    lsl temp1
    add temp1, row
    add temp1, col          ; temp1 = row*3 + col

	cpi display1, 0
	breq mov1
	cpi temp1, 0
	breq Tletters1    ;put A-I 1
	cpi temp1, 1
	breq Tletters2		;put J-R 2
	cpi temp1, 2        ;put S-Z 3
	breq Tletters3
	jmp ERM
	mov1:
	mov display1, temp1		; mov temp1 to dispaly1
	mov STN, temp1
	subi STN, -48

	subi temp1, -'1'
    jmp convert_end
Tletters1:
	cpi display1, 65
	brlt apply1
	inc display1
	do_lcd_command 0b00010000 ; keep cursor
	do_lcd_data display1
	jmp debounce
	apply1:
	ldi display1, 65
	do_lcd_data display1
	jmp debounce
Tletters2:
	cpi display1, 74
	brlt apply2
	inc display1
	do_lcd_command 0b00010000 ; keep cursor
	do_lcd_data display1
	jmp debounce
	apply2:
	do_lcd_command 0b00010000
	ldi display1, 74
	do_lcd_data display1
	jmp debounce
Tletters3:
	cpi display1, 83
	brlt apply3
	inc display1
	do_lcd_command 0b00010000 ; keep cursor
	do_lcd_data display1
	jmp debounce
	apply3:
	do_lcd_command 0b00010000
	ldi display1, 83
	do_lcd_data display1
	jmp debounce
convert_end:
	do_lcd_data temp1
	jmp debounce
debounce:
	rcall sleep_5ms
	dec flag
	brne debounce
	jmp read                ; restart the main loop

NameFun:
	cp STN,temp3
	brlo JPT
	jmp NT7
	JPT:
		ldi temp3, 48
		jmp TimeFun
	NT7:
	inc temp3
	do_lcd_command 0b00000001 ; clear display
	do_lcd_letters 'N'
	do_lcd_letters 'a'
	do_lcd_letters 'm'
	do_lcd_letters 'e'
	do_lcd_letters ' '
	do_lcd_letters 'o'
	do_lcd_letters 'f'
	do_lcd_letters ' '
	do_lcd_letters 's'
	do_lcd_letters 't'
	do_lcd_letters 'a'
	do_lcd_letters 't'
	do_lcd_letters 'i'
	do_lcd_letters 'o'
	do_lcd_letters 'n'
	do_lcd_data temp3

debounce1:
	rcall sleep_5ms
	dec flag
	brne debounce1
      
	do_lcd_command 0b10101000
	do_lcd_letters ':'
	rjmp read

ERM:
	do_lcd_command 0b00000001 ; clear display
	do_lcd_letters 'I'
	do_lcd_letters 'n'
	do_lcd_letters 'c'
	do_lcd_letters 'o'
	do_lcd_letters 'r'
	do_lcd_letters 'r'
	do_lcd_letters 'e'
	do_lcd_letters 'c'
	do_lcd_letters 't'
	debounce8:
	rcall sleep_5ms
	dec flag
	brne debounce8
	dec temp3
	rjmp NameFun
;=======================================================================================================PART B============================================


 ;input, display1, SC


readtime:
    ldi cmask, INITCOLMASK  ; initial column mask
    clr col                 ; initial column
colloop1:
    cpi col, 4
    breq readtime               ; If all keys are scanned, repeat.
    sts PORTL, cmask        ; Otherwise, scan a column.
    ldi temp1, 0xFF         ; Slow down the scan operation.
delay1:
    dec temp1
    brne delay1              ; until temp1 is zero? - delay
    lds temp1, PINL          ; Read PORTL
    andi temp1, ROWMASK     ; Get the keypad output value
    cpi temp1, 0xF          ; Check if any row is low
    breq nextcol1            ; if not - switch to next column
                            ; If yes, find which row is low
    ldi rmask, INITROWMASK  ; initialize for row check
    clr row
rowloop1:
    cpi row, 4              ; is row already 4?
    breq nextcol1            ; the row scan is over - next column
    mov temp2, temp1
    and temp2, rmask        ; check un-masked bit
    breq convert1            ; if bit is clear, the key is pressed
    inc row                 ; else move to the next row
    lsl rmask
    jmp rowloop1
nextcol1:                    ; if row scan is over
     lsl cmask
     inc col                ; increase col value
     jmp colloop1            ; go to the next column
convert1:
    cpi col, 3              ; If the pressed key is in col 3
    breq letters1            ; we have letter
                            ; If the key is not in col 3 and
    cpi row, 3              ; if the key is in row 3,
    breq symbols1            ; we have a symbol or 0
    mov temp1, row          ; otherwise we have a number 1-9
    lsl temp1
    add temp1, row
    add temp1, col          ; temp1 = row*3 + col
;=========================================================store Travel Time=====================================
	subi  temp1, -1    
	st X+, temp1
	
	

;=========================================================store Travel Time=====================================
    subi temp1, -48
    jmp convert_end1
letters1:
    cpi row, 0
	breq TimeFun
	cpi row, 3
	breq FinFun
    jmp convert_end1
symbols1:
    cpi col, 0              ; Check if we have a star
    breq star1
    cpi col, 1              ; or if we have zero
    breq zero1
    ldi temp1, 35          ; if not we have hash
    jmp convert_end1
star1:
    ldi temp1, 42          ; set to star
    jmp convert_end
zero1:
    ldi temp1, 48          ; set to zero
convert_end1:
	do_lcd_data temp1
debounce3:
	rcall sleep_5ms
	dec flag
	brne debounce3
	jmp readtime                ; restart the main loop

TimeFun:
	cp STN, temp3
	brlo JPST
	jmp NT8
	JPST:
		ldi temp3, 48
		jmp StopTimeFun
	NT8:
	inc temp3
	do_lcd_command 0b00000001 ; clear display
	do_lcd_letters 'T'
	do_lcd_letters 'i'
	do_lcd_letters 'm'
	do_lcd_letters 'e'
	do_lcd_data temp3
	debounce2:
	rcall sleep_5ms
	dec flag
	brne debounce2
	do_lcd_letters ':'

	rjmp readtime
FinFun:
	do_lcd_command 0b00000001 ; clear display
	do_lcd_letters 'F'
	do_lcd_letters 'i'
	do_lcd_letters 'n'
	do_lcd_letters 'i'
	do_lcd_letters 's'
	do_lcd_letters 'h'
	debounce6:
	rcall sleep_5ms
	dec flag
	brne debounce6
	jmp motorFUN
StopTimeFun:
	do_lcd_command 0b00000001 ; clear display
	do_lcd_letters 'S'
	do_lcd_letters 't'
	do_lcd_letters 'o'
	do_lcd_letters 'p'
	do_lcd_letters 'T'
	do_lcd_letters 'i'
	do_lcd_letters 'm'
	do_lcd_letters 'e'
	do_lcd_letters ':'
	debounce5:
	rcall sleep_5ms
	dec flag
	brne debounce5
	rjmp readtime


;============================================motor===================================================================

motorFun:
	ld temp3, -X		;r15: stop time
	mov r15, temp3
	clr temp3
	st X, temp3
	ldi XL, low(Ttime)
	ldi XH, high(Ttime)

motorFun0:
	clr temp3
	clr flag
	clr STN
	clr display1
	mov STN, r14
	cpi STN, 1			;if r14 = 1, count stop time
	breq stoptime
	
	ld SC, X+		;SC = travel time
	cpi SC, 0
	breq jstop
	ldi input, 60
	rjmp main
	jstop: jmp stop

	stoptime:
		mov SC, r15
		ldi input, 1
		mov r13, input
		ldi input, 0
		mov r14, input
		
	main:
	ldi temp3, (1<<PE4)		;labeled PE2 acctully PE4 
	out DDRE, temp3
	ser temp3
	sts OCR3BL, temp3
	clr temp3
	sts OCR3BH, temp3
	ldi temp3, (1 << ISC21 | 1 << ISC11 | 1 << ISC01)      ; set INT2 as falling-
    sts EICRA, temp3             ; edge triggered interrupt
    in temp3, EIMSK              ; enable INT2
    ori temp3, (1<<INT2 | 1<<INT1 | 1<<INT0)
    out EIMSK, temp3
	;set timer interrupt
	ldi temp3, (1<< WGM30)|(1<<COM3B1) ; set the Timer3 to Phase Correct PWM mode.
	sts TCCR3A, temp3
	ldi temp3, (1 << CS32)
	sts TCCR3B, temp3		; Prescaling value=8
	
	clr temp3
	out TCCR0A, temp3
	ldi temp3, (1<<CS01)
	out TCCR0B, temp3		; Prescaling value=8
	ldi temp3, 1<<TOIE0		; Enable timeroverflow flag
	sts TIMSK0, temp3	
	sts OCR3BH,input
	sts OCR3BL,input
	sts Zer, input
	sei						; Enable global interrupt
loop:
	rjmp loop

PB:
	cpi flag,0	; Check if the DebounceFlag is enabled
	breq DecreaseSpeed
	reti	
DecreaseSpeed:
	ldi flag,1
	mov r14, flag		;r14 -> ??pb??????. ????pb_0/1?flag = 1???? count stop time ?? ?????????
	reti

Timer0OVF:
	in temp3, SREG
	push temp3			; Prologue starts.
	push YH				; Save all conflict registers in the prologue.
	push YL
	push r25
	push r24
	// Update Debounce Flag here
	lds R24,DebounceCounter
	lds R25,DebounceCounter+1
	adiw r25:r24,1;
	cpi r24, low(1700)		; We set the debounce rate to ~200ms
	ldi temp3,high(1700)
	cpc r25, temp3
	breq SetDebounceFlag
	sts DebounceCounter,r24
	sts DebounceCounter+1,r25
	rjmp TimeCounter

SetDebounceFlag:
	clear DebounceCounter
	clr flag

TimeCounter:
	;clr temp3
	;mov r12, temp3
	lds r24, TC
	lds r25, TC+1 
	adiw r25:r24, 1
	cpi r25, high(2604)
	ldi temp3, low(2604)
	cpc r24, temp3
	brne NotaSecond
	
	clear TC
	mov temp3, r13
	cpi temp3, 1
	breq blink
	blink0:
	mov temp3, r12
	inc temp3
	mov r12, temp3
	cpi temp3, 3
	brne ENDIF

	N10:
		
		clr temp3
		mov r12, temp3
		;jmp readhash
	N11:
		subi SC, 1
		cpi SC, 0
		breq endcountingtime
		N12:
		sts OCR3BL, input		
		rjmp Endif

	endcountingtime:
		clr STN
		mov r13, STN		
		jmp motorFUN0
	blink:
		ldi STN, pattern
		out PORTC, STN
		rcall sleep_5ms
		rcall sleep_5ms
		rcall sleep_5ms
		clr STN
		out PORTC, STN
		rjmp blink0

NotaSecond:
	sts TC, r24
	sts TC+1, r25
Endif:
	pop	r24
	pop	r25
	pop	YL
	pop	YH
	pop	temp3
	out SREG, temp3
	reti

stop:
	ldi input, 0
	sts OCR3BL, input

readhash:
    ldi cmask, INITCOLMASK  ; initial column mask
    clr col                 ; initial column
colloop2:
    cpi col, 4
    breq readhash               ; If all keys are scanned, repeat.
    sts PORTL, cmask        ; Otherwise, scan a column.
    ldi temp1, 0xFF         ; Slow down the scan operation.
delay2:
    dec temp1
    brne delay2              ; until temp1 is zero? - delay
    lds temp1, PINL          ; Read PORTL
    andi temp1, ROWMASK     ; Get the keypad output value
    cpi temp1, 0xF          ; Check if any row is low
    breq nextcol2            ; if not - switch to next column
                            ; If yes, find which row is low
    ldi rmask, INITROWMASK  ; initialize for row check
    clr row
rowloop2:
    cpi row, 4              ; is row already 4?
    breq nextcol2            ; the row scan is over - next column
    mov temp2, temp1
    and temp2, rmask        ; check un-masked bit
    breq convert2            ; if bit is clear, the key is pressed
    inc row                 ; else move to the next row
    lsl rmask
    jmp rowloop2
nextcol2:                    ; if row scan is over
     lsl cmask
     inc col                ; increase col value
     jmp colloop2            ; go to the next column
convert2:
    cpi row, 3             ; If the pressed key is in col 3
    breq symbols2            ; we have letter
	jmp N10
symbols2:
    cpi col, 2
	breq clearS
	jmp N10
	clearS:
		cpi input, 0
		breq inc1
		
		ldi input, 0          ; if not we have hash
		sts OCR3BL, input
		jmp N12
		inc1:
		ldi input, 60
		sts OCR3BL, input
		jmp endcountingtime


;====================LCD Display==============
.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

.macro lcd_set
	sbi PORTA, @0
.endmacro
.macro lcd_clr
	cbi PORTA, @0
.endmacro
;
; Send a command to the LCD (r16)
;

lcd_command:
	out PORTF, r16
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	ret

lcd_data:
	out PORTF, r16
	lcd_set LCD_RS
	nop
	nop
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	lcd_clr LCD_RS
	ret

lcd_wait:
	push r16
	clr r16
	out DDRF, r16
	out PORTF, r16
	lcd_set LCD_RW
lcd_wait_loop:
	nop
	lcd_set LCD_E
	nop
	nop
        nop
	in r16, PINF
	lcd_clr LCD_E
	sbrc r16, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser r16
	out DDRF, r16
	pop r16
	ret

.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4
; 4 cycles per iteration - setup/call-return overhead

sleep_1ms:
	push r24
	push r25
	ldi r25, high(DELAY_1MS)
	ldi r24, low(DELAY_1MS)
delayloop_1ms:
	sbiw r25:r24, 1
	brne delayloop_1ms
	pop r25
	pop r24
	ret

sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret	