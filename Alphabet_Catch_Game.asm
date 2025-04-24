[org 0x0100]
jmp start

; Variables
char_bank:db "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
rand: dw 0
randnum: dw 0
score:dw 0
key_code: db 0
box_position: dw 4000-160-160+30+50 ; Initial cursor position  Row 23 Column 40 
Score_Message: db "Score:",0
Live_Message: db "Lives:",0
lives: dw 10
Game_Over: db "Game is Over! Well Played!",0
clock:          dw 0
loc_array:      dw 76+160, 78+160, 80+160, 82+160, 84+160
timer_array:    dw 2,4, 6, 8, 10
char_array:		db "START"
old_int:        dw 0
old_seg:		dw 0
old_seg_keyboard:dw 0
old_int_keyboard:dw 0
game_end_flag:dw 0
char_to_draw: db '*'
; Random Number Generator Routine (randG)
randG:
   push bp
   mov bp, sp
   pusha
   cmp word [rand], 0
   jne next

   ; Get System Timer for Seed
   mov ah, 00h   
   int 1Ah
   inc word [rand]
   mov [randnum], dx
   jmp next1

next:
   mov ax, 25173
   mul word [randnum]
   add ax, 13849
   mov [randnum], ax

next1:
   xor dx, dx
   mov ax, [randnum]
   mov cx, [bp+4]
   inc cx
   div cx
   mov [bp+6], dx
   popa
   pop bp
   ret 2

; Clear Screen Routine
clrscr: 
   push es
   mov ax, 0xb800
   mov es, ax
   xor di, di
   mov ax, 0x0720
   mov cx, 2000
   rep stosw
   pop es
   ret

; Draw Screen Borders
draw_borders:
   push 0xb800
   pop es
   mov di, 30
   mov al,[char_to_draw]
   mov ah, 11001100b
   mov cx, 50
upper_border:
   mov [es:di], ax
   add di, 2
   loop upper_border
   sub di, 2
   mov cx, 25
right_border:
   mov [es:di], ax
   add di, 160
   loop right_border
   sub di, 160
   mov cx, 50
lower_border:
   mov [es:di], ax
   sub di, 2
   loop lower_border
   add di, 2
   mov cx, 25
left_border:
   mov [es:di], ax
   sub di, 160
   loop left_border
   ret

; Draw Box Function
draw_box:
   push di
   push ax
   
   mov ah, 01001100b
   mov al, 0xDB
   ; Draw the box (6 bytes in both directions)
   mov [es:di], ax
   mov [es:di+2], ax 
   mov [es:di+4], ax 
   mov [es:di+6], ax 
   mov [es:di-2], ax 
   mov [es:di-4], ax
   mov [es:di-6], ax
   
   pop ax
   pop di
   ret

; Left Movement Function
left:
   push ax
   push di
   
   push 0xb800
   pop es
   
   mov ax, [box_position]
   mov di, ax
   cmp di, 4000-160-160+32+6
   jle no_move_left
   
   cmp word [score],50
   jge fast_move
   
   mov ax,0x0720
   mov word [es:di+6],ax
   
   mov ah, 01001100b
   mov al, 0xDB
   mov word [es:di-8],ax
   
   sub di, 2
   jmp final
   
   fast_move:
   mov ax,0x0720
   mov word [es:di+6],ax
   mov word [es:di+8],ax
   
   mov ah, 01001100b
   mov al, 0xDB
   mov word [es:di-8],ax
   mov word [es:di-10],ax
   
   sub di,4
   final:
   mov al, '*'
   mov ah, 01000100b
   mov word [es:3808],ax
   mov [box_position], di
   
no_move_left:
   pop di
   pop ax
   ret

; Right Movement Function
right:
   push ax
   push di
   
   mov ax, [box_position]
   mov di, ax
   cmp di, 4000-160-30-10
   jge no_move_right
   
   cmp word [score],50
   jge fast_move_
   
   mov ax,0x0720
   mov word [es:di-6],ax
   
   mov ah, 01001100b
   mov al, 0xDB
   mov word [es:di+8],ax
   
   add di, 2
   jmp final_
   
   fast_move_:
   mov ax,0x0720
   mov word [es:di-6],ax
   mov word [es:di-8],ax
   
   mov ah, 01001100b
   mov al, 0xDB
   mov word [es:di+8],ax
   mov word [es:di+10],ax
   
   add di,4
   final_:
   mov al, '*'
   mov ah, 01000100b
   mov word [es:3710],ax
   mov [box_position], di
   
no_move_right:
   pop di
   pop ax
   ret

; Keyboard Interrupt Handler (INT 9h)
new_key_int:
   pusha
   
   in al, 0x60
   mov [key_code], al
   cmp al, 0x4B
   je call_left
   cmp al, 0x4D
   je call_right
   jmp end_key_int

call_left:
   call left
   jmp end_key_int

call_right:
   call right
   jmp end_key_int

end_key_int:
   mov al, 0x20
   out 0x20, al
   
   popa
   iret

; Hook Keyboard Interrupt
hook_keyboard_isr:
   cli
   mov ax, 0
   mov es, ax
	mov word ax,[es:8*4]
	mov word [old_int],ax
		mov word ax,[es:8*4+2]
	mov word [old_seg],ax
	mov word ax,[es:9*4]
	mov word [old_int_keyboard],ax
	mov word ax,[es:9*4+2]
	mov word [old_seg_keyboard],ax
   mov word [es:9 * 4 + 2], cs
   mov word [es:9 * 4], new_key_int
   mov word [es:8*4+2],cs
   mov word [es:8*4],alphabet_thrower
   sti
   ret
  
;Incrementing Score  


Check_Score:
	push di
	push si
	push ax
	mov byte [char_to_draw],al
	push es
	mov ax,0xb800
	push ax
	pop es

	mov di,[box_position]
	;add si,160
	cmp si,di
	je scoreinc
	add di,2
	cmp si,di
	je scoreinc
	add di,2
	cmp si,di
	je scoreinc
	add di,2
	cmp si,di
	je scoreinc
	
	sub di,6
	sub di,2
	cmp si,di
	je scoreinc
	sub di,2
	cmp si,di
	je scoreinc
	sub di,2
	cmp si,di
	jne reduce_life
	
scoreinc:
	add word [score],1
	jmp exit
reduce_life:
	sub word [lives],1
	cmp word [lives],0
	jle set_game_over_flag
	jmp exit
set_game_over_flag:
	mov word [game_end_flag],1
exit:
	pop es
	pop ax
	pop si
	pop di
	ret 
;Print Score And Lives Algorithm
;subroutine to calculate the length of a string
; takes the segment and offset of a string as parameters
strlen: 
	push bp
	mov bp,sp
	push es
	push cx
	push di
	les di, [bp+4] ; point es:di to string
	mov cx, 0xffff ; load maximum number in cx
	xor al, al ; load a zero in al
	repne scasb ; find zero in the string
	mov ax, 0xffff ; load maximum number in ax
	sub ax, cx ; find change in cx
	dec ax ; exclude null from length
	pop di
	pop cx
	pop es
	pop bp
	ret 4
printnum: 
    push bp 
    mov bp, sp 
    push es 
    push ax 
    push bx 
    push cx 
    push dx 
    push di 
    mov ax, 0xb800 
    mov es, ax 
    mov ax, [bp+4]  
    mov bx, 10 ; use base 10 for division 
    mov cx, 0 ; initialize count of digits 
    nextdigit: 
        mov dx, 0 ; zero upper half of dividend 
        div bx ; divide by 10 
        add dl, 0x30 ; convert digit into ascii value 
        push dx ; save ascii value on stack 
        inc cx ; increment count of values 
        cmp ax, 0 ; is the quotient zero 
        jnz nextdigit ; if no divide it again 
        mov di,[bp+6] 
    nextpos: 
        pop dx ; remove a digit from the stack 
        mov dh, 00001100b ; use normal attribute 
        mov [es:di], dx ; print char on screen 
        add di, 2 ; move to next screen location 
        loop nextpos ; repeat for all digits on stack
		
	mov ax,0x0720
    mov word [es:di],ax
	mov ax,0x0720
    mov word [es:di+2],ax
	
    pop di 
    pop dx 
    pop cx 
    pop bx 
    pop ax 
    pop es 
    pop bp 
    ret 4		
; subroutine to print a string
; takes the x position, y position, attribute, and address of a null
; terminated string as parameters
printstr: 
	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push ds ; push segment of string
	mov ax, [bp+4]
	push ax ; push offset of string
	call strlen ; calculate string length
	cmp ax, 0 ; is the string empty
	jz exit_1 ; no printing if string is empty
	mov cx, ax ; save length in cx
	mov ax, 0xb800
	mov es, ax ; point es to video base
	mov al, 80 ; load al with columns per row
	mul byte [bp+8] ; multiply with y position
	add ax, [bp+10] ; add x position
	shl ax, 1 ; turn into byte offset
	mov di,ax ; point di to required location
	mov si, [bp+4] ; point si to string
	mov ah, [bp+6] ; load attribute in ah
	cld ; auto increment mode
nextchar: 
	lodsb ; load next char in al
	stosw ; print char/attribute pair
	loop nextchar ; repeat for the whole string
exit_1: pop di
	pop si
	pop cx
	pop ax
	pop es
	pop bp
	ret 8
	
;Draw Score And Lives
Draw_Score_Lives:
   push di
   push ax
   push bx
   push cx
   push dx
   push 70-2
   push 1
   push 11001010b
   push Score_Message
   call printstr
   push 78*2+160-2
	push word [score]
	call printnum
   push 70-2
   push 2
   push 11001010b
   push Live_Message
   call printstr
   push 78*2+160+160-2
	push word [lives]
	call printnum
   pop dx
   pop cx
   pop bx
   pop ax
   pop di
   ret 
; Unhook Keyboard Interrupt
; Alphabet Thrower Animation
alphabet_thrower: 
	
main:
    push ax
    push es
	push dx
	call Draw_Score_Lives
    add word [clock], 1
	
	mov dx,0

    mov cx, [clock]
    mov ax, cs
    mov ds, ax

    cmp cx, [ds:timer_array]
    jne slot_2
    mov bx, loc_array
    mov ax, 0xb800
    mov es, ax
    mov si, [bx]
    mov word [es:si], 0x0720

    add si, 160
    cmp si, 4000-160-160
    jge reset_char_1
    mov word [bx], si
	push bx
	mov bx,char_array
	mov al,[bx]
	mov ah ,10100100b
	pop bx
    mov word [es:si], ax
    add word [ds:timer_array], 2
	add dx,2
	jmp slot_2
	
reset_char_1:
	    call Check_Score
    mov word [loc_array], 0
    ;mov word [timer_array], 1
    ;jmp continue
slot_2:
    cmp cx, timer_array[2];clock
    jne slot_3
    mov bx, loc_array
    mov ax, 0xb800
    mov es, ax
    mov si, bx[2]
    mov word [es:si], 0x0720
    add si, 160
    cmp si, 4000-160-160
    jge reset_char_2

    ; Print si value after the operation for debugging

    mov word [bx + 2], si
	push bx
	mov bx,char_array
	mov al,[bx+1]
	mov ah ,10100100b
	pop bx
    mov word [es:si], ax
    add word timer_array[2], 4
	jmp slot_3
	reset_char_2:
	    call Check_Score

    mov word [loc_array + 2], 0
    ;mov word [timer_array + 2], 2
    ;jmp continue

slot_3:
		
    cmp cx, [ds:timer_array + 4]
    jne slot_4
    mov bx, loc_array
    mov ax, 0xb800
    mov es, ax
    mov si, [bx + 4]
    mov word [es:si], 0x0720
    add si, 160
    cmp si, 4000-160-160
    jge  reset_char_3
    mov word [bx + 4], si
	push bx
	mov bx,char_array
	mov al,[bx+2]
	mov ah ,10100100b
	pop bx
    mov word [es:si], ax
    add word [ds:timer_array + 4], 6
	jmp slot_4
reset_char_3:
	    call Check_Score

    mov word [loc_array + 4], 0
    ;mov word [timer_array + 4], 3
    ;jmp continue
slot_4:
    cmp cx, [ds:timer_array + 6]
    jne slot_5
    mov bx, loc_array
    mov ax, 0xb800
    mov es, ax
    mov si, [bx + 6]
    mov word [es:si], 0x0720
    add si, 160
    cmp si, 4000-160-160
    jge  reset_char_4
    mov word [bx + 6], si
	push bx
	mov bx,char_array
	mov al,[bx+3]
	mov ah ,10100100b
	pop bx
    mov word [es:si], ax
    add word [ds:timer_array + 6], 8
	jmp slot_5
reset_char_4:
	    call Check_Score

    mov word [loc_array + 6], 0
    ;mov word [timer_array + 6], 4
    ;jmp continue

slot_5:
	
    cmp cx, [ds:timer_array + 8]
    jne continue
    mov bx, loc_array
    mov ax, 0xb800
    mov es, ax
    mov si, [bx + 8]
    mov word [es:si], 0x0720
    add si, 160
    cmp si, 4000-160-160
    jge  reset_call
    mov word [bx + 8], si
	push bx
	mov bx,char_array
	mov al,[bx+4]
	mov ah ,10100100b
	pop bx
    mov word [es:si], ax
    add word [ds:timer_array + 8], 10
    jmp continue
reset_call:
	    call Check_Score
	    mov word [loc_array + 8], 0

    xor si, si
    mov cx, 5
load:
    sub sp, 2
    push 47
    call randG
    pop dx
    shl dx, 1
    add dx, 32 + 160               ; Align to rows
    mov word [loc_array + si], dx
    add si, 2
    loop load
	xor si, si
    mov cx, 5
	push di
char_allot:
	sub sp, 2
    push 26
    call randG
	pop di
	mov bx,char_bank
	mov al,[bx+di]
	mov byte [char_array+si],al
	add si,1
	loop char_allot
	pop di
    mov word [timer_array], 2
    mov word [timer_array + 2], 4
    mov word [timer_array + 4], 6
    mov word [timer_array + 6], 8
    mov word [timer_array + 8], 10
    mov word [clock], 0

continue:

	cmp word [game_end_flag], 0
	je dontFinish
	mov ax, 0
	mov es, ax
	cli
	push word [cs:old_int]
	pop word [es:8 * 4]
	push word [cs:old_seg]
	pop word [es:8*4+2]
		push word [cs:old_int_keyboard]
	pop word [es:9 * 4]
	push word [cs:old_seg_keyboard]
	pop word [es:9*4+2]
	sti

	call Game_Over_

dontFinish:
    mov al, 0x20
    out 0x20, al
	pop dx
    pop es
    pop ax
    iret
; Delay Routine
delay:
   push cx
   mov cx, 0xFFFF
delay_loop:
   loop delay_loop
   pop cx
   ret

;Game Over
Game_Over_:
   call clrscr
	
   call Draw_Score_Lives
   
   mov ah, 0x13 ; service 13 - print string 
   mov al, 1 ; subservice 01 – update cursor 
   mov bh, 0 ; output on page 0 
   mov bl, 00001100b ; normal attrib 
   mov dx, 0x0505 ; row 5 column 5 
   mov cx, 26 ; length of string 
   push cs 
   pop es ; segment of string 
   mov bp, Game_Over ; offset of string 
   int 0x10
   
   ret 
   
; Game Start Logic
game_start:
   call clrscr
   call draw_borders
   mov di, [box_position]
   call draw_box
   call hook_keyboard_isr
   ret

start:
	mov ax,0
	out 0x40,al
	mov al,ah
	out 0x40,al
   call game_start
   
   mov dx, t1
    add dx, 15
    mov cl, 4
    shr dx, cl
    mov ax, 0x3100
    int 0x21
t1: