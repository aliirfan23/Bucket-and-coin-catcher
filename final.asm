[org 0x0100]

jmp start

message: db 'Timer: ', 0 ; null terminated string
message1: db 'Score: ', 0 ; null terminated string
message2: db '*', 0 ; null terminated string
messagec: db '+', 0 ; null terminated string
messageb: db '.', 0 ; null terminated string
messagedollar: db '$', 0 ; null terminated string
msg: db ' ', 0 ; null terminated string
num : dw 120
score : dw 00
temp: dw 21,22,23; bucket y pos
temp1: dw 25,26,27 ;bucket x pos
tempp: dw 2,3 ; bomb y pos main triangle
temppcoin: dw 3,4,5 ; coin y pos
temppp: dw 5,6     ; bomb  y pos square

temp11: dw 5,6,7 ; bomb x position
temp11coin: dw 0 ; coin x position

ycoin2: dw 3,4,5
xcoin2: dw 40

ybomb2: dw 2,4,5
xbomb2: dw 31,32,33
msgs: db '<PRESS TO START>', 0 ; null terminated strin4
msgs1: db '<TIME OVER>', 0 ; null terminated string
; display a tick count on the top right of screen

tickcount: dw 0
seconds: dw 0
oldisr: dd 0 ; space for saving old isr
oldisr1: dd 0 ; space for saving old isr
; subroutine to scroll up the screen
; take the number of lines to scroll as parameter
scrollup: push bp
mov bp,sp
push ax
push cx
push si
push di
push es
push ds
mov ax, 80 ; load chars per row in ax
mul byte [bp+4] ; calculate source position
mov si, ax ; load source position in si
push si ; save position for later use
shl si, 1 ; convert to byte offset
mov cx, 2000 ; number of screen locations
sub cx, ax ; count of words to move
mov ax, 0xb800
mov es, ax ; point es to video base
mov ds, ax ; point ds to video base
xor di, di ; point di to top left column
cld ; set auto increment mode
rep movsw ; scroll up
mov ax, 0x120 ; space in normal attribute
pop cx ; count of positions to clear
rep stosw ; clear the scrolled space
pop ds
pop es
pop di
pop si
pop cx
pop ax
pop bp
ret 2

; clear screen using string instructions
; subroutine to clear the screen
clrscr: 
push es
push ax
push cx
push di
mov ax, 0xb800	
mov es, ax ; point es to video base
xor di, di ; point di to top left column
mov ax, 0x3120 ; space char in normal attribute
mov cx, 2000 ; number of screen locations
cld ; auto increment mode
rep stosw ; clear the whole screen
pop di
pop cx
pop ax
pop es
ret

clrscr1: 
push es
push ax
push cx
push di
mov ax, 0xb800	
mov es, ax ; point es to video base
xor di, di ; point di to top left column
mov ax, 0x1020 ; space char in normal attribute
mov cx, 2000 ; number of screen locations
cld ; auto increment mode
rep stosw ; clear the whole screen
pop di
pop cx
pop ax
pop es
ret

clrscr2: 
push es
push ax
push cx
push di
mov ax, 0xb800	
mov es, ax ; point es to video base
xor di, di ; point di to top left column
mov ax, 0x7020 ; space char in normal attribute
mov cx, 2000 ; number of screen locations
cld ; auto increment mode
rep stosw ; clear the whole screen
pop di
pop cx
pop ax
pop es
ret

top:
push es
push ax
push cx
push di
mov ax, 0xb800
mov es, ax ; point es to video base
xor di, di ; point di to top left column
mov ax, 0x1020 ; space char in normal attribute10
mov cx, 160 ; number of screen locations
cld ; auto increment mode
rep stosw ; clear the whole screen
pop di
pop cx
pop ax
pop es
ret
bottom:
push es
push ax
push cx
push di
mov ax, 0xb800
mov es, ax ; point es to video base
xor di, di ; point di to top left column
mov di,3680
mov ax, 0x6120 ; space char in normal attribute10
mov cx, 160 ; number of screen locations
cld ; auto increment mode
rep stosw ; clear the whole screen
pop di
pop cx
pop ax
pop es
ret


; subroutine to print a string
; takes the x position, y position, attribute, and address of a null
; terminated string as parameters
printstr: push bp
mov bp, sp
push es
push ax
push cx
push si
push di
push ds
pop es ; load ds in es
mov di, [bp+4] ; point di to string
mov cx, 0xffff ; load maximum number in cx
xor al, al ; load a zero in al
repne scasb ; find zero in the string
mov ax, 0xffff ; load maximum number in ax
sub ax, cx ; find change in cx
dec ax ; exclude null from length
jz exit ; no printing if string is empty
mov cx, ax ; load string length in cx
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
nextchar: lodsb ; load next char in al
stosw ; print char/attribute pair
loop nextchar ; repeat for the whole string
exit: pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 8





;time delay = 1second
delay:
mov ax,1200
loop1:
mov bx,1200

loop2:
dec bx
cmp bx,0
JNE loop2

dec ax
cmp ax,0
JNE loop1
ret



;number printing
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
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax
mov bx, 10 ; use base 10 for division
mov cx, 0 ; initialize count of digits
nextdigit:
mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30 ; convert digit into ascii value
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit 
; if no divide it again
mov di, 240 ; point di to top left column
nextpos:
pop dx ; remove a digit from the stack
mov dh, 23 ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop nextpos ; repeat for all digits on stack
pop di
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 2


printnum1: 
push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push di
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax
mov bx, 10 ; use base 10 for division
mov cx, 0 ; initialize count of digits
nextdigit1:
mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30 ; convert digit into ascii value
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit1 
; if no divide it again
mov di, 310 ; point di to top left column
nextpos1:
pop dx ; remove a digit from the stack
mov dh, 0x17 ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop nextpos1 ; repeat for all digits on stack
pop di
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 2
printscore: 
push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push di
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax
mov bx, 10 ; use base 10 for division
mov cx, 0 ; initialize count of digits
nextdigit11:
mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30 ; convert digit into ascii value
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit11 
; if no divide it again
mov di, 2160 ; point di to top left column
nextpos11:
pop dx ; remove a digit from the stack
mov dh, 0x71 ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop nextpos11 ; repeat for all digits on stack
pop di
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 2

check_death:
mov cx,0
mov bx,[temp1]
again1:
mov ax,[temp11]

cmp ax,bx
JE death
inc bx
inc cx
cmp cx,15
JNE again1

mov cx,0
mov bx,[temp11]
againleft:
mov ax,[temp1]

cmp ax,bx
JE death
inc bx
inc cx
cmp cx,6
JNE againleft

jmp exitt
death:
jmp endstr
exitt:
ret

check_death2:
mov cx,0
mov bx,[cs:temp1]
again12:
mov ax,[cs:xbomb2]

cmp ax,bx
JE death2
inc bx
inc cx
cmp cx,15
JNE again12

mov cx,0
mov bx,[cs:xbomb2]
againleft2:
mov ax,[cs:temp1]

cmp ax,bx
JE death2
inc bx
inc cx
cmp cx,10
JNE againleft2

jmp exitt2
death2:
jmp endstr
exitt2:
ret



check_score:
mov cx,0
mov bx,[temp1]
again:
mov ax,[temp11coin]

cmp ax,bx
JE in_score
inc bx
inc cx
cmp cx,15
JNE again
jmp exi
in_score:
mov ax,[score]
add ax,10
mov word [score],ax
exi:
ret

;bucket printing

bucket:
mov dx ,15
mov si,0
outer:
mov cx,0
mov bx ,[temp1+si]
add bx,1

loopp:
mov ax, bx
add ax,cx
push ax ; push x position
mov ax, [temp+si]
push ax ; push y position
mov ax, 0x6E	
push ax ; push attribute
mov ax, message2
push ax ; push address of message
call printstr ; call the printstr subroutine
add cx,1
cmp cx,dx
JNE loopp

add si,2
dec dx
dec dx
cmp si,6
JL outer
ret



;bomb printing

bomb:
mov dx ,10
mov si,0
outer1:
mov cx,0
mov bx ,[temp11+si]
add bx,1

loopp1:
mov ax, bx
add ax,cx
push ax ; push x position
mov ax, [tempp+si]
push ax ; push y position
mov ax, 0x04	
push ax ; push attribute
mov ax, messageb
push ax ; push address of message
call printstr ; call the printstr subroutine
inc cx
cmp cx,dx
JL loopp1

add si,2
dec dx
dec dx
cmp si,4
JNE outer1

ret

bomb2:
mov dx ,10
mov si,0
outer2:
mov cx,0
mov bx ,[xbomb2+si]
add bx,1

loopp2:
mov ax, bx
add ax,cx
push ax ; push x position
mov ax, [ybomb2+si]
push ax ; push y position
mov ax, 0x04	
push ax ; push attribute
mov ax, messageb
push ax ; push address of message
call printstr ; call the printstr subroutine
inc cx
cmp cx,dx
JL loopp2

add si,2
dec dx
dec dx
cmp si,4
JNE outer2


ret

;coin printing

coin:
mov dx ,4
mov si,0
Outer:
mov cx,0
mov bx ,[temp11coin+si]
add bx,1

loopp4:
mov ax, bx
add ax,cx
push ax ; push x position
mov ax, [temppcoin+si]
push ax ; push y position
mov ax, 0x2E
push ax ; push attribute
mov ax, messagedollar
push ax ; push address of message
call printstr ; call the printstr subroutine
add cx,1
cmp cx,dx
JL loopp4

add si,2
dec dx
dec dx
cmp si,4
JNE Outer


mov dx,2
mov bx ,[temp11coin+4]
mov cx,0
lop:
mov ax, bx
add ax,cx
push ax ; push x position
mov ax, [temppcoin+4]
push ax ; push y position
mov ax, 0x2E
push ax ; push attribute
mov ax, messagedollar
push ax ; push address of message
call printstr ; call the printstr subroutine
add cx,1
cmp cx,dx
JL lop

ret

dollar:
mov dx ,10
mov si,0
outerd:
mov cx,0
mov bx ,[cs:xcoin2]
add bx,1

looppd:
mov ax, bx
add ax,cx
push ax ; push x position
mov ax, [cs:ycoin2+si]
push ax ; push y position
mov ax, 0x21	
push ax ; push attribute
mov ax, messagedollar
push ax ; push address of message
call printstr ; call the printstr subroutine
inc cx
cmp cx,dx
JL looppd

add si,2
cmp si,4
JNE outerd
ret

;timer
timer: push ax 
jmp loopstate1
 loopstate:
 mov word[cs:tickcount],0
 push word [cs:seconds]
 ;bomb and coins moving and new position
 call inc_bomb 
 call inc_bomb2
 call inc_coin
 call main
 call printnum
 inc word[cs:seconds]
 cmp word[cs:seconds],60
jne loopstate1

loopstate1:
 inc word [cs:tickcount]
 cmp word[cs:tickcount],18
 je loopstate

; end of interrupt 
 pop ax 
 jmp far[cs:oldisr]



;timer subroutine

time:
xor ax, ax
mov es, ax ; point es to IVT base
mov ax,[es:8*4]
mov word[oldisr],ax
mov ax,[es:8*4+2]
mov word[oldisr+2],ax
cli ; disable interrupts
mov word [es:8*4], timer; store offset at n*4
mov [es:8*4+2], cs ; store segment at n*4+2
sti ; enable interrupts
ret

inc_bucket:
mov ax,[temp1]
mov bx,[temp1+2]
mov cx,[temp1+4]
inc ax
inc bx
inc cx
mov [temp1],ax
mov [temp1+2],bx
mov [temp1+4],cx
ret

dec_bucket:
mov ax,[temp1]
mov bx,[temp1+2]
mov cx,[temp1+4]
dec ax
dec bx
dec cx
mov [temp1],ax
mov [temp1+2],bx
mov [temp1+4],cx
ret


kbisr: 
push ax
push es
mov ax, 0xb800
mov es, ax ; point es to video memory
in al, 0x60 ; read a char from keyboard port
cmp al, 0x4b ; has the left shift pressed
jne nextcmp ; no, try next comparison
call dec_bucket

call main
mov byte [es:0], 'L' ; yes, print L at first column
jmp exit1 ; leave interrupt routine
nextcmp:
cmp al, 0x4d ; has the right shift pressed
jne nextcmp2 ; no, try next comparison
;; running game
call inc_bucket
call main
mov byte [es:0], 'R' ; yes, print R at second column
jmp exit1 ; leave interrupt routine
nextcmp2: 
cmp al, 0xaa ; has the left shift released
jne nextcmp3 ; no, try next comparison
mov byte [es:0], ' ' ; yes, clear the first column
jmp exit1 ; leave interrupt routine
nextcmp3: cmp al, 0xb6 ; has the right shift released
jne nomatch ; no, chain to old ISR
mov byte [es:2], ' ' ; yes, clear the second column
jmp exit1 ; leave interrupt routine
nomatch: pop es
pop ax
jmp far [cs:oldisr1] ; call the original ISR
exit1: 
mov al, 0x20
out 0x20, al ; send EOI to PIC
pop es
pop ax
iret



; main
main:
call clrscr ; call clrscr subroutine
call top ; call clrscr subroutine
call bottom ; call clrscr subroutine
mov ax, 33
push ax ; push x position
mov ax, 1
push ax ; push y position
mov ax, 151 
push ax ; push attribute
mov ax, message
push ax ; push address of message
;call delay
call printstr ; call the printstr subroutine




mov ax, 69
push ax ; push x position
mov ax, 1
push ax ; push y position
mov ax, 23
push ax ; push attribute
mov ax, message1
push ax ; push address of message
call printstr ; call the printstr subroutine

;score printing
mov ax,[score]
push ax 
call printnum1


call bucket


mov ax,[temp11]
inc ax
mov word[temp11+2],ax
inc ax
mov word[temp11+4],ax
;tempp: dw 10,11
;temppp: dw 8,9
;mov word[tempp],7
mov cx,word[cs:tempp]
add cx,1
mov word[cs:tempp+2],cx

;call delay
call bomb

mov ax,[cs:xbomb2]
inc ax
mov word[cs:xbomb2+2],ax
inc ax
mov word[cs:xbomb2+4],ax
;tempp: dw 10,11
;temppp: dw 8,9
;mov word[tempp],7
mov cx,word[cs:ybomb2]
add cx,1
mov word[cs:ybomb2+2],cx

;call delay
call bomb2




;xpos
mov ax,word[temp11coin]
inc ax
mov word[temp11coin+2],ax
inc ax
mov word[temp11coin+4],ax
;ypos
mov ax,[temppcoin]
inc ax
mov word[temppcoin+2],ax
dec ax
dec ax
mov word[temppcoin+4],ax
call coin






ret


;; increment coin
inc_coin:
mov ax,[temppcoin]
cmp ax,21
JE bringing_to_top


inc ax
inc ax
inc ax
mov [temppcoin],ax
jmp endl


bringing_to_top:
call check_score
mov word [temppcoin],3
mov ax,[temp11coin]
add ax,5
mov word [temp11coin],ax

endl:





;; increment bomb
inc_bomb:
mov bx,[tempp+2]
cmp bx,21	
JE bringing_back

inc bx
mov [tempp],bx
jmp endll


bringing_back:
call check_death

mov word [tempp],2
mov cx,[temp11]
add cx,4
mov word[temp11],cx
endll:

ret

inc_bomb2:
mov bx,[cs:ybomb2+2]
cmp bx,21	
JE bringing_back2

inc bx
inc bx
mov [cs:ybomb2],bx
jmp endll2


bringing_back2:
call check_death2

mov word [cs:ybomb2],2
mov cx,[cs:xbomb2]
add cx,5
mov word[cs:xbomb2],cx
endll2:
ret

;the main
start:
;start screen 
call clrscr1

mov ax, 30 
push ax ; push x position
mov ax, 12
push ax ; push y position
mov ax, 0x1F
push ax ; push attribute
mov ax, msgs
push ax ; push address of message
call printstr ; call the printstr subroutine
mov ah,0
int 0x16

call main

;timer

call time


xor ax, ax
mov es, ax ; point es to IVT base
mov ax, [es:9*4]
mov [oldisr1], ax ; save offset of old routine
mov ax, [es:9*4+2]
mov [oldisr1+2], ax ; save segment of old routine
cli ; disable interrupts
mov word [es:9*4], kbisr ; store offset at n*4
mov [es:9*4+2], cs ; store segment at n*4+2
sti ; enable interrupts


; mov dx, start ; end of resident portion
; add dx, 15 ; round up to next para
; mov cl, 4
; shr dx, cl ; number of paras
; mov ax, 0x3100 ; terminate and stay resident



luup:
mov ax,120
;call inc_bomb
mov al,5
int 16h
cmp al,27
JE endstr
cmp [cs:seconds],ax
jne luup
;call unhok





endstr:
mov ax,[oldisr];unhooking
mov [es:8*4],ax
mov ax,[oldisr+2]
mov [es:8*4+2],ax
mov ax,[oldisr1];unhooking
mov [es:9*4],ax
mov ax,[oldisr1+2]
mov [es:9*4+2],ax

call clrscr2
mov ax, 32
push ax ; push x position
mov ax, 11
push ax ; push y position
mov ax, 0xF4
push ax ; push attribute
mov ax, msgs1
push ax ; push address of message
call printstr ; call the printstr subroutine

mov ax, 32
push ax ; push x position
mov ax, 13
push ax ; push y position
mov ax, 0x71
push ax ; push attribute-
mov ax, message1
push ax ; push address of message
call printstr ; call the printstr subroutine
mov ax,[score]
push ax 
call printscore



mov ax, 0x4c00 ; terminate program
int 0x21