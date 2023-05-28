[org 0x0100] 
jmp start 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pcb: times (16*16) dw 0
mstack: times (16*256) dw 0
temp: dw 0

freepcb: dw 1 ; index of next free pcb 
current: dw 0 ; index of current pcb 

;;;;;;;;;;;;;;;;;;;;REQUIREMENTS FOR FUNCTIONS;;;;;;;;;;;;;;;;;;;;
;RemoveThread: push thread number, mov ax <- 59h and call int 21h
;SuspendThread: push thread number, mov ax <- 5Ah and call int 21h
;ResumeThread: push thread number, mov ax <- 5Bh and call int 21h
;createThread: push cs, push ip i.e offset, mov ax <- 58h and call int 21h



;;;;;;;;;;;;;;;YOUR  VARIABLE DECLARATION GOES HERE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

AXSAVE equ 0x04
BXSAVE equ 0x06
CXSAVE equ 0x08
DXSAVE equ 0x0A
SISAVE equ 0x0C
DISAVE equ 0x0E
BPSAVE equ 0x10
SPSAVE equ 0x12
IPSAVE equ 0x14
CSSAVE equ 0x16
DSSAVE equ 0x18
SSSAVE equ 0x1A
ESSAVE equ 0x1C
FLAGSSAVE equ 0x1E

;;;;;;;;;;;;;;;;;;;;INTERRUPT CODE;;;;;;;;;;;;;;;;;;;;
oldINT08: dw 0,0
oldINT21h: dw 0,0

ISR08: 
    call saveState
	call getNext  ;get next pcb number in ax
	call restoreState ;use ax to restore pcb

	mov al,0x20
	out 0x20,al
	jmp far[cs:oldINT08]

saveState: push bp
	mov bp,sp
	push bx
	mov bx,[current]
	shl bx,5
	mov [cs:pcb+AXSAVE+bx],ax
	mov [cs:pcb+CXSAVE+bx],cx
	mov [cs:pcb+DXSAVE+bx],dx
	mov [cs:pcb+SISAVE+bx],si
	mov [cs:pcb+DISAVE+bx],di
	mov [cs:pcb+DSSAVE+bx],ds
	mov [cs:pcb+SSSAVE+bx],ss
	mov [cs:pcb+ESSAVE+bx],es
	mov ax,[bp-2]
	mov [cs:pcb+BXSAVE+bx],ax
	mov ax,[bp]
	mov [cs:pcb+BPSAVE+bx],ax
	mov ax,[bp+4]
	mov [cs:pcb+IPSAVE+bx],ax
	mov ax,[bp+6]
	mov [cs:pcb+CSSAVE+bx],ax
	mov ax,[bp+8]
	mov [cs:pcb+FLAGSSAVE+bx],ax
	mov ax,bp
	add ax,10
	mov [cs:pcb+SPSAVE+bx],ax
    pop bx
    pop bp
	ret

getNext:push bx
	mov bx,[current]
	shl bx,5
	mov ax,[pcb+bx+0]
	and ax,0x00FF
	pop bx
	ret

restoreState: mov bx,ax
    mov [cs:current],ax
    shl bx,5
    mov cx,[cs:pcb+CXSAVE+bx]
	mov dx,[cs:pcb+DXSAVE+bx]
	mov si,[cs:pcb+SISAVE+bx]
	mov di,[cs:pcb+DISAVE+bx]
	mov bp,[cs:pcb+BPSAVE+bx]
	mov ax,[cs:pcb+ESSAVE+bx]
	mov es,ax
	mov ax,[cs:pcb+DSSAVE+bx]
	mov ds,ax
	pop ax
	mov [cs:temp],ax

	cli
	mov ss,[cs:pcb+SSSAVE+bx]
	mov sp,[cs:pcb+SPSAVE+bx]
	sti

	mov ax,[cs:pcb+FLAGSSAVE+bx]
	push ax
	mov ax,[cs:pcb+CSSAVE+bx]
	push ax
	mov ax,[cs:pcb+IPSAVE+bx]
	push ax
	mov ax,[temp]
	push ax
	mov ax,[cs:pcb+AXSAVE+bx]
	mov bx,[cs:pcb+BXSAVE+bx]
	ret

createThread: 
    call getFreePCB
    cmp word[cs:freepcb],0
    je exit1
    call initializePCB
    call insertThread
exit1:
    iret

getFreePCB:
    push ax
    push bx
    push cx
    push dx
    mov cx,16
    mov dx,0
    freePcbLoop: mov bx,dx
        shl bx,5
        mov ax,[cs:pcb+bx+2]
        cmp al,0
        je returnPCB
        inc dx
        loop freePcbLoop
    xor dx,dx
returnPCB:
    mov [freepcb],dx
    pop dx
    pop cx
    pop bx
    pop ax
    ret

initializePCB:
    push bp
    mov bp,sp
    push ax
    push bx
    mov ax,[cs:freepcb]
    mov bx,ax
    shl bx,5
    mov word[cs:pcb+bx+0],0 ;next and previous pointers
    mov word[cs:pcb+AXSAVE+bx],0
    mov word[cs:pcb+BXSAVE+bx],0
    mov word[cs:pcb+CXSAVE+bx],0
    mov word[cs:pcb+DXSAVE+bx],0
    mov word[cs:pcb+SISAVE+bx],0
    mov word[cs:pcb+DISAVE+bx],0
    mov word[cs:pcb+BPSAVE+bx],0
    mov [cs:pcb+SSSAVE+bx],cs
    mov [cs:pcb+DSSAVE+bx],ds
    mov [cs:pcb+ESSAVE+bx],es
    mov word[cs:pcb+FLAGSSAVE+bx],0x200
    mov word[cs:pcb+bx+2],1  ;setting pcb to occupied
    inc ax
    shl ax,9
    mov [cs:pcb+SPSAVE+bx],ax
    mov ax,[cs:bp+12]
    mov [cs:pcb+CSSAVE+bx],ax;saving cs from stack for new thread
    mov ax,[cs:bp+10]
    mov [cs:pcb+IPSAVE+bx],ax;saving ip from stack for new thread
    pop bx
    pop ax
    pop bp
    ret

insertThread:
push ax
push bx
push cx 
push bp

cli
mov bx,[cs:freepcb]
mov cx,bx
shl bx,5

mov ax, [cs:pcb+0] ; read next of 0th thread in ax
and ax,0x00FF

mov byte[cs:pcb+bx+0],al ;writing next of new thread

mov byte[cs:pcb+bx+1],0 ; set previous of new thread i.e 0

mov ax,[cs:pcb+bx+0];reading next of new thread assigned earlier
and ax,0x00FF
shl ax,5
mov bp,ax       ;going to the next of new thread

mov byte[cs:pcb+bp+1],cl ;writing previous of previously next of 0th thread 

mov byte[cs:pcb+0], cl ; set next of 0th thread
sti

pop bp
pop cx
pop bx
pop ax
ret

removeThread:
push bp
mov bp,sp
push ax
push bx
push si
mov bx,[bp+8]
shl bx,5

mov ax,[cs:pcb+bx+0] ;reading next & previous
and ax,0x00FF
shl ax,5
mov si,ax
mov ah,[pcb+bx+1];reading current previous
mov byte[pcb+si+1],ah ;writing previous of current's next

xor ax,ax
mov al,[cs:pcb+bx+1] ;reading previous
shl ax,5
mov si,ax
mov al,[cs:pcb+bx+0];reading next of current
mov [cs:pcb+si+0],al ;writing next of current's previous

mov word[cs:pcb+bx+2],0 ; setting suspended to 0 of current pcb i.e it is now free 

pop si
pop bx
pop ax
pop bp
iret

suspendThread:
push bp
mov bp,sp
push ax
push bx
push si
mov bx,[bp+8]
shl bx,5

mov ax,[cs:pcb+bx+0] ;reading next & previous
and ax,0x00FF
shl ax,5
mov si,ax
mov ah,[pcb+bx+1];reading current previous
mov byte[pcb+si+1],ah ;writing previous of current's next

xor ax,ax
mov al,[cs:pcb+bx+1] ;reading previous
shl ax,5
mov si,ax
mov al,[cs:pcb+bx+0];reading next of current
mov [cs:pcb+si+0],al ;writing next of current's previous

mov word[cs:pcb+bx+2],2 ; setting suspended to 0 of current pcb i.e it is now suspended

pop si
pop bx
pop ax
pop bp
iret

resumeThread:
push bp
mov bp,sp
push ax
push bx
push cx
push si
mov bx,[bp+8]
mov cx,bx
shl bx,5
	
mov ax,[pcb+bx+0] ;reading next & previous
and ax,0x00FF
shl ax,5
mov si,ax
mov [pcb+si+1],cl ;connecting current pcb to the prev of curr->next

xor ax,ax
mov al,[pcb+bx+1] ;reading previous
shl ax,5
mov si,ax
mov [pcb+si+0],cl ;connecting current pcb to the next of curr->prev

mov word[cs:pcb+bx+2],1 ;setting suspended to 0 of current pcb i.e it is now suspended

pop si
pop cx
pop bx
pop ax
pop bp
iret

ISR21h:
    cmp al,0x58
    je createThread
    cmp al,0x59
    je removeThread
    cmp al,0x5A
    je suspendThread
    cmp al,0x5B
    je resumeThread
    jmp far[oldINT21h]


start:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HOOK THE TIMER INTERRUPT;;;;;;;;;;;;;;;;;;;;;;;
	cli
	xor ax,ax
	mov es,ax
    
    mov ax,[es:8*4];saving old timer interrupt
    mov [oldINT08],ax
    mov ax,[es:8*4+2]
    mov [oldINT08+2],ax

	mov word[es:8*4],ISR08;Hooking new timer interrupt
	mov [es:8*4+2],cs

    mov ax,[es:0x21*4];saving old 21h interrupt
    mov [oldINT21h],ax
    mov ax,[es:0x21*4+2]
    mov [oldINT21h+2],ax

    mov word[es:0x21*4],ISR21h
    mov [es:0x21*4+2],cs

	sti
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;SETTING 0 THREAD TO OCCUPIED;;;;;;;;;;;;;;;;;;;;;
    mov word[cs:pcb+2],1
    

    mov ax,0xb800
    mov es,ax

    xor ax,ax

    push cs
    push thread1
    mov al,0x58	; create it's thread
    int 21h

    push cs
    push thread2
    mov al,0x58	; create it's thread
    int 21h

    ; ;deleting thread 
    ; mov ax,2
    ; push ax
    ; xor ax,ax
    ; mov al,0x59 ;remove thread 
    ; int 21h

    ; ;suspending thread
    ; mov ax,2
    ; push ax
    ; xor ax,ax
    ; mov al,0x5A ;suspend thread
    ; int 21h

    ; ; resuming thread
    ; mov ax,2
    ; push ax
    ; xor ax,ax
    ; mov al,0x5B ;resume thread
    ; int 21h

    jmp thread3

thread1: 
    mov ax,0xb800
    mov es,ax
    thread1loop:
	mov ah,0x07
	mov al, [cs:array1+bx] ; read the next char
	mov [es:0], ax ; write at top left of screen
	inc bx
	cmp bx,5
    jne thread1loop
    xor bx,bx   ; reset bx if at end of array1
	jmp thread1loop; infinite task

; second task 
thread2:
    mov ax,0xb800
    mov es,ax
    thread2loop:
	mov ah,0x07
	mov al, [cs:array2+bx] ; read the next char
	mov [es:158], ax ; write at top right of screen
	inc bx
	cmp bx,6
    jne thread2loop
    xor bx,bx   ; reset bx if at end of array2
    jmp thread2loop ; infinite task

thread3:
    mov ax,0xb800
    mov es,ax
    thread3loop:
    mov ah,0x07
    mov al, [cs:array3+bx] ; read the next char
    mov [es:240], ax ; write at center of second line
    inc bx
    cmp bx,5
    jne thread3loop
    xor bx,bx   ; reset bx if at end of array3
    jmp thread3loop ; infinite task



array1: db 'First'
array2: db 'Second'
array3: db 'Third'