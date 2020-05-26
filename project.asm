IDEAL
MODEL small
STACK 100h
DATASEG
tempX dw 0 ; temporary x for the rectangle procedure
tempY dw 0 ; temporary y for the rectangle procedure
xParam dw 60 ; x coordinate that will be pushed for the rectangle function
notesOde db 'E4   A4   B4  C#5   D5   E5$' ; the notes that will appear onscreen for ode to joy
notesTwinkle db 'G4   A4   B4   C5   D5   E5$'   ; the notes that will appear onscreen for ah! vous dirai-je, maman
keys db ' 1    2    3   4    5    6$' ; the keybinds
space db '              ',10,13,'$' ; for some reason it is needed in order to place the strings correctly
song db 'Piece: Ode to Joy$' ; the name of the piece that will appear at the top of the string
twinkletwinkle db 'Piece: Twinkle Twinkle Little Star$' ; the name of the piece that will appear at the top of the string
score dw 0 ; score variable that will be printed
currentKey dw 60 ; the place of the rectangle onscreen
currentNote dw 0 ; the current note playing
last db 0 ; last ah. for loops waiting for data.
switch db 0 ; if pressed correctly - value is 1. incorrectly - 2.
;the first and last arrays are the arrays in which are stored the scan codes corresponding to the correct notes in the piece, according to the keybinds
; in the second and third arrays are stored the visual places onscreen on which the next note will appear to the user in blue
twinklearr db 2h,2h,6h,6h,7h,7h,6h,5h,5h,4h,4h,3h,3h,2h,6h,6h,5h,5h,4h,4h,4h,3h,6h,6h,5h,5h,4h,4h,4h,3h,2h,2h,6h,6h,7h,7h,6h,5h,5h,4h,4h,3h,3h,2h
twinklekeys dw 60,60,220,220,260,260,220,180,180,140,140,100,100,60,220,220,180,180,140,140,140,100,220,220,180,180,140,140,140,100,60,60,220,220,260,260,220,180,180,140,140,100,100,60
odekeys dw 180,180,220,260,260,220,180,140,100,100,140,180,180,140,140,180,180,220,260,260,220,180,140,100,100,140,180,140,100,100,140,140,180,100,140,180,220,180,100,140,180,220,180,140,100,140,60
odearr db 5h,5h,6h,7h,7h,6h,5h,4h,3h,3h,4h,5h,5h,4h,4h,5h,5h,6h,7h,7h,6h,5h,4h,3h,3h,4h,5h,4h,3h,3h,4h,4h,5h,3h,4h,5h,6h,5h,3h,4h,5h,6h,5h,4h,3h,4h,2h
endd dw 0 ; this variable will contain the offset of an array plus double the size of it, so that when the song is over it'll exit the main game
endb dw 0 ; this variable will contain the offset of an array plus double the size of it, so that when the song is over it'll exit the main game
key dw 60 ; the x coordinate of the key that will be whitened after releasing 
count dw 0 ; count of how many keys are needed to be drawn
endmsg db 'You have finished, well done!',10,13 ; finished message
    db 'To return to the main menu, press h.',10,13 
    db 'To exit, press esc. Your score is: $'
lostmsg db 'You lost! Try again next time. To return to the main menu, press h. To exit, press esc$' ; lost message
geNote dw 0 ; the variable that will be changing according to the user's key press and the song selected. this containes either the G or E notes.
cNote dw 0 ; the variable that will be changing according to the user's key press and the song selected. this containes either the C# or C notes.
choose db 'Choose song: Ode to joy - 7, Twinkle Twinkle Little Star: 8$' ; song choosing
scoreMsg db 'score: $' ; score message
msg db 'Welcome to PianoASM!',10,13 ; main menu message, instructions
    db 10,13
    db 'In this game you will need to press the matching key on your keyboard. The next',10,13
    db 'key is the blue one.',10,13
    db 'For each note, you have 2 seconds to press.',10,13
    db 'If pressed a wrong key or out of time, the key will be lit up in red.',10,13
    db 'If pressed correctly, the key will be lit up in green.',10,13
    db 'If you score falls bellow 0, you will lose.',10,13
    db '(Press a) Arcade mode - the rhythm game.',10,13
    db '(Press esc) To exit the program',10,13
	db ' $'

CODESEG
   
;A PROCEDURE THAT DRAWS A RECTANGLE WITH FIXED WIDTH AND LENGTH
;PARAMETERS:
; x - X COORDINATE OF THE LEFTMOST PIXEL OF THE RECTANGLE
; y - Y COORDINATE OF THE TOPMOST PIXEL OF THE RECTANGLE
; color - COLOR CODE FOR THE RECTANGEL'S COLOR
x equ [bp + 6] ;set the spot in the stack to the X coordinate
y equ [bp + 8] ;set the spot in the stack to Y coordinate
color equ [bp + 4] ;set the spot in the stack to the color
proc drawRect
	push bp ; push base pointer, save value
	mov bp,sp ; move the base pointer to the stack pointer
	push ax ; save value
	push bx ; save value
	push cx ; save value
	push dx ; save value
		
	mov bh,0h ;to set the coordinates on screen
	mov cx,x ;get the X parameter
	mov dx,y ;get the Y parameter
	mov [tempX],cx ; temporary x coordinate
	add [tempX],20 ; width of rectangle
	mov [tempY],dx ; temporary y coordinate
	add [tempY],100 ; length of rectangle

    line: ; the label that draws the line
      inc dx ; to draw a line 
			mov al,color ; get the color
			mov ah,0Ch ; move ah so that the interrupt code can be used
			int 10h ;interrupt - draw
      cmp dx,[tempY] ; loop until 20
      je square ; if finished, continue
      jne line ; else, loop

	square: 
		inc cx ; increment cx until reaches the desired x coordinate
		mov dx,y ; reset y to fixed coordinate
		cmp cx,[tempX] ; if reached the desired x coordinate
		je finish ; go to the end of the procedure
		jne line ; else, loop

    finish:
		pop dx ; save the value of dx
		pop cx ; save the value of cx
		pop bx ; save the value of bx
		pop ax ; save the value of ax
		pop bp ; save the value of bp
		ret 6 ; to prevent a stack overflow, return - "clear" the stack
endp drawRect

; A PROCEDURE THAT PRINTS A VARIABLE
; PARAMETERS: 
; VAR - THE VARIABLE TO PRINT
var equ [bp + 4] ; variable to be printed
proc print       
  push bp ; push base pointer, save value
	mov bp,sp ; move the base pointer to the stack pointer
	push ax ; save value
	push bx ; save value
	push cx ; save value
	push dx ; save value

    ;initialize count: 
  xor cx,cx ; resetting cx
	xor dx,dx ; resetting dx
  mov ax,var ; set ax to the desired variable

	;LOOP UNTIL AX IS 0 - DIVIDE AND THEN KEEP THE REMAINDER IN THE STACK, (INSTEAD OF 4 LABELS, ONE FOR EACH DIGIT)
    dig: 
        cmp ax,0 ; if ax is zero
        je print1 ; move to next label that prints
        mov bx,10 ;initialize bx to 10         
        div bx ; extract the last digit                  
        push dx ;push it in the stack              
        inc cx ;increment the count
        xor dx,dx ;set dx to 0  
        jmp dig
    print1: 
        cmp cx,0 ;check if count is greater than zero 
        je fin ; if equals, don't print
        pop dx ;pop the top of stack 
        add dx,48 ; add to the number to match ascii value
        mov ah,02h ;interuppt to print a character 
        int 21h  ; interrupt - print charater
        dec cx ; decrease the count
        jmp print1 
	fin: 
		pop dx ; save the value of dx
		pop cx ; save the value of cx
		pop bx ; save the value of bx
		pop ax ; save the value of ax
		pop bp ; save the value of bp
	ret 2
endp print

; A PROCEDURE THAT PLAYS A SOUND
; PARAMETER:
; THE QUOTIENT OF THE SOUND CONSTANT DIVIDED BY THE NOTE FREQUENCY
note equ [bp + 4] ; parameter
proc sound
  push bp ; push base pointer, save value
	mov bp,sp ; move the base pointer to the stack pointer
	push ax ; save value

	in al, 61h ; open speaker
  or al, 00000011b ;clearing the last two bits
  out 61h, al ; activate speaker through the port
  mov al, 0B6h ; to change frequency
  out 43h, al ;send to port
  mov ax,note
  out 42h, al ; playing lower byte
  mov al,ah
  out 42h, al ;playing higher byte

	pop ax ; save the value of ax
	pop bp ; save the value of bp
  ret 2 ; to prevent stack overflow
endp sound

; A PROCEDURE THAT ENDS SOUND
proc endSound
	push ax
  in al, 61h ; closing the speaker
  and al, 11111100b ; change the last two bits
  out 61h, al ; send to port
	pop ax ; save the value of ax
    ret 
endp endSound

	
	; THE MAIN SCREEN OF THE GAME ITSELF, CONTAINING THE KEYBOARD AND THE CONTROLS OF THE GAME.
	; PARAMTERS:
	; WHICHSONG - STORES THE SONG CHOOSE (1 OR 2)
	whichSong equ [bp + 4] ; a parameter that stores the song choose
	proc mainGame
		push bp ; push base pointer, save value
		mov bp,sp ; move the base pointer to the stack pointer
		push ax ; save value
		push bx ; save value
		push cx ; save value
		push dx ; save value
		push di ; save value
		push si ; save value
		mov ax,13h ; move to graphic mode
		int 10h ; interrupt - graphic mode

		mov  dl, 0   ; Column
		mov  dh, 47   ; Row
		mov  bh, 0    ; Display page
		mov  ah, 02h  ; Set cursor position
		int  10h ; interrupt - change cursor location

		mov dx,offset space ;set dl to space
		mov ah,9h ; Set interrupt code to 9, write a string
		int 21h ; Interrupt - print char

		mov ax,whichSong
		
		cmp ax,1 ; if 1 - ode to joy. if 2 - twinkle twinkle little star
		je ode ; move to ode to joy label
		jne twinkle ; move to twinkle label

		twinkle: ; a label that initializes the game in case twinkle twinkle little star was chosen
			mov dx,offset notesTwinkle ;set dl to notesTwinkle
			mov ah,9h ; Set interrupt code to 9, write a string
			int 21h ; Interrupt - print char
			mov  dl, 1 ; Column
			mov  dh, 1 ; Row
			mov  bh, 0 ; Display page
			mov  ah, 02h ; Set cursor position
			int  10h ; interrupt - change cursor location
			mov dx,offset twinkletwinkle ;set dl to twinkletwinkle
			mov ah,9h ; Set interrupt code to 9, write a string
			int 21h ; Interrupt - print char
			mov [geNote],0BE3h ; the hex value of the note G divided by the sound constant
			mov [cNote],08E9h ; the hex value of the note C divided by the sound constant
			mov di,offset twinklekeys ; to access the array, we use a register that will contain the address of the first element.
			mov si,offset twinklearr ; to access the array
			mov [endd],di ; the cue to end the game is when we have iterated 
			mov [endb],si ; over the last element of either arrays - the notes or the keys
			add [endd],88 ; the address that contains the end of the keys array
			add [endb],88 ; the address that contains the end of the notes - scan codes array
			jmp initScreen ; jump to the loop initializing the screen 

    ode:
			mov dx,offset notesOde ; set dl to notesOde
			mov ah,9h ; Set interrupt code to 9, write a string
			int 21h ; Interrupt - print char
			mov  dl, 8 ; Column
			mov  dh, 1 ; Row
			mov  bh, 0 ; Display page
			mov  ah, 02h ; Set cursor position
			int  10h ; interrupt - change the cursor location
			mov dx,offset song ;set dl to song
			mov ah,9h ; Set interrupt code to 9, write a string
			int 21h ; Interrupt - print char
			mov [geNote],0E2Ah ; move the variable that contains either G or E to E
			mov [cNote],0869h ; move the variable that contains either C or C# to C#
			mov di,offset odekeys
		
		 ; set di to the offset of the matching ode to joy array keys
			mov si,offset odearr ; to access the array that contains scan codes for the piece
			mov [endd],di ; the cue to end the game is when we have iterated...
			mov [endb],si ; ...over the last element of either arrays - the notes or the keys
			add [endd],94 ; the address that contains the end of the keys array
			add [endb],94 ; the address that contains the end of the notes - scan codes array
			jmp initScreen ; jump to the loop initializing the screen 

		initScreen:
			mov  dl, 29 ; Column
			mov  dh, 2 ; Row
			mov  bh, 0 ; Display page
			mov  ah, 02h ; Set cursor position
			int  10h ; interrupt - change the cursor location
			mov dx,offset scoreMsg ;set dl to scoreMsg
			mov ah,9h ; Set interrupt code to 9, write a string
			int 21h ; Interrupt - print char
			mov  dl, 8 ; Column
			mov  dh, 7 ; Row
			mov  bh, 0 ; Display page
			mov  ah, 02h  ; Set cursor position
			int  10h ; interrupt - change the cursor location
			mov dx,offset keys ;set dl to keys
			mov ah,9h ; Set interrupt code to 9, write a string
			int 21h ; Interrupt - print char
			mov [count],6 ; initialize count variable for drawing keys
			;INITIALIZE KEYS
		drawkeys:
			push 70 ; y coordinate of the topmost point of the key
			push [xParam] ; initialize the keys using a variable
			push 15 ; white color code
			call drawRect ; draw the key
			add [xParam],40 ; add so that the next key will be drawn near the previous
			dec [count] ; decrease count for the loop
			cmp [count],0 ; if looped 6 times, continue
			jne drawkeys ; else, loop

			;print the first note to play in blue
    push 70 ; y coordinate of the topmost point of the key
    push [di] ; the first element in the keys array
    push 1 ; blue color code
    call drawRect ; draw the key
	
      printScore:
				mov  dl, 35 ; Column
				mov  dh, 2 ; Row
				mov  bh, 0 ; Display page
				mov  ah, 02h ; Set cursor position
				int  10h ; interrupt - change cursor location
				push [score] ; push updated score
				call print ; update score on screen
				mov dl,' ' ;set char to 'A'
				mov ah,02h ; Set interrupt code to space - a space so that the place near the score won't show the digits of the previous number
				int 21h ; Interrupt - print char
				mov ah,02Ch ; Get current second code  
				int 21h ; interrup - current second in bh. 
				mov bh,dh ; Store current second in bh

		waitfordata: ;a loop that waits for key press
			mov ah,02Ch ; Call function 02C of int 21 to get new time
			int 21h ; interrup - current second in bh. 
			sub dh,02h ; Subtract 2 from new second. Giving 2 seconds for input.
			cmp bh,dh ; when bh and dh are equal, two seconds have passed.   
			je outoftime  ; jump to outoftime label when dh is finally equal to bh.
			mov [count],6 ; re-initialize count variable for drawing keys
			in al,64h ;gaining access to the keyboard
			cmp al,10b ;gaining access to the port
			in al,60h ;gaining access to the keyboard
			cmp [last],al ;comparing the scan code of the current key to the previous one
			je waitfordata ;if nothing changed, go back to the loop
			mov [last],al ; set the variable for next time
			cmp al,1h ; if esc pressed, 
			je midfinal ; jump to exit label
			cmp al,[si] ; array element
			je midCorrect ; if the key pressed equals to the current element in the array storing scan codes,
			jne midincc ; jump to correct label
		jmp waitfordata ; loop

			;A LABEL THAT DRAWS THE NEXT KEY IN BLUE
      drawNext:
        push 70 ; y coordinate of the topmost point of the key
        push [di] ; the next element in the keys array
        push 1 ; blue color code
        call drawRect ; call procedure to draw the key
        cmp di,[endd] ; if reached the end of the array, exit
        je midfinish ; jump to midfinish label
        jmp printScore ; print updated score

		midfinal:
		jmp final ; jump to very end of procedure

		midCorrect:
	  jmp switchCorrect ; jump to switchCorrect label

		midfinish:
		jmp finishh ; jump to finished game

		midincc:
		jmp incc ; jump to incc label

		midlost:
		jmp lost ; jump to lost label

		outoftime:
			; CLEAR KEYS
			push 70 ; y coordinate of the topmost point of the key
			push [key] ; initialize the keys using a variable
			push 15 ; white color code
			call drawRect ; draw the key
			add [key],40 ; add so that the next key will be drawn near the previous
			dec [count] ; decrease count for the loop
			cmp [count],0 ; if looped 6 times, continue
			jne outoftime ; else, loop
			mov [key],60
			; DRAW A RED RECTANGLE
			mov bx,[di] ; set bx to the value of current element in array
			sub [score],5 ; subtract 5 from score
			add di,2 ; advance to next element
			inc si ; advance to next element - next note
			cmp si,[endb] ; if reached end of array
			je midfinish ; finished the game
			push 70 ; y coordinate of the topmost point of the key
			push bx ; the current element 
			push 4 ; red color code
			call drawRect ; call procedure to draw key
			cmp [score],500 ; if score above 500, the player have lost (best score possible is 235)
      ja midlost ; due to the fact that if falls bellow zero, a variable will be changed to the maximum integer value
		jmp drawNext ; draw next key
		
		incc:
			cmp al,1h ; if esc pressed
			je midfinal ; exit
			; bellow are compares for scan codes...
			cmp al,2h ; if all the bellow pressed, go to incorrect (since we got here due to incorrect press...)
			je switchIncorrect ; ... all options for mistake are legitimate after we have determined...
			cmp al,3h ; ...the correct key in previous label. 
			je switchIncorrect ; so it necessarily will always execute only after the initial correct check...
			cmp al,4h ; ... if either one of 1,2,3,4,5,6 keys were pressed, it'll jump to switchIncorrect label
			je switchIncorrect ; that changes the value of the switch variable to incorrect value (2)
			cmp al,5h ; if none of the following keys (1,2,3,4,5,6) were pressed...
			je switchIncorrect ; continue running the code in order
			cmp al,6h
			je switchIncorrect
			cmp al,7h
			je switchIncorrect

		released:
			push 70 ; y coordinate of the topmost point of the key
			push [key] ; the released key x coordinate
			push 15 ; white color code
			call drawRect ; call procedure to draw key
			add [key],40 ; add to the variable 40, to draw next key
			dec [count] ; decrement count for looping purposes
			cmp [count],0 ; if reached 0, continue
			jne released ; else, loop
			mov [key],60 ; re-initialize for next time
      call endSound ; end sound
    jmp drawNext

		switchCorrect:
			mov [switch],1 ; change the switch to correct code

			;	THE FOLLOWING LABEL CHECKS FOR THE PRESSED KEY USING SCAN CODES AND JUMPS TO CORRECT LABEL ACCORDINGLY
			setkeys:
				cmp al,2h ; 1 scan code
				je setKey1
				cmp al,3h ; 2 scan code
				je setKey2
				cmp al,4h ; 3 scan code
				je setKey3
				cmp al,5h ; 4 scan code
				je setKey4
				cmp al,6h ; 5 scan code 
				je setKey5
				cmp al,7h ; 6 scan code
				je setKey6

		switchIncorrect: 
			mov [switch],2 ; change the switch to incorrect code
			jmp setkeys ; jump to label setting keys

		setKey1:
			mov [currentKey],60 ; x coordinate for the current key
			mov dx,[geNote] ; to avoid illegal memory reference error
			mov [currentNote],dx ; to play matching note to piece
			cmp al,2h ; 1 scan code
			je check ; jump to a label that checks the correct/incorrect code
		setKey2:
			mov [currentKey],100
			mov [currentNote],0A97h
			cmp al,3h ; 2 scan code
			je check ; jump to a label that checks the correct/incorrect code
		setKey3:
			mov [currentKey],140
			mov [currentNote],0974h
			cmp al,4h ; 3 scan code
			je check ; jump to a label that checks the correct/incorrect code
		setKey4:
			mov [currentKey],180
			mov dx,[cNote]
			mov [currentNote],dx
			cmp al,5h ; 4 scan code
			je check ; jump to a label that checks the correct/incorrect code
		setKey5:
			mov [currentKey],220
      mov [currentNote],07F4h
			cmp al,6h ; 5 scan code
			je check ; jump to a label that checks the correct/incorrect code
		setKey6:
			mov [currentKey],260
      mov [currentNote],0712h
			cmp al,7h ; 6 scan code
			je check ; jump to a label that checks the correct/incorrect code

		midPrint: ; midjump to avoid out of range error
      jmp printScore ; print updated score
		
		check: ; a label that checks for the code and jump to correct piece of code accordingly
		cmp [switch],1 ; code for correct
		je correct
		cmp [switch],2 ; code for incorrect
		je incorrect
		jmp waitfordata

		correct:
			push 70 ; y coordinate of the topmost point of the key
			push [currentKey] ; x coordinate of the current key
			push 10 ; green color code
			call drawRect ; call procedure to draw key
			push [currentNote] ; push to play current note
			call sound ; call procedure to play sound
			inc si ; advance in array
			add di,2 ; advance in array
			cmp si,[endb] ; if reached the end...
			je finishh ;... jump to finish label
			add [score],5 ; add 5 to score according to rules
			mov [switch],0 ; reset switch
    jmp midPrint ; jump to update score

		incorrect:
			push 70 ; y coordinate of the topmost point of the key
			push [currentKey] ; x coordinate of the current key
			push 4 ; red color code
			call drawRect ; call procedure to draw key
			push [currentNote] ; push to play current note
			call sound ; call procedure to play sound
			inc si ; advance in array
			add di,2 ; advance in array
			cmp si,[endb] ; if reached the end...
			je finishh ;... jump to finish label
			sub [score],5 ; subtract 5 to score according to rules
			mov [switch],0 ; reset switch
			mov ah,02ch ; update current second         
			int 21h ; interrupt - get new time
			mov bh,dh ; Store current second   
			cmp [score],500 ; if score above 500, the player have lost (best score possible is 235)
			ja lost ; due to the fact that if falls bellow zero, a variable will be changed to the maximum integer value
		jmp midPrint ; jump to print updated score

		finishh: ; when finished the game
			call endSound ; stop sound
			mov ah, 0 ; move to text mode codes
			mov al, 2
			int 10h ; interrupt - move to text mode
			mov dx,offset endmsg ;set dl to notes
			mov ah,9h ; Set interrupt code to 9, write a string
			int 21h ; Interrupt - print char
			push [score] ; show score
			call print ; print score

			finishw: ; waiting for key press to change to desired screen
				mov ah,1 ; prepare for accepting input
				int 16h ; interrupt - get input
				jz lostw ; if not pressed, (zero flag equals zero) loop
				mov ah,0 ; reset ah
				int 16h ; interrupt - get input
				cmp ah,23h ; h scan code
				je final ; end procedure
				cmp ah,1h ; esc scan code
				je escape ; exit label
			jne finishw ; loop
		
		lost: ; when lost the game
			call endSound ; stop sound
			mov ah, 0 ; move to text mode codes
			mov al, 2
			int 10h ; interrupt - move to text mode
			mov dx,offset lostmsg ;set dl to lostmsg
			mov ah,9h ; Set interrupt code to 9, write a string
			int 21h ; Interrupt - print char

		lostw: ; waiting for key press to change to desired screen
			mov ah,1 ; prepare for accepting input
    	int 16h ; interrupt - get input
    	jz lostw ; if not pressed, (zero flag equals zero) loop
  		mov ah,0 ; reset ah
  		int 16h ; interrupt - get input
			cmp ah,23h ; h scan code
			je final ; end procedure
			cmp ah,1h ; esc scan code
			je escape ; exit label
			jne lostw ; loop

		final:
		call endSound ; stop sound
			mov ah, 0 ; move to text mode codes
    	mov al, 2
    	int 10h ; interrupt - move to text mode

		pops:
		mov [xParam],60 ; reset xParam value
		mov [score],0 ; reset score
		mov [endb],0 ; reset end array address
		pop si ; load the value of si
		pop di ; load the value of di
		pop dx ; load the value of dx
		pop cx ; load the value of cx
		pop bx ; load the value of bx
		pop ax ; load the value of ax
		pop bp ; load the value of bp
		ret 2 ; return to avoid stack overflow - 'clear stack'
	endp mainGame

	; MAIN SCREEN PROCEDURE

proc mainScreen
	push ax ; save value
	mov dx,offset msg ;set dl to notes
	mov ah,9h ; Set interrupt code to 9, write a string
	int 21h ; Interrupt - print char
	waitMain: ; waiting for key press to change to desired screen
		mov ah,1 ; prepare for accepting input
    int 16h ; interrupt - get input
  	jz waitMain ; if not pressed, (zero flag equals zero) loop
  	mov ah,0 ; reset ah
  	int 16h ; interrupt - get input
		cmp ah,1Eh ; A scan code
		je choosesong ; song choosing label
		cmp ah,1h ; escape scan code
		je escape ; escape label
		jne waitMain ; loop
	escape:
		jmp exit ; exit the program
	choosesong:
		mov dx,offset choose ;set dl to choose
		mov ah,9h ; Set interrupt code to 9, write a string
		int 21h ; Interrupt - print char
	play: ; waiting for key press to change to desired screen
		mov ah,1 ; prepare for accepting input
    int 16h ; interrupt - get input
    jz play ; if not pressed, (zero flag equals zero) loop
  	mov ah,0 ; reset ah
  	int 16h ; interrupt - get input
		cmp ah,8h ; 7 scan code
		je playOde ; playOde label that pushes corresponding variable and calls procedure
		cmp ah,9h ; 8 scan code
		je playTwinkle ; playTwinkle label that pushes corresponding variable and calls procedure
		cmp ah,1h ; esc scan code
		je exit ; exit the program
		jmp play ; loop
		
	playOde:
		push 1 ; code for ode to joy playing
		call mainGame ; call the main game
		jmp start ; after the game has ended, return to main block

	playTwinkle:
		push 2 ; code for twinkle twinkle little star playing
		call mainGame ; call the main game
		jmp start ; after the game has ended, return to main block
		pop ax ; load value
		ret ; return
endp mainScreen ; end procedure

start:
	mov ax,@data
	mov ds,ax

	mov ax,03h ; move to graphic mode code
	int 10h ; interrupt - switch to graphic mode
infinite: ; infinite loop for invoking mainScreen procedure
	call mainScreen ; call mainScreen to show main menu
	jmp infinite ; loop infinitely

exit:
	mov ax,03h
	int 10h
	
	mov ax,4c00h
	int 21h
END start
