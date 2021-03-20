TITLE  Final Project    (Proj6_gajardo.asm)

; Author: Sebastian Gajardo
; Last Modified: 3/8/21
; OSU email address: gajardos@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number: 6                Due Date: 3/14/21
; Description: Test program using macros and procedures to get 10 numbers, display them, their sum and average. Does this by calling the 
;			   ReadVal procedure which uses the mGetString macro to get the string from the user and the number of bytes read. Then 
;			   converts from character (ASCII) to signed integer form and saves the signed integer into memory, then fillarray is called 
;			   which inputs that integer into memory. This is looped 10 times until all 10 inputs are recieved and saved as signed integers
;			   Then the sum and average is calculated vis the GetAvg procedure which saves it into memory and finally the Writeval 
;			   procedure is used to transform those integers back to string format and calls the mDisplayString to display them. Then the
;			   sum and finally the average. Then displays parting message. Also displays program name and programmer along with 
;		       instructions prior to prompting user for initial signed integers that fit within 32 bits.



INCLUDE Irvine32.inc

; ****************************************************************
; Name: mGetString
; Gets a string from the user.
; Preconditions: Must be passed prompt, input and read by reference and count by value.
; Receives:
; prompt = String to prompt the user.
; input = Reference address to store user input.
; count = Max value of bytes to be read.
; read = Address for actual number of bytes read.
; returns: Actual number of bytes read and string inputed saved in input memory address.
; *****************************************************************
mGetString			MACRO prompt:REQ, input:REQ, count:REQ, read:REQ
	push	EDX														; Save used registers.
	push	ECX
	push	EAX

	mov		EDX, prompt
	call	WriteString
	mov		ECX, count												; value of max size of bytes to be read.
	mov		EDX, input												; Input reference address for string.
	call	ReadString
	MOV		ECX	, read												; Address reference read variable.
	mov		[ECX], EAX	

	pop		EAX														; Restore used registers.
	pop		ECX														
	pop		EDX
ENDM

; ****************************************************************
; Name: mDisplayString
; Displays received string, stored at specified memory location.
; Preconditions: Passed input by reference.
; Receives: Input memory reference of a string.
; returns: Displays passed string.
; *****************************************************************
mDisplayString		MACRO input:REQ
	push	EDX														; Save used registers.
	
	mov		EDX, input
	call	WriteString

	pop		EDX														; Restore used register.
ENDM


; (insert constant definitions here)

.data

intro			BYTE	"PROGGRAMMING ASSIGNMENT 6: Designing low level I/O procedures",13,10,
						"Written by: Sebastian Gajardo",13,10,13,10,0
instruction		BYTE	"Please provide 10 signed decimal integers. Each number needs to be small enough to fit inside a 32 bit register.",13,10, 
						"After you have finished inputting the raw numbers I will display a list of the integers, their sum, and their",13,10, 
						"average value.",13,10,13,10,0
prompt1			BYTE	"Please enter a signed number: ",0
prompt2			BYTE	"Please try again: ",0
input			BYTE	33 DUP(0)
error			BYTE	"ERROR: You did not enter a signed number or your number was too big.",13,10,0
count			DWORD	32
read			DWORD	?
numbers			SDWORD	10 DUP(?)										; Signed numbers to be saved.
sizeNums		SDWORD	LENGTHOF numbers
currentNum		SDWORD	?												; String number being converted
sign			SDWORD	?												; Sign of string number being converted.
sum				SDWORD	?
average			SDWORD	?
numPrompt		BYTE	"You entered the following numbers:",13,10,0
sumPrompt		BYTE	"The sum of these numbers is: ",0
avgPrompt		BYTE	"The average of these numbers is: ",0
currentString	BYTE	33 DUP(0)
newString		BYTE	33 DUP(0)
comma			BYTE	", ",0
farewell		BYTE	"Good Bye!",13,10,0

.code
main				PROC

; Introduction
	push	OFFSET intro
	push	OFFSET instruction
	call	introduction
	mov		ECX, 0

_GetValues:
; ReadVal and FillArray 10 times to get valid string number input and put them in array.
	mov		currentNum, 0
	mov		sign, 0
	push	OFFSET prompt2
	push	OFFSET currentNum
	push	OFFSET sign 
	push	OFFSET error
	push	OFFSET prompt1
	push	OFFSET input
	push	count
	push	OFFSET read
	call	ReadVal
; FillArray
	push	ECX															; Current number out of 10.
	push	OFFSET numbers
	push	currentNum													; CurrentNum value.
	call	FillArray
	INC		ECX
	CMP		ECX, 10
	JB		_GetValues
	call	Crlf

; GetAverage
	push	sizeNums
	push	OFFSET numbers
	push	OFFSET average
	push	OFFSET sum
	call	GetAverage

; WriteVal for 10 inputs.
    mov		EDX, OFFSET numPrompt
	call	WriteString
	mov		EAX, 0
	mov		ECX, 0
_DisplayNums:
	mov		ESI, OFFSET numbers
	push	OFFSET newString
	push	OFFSET currentString
	push	[ESI + EAX]
	call	WriteVal
	ADD		EAX, 4														; Displacement.
	INC		ECX
	CMP		ECX, 10
	JE		_Done
	mov		EDX, OFFSET comma
	call	WriteString
	JMP		_DisplayNums
_Done:
	call	Crlf
													
; WriteVal for sum.
	mov		EDX, OFFSET sumPrompt
	call	WriteString
	push	OFFSET newString
	push	OFFSET currentString
	push	sum
	call	WriteVal
	call	Crlf

; WriteVal for avg.
	mov		EDX, OFFSET avgPrompt
	call	WriteString
	push	OFFSET newString
	push	OFFSET currentString
	push	average
	call	WriteVal
	call	Crlf
	call	Crlf

; Goodbye
	push	OFFSET farewell
	call	GoodBye

	Invoke ExitProcess,0	; exit to operating system
main				ENDP

; ***************************************************************
; introduction
; Procedure to displays program title and programmers name.
; Preconditions: Intro and instruction must be strings.
; Postconditions: None.
; Receives: Intro and instruction passed by reference.
; Returns: None.
; ***************************************************************
introduction		PROC
	
	push	EBP
	mov		EBP, ESP												
	push	EDX
	mov		EDX, [EBP + 12]											; Intro address reference.
	call	WriteString
	mov		EDX, [EBP + 8]											; Instruction address reference.
	call	WriteString
	POP		EDX
	pop		EBP
	RET		8														
introduction		ENDP

; ***************************************************************
; ReadVal
; Procedure to read number string input by the use of the mGetString macro. Once the string number is read it converts from string
; to signed integer using string primitives by subtracting 48 from the ASCCI representation and then saves it in memory. If number is
; negative (sign = 1) it negates the number prior to saving it in memory (currentNum). Has input validation for numbers that don't fit 
; in 32 bits for both positive a negative cases, it also validates i it's a number vs another character or letter. When the input is 
; Invalid it displays an error message and re prompts.
; Preconditions: Prompt1, prompt2 and error are strings, mGetString is a macro, count is an SDWORD, sign is an integer initialized to 0, 
; read is a DWORD and input is an uninitialized string.
; Postconditions: None.
; Receives: Prompt1, prompt2, sign, error, read, currenNum and input passed by reference. Count passed by value.
; Returns: Input string number translated into integer format in currentNum.
; ***************************************************************
ReadVal				PROC

	push	EBP
	mov		EBP, ESP
	push	EDX															; Save used registers.													
	push	ECX														
	push	EBX
	push	EAX
	push	ESI
	push	EDI

_Input:														
; Macro used to get input string from the user.
	mGetString [EBP + 20], [EBP + 16], [EBP + 12], [EBP + 8]			; Prompt1, input and read references and count value.
_Skip:
	mov		ESI, [EBP + 8]											
	mov		ECX, [ESI]													; Size of array input.
	cmp		ECX, 0														; If no input, send to error.
	JE		_Error
	CLD																	; Clear direction flag.
	mov		ESI, [EBP + 16]												; Input string reference address.

	_Convert:
	; Convert string to signed integer.
		LODSB															
		mov		EBX, [EBP + 8]											
		CMP		ECX, [EBX]												; If first character check + or - also.
		JE		_Sign													
	_Numbers:
	; Validate character is a number within 48-57 ASCCI inclusive.
		CMP		AL, 48
		JB		_Error
		CMP		AL, 57
		JA		_Error
		SUB		AL, 48
		MOVSX	EAX, AL													; Extend to 32 bit.
		mov		EDI, [EBP + 32]											; Put current number reference in EDI
		mov		EBX, [EDI]
		imul	EBX, 10
		JO		_Error
		ADD		EBX, EAX
		JO		_Error													; If overflow, jump to error.
		mov		[EDI], EBX												; CurrentNum*10 + converted number.
		LOOP	_Convert
		; Change sign if necessary.
		mov		EBX, [EBP + 28]											; Sign reference address.
		mov 	EDX, [EBX]
		cmp		EDX, 0
		JE		_End
		mov		EDI, [EBP + 32]											; Currentnum 
		NEG		SDWORD PTR[EDI]											; Change to negative if sign != 0.
		JO		_Error													; If negation causes overflow go to error.
		JMP		_End

	_Sign:
	; If initial character check if it's + or - save sign and go to next character, if not check if it's number.
		mov		EBX, [EBP + 28]											; Sign reference address.
		CMP		AL, 43													; +.
		JE		_Positive										
		CMP		AL, 45													; -.
		JE		_Negative
		JMP		_Numbers
	_Positive:
	; If positive sign do nothing, just go to next character.
		DEC		ECX
		JMP		_Convert
	_Negative:
	; If negative change sign to 1 for negative.
		INC		SDWORD PTR[EBX]				
		DEC		ECX 
		JMP		_Convert

_Error:
; Error message displayed if user enters invalid number, non number characters or no input.
	mov		EDX, [EBP + 24]
	call	WriteString
	mGetString [EBP + 36], [EBP + 16], [EBP + 12], [EBP + 8]			; Prompt2, input and read references and count value.
	mov		EDX, [EBP + 32]
	mov		EBX, 0
	mov		[EDX], EBX													; Reset currenNum and sign to 0 if error occured.
	mov		EDX, [EBP + 28]
	mov		[EDX], EBX
	JMP		_Skip

_End:
	pop		EDI															; Restore used registers.
	pop		ESI
	pop		EAX
	pop		EBX
	pop		ECX
	pop		EDX
	pop		EBP
	RET		32
ReadVal			ENDP

; ***************************************************************
; FillArray
; Fills array with a signed integer at the passed index.
; Preconditions: currenNum is an integer, numbers is an array reference and index is an integer less than length numbers.
; Postconditions: None.
; Receives: CurrentNum reference, numbers referenc and index to insert currentNum at.
; Returns: Numbers array filled out at the index passed.
; ***************************************************************
FillArray			PROC
	
	push	EBP
	mov		EBP, ESP
	push	ESI															; Save used registers.
	push	EDI
	push	EAX

	mov		ESI, [EBP + 8]												; Current number value
	mov		EDI, [EBP + 12]												; Numbers array raference address.
	mov		EAX, [EBP + 16]												; Number out of 10 total loops.
	imul	EAX, 4		
	add		EDI, EAX
	mov		[EDI], ESI													; CurrentNum into array position.

	pop		EAX															; Restore used registers.
	pop		EDI
	pop		ESI
	pop		EBP
	RET		12
FillArray			ENDP


; ***************************************************************
; GetAverage
; Procedure gets sum and average of inputed array based on reference address and number of elements passed.
; Preconditions: Numbers array is filled with signed integers, average and sum are SDWORDS.
; Postconditions: None.
; Receives: Numbers array, sum, average  reference addresses and number of elements in numbers. 
; Returns: Average and sum of numbers array.
; ***************************************************************
GetAverage				PROC
	
	push	EBP
	mov		EBP, ESP
	push	ESI
	push	EDI
	push	ECX
	push	EBX

	mov		ESI, [EBP + 16]												; Numbers array address reference.
	mov		EDI, [EBP + 8]												; Sum address reference.
	mov		ECX, [EBP + 20]												; Size of array.
	mov		EBX, 0														; Start displacement at 0.
	mov		EAX, 0
_Loop:
; Goes through array to sum up all the values.
	MOV		EDX, [ESI]
	ADD		EAX, EDX
	ADD		ESI, 4
	LOOP	_Loop
; End of sum loop now get average and store both values.
	mov		[EDI], EAX													; Move sum to sum address reference.
	mov		EBX, [EBP + 12]												; Average address references.
	mov		ECX, [EBP + 20]												; Size of array.
	CDQ																	
	IDIV	ECX
	mov		[EBX], EAX													; Average into average address reference.
; Restore used registers.
	pop		EBX
	pop		ECX
	pop		EDI
	pop		ESI
	pop		EBP
	RET		16
GetAverage				ENDP

; ***************************************************************
; WriteVal
; Procedure converts numeric SDWORD value into string of ascii digits and calls mDisplayString to display it. Does this by continously 
; diving SDWORD input by 10, appending remainder to passed string address and continuing until result is 0. Then null terminates the 
; string and reverses it before using macro to display it.
; Preconditions: Passed number value is a signed integer. Two string addresses are passed,
; Postconditions: None.
; Receives: number value reference, currentString and newString address reference.
; Returns: Displays sum value.
; ***************************************************************
WriteVal			PROC
	LOCAL	result: SDWORD												; Local variable to hold temp result.
	
	push	ESI															; Save used registers.
	push	EBX
	push	ECX
	push	EAX
	push	EDX
	push	EDI

	mov		EAX, [EBP + 8]												; Sum reference by value.
	mov		EDI, [EBP + 12]												; currentString address reference.
	ADD		EAX, 0
	JNS		_Positive
	NEG		EAX
_Positive:
; Skips turning to positive if positive already.
	mov		result, EAX
	CLD																
_Convert:
; Divide by 10 continously until result is 0, insert result to currentString reference.
	mov		EAX, result
	mov		EBX, 10
	CDQ		
	IDIV	EBX
	mov		result, EAX
	ADD		EDX, 48														; + 48 to convert to ASCII.
	mov		AL, DL														; remainder into AL.
	STOSB																; Copy AL to [EDI] and increase by 1 byte.
	CMP		result, 0
	JE		_Sign														; Jump to sign check once result 0.
	JMP		_Convert

_Sign:
; If input number negative, append negative sign at end of currentString.
	mov		EBX, [EBP + 8]
	ADD		EBX, 0
	JNS		_Reverse
	mov		AL, 45
	STOSB			

_Reverse:
; Prepare registers for reverse.
	mov		ECX, EDI
	SUB		EDI, 1
	mov		ESI, EDI													; Read from end of currentString.
	mov		EDI, [EBP + 16]												; Into begining of newString.
	SUB		ECX, [EBP + 12]												; Get size of string into counter.
_RevLoop:
; Reverses currentString into newString.
	STD																	; Set Df.
	LODSB
	CLD																	; Clear df.
	STOSB
	LOOP	_RevLoop
	mov		AL, 0														; Null terminate reversed string.
	STOSB
; Use macro to display newString.
	mDisplayString [EBP + 16]											; Current string reference by address.

; Restore used registers
	pop		EDI								
	pop		EDX
	pop		EAX
	pop		ECX
	pop		EBX
	pop		ESI
	RET		12
WriteVal			ENDP

; ***************************************************************
; goodBye
; Procedure displays goodbye message.
; Preconditions: goodBye message is a string.
; Postconditions: None.
; Receives: goodBye message reference via the stack.
; Returns: None.
; ***************************************************************
GoodBye	PROC
	
	push	EBP
	mov		EBP, ESP
	push	EDX
	mov		EDX, [EBP + 8]												; goodBye string reference.
	call	WriteString
	pop		EDX
	pop		EBP
	RET		4														
goodBye	ENDP

END main
