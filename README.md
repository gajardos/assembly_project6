# assembly_project6
Description: Test program using macros and procedures to get 10 numbers, display them, their sum and average. Does this by calling the 
;			   ReadVal procedure which uses the mGetString macro to get the string from the user and the number of bytes read. Then 
;			   converts from character (ASCII) to signed integer form and saves the signed integer into memory, then fillarray is called 
;			   which inputs that integer into memory. This is looped 10 times until all 10 inputs are recieved and saved as signed integers
;			   Then the sum and average is calculated vis the GetAvg procedure which saves it into memory and finally the Writeval 
;			   procedure is used to transform those integers back to string format and calls the mDisplayString to display them. Then the
;			   sum and finally the average. Then displays parting message. Also displays program name and programmer along with 
;		       instructions prior to prompting user for initial signed integers that fit within 32 bits.
