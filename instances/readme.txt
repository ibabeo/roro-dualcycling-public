Deck layout ---> Fore & Aft (row 1:end), Port & Starboard (col 1:end)

Slots are represented by row and column "rc", 0 means no slot on that location in the table

Each sheet represents a layout.

There are four rules determining the precedence matrix
1.(m) if no clearance is needed from the side (only the front slot is blocking)
2.(mp) if threstle lock is located on the port side of trailer (the front and one side is blocking)
3.(ms) if threstle lock is located on the starboard side
4.(mps) if both sides needs clearance 

To be more specific

For each slot (r,c), its precedences 
	Unloading			Loading
1.	(r+1,c)				(r-1,c)
2.	(r+1,c),(r+1,c-1)		(r-1,c),(r-1,c+1)
		or (r+1,c+1)			or (r-1)(c-1)
3.	(r+1,c),(r+1,c+1)		(r-1,c),(r-1,c-1)	
		or (r+1,c-1)			or (r-1,c+1)
4.	(r+1,c),(r+1,c-1)		(r-1,c),(r-1,c+1) 
		and/or (r+1,c+1)		and/or (r-1,c-1)



Layout1-4: 13*7 = 91 (including 0)
layout6-10: 15*9 = 135 (including 0)
nr. of trailers: 79, 70, 67, 67, 113, 93, 89, 97, 95, 87


*note, no precedence within a row