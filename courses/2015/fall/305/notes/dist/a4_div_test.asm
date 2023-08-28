; test for the new DIV assembly instruction
start:
    LODD div1:
    PUSH
	LODD dnd1:
	PUSH        ; dnd1 / div1
	DIV         ; AC has status, quotient and remainder on stack
	HALT
    INSP 2
	LODD div2:
    PUSH
    LODD dnd2:
    PUSH        ; dnd2 / div2
    DIV         ; AC has status, quotient and remainder on stack
    HALT
    INSP 2
	LODD div3:
    PUSH
    LODD dnd3:
    PUSH        ; dnd3 / div3
    DIV         ; AC has status, quotient and remainder on stack
    HALT
    INSP 2
    LODD div4:
    PUSH
    LODD dnd4:
    PUSH        ; dnd4 / div4
    DIV         ; AC has status, quotient and remainder on stack
    HALT
    INSP 2
    LODD div5:
    PUSH
    LODD dnd5:
    PUSH        ; dnd5 / div5
    DIV         ; AC has status, quotient and remainder on stack
    HALT
    INSP 2
    LODD div6:
    PUSH
    LODD dnd6:
    PUSH        ; dnd6 / div6
    DIV         ; AC has status, quotient and remainder on stack
    HALT
    INSP 2
    LODD div7:
    PUSH
    LODD dnd7:
    PUSH        ; dnd7 / div7
    DIV         ; AC has status, quotient and remainder on stack
    HALT
    INSP 2
.LOC 50
div1:	  -5152
dnd1:	   4943
div2:	      0
dnd2:       437
div3:       -16
dnd3:     -8199
div4:	   -256
dnd4:	  24575
div5:	    511
dnd5:         0
div6:	     17
dnd6:	   8191
div7:        16
dnd7:     -8199
