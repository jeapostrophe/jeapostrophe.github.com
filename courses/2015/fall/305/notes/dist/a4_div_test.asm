; test for the new DIV assembly instruction
start:  LODD div1:
        PUSH
	LODD dnd1:
	PUSH
	DIV         ; AC has status, quotient and remainder on stack
	HALT
	LODD div2:
        PUSH
        LODD dnd2:
        PUSH
        DIV         ; AC has status, quotient and remainder on stack
        HALT
	LODD div3:
        PUSH
        LODD dnd3:
        PUSH
        DIV         ; AC has status, quotient and remainder on stack
        HALT
        LODD div4:
        PUSH
        LODD dnd4:
        PUSH
        DIV         ; AC has status, quotient and remainder on stack
        HALT
        LODD div5:
        PUSH
        LODD dnd5:
        PUSH
        DIV         ; AC has status, quotient and remainder on stack
        HALT
        LODD div6:
        PUSH
        LODD dnd6:
        PUSH
        DIV         ; AC has status, quotient and remainder on stack
        HALT
	.LOC 40
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
