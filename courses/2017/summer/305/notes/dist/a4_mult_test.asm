; test for the new MULT assembly instruction
start:
	LODD d1:
	PUSH
	MULT 0
	HALT	     ; AC has status, product on stack
    INSP 1
    LODD d2:
    PUSH
    MULT 37
    HALT         ; AC has status, product on stack
    INSP 1
    LODD d3:
    PUSH
    MULT 63
    HALT         ; AC has status, product on stack
    INSP 1
    LODD d4:
    PUSH
    MULT 16
    HALT         ; AC has status, product on stack
    INSP 1
    LODD d5:
    PUSH
    MULT 49
    HALT         ; AC has status, product on stack
    INSP 1
    LODD d6:
    PUSH
    MULT 55
    HALT         ; AC has status, product on stack
    INSP 1
.LOC 30
d1:	 2542
d2:	 -362
d3:	  520
d4:	-2048
d5:	  715
d6:	   -1

