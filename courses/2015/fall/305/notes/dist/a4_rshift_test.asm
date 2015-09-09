; test for the new RSHIFT assembly instruction
start:  LODD d1:
	RSHIFT 0
	HALT         ; AC has shifted value
	LODD d2:
        RSHIFT 3
        HALT         ; AC has shifted value
        LODD d3:
        RSHIFT 7
        HALT         ; AC has shifted value
        LODD d4:
        RSHIFT 15
        HALT         ; AC has shifted value
        LODD d5:
        RSHIFT 4
        HALT         ; AC has shifted value
	.LOC 20
d1:       3451
d2:       8192
d3:      -8192
d4:     -32768
d5:        944

