LOOP:
    ;; Ai = M[A++]
    LODD A:                     ; AC <- A
    SWAP                        ; SP <- A, AC = SP
    STOD SPi:                   ; SPi <- SP
    LODL 0                      ; AC <- A[i]
    STOD Ai:                    ; Ai <- A[i]
    LODD SPi:                   ; AC <- SP
    SWAP
    LOCO 1
    ADDD A:
    STOD A:

    ;; Bi = M[B++]
    LODD B:                     
    SWAP                        
    STOD SPi:                   
    LODL 0                      
    STOD Bi:                    
    LODD SPi:                   
    SWAP
    LOCO 1
    ADDD B:
    STOD B:

    ;; PUSH (Ai+Bi)
    LODD Ai:
    ADDD Bi:
    PUSH

    LODD N:
    SUBD One:
    STOD N:

    JNZE LOOP:
    
    HALT
One:    1
SPi:    0
Ai: 0
Bi: 0
A: ADATA:
B: BDATA:
N:  10
ADATA:
    0 2 4 6 8 10 12 14 16 18
BDATA:
    1 3 5 7 9 11 13 15 17 19
