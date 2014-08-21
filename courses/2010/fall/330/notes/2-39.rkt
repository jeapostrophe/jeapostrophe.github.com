#lang planet jaymccarthy/parenlog

(advisor shriram jay)
(? (advisor shriram jay))

(? (advisor shriram arjun))
(advisor shriram arjun)
(? (advisor shriram arjun))

(? (advisor shriram X))

(:- (advisor X Y)
    (advisee Y X))

(advisee shriram matthias)

(? (advisor matthias shriram))

(:- (descendent X Y)
    (advisor Y X))
(:- (descendent X Z)
    (advisor Y X)
    (descendent Y Z))

(? (descendent jay shriram))
(? (descendent jay matthias))

(advisor matthias mflatt)
(advisor mflatt tewk)
(advisor jay neil)
(advisor harper tom7)

(? (descendent X matthias))

;;;; 

(type-of Gamma number num)
(:- (type-of Gamma (+ E1 E2) num)
    (type-of Gamma E1 num)
    (type-of Gamma E2 num))

(? (type-of mt (+ number number) T))
(? (type-of sam (+ number number) T))

(type-of Gamma boolean bool)
(:- (type-of Gamma (if Eb Et Ef) T)
    (type-of Gamma Eb bool)
    (type-of Gamma Et T)
    (type-of Gamma Ef T))

(? (type-of mt (if boolean 
                   (+ number number)
                   number)
            T))
(? (type-of mt (if boolean 
                   (+ number number)
                   boolean)
            T))

(:- (type-of Gamma (Ef Ea) T)
    (type-of Gamma Ef (-> I T))
    (type-of Gamma Ea I))
(:- (type-of Gamma (fun X E) (-> I O))
    (type-of (extend Gamma X I) E O))
(:- (type-of Gamma Var T)
    (in Gamma Var T))

(in (extend Gamma Var T) Var T)
(:- (in (extend Gamma Var0 T0) Var1 T1)
    (=/= Var0 Var1)
    (in Gamma Var1 T1))

(? (type-of mt (fun x (+ x x)) T))
(? (type-of mt ((fun x (+ x x)) number) T))
(? (type-of mt ((fun x (+ x x)) boolean) T))
    
(? (type-of mt (fun x number) T))
(? (type-of mt (fun x x) T))
(? (type-of mt ((fun x x) number) T))

(? (type-of mt (fun f (+ (f number) (f boolean))) T))
(? (type-of mt ((fun f (+ (f number) (f boolean)))
                (fun x number))
            T))

#|
Welcome to DrRacket, version 5.0.99.3--2010-11-29(45992ea/g) [3m].
Language: planet jaymccarthy/parenlog; memory limit: 256 MB.
yes
yes
yes
X=jay
yes
yes
yes
X=shriram
T=num
T=num
T=num
no
T=(-> num num)
T=num
no
T=(-> I77513 num)
T=(-> O77549 O77549)
T=num
no
no
> (? (type-of mt P num))
no
> (type-of mt P num)
P=number
> next
P=(+ number number)
> next
P=(+ number (+ number number))
> next
P=(+ number (+ number (+ number number)))
> next
P=(+ number (+ number (+ number (+ number number))))
> next
P=(+ number (+ number (+ number (+ number (+ number number)))))
> next
P=(+ number (+ number (+ number (+ number (+ number (+ number number))))))
> next
P=(+ number (+ number (+ number (+ number (+ number (+ number (+ number number)))))))
> next
P=(+ number (+ number (+ number (+ number (+ number (+ number (+ number (+ number number))))))))
> (type-of Gamma x num)
Gamma=(extend Gamma80392 x num)
> next
no
> (type-of Gamma P T)
T=num
P=number
> next
T=num
P=(+ number number)
> next
T=num
P=(+ number (+ number number))
> next
T=num
P=(+ number (+ number (+ number number)))
> 
|#