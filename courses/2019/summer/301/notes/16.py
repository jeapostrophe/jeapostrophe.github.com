# SExprs
class Empty:
    def __init__(self):
        pass
    def show(self):
        return "MT"
    def ishow(self):
        return ""

class Cons:
    def __init__(self,left, right):
        self.l = left
        self.r = right
    def show(self):
        return "(" + self.l.show() + self.r.ishow() + ")"
    def ishow(self):
        return " " + self.l.show() + self.r.ishow()
        
class Atom:
    def __init__(self,value):
        self.v = value
    def show(self):
        return str(self.v)
    def ishow(self):
        return " . " + str(self.v)

# J0
class Num:
    def __init__(self,num):
        self.n = num
    def show(self):
        return str(self.n)
        
class Add:
    def __init__(self,left, right):
        self.l = left
        self.r = right
    def show(self):
        return "(+ " + self.l.show() + " " + self.r.show() + ")"

class Mul:
    def __init__(self,left, right):
        self.l = left
        self.r = right
    def show(self):
        return "(* " + self.l.show() + " " + self.r.show() + ")"

# desugar : SExpr -> J0
def desugar(se):
    if isinstance(se, Cons):
        if isinstance(se.l, Atom):
            # se = Cons (Atom(+), Cons( X, Cons( Y, Empty)))
            if se.l.v == "+" and isinstance(se.r.r.r,Empty):
                return Add( desugar(se.r.l), desugar(se.r.r.l) )
            if se.l.v == "+":
                return Add( desugar(se.r.l), desugar(Cons(se.l, se.r.r)) )
            if se.l.v == "*":
                return Mul( desugar(se.r.l), desugar(se.r.r.l) )
            if se.l.v == "-":
                return Mul( Num(-1), desugar(se.r.l) )
            else:
                raise "Bad!"
        else:
            raise "Bad!"
    elif isinstance(se, Atom):
        return Num( int(se.v) )

# Main
se = Cons(Atom("+"), Cons(Cons(Atom("+"), Cons(Atom("3"), Cons(Atom("4"), Cons(Atom("4"), Empty())))),
                                    Cons(Cons(Atom("+"), Cons(Cons(Atom("-"), Cons(Atom("3"),Empty())), Cons(Atom("4"), Empty()))), Empty())))
print se.show()
print desugar( se ).show()
