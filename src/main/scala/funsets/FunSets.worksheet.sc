var bound = 1000

def exists(s: Int => Boolean, p: Int => Boolean): Boolean = 
    def iter(a: Int): Boolean =
        if (s(a) && p(a)) then
            true
        else if a > bound then
            false
        else
            iter(a+1)
    iter(-bound)

exists((x: Int) => x%100==0&&x!=0, (x: Int) => x%11==0)