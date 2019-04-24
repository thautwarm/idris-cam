"""
string structure:
    (ch, tail)
empty string: ()
"""

def make_funcs_from_session(link_session):
    tp = tuple
    list_pop = list.pop

    HVECT_NIL = link_session["Data.HVect.Nil"]
    HVECT_CONS = link_session["Data.HVect.::"]

    VECT_NIL = link_session["Data.Vect.Nil"]
    VECT_CONS = link_session["Data.Vect.::"]

    LIST_NIL = link_session["Prelude.List.Nil"]
    LIST_CONS = link_session["Prelude.List.::"]


    def str_index(x, i):
        while i is not 0:
            x = x[1]
        return x[0]

    def str_lt(x, y):
        while x is not () and y is not ():
            a, b = x[0], y[0]
            if a < b:
                return True
            elif a > b:
                return False
            x = x[1]
            y = y[1]
        return y is not ()

    def str_len(x):
        n = 0
        while x is not ():
            n += 1
            x = x[1]

    def str_head(x):
        return x[0]

    def str_tail(x):
        return x[1]

    def str_cons(x, y):
        return (x, y)

    def str_concat(x, y):
        if x is ():
            return y
        stack = []
        append = stack.append
        while x is not ():
            append(x[0])
            x = x[1]
        pop = list_pop
        ret = y
        while stack:
            ret = (pop(stack), ret)
        return ret

    def to_text(xs):
        def f(xs):
            while xs is not ():
                yield xs[0]
                xs = xs[1]
        return ''.join(f(xs))

    def from_text(s):
        ret = ()
        for e in reversed(s):
            ret = (e, ret)
        return ret

    def read_all_text(f):
        return f.read()

    def reverse_flist(lst):
        return lst[::-1]

    def from_flist(lst):
        x = LIST_NIL
        for elt in lst:
            x = (LIST_CONS, elt, x)
        return x

    def to_flist(xs):
        def f(xs):
            while xs is not LIST_NIL:
                yield xs[1]
                xs = xs[2]
        return tp(f(xs))

    def reverse_fvect(vec):
        return vec[::-1]

    def from_fvect(n, lst):
        x = VECT_NIL
        for elt in lst:
            x = (VECT_CONS, elt, x)
        return x

    def to_fvect(n, xs):
        def f(xs):
            while xs is not VECT_NIL:
                yield xs[1]
                xs = xs[2]
        return tp(f(xs))

    def reverse_fhvect(vec):
        return vec[::-1]

    def from_fhvect(n, lst):
        x = HVECT_NIL
        for elt in lst:
            x = (HVECT_CONS, elt, x)
        return x

    def to_fhvect(n, xs):
        def f(xs):
            while xs is not HVECT_NIL:
                yield xs[1]
                xs = xs[2]
        return tp(f(xs))

    def get_module(m):
        m = to_text(m)
        return __import__(m, m.split(".")[0])

    return locals()