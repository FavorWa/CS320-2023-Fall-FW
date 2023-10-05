class fnlist:
    ctag = -1

    def get_ctag(self):
        return self.ctag

class MyNil(fnlist):
    ctag = 0

def mylist_nil():
    return MyNil()


class MyCons(fnlist):
    ctag = 1

    def __init__(self, x1, xs):
        self.x1 = x1
        self.xs = xs


def mylist_cons(x1, xs):
    return MyCons(x1, xs)


class MySnoc(fnlist):
    ctag = 2
    def __init__(self, xs, x1):
        self.xs = xs
        self.x1 = x1


def mylist_snoc(xs, x1):
    return MySnoc(xs, x1)


class MyReverse(fnlist):
    ctag = 3

    def __init__(self, xs):
        self.xs = xs


def mylist_reverse(xs):
    return MyReverse(xs)


class MyAppend2(fnlist):
    ctag = 4
    def __init__(self, xs1, xs2):
        self.xs1 = xs1
        self.xs2 = xs2


def mylist_append2(xs1, xs2):
    return MyAppend2(xs1, xs2)

def mylist_rforeach(xs, work):
    if isinstance(xs, MyNil):
        return
    
    elif isinstance(xs, MyCons):
        mylist_rforeach(xs.xs, work)
        work(xs.x1)

    elif isinstance(xs, MySnoc):
        work(xs.x1)
        mylist_rforeach(xs.xs, work)

    elif isinstance(xs, MyReverse):
        mylist_foreach(xs.xs, work)

    elif isinstance(xs, MyAppend2):
        mylist_rforeach(xs.xs2, work)
        mylist_rforeach(xs.xs1, work)


def mylist_foreach(xs, work):
    if isinstance(xs, MyNil):
        return
    
    elif isinstance(xs, MyCons):
        work(xs.x1)
        mylist_foreach(xs.xs, work)

    elif isinstance(xs, MySnoc):
        mylist_foreach(xs.xs, work)
        work(xs.x1)

    elif isinstance(xs, MyReverse):
        mylist_rforeach(xs.xs, work)

    elif isinstance(xs, MyAppend2):
        mylist_foreach(xs.xs1, work)
        mylist_foreach(xs.xs2, work)



