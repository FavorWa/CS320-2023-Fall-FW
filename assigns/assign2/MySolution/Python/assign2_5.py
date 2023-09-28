class List(list):
    def __init__(self, *args):
        super().__init__(*args)


def fnlist_reverse(mylist):
    return List(reversed(mylist))


def fnlist_make_fwork(fwork):
    arr = []

    def work(x0):
        arr.append(x0)

    fwork(work)
    result = List(arr)
    result.cons1 = arr[0] if arr else None
    return result
