def int1_foreach(n0, work_func):
    i0 = 0
    while (i0 < n0):
        work_func(i0)
        i0 += 1
    return


def string_merge(cs1, cs2):
    n1, n2 = len(cs1), len(cs2)

    result = []
    i1 = 0
    i2 = 0

    def merge_characters():
        nonlocal i1, i2

        if i1 < n1 and i2 < n2:
            c1 = cs1[i1]
            c2 = cs2[i2]

            if c1 <= c2:
                result.append(c1)
                i1 += 1
            else:
                result.append(c2)
                i2 += 1
        elif i1 < n1:
            int1_foreach(n1 - i1, lambda i: result.append(cs1[i1 + i]))
            i1 = n1
        else:
            int1_foreach(n2 - i2, lambda i: result.append(cs2[i2 + i]))
            i2 = n2

    while i1 < n1 or i2 < n2:
        merge_characters()

    return ''.join(result)
