dirs = "North East South West".split()
def right(dir, turns=1):
    nextIndex = dirs.index(dir) + turns
    return dirs[nextIndex % len(dirs)]

def isRightOf(origin, target): 
    return right(origin) == target

def isOppositeOf(origin, target):
    return right(origin, 2) == target

def isLeftOf(origin, target):
    return right(origin, 3) == target

def yieldRightOfWay(origin, target, approaching):
    if isOppositeOf(origin, target) and isRightOf(approaching, target):
        return True
    return isLeftOf(target, origin) and not isLeftOf(approaching, target)


in_dirs = input().split()

assert len(in_dirs) == 3
for x in in_dirs:
    assert x in dirs

a, b, c = in_dirs
print("Yes" if yieldRightOfWay(a, b, c) else "No")
