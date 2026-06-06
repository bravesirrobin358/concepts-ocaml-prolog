x = 2
def mo():
    x = 3
    def gx():
        return x
    return gx()

print(mo())