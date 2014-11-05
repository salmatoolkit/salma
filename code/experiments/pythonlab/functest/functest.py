def create_adder(addend):
    def newadder(augend):

        return augend + addend

    return newadder



add3 = create_adder(3)

print(add3(4))



