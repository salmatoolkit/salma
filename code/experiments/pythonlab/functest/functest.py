def create_adder(addend):
    def newadder(augend):

        return augend + addend

    return newadder


a = 0


def increment_test(start):
    global a
    a = start

    def my_inc():
        global a
        a += 1

    my_inc()
    my_inc()

    print(a)

add3 = create_adder(3)

print(add3(4))

increment_test(2)

