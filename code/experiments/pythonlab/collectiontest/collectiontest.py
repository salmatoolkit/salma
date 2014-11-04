import heapq
import random

h = []

heapq.heappush(h, (42, "fourty two"))
heapq.heappush(h, (12, "twelve"))

print(h)
while len(h) > 0 and h[0][0] > 10:
    e = heapq.heappop(h)
    print(e)

print(h)

count = 0
tcount = 0
fcount = 0
for i in range(0, 1000000):
    v = random.random()
    if v >= 0.5:
        tcount += 1
    else:
        fcount += 1
    count += 1

prob = tcount / count
print("count = {}, tcount = {}, fcount = {}, prob = {}".format(count, tcount, fcount, prob))
