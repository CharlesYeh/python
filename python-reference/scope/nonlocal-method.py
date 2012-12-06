def f(x):
    class c:
        def inc(self):
            nonlocal x
            x += 1
            return x
        def dec(self):
            nonlocal x
            x -= 1
            return x
    return c()
c = f(0)
print("HI")
___assertEqual(c.inc(), 1)
print("HI")
___assertEqual(c.inc(), 2)
print("HI")
___assertEqual(c.dec(), 1)
print("HI")
___assertEqual(c.dec(), 0)
