"""
This little program simulates performing a dot product
between two randomly generated vectors
"""
import numpy as np

VECSIZE = 32
AIN = np.random.randint(0, VECSIZE, size=VECSIZE)
BIN = np.random.randint(0, VECSIZE, size=VECSIZE)

accum = 0
for a, b in zip(AIN, BIN):
    accum += a * b

gold = np.dot(AIN, BIN)
cksum = gold == accum
print("PASS: ", cksum, "(DotProduct)")
