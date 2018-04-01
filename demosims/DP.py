"""
This little program simulates a dot product
between two randomly generated vectors
"""
import numpy as np

# Set up the data
size = 256
aIn = np.random.randint(0, size, size=size)
bIn = np.random.randint(0, size, size=size)

# Perform DP
accum = 0
for a, b in zip(aIn, bIn):
    accum += a * b

# Check if the results match
gold = np.dot(aIn, bIn)
cksum = gold == accum
print("PASS: ", cksum, "(DotProduct)")
