Test to investigate if calling an actor for the information we want is faster than calling a class for the same information.

We will use the GRU class as the example:

When running with 10000 GRUs:
- Actor time = 1.4335s
- Class time = 0.2429s

When running with 100000 GRUs:
- Actor time = 14.7135s
- Class time = 2.074549s
