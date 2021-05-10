May 9, 2021.

Package tensorsign is combined into TensorComplete, which is independent of rTensor package. 

Detailed changes are

1. Imported ``tensorregress'' for some functions in ``rTensor''.
2. Added cp function and related functions in ``rTensor''.
3. Change functions names (signT -> fit_nonparaT, Alt -> Altopt, fit_continuous -> fit_continuous_cp, likelihood -> SEL) for possible conflicts and consistency with ``tensorordinal''.
3. Combined matrix- and tensor-versioned functions into fit_nonparaT and Altopt functions
4. Updated and kept ``tensorsign'' version for future anonymous submission.
