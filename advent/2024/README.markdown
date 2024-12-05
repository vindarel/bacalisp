# Advent of code 2024

notes

I usually use my CIEL package, to get libraries out of the box, and to
run the .lisp files as a script from the terminal (not that I do it,
just to demo).

### day 01

remind that gethash gets a default value. `dolist` and `&aux` is concise (reddit).

### day 02

not brute force, at most 3 more attemps per item. A loop-y solution and a functional-y one inspired by others.

### day 03: using a PEG grammar

https://github.com/ravi-delia/uclp

- not obvious how to capture *all* matches and ignore any character. Got a segfault.
- clever one: https://www.reddit.com/r/adventofcode/comments/1h5frsp/2024_day_3_solutions/m06o6ms/
- I also tried the `parseq` library, see day03-parseq.lisp (with credit inline)
- could also use `str:match`

### day 04 - a map of characters


- atgreen: https://www.reddit.com/r/adventofcode/comments/1h689qf/2024_day_4_solutions/m0ejih1/ building a hash-table with keys as complex numbers (or list or else) with (i j) and values the characters.

## Others
