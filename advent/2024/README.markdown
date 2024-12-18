# Advent of code 2024

notes

I usually use my CIEL package, to get libraries out of the box, and to
run the .lisp files as a script from the terminal (not that I do it,
just to demo).

Other goals:

- try libraries this year. Ideas:
  - parseq for PEG grammars
  - FSet
  - Coalton


### day 01

remind that gethash gets a default value. `dolist` and `&aux` is concise (reddit).

### day 02

not brute force, at most 3 more attemps per item. A loop-y solution and a functional-y one inspired by others.

### day 03: using a PEG grammar

https://github.com/ravi-delia/uclp

- not obvious how to capture *all* matches and ignore any character. Got a segfault.
- clever one: https://www.reddit.com/r/adventofcode/comments/1h5frsp/2024_day_3_solutions/m06o6ms/
- I also tried the `parseq` library, see day03-parseq.lisp (with credit inline), and it was easier. I liked it.
- could also use `str:match`


### day 04 - a map of characters

represent the grid as a hash-table: keys are the coordinates as one object.

- atgreen: https://www.reddit.com/r/adventofcode/comments/1h689qf/2024_day_4_solutions/m0ejih1/ building a hash-table with keys as complex numbers (or list or else) with (i j) and values the characters.
- lispm: https://www.reddit.com/r/Common_Lisp/comments/1h7ib2v/advent_of_code_2024_day_4_in_common_lisp_lispworks/

### day 05


### day 06 - moving in a map

I only did part 1, didn't feel like burning CPU and brain cycles for a
non-clever brute force solution.

(I mean, even trying the *legitimate* positions on the guard's path is stupid enough lol)

OK, I tried a bit, but I like my recursion solution for part 1 and I don't see how to keep it for part 2. It would exhaust the stack / I don't see how to make it tail call. Time to sleep.

This year: didn't use arrays for the grid but a hash-table with positions -> `(dict :char character :guard nil :visited nil)` (some useless).

My part 1 doesn't use `loop` o/ (recursion)

others:

- https://github.com/ak-coram/advent/blob/main/2024/06.lisp
- [topaz](https://topaz.github.io/paste/#XQAAAQC+DwAAAAAAAAAUGQimgx+p6OIwQNpuWOKVFo+AdfvjN2jklHiFmk2KTRBc9YTJjNXnYRyfNsLNWyrnGUBpyYZvCr5JClW/2r0kt/FZ0EjFc7qJblmV+b5ENyVT0gIZ8q6gm1OoQmfm9wmsUtHx0A2Pe7cSwSu1pgEhJ1LSsNmwT/q3g5ngmvPYE7NtJ+DwWK8pF2Sg9MqTVMRzBEGn9aatd0grHt78r+8DfFrtPdRKvh+MxS3pBNDAkVSlRP/8setnPaa8Un+SfhyhpVpwPYdUgA9lhJrixokD9Dl1x2z63nMZPs/uZgiTy99VhlY5lVh8BeNv8FVKIxmSFoXhtOF9v4JvdCNmHs/Tjpm8Hj4BACWkYC+nr3+XeOCByn1Nrb1brJOUsZoGspWzRcMe4rkKBzQL32UGSlxFlttjo2p4Xk/52OaKYkvMFO71wU+DP9vAKUzclN13iMTOfvU/NOsH8AkI0AzI5/UDGaXiiuTIS2lTW+R3vz01T/YtyRlOwoOLXzoM+uAXLRAK1mUd1mqy7/dQfzQbU8Ysz4Ff8vTDBhGtDbrUJx/8ULZD79+y1rIizsgT4JEDy1S0GK17d5lRmtG08PocI4Bluc+Kc0TTxuJCxBOiWW+nIXcIHyuR0jA7ls1hXQ0OjzbjyfFYBzmLc8zXfFFR/yQj2cvvVVwksNogJJJ6mBqgGKdgTKlIAS0u36OTBuHyN7t4WLOks7mIiciNxl6EFbnw4NiCR17DCbSajRyDXXQBCfMfOFekCPTVLVF+cUmPnsD7ZXNRX/88K8o8BN/zBxwlJJAvGVv5acnXo+H+JLv7mE/tuRcKtidVg1Vf9tn3TreJVflZeI9VmVWyvBwI1EAH2PwRqKaO9aIUEjs5mUifFEZLqrpds8gNLBXz5P2+/u5o+2EibeqEVNa73I0MCBXGy8sHnI1KG94oqqMzb0nfFtPQVlGS4EeR6XPolzkKQeTP2+t2F64fIOShG0a8unD4qxWr78cL0k2BAs9KFzu6gMYDjzvdq59yvMYweWlBWtUjGvvbUgHPwhNctr1mjiyib34qmDqHTwP1VpXwaPzT1XYJygE9aA2rEHWT83b3VAWFahVRW+s+0p191xv3JMtSpG2iW/vaExCD/HEY0ELjL0gKo5Mq294o8rjUvHX4D4eRIr6tRi6aVRg6QXFZ4kL1ADK3N1ptWgsqU7iQOwFBaYb8yB5OR48oM0lPvMrIvmSCZqSfJHQ9k9HZJzo1OcWlAf+aVgkzQLp+CG0tIVvtKaMCN59HXcj2IqM/VEx/vDbgccPpnAC0EkwDBH6D5v1WiKLF529oOaZbi7k472Z/feuqBqBlFwwvckF+LU1JzwOW/Y+l+HtaZU7Rhqj4hcDRl0HVH8JsUBK8MU+wSipf//FpiKA=)
- https://old.reddit.com/r/adventofcode/comments/1h7tovg/2024_day_6_solutions/m0rouxb/

### day 07 - calculation machine, permutations

Let's *not* compute all the permutations (alexandria:map-permutations isn't enough, we have too few operators for the number of numbers.

We recurse, try all operators, and stop as early as possible.

- [reddit day07](https://www.reddit.com/r/adventofcode/comments/1h8l3z5/2024_day_7_solutions/)
- with the Screamer library: [bo-tato on reddit](https://www.reddit.com/r/Common_Lisp/comments/1h94tfe/advent_of_code_2024_day_7_with_screamer_non/)


### day 08 - positions on a map, permutations

This time, a perfect use of alexandria's `map-permutations`.

```
  Documentation:
    Calls function with each permutation of LENGTH constructable
    from the subsequence of SEQUENCE delimited by START and END. START
    defaults to 0, END to length of the sequence, and LENGTH to the
    length of the delimited subsequence.
```

So IIUC, it doesn't *cons*.

https://dev.to/vindarel/advent-of-code-alexandrias-map-permutations-was-perfect-for-day-08-common-lisp-tip-16il

### day 09 - disk defragmenter

### day 10 - finding mountain paths

a map, a simple algo to explore and walk a grid.


### day 11 - exponentially multiplicating stones - recursion, memoization

Using fare-memoization simplified my reasoning and solution by
far. Then it was a regular recursive count.

### day 12 - maps, areas, sides

My part 02 isn't efficient (easy fix ups).


- https://www.reddit.com/r/adventofcode/comments/1hcdnk0/2024_day_12_solutions/

### day 13 - 2 equations with 2 unknows - Cramer - with infix math - very large numbers

Using cmu-infix for infix math. (no dashes in variable names)

Very large numbers -> use double floats.


### day 14 - moving robots in a grid - recognizing a pattern

part 1 is simple. We didn't use hash-tables but CLOS objects with defclass-std to use their defclass/std and define-print-object macros.

The grid is pretty printed with an array like so

```
#2A((1 0 1 2 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 1 1 0 1 1)
    (1 0 1 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 1 0)
    (0 0 0 0 0 0 0 1 0 0 0))
```

but just invert the x and y positions.



others

- https://old.reddit.com/r/adventofcode/comments/1hdvhvu/2024_day_14_solutions/
- https://www.reddit.com/r/adventofcode/comments/1hegacd/day_14_part_2_common_lisp_human_visual/ - recognizing a trunk, asking if it's a tree.


## Others

- https://github.com/ak-coram/advent

```lisp
(ppcre:do-register-groups ((#'parse-integer x y))
          ("(\\d+)\\|(\\d+)" rule-input)
```

- https://buffer.thebitmage.com/sitemap?stack=%2F20241202121706-advent_of_code_2024.html
