
Practice!

https://lisp-journey.gitlab.io/blog/practice-for-advent-of-code-in-common-lisp/


# Others

- https://github.com/ak-coram/advent/blob/main/2025/
- https://github.com/fosskers/aoc-2025 (author of [parcom](https://github.com/fosskers/parcom/) etc)
- https://git.sr.ht/~q3cpma/aoc2025/tree/master/item/01
- https://github.com/Ytrog/adventofcode (first year ever)
- https://nest.pijul.org/quickdudley/adventofcode:main/57A7YJ6RIIMFS.CAAAA
- https://github.com/wsgac/advent-of-code-2024/blob/master/2025/

## day 03

- https://www.reddit.com/r/adventofcode/comments/1pcvaj4/2025_day_3_solutions/ns2mdnt/

## day 05

- [paste](https://topaz.github.io/paste/#XQAAAQDaCAAAAAAAAAAFYJzkZThXj+6Q59kQEdWy3obmc3FkLeFE/WbeMUeH4ZSgpp10BOR+EHF46PC8iZeoKN+1Dx6DhFLWLyd2F27MGK3+F16aYwqorI6KaIiLxsnClw/6vzak/dGnsjv/dDAcZdYSQydxqNVs+0UfUbrlcp6bK2Uc2ZEE45LbgxGX16N9N8qnzEBFA1KXKkwif91Kp8hnjsacXzkHnctl6lvDl/rFkfkeZ/LMcEy+RdDQiWIurCmWuGA9Sg90ROvFekHuwNcEk/eWSxOkxWKhBjKB+KqFcI0vvad3PfSCICmXniKTeAwBFvYb/P/pnt8xlMWx3NrIDhlEJmdfjtdOils8B77/6AfqjsJFJZhiKbUO12B4VKcmc3fotWlAz9EliFJJc6i0IY1+/DQrX8Ti0B/PpF0O1EppyWyN874fp/XUlQTCkeqrPXD8J15wlIgXZnpt4KnY0Bs4ANWoEX45Oaed7+kWzI4D9inMqvsS2KcR4KOKSwTCaqjcz2XgnWYYISXR/L+KuwTy9I8I3hsHRVB2ub2lmcAdq4UzOxWdAuRdt69avj0QrN67iTKJVjIl2znWxk2DNm5dz4vE1K6u42z2ntOTIAfNHcnLq2utoraBFM3DJnrkKyEML7ilc2QQzJt2SU/xn7dpNmDXYenP6eNvRLot8CcRTNHdok1MGELK2i9NgjBhiMFxLAemrQk1m+N5gBo3s7tPu1S8Y0as6zSLrNjEZ0HjTR4c6jhv0ODu0zg0cAViNv3LRgz0toeT6z9JqV+uInyp4+lzJTKqSU+lHzrJM8OGdj88GOuW5GSzLhNvT7b+2ktA9FsYg1VZ/APQP5PVPFTWL6q277b+ej3M7otnqcA9oG5YcWW2D2oIhgZjL4HIwq9Qm4VceWnBIuPJ+s66izQSGBOOcS/cVkGh9cN2BkQkRcyqmbzwAFAnjEPwwvUhqF1A/+VrFkQ=)

- https://www.reddit.com/r/adventofcode/comments/1pemdwd/2025_day_5_solutions/nsg2tm5/

## day 06

- https://www.reddit.com/r/adventofcode/comments/1pfguxk/2025_day_6_solutions/nsm725i/
- https://codeberg.org/nemin/AOC2025/src/branch/main/day6.lisp
- https://www.reddit.com/r/adventofcode/comments/1pfguxk/2025_day_6_solutions/nskn1tq/?share_id=7L6-N4BYPwhNYIo-aRhHa

---
So, yes: I picked this hill to die on just to do something clever & lisp-y: If you

1. take the last row (with the + or * characters) and move it to the top,
2. then follow that with a row of spaces,
3. then transpose the list of lines and concatenate them together, and finally
4. liberally apply parentheses,

you produce a list of valid lisp expressions such as (+ 4 431 623) that can each be directly evaluated (i.e., eval '(+ 4 431 623)) and summed in a big sweep.
---

Part one gets as simple as: (/u/nohillside)

```lisp
(defun transpose (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

(defun run-step-1 (input)
  (loop for line in (transpose input)
        sum (eval (reverse line))))
```

(revelation: this is a transpose:

```lisp
(apply #'mapcar #'list '((1 2 3) (10 20 30)))
;; ((1 10) (2 20) (3 30))
```
)

- https://www.reddit.com/r/adventofcode/comments/1pfguxk/2025_day_6_solutions/nsm725i/

> Approach: rotate the whole input, then handle as if it were given as lisp data.
> "I played with conditions to handle numbers vs operator."

## day 07

- https://www.reddit.com/r/adventofcode/comments/1pg9w66/2025_day_7_solutions/nst38vh/
- https://www.reddit.com/r/adventofcode/comments/1pg9w66/2025_day_7_solutions/nsqrve4/
- https://www.reddit.com/r/adventofcode/comments/1pg9w66/2025_day_7_solutions/nsqn3jd/
- https://www.reddit.com/r/adventofcode/comments/1pg9w66/2025_day_7_solutions/nsqn3jd/

## day 08

- https://old.reddit.com/r/adventofcode/comments/1ph3tfc/2025_day_8_solutions/nt26px9/
- wsgac: "Circuits are constructed by storing connectivity chains. Common circuit membership is established by following the connectivity chains till the end and comparing points. Tried Bron-Kerbosch before, but couldn't make it to work."

## day 09

- https://old.reddit.com/r/adventofcode/comments/1phywvn/2025_day_9_solutions/nt94p4v/
- ak-coram: "Using DuckDB with the spatial extension feels a bit like cheating, but it was more fun for me to implement than a direct approach."

## day 10

hard
