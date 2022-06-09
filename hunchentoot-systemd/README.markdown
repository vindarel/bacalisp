
Test:

# Terminal

    make run

```
To load "hunchentoot":
  Load 1 ASDF system:
    hunchentoot
; Loading "hunchentoot"
....
To load "trivial-backtrace":
  Load 1 ASDF system:
    trivial-backtrace
; Loading "trivial-backtrace"

-------- create hunchentoot acceptor…
-------- start app on port 4000… --------------
-------- … and done. Now we have the REPL. --------------
*
```

and access http://127.0.0.1:4000/

see

```
Not Found
The requested URL / was not found on this server.

Hunchentoot 1.3.0 (SBCL 1.4.5.debian) at 127.0.0.1:4000
```

OK, Hunchentoot is working.

# Systemd

You have to put Hunchentoot on the foreground, otherwise Lisp quits instantly, and systemd will tell you

```
compilation unit aborted
; caught 1 fatal ERROR condition
```

but there is no actual error, just a bad return code. See .lisp demo.

---

Other things to keep in mind when starting your app:

* you probably rely on Quicklisp. Then on your .sbclrc. Then on QL's snippet which uses (user-homedir-pathname). But you use Systemd from root, and you probably didn't install QL in your root home directory. So, adapt this line, or use this sbcl switch:

* sbcl --userinit /path/to/your/.sbclrc

* the --disable-debugger switch is good for production (but handy to keep for debugging…)

* don't forget you can build a binary, so you won't face these two issues.
