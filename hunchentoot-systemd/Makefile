
LISP ?= /usr/bin/sbcl --disable-debugger --userinit /home/vince/.sbclrc

# Things to have in your .sbclrc:
# (require 'asdf)
# load quicklisp (you'll run systemd as root, so (user-homedir-pathname) will
# return a directory where you likely didn't install Quicklisp.

run:
	rlwrap $(LISP) --load run.lisp
