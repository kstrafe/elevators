.PHONY:
2 all:
	script -f -q /dev/null -c 'racket main.rkt >& debug && echo this-is-the-racket-program-that-needs-to-be-killed > /dev/null' &
.PHONY:
3 kill:
	kill $$(pgrep -f this-is-the-racket-program-that-needs-to-be-killed)

