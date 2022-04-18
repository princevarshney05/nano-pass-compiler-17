CC 		= gcc

UNAME_S := $(shell uname -s)
UNAME_P := $(shell uname -p)
ifeq ($(UNAME_S),Darwin)
    ifeq ($(UNAME_P),arm)
        $(info M1 MacOSX detected)
        CC := arch -x86_64 $(CC)
    endif
endif

.PHONY: all test 

all: runtime.o fake_prog

# test
test: runtime.o
	racket run-tests.rkt

runtime.o: runtime.c runtime.h
	$(CC) -std=c11 -c $^

fake_prog: fake_prog.c runtime.o
	$(CC) -std=c11 $^ -o $@

clean:
	rm -rf *~ fake_prog runtime.o runtime.h.gch ./compiled tests/*.s tests/*.out tests/*.dSYM tests/*~ *.dot *.png log.* *.log
