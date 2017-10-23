include ../Makefile

# Makefile for nmf_mu
CCFLAGS = -Wall -Wextra -g -fPIC
LIBS = $(shell pkg-config --libs lapack arpack || echo -llapack -larpack)

SOURCE = $(wildcard *.c)
OBJ = $(SOURCE:%.c=%.o)

.INTERMEDIATE: $(OBJ)

nmf_mu.so: nmf_mu.o
	$(CC) -shared -Wl,-soname,$@ $(LIBS) -o $@ $^

%.o: %.c
	$(CC) $(CCFLAGS) -c $<

clean:
	rm -f nmf_mu.so $(OBJ)
