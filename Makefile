CC := ocamlbuild
LIBS := graphics,unix
SRC := src
CFLAGS := -libs $(LIBS) 

.PHONY: all mproper

all: native

native:
	$(CC) $(CFLAGS) $(SRC)/main.$@
	$(CC) $(CFLAGS) $(SRC)/missilecommand.$@
	$(CC) $(CFLAGS) $(SRC)/billard.$@

byte:
	$(CC) $(CFLAGS) $(SRC)/main.$@
	$(CC) $(CFLAGS) $(SRC)/missilecommand.$@
	$(CC) $(CFLAGS) $(SRC)/billard.$@

mproper:
	$(CC) -clean
