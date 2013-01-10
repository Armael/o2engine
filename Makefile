CC := ocamlbuild
LIBS := graphics,unix
CFLAGS := -libs $(LIBS)

.PHONY: all main.native main.byte mproper

all: native

native:
	$(CC) $(CFLAGS) main.$@
	$(CC) $(CFLAGS) missilecommand.$@

byte:
	$(CC) $(CFLAGS) main.$@

mproper:
	$(CC) -clean
