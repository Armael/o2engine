CC := ocamlbuild
LIBS := graphics,unix
CFLAGS := -libs $(LIBS)

.PHONY: all mproper

all: native

native:
	$(CC) $(CFLAGS) main.$@
	$(CC) $(CFLAGS) missilecommand.$@
	$(CC) $(CFLAGS) billard.$@

byte:
	$(CC) $(CFLAGS) main.$@
	$(CC) $(CFLAGS) missilecommand.$@
	$(CC) $(CFLAGS) billard.$@

mproper:
	$(CC) -clean
