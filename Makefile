CC := ocamlbuild
LIBS := graphics,unix
SRC := src
CFLAGS := -libs $(LIBS) 

.PHONY: all mproper

all: main billard missilecommand

main:
	$(CC) $(CFLAGS) $(SRC)/$@.native

billard:
	$(CC) $(CFLAGS) $(SRC)/$@.native

missilecommand:
	$(CC) $(CFLAGS) $(SRC)/$@.native

mproper:
	$(CC) -clean
	rm -rf rapport/*.toc
	rm -rf rapport/*.aux

