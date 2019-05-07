.SUFFIXES: .$(EXT) .asm .obj .exe
LANG=diy
EXT=diy
LIB=lib
RUN=run
ARCH=
AS=nasm -felf32
#ARCH=-DpfARM
#AS=as
CC=gcc
CFLAGS=-g -DYYDEBUG $(ARCH)

$(LANG): $(LANG).y $(LANG).l
	make -C lib
	byacc -dv $(LANG).y
	flex -l $(LANG).l
	$(LINK.c) -o $(LANG) -I$(LIB) lex.yy.c y.tab.c -L$(LIB) -lutil

clean::
	rm -f *.o $(LANG) lex.yy.c y.tab.c y.tab.h y.output yyselect.c *.asm *~ *.obj *.exe
