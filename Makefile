CC=gcc
CFLAGS=-g

diy: diy.l initial.y
	byacc -dv initial.y
	flex -l diy.l
	gcc -g -o $@ lex.yy.c y.tab.c
