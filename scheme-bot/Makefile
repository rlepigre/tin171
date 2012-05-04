CFLAGS = -optimize-level 4 -unsafe -disable-interrupts -no-trace -no-lambda-info
OBJECTS = protocol.o board.o

.SUFFIXES:
.SUFFIXES: .scm .o
.scm.o:
	csc -c $(CFLAGS) $< -o $@

all: scheme-bot

scheme-bot: bot.scm $(OBJECTS)
	csc $(CFLAGS) bot.scm $(OBJECTS) -o scheme-bot

erlang-lexer.scm: erlang-lexer.l
	csi -R silex -e '(lex "erlang-lexer.l" "erlang-lexer.scm")'

erlang-literals.yy.scm: erlang-literals.grm.scm
	csi < erlang-literals.grm.scm

protocol.o: erlang-lexer.scm erlang-literals.yy.scm
board.o:

clean:
	rm -f erlang-lexer.scm erlang-literals.yy.scm
	rm -f scheme-bot *~ $(OBJECTS)

