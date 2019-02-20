all: lex.erl grm.erl

lex.erl: lex.xrl
	erl -eval "leex:file(lex)" -noshell -detached

grm.erl: grm.yrl
	erl -eval "yecc:file(grm)" -noshell -detached

clean:
	rm -f lex.erl grm.erl
