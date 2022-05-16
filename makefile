all: interpreter
interpreter:
	ghc -Wall -Wno-unused-do-bind -Wno-unused-imports -isrc/grammar/ -isrc/ -outputdir buildInterpreter -o interpreter src/RunInterpreter.hs
clean:
	rm -rf buildInterpreter interpreter
.PHONY:
	clean
