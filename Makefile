HC    = ghc
OBJ   = -c

all: main simplejson	

main: simplejson
	mkdir -p bin
	$(HC) -o bin/SimpleJSON lib/SimpleJSON.o


simplejson:
	mkdir -p lib
	$(HC) $(OBJ) src/SimpleJSON.hs -o lib/SimpleJSON.o

clean:
	rm -rf bin
	rm -rf lib
	mkdir bin
