HC    = ghc
OBJ   = -c

all: simplejson	

simplejson:
	mkdir -p bin
	$(HC) $(OBJ) src/SimpleJSON.hs -o lib/SimpleJSON.o

clean:
		rm -rf bin
		mkdir bin
