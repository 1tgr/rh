all: build

build:
	mkdir -p bin obj
	ghc --make -Wall -o bin/rh -outputdir obj *.hs

clean:
	rm -r bin obj

test: build
	bin/rh
