DIR = ../ray-tracer
GHCFLAGS = --make -Wall -fno-warn-name-shadowing -iRh

all: build-program build-tests

init:
	mkdir -p bin obj

build-program: init
	ghc $(GHCFLAGS) -outputdir obj/rh -o bin/rh Program/*.hs

build-tests: init
	ghc $(GHCFLAGS) -outputdir obj/tests -o bin/tests Tests/*.hs

clean:
	rm -r bin obj

test-program: build-program
	(cd $(DIR); git checkout -- *.hs)
	bin/rh Vector ZVector ../ray-tracer/*.hs
	bin/rh Shader VShader ../ray-tracer/*.hs
	(cd $(DIR); git diff *.hs)

tests: build-tests
	bin/tests