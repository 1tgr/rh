DIR = ../ray-tracer

all: build

build:
	mkdir -p bin obj
	ghc --make -Wall -fno-warn-name-shadowing -o bin/rh -outputdir obj *.hs

clean:
	rm -r bin obj

test: build
	(cd $(DIR); git checkout -- *.hs)
	bin/rh Shader NewShader ../ray-tracer/*.hs
	(cd $(DIR); git diff *.hs)