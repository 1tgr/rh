all: build

build:
	mkdir -p bin obj
	ghc --make -Wall -fno-warn-name-shadowing -o bin/rh -outputdir obj *.hs

clean:
	rm -r bin obj

test: build
	(cd ../ray-tracer; git checkout -- *.hs)
	bin/rh ../ray-tracer/*.hs
