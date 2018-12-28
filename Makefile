default: build

install-deps:
	cabal update; \
	cabal install mtl

mimuw:
	wget https://www.stackage.org/stack/linux-x86_64-static -O stack.tgz
	tar xvvf stack.tgz
	mv stack-*/stack stack
	rm -rf stack.tgz stack-*

	./stack build
	./stack install --local-bin-path .

stack-mimuw:
	wget https://www.stackage.org/stack/linux-x86_64-static -O stack.tgz
	tar xvvf stack.tgz
	mv stack-*/stack stack
	rm -rf stack.tgz stack-*

build-mimuw:
	cabal build
	cp ~/.cabal/bin/latc_x86_64 .

build-bnfc:
	stack build BNFC; \
	cd src; \
	stack exec bnfc -- -p Frontend --functor -m Frontend/Latte.cf; \
	make; \
	mv Frontend/TestLatte ../;

clean-bnfc:
	rm src/Frontend/LexLatte.x; \
	rm src/Frontend/ParLatte.y; \
	rm src/Frontend/TestLatte.hs; \

build-stack:
	stack setup 8.2.2; \
	stack build;

build:
	stack build
	stack install --local-bin-path .

clean:
	stack clean; \
	rm -f src/Frontend/*Latte.hs; \
	rm -f src/Frontend/*Latte.txt; \
	rm -f TestLatte; \
	rm -f src/Frontend/ErrM.hs;
	rm -f src/Frontend/TestLatte.hs; \