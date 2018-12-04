default: build

install-deps:
	cabal update; \
	cabal install mtl

build:
	stack setup 8.2.2; \
	stack build BNFC; \
	cd src; \
	stack exec bnfc -- -p Frontend --functor -m Frontend/Latte.cf; \
	make; \
	mv Frontend/TestLatte ../; \
	rm Frontend/TestLatte.hs; \
	rm Makefile; \
	rm Frontend/LexLatte.x; \
	rm Frontend/ParLatte.y;

clean:
	stack clean; \
	rm -f src/Frontend/*Latte.hs; \
	rm -f src/Frontend/*Latte.txt; \
	rm -f TestLatte; \
	rm -f src/Frontend/ErrM.hs;