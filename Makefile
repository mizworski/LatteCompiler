default: build-mimuw

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

build-mimuw:
	cabal update; \
	cabal build; \
	cp dist/build/latc_llvm/latc_llvm .;

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