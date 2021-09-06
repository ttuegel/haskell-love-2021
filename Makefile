sawtooth: README.lhs
	ghc $< -pgmL markdown-unlit -optL haskell+sawtooth -o $@ -rtsopts -prof -fprof-auto

sawtooth.hp: sawtooth
	./$< +RTS -p -h -i0.01

sawtooth.svg: sawtooth.hp
	hp2pretty $<

sawtooth.png: sawtooth.svg
	convert +antialias $< $@

small: README.lhs
	ghc $< -pgmL markdown-unlit -optL haskell+small -o $@ -rtsopts -prof -fprof-auto

small.prof: small
	./$< +RTS -p

ghci:
	ghci -Wno-missing-signatures -XBangPatterns -pgmL markdown-unlit -optL haskell+lazy README.lhs
.PHONY: ghci

clean:
	rm -f small*
	rm -f sawtooth*
	rm -f *.o *.hi *.hp *.dump-simpl
.PHONY: clean
