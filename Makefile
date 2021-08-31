sawtooth: README.lhs
	ghc $< -pgmL markdown-unlit -optL haskell+sawtooth -o sawtooth -rtsopts -prof -fprof-auto

sawtooth.hp: sawtooth
	./$< +RTS -p -h -i0.01

sawtooth.svg: sawtooth.hp
	hp2pretty $<
