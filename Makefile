

MAIN=Main.hs

pstopy : $(MAIN)
	ghc $< -o $@ --make


