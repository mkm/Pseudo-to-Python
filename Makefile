

MAIN=Main.hs

pstopy : $(MAIN)
	ghc $< -o $@ --make

.PHONY : examples

examples : pstopy
	$(MAKE) -BC examples

