all: trim-qual sam-filter embl-to-fasta
GHCOPTS = -O2 -threaded -rtsopts -Wall -fno -fwarn-tabs -fno-warn-missing-signatures

trim-qual: trim-qual.hs Utils.hs
	ghc -o $@ $< $(GHCOPTS)

sam-filter: sam-filter.hs
	ghc -o sam-filter -O2 sam-filter.hs -threaded -rtsopts

embl-to-fasta: embl-to-fasta.hs
	ghc -o $@ $< $(GHCOPTS)

.PHONY: all

