all: trim-qual sam-filter embl-to-fasta
GHCOPTS = -O2 -threaded -rtsopts

trim-qual: trim-qual.hs
	ghc -o trim-qual -O2 trim-qual.hs -threaded -rtsopts
sam-filter: sam-filter.hs
	ghc -o sam-filter -O2 sam-filter.hs -threaded -rtsopts

embl-to-fasta: embl-to-fasta.hs
	ghc -o $@ $< $(GHCOPTS)

.PHONY: all

