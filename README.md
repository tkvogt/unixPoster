unixPoster
==========

A huge Unix poster (Din A0 or Din A1) showing the most important commands with examples.
The special aspect about this poster is that it was programmed with the
Haskell diagrams library instead of using a tool like Inkscape.

Entering

ghc --make poster.hs     or go to the folder and
cabal install

builds an executable, and entering:

./poster -o unixposter.pdf -w 1685

produces [unixposter.pdf](unixposter.pdf)
