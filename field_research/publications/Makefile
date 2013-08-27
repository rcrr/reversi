MAKE = make
RM = rm

.PHONY: pdf clean distclean

pdf: build/reversi_bib.pdf

build/reversi_bib.pdf: reversi_bib.tex reversi.bib
	cp reversi_bib.tex ./build
	cp reversi.bib ./build
	cp elsarticle.cls ./build
	cp othelloboard.sty ./build
	(cd build; pdflatex -shell-escape reversi_bib)
	(cd build; bibtex reversi_bib)
	(cd build; pdflatex -shell-escape reversi_bib)
	(cd build; pdflatex -shell-escape reversi_bib)
	(cd build; pdflatex -shell-escape reversi_bib)

clean:
	$(RM) -f ./build/*.aux ./build/*.log ./build/*.out ./build/*.spl ./build/*.dvi ./build/*.bbl ./build/*.blg \
	./build/*.tex ./build/*.bib ./build/*.sty ./build/*.cls

distclean: clean
	$(RM) -f ./build/*.pdf






