
all: oases.pdf

clean:
	rm oases.pdf oases.ps oases.dvi oases.log oases.aux oases.toc

oases.pdf: oases.ps
	ps2pdf oases.ps oases.pdf
	@echo "Built oases.pdf"

oases.ps: oases.dvi
	dvips -o oases.ps oases.dvi

oases.dvi: *.tex figs/*.ps
	latex -interaction=nonstopmode oases.tex


