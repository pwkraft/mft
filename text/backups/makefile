mft_draft.docx: mft_draft.tex
	pandoc mft_draft.tex -o mft_draft.docx --bibliography=/data/Copy/1-src/lit/Literature.bib --number-sections ##--csl=ajps.csl
## csl produces an error for some reason

mft_draft.pdf: ideal_draft.tex
	pandoc -H options.sty mft_draft.tex -o mft_draft.pdf --bibliography=/data/Copy/1-src/lit/Literature.bib --variable fontsize=12pt
