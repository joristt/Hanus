
default:slides.pdf

verbose:slides.md
	pandoc -t beamer slides.md -o slides.pdf --pdf-engine=xelatex --verbose --template beamer-template.tex


slides.pdf:slides.md
	pandoc -t beamer slides.md -o slides.pdf --pdf-engine=xelatex --template beamer-template.tex

