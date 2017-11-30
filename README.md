# An unofficial bookdown template for a doctoral thesis at Karolinska Institutet

The files in this repository provide an unofficial blueprint for a doctoral thesis at [Karolinska Institutet](http://ki.se), compiled using the [bookdown](https://bookdown.org/yihui/bookdown/) package to combine multiple RMarkdown documents into one. The primary use is export to PDF via Latex using pandoc. There are a lot of ad hoc Latex insertions which might complicate export to other formats.

## Workflow

1. Execute `bookdown::render_book("index.Rmd")` twice to generate the main body of the thesis in the output subfolder.
2. Knit before_body.Rmd in the before_body subfolder to a PDF. This includes the unnumbered pages before the table of contents, such as the title page, copyright, spikblad (there is a separate spikblad with date and time in the spikblad subfolder), abstract and publication list.
3. Compile PDF (easiest from within RStudio) for combine.tex in the output subfolder to combine the two documents.
4. The output is the final document combine.pdf in the output subfolder.

## Acknowledgments

This thesis template has been inspired by incredibly valuable blogposts from [\@ed_berry](https://twitter.com/ed_berry): [Writing your thesis with bookdown](https://eddjberry.netlify.com/post/writing-your-thesis-with-bookdown/) and [\@rosannavhespen](https://twitter.com/rosannavhespen): [Writing your thesis with R Markdown](https://rosannavanhespenresearch.wordpress.com/2016/02/03/writing-your-thesis-with-r-markdown-1-getting-started/).

The files in this repository compile into my thesis at KI which can also be found [here](https://openarchive.ki.se/xmlui/handle/10616/46090).

## Package versions used to compile the thesis

* R version 3.3.0
* Bookdown 0.5
* RMarkdown 1.6
* Knitr 1.17
