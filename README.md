# Programming In Haskell, 2nd Edition

Solutions to Graham Hutton's *Programming in Haskell (2nd ed)*.

This repository also contains the markdown files that are converted to HTML to create my blog posts.

I use the following command to generate the HTML files (the `if` statement in the awk script avoids including the `pdfs` folder):

```bash
ls -1 | awk -F ".md" '{if(NF==2)print $1}' | xargs -I% pandoc --standalone --katex --from markdown --to html5 %.md -o %.html
```

I use the following command to generate the PDF files:

```bash
ls -1 | awk -F ".md" '{if(NF==2)print $1}' | xargs -P 8 -n 8 -I % pandoc --template eisvogel --listings --pdf-engine=lualatex --from markdown --to pdf %.md -o pdfs/%.pdf
```
