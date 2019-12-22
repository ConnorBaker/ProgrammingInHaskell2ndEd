# Programming In Haskell, 2nd Edition

Solutions to Graham Hutton's *Programming in Haskell (2nd ed)*.

This repository also contains the markdown files that are converted to HTML to create my blog posts.

I use the following command to generate the HTML files:

```bash
ls -1 | awk -F ".md" '{print $1}' | xargs -I% pandoc --standalone --katex --from markdown --to html5 %.md -o %.html
```
