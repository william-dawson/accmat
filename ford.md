---
project: fbuf
summary: Experimenting with AI to develop a new Fortran style for GPU
author: William Dawson
src_dir: ./api
output_dir: ./docs
graph: true
search: true
source: true
warn: true
macro: TEST
       LOGIC=.true.
preprocess: true
docmark: >
docmark_alt: <
predocmark: |
predocmark_alt: #
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
exclude_dir: ../lib
             ../build
             ../example
license: mit
---

{!README.md!}
