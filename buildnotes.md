# Some notes to remind myself of the build and release process

[devtools cheat sheet](https://raw.githubusercontent.com/rstudio/cheatsheets/main/package-development.pdf)

Reload the package while building

```R
devtools::load_all()
```

```R
# check package
devtools:check()

# submit to CRAN.
devtools:release()

```