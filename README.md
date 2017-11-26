## `freezr`: A simple, transparent reproducibility tool

If you are a data analyst in a scientific or scholarly field, then you've likely experienced the tension between reproducibility demands and fast-changing custom analysis scripts. `freezr` is an `R` package meant to alleviate this tension. It helps you archive your current analysis in a snap so you can move on to the next question or approach. In a single line of code, it will helpfully:

- **run your script**,
- **gather the output** into a time-stamped results folder,
- **save a copy of your code, repo hash, and `sessionInfo()`** along with the results, and
- **nag you to write down some notes** about what you did.

For bigger projects, `freezr` includes a simple "inventory" system to keep track of saved data. This allows you to load data into a downstream script without hard-coding paths, which is particularly useful when saving intermediates inside of timestamped `freezr` archives.

-----

#### Getting started

You can install `freezr` using the [`devtools`](https://www.rstudio.com/products/rpackages/devtools/) package:

    install.packages("devtools") # installs devtools
    devtools::install_github("ekernf01/freezr") # installs freezr
	
Then you can start freezing code immediately. The following line will run `my_functions.R` and then `my_script.R`, saving them and their results to `~/my_project/results/<timestamp>`.

    library(freezr)
    freezr::freeze( analyses_to_run = c( "my_functions.R", "my_script.R" ),
                    destination = file.path( "~", "my_project", "results" ) )
-----

#### Features

- The `freeze` function offers:
	- **easy customization**: if you want to save your own plots, your scripts can access the `user` subfolder of the freezr archive via `Sys.getenv()[["FREEZR_DESTINATION"]]`.
	- **dependency tracking:** `freezr` is not limited to saving code. You can also save tables or other files that your analysis depends on. 
	- **R markdown support:** `freezr` can `purl` an R Markdown file and run the resulting R code. 
	- **Graphical and plain-text console output:** `freezr` logs all output to files for you to peruse later.
	- **Logs** of your `sessionInfo` as well as the commit your repo is on. You can tell it to track other repos, too, if there is a package you are developing in a separate repo alongside your analysis repo.

- The `inventory_*` functions help you follow data from one analysis to the next. Use them to **retrieve files without hard-coding a bunch of paths** into your scripts. See `?freezr::inventory_make`, `?freezr::inventory_add`, `?freezr::inventory_get`. These are most useful to me on larger projects, especially projects where my scripts save Seurat objects of size 1GB+ that are excluded from my git repo.

-----

#### Other reproducibility tools

*I recently found out about [`recordr`](https://github.com/NCEAS/recordr), which is very similar to `freezr`. I haven't tried `recordr` yet, but I know it is quite sophisticated and has some great interactive features. As far as I can see, the only functionality unique to `freezr` is the "inventory" system, which helps pass data between scripts. The inventory is very handy for the single cell RNA-seq projects that I work on.*  

Here are some other tools relating to reproducible research in R. These solve problems that overlap with the domain of `freezr`, but none has the same purpose or functionality.

- To deal with statistical issues such as the ["Garden of forking paths"](http://www.stat.columbia.edu/~gelman/research/unpublished/p_hacking.pdf), there's [`revisit`](https://github.com/matloff/revisit). 
- The R Markdown and `knitr` family allow for readable documents with embedded source code.
- There are several systems to keep track of R package versions ([PackRat](https://rstudio.github.io/packrat/), [`checkpoint`](https://mran.microsoft.com/documents/rro/reproducibility/), [`pkgsnap`](https://github.com/MangoTheCat/pkgsnap)). 
 

One inevitable question people ask is why we can't just use Git. Here's why I view Git as complementary to `freezr`, rather than capable of replacing `freezr`.

- `freezr` lets you run the analysis and archive it, both with a single command. This is easier than constantly forcing yourself to alternate between interacting with data and taking notes. 
- `freezr` makes it easy to run and archive multiple analyses simultaneously. You can view old and new material together, with no need to switch branches or go back to previous commits. 
- `freezr` is tailored to R, saving information on language and package versions that does not usually live inside a repo (unless you use PackRat or similar).
- `freezr` is easy to learn. 

-----

#### FAQ

- I'm new to R. How do I access the help/manual page for your package? 

 Type `?freezr::freeze` (core functionality) or `?freezr::inventory_add` (extra tools for bigger projects).


- You broke my R console!

 I'm sorry! This happens when `freezr` calls `sink()` and gets interrupted before it can clean up after itself. Try running `freezr::sink_reset()`. 
 
- What's next?

I'm working on a vignette and on [linking freezr archives to the commit on which they were built.](https://discuss.ropensci.org/t/associating-an-rmd-file-with-a-commit/273)

                   
-----

#### Citation

`freezr` is not published in any academic-style manuscripts. If `freezr` helped you in your work, you can refer to this repository, `https://github.com/ekernf01/freezr` . 

