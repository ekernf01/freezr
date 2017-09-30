## A simple, transparent reproducibility tool

If you are a data analyst in a scientific or scholarly field, then you've likely experienced the tension between reproducibility demands and fast-changing custom analysis scripts. `freezr` is an `R` package meant to alleviate this tension. It helps you archive your current analysis in a snap so you can move on to the next question or approach. In a single line of code, it will helpfully:

- **run your script**,
- **gather the output** into a time-stamped results folder,
- **save a copy of your code and `sessionInfo()`** along with the results, and
- **nag you to write down some notes** about what you did.

For bigger projects, `freezr` integrates a simple "inventory" system to keep track of saved data. This allows you to load data into a downstream script without hard-coding a bunch of paths.

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

- The `inventory_*` functions help you follow data from one analysis to the next. Use it to **retrieve files without hard-coding a bunch of paths** into your scripts. See `?freezr::inventory_make`, `?freezr::inventory_add`, `?freezr::inventory_get`.

- The `freeze` function offers:
	- **easy customization**: if you want to save your own plots, your scripts can access the `user` subfolder of the freezr archive via `Sys.getenv()[["FREEZR_DESTINATION"]]`.
	- **dependency tracking:** `freezr` is not limited to saving code. You can also save tables or other files that your analysis depends on. 
	- **R markdown support:** `freezr` can `purl` an R Markdown file and run the resulting R code. 
	- **Graphical and plain-text console output logs:** `freezr` redirects all output to files for you to peruse later.

-----

#### Other reproducibility tools

- There are several systems to keep track of R package versions ([PackRat](https://rstudio.github.io/packrat/), [`checkpoint`](https://mran.microsoft.com/documents/rro/reproducibility/), [`pkgsnap`](https://github.com/MangoTheCat/pkgsnap)). 
- To deal with statistical issues such as the ["Garden of forking paths"](http://www.stat.columbia.edu/~gelman/research/unpublished/p_hacking.pdf), there's [`revisit`](https://github.com/matloff/revisit). 
- The R Markdown and `knitr` family allow for readable documents with embedded source code. 

These solve problems that overlap with the domain of `freezr`, but none have the same purpose or functionality.

-----

#### FAQ

- I'm new to R. How do I access the help/manual page for your package? 

 Type `?freezr::freeze` (core functionality) or `?freezr::inventory_add` (extra tools for bigger projects).

- Why not just use Git?

    - `freezr` lets you run the analysis and archive it, both with a single command. This is easier than constantly forcing yourself to alternate between interacting with data and taking notes. 
    - `freezr` makes it easy to run and archive multiple analyses simultaneously. You can view old and new material simultaneously, with no need to switch branches or go back to previous commits. 
    - `freezr` is tailored to R, saving information on language and package versions that does not usually live inside a repo (unless you use PackRat or similar).
    - `freezr` is easy to learn. 

- You broke my R console!

 I'm sorry! This happens when `freezr` calls `sink()` and gets interrupted before it can clean up after itself. Try running `freezr::sink_reset()`. 