##A simple, transparent reproducibility tool

If you are a data analyst in a scientific or scholarly field, then you've likely experienced the tension between fast-changing code and reproducibility. `freezr` is an `R` package meant to alleviate this tension. It helps you archive your current analysis more easily so you can move on to the next question or approach. In a single line of code, it will helpfully:

- **run your scripts**,
- **gather the output** into a time-stamped results folder,
- **save a copy of your code** along with the results, and
- **nag you to write down some notes** about what you did.

-

####Getting started

You can install `freezr` using the [`devtools`](https://www.rstudio.com/products/rpackages/devtools/) package:

    install.packages("devtools") # installs devtools
    devtools::install_github("ekernf01/freezr") # installs freezr
	
Then you can start freezing code immediately. The following line will run `my_functions.R` and then `my_script.R`, saving them and their results to `~/my_project/results/<timestamp>`.

    library(freezr)
    freezr::freeze( analyses_to_run = c( "my_functions.R", "my_script.R" ),
                    destination = file.path( "~", "my_project", "results" ) )
                   
-

####Features

- This package has other functions:
	- `inventory` helps follow data from one analysis to the next. Use it to **retrieve files without hard-coding a bunch of paths** into your scripts. See `?freezr::inventory`.
	- `thaw` is not ready yet but will re-run a frozen analysis.
- The `freeze` function offers:
	- **easy customization**: if you want to further format the results, your scripts can access the destination via `Sys.getenv()[["FREEZR_DESTINATION"]]`.
	- **dependency tracking:** `freezr` is not limited to saving code. You can also save tables or other files that your analysis depends on. 
	- **R markdown:** `freezr` can `purl` an R Markdown file and run the resulting R code. (It cannot `knit` yet, but that may change in future versions.)
	- **Graphical and plain-text console output:** `freezr` redirects these to files for you to peruse later.

-

####FAQ

- I'm new to R. How do I access the help/manual page for your package? 

 Type `?freezr::freeze`.

- Why not just use Git?

    - `freezr` is meant to be more accessible to software non-experts, especially wet-lab biologists. Git is [notoriously confusing](https://xkcd.com/1597/).
    - `freezr` allows you to view old and new material simultaneously, with no need to switch branches or go back to previous commits.
    - `freezr` saves information on R and package versions that might be difficult to retrieve otherwise.

- I'm used to manually saving a copy of my code as I work, and that way I always remember to put notes to myself about what I'm doing. Will this make me (or my students) lazy about documenting analyses?

 No! `freezr` is configured to **automatically nag you** about your note-taking, though this feature can be suppressed.

- You broke my R console!

 I'm sorry! I must have used `sink()` and failed to clean up after myself. Try running `freezr::sink_reset()`. 