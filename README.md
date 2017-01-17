##Welcome to the cold room!

If you are a data analyst in a scientific or scholarly field, then you've likely experienced the tension between fast-changing code and reproducibility. `freezr` helps you keep track of previous analyses. It will simultaneously:

- **run your code**,
- **gather the output** into a time-stamped results folder, and
- **save a copy of your code** along with the results.

-

####Getting started

You can install `freezr` using Hadley Wickham's `devtools` package:

    devtools::install_github("ekernf01/freezr")
	
Then you can start freezing code immediately. The following line will run `my_functions.R` and then `my_script.R`, saving them and their resu

    library(freezr)
    freezr::freeze(analyses_to_run = c( "my_functions.R", "my_script.R" ),
                   destination = file.path("~", "my_project", "results") )
                   
-

####Features

- **dependency tracking:** `freezr` is not limited to saving code. You can also save tables or other files that your analysis depends on. 
- **R markdown:** `freezr` can `purl` an R Markdown file and run the resulting R code. (It cannot `knit` yet, but that may change in future versions.)
- **graphics and text output:** `freezr` redirects these to files.
- **easy customization**: if you want to further format the results, your scripts can access the destination via `Sys.getenv()[["FREEZR_DESTINATION"]]`.

-

####FAQ

- Why not just use Git?

 `freezr` is meant to be more accessible to non-experts such as wet-lab biologists. Also, it allows you to view old and new material simultaneously, with no need to switch branches or go back to previous commits.

- I'm used to manually saving a copy of my code as I work, and that way I always remember to put notes to myself about what I'm doing. Will this make me (or my students) lazy about documenting analyses?

 No! `freezr` is configured to **automatically nag you** about your note-taking, though this feature can be suppressed.

- You broke my R console!

 I'm sorry! I used `sink()` and failed to clean up after myself. Try running `freezr::sink.reset()`. 