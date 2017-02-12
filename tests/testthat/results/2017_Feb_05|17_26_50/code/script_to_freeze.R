print_dependency_contents()
library(ggplot2)
ggsave( file.path( Sys.getenv()[["FREEZR_DESTINATION"]], "scatterplot_from_ggsave.pdf" ),
        qplot(1:5, 1:5, main = "I'm a scatterplot!" ) )
