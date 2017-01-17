print_dependency_contents()
ggsave( file.path( FREEZR_DESTINATION, "scatterplot_from_ggsave.pdf" ),
        qplot(1:5, 1:5, main = "I'm a scatterplot!" ) )
