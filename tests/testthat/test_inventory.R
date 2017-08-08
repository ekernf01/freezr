# # Helpful stuff # #

# Cleanup procedure from previous testing
clean_up_inv = function(){
  suppressWarnings( file.remove( file.path( dirname( dirname( Sys.getenv()[["FREEZR_DESTINATION"]] ) ), ".inventory.txt") ) )
}
# Files to use as guinea pigs
f1     = file.path( "inventory_guinea_pig", "user", "custom_user_output.txt" )
f1_abs = file.path( "/Users", "erickernfeld", "Desktop",
                    "software_projects", "freezr",
                    "tests", "testthat", "results", f1 )

# # Old functions # #
# # Testing
clean_up_inv()
freezr::inventory()
# Test with essential args
freezr::inventory( tag="my_dummy_output", filename=f1 )
# Test with parent_tag arg
freezr::inventory( tag="my_dummy_output_3", filename = f1, parent_tag = "my_dummy_output_2" )
# Test with incorrect arg
freezr::inventory( tag="this_tag_doesnt_exist", delete = T )
# Test with extra arg
freezr::inventory( tag="my_dummy_output_2", filename=f1, extra = paste( "This file is just some dummy output",
                                                                        "but if it were from one of my scientific projects",
                                                                        "I would ideally write about its origin, the processing it ",
                                                                        "has undergone, and its purpose." ) )
assertthat::are_equal(f1_abs, inventory( tag = "my_dummy_output" ))
assertthat::are_equal(f1_abs, inventory( tag = "my_dummy_output_2" ))
assertthat::are_equal(f1_abs, inventory( tag = "my_dummy_output_3" ))

# # New functions # #
testthat::test_that( "inventory_show errs when inv absent but works with make_new", {
  clean_up_inv()
  expect_error(freezr::inventory_show())
  freezr::inventory_show(make_new = TRUE)
} )
testthat::test_that( "inventory_find runs",  {freezr::inventory_find()} )


clean_up_inv()
testthat::test_that( "add works with inventory absent", {
  freezr::inventory_add( tag="my_dummy_output", filename=f1 )
})

testthat::test_that( "add works with dupes", {
  freezr::inventory_add( tag="my_dummy_dupe_tag", filename=f1 )
  expect_warning( freezr::inventory_add( tag="my_dummy_dupe_tag", filename=f1 ) )
})

testthat::test_that( "add works with extra args", {
  freezr::inventory_add( tag="my_dummy_output_2", filename = f1, parent_tag = "my_dummy_output_2" )
  freezr::inventory_add( tag="my_dummy_output_3", filename=f1, extra = paste( "This file is just some dummy output",
                                                                              "but if it were from one of my scientific projects",
                                                                              "I would ideally write about its origin, the processing it ",
                                                                              "has undergone, and its purpose." ) )
})

testthat::test_that( "get works with incorrect arg", {
  expect_warning(freezr::inventory_get( tag="this_tag_doesnt_exist" ))
})

testthat::test_that( "get works with correct args", {
  assertthat::are_equal(f1_abs, freezr::inventory_get( tag = "my_dummy_output" ))
  assertthat::are_equal(f1_abs, freezr::inventory_get( tag = "my_dummy_output_2" ))
  assertthat::are_equal(f1_abs, freezr::inventory_get( tag = "my_dummy_output_3" ))
})

testthat::test_that( "add/get interface properly with absolute filepaths", {
  freezr::inventory_add( tag="my_absolute_input", filename = f1_abs )
  expect_equal(f1_abs, freezr::inventory_get( tag = "my_absolute_input" ))
})
clean_up_inv()
