# # Helpful stuff # #

library(testthat)


# Files to use as guinea pigs
f1 = f1_rel = file.path( "inventory_guinea_pig", "user", "custom_user_output.txt" )
file_no_ext_rel = file.path( "inventory_guinea_pig", "user", "custom_user_output.txt" )
folder_rel      = file.path( "inventory_guinea_pig", "user", "file_without_extension" )
results_path = file.path("", "Users", "erickernfeld", "Desktop",
                         "software_projects", "freezr",
                         "tests", "testthat", "results")
f1_abs = file.path( results_path, f1 )

# Cleanup procedure from previous testing
clean_up_inv = function(){
  if(inventory_find(f1_abs, return_existence_logical = TRUE)){
    file.remove( inventory_find(f1_abs) )
  }
  if(inventory_find(dirname(f1_abs), return_existence_logical = TRUE)){
    file.remove( inventory_find(f1_abs) )
  }
}

# # Old functions # #
# # Testing
clean_up_inv()
test_that( "deprecated inventory call still works (Test with no args)",
                     {expect_warning(inventory())})
test_that( "deprecated inventory call still works (Test with essential args)",
                     {expect_warning(inventory( tag="my_dummy_output", filename=f1 ))})
test_that( "deprecated inventory call still works (Test with parent_tag arg)",
                     {expect_warning(inventory( tag="my_dummy_output_3",
                                                        filename = f1,
                                                        parent_tag = "my_dummy_output_2" ))})
test_that("deprecated inventory call still works (Test with incorrect arg)",
          {expect_warning(inventory( tag="this_tag_doesnt_exist", delete = T ))})
test_that("deprecated inventory call still works (Test with extra arg)",
          {expect_warning(inventory( tag="my_dummy_output_2", filename=f1, extra = paste( "This file is just some dummy output",
                                                                        "but if it were from one of my scientific projects",
                                                                        "I would ideally write about its origin, the processing it ",
                                                                        "has undergone, and its purpose." ) ) )})
assertthat::are_equal(f1_abs, inventory( tag = "my_dummy_output" ))
assertthat::are_equal(f1_abs, inventory( tag = "my_dummy_output_2" ))
assertthat::are_equal(f1_abs, inventory( tag = "my_dummy_output_3" ))

# # New functions # #
test_that( "inventory_show errs when inv absent but works and warns with make_new", {
  clean_up_inv()
  expect_error(inventory_show(inv_location = dirname( f1_abs )))
  expect_warning( inventory_show(make_new = TRUE, inv_location = dirname( f1_abs ) ) )
  clean_up_inv()
} )

test_that( "make works", {
  clean_up_inv()
  expect_error(inventory_make())
  inventory_make( results_path )
  clean_up_inv()
})

test_that( "find works", {
  clean_up_inv()
  inventory_make( results_path )
  assertthat::assert_that(inventory_find(f1_abs, return_existence_logical = T))
  assertthat::assert_that(inventory_find(dirname(f1_abs), return_existence_logical = T))
  inventory_find( )
  clean_up_inv()
  assertthat::assert_that(!inventory_find(f1_abs, return_existence_logical = T))
  assertthat::assert_that(!inventory_find(dirname(f1_abs), return_existence_logical = T))
  clean_up_inv()
})

test_that( "add works after make", {
  clean_up_inv()
  inventory_make( results_path )
  inventory_find( )
  inventory_add( tag="my_dummy_dupe_tag", filename=f1 )
  expect_warning( inventory_add( tag="my_dummy_dupe_tag", filename=f1 ) )
  clean_up_inv()
})

test_that( "add works with extra args", {
  clean_up_inv()
  inventory_make( results_path )
  inventory_add( tag="my_dummy_output_2", filename = f1, parent_tag = "my_dummy_output_2" )
  inventory_add( tag="my_dummy_output_3", filename=f1, extra = paste( "This file is just some dummy output",
                                                                              "but if it were from one of my scientific projects",
                                                                              "I would ideally write about its origin, the processing it ",
                                                                              "has undergone, and its purpose." ) )
  clean_up_inv()
})

test_that( "get works with incorrect arg", {
  clean_up_inv()
  inventory_make( results_path )
  expect_warning(inventory_get( tag="this_tag_doesnt_exist" ))
  clean_up_inv()
})

test_that( "get works with correct args", {
  clean_up_inv()
  inventory_make( results_path )
  inventory_add( tag="my_dummy_output_2", filename = f1, parent_tag = "my_dummy_output_2" )
  assertthat::are_equal(f1_abs, inventory_get( tag = "my_dummy_output_2" ))
  clean_up_inv()
})

test_that( "add/get interface properly with absolute filepaths", {
  clean_up_inv()
  inventory_make( results_path )
  inventory_add( tag="my_absolute_input", filename = f1_abs )
  expect_equal(f1_abs, inventory_get( tag = "my_absolute_input" ))
  clean_up_inv()
})

test_that( "_transfer works for folders and for files with and without extensions.", {
  transferred_inv_path = file.path( dirname( results_path ), "transferred_inv" )
  clean_up_inv()
  inventory_make( results_path )
  inventory_add( tag="f1_rel", filename = f1_rel )
  inventory_add( tag="file_no_ext", filename = file_no_ext_rel )
  inventory_add( tag="folder", filename = folder_rel )
  unlink(transferred_inv_path, recursive = T)
  inventory_transfer( target_location = transferred_inv_path, overwrite = F )
  expect_warning( inventory_transfer( target_location = transferred_inv_path, overwrite = T ),
                  regexp = "*inventory already exists*")
  clean_up_inv()
})
clean_up_inv()

