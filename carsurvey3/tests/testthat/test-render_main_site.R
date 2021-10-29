render_main_site(dummy_data, "../rmarkdown/main")

test_that("site folder exists", expect_true("docs" %in% list.files("../")))

test_that("the output folder contains the right files",  {
  file_list <- list.files("../docs")
  
  expected_files <- c("site_libs", "index.html", "summary.html", "style.css")
  
  expect_true(setequal(expected_files, file_list))
})

rmarkdown::clean_site("../docs")