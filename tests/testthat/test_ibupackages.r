library(ibufunctions)
context("capitalise first letter")

test_that("ibu_simple_cap is use to capitalise the first letter", {
  expect_equal(ibu_simple_cap("abc"), "Abc")
  expect_equal(ibu_simple_cap("ABC"), "Abc")
  expect_equal(ibu_simple_cap("aBC"), "Abc")
})

# simple dataframe for test purpose
x <- data.frame("S N" = 1:2, "Age" = c(21,15), "Name" = c("John","Dora"), stringsAsFactors = FALSE)

context("reformat column names")
test_that("kol_name is use to reformatted column name", {
  expect_that(x, is_a("data.frame"))
  expect_match(kol_name(x), "[a-z]")
})
