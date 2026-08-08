# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(cpge)
test_that("graph_data retourne une structure correcte", {

  g <- data_graph()

  expect_type(g, "list")
  expect_named(g, c("nodes", "edges","only_domaine", "only_ecoles" ,"only_concours"))
})

test_that("nodes et edges sont des data frames", {

  g <- data_graph()

  expect_s3_class(g$nodes, "data.frame")
  expect_s3_class(g$edges, "data.frame")

})

test_that("graph_data refuse des données invalides", {

  expect_error(graph_data(NULL))

})

test_that("nodes have required columns", {

  g <- data_graph()

  expect_true(all(c("id", "label") %in% names(g$nodes)))

})


test_that("edges have required columns", {

  g <- data_graph()

  expect_true(all(c("from", "to") %in% names(g$edges)))

})
