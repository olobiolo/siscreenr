
df <- Reduce(rbind, by(iris, iris$Species, function(x) x[sample(5), ]))


df1 <- df
df1[df$Species == "setosa", "Sepal.Length"] <- NA
df2 <- df1
df2[df$Species == "setosa", "Sepal.Width"] <- NA
df3 <- df1
df3[df$Species == "virginica", "Sepal.Width"] <- NA



test_that("single column and single predicate, string", {
  expect_identical(mask_values(df,
                               "Sepal.Length",
                               "Species == 'setosa'"),
                   df1)
})
test_that("single column and single predicate, bare", {
  expect_identical(mask_values(df,
                               "Sepal.Length",
                               Species == 'setosa'),
                   df1)
})

test_that("multiple columns and single predicate, string", {
  expect_identical(mask_values(df,
                               c("Sepal.Length", "Sepal.Width"),
                               "Species == 'setosa'"),
                   df2)
})
test_that("multiple columns and single predicate, bare", {
  expect_identical(mask_values(df,
                               c("Sepal.Length", "Sepal.Width"),
                               Species == 'setosa'),
                   df2)
})

test_that("multiple columns and multiple predicates, string", {
  expect_identical(mask_values(df,
                               c("Sepal.Length", "Sepal.Width"),
                               list('Species == "setosa"', 'Species == "virginica"')),
                   df3)
})
test_that("multiple columns and multiple predicates, bare", {
  expect_identical(mask_values(df,
                               c("Sepal.Length", "Sepal.Width"),
                               list(Species == "setosa", Species == "virginica")),
                   df3)
})


test_that("conditions are signalled", {
  expect_error(mask_values(letters, "Sepal.Length", Species == "setosa"))
  expect_error(mask_values(df, "sepal.length", Species == "setosa"))
  expect_warning(mask_values(df, c("Sepal.Length", "sepal.length"), Species == "setosa"))

})
