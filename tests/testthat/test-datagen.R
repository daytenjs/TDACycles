test_that("datagen works", {
  expect_equal(datagen() %>% dim, c(30, 2))
})

# TODO: Add tests for inputs
# test_that("str_split_one() errors if input length > 1", {
#   expect_error(str_split_one(c("a,b","c,d"), ","))
# })

#test_that(expect_equal, expect_error)
