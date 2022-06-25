test_that("datagen works", {
  expect_equal(datagen() %>% dim, c(30, 2))
})
