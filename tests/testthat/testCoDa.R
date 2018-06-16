library(CoDa)

# Test model fitting
M <- CoDa(CoDa.data, x = 0:110, y = 1960:2014)
vsn <- 1e-200

test_that("Test model fitting",{
  expect_s3_class(M, "CoDa")
  expect_output(print(M))
  expect_output(print(summary(M)))
  expect_true(all(fitted(M) >= 0))
  expect_true(all(round(colSums(fitted(M)), vsn) == 1)) # we apply a very small rounding to avoid a false negative
  expect_false(all(is.infinite(fitted(M))))
  expect_false(all(is.na(coef(M))))
  expect_false(all(is.na(resid(M))))
  expect_equal(nrow(M$input$dx), length(M$input$x))
  expect_equal(ncol(M$input$dx), length(M$input$y))
  expect_identical(dim(fitted(M)), dim(resid(M)), dim(M$input$dx))
})


# Test model prediction
P <- predict(M, n = 20)

test_that("Test model prediction", {
  expect_s3_class(P, "predict.CoDa")
  expect_output(print(P))
  expect_true(all(P$predicted.values$mean >= 0))
  expect_true(all(P$predicted.values$L80 >= 0))
  expect_true(all(P$predicted.values$L95 >= 0))
  expect_true(all(P$predicted.values$U80 >= 0))
  expect_true(all(P$predicted.values$U95 >= 0))
  expect_true(all(round(colSums(P$predicted.values$mean), vsn) == 1))
  expect_true(all(round(colSums(P$predicted.values$L80), vsn) == 1))
  expect_true(all(round(colSums(P$predicted.values$L95), vsn) == 1))
  expect_true(all(round(colSums(P$predicted.values$U80), vsn) == 1))
  expect_true(all(round(colSums(P$predicted.values$U95), vsn) == 1))
  expect_equal(length(P$years), ncol(P$predicted.values$mean), nrow(P$kt))
})
