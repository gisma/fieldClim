test_that("Potential Temperature", {
  expect_equal(temp_pot_temp(25, 1000),
    26.124,
    tolerance = 1e-3
  )
  # expect_equal(temp_pot_temp(c(25, NA), c(NA, 1000)),
  #             c(NA_integer_, NA_integer_))
})
