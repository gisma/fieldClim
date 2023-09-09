slp <- 20 # slope of terrain in degrees

test_that("terr_sky_view", {
  expect_equal(terr_sky_view(slope = slp, valley = T),
    0.06030738,
    tolerance = 1e-4
  )

  expect_equal(terr_sky_view(slope = slp, valley = F),
    0.03015369,
    tolerance = 1e-4
  )
})
