# define values for test data
hum1 <- 89
hum2 <- 88
t1 <- 22
t2 <- 21
z1 <- 2
z2 <- 10
v1 <- 1
v2 <- 2.3
elev <- 270
rad_bal <- 400
soil_flux <- 40

# test sensible_bowen
test_that("sensible_bowen", {
  expect_equal(
    sensible_bowen(t1, t2, hum1, hum2, z1, z2, elev, rad_bal, soil_flux),
    -22.5,
    tolerance = 1e-2
  )
})

# test sensible_monin
test_that("sensible_monin", {
  expect_equal(
    sensible_monin(t1, t2, z1, z2, v1, v2, elev, surface_type = "field"),
    180,
    tolerance = 1e-3
  )
})

# test sensible_priestley_taylor
test_that("sensible_priestley_taylor", {
  expect_equal(sensible_priestley_taylor(t = t1, rad_bal, soil_flux, surface_type = "field"),
    -84.21671594,
    tolerance = 1e-3
  )
})
