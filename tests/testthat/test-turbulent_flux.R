# define values for test data
hum1 <- 89
hum2 <- 88
t1 <- 22
t2 <- 21
v1 <- 1
v2 <- 2.3
z1 <- 2
z2 <- 10
elev <- 270
grad_rich_no <- c(-1, 0, 1)

# test turb_flux_monin
test_that("turb_flux_monin", {
  expect_equal(
    turb_flux_monin(z1, z2, v1, v2, t1, t2, elev, surface_type = "field"),
    -12.7,
    tolerance = 1e-3
  )
  expect_equal(
    turb_flux_monin(z1, z2, v1, v2, t1, t2, elev, obs_height = 0.02),
    -12.7,
    tolerance = 1e-3
  )
})

# test turb_flux_stability
test_that("turb_flux_stability", {
  expect_equal(turb_flux_stability(grad_rich_no),
    c("unstable", "neutral", "stable")
  )
})

# test turb_flux_ex_quotient_temp
test_that("turb_flux_ex_quotient_temp", {
  expect_equal(
    turb_flux_ex_quotient_temp(t1, t2, z1, z2, v1, v2, elev, surface_type = "field"),
    0.0465,
    tolerance = 1e-3
  )
  expect_equal(
    turb_flux_ex_quotient_temp(t1, t2, z1, z2, v1, v2, elev, obs_height = 0.02),
    0.031,
    tolerance = 1e-3
  )
})

# test turb_flux_ex_quotient_imp
test_that("turb_flux_ex_quotient_imp", {
  expect_equal(
    turb_flux_ex_quotient_imp(t1, t2, z1, z2, v1, v2, elev, surface_type = "field"),
    0.03,
    tolerance = 1e-3
  )
  expect_equal(
    turb_flux_ex_quotient_imp(t1, t2, z1, z2, v1, v2, elev, obs_height = 0.02),
    0.02,
    tolerance = 1e-3
  )
})

# test turb_flux_imp_exchange
test_that("turb_flux_imp_exchange", {
  expect_equal(
    turb_flux_imp_exchange(t1, t2, v1, v2, z1, z2, elev, surface_type = "field"),
    0.00487,
    tolerance = 1e-3
  )
  expect_equal(
    turb_flux_imp_exchange(t1, t2, v1, v2, z1, z2, elev, obs_height = 0.02),
    0.00325,
    tolerance = 1e-3
  )
})
