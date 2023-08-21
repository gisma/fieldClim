context("Turbulent Flux")

# Test data
ah <- 10 # measurement height
h <- 1.2 # obstacle height in m
vh <- 1.2 # vegetation height in m
v <- 3.5 # wind velocity
z0 <- 0.92 # roughness length
rh1 <- 89 # relative humidity height 1
rh2 <- 88 # relative humidity height 2
p0 <- 1013.25
p <- 1000
p1 <- 1000
p2 <- 1000
t1 <- 25
t2 <- 21
z1 <- 2
z2 <- 10
v1 <- 1
v2 <- 2.3
ludi <- 1.225
grad_rich_no = -0.6230135


test_that("turb_flux_grad_rich_no", {
  expect_equal(turb_flux_grad_rich_no(t1, t2, z1, z2, v1, v2, p1, p2),
    -7.137048728,
    tolerance = 1e-5
  )
})
# old value: 0.6230135

test_that("turb_flux_monin", {
  expect_equal(
    turb_flux_monin(
      grad_rich_no = grad_rich_no,
      z1, z2, z0, v1, v2, t1, t2
    ),
    -3.210203,
    tolerance = 1e-4
  )
})
