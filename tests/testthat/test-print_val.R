context("Printing of formatted values")

test_that("print_val_succeeds", {
  tmp1 <- print_val(val_name = "SL: ", val_value = 90.5, val_unit = "%",
                   val_sf = 3, prefix = "L", suffix = "  ")
  tmp2 <- print_val(val_name = "SL: ", val_value = 90.5, val_unit = "%",
                   val_sf = 3, prefix = "L")
  tmp3 <- print_val(val_name = "", val_value = 90.5437879, val_unit = "",
                    val_sf = 3)

  # <-><-><-><->

  expect_equal(nchar(tmp1), 12)
  expect_equal(tmp1, "LSL: 90.5%  ")

  expect_equal(nchar(tmp2), 10)
  expect_equal(tmp2, "LSL: 90.5%")

  expect_equal(nchar(tmp3), 4)
  expect_equal(tmp3, "90.5")
})

test_that("print_val_fails", {
  expect_error(
    print_val(val_name = 1, val_value = 90.5, val_unit = "%",
              val_sf = 4, prefix = "L", suffix = "  "),
    "val_name must be a string")
  expect_error(
    print_val(val_name = "SL: ", val_value = "90.5", val_unit = "%",
              val_sf = 4, prefix = "L", suffix = "  "),
    "val_value must be a numeric value of length 1")
  expect_error(
    print_val(val_name = "SL: ", val_value = c(90.5, 100.4), val_unit = "%",
              val_sf = 4, prefix = "L", suffix = "  "),
    "val_value must be a numeric value of length 1")
  expect_error(
    print_val(val_name = "SL: ", val_value = 90.5, val_unit = 1,
              val_sf = 4, prefix = "L", suffix = "  "),
    "val_unit must be a string")
  expect_error(
    print_val(val_name = "SL: ", val_value = 90.5, val_unit = "%",
              val_sf = "4", prefix = "L", suffix = "  "),
    "val_sf must be a positive integer")
  expect_error(
    print_val(val_name = "SL: ", val_value = 90.5, val_unit = "%",
              val_sf = c(2, 4), prefix = "L", suffix = "  "),
    "val_sf must be a positive integer")
  expect_error(
    print_val(val_name = "SL: ", val_value = 90.5, val_unit = "%",
              val_sf = 4.2, prefix = "L", suffix = "  "),
    "val_sf must be a positive integer")
  expect_error(
    print_val(val_name = "SL: ", val_value = 90.5, val_unit = "%",
              val_sf = -4, prefix = "L", suffix = "  "),
    "val_sf must be a positive integer")
  expect_error(
    print_val(val_name = "SL: ", val_value = 90.5, val_unit = "%",
              val_sf = 4, prefix = 1, suffix = "  "),
    "prefix must be a string")
  expect_error(
    print_val(val_name = "SL: ", val_value = 90.5, val_unit = "%",
              val_sf = 4, prefix = "L", suffix = 1),
    "suffix must be a string")
})
