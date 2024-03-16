# Performing an "ordinary shelf life estimation" (osle)
res1 <-
  expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                response_vbl = "Potency", time_vbl = "Month",
                batch_vbl = "Batch", sl = 95, sl_sf = 3,
                srch_range = c(0, 500), sf_option = "loose")

# The 'expirest_osle' object can be passed on to the plot_expirest_osle()
# function. This function does not produce any output but returns a
# 'plot_expirest_osle' object.
\dontrun{
  gg1 <- plot_expirest_osle(
    model = res1, response_vbl_unit = "%", x_range = NULL, y_range = c(93, 105),
    mtbs = "verified", plot_option = "full", ci_app = "line")
  gg2 <- print(gg1)

  # The print() function returns the 'plot_expirest_osle' object invisibly.
  class(gg1)
  class(gg2)
}
