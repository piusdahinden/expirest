# Performing a "what-if (approach for) shelf life estimation" (wisle)
res1 <-
  expirest_wisle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                 sl_sf = 3, srch_range = c(0, 500))

# The 'expirest_wisle' object can be passed on to the plot_expirest_wisle()
# function. This function does not produce any output but returns a
# 'plot_expirest_wisle' object.
\dontrun{
  gg1 <- plot_expirest_wisle(
    model = res1, rl_index = 1, response_vbl_unit = "%", x_range = NULL,
    y_range = c(93, 105), scenario = "standard", mtbs = "verified",
    plot_option = "full", ci_app = "line")
  gg2 <- plot(gg1)

  # The plot() function returns the 'plot_expirest_wisle' object invisibly.
  class(gg1)
  class(gg2)
}
