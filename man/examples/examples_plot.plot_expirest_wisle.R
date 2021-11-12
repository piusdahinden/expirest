# Potency stability data (in % of label claim (LC)) of five batches of a drug
# product obtained over a 24 months period:
str(exp1)

# 'data.frame':	53 obs. of  3 variables:
# $ Batch  : Factor w/ 6 levels "b2","b3","b4",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Month  : num  0 1 3 3 6 6 12 12 24 24 ...
# $ Potency: num  101 101.3 99.8 99.2 99.5 ...

# Performing a "what-if (approach for) shelf life estimation" (wisle)
res1 <-
  expirest_wisle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                 sl_sf = 3, srch_range = c(0, 500))

# The 'expirest_wisle' object can be passed on to the plot_expirest_wisle()
# function. This function does not produce any output but returns a
# 'plot_expirest_wisle' object.
gg1 <- plot_expirest_wisle(
  model = res1, rl_index = 1, show_grouping = "no",
  response_vbl_unit = "%", y_range = c(93, 107), x_range = NULL,
  scenario = "standard", plot_option = "full", ci_app = "line")
gg2 <- plot(gg1)

# The plot() function returns the 'plot_expirest_wisle' object invisibly.
class(gg1)
class(gg2)
