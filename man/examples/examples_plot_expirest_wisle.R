# Potency stability data (in % of label claim (LC)) of five batches of a drug
# product obtained over a 24 months period:
str(exp1)

# 'data.frame':	53 obs. of  3 variables:
# $ Batch  : Factor w/ 6 levels "b2","b3","b4",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Month  : num  0 1 3 3 6 6 12 12 24 24 ...
# $ Potency: num  101 101.3 99.8 99.2 99.5 ...

# Start by making a "what-if (approach for) shelf life estimation" (wisle)
res1 <-
  expirest_wisle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                 sl_sf = 3, srch_range = c(0, 500))

# Pass the 'expirest_wisle' object on to the plot_expirest_wisle() function.
# This function does not produce any output. It returns a 'plot_expirest_wisle'
# object that is essentially an 'expirest_wisle' object augmented by a 'ggplot'
# object.
gg1 <- plot_expirest_wisle(
  model = res1, rl_index = 1, show_grouping = "no",
  response_vbl_unit = "%", y_range = c(93, 107), x_range = NULL,
  scenario = "standard", plot_option = "full", ci_app = "line")
gg1

# Since the element gg1$Graph is a 'ggplot' object it can be used for further
# manipulation by aid of 'ggplot2' functions.
if (requireNamespace("ggplot2")) {
  library(ggplot2)

  gg1$Graph + labs(title = "What-if Shelf Life Estimation (WISLE)",
                   x = "Time [months]", y = "Potency [% LC]") +
    scale_x_continuous(limits = c(-5, 31), breaks = seq(0, 30, 6))
}

# Illustration of the grouping
gg2 <- plot_expirest_wisle(
  model = res1, rl_index = 1, show_grouping = "yes",
  response_vbl_unit = "%", y_range = c(93, 107), x_range = NULL,
  scenario = "standard", plot_option = "full", ci_app = "line")
gg2

# Repeat this for a different intercept / different slope (dids) model.
res2 <-
  expirest_wisle(data = exp1[exp1$Batch %in% c("b4", "b5", "b8"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                 sl_sf = 3, srch_range = c(0, 500))

gg3 <- plot_expirest_wisle(
  model = res2, rl_index = 1, show_grouping = "yes",
  response_vbl_unit = "%", y_range = c(89, 107), x_range = c(0, 26),
  scenario = "standard", plot_option = "full", ci_app = "ribbon")
gg3
