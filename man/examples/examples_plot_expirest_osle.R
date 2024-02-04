# Potency stability data (in % of label claim (LC)) of five batches of a drug
# product obtained over a 24 months period:
str(exp1)

# 'data.frame':	53 obs. of  3 variables:
# $ Batch  : Factor w/ 6 levels "b2","b3","b4",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Month  : num  0 1 3 3 6 6 12 12 24 24 ...
# $ Potency: num  101 101.3 99.8 99.2 99.5 ...

# Start by making an "ordinary shelf life estimation" (osle).
res1 <-
  expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                response_vbl = "Potency", time_vbl = "Month",
                batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500))

# Pass the 'expirest_osle' object on to the plot_expirest_osle() function.
# This function does not produce any output. It returns a 'plot_expirest_osle'
# object that is essentially an 'expirest_osle' object augmented by a 'ggplot'
# object.
gg1 <- plot_expirest_osle(
  model = res1, response_vbl_unit = "%", x_range = NULL, y_range = c(93, 105),
  mtbs = "verified", plot_option = "full", ci_app = "line")
gg1

# Since the element gg1$Graph is a 'ggplot' object it can be used for further
# manipulation by aid of 'ggplot2' functions.
if (requireNamespace("ggplot2")) {
  library(ggplot2)

  gg1$Graph + labs(title = "Ordinary Shelf Life Estimation (OSLE)",
                   x = "Time [months]", y = "Potency [% LC]") +
    scale_x_continuous(limits = c(-1, 31), breaks = seq(0, 30, 6))
}

# Repeat this for a different intercept / different slope (dids) model.
res2 <-
  expirest_osle(data = exp1[exp1$Batch %in% c("b4", "b5", "b8"), ],
                response_vbl = "Potency", time_vbl = "Month",
                batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500))

gg2 <- plot_expirest_osle(
  model = res2, response_vbl_unit = "%", x_range = c(0, 43),
  y_range = c(83, 107), mtbs = "verified", plot_option = "full",
  ci_app = "ribbon")
gg2

# In case of different intercept / different slope models, individually fit
# linear models are shown by default, i.e. with the 'mtbs' parameter set
# as "verified". To get the different intercept / different slope model
# displayed where the mean square error is pooled across batches, i.e. the
# dids.pmse model, the 'mtbs' parameter has to be set accordingly.

gg3 <- plot_expirest_osle(
  model = res2, response_vbl_unit = "%", x_range = c(0, 43),
  y_range = c(83, 107), mtbs = "dids.pmse", plot_option = "full",
  ci_app = "ribbon")
gg3
