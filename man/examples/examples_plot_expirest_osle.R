# Successful estimation
r.exp <-
  expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                response_vbl = "Potency", time_vbl = "Month",
                batch_vbl = "Batch", sl = 95, sl_sf = 3,
                srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower")

# Graphical display
tmp <- plot_expirest_osle(
  model = r.exp, show_grouping = "no", response_vbl_unit = "%",
  y_range = c(93, 107), x_range = NULL, plot_option = "full",
  ci_app = "ribbon")

# Results in tmp[["Expiry"]]
# cics     dics     dids
# 25.99576 24.56688 23.14804

# Plotting the graph
# tmp[["Graph"]]
