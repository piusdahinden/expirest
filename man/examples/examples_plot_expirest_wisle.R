# Successful estimation
r.exp <-
  expirest_wisle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                 sl_sf = 3, srch_range = c(0, 500), alpha = 0.05,
                 alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
                 sf_option = "tight", ivl = "confidence",
                 ivl_type = "one.sided", side = "lower")

# Graphical display
tmp <- plot_expirest_wisle(
  model = r.exp, rl_index = 1, show_grouping = "yes",
  response_vbl_unit = "%", y_range = c(93, 107), x_range = NULL,
  scenario = "standard", plot_option = "full", ci_app = "ribbon")

# Results in tmp[["Expiry"]]
# Intercept.cics Intercept.dics Intercept.dids Delta.cics Delta.dics
#       100.5669       100.3638       100.2491          3          3
# Delta.dids WCSL.cics WCSL.dics WCSL.dids Exp.Spec.Report Exp.Spec
#          3  97.56688  97.36375  97.24914              95       95
# Rel.Spec.Report Rel.Spec Shelf.Life.cics Shelf.Life.dics
#              98       98        14.07398        13.23176
# Shelf.Life.dids POI.Model.cics POI.Model.dics POI.Model.dids
#        13.34561       25.99576       24.56688       23.14804

# Plotting the graph
# tmp[["Graph"]]
