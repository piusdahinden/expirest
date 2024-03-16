# Successful estimations
# A model with common intercepts / common slopes (cics)
res1 <-
  expirest_wisle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                 sl_sf = 3, srch_range = c(0, 500), sf_option = "loose")
res1$Model.Type
res1$POI

# Expected results in res1$Model.Type
# $type.spec
# common.icpt  common.slp
#           1           1
#
# $type.acronym
# [1] "cics"

# (Expected) results in res1$POI
# Intercept.cics Intercept.dics Intercept.dids Intercept.dids.pmse Delta.cics
#       100.5669       100.3638       100.2491            100.2491          3
# Delta.dics Delta.dids Delta.dids.pmse WCSL.cics WCSL.dics WCSL.dids
#          3          3               3  97.56688  97.36375  97.24914
# WCSL.dids.pmse Exp.Spec.Report Exp.Spec Rel.Spec.Report Rel.Spec
#       97.24914              95    94.95              98    97.95
# Shelf.Life.cics Shelf.Life.dics Shelf.Life.dids Shelf.Life.dids.pmse
#        14.07398        13.23176        13.34561             13.80695
# POI.Model.cics POI.Model.dics POI.Model.dids POI.Model.dids.pmse
#        26.2241        24.8003       23.34184            23.66724

# The parameter settings sf_option = "loose" and ivl_side = "lower" (the
# default setting of ivl_side) cause the specification limit of 95.0
# (sl_sf = 3, i.e. 3 significant digits) to be reduced by 0.05, i.e. the
# actual specification limit is 94.95. Analogously, the release limit of 98.0
# is reduced to 97.95.

# A model with different intercepts / different slopes (dids)
res2 <-
  expirest_wisle(data = exp1[exp1$Batch %in% c("b4", "b5", "b8"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                 sl_sf = 3, srch_range = c(0, 500), sf_option = "loose")
res2$Model.Type
res2$POI

# Expected results in res2$Model.Type
# $type.spec
# common.icpt  common.slp
#           0           0
#
# $type.acronym
# [1] "dids"

# (Expected) results in res2$POI
# Intercept.cics Intercept.dics Intercept.dids Intercept.dids.pmse Delta.cics
#       101.5498       100.4882       101.2594            101.2594          3
# Delta.dics Delta.dids Delta.dids.pmse WCSL.cics WCSL.dics WCSL.dids
#          3          3               3  98.54976  97.48822  98.25938
# WCSL.dids.pmse Exp.Spec.Report Exp.Spec Rel.Spec.Report Rel.Spec
#       98.25938              95    94.95              98    97.95
# Shelf.Life.cics Shelf.Life.dics Shelf.Life.dids Shelf.Life.dids.pmse
#        13.03332        11.42141        7.619661             7.483223
# POI.Model.cics POI.Model.dics POI.Model.dids POI.Model.dids.pmse
#       28.12518       22.47939       15.96453            15.72348

# Analysis with a single batch (i.e. the worst case batch of res2 above)
res3 <-
  expirest_wisle(data = exp1[exp1$Batch == "b8", ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                 sl_sf = 3, srch_range = c(0, 500), sf_option = "loose")
res3$Model.Type
res3$POI

# Since only one batch is involved there is no model type. Nevertheless, the
# result is reported under the dids model name.

# Expected results in res3$Model.Type
# $type.spec
# common.icpt  common.slp
#         NA           NA
#
# $type.acronym
# [1] "n.a."

# (Expected) results in res3$POI
# Intercept.cics Intercept.dics Intercept.dids Intercept.dids.pmse Delta.cics
#             NA             NA       101.2594                  NA         NA
# Delta.dics Delta.dids Delta.dids.pmse WCSL.cics WCSL.dics WCSL.dids
#         NA          3              NA        NA        NA  98.25938
# WCSL.dids.pmse Exp.Spec.Report Exp.Spec Rel.Spec.Report Rel.Spec
#             NA              95    94.95              98    97.95
# Shelf.Life.cics Shelf.Life.dics Shelf.Life.dids Shelf.Life.dids.pmse
#              NA              NA        7.619661                   NA
# POI.Model.cics POI.Model.dics POI.Model.dids POI.Model.dids.pmse
#             NA             NA       15.96453                  NA

# Unsuccessful estimation
\dontrun{
  # The interval does not cross the limit (i.e. not within srch_range).
  res4 <-
    expirest_wisle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                   response_vbl = "Potency", time_vbl = "Month",
                   batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 105,
                   sl_sf = 4, srch_range = c(0, 500), sf_option = "loose",
                   ivl_side = "upper")
  res4$POI

  # (Expected) results in res3$POI
  # Intercept.cics Intercept.dics Intercept.dids Intercept.dids.pmse Delta.cics
  #       100.5669             NA             NA                  NA          3
  # Delta.dics Delta.dids Delta.dids.pmse WCSL.cics WCSL.dics WCSL.dids
  #         NA         NA              NA  107.5669        NA        NA
  # WCSL.dids.pmse Exp.Spec.Report Exp.Spec Rel.Spec.Report Rel.Spec
  #             NA             105   105.04              98    98.04
  # Shelf.Life.cics Shelf.Life.dics Shelf.Life.dids Shelf.Life.dids.pmse
  #              NA              NA              NA                   NA
  # POI.Model.cics POI.Model.dics POI.Model.dids POI.Model.dids.pmse
  #             NA             NA             NA                  NA

  # Estimation may also fail because of an inappropriate 'srch_range' setting.
  res5 <-
    expirest_wisle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                   response_vbl = "Potency", time_vbl = "Month",
                   batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                   sl_sf = 3, srch_range = c(0, 5), sf_option = "loose")
  res5$POI

  # (Expected) results in res4$POI
  # Intercept.cics Intercept.dics Intercept.dids Intercept.dids.pmse Delta.cics
  #       100.5669             NA             NA                  NA          3
  # Delta.dics Delta.dids Delta.dids.pmse WCSL.cics WCSL.dics WCSL.dids
  #         NA         NA              NA  97.56688        NA        NA
  # WCSL.dids.pmse Exp.Spec.Report Exp.Spec Rel.Spec.Report Rel.Spec
  #             NA              95    94.95              98    97.95
  # Shelf.Life.cics Shelf.Life.dics Shelf.Life.dids Shelf.Life.dids.pmse
  #              NA              NA              NA                   NA
  # POI.Model.cics POI.Model.dics POI.Model.dids POI.Model.dids.pmse
  #             NA             NA             NA                  NA
}
