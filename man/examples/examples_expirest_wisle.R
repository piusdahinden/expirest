# Potency stability data (in % of label claim (LC)) of five batches of a drug
# product obtained over a 24 months period:
str(exp1)

# 'data.frame':	53 obs. of  3 variables:
# $ Batch  : Factor w/ 6 levels "b2","b3","b4",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Month  : num  0 1 3 3 6 6 12 12 24 24 ...
# $ Potency: num  101 101.3 99.8 99.2 99.5 ...

# Successful estimations
res1 <-
  expirest_wisle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                 sl_sf = 3, srch_range = c(0, 500))
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
#  Intercept.cics Intercept.dics Intercept.dids Delta.cics Delta.dics
#        100.5669       100.3638       100.2491          3          3
#  Delta.dids WCSL.cics WCSL.dics WCSL.dids Exp.Spec.Report Exp.Spec
#           3  97.56688  97.36375  97.24914              95    94.95
#  Rel.Spec.Report Rel.Spec Shelf.Life.cics Shelf.Life.dics Shelf.Life.dids
#               98    97.95        14.07398        13.23176        13.34561
#  POI.Model.cics POI.Model.dics POI.Model.dids
#         26.2241        24.8003       23.34184

res2 <-
  expirest_wisle(data = exp1[exp1$Batch %in% c("b4", "b5", "b8"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                 sl_sf = 3, srch_range = c(0, 500))
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
#  Intercept.cics Intercept.dics Intercept.dids Delta.cics Delta.dics
#        101.5498       100.4882       101.2594          3          3
#  Delta.dids WCSL.cics WCSL.dics WCSL.dids Exp.Spec.Report Exp.Spec
#           3  98.54976  97.48822  98.25938              95    94.95
#  Rel.Spec.Report Rel.Spec Shelf.Life.cics Shelf.Life.dics Shelf.Life.dids
#               98    97.95        13.03332        11.42141        7.619661
#  POI.Model.cics POI.Model.dics POI.Model.dids
#        28.12518       22.47939       15.96453

# Unsuccessful estimation
\dontrun{
  res3 <-
    expirest_wisle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                   response_vbl = "Potency", time_vbl = "Month",
                   batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                   sl_sf = 3, srch_range = c(0, 500), alpha = 1E-16)
  res3$POI
}

# (Expected) results in res3$POI
#  Intercept.cics Intercept.dics Intercept.dids Delta.cics Delta.dics
#        100.5669             NA             NA          3         NA
#  Delta.dids WCSL.cics WCSL.dics WCSL.dids Exp.Spec.Report Exp.Spec
#          NA  97.56688        NA        NA              95    94.95
#  Rel.Spec.Report Rel.Spec Shelf.Life.cics Shelf.Life.dics Shelf.Life.dids
#               98    97.95              NA              NA              NA
#  POI.Model.cics POI.Model.dics POI.Model.dids
#        14.42089       2.480635             NA

# Estimation may also fail because of an inappropriate 'srch-range' setting.
\dontrun{
  res4 <-
    expirest_wisle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                   response_vbl = "Potency", time_vbl = "Month",
                   batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                   sl_sf = 3, srch_range = c(0, 5))
  res4$POI
}

# (Expected) results in res4$POI
#  Intercept.cics Intercept.dics Intercept.dids Delta.cics Delta.dics
#        100.5669             NA             NA          3         NA
#  Delta.dids WCSL.cics WCSL.dics WCSL.dids Exp.Spec.Report Exp.Spec
#          NA  97.56688        NA        NA              95    94.95
#  Rel.Spec.Report Rel.Spec Shelf.Life.cics Shelf.Life.dics Shelf.Life.dids
#               98    97.95              NA              NA              NA
#  POI.Model.cics POI.Model.dics POI.Model.dids
#              NA             NA             NA
