# Potency stability data (in % of label claim (LC)) of five batches of a drug
# product obtained over a 24 months period:
str(exp1)

# 'data.frame':	53 obs. of  3 variables:
# $ Batch  : Factor w/ 6 levels "b2","b3","b4",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Month  : num  0 1 3 3 6 6 12 12 24 24 ...
# $ Potency: num  101 101.3 99.8 99.2 99.5 ...

# Successful estimations
# A model with common intercepts / common slopes (cics)
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
#   Intercept.cics Intercept.dics Intercept.dids Intercept.dids.pmse Delta.cics
# 1       100.5669       100.3638       100.2491            100.2491          3
#   Delta.dics Delta.dids Delta.dids.pmse WCSL.cics WCSL.dics WCSL.dids
# 1          3          3               3  97.56688  97.36375  97.24914
#   WCSL.dids.pmse Exp.Spec.Report Exp.Spec Rel.Spec.Report Rel.Spec
# 1       97.24914              95    94.95              98    97.95
#   Shelf.Life.cics Shelf.Life.dics Shelf.Life.dids Shelf.Life.dids.pmse
# 1        14.07398        13.23176        13.34561             13.80695
#   POI.Model.cics POI.Model.dics POI.Model.dids POI.Model.dids.pmse
# 1        26.2241        24.8003       23.34184            23.66724

# A model with different intercepts / different slopes (dids)
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
#   Intercept.cics Intercept.dics Intercept.dids Intercept.dids.pmse Delta.cics
# 1       101.5498       100.4882       101.2594            101.2594          3
#   Delta.dics Delta.dids Delta.dids.pmse WCSL.cics WCSL.dics WCSL.dids
# 1          3          3               3  98.54976  97.48822  98.25938
#   WCSL.dids.pmse Exp.Spec.Report Exp.Spec Rel.Spec.Report Rel.Spec
# 1       98.25938              95    94.95              98    97.95
#   Shelf.Life.cics Shelf.Life.dics Shelf.Life.dids Shelf.Life.dids.pmse
# 1        13.03332        11.42141        7.619661             7.483223
#   POI.Model.cics POI.Model.dics POI.Model.dids POI.Model.dids.pmse
# 1       28.12518       22.47939       15.96453            15.72348

# Analysis with a single batch (i.e. the worst case batch of res2 above)
res3 <-
  expirest_wisle(data = exp1[exp1$Batch == "b8", ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                 sl_sf = 3, srch_range = c(0, 500))
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
#   Intercept.cics Intercept.dics Intercept.dids Intercept.dids.pmse Delta.cics
# 1             NA             NA       101.2594                  NA         NA
#   Delta.dics Delta.dids Delta.dids.pmse WCSL.cics WCSL.dics WCSL.dids
# 1         NA          3              NA        NA        NA  98.25938
#   WCSL.dids.pmse Exp.Spec.Report Exp.Spec Rel.Spec.Report Rel.Spec
# 1             NA              95    94.95              98    97.95
#   Shelf.Life.cics Shelf.Life.dics Shelf.Life.dids Shelf.Life.dids.pmse
# 1              NA              NA        7.619661                   NA
#   POI.Model.cics POI.Model.dics POI.Model.dids POI.Model.dids.pmse
# 1             NA             NA       15.96453                  NA

# Unsuccessful estimation
# The interval does not cross the limit (i.e. not within srch_range).
res4 <-
  expirest_wisle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 105,
                 sl_sf = 4, srch_range = c(0, 500), ivl_side = "upper")
res4$POI

# (Expected) results in res3$POI
#   Intercept.cics Intercept.dics Intercept.dids Intercept.dids.pmse Delta.cics
# 1       100.5669             NA             NA                  NA          3
#   Delta.dics Delta.dids Delta.dids.pmse WCSL.cics WCSL.dics WCSL.dids
# 1         NA         NA              NA  107.5669        NA        NA
#   WCSL.dids.pmse Exp.Spec.Report Exp.Spec Rel.Spec.Report Rel.Spec
# 1             NA             105   105.04              98    97.95
#   Shelf.Life.cics Shelf.Life.dics Shelf.Life.dids Shelf.Life.dids.pmse
# 1              NA              NA              NA                   NA
#   POI.Model.cics POI.Model.dics POI.Model.dids POI.Model.dids.pmse
# 1             NA             NA             NA                  NA

# Estimation may also fail because of an inappropriate 'srch_range' setting.
res5 <-
  expirest_wisle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                 sl_sf = 3, srch_range = c(0, 5))
res5$POI

# (Expected) results in res4$POI
#   Intercept.cics Intercept.dics Intercept.dids Intercept.dids.pmse Delta.cics
# 1       100.5669             NA             NA                  NA          3
#   Delta.dics Delta.dids Delta.dids.pmse WCSL.cics WCSL.dics WCSL.dids
# 1         NA         NA              NA  97.56688        NA        NA
#   WCSL.dids.pmse Exp.Spec.Report Exp.Spec Rel.Spec.Report Rel.Spec
# 1             NA              95    94.95              98    97.95
#   Shelf.Life.cics Shelf.Life.dics Shelf.Life.dids Shelf.Life.dids.pmse
# 1              NA              NA              NA                   NA
#   POI.Model.cics POI.Model.dics POI.Model.dids POI.Model.dids.pmse
# 1             NA             NA             NA                  NA
