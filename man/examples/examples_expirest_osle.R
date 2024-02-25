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
  expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                response_vbl = "Potency", time_vbl = "Month",
                batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500))
res1$Model.Type
res1$POI

# Expected results in res1$Model.Type
# $type.spec
# common.icpt  common.slp
#           1           1
#
# $type.acronym
# [1] "cics"

# Expected results in res1$POI
#     cics      dics dids.pmse      dids
# 26.22410  24.80030  23.66724  23.34184
# attr(,"side")
#     cics      dics dids.pmse      dids
#  "lower"   "lower"   "lower"   "lower"

# A model with different intercepts / different slopes (dids)
res2 <-
  expirest_osle(data = exp1[exp1$Batch %in% c("b4", "b5", "b8"), ],
                response_vbl = "Potency", time_vbl = "Month",
                batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500))
res2$Model.Type
res2$POI

# Expected results in res2$Model.Type
# $type.spec
# common.icpt  common.slp
#           0           0
#
# $type.acronym
# [1] "dids"

# Expected results in res2$POI
#     cics      dics dids.pmse      dids
# 28.12518  22.47939  15.72348  15.96453
# attr(,"side")
#     cics      dics dids.pmse      dids
#  "lower"   "lower"   "lower"   "lower"

# Moisture stability data (% (w/w)) of three batches of a drug product obtained
# over a 24 months period:
str(exp3)

# 'data.frame':	33 obs. of  3 variables:
# $ Batch   : Factor w/ 3 levels "b1","b2","b3": 1 1 1 1 1 1 1 1 1 1 ...
# $ Month   : num  0 1 2 3 3 6 6 12 12 24 ...
# $ Moisture: num  2.2 1.7 3.32 2.76 2.43 ...

# Estimation where it is not know a priori which limit is crossed first, i.e.
# letting the estimation be done for both specification limits.
res3 <-
  expirest_osle(
    data = exp3, response_vbl = "Moisture", time_vbl = "Month",
    batch_vbl = "Batch", sl = c(1.5, 3.5), sl_sf = c(2, 2),
    srch_range = c(0, 500), sf_option = "tight", ivl = "confidence",
    ivl_type = "two.sided", ivl_side = "both")
res3$Model.Type
res3$POI

# Expected results in res3$Model.Type
# $type.spec
# common.icpt  common.slp
#           1           1
#
# $type.acronym
# [1] "cics"

# Expected results in res3$POI
#     cics      dics dids.pmse      dids
# 45.34604  40.28562  21.73080  21.42596
# attr(,"side")
#    cics      dics dids.pmse      dids
# "upper"   "upper"   "upper"   "lower"

# Analysis with a single batch
res4 <-
  expirest_osle(
    data = exp3[exp3$Batch == "b1", ], response_vbl = "Moisture",
    time_vbl = "Month", batch_vbl = "Batch", sl = c(1.5, 3.5),
    sl_sf = c(2, 2), srch_range = c(0, 500), sf_option = "tight",
    ivl = "confidence", ivl_type = "two.sided", ivl_side = "both")

# Since only one batch is involved there is no model type. Nevertheless, the
# result is reported under the dids model name.
res4$Model.Type
res4$POI

# Expected results in res4$Model.Type
# $type.spec
# common.icpt  common.slp
#         NA           NA
#
# $type.acronym
# [1] "n.a."

# Expected results in res4$POI
# cics      dics dids.pmse      dids
#   NA        NA        NA  21.42596
# attr(,"side")
# cics      dics dids.pmse      dids
# "NA"      "NA"      "NA"   "lower"

# Unsuccessful estimations
# Intervals are wider than the specification limits (no intersection).
res5 <-
  expirest_osle(
    data = exp3, response_vbl = "Moisture", time_vbl = "Month",
    batch_vbl = "Batch", sl = 1.5, sl_sf = 2, srch_range = c(0, 500),
    sf_option = "tight", ivl = "prediction", ivl_type = "two.sided",
    ivl_side = "lower")
res5$POI

# (Expected) results in res5$POI
#    cics      dics dids.pmse      dids
#     NA        NA        NA        NA
# attr(,"side")
#    cics      dics dids.pmse      dids
# "lower"   "lower"   "lower"   "lower"

# Estimation may also fail because of an inappropriate 'srch_range' setting.
res6 <-
  expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                response_vbl = "Potency", time_vbl = "Month",
                batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 5))
res6$POI

# (Expected) results in res6$POI
#    cics      dics dids.pmse      dids
#     NA        NA        NA        NA
# attr(,"side")
#    cics      dics dids.pmse      dids
# "lower"   "lower"   "lower"   "lower"
