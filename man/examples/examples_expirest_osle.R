# Successful estimations
# A model with common intercepts / common slopes (cics)
res1 <-
  expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                response_vbl = "Potency", time_vbl = "Month",
                batch_vbl = "Batch", sl = 95, sl_sf = 3,
                srch_range = c(0, 500), sf_option = "loose")
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

# The parameter settings sf_option = "loose" and ivl_side = "lower" (the
# default setting of ivl_side) cause the specification limit of 95.0
# (sl_sf = 3, i.e. 3 significant digits) to be reduced by 0.05, i.e. the
# actual specification limit is 94.95.

# A model with different intercepts / different slopes (dids)
res2 <-
  expirest_osle(data = exp1[exp1$Batch %in% c("b4", "b5", "b8"), ],
                response_vbl = "Potency", time_vbl = "Month",
                batch_vbl = "Batch", sl = 95, sl_sf = 3,
                srch_range = c(0, 500), sf_option = "loose")
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

# Estimation where it is not know a priori which limit is crossed first, i.e.
# letting the estimation be done for both specification limits.
res3 <-
  expirest_osle(
    data = exp3, response_vbl = "Moisture", time_vbl = "Month",
    batch_vbl = "Batch", sl = c(1.5, 3.5), sl_sf = c(2, 2),
    srch_range = c(0, 500), sf_option = "loose", ivl = "confidence",
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
# 46.85172  41.84802  22.41808  22.50966
# attr(,"side")
#     cics      dics dids.pmse      dids
#  "upper"   "upper"   "upper"   "lower"

# The parameter settings sf_option = "loose" and ivl_side = "both" (the
# default setting of ivl_side) cause the specification limits of 1.5 and 3.5
# (sl_sf = 2, i.e. 2 significant digits) to be reduced by 0.05 and increased
# by 0.04, respectively, i.e. the actual specification limits are 1.45 and
# 3.54, respectively.

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
\dontrun{
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
                  batch_vbl = "Batch", sl = 95, sl_sf = 3,
                  srch_range = c(0, 5))
  res6$POI

  # (Expected) results in res6$POI
  #    cics      dics dids.pmse      dids
  #     NA        NA        NA        NA
  # attr(,"side")
  #    cics      dics dids.pmse      dids
  # "lower"   "lower"   "lower"   "lower"
}
