# Fit models of different type
res1 <-
  expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                response_vbl = "Potency", time_vbl = "Month",
                batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500))
res2 <-
  expirest_osle(data = exp1[exp1$Batch %in% c("b3", "b4", "b5"), ],
                response_vbl = "Potency", time_vbl = "Month",
                batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500))
res3 <-
  expirest_osle(data = exp1[exp1$Batch %in% c("b4", "b5", "b8"), ],
                response_vbl = "Potency", time_vbl = "Month",
                batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500))

summary(res1)
# Expected output of summary(res1)
# Summary of shelf life estimation following the ICH Q1E guideline
#
# The best model accepted at a significance level of 0.25 has
# Common intercepts and Common slopes (acronym: cics).
#
# Worst case intercept: 100.5669 (Potency)
# Worst case batch: NA
# Estimated shelf life for cics model:  26.2241 (Month)
#
# Worst case intercepts, POIs and batches of all models
# (Including information about the side where the confidence
#   interval crosses the specification boundary):
#           Intercept      POI  Side Batch
# cics       100.5669  26.2241 lower    NA
# dics       100.3638  24.8003 lower    b2
# dids.pmse  100.7819 23.66724 lower    b5
# dids       100.7819 23.34184 lower    b5

summary(res2)
# Expected output of summary(res2)
# Summary of shelf life estimation following the ICH Q1E guideline
#
# The best model accepted at a significance level of 0.25 has
# Different intercepts and Common slopes (acronym: dics).
#
# Worst case intercept:   100.82 (Potency)
# Worst case batch: b5
# Estimated shelf life for dics model: 23.60194 (Month)
#
# Worst case intercepts, POIs and batches of all models
# (Including information about the side where the confidence
#   interval crosses the specification boundary):
#           Intercept      POI  Side Batch
# cics       102.0513 29.18093 lower    NA
# dics         100.82 23.60194 lower    b5
# dids.pmse  100.7819 22.49726 lower    b5
# dids       102.3841 23.26251 lower    b3

summary(res3)
# Expected output of summary(res3)
# Summary of shelf life estimation following the ICH Q1E guideline
#
# The best model accepted at a significance level of 0.25 has
# Different intercepts and Different slopes (acronym: dids).
#
# Worst case intercept: 101.2594 (Potency)
# Worst case batch: b8
# Estimated shelf life for dids model: 15.96453 (Month)
#
# Worst case intercepts, POIs and batches of all models
# (Including information about the side where the confidence
#   interval crosses the specification boundary):
#           Intercept      POI  Side Batch
# cics       101.5498 28.12518 lower    NA
# dics       100.4882 22.47939 lower    b8
# dids.pmse  101.2594 15.72348 lower    b8
# dids       101.2594 15.96453 lower    b8
