# Potency stability data (in % of label claim (LC)) of five batches of a drug
# product obtained over a 24 months period:
str(exp1)

# 'data.frame':	53 obs. of  3 variables:
# $ Batch  : Factor w/ 6 levels "b2","b3","b4",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Month  : num  0 1 3 3 6 6 12 12 24 24 ...
# $ Potency: num  101 101.3 99.8 99.2 99.5 ...

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
# Worst case intercept: 100.5669
# Worst case batch: NA
#
# Estimated shelf life for cics model:  26.2241

summary(res2)
# Expected output of summary(res2)
# Summary of shelf life estimation following the ICH Q1E guideline
#
# The best model accepted at a significance level of 0.25 has
# Different intercepts and Common slopes (acronym: dics).
#
# Worst case intercept:   100.82
# Worst case batch: b5
#
# Estimated shelf life for dics model: 23.60194

summary(res3)
# Expected output of summary(res3)
# Summary of shelf life estimation following the ICH Q1E guideline
#
# The best model accepted at a significance level of 0.25 has
# Different intercepts and Different slopes (acronym: dids).
#
# Worst case intercept: 101.2594
# Worst case batch: b8
#
# Estimated shelf life for dids model: 15.96453
