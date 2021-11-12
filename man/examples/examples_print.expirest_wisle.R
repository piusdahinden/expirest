# Potency stability data (in % of label claim (LC)) of five batches of a drug
# product obtained over a 24 months period:
str(exp1)

# 'data.frame':	53 obs. of  3 variables:
# $ Batch  : Factor w/ 6 levels "b2","b3","b4",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Month  : num  0 1 3 3 6 6 12 12 24 24 ...
# $ Potency: num  101 101.3 99.8 99.2 99.5 ...

# Fit models of different type
res1 <-
  expirest_wisle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                 sl_sf = 3, srch_range = c(0, 500))

res2 <-
  expirest_wisle(data = exp1[exp1$Batch %in% c("b3", "b4", "b5"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                 sl_sf = 3, srch_range = c(0, 500))
res3 <-
  expirest_wisle(data = exp1[exp1$Batch %in% c("b4", "b5", "b8"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95,
                 sl_sf = 3, srch_range = c(0, 500))

res1
# Summary of shelf life estimation following the ARGPM
# guidance "Stability testing for prescription medicines"
#
# The best model accepted at a significance level of 0.25 has
# Common intercepts and Common slopes (acronym: cics).
#
# Worst case intercept(s): 100.5669
# Worst case batch(es): NA
#
# Estimated shelf life (lives) for cics model:
#      SL RL    wisle    osle
# cics 95 98 14.07398 26.2241
#
# Abbreviations:
#   ARGPM: Australian Regulatory Guidelines for Prescription Medicines;
#   ICH: International Council for Harmonisation; osle: Ordinary shelf life
#   estimation (i.e. following the ICH guidance); RL: Release Limit; SL:
#   Specification Limit; wisle: What-if (approach for) shelf life estimation
#   (i.e. following ARGPM guidance).

res2
# Summary of shelf life estimation following the ARGPM
# guidance "Stability testing for prescription medicines"
#
# The best model accepted at a significance level of 0.25 has
# Different intercepts and Common slopes (acronym: dics).
#
# Worst case intercept(s):   100.82
# Worst case batch(es): b5
#
# Estimated shelf life (lives) for dics model:
#      SL RL    wisle     osle
# cics 95 98 11.40993 23.60194
#
# Abbreviations:
#   ARGPM: Australian Regulatory Guidelines for Prescription Medicines;
#   ICH: International Council for Harmonisation; osle: Ordinary shelf life
#   estimation (i.e. following the ICH guidance); RL: Release Limit; SL:
#   Specification Limit; wisle: What-if (approach for) shelf life estimation
#   (i.e. following ARGPM guidance).

res3
# Summary of shelf life estimation following the ARGPM
# guidance "Stability testing for prescription medicines"
#
# The best model accepted at a significance level of 0.25 has
# Different intercepts and Different slopes (acronym: dids).
#
# Worst case intercept(s): 101.2594
# Worst case batch(es): b8
#
# Estimated shelf life (lives) for dids model:
#      SL RL    wisle     osle
# cics 95 98 7.619661 15.96453
#
# Abbreviations:
#   ARGPM: Australian Regulatory Guidelines for Prescription Medicines;
#   ICH: International Council for Harmonisation; osle: Ordinary shelf life
#   estimation (i.e. following the ICH guidance); RL: Release Limit; SL:
#   Specification Limit; wisle: What-if (approach for) shelf life estimation
#   (i.e. following ARGPM guidance).
