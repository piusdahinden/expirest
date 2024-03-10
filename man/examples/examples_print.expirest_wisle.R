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
# Expected output of print(res1)
# Summary of shelf life estimation following the ARGPM
# guidance "Stability testing for prescription medicines"
#
# The best model accepted at a significance level of 0.25 has
# Common intercepts and Common slopes (acronym: cics).
#
# Worst case intercept and batch:
#   RL Batch Intercept
# 1 98    NA  100.5669
#
# Estimated shelf lives for the cics model:
#   SL RL    wisle    osle
# 1 95 98 14.07398 26.2241
#
# Abbreviations:
#   ARGPM: Australian Regulatory Guidelines for Prescription Medicines;
#   ICH: International Council for Harmonisation;
#   osle: Ordinary shelf life estimation (i.e. following the ICH guidance);
#   RL: Release Limit;
#   SL: Specification Limit;
#   wisle: What-if (approach for) shelf life estimation (see ARGPM guidance).

res2
# Expected output of print(res2)
# Summary of shelf life estimation following the ARGPM
# guidance "Stability testing for prescription medicines"
#
# The best model accepted at a significance level of 0.25 has
# Different intercepts and Common slopes (acronym: dics).
#
# Worst case intercept and batch:
#   RL Batch Intercept
# 1 98    b5    100.82
#
# Estimated shelf lives for the dics model:
#   SL RL    wisle     osle
# 1 95 98 11.40993 23.60194
#
# Abbreviations:
#   ARGPM: Australian Regulatory Guidelines for Prescription Medicines;
#   ICH: International Council for Harmonisation;
#   osle: Ordinary shelf life estimation (i.e. following the ICH guidance);
#   RL: Release Limit;
#   SL: Specification Limit;
#   wisle: What-if (approach for) shelf life estimation (see ARGPM guidance).

res3
# Expected output of print(res3)
# Summary of shelf life estimation following the ARGPM
#   guidance "Stability testing for prescription medicines"
#
# The best model accepted at a significance level of 0.25 has
#  Different intercepts and Different slopes (acronym: dids).
#
# Worst case intercept and batch:
#   RL Batch Intercept
# 1 98    b8  101.2594
#
# Estimated shelf lives for the dids model (for information, the results of
#   the model fitted with pooled mean square error (pmse) are also shown:
#   SL RL    wisle wisle (pmse)     osle osle (pmse)
# 1 95 98 7.619661     7.483223 15.96453    15.72348
#
# Abbreviations:
#   ARGPM: Australian Regulatory Guidelines for Prescription Medicines;
#   ICH: International Council for Harmonisation;
#   osle: Ordinary shelf life estimation (i.e. following the ICH guidance);
#   pmse: Pooled mean square error;
#   RL: Release Limit;
#   SL: Specification Limit;
#   wisle: What-if (approach for) shelf life estimation (see ARGPM guidance).
