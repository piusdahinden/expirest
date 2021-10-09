# Successful estimation
tmp <-
  expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                response_vbl = "Potency", time_vbl = "Month",
                batch_vbl = "Batch", sl = 95, sl_sf = 3,
                srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
                ivl = "confidence", ivl_type = "one.sided",
                ivl_side = "lower")

# (Expected) results in tmp[["POI"]]
#     cics     dics     dids
# 26.22410 24.80030 23.34184

tmp <-
  expirest_osle(data = exp3, response_vbl = "Moisture", time_vbl = "Month",
                batch_vbl = "Batch", sl = 3.5, sl_sf = 2,
                srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                ivl = "confidence", ivl_type = "two.sided",
                ivl_side = "upper")

# (Expected) results in tmp[["POI"]]
#     cics     dics     dids
# 45.34604 40.28562 23.76431

# Unsuccessful estimation
\dontrun{
  tmp <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = 1.5, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "tight", ivl_type = "two.sided", ivl_side = "lower",
      ivl = "prediction")
}

# (Expected) results in tmp[["POI"]]
#     cics dics dids
#       NA   NA   NA
