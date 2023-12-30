context("Ordinary shelf life estimation")

# The tests use the values published by LeBlond et al. (LeBlond 2011),
# i.e. in the various tables and figures that are presented in
# LeBlond, D., Griffith, D. and Aubuchon, K. Linear Regression 102:
# Stability Shelf Life Estimation Using Analysis of Covariance.
# J Valid Technol (2011) 17(3): 47-68.

test_that("expirest_osle_results_match_LeBlond_2011", {
  re <-
    expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                  response_vbl = "Potency", time_vbl = "Month",
                  batch_vbl = "Batch", sl = 95, sl_sf = 2,
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "one.sided",
                  ivl_side = "lower")

  # <-><-><-><->

  expect_equal(re[["Model.Type"]]$type.acronym, "cics")
  expect_equivalent(signif(re[["POI"]]["cics"], 12), 25.9957631369)
  expect_equivalent(signif(re[["Intercepts"]][["cics"]]$icpt, 12),
                    100.566878981)
  expect_equivalent(signif(re[["Models"]][["cics"]]$coefficients[2], 12),
                    -0.192993630573)
  expect_equal(signif(c(stats::anova(re[["Models"]][["dids"]],
                                     re[["Models"]][["dics"]],
                                     re[["Models"]][["cics"]])[3, "F"],
                        stats::anova(re[["Models"]][["dids"]],
                                     re[["Models"]][["dics"]])[2, "F"]), 12),
               c(0.435993484727, 0.228684731019))
  expect_equivalent(
    signif(summary(re[["Models"]][["cics"]])[["fstatistic"]][1], 12),
    129.051417200)

  # Results presented in LeBlond 2011
  # Table V and Figure 4
  # Model Type: cics
  # POI: 26
  # Regression Equation: y = 100.567 - 0.192994 time
  # ANCOVA - F value for Batch: 0.436
  # ANCOVA - F value for Batch * Time: 0.229
  # Analysis of Variance - F value for Regression: 129.051

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  re <-
    expirest_osle(data = exp1[exp1$Batch %in% c("b3", "b4", "b5"), ],
                  response_vbl = "Potency", time_vbl = "Month",
                  batch_vbl = "Batch", sl = 95, sl_sf = 2,
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "one.sided",
                  ivl_side = "lower")

  # <-><-><-><->

  expect_equal(re[["Model.Type"]]$type.acronym, "dics")
  expect_equivalent(signif(re[["POI"]]["dics"], 12), 23.3972651235)
  expect_equivalent(signif(re[["Intercepts"]][["dics"]]$icpt, 12),
                    c(102.175653109, 104.255189423, 100.820021871))
  expect_equivalent(signif(re[["Models"]][["dics"]]$coefficients[4], 12),
                    -0.213120866465)
  expect_equal(signif(c(stats::anova(re[["Models"]][["dids"]],
                                     re[["Models"]][["dics"]],
                                     re[["Models"]][["cics"]])[3, "F"],
                        stats::anova(re[["Models"]][["dids"]],
                                     re[["Models"]][["dics"]])[2, "F"]), 12),
               c(21.7380213596, 0.183108926252))
  expect_equivalent(
    signif(summary(re[["Models"]][["dics"]])[["fstatistic"]][1], 12),
    37.0144226385)

  # Results presented in LeBlond 2011
  # Table VII and Figure 6
  # Model Type: dics
  # POI: 23.4
  # Regression Equations:
  # Batch 3: y = 102.176 - 0.213121 time
  # Batch 4: y = 104.255 - 0.213121 time
  # Batch 5: y = 100.82 - 0.213121 time
  # ANCOVA - F value for Batch: 21.738
  # ANCOVA - F value for Batch * Time: 0.183
  # Analysis of Variance - F value for Regression: 37.0144

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  re <-
    expirest_osle(data = exp1[exp1$Batch %in% c("b4", "b5", "b8"), ],
                  response_vbl = "Potency", time_vbl = "Month",
                  batch_vbl = "Batch", sl = 95, sl_sf = 2,
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "one.sided",
                  ivl_side = "lower")

  # <-><-><-><->

  expect_equal(re[["Model.Type"]]$type.acronym, "dids")
  expect_equivalent(signif(re[["POI"]]["dids"], 12), 15.8448655130)
  expect_equivalent(signif(re[["Intercepts"]][["dids"]]$icpt, 12),
                    c(104.070645793, 100.781872268, 101.259375000))
  expect_equivalent(signif(c(re[["Models"]][["dids"]]$coefficients[4],
                             re[["Models"]][["dids"]]$coefficients[4] +
                               re[["Models"]][["dids"]]$coefficients[5:6]), 12),
                    c(-0.196151337247, -0.208608547839, -0.330208333333))
  expect_equal(signif(c(stats::anova(re[["Models"]][["dids"]],
                                     re[["Models"]][["dics"]],
                                     re[["Models"]][["cics"]])[3, "F"],
                        stats::anova(re[["Models"]][["dids"]],
                                     re[["Models"]][["dics"]])[2, "F"]), 12),
               c(72.1242158003, 1.95541944320))
  expect_equivalent(
    signif(summary(re[["Models"]][["dids"]])[["fstatistic"]][1], 12),
    49.8306556826)

  # Results presented in LeBlond 2011
  # Table IX and Figure 7
  # Model Type: dids
  # POI: 15.6
  # Regression Equations:
  # Batch 4: y = 104.071 - 0.196151 time
  # Batch 5: y = 100.782 - 0.208609 time
  # Batch 8: y = 101.259 - 0.330208 time
  # ANCOVA - F value for Batch: 72.124
  # ANCOVA - F value for Batch * Time: 1.955
  # Analysis of Variance - F value for Regression: 49.831
  #   Note that the POI of 15.6 is obtained when using the dids model. When
  #   the POI estimation is based on the models fitted to the individual
  #   batches, and using the result of the worst case batch, the estimated POI
  #   is 15.8. This procedure is the one that is used also by SAS JMP. In
  #   JMP 12.0.1, for this data set the POI is estimated as 15.844877978. The
  #   result differs from the fifth decimal place. This is due do the fact
  #   that the POI estimation is based on an iterative method. It is not
  #   calculated analytically.

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  re <-
    expirest_osle(data = exp2, response_vbl = "Related", time_vbl = "Month",
                  batch_vbl = "Batch", sl = 0.3, sl_sf = 2,
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "one.sided",
                  ivl_side = "upper")

  # <-><-><-><->

  expect_equal(re[["Model.Type"]]$type.acronym, "dids")
  expect_equivalent(signif(re[["POI"]]["dids"], 12), 15.8448655130)
  expect_equivalent(signif(re[["Intercepts"]][["dids"]]$icpt, 12),
                    c(0.027880626223, 0.126543831957, 0.112218750000))
  expect_equivalent(signif(c(re[["Models"]][["dids"]]$coefficients[4],
                             re[["Models"]][["dids"]]$coefficients[4] +
                               re[["Models"]][["dids"]]$coefficients[5:6]), 12),
                    c(0.005884540117, 0.006258256435, 0.009906250000))
  expect_equal(signif(c(stats::anova(re[["Models"]][["dids"]],
                                     re[["Models"]][["dics"]],
                                     re[["Models"]][["cics"]])[3, "F"],
                        stats::anova(re[["Models"]][["dids"]],
                                     re[["Models"]][["dics"]])[2, "F"]), 12),
               c(72.1242158003, 1.95541944320))
  expect_equivalent(
    signif(summary(re[["Models"]][["dids"]])[["fstatistic"]][1], 12),
    49.8306556826)

  # Results presented in LeBlond 2011
  # Table XII and Figure 8
  # Model Type: dids
  # POI: 15.61
  # Regression Equations:
  # Batch 4: y = 0.0278806 + 0.00588454 time
  # Batch 5: y = 0.126544 + 0.00625826 time
  # Batch 8: y = 0.112219 + 0.00990625 time
  # ANCOVA - F value for Batch: 72.124
  # ANCOVA - F value for Batch * Time: 1.955
  # Analysis of Variance - F value for Regression: 49.831

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  rel <-
    expirest_osle(data = exp3, response_vbl = "Moisture", time_vbl = "Month",
                  batch_vbl = "Batch", sl = c(1.5, 3.5), sl_sf = c(2, 2),
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "two.sided",
                  ivl_side = "lower")
  reu <-
    expirest_osle(data = exp3, response_vbl = "Moisture", time_vbl = "Month",
                  batch_vbl = "Batch", sl = c(1.5, 3.5), sl_sf = c(2, 2),
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "two.sided",
                  ivl_side = "upper")

  # <-><-><-><->

  expect_equal(rel[["Model.Type"]]$type.acronym, "cics")
  expect_equivalent(signif(rel[["POI"]]["cics"], 12), 50.7665191694)
  expect_equivalent(signif(reu[["POI"]]["cics"], 12), 45.3460440833)
  expect_equivalent(signif(reu[["Intercepts"]][["cics"]]$icpt, 12),
                    2.45678204711)
  expect_equivalent(signif(reu[["Models"]][["cics"]]$coefficients[2], 12),
                    0.002272338514)
  expect_equal(signif(c(stats::anova(reu[["Models"]][["dids"]],
                                     reu[["Models"]][["dics"]],
                                     reu[["Models"]][["cics"]])[3, "F"],
                        stats::anova(reu[["Models"]][["dids"]],
                                     reu[["Models"]][["dics"]])[2, "F"]), 12),
               c(0.353853883257, 0.748152729736))
  expect_equivalent(
    signif(summary(reu[["Models"]][["cics"]])[["fstatistic"]][1], 12),
    0.034723832039)

  # Results presented in LeBlond 2011
  # Table XIV and Figure 9
  # Model Type: cics
  # POI: 45.35
  # Regression Equations: 2.45678 + 0.0022724
  # ANCOVA - F value for Batch: 0.354
  # ANCOVA - F value for Batch * Time: 0.748
  # Analysis of Variance - F value for Regression: 0.034726
  #   Note that the value at the sixth decimal place of the F value differs
  #   from the corresponding value of the calculated value. Thus, the model
  #   was fitted also in JMP 12.0.1 where the F value was calculated as
  #   0.034723832039 which is exactly the same value that was calculated
  #   here (by R). Therefore, it must be concluded that the results presented
  #   in LeBlond 2011, Table XIV, must have been obtained with a data set
  #   that slightly deviates from the data set that is presented in Table XIII.
})

# The tests use the values published in the JMP(R) 12 Reliability and Survival
# Methods manual, i.e. in chapter Degradation, sub-chapter Stability Analysis
# on p. 174-176 of the manual, i.e. in SAS Institute Inc. 2015. JMP(R) 12
# Reliability and Survival Methods. Cary, NC: SAS Institute Inc.

test_that("expirest_osle_results_match_JMP_reliability_and_survival_methods_
          manual", {
  re <-
    expirest_osle(data = exp4, response_vbl = "Conc", time_vbl = "Month",
                  batch_vbl = "Batch", sl = 95, sl_sf = 2,
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "one.sided",
                  ivl_side = "lower")

  r_mod <- stats::lm(Conc ~ Month * Batch, data = exp4)
  usl <- 105
  lsl <- 95

  tmp <- find_poi(srch_range = c(0, 500), model = r_mod, sl = lsl,
                  alpha = 0.05, ivl_type = "one.sided",
                  ivl_side = "lower", ivl = "confidence")

  # <-><-><-><->

  expect_is(re, "expirest_osle")
  expect_equal(re[["Model.Type"]]$type.acronym, "dics")
  expect_equivalent(re$wc.batch, 2)
  expect_equivalent(signif(re[["POI"]], 12),
                    c(29.9856687174, 23.4750503440, 23.1159717387,
                      23.2441996167))
  expect_equal(signif(c(stats::anova(re[["Models"]][["dids"]],
                                     re[["Models"]][["cics"]])[2, "F"],
                        stats::anova(re[["Models"]][["dids"]],
                                     re[["Models"]][["dics"]],
                                     re[["Models"]][["cics"]])[3, "F"],
                        stats::anova(re[["Models"]][["dids"]],
                                     re[["Models"]][["dics"]])[2, "F"]), 12),
               c(10.1083606518, 19.8876404032, 0.329080900382))

  expect_equivalent(signif(re[["Intercepts"]][["cics"]]$icpt, 12),
                    101.906446205)
  expect_equivalent(signif(re[["Models"]][["cics"]]$coefficients[2], 12),
                    -0.190871685462)
  expect_equivalent(signif(re[["Intercepts"]][["dics"]]$icpt, 12),
                    c(101.833246643, 100.450143334, 102.066579977,
                      104.136572391))
  expect_equivalent(signif(re[["Models"]][["dics"]]$coefficients[5], 12),
                    -0.202213553211)
  expect_equivalent(signif(re[["Intercepts"]][["dids"]]$icpt, 12),
                    c(101.817460317, 100.249139280, 102.384126984,
                      104.070645793))
  expect_equivalent(signif(c(re[["Models"]][["dids"]]$coefficients[5],
                             re[["Models"]][["dids"]]$coefficients[5] +
                               re[["Models"]][["dids"]]$coefficients[6:8]), 12),
                    c(-0.200634920635, -0.180125195618153, -0.233968253968254,
                      -0.196151337247228))

  # Transcription of results shown in Figure 6.27 Stability Models
  # The best model accepted at the significance level of 0.25 has Different
  # intercepts and Common slopes. The model suggests the earliest crossing
  # time at 23.47505 with 95 percent confidence. ICH Guidelines indicate an
  # expiration time of 23.47505.
  # Display Intercept     Slope Earliest Crossing Time
  #       O Different Different               23.11596
  #       X Different    Common               23.47505
  #       O    Common    Common               29.98567
  #
  #   Note that when using the dids model for estimation of the POI as proposed
  #   in LeBlond 2011, a POI of 23.244185941565 is obtained, i.e. a value that
  #   is greater. There are examples, though, where the situation is the other
  #   way round, i.e. where the estimate that is obtained based on the dids
  #   model results in a smaller POI (see example in LeBlond 2011, Table IX).

  # Results from analysis in JMP 12.0.1 software, Degradation platform,
  # Stability Test tab
  # Worst case batch: 2_12 (from graphical output)
  #
  # Model Comparisons - F Statistic
  # Source Intercept	   Slope | Intercept   Slope F Statistic
  # A		   Different Different |    Common  Common    10.10836
  # B		   Different    Common |    Common  Common    19.88764
  # C		   Different Different | Different  Common    0.329081
  #
  # Model 1 - Simple Linear Path, Parameter Estimates
  # Parameter                      Estimate
  # Intercept[Batch Number=1_11]	101.8175
  # Slope[Batch Number=1_11]	     -0.20063
  # Intercept[Batch Number=2_12]	100.2491
  # Slope[Batch Number=2_12]	     -0.18013
  # Intercept[Batch Number=3_13]	102.3841
  # Slope[Batch Number=3_13]	     -0.23397
  # Intercept[Batch Number=4_14]	104.0706
  # Slope[Batch Number=4_14]	     -0.19615
  #
  # Model 2 - Simple Linear Path, Parameter Estimates
  # Parameter                      Estimate
  # Slope	                         -0.20221
  # Intercept[Batch Number=1_11]	101.8332
  # Intercept[Batch Number=2_12]	100.4501
  # Intercept[Batch Number=3_13]	102.0666
  # Intercept[Batch Number=4_14]	104.1366
  #
  # Model 3 - Simple Linear Path, Parameter Estimates
  # Parameter	                     Estimate
  # Intercept	                    101.9064
  # Slope	                         -0.19087
  #
  # Model 1 - Simple Linear Path, Parameter Estimates
  # Parameter	Batch Number	       Estimate
  # Intercept	 1_11                101.8175
  # Slope	     1_11                 -0.20063
  # Intercept	 2_12               100.2491
  # Slope	     2_12                -0.18013
  # Intercept	 3_13               102.3841
  # Slope	     3_13                -0.23397
  # Intercept	 4_14               104.0706
  # Slope	     4_14                -0.19615
})

test_that("expirest_osle_succeeds_for_poi", {
  usl <- 4.5
  lsl <- 0.5

  tmp <- rep(NA, 8)

  # <-><-><-><->

  tmp[1] <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = lsl, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "lower")[["POI"]]["cics"]
  tmp[2] <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = lsl, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "tight", ivl = "prediction",
      ivl_type = "one.sided", ivl_side = "lower")[["POI"]]["cics"]
  tmp[3] <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = usl, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper")[["POI"]]["cics"]
  tmp[4] <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = usl, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "tight", ivl = "prediction",
      ivl_type = "one.sided", ivl_side = "upper")[["POI"]]["cics"]
  tmp[5] <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = lsl, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "two.sided", ivl_side = "lower")[["POI"]]["cics"]
  tmp[6] <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = lsl, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "tight", ivl = "prediction",
      ivl_type = "two.sided", ivl_side = "lower")[["POI"]]["cics"]
  tmp[7] <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = usl, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "two.sided", ivl_side = "upper")[["POI"]]["cics"]
  tmp[8] <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = usl, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "tight", ivl = "prediction",
      ivl_type = "two.sided", ivl_side = "upper")[["POI"]]["cics"]

  # <-><-><-><->

  expect_equal(signif(tmp, 12),
               c(115.468684445, 102.847355846, 96.3055219561, 84.0292339443,
                 95.4652197782, 79.6924982370, 82.6037890116, 67.2979863914))
})

test_that("expirest_osle_succeeds_with_transformations", {
  tmp <- rep(NA, 7)

  # <-><-><-><->

  tmp[1] <-
    expirest_osle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", sl = 0.3, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("log", "log"),
      shift = c(1, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper")[["POI"]]["dids"]
  tmp[2] <-
    expirest_osle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", sl = 0.3, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("sqrt", "sqrt"),
      shift = c(0, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper")[["POI"]]["dids"]
  tmp[3] <-
    expirest_osle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", sl = 0.3, sl_sf = 2, srch_range = c(0, 5000),
      alpha = 0.05, alpha_pool = 0.25, xform = c("sq", "sq"),
      shift = c(0, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper")[["POI"]]["dids"]

  tmp[4] <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(0.5, 4.5), sl_sf = c(2, 2),
      srch_range = c(0, 5000), alpha = 0.05, alpha_pool = 0.25,
      xform = c("sq", "sq"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"
      )[["POI"]]["dids"]
  tmp[5] <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(0.5, 4.5), sl_sf = c(2, 2),
      srch_range = c(0, 5000), alpha = 0.05, alpha_pool = 0.25,
      xform = c("sq", "no"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"
    )[["POI"]]["dids"]
  tmp[6] <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(0.5, 4.5), sl_sf = c(2, 2),
      srch_range = c(0, 5000), alpha = 0.05, alpha_pool = 0.25,
      xform = c("sq", "sq"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper"
    )[["POI"]]["dids"]
  tmp[7] <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(0.5, 4.5), sl_sf = c(2, 2),
      srch_range = c(0, 5000), alpha = 0.05, alpha_pool = 0.25,
      xform = c("sq", "no"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper"
    )[["POI"]]["dids"]


  # <-><-><-><->

  expect_equal(signif(tmp, 12),
               c(22.9926049346, 20.3205949274, 14.7086080921, 26.1751174005,
                 31.8950504925, 37.1116679193, 33.9520528257))
})

test_that("expirest_osle_succeeds_for_model_type", {
  t_dat1 <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]
  t_dat2 <- exp1[exp1$Batch %in% c("b3", "b4", "b5"), ]
  t_dat3 <- exp1[exp1$Batch %in% c("b4", "b5", "b8"), ]

  usl <- 105
  lsl <- 95

  # <-><-><-><->

  r_ret1 <-
    expirest_osle(
      data = t_dat1, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = lsl, sl_sf = 3, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "lower")
  r_ret2 <-
    expirest_osle(
      data = t_dat2, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = lsl, sl_sf = 3, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "lower")
  r_ret3 <-
    expirest_osle(
      data = t_dat3, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = lsl, sl_sf = 3, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "lower")

  # <-><-><-><->

  expect_equal(r_ret1[["Model.Type"]][[2]], "cics")
  expect_equal(r_ret2[["Model.Type"]][[2]], "dics")
  expect_equal(r_ret3[["Model.Type"]][[2]], "dids")

  expect_equivalent(r_ret1[["Model.Type"]][[1]], c(1, 1))
  expect_equivalent(r_ret2[["Model.Type"]][[1]], c(0, 1))
  expect_equivalent(r_ret3[["Model.Type"]][[1]], c(0, 0))
})

test_that("expirest_osle_succeeds_for_variables", {
  tmp1 <-
    expirest_osle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", sl = 0.3, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(1, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper")[["Variables"]]
  tmp2 <-
    expirest_osle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", sl = 0.3, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("log", "log"),
      shift = c(1, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper")[["Variables"]]
  tmp3 <-
    expirest_osle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", sl = 0.3, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("log", "no"),
      shift = c(1, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper")[["Variables"]]
  tmp4 <-
    expirest_osle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", sl = 0.3, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "log"),
      shift = c(1, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper")[["Variables"]]

  # <-><-><-><->

  expect_equal(tmp1[["response"]], "Related")
  expect_equal(tmp1[["time"]], "Month")
  expect_equal(tmp1[["batch"]], "Batch")

  expect_equal(tmp2[["response"]], "log.Related")
  expect_equal(tmp2[["response.orig"]], "Related")
  expect_equal(tmp2[["time"]], "log.Month")
  expect_equal(tmp2[["time.orig"]], "Month")
  expect_equal(tmp2[["batch"]], "Batch")

  expect_equal(tmp3[["response"]], "Related")
  expect_equal(tmp3[["time"]], "log.Month")
  expect_equal(tmp3[["time.orig"]], "Month")
  expect_equal(tmp3[["batch"]], "Batch")

  expect_equal(tmp4[["response"]], "log.Related")
  expect_equal(tmp4[["response.orig"]], "Related")
  expect_equal(tmp4[["time"]], "Month")
  expect_equal(tmp4[["batch"]], "Batch")
})

test_that("expirest_osle_warns", {
  usl <- 3.5
  lsl <- 1.5

  # <-><-><-><->

  expect_warning(
    expirest_osle(data = exp3, response_vbl = "Moisture", time_vbl = "Month",
                  batch_vbl = "Batch", sl = lsl, sl_sf = 2,
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "prediction", ivl_type = "one.sided",
                  ivl_side = "lower"),
    "Not for all model types POI values obtained.")
  expect_warning(
    expirest_osle(data = exp3, response_vbl = "Moisture", time_vbl = "Month",
                  batch_vbl = "Batch", sl = lsl, sl_sf = 2,
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "prediction", ivl_type = "one.sided",
                  ivl_side = "upper"),
    "Not for all model types POI values obtained.")

  expect_warning(
    expirest_osle(data = exp3, response_vbl = "Moisture", time_vbl = "Month",
                  batch_vbl = "Batch", sl = c(0.5, 2.5), sl_sf = c(2, 2),
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "two.sided",
                  ivl_side = "upper"),
    "You specified ivl_side = \"upper\".")
  expect_warning(
    expirest_osle(data = exp3, response_vbl = "Moisture", time_vbl = "Month",
                  batch_vbl = "Batch", sl = c(2.5, 4.5), sl_sf = c(2, 2),
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "two.sided",
                  ivl_side = "lower"),
    "You specified ivl_side = \"lower\".")
  expect_warning(
    expirest_osle(data = exp3, response_vbl = "Moisture", time_vbl = "Month",
                  batch_vbl = "Batch", sl = 2.5, sl_sf = 2,
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "one.sided",
                  ivl_side = "upper"),
    "You specified ivl_side = \"upper\".")
  expect_warning(
    expirest_osle(data = exp3, response_vbl = "Moisture", time_vbl = "Month",
                  batch_vbl = "Batch", sl = 2.5, sl_sf = 2,
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "one.sided",
                  ivl_side = "lower"),
    "You specified ivl_side = \"lower\".")
})

test_that("expirest_osle_fails_with_warning_tight_spec_limits", {
  usl <- 3.5
  lsl <- 1.5

  tmp <- numeric(2)

  # <-><-><-><->

  tmp[1] <-
    suppressWarnings(
      expirest_osle(
        data = exp3, response_vbl = "Moisture", time_vbl = "Month",
        batch_vbl = "Batch", sl = lsl, sl_sf = 3, srch_range = c(0, 500),
        alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
        shift = c(0, 0), sf_option = "tight", ivl = "prediction",
        ivl_type = "two.sided", ivl_side = "lower")[["POI"]]["cics"])
  tmp[2] <-
    suppressWarnings(
      expirest_osle(
        data = exp3, response_vbl = "Moisture", time_vbl = "Month",
        batch_vbl = "Batch", sl = lsl, sl_sf = 3, srch_range = c(0, 500),
        alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
        shift = c(0, 0), sf_option = "tight", ivl = "prediction",
        ivl_type = "two.sided", ivl_side = "upper")[["POI"]]["cics"])

  # <-><-><-><->

  expect_equal(is.na(tmp[1]), TRUE)
  expect_equal(is.na(tmp[2]), TRUE)
})

test_that("expirest_osle_fails_with_warning_tight_uniroot_interval", {
  t_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]

  usl <- 105
  lsl <- 95

  tmp <- numeric(4)

  # <-><-><-><->

  tmp[1] <-
    suppressWarnings(
      expirest_osle(
        data = t_dat, response_vbl = "Potency", time_vbl = "Month",
        batch_vbl = "Batch", sl = lsl, sl_sf = 3, srch_range = c(0, 5),
        alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
        shift = c(0, 0), sf_option = "loose", ivl = "confidence",
        ivl_type = "one.sided", ivl_side = "lower")[["POI"]]["cics"])
  tmp[2] <-
    suppressWarnings(
      expirest_osle(
        data = t_dat, response_vbl = "Potency", time_vbl = "Month",
        batch_vbl = "Batch", sl = lsl, sl_sf = 3, srch_range = c(0, 5),
        alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
        shift = c(0, 0), sf_option = "loose", ivl = "prediction",
        ivl_type = "one.sided", ivl_side = "lower")[["POI"]]["cics"])
  tmp[3] <-
    suppressWarnings(
      expirest_osle(
        data = t_dat, response_vbl = "Potency", time_vbl = "Month",
        batch_vbl = "Batch", sl = lsl, sl_sf = 3, srch_range = c(0, 5),
        alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
        shift = c(0, 0), sf_option = "loose", ivl = "confidence",
        ivl_type = "one.sided", ivl_side = "upper")[["POI"]]["cics"])
  tmp[4] <-
    suppressWarnings(
      expirest_osle(
        data = t_dat, response_vbl = "Potency", time_vbl = "Month",
        batch_vbl = "Batch", sl = lsl, sl_sf = 3, srch_range = c(0, 5),
        alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
        shift = c(0, 0), sf_option = "loose", ivl = "prediction",
        ivl_type = "one.sided", ivl_side = "upper")[["POI"]]["cics"])

  # <-><-><-><->

  expect_equal(is.na(tmp[1]), TRUE)
  expect_equal(is.na(tmp[2]), TRUE)
  expect_equal(is.na(tmp[3]), TRUE)
  expect_equal(is.na(tmp[4]), TRUE)
})

test_that("expirest_osle_fails_with_error", {
  t_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]
  t_dal <- t_dat
  t_dal$Batch <- as.character(t_dal$Batch)

  # <-><-><-><->

  expect_error(
    expirest_osle(
      data = as.matrix(t_dat[, c("Month", "Potency")]),
      response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 105, sl_sf = 4, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "data must be provided as data frame")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = 2, time_vbl = "Month",
      batch_vbl = "Batch", sl = 105, sl_sf = 4, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "response_vbl must be a string")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Mass", time_vbl = "Month",
      batch_vbl = "Batch", sl = 105, sl_sf = 4, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "response_vbl was not found in the provided data frame")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = 3,
      batch_vbl = "Batch", sl = 105, sl_sf = 4, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "time_vbl must be a string")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Year",
      batch_vbl = "Batch", sl = 105, sl_sf = 4, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "time_vbl was not found in the provided data frame")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = 4, sl = 105, sl_sf = 4, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "batch_vbl must be a string")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Lot", sl = 105, sl_sf = 4, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "batch_vbl was not found in the provided data frame")
  expect_error(
    expirest_osle(
      data = t_dal, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 105, sl_sf = 4, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "column in data specified by batch_vbl")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = "sl", sl_sf = 4, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "sl must be a numeric")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(95, 100, 105), sl_sf = 4,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper"),
    "sl must be a numeric or vector of length 1 or 2")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(105, 95), sl_sf = 4,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper"),
    "sl must be of the form")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 105, sl_sf = "4", srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "sl_sf must be a positive integer")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 105, sl_sf = -3, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "sl_sf must be a positive integer")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(95, 105), sl_sf = 4,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper"),
    "sl_sf must be a positive integer")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(95, 105), sl_sf = c(4.4, 3.3),
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper"),
    "sl_sf must be a positive integer")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = "alpha",
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "srch_range must be a vector of length 2")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = 500,
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "srch_range must be a vector of length 2")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500),
      alpha = 5, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "specify alpha")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500),
      alpha = -1, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "specify alpha")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 5, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "specify alpha_pool")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = -1, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "specify alpha_pool")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = "no",
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "specify xform appropriately")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("yes", "no"),
      sf_option = "loose", ivl_type = "one.sided",
      shift = c(0, 0), ivl_side = "upper", ivl = "confidence"),
    "specify xform appropriately")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "yes"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "specify xform appropriately")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c("no", "no"), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "shift must be a numeric vector of length 2")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = 1, sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "shift must be a numeric vector of length 2")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "strict", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper"),
    "specify sf_option either as \"tight\" or \"loose\"")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "incorrect",
      ivl_type = "one.sided", ivl_side = "upper"),
    "specify ivl either as \"confidence\" or \"prediction\"")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "incorrect", ivl_side = "upper"),
    "specify ivl_type either as \"one.sided\" or \"two.sided\"")
  expect_error(
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "incorrect"),
    "specify ivl_side either as \"lower\" or \"upper\"")
})
