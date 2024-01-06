cm <- dx_cm(
  dx_heart_failure$predicted,
  dx_heart_failure$truth,
  threshold = 0.3,
  poslabel = 1
)

dx_obj <- dx(
  data = dx_heart_failure,
  true_varname = "truth",
  pred_varname = "predicted",
  outcome_label = "Heart Attack",
  setthreshold = .3
)

test_that("Sensitivity", {

  expect_equal(
    dx_sensitivity(cm, detail = "simple"),
    0.84693877551020413375
  )

})

test_that("Recall", {

  expect_equal(
    dx_recall(cm, detail = "simple"),
    0.84693877551020413375
  )

})

test_that("Accuracy", {

  expect_equal(
    dx_accuracy(cm, detail = "simple"),
    0.79310344827586209959
  )

})

test_that("PPV", {

  expect_equal(
    dx_ppv(cm, detail = "simple"),
    0.68032786885245910558
  )

})


test_that("PPV", {

  expect_equal(
    dx_ppv(cm, detail = "simple"),
    0.68032786885245910558
  )

})

test_that("NPV", {

  expect_equal(
    dx_npv(cm, detail = "simple"),
    0.89208633093525191438
  )

})

test_that("Specificity", {

  expect_equal(
    dx_specificity(cm, detail = "simple"),
    0.76073619631901845395
  )

})

test_that("Precision", {

  expect_equal(
    dx_precision(cm, detail = "simple"),
    0.68032786885245899455
  )

})

test_that("MCC", {

  expect_equal(
    dx_mcc(cm, detail = "simple"),
    0.58978113120940389713
  )

  cm2 <- dx_cm(
    c(0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 1),
    threshold = 0.5,
    poslabel = 1
  )

  expect_equal(
    dx_mcc(cm2, detail = "simple"),
    0
  )


})

test_that("Youden J", {

  expect_equal(
    dx_youden_j(cm, detail = "simple"),
    0.60767497182922269872
  )

})

test_that("F1", {

  expect_equal(
    dx_f1(cm, detail = "simple"),
    0.75454545454545451921
  )

})

test_that("F2", {

  expect_equal(
    dx_f2(cm, detail = "simple"),
    0.80739299610894943449
  )

})

test_that("F0.5", {

  expect_equal(
    dx_fbeta(cm, beta = 0.5, detail = "simple"),
    0.70819112627986346986
  )

})

test_that("Cohen's Kappa", {

  expect_equal(
    dx_cohens_kappa(cm, detail = "simple"),
    0.57938402769487884481
  )

  full <- dx_cohens_kappa(cm)

  expect_equal(
    full$conf_low,
    0.4794738179054287
  )

  expect_equal(
    full$conf_high,
    0.6792942374843292
  )


  expect_equal(get_kappa_interpretation(-.1), "Less than chance agreement")
  expect_equal(get_kappa_interpretation(0.1), "Slight Agreement")
  expect_equal(get_kappa_interpretation(0.3), "Fair Agreement")
  expect_equal(get_kappa_interpretation(0.4), "Moderate Agreement")
  expect_equal(get_kappa_interpretation(0.6), "Substantial Agreement")
  expect_equal(get_kappa_interpretation(.8), "Almost Perfect Agreement")

})


test_that("Balanced Accuracy", {

  expect_equal(
    dx_balanced_accuracy(cm, detail = "simple"),
    0.80383748591461134936
  )

})

test_that("Prevalence", {

  expect_equal(
    dx_prevalence(cm, detail = "simple"),
    0.37547892720306514
  )

})

test_that("Detection Prevalence", {

  expect_equal(
    dx_detection_prevalence(cm, detail = "simple"),
    0.46743295019157088399
  )

})

test_that("ROC AUC", {

  expect_equal(
    dx_auc(dx_heart_failure$truth, dx_heart_failure$predicted,  detail = "simple"),
    0.90365594090396894611
  )

})

test_that("PR AUC", {


  auc_pr <- dx_auc_pr(
    dx_obj$thresholds$precision,
    dx_obj$thresholds$sensitivity,
    detail = "simple"
  )

  expect_equal(
    auc_pr,
    0.87271242708725860204
  )

})

test_that("Brier Score", {

  expect_equal(
    dx_brier(dx_heart_failure$predicted, dx_heart_failure$truth, detail = "simple"),
    0.11374281513546434519
  )

})

test_that("No Information Rate (NIR)", {

  expect_equal(
    dx_nir(cm, detail = "simple"),
    0.624521072796935
  )

})

test_that("No Information Rate (NIR)", {

  expect_equal(
    dx_nir(cm, detail = "simple"),
    0.624521072796935
  )

})

test_that("False Positive Rate (FPR)", {

  expect_equal(
    dx_fpr(cm, detail = "simple"),
    0.23926380368098160156
  )

})

test_that("False Negative Rate (FNR)", {

  expect_equal(
    dx_fnr(cm, detail = "simple"),
    0.15306122448979592177
  )

})

test_that("Negative Likelihood Ratios", {

  expect_equal(
    dx_lrt_neg(cm, detail = "simple"),
    0.201201448321264
  )

  full <- dx_lrt_neg(cm)

  expect_equal(full$conf_low, 0.125297617327744387206)
  expect_equal(full$conf_high, 0.32308693229723844942)


})

test_that("Positive Likelihood Ratios", {

  expect_equal(
    dx_lrt_pos(cm, detail = "simple"),
    3.5397697540554685
  )

  full <- dx_lrt_pos(cm)

  expect_equal(full$conf_low, 2.658275740936100195455)
  expect_equal(full$conf_high, 4.71357042415530003154)


})

test_that("Odds Ratio", {

  expect_equal(
    dx_odds_ratio(cm, detail = "simple"),
    17.593162393162395
  )

})

test_that("Markedness", {

  expect_equal(
    dx_markedness(cm, detail = "simple"),
    0.57241419978771101995
  )

})

test_that("Fowlkes-Mallows Index", {

  expect_equal(
    dx_fowlkes_mallows(cm, detail = "simple"),
    0.7590757881735975543
  )

})

test_that("G-mean", {

  expect_equal(
    dx_g_mean(cm, detail = "simple"),
    0.8026811213655892
  )

})

test_that("False Discovery Rate (FDR)", {

  expect_equal(
    dx_fdr(cm, detail = "simple"),
    0.31967213114754100545
  )

})

test_that("Metric Binomial", {


  num <- 113
  denom <- 13523

  # test method is getting passed correctly
  methods <- c("exact", "logit")

  for (method in methods) {
    res <- metric_binomial(113, 13523, name = method, citype = method)
    res_bare <- binom::binom.confint(num, denom, methods = method)

    expect_equal(res$estimate, res_bare$mean)
    expect_equal(res$conf_low, res_bare$lower)
    expect_equal(res$conf_high, res_bare$upper)
  }

})
