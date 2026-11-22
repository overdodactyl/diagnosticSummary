# Set options for diagnostic analysis

Use this function to return a list of tuning parameters to analyze your
diagnostic test

## Usage

``` r
dx(
  data,
  classlabels = c("Negative", "Positive"),
  threshold_range = NA,
  outcome_label = NA,
  pred_varname,
  true_varname,
  setthreshold = 0.5,
  poslabel = 1,
  grouping_variables = NA,
  prevalence = NA,
  citype = "exact",
  bootreps = 2000,
  bootseed = 20191015,
  doboot = FALSE,
  direction = "<",
  ...
)
```

## Arguments

- data:

  A tbl.

- classlabels:

  Labels for predicted variable. Needs to be 0, 1 order.

- threshold_range:

  Optional. A numeric vector of thresholds to loop over.

- outcome_label:

  Label for outcome (string)

- pred_varname:

  Column name containing AI prediction (string). Values should be
  predicted probabilities between 0 and 1, with no missing values.

- true_varname:

  Column name containing AI reference standard (string). Values should
  be 0 or 1 (numeric, logical, factor, or character), with no missing
  values, and both classes must be present.

- setthreshold:

  A numeric value representing the threshold used to identify AI
  prediction

- poslabel:

  Positive class. Variable should be coded as 0/1 with 1 being the event

- grouping_variables:

  Character vector of variable names to be summarized by. Variables are
  converted to factors if not already one.

- prevalence:

  Numeric value between 0 and 1, representing a target prevalence for
  additional NPV and PPV calculations.

- citype:

  Confidence interval type.

- bootreps:

  Number of bootstrap samples used to generate F1 score CI

- bootseed:

  Seed value to be used when calculating bootsraped CI's

- doboot:

  Logical. Generate bootstrap estimate of F1 confidence interval?

- direction:

  Direction for roc comparison. See ?pROC::roc. The default, "\<",
  treats higher predicted probabilities as more likely to be positive.
  "auto" lets pROC choose the direction that gives an AUC of at least
  0.5, which hides models that perform worse than chance. Only the AUC
  and ROC curve use `direction`; sensitivity, specificity, and other
  threshold based results always treat higher predictions as positive,
  so other values give a warning.

- ...:

  currently unused
