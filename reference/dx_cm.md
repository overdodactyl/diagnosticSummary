# Create a Confusion Matrix from Predictions and Truth

This function calculates a confusion matrix from predicted
probabilities, true outcomes, a threshold for classification, and a
designated positive label. It calculates true positives, false
negatives, true negatives, false positives, and several other useful
metrics.

## Usage

``` r
dx_cm(predprob, truth, threshold, poslabel = 1)
```

## Arguments

- predprob:

  Numeric vector of predicted probabilities that the outcome is 1,
  between 0 and 1, with no missing values.

- truth:

  Vector of true binary class outcomes (0 and 1), the same length as
  `predprob`, with no missing values.

- threshold:

  A single number between 0 and 1. Predictions at or above it are
  classified as outcome 1.

- poslabel:

  The outcome class that is the event: 1 (default) or 0.

## Value

A data frame of class "dx_cm" with one row, containing the components of
the confusion matrix and additional metrics:

- `tp`: True Positives

- `fn`: False Negatives

- `tn`: True Negatives

- `fp`: False Positives

- `dispos`: Number of Actual Positives

- `disneg`: Number of Actual Negatives

- `n`: Total Number of Observations

- `correct`: Number of Correct Predictions

- `testpos`: Number of Predicted Positives

- `testneg`: Number of Predicted Negatives

## Details

The function takes predicted probabilities and a threshold to create
binary predictions which are then compared to the true labels to create
a confusion matrix. It is useful for evaluating the performance of a
binary classification model. With `poslabel = 0`, outcome 0 is the
event, so a prediction below the threshold is a positive test.

## Examples

``` r
# Example usage:
true_labels <- c(1, 0, 1, 1, 0)
predicted_probs <- c(0.9, 0.3, 0.6, 0.8, 0.1)
cm <- dx_cm(predicted_probs, true_labels, threshold = 0.5, poslabel = 1)
print(cm)
#>   tp fn tn fp dispos disneg n correct testpos testneg
#> 1  3  0  2  0      3      2 5       5       3       2
```
