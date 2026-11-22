# Create a Confusion Matrix from Predictions and Truth

This function calculates a confusion matrix from predicted
probabilities, true outcomes, a threshold for classification, and a
designated positive label. It calculates true positives, false
negatives, true negatives, false positives, and several other useful
metrics.

## Usage

``` r
dx_cm(predprob, truth, threshold, poslabel)
```

## Arguments

- predprob:

  Numeric vector of prediction probabilities.

- truth:

  Numeric vector of true binary class outcomes.

- threshold:

  Numeric value to determine the cutoff for classifying predictions as
  positive.

- poslabel:

  The label of the positive class in the truth data.

## Value

A dataframe object of class "dx_cm" containing the components of the
confusion matrix and additional metrics:

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
binary classification model.

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
