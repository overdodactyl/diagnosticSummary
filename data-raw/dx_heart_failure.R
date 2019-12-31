library(dplyr)
set.seed(20191104)

# Originally obtained from Kaggle: https://www.kaggle.com/imnikhilanand/heart-attack-prediction
ha_data <- readr::read_csv("data-raw/heart_failure.csv", na = c("", " ", "?", -9))

# Select variables for prediction and keep compltete cases
ha_data <- ha_data %>%
  select(num, cp, age, sex, trestbps, chol, restecg, fbs, thalach, oldpeak) %>%
  filter_all(all_vars(!is.na(.)))

# Generate model and predictions
pred <- glm(num ~ cp + age + sex + trestbps + chol + restecg + fbs + thalach + oldpeak, family = "binomial", data = ha_data)
pred <- predict(pred, type = "response")

dx_heart_failure <- data.frame(
  AgeGroup = ha_data$age,
  Sex = ha_data$sex,
  truth = ha_data$num,
  predicted = pred,
  stringsAsFactors = FALSE
)

dx_heart_failure$AgeGroup <- cut(dx_heart_failure$AgeGroup, breaks = c(20, 50, 80))
dx_heart_failure$Sex <- factor(dx_heart_failure$Sex, levels = c(0, 1), labels = c("Female", "Male"))
dx_heart_failure$AgeSex <- factor(paste0(dx_heart_failure$AgeGroup, " - ", dx_heart_failure$Sex))


usethis::use_data(dx_heart_failure, overwrite = TRUE)
