# Install required packages (run only once)
# install.packages(c("dplyr", "ggplot2", "caret", "pROC"))

# Load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(pROC)

# Step 1: Simulate dataset
set.seed(123)
df <- data.frame(
  age = sample(21:65, 500, replace = TRUE),
  income = sample(20000:100000, 500, replace = TRUE),
  loan_amount = sample(5000:50000, 500, replace = TRUE),
  default = rbinom(500, 1, 0.3)  # binary outcome: 0 = no default, 1 = default
)

# Step 2: Explore the data
glimpse(df)
summary(df)

# Visualize income by default status
ggplot(df, aes(x = income, fill = factor(default))) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(title = "Income Distribution by Default Status", fill = "Default")

# Step 3: Split data into training and test sets
set.seed(42)
split <- createDataPartition(df$default, p = 0.8, list = FALSE)
train_data <- df[split, ]
test_data <- df[-split, ]

# Step 4: Train logistic regression model
model <- glm(default ~ age + income + loan_amount, data = train_data, family = "binomial")
summary(model)

# Step 5: Predict probabilities and classify
probs <- predict(model, newdata = test_data, type = "response")
pred_class <- ifelse(probs > 0.5, 1, 0)

# Step 6: Evaluate model performance
confusion <- confusionMatrix(factor(pred_class), factor(test_data$default), positive = "1")
print(confusion)

# ROC curve and AUC
roc_curve <- roc(test_data$default, probs)
plot(roc_curve, col = "blue", main = "ROC Curve")
auc_val <- auc(roc_curve)
cat("AUC:", auc_val, "\n")
