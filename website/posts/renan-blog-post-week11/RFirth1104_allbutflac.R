# Required Packages
install.packages(c("dplyr", "car", "ResourceSelection", "caret", "pROC",  "logistf", "ggplot2"))
packages <- c("dplyr", "car", "ResourceSelection", "caret", "pROC",  "logistf", "ggplot2")
lapply(packages, library, character.only = TRUE)

# Data loading and cleaning (adapt as per your file location/column names)
stroke1 <- read.csv("stroke.csv")
stroke1[stroke1 == "N/A" | stroke1 == "Unknown" | stroke1 == "children" | stroke1 == "other"] <- NA
stroke1$bmi <- round(as.numeric(stroke1$bmi), 2)
stroke1$gender[stroke1$gender == "Male"] <- 1
stroke1$gender[stroke1$gender == "Female"] <- 2
stroke1$gender <- as.numeric(stroke1$gender)
# ... Repeat for other categorical or numeric fields as per original script ...

stroke1$stroke <- as.factor(stroke1$stroke)
stroke1_clean <- na.omit(stroke1)
strokeclean <- stroke1_clean
#Model for logistic Regression #
formula <- stroke ~ gender + age + hypertension + heart_disease + ever_married +
  work_type + Residence_type + avg_glucose_level + bmi + smoking_status
# Histograms for categorical variables and density plots for continous variables#
ggplot(strokeclean, aes(x = gender)) +
  geom_bar(fill = "blue", 
           color = "white") +
  labs(title = "Histogram of gender", 
       x = "gender", 
       y = "Frequency")
ggplot(strokeclean, aes(x = age)) +
  geom_histogram(binwidth = 5, 
                 fill = "green", 
                 color = "white") +
  labs(title = "Histogram of Age", 
       x = "Age", 
       y = "Frequency")

ggplot(strokeclean, aes(x = hypertension)) +
  geom_bar(fill = "purple", 
           color = "white") +
  labs(title = "Histogram of hypertension", 
       x = "hypertension", 
       y = "Frequency")

ggplot(strokeclean, aes(x = heart_disease)) +
  geom_bar( fill = "orange",
            color = "white") +
  labs(title = "Histogram of heart_diseasd", 
       x = "Age", 
       y = "Frequency")

ggplot(strokeclean, aes(x = ever_married)) +
  geom_bar(fill = "aquamarine", 
           color = "white") +
  labs(title = "Histogram of ever_married", 
       x = "Age", 
       y = "Frequency")

ggplot(strokeclean, aes(x = work_type)) +
  geom_bar(fill = "steelblue", 
           color = "white") +
  labs(title = "Histogram of work_type", 
       x = "Age", 
       y = "Frequency")

ggplot(strokeclean, aes(x = Residence_type)) +
  geom_bar(fill = "magenta", 
           color = "white") +
  labs(title = "Histogram of Residence_type", 
       x = "Residence_type", 
       y = "Frequency")

ggplot(strokeclean, aes(x = avg_glucose_level)) +
  geom_histogram(binwidth = 5, 
                 fill = "chartreuse", 
                 color = "white") +
  labs(title = "Histogram of avg_gloucose_level",
       x = "avg-glucose_level", 
       y = "Frequency")

ggplot(strokeclean, aes(x = bmi)) +
  geom_histogram(binwidth = 5, 
                 fill = "gold", 
                 color = "white") +
  labs(title = "Histogram of bmi", 
       x = "bmi", 
       y = "Frequency")

ggplot(strokeclean, aes(x = smoking_status)) +
  geom_bar(fill = "deepskyblue", 
           color = "white") +
  labs(title = "smoking_status", 
       x = "smoking_status", 
       y = "Frequency")

ggplot(strokeclean, aes(x = stroke)) +
  geom_bar(fill = "tan", 
           color = "white") +
  labs(title = "Histogram of Age", 
       x = "stroke", 
       y = "Frequency")

# Baseline Logistic Regression
model_base <- glm(formula, data=strokeclean, family=binomial)
prob_base <- predict(model_base, type="response")

# Firth Logistic Regression
model_firth <- logistf(formula, data=strokeclean)
prob_firth <- predict(model_firth, type="response")

# FLIC Correction (this correction changes the intercept)
model_flic <- flic(formula, data=strokeclean)
prob_flic <- predict(model_flic, type="response")


labels <- strokeclean$stroke

# ---- Youden's J calculation function ----
youden_point <- function(roc_obj) {
  coords <- coords(roc_obj, "best", best.method = "youden", ret=c("threshold", "sensitivity", "specificity", "youden"))
  return(coords)
}

# Predictions with default threshold 0.5 (for output consistency)
pred_base <- factor(ifelse(prob_base > 0.5, 1, 0), levels=c(0,1))
pred_firth <- factor(ifelse(prob_firth > 0.5, 1, 0), levels=c(0,1))
pred_flic <- factor(ifelse(prob_flic > 0.5, 1, 0), levels=c(0,1))

# Function to Compute Metrics

metrics <- function(pred, prob, labels, name) {
cm <- confusionMatrix(pred, labels, positive="1")
roc_obj <- roc(labels, as.numeric(prob))
auc_val <- auc(roc_obj)
precision <- cm$byClass["Pos Pred Value"]
recall <- cm$byClass["Sensitivity"]
f1 <- 2 * ((precision * recall)/(precision + recall))
youden <- youden_point(roc_obj)
# Add to output
list(
  confusion=cm$table, precision=precision, recall=recall, f1=f1, auc=auc_val,
  roc_obj=roc_obj, youden=youden, model=name
)
}
results_base <- metrics(pred_base, prob_base, labels, "Baseline GLM")
results_firth <- metrics(pred_firth, prob_firth, labels, "Firth Logistic")
results_flic <- metrics(pred_flic, prob_flic, labels, "Firth Flic Logistic")


 # Predictions with default threshold 0.5 (for output consistency)
  pred_base <- factor(ifelse(prob_base > 0.5, 1, 0), levels=c(0,1))
  pred_firth <- factor(ifelse(prob_firth > 0.5, 1, 0), levels=c(0,1))
  pred_flic <- factor(ifelse(prob_flic > 0.5, 1, 0), levels=c(0,1))

  metrics <- function(pred, prob, labels, name) {
    cm <- confusionMatrix(pred, labels, positive = "1")
    roc_obj <- roc(labels, as.numeric(prob))
    auc_val <- auc(roc_obj)
    precision <- cm$byClass["Pos Pred Value"]
    recall <- cm$byClass["Sensitivity"]
    f1 <- 2 * ((precision * recall) / (precision + recall))
    youden <- youden_point(roc_obj)
    # All list arguments separated by commas only, no '+'
    list(
      confusion = cm$table,
      precision = precision,
      recall = recall,
      f1 = f1,
      auc = auc_val,
      roc_obj = roc_obj,
      youden = youden,
      model = name
    )
  }
# Print Results
cat("\n== Baseline Logistic Regression ==\n")
print(results_base[1:6])
cat("\nYouden's J (optimal threshold):\n")
print(results_base$youden)
cat("\n== Firth Logistic Regression ==\n")
print(results_firth[1:6])
cat("\nYouden's J (optimal threshold):\n")
print(results_firth$youden)
cat("\n== FLIC Logistic Regression ==\n")
print(results_flic[1:6])
cat("\nYouden's J (optimal threshold):\n")
print(results_flic$youden)


#---- Plot ROC Curves with Youden's points highlighted ----
plot(results_base$roc_obj, col="cyan", main="ROC Curves: Baseline (blue) vs Firth (red)")
plot(results_firth$roc_obj, col="magenta", add=TRUE)
plot(results_flic$roc_obj, col ="gold", add=TRUE)
auc(results_base$roc_obj)
auc(results_firth$roc_obj)
auc(results_flic$roc_obj)


# Mark Youden points: use pch=19 and cex=1.5 for visibility
points(
  1-results_base$youden["specificity"],
  results_base$youden["sensitivity"],
  col="cyan", pch=19, cex=1.5
)
points(
  1-results_firth$youden["specificity"],
  results_firth$youden["sensitivity"],
  col="magenta", pch=19, cex=1.5
)
points(
  1-results_flic$youden["specificity"],
  results_flic$youden["sensitivity"],
  col="gold", pch=19, cex=1.5
)

legend("bottomright", legend=c("Baseline", "Firth","flic"), col=c("cyan", "magenta", "gold"), lwd=2)

# Annotate Youden J values on the plot#
text(
  x=1-results_base$youden["specificity"], y=results_base$youden["sensitivity"],
  labels=paste0("Youden: ", round(results_base$youden["youden"], 3)),
  pos=4, col="cyan"
)
text(
  x=1-results_firth$youden["specificity"], y=results_firth$youden["sensitivity"],
  labels=paste0("Youden: ", round(results_firth$youden["youden"], 3)),
  pos=4, col="magenta"
)
text(
  x=1-results_flic$youden["specificity"], y=results_flic$youden["sensitivity"],
  labels=paste0("Youden: ", round(results_flic$youden["youden"], 3)),
  pos=4, col="gold"
)
# Plot Confusion Matrices,
par(mfrow=c(1,2))
fourfoldplot(results_base$confusion, color = c("lightskyblue", "plum2"),
             conf.level = 0, margin = 1, main = "Baseline Confusion Matrix")
fourfoldplot(results_firth$confusion, color = c("lightskyblue", "plum2"),
             conf.level = 0, margin = 1, main = "Firth Confusion Matrix")
fourfoldplot(results_flic$confusion, color = c("lightskyblue", "plum2"),
             conf.level = 0, margin = 1, main = "Flic Confusion Matrix")

par(mfrow=c(1,1))


