Code that Shree used

# Load data
stroke <- read.csv("stroke.csv") 



#Elimination of non-predictive identifiers, such as patient ID
Code --  
stroke = stroke %>% select(-id) Command

#Making components out of categorical variables (converting into factor)
Code --

stroke = stroke %>%
mutate(
gender          = factor(gender),
ever_married    = factor(ever_married),
work_type       = factor(work_type),
Residence_type  = factor(Residence_type),
smoking_status  = factor(smoking_status),
hypertension    = factor(hypertension),
heart_disease   = factor(heart_disease),
stroke          = factor(stroke, levels = c(0, 1),
labels = c("No", "Yes"))
)

## Managing uncommon categories (like the gender group "Other")
Code --
stroke$gender[stroke$gender == "Other"] = "Male"
stroke$gender = droplevels(stroke$gender)
 
## Using median imputation to impute missing BMI values
Code- 

stroke$bmi[is.na(stroke$bmi)] = median(stroke$bmi, na.rm = TRUE)

##Convert Bmi into numberic

stroke$bmi[stroke$bmi == "N/A"] = NA
stroke$bmi = as.numeric(stroke$bmi)
median_bmi = median(stroke$bmi, na.rm = TRUE)
stroke$bmi[is.na(stroke$bmi)] <- median_bmi
summary(stroke$bmi)

##Train/Test division: 

set.seed(123)
index = createDataPartition(stroke$stroke, p = 0.7, list = FALSE)
train_data  = stroke[index, ]
test_data   = stroke[-index, ]
prop.table(table(train_data$stroke))
prop.table(table(test_data$stroke))

#1 Logestic Regression: 
set.seed(123)
fit_glm = train(
model_formula,
data = train_data,
method = "glm",
family = "binomial",
trControl = ctrl,
metric = "ROC"
fit_glm

varImp(fit_glm)

#Evaluate model code:

evaluate_model = function(model, test_data, positive_class = "Yes") {

# Class predictions
pred_class =  predict(model, newdata = test_data)

# Probabilities for positive class "Yes"
pred_prob =  predict(model, newdata = test_data, type = "prob")[, positive_class]

# Confusion matrix
cm =  confusionMatrix(pred_class, test_data$stroke, positive = positive_class)

# ROC & AUC
  roc_obj =  roc(
    response  = test_data$stroke,
    predictor = pred_prob,
    levels    = c("No", "Yes")  # "No" = control, "Yes" = case
  )
   list(
    cm      = cm,
    auc     = auc(roc_obj),
    roc_obj = roc_obj
  )
}
res_glm = evaluate_model(fit_glm, test_data)
res_glm$cm      
res_glm$auc  


#2 Decision tree- 

set.seed(123)
fit_rpart = train(
model_formula,
... (191 lines left)