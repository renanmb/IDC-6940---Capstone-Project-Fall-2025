# changes and notes

Old text for **Dataset Preprocessing**

The initial exploration demonstrated that the **Stroke Prediction Dataset** @kaggle01 has several issues requiring changes for handling missing values, converting character (categorical) features into numerical codes, and removing the identifier column.

Our initial step is to addresses specific string values that represent missing data or require special handling:

* All instances of the string values "N/A", "Unknown", "children", and "other" across the entire dataset were replaced with the standard R missing value representation, NA.

Then we must convert the data type of several character (categorical) features into numerical (integer) codes for use in machine learning models. The **bmi** column, which was initially read as character due to the presence of NA values, was converted to numeric then rounded to two decimal places. The categorical **gender** feature was re-coded to numeric with the values "Male" = 1 and "Female" = 0. The categorical **ever_married** feature was re-coded to numeric with the values "Yes" = 1 and "No" = 0. The categorical **work_type** feature was re-coded to numeric with values "Govt_job" $\rightarrow 1$, "Private" $\rightarrow 2$, "Self-employed" $\rightarrow 3$, "Never_worked" $\rightarrow 4$. The categorical **Residence_type** feature was re-coded to numeric with values "Urban" $\rightarrow 1$ and "Rural" $\rightarrow 2$. The categorical **smoking_status** feature was re-coded to numeric with values "never smoked" $\rightarrow 1$", formerly smoked" $\rightarrow 2$ and "smokes" $\rightarrow 3$. Additionally **avg_glucose_level**, **heart_disease**, **hypertension**, **age**, and **stroke** were all explicitly converted or confirmed as numeric data types, with **age** being rounded to two decimal places.

Lastly, the **id** column, which is a unique identifier and not useful for predictive modeling, was removed from the dataset leaving us with 11 predictors. Now we can proceed on converting the target Variable **stroke** variable in **stroke1** to a factor (a categorical type used in R). Removal of missing and inconsistent entries and finally creating the Data Frames **strokeclean** and **fourassume**. Then the stroke factor levels were explicitly labeled 0 $\rightarrow$ "No" and 1 $\rightarrow$ "Yes".

<!-- TODO -->
So as part of data preprocessing we will be focused on establishing consistency and ensuring all variables are in a format suitable for predictive modeling. This process starts by systematically addressing non-standard representations of missing data. Specifically, all instances of the string values "N/A", "Unknown", "children", and "other" found across the dataset were unified and replaced with the standard statistical missing value representation, NA.

Then we proceed with converting several character-based (categorical) features into numerical features, which is necessary for predictive modeling.

