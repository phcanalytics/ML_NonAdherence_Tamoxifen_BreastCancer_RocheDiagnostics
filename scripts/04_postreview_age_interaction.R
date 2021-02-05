# Purpose:    Exploration of age interactions (according to reviewer(s) feedback)
# Programers:  Thanos Siadimas [thanos.siadimas@roche.com], Gayathri Yerrapragada [gyerrap@g.clemson.edu ]

# load libraries
library(data.table) 
library(dplyr) 
library(caret) 
library(randomForest)
library(caTools)
library(mlbench)
library(FSelector)
library(unbalanced)
library(mlbench)
library(FSelector)
library(caret)

##########################
# Model data preparation #
##########################

cohort     <- data.table(tdRWDSquery("SELECT * FROM f_cohort;"), stringsAsFactors=F) %>% rename_all(tolower)
cohort_all <- data.table(tdRWDSquery("SELECT * FROM all_features;"), stringsAsFactors=F) %>% rename_all(tolower)
cohort1 <  - merge(cohort,cohort_all, by = "enrolid")
cohort_f   <- merge(cohort1,medications, by = "enrolid")
cohort_f  <- merge(cohort_f,adherdata, by = "enrolid")

data <- data.frame(cohort_f) # loading data from Teradata table

#selecting columns from the dataset
pdc <- data[,c(1,5:8)]

pdc_n<- data[,c(1,11,15:18,20,26,28,29,31:36,38:42,44,58,59,60,62:66,68,69,72,73,75,77:79,93,99,101,102,113,114,125:127,130,131,133,150,151,153,170,182,190,193,
                196:200,207,210,211,213,215,217,218,221,225,226,228:233,236,239,240,245,247,266,274,
                9,   # eestatu
                214, # radiation_oncology
                216  # nuclearmedicine_lymphatic
)]

pdc$plantyp[is.na(pdc$plantyp)] <- "missing"

# one hot encoding categorical variables
dmy <- dummyVars(" ~.", data = pdc)
pdc_s <- data.frame(predict(dmy, newdata = pdc))
pdc_full <- merge(pdc_s, pdc_n, by = "enrolid")

pdc_final <- pdc_full[,c(-1)]

pdc_final[] <- lapply(pdc_final, factor) # the "[]" keeps the dataframe structure
col_names <- names(pdc_final)
# do it for some names in a vector named 'col_names'
pdc_final[col_names] <- lapply(pdc_final[col_names],factor)

pdc_final$age <- as.numeric(as.character(pdc_final$age.y))
pdc_final <- pdc_final[, !(names(pdc_final) %in% c("age.y"))]

# Select top 40 variables based on Chi square statistics and
# add the requested variables to examine the inrection terms 
# if they were not significant
weights<- chi.squared(pdc1flag~., pdc_final)

# Select top 40 variables
subset<- cutoff.k(weights, 40)

modeldata <- data.frame(data.table(pdc_final)[, c('pdc1flag',
                                                  'abnormalities_gait',
                                                  'nuclearmedicine_endocrine',
                                                  'nuclearmedicine_musco', subset), with=F])
#################################
# End of Model data preparation #
#################################

# Age distribution 

# Generate the histogram of age along with the boxplot 
par(mfrow = c(2, 1))

hist(modeldata$age, xlab="Age", main="Histogram of Age (entire dataset)")

boxplot(modeldata$age,
        main = "Distribution of age (entire dataset)",
        xlab = "Age",  ylab = "", col = "blue",
        border = "brown", horizontal = T, notch = T)

# the above plots suggest the presence of extreme age values (outliers), 
# which can cause a disproportionate impact on the relationship of age with other variables. 
# To minimize the impact of outliers 2% of the extreme age values was removed by 
# excluding the upper and lower 0.01 percentile. 

# Outliers can have disproportionate impact on the relationship of Age with other variables
# Remove %2 of the extreme values by excluding the upper and lower 0.01 percentile

lower_bound <- quantile(modeldata$age, 0.01) # 28.21
upper_bound <- quantile(modeldata$age, 0.99) # 82

# Initial number of patients
nrow(modeldata) # 3022

# Restrict the cohort to patients between 29 and 82 years old, inclusive, 
# for the exploration of interaction terms 


# Surviving spouse status is coded with number 8 
# in the employment status variable (column eestatu)

modeldata2 <- 
  modeldata %>%
  filter(age>=lower_bound & age<=upper_bound) %>%
  mutate(surviving_spouse_status=ifelse(eestatu=="8", "alive", "not alive/Unknown")) %>%
  # mutate(eestatu=fct_lump(eestatu, prop=0.05)) %>%
  data.frame

# Number of patients aged between [29,82]
nrow(modeldata2) # 2973 

# percentage of patients excluded 
100*(nrow(modeldata)-nrow(modeldata2))/nrow(modeldata)
# 1.98%

# Set validation method
train.control <- trainControl(method="repeatedcv", number=10, repeats=3) # 10 fold CV

# Stepwise LR for variable selection
step.model <- caret::train(
  pdc1flag ~ 
   age + agegrp6 + plantyp2 + region3 + diagimaging_head_neck + 
   agegrp2 + dorsopathies + immunology_proc + agegrp3 + diagimaging_spine + 
   beta_blocker + hematology_proc + abdominalpain + diseases_hypertensive + 
   diagimaging_chest + surgery_skin_nails + chemistry_proc + 
   bacterial_diseases + findings_tissues + plantyp6 + transfusion_proc + 
   agegrp5 + region1 + diagultrasound_head_neck + surgery_bladder + 
   surgery_head + acute_rheumatic_fever + nuclearmedicine_nervous + 
   disorders_thyroidglands + surgery_heart + diseases_cerebrovascular + 
   surgery_auditory + disorders_endocrineglands + calcium_supp + 
   evocative_proc + region2 + diabetesmellitus + nuclearmedicine_endocrine + 
   nuclearmedicine_musco + abnormalities_gait + nuclearmedicine_lymphatic +
   radiation_oncology + surviving_spouse_status +
   
   # Include the following interaction terms as requested from the reviewers
   # i.   Age + surviving spouse status
   # ii.  Age + radiation oncology
   # iii. Age + lymphatic nuclear medicine
   # iv.  Age + cerebrovascular disease
   
   age:surviving_spouse_status + age:radiation_oncology + age:nuclearmedicine_lymphatic +
   age:diseases_cerebrovascular,
   
  data=modeldata2,
  method="glmStepAIC",
  trControl=train.control,
  trace=F)

# Model accuracy
step.model$results             
# parameter  Accuracy      Kappa AccuracySD    KappaSD
# 1      none 0.6037584 0.09901315 0.02142553 0.04572684

# Summary of the model
summary(step.model$finalModel) 

# Call:
#   NULL
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -3.3483  -1.2223   0.8133   1.0433   1.6249  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                     1.109e+03  2.031e+04   0.055 0.956468    
# age                                            -1.359e+01  2.477e+02  -0.055 0.956255    
# agegrp61                                        4.862e+00  1.013e+00   4.797 1.61e-06 ***
#   region31                                       -2.324e-01  8.239e-02  -2.821 0.004790 ** 
#   diagimaging_head_neck1                         -2.064e-01  1.023e-01  -2.017 0.043647 *  
#   dorsopathies1                                  -2.199e-01  1.025e-01  -2.144 0.032027 *  
#   diagimaging_spine1                             -2.121e-01  1.294e-01  -1.639 0.101151    
# abdominalpain1                                 -2.254e-01  1.074e-01  -2.099 0.035781 *  
#   diagimaging_chest1                             -1.238e-01  8.077e-02  -1.532 0.125422    
# bacterial_diseases1                             5.124e-01  1.868e-01   2.743 0.006083 ** 
#   diagultrasound_head_neck1                       2.945e-01  1.466e-01   2.009 0.044525 *  
#   nuclearmedicine_nervous1                       -1.883e+01  4.291e+03  -0.004 0.996499    
# calcium_supp1                                  -1.551e+00  1.173e+00  -1.322 0.186029    
# evocative_proc1                                 1.824e+01  3.694e+03   0.005 0.996060    
# nuclearmedicine_lymphatic1                      3.882e-01  9.584e-02   4.050 5.12e-05 ***
#   radiation_oncology1                             3.004e-01  7.945e-02   3.781 0.000156 ***
#   `surviving_spouse_statusnot alive/Unknown`     -1.109e+03  2.031e+04  -0.055 0.956462    
# `age:surviving_spouse_statusnot alive/Unknown`  1.359e+01  2.477e+02   0.055 0.956226    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 3988.7  on 2961  degrees of freedom
# Residual deviance: 3680.4  on 2944  degrees of freedom
# AIC: 3716.4
# 
# Number of Fisher Scoring iterations: 17

#############################
# tidy further model output #
#############################

stepsummary <- summary(step.model$finalModel)
out <- data.frame(coef(stepsummary))
rownames(out)[1] <- "intercept"
colnames(out) <- c("est", "sterror", "zvalue","pvalue")
out$parameter <- rownames(out)
stepwVars <- data.table(parameter=out$parameter, pvalue=format(out$pvalue,scientific=F))[order(pvalue)]

# print variables with p.values <0.05
stepwVars[pvalue<0.05] 

# parameter         pvalue
# 1:                   agegrp61 0.000001608512
# 2: nuclearmedicine_lymphatic1 0.000051202180
# 3:        radiation_oncology1 0.000156081463
# 4:                   region31 0.004789616162
# 5:        bacterial_diseases1 0.006082553960
# 6:              dorsopathies1 0.032027210982
# 7:             abdominalpain1 0.035781357691
# 8:     diagimaging_head_neck1 0.043647272197
# 9:  diagultrasound_head_neck1 0.044524740495
