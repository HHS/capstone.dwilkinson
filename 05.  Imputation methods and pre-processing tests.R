#=============================================================================#
#
#The following code compares a variety of different imputation methods on a
#training data set to see which one produces the best results.  I'll be
#comparing imputing with a combination of the mean and median, both overall and
#by group, imputing usine k nearest neighbors, and imputing using bagged trees.
#Then, the code looks to find a subset of variables that will be the most
#predictive for the outcome. Finally, the code runs through multiple
#pre-processing options to find the best ones
#
#=============================================================================#

#  01.   Load packages and establish defaults ==================================
library(caret)
library(broom)
library(RVAideMemoire)
library(tidyverse)
library(gridExtra)

# Save the default options and turn off scientific notation
def.options <- options()
options(scipen = 999)

# Set the working directory to be the same as this file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Check the working directory
getwd()

# Set the ggplot theme
theme_set(theme_classic())

#  02.   Import/clean data =====================================================
load("./Data/d02_Final_Sample.Rda")

#All data sanitizing should have happened in the previous steps.  Check out the
#final sample headers just to be safe.
names(Final_Sample)
str(Final_Sample)

#Looks good.  

#Keep only variables with less than 16% missing. According to the documentation,
#count data are suppressed if a value is between 1 and 10 or counter suppressed
#if you can mathematically determine a value between 1 and 10.  For those, you
#can impute a value of 5 following the documentation's recommendation.
df_final <- Final_Sample %>% 
  select_if(function(x) sum(is.na(x))/length(x) <= 0.16) %>% 
  mutate_at(vars(contains("bene_count"),
                 -contains("suppress"),
                 contains("claim_count")), 
            function(x) ifelse(is.na(x), 5, x))

#  03.   Imputation method tests ================================================

#First, parition the data into training and testing sets
set.seed(1234)
ind <- createDataPartition(y = df_final$Group, p = 0.67, list = FALSE)
training <- df_final[ind, ]
testing <- df_final[-ind, ]

#Set your control for 10-fold cross-validation
set.seed(0522)
control <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

#Run a base logistic regression with only the non-missing data.  Any model you
#build after should be better than this one.
set.seed(1022)
fit.base <- train(Group ~ ., 
                  data = training %>% select_if(function(x) sum(is.na(x)) == 0), 
                  method = "glm", 
                  family = "binomial", 
                  preProcess = c("zv", "nzv"), 
                  trControl = control)

fit.base
#Accuracy   Kappa    
#0.5630952  0.1216957

#  03.1  Median/mean combination ==============================================

#Impute missing values by mean or median, where appropriate, and by group or
#overall, where appropriate

#First, examine the distributions of numeric variavables with missing data to
#determine if you want to impute by mean or median and by group or overall
training %>% 
  select_if(function(x) sum(is.na(x)) > 0) %>% 
  cbind(Group = training$Group) %>% 
  gather(variable, value, -Group) %>% 
  ggplot(aes(x = value, fill = Group)) +
  geom_density(color = "black", alpha = 3/4) +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Density Plots of Variables with Missing Data", 
       subtitle = "Distributions by Group")

#All of the distributions are skewed, except for average_age_of_beneficiaries
#and beneficiary_average_risk_score, which follow a more normal distribution.
#Use the Shapiro-Wilk test to test for normality of these two variables
shapiro_wilk <- training %>% 
  select(Group, 
         beneficiary_average_risk_score, 
         average_age_of_beneficiaries) %>% 
  gather(variable, value, -Group) %>% 
  group_by(Group, variable) %>%
  summarize(p.value = shapiro.test(value)$p.value)

shapiro_wilk

#The null-hypothesis of this test is that the population is normally
#distributed. Thus, on the one hand, if the p-value is less than the chosen
#alpha level, then the null hypothesis is rejected and there is evidence that
#the data tested are not from a normally distributed population. On the other
#hand, if the p-value is greater than the chosen alpha level, then the null
#hypothesis that the data came from a normally distributed population can not be
#rejected

#Based on the results of shapiro_wilk, the data are not normally distributed.
#Therefore, you should use a Use a non-parametric test to see if the means
#between the two groups are equal.

#Do a wilcoxon test between groups to see if they are different
wilcoxon_tests <- training %>% 
  select(Group, 
         beneficiary_average_risk_score, 
         average_age_of_beneficiaries) %>% 
  gather(variable, value, -Group) %>% 
  group_by(variable) %>% 
  do(tidy(wilcox.test(.$value ~ .$Group)))

wilcoxon_tests

#According to the wilcoxon tests, there are differences in the groups in some
#variables.  Impute based on group where the wilcoxon test is significant and
#with the overall mean where it is not
df_impute_wilcox <- training %>% 
  mutate_at(vars(wilcoxon_tests %>% filter(p.value >= 0.05) %>% pull(variable)), 
            function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)) %>% 
  group_by(Group) %>% 
  mutate_at(vars(wilcoxon_tests %>% filter(p.value < 0.05) %>% pull(variable)), 
            function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)) %>% 
  ungroup()

#Next, use the mood's test of medians to see if there is a difference between
#the medians of the rest of the variables.  
mood_tests <- df_impute_wilcox %>% 
  select_if(function(x) sum(is.na(x)) > 0) %>% 
  cbind(Group = df_impute_wilcox$Group) %>% 
  gather(variable, value, -Group) %>% 
  group_by(variable) %>% 
  summarize(p.value = mood.medtest(value ~ Group)$p.value) %>% 
  arrange(desc(p.value))

#See the resulsts and highlight the differences
df_impute_wilcox %>% 
  select_if(function(x) sum(is.na(x)) > 0) %>% 
  cbind(Group = df_impute_wilcox$Group) %>% 
  gather(variable, value, -Group) %>% 
  left_join(mood_tests) %>% 
  ggplot(aes(x = Group, y = value)) +
  geom_boxplot(aes(fill = ifelse(p.value < 0.05, "Different", "Same"))) +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Median's Between Groups", 
       subtitle = "Mood's Test of Medians",
       fill = "Result")

#There are a few variables that are significantly different.  For those, impute
#the median by group.  For the rest, impute the overall median
df_impute_mood <- df_impute_wilcox %>% 
  mutate_at(vars(mood_tests %>% filter(p.value >= 0.05) %>% pull(variable)), 
            function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)) %>% 
  group_by(Group) %>% 
  mutate_at(vars(mood_tests %>% filter(p.value < 0.05) %>% pull(variable)), 
            function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)) %>% 
  ungroup()

#See what's still missing
map_int(df_impute_mood, ~sum(is.na(.x)))

#Looks good!

#Examine the distributions again
df_impute_mood %>% 
  select_if(negate(is.factor)) %>% 
  cbind(Group = df_impute_mood$Group) %>% 
  gather(variable, value, -Group) %>% 
  ggplot(aes(x = value, fill = Group)) +
  geom_density(color = "black", alpha = 3/4) +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  scale_fill_brewer(palette = "Set1")

#Run a logistic regression with 10-fold cross validation and zv/nzv
set.seed(1022)
fit_mm <- train(Group ~ ., 
                data = df_impute_mood, 
                trControl = control, 
                method = "glm",
                family = "binomial", 
                preProcess = c("zv", "nzv"))

#Save the warnings
warnings_mm <- warnings()

#See the results
fit_mm
#Accuracy   Kappa    
#0.5511905  0.1016667

#Not so good!  Try it without the flag variable factors.
set.seed(1022)
fit_mm_flags <- train(Group ~ ., 
                      data = df_impute_mood %>% select(-contains("suppress")), 
                      trControl = control,
                      method = "glm",
                      family = "binomial", 
                      preProcess = c("zv", "nzv"))

#Save the warnings
warnings_mm_flag <- warnings()

#See the results
fit_mm_flags

#Accuracy   Kappa    
#0.5869048  0.1736667

#Much better without the flag variables but still not that great.

#  03.2  knnImpute  ============================================================

#Impute using caret's built-in knnImpute function.  Note that this will
#automatically center and scale the data.
set.seed(1022)
fit_knn <- train(Group ~ ., 
                 data = training, 
                 method = "glm", 
                 family = "binomial", 
                 preProcess = c("knnImpute", "zv", "nzv"), 
                 trControl = control, 
                 na.action = na.pass)

#Save the warnings
warnings_knn <- warnings()

#See the results
fit_knn
#Accuracy   Kappa    
#0.5714286  0.1456667

#Better than base, better than the median/mean imputation method, but not as
#good as the median/mean imputation without flags.  Try it without the flags
set.seed(1022)
fit_knn_flags <- train(Group ~ ., 
                       data = training %>% select(-contains("suppress")), 
                       method = "glm", 
                       family = "binomial", 
                       preProcess = c("knnImpute", "zv", "nzv"), 
                       trControl = control, 
                       na.action = na.pass)

#Save the warnings
warnings_fit_knn_flags <- warnings()

#See the results
fit_knn_flags

#Accuracy   Kappa    
#0.6029762  0.1996957

#Now we're cooking with gas!  60% accuracy is still not great, but so far it's
#the best

#  03.3  bagImpute ============================================================

#Impute using caret's built-in bagImpute function.  Note that this method is
#computationally taxing
set.seed(1022)
fit_bag <- train(Group ~ ., 
                 data = training, 
                 method = "glm", 
                 family = "binomial", 
                 preProcess = c("bagImpute", "zv", "nzv"), 
                 trControl = control, 
                 na.action = na.pass)

#Save the warnings
warnings_fit_bag <- warnings()

#See the results
fit_bag
#Accuracy   Kappa    
#0.5565476  0.1105652

#Worse than base, median/mean imputation, and knn imputation, probably because
#of the small number of observations.  Try it without the flag variables
set.seed(1022)
fit_bag_flags <- train(Group ~ ., 
                       data = training %>% select(-contains("suppress")),
                       method = "glm", 
                       family = "binomial", 
                       preProcess = c("bagImpute", "zv", "nzv"), 
                       trControl = control, 
                       na.action = na.pass)

#Save the warnings
warnings_fit_bag <- warnings()

#See the results
fit_bag_flags
#Accuracy   Kappa
#0.5595238  0.116

#Still pretty bad but slightly better without the flags.

#  04.   Summary ===============================================================

#So far, it looks like excluding the flag variables and imputing using k nearest
#neighbors produces the best accuracy.  Compare all of the models head-to-head
final_models <- mget(ls(pattern = "^fit_"))
dotplot(resamples(final_models))

#Yup, knn imputation without the flags is best!

#  05.   Semi-feature selection ===============================================

#Even though knn imputation without the flag variables worked best, there may
#some flag variables that will be helpful in predicting the outcome.  Rather
#than exclude all of them upfront, try a stepwise logistic regression to see if
#it can determine the best subset of features using knn imputation.  Run three
#models, one with all variables, one with the flags only, and one without the
#flags.

#To reduce redunancy, create a function for running the stepwise logistic
#regression
stepwise_lr <- function(df){
  set.seed(1022)
  train(Group ~ ., 
        data = df, 
        method = "glmStepAIC", 
        family = "binomial", 
        preProcess = c("knnImpute", "zv", "nzv"),
        trControl = control, 
        na.action = na.pass)
}

#Run the model for all variables
fit_step_all <- stepwise_lr(training)

#See the results
fit_step_all
#Accuracy   Kappa    
#0.5880952  0.1788205

#See the variable importance
varImp(fit_step_all)

#brand_suppress_flag, bene_count_ge65_suppress_flag, lis_suppress_flag, and
#nonlis_suppress_flag were the only flag variables that were deemed important
#out of the top 20 most important variables.

#Next, try it with flag variables only.  Note that there shouldn't be any
#imputation here
fit_step_factors <- stepwise_lr(training %>% select_if(is.factor))

#See the results
fit_step_factors
#Accuracy   Kappa    
#0.5803571  0.1509772

#It's interesting that the accuracy was relatively high (but still poor) with
#the factors only!

#See the variable importance
varImp(fit_step_factors)

#The same 4 flag variables were in the top 20, along with some other ones.

#Next, try it with the numeric variables only.
fit_step_numeric <- stepwise_lr(training %>% select(-contains("suppress")))

#See the results
fit_step_numeric
#Accuracy   Kappa   
#0.6630952  0.317029

#Whoa!  Look at that accuracy.  It's the best yet!  See the variable importance
varImp(fit_step_numeric)

#I'm fairly convinced that adding the flag variables is a bad idea.
#  06.   The kitchen sink =====================================================

#Just because I'm crazy, run through every possible combination of likely
#pre-processing options!

#  06.1  Generate a pre-processing options list ===============================

#Create a vector of possible pre-processing options.  Keep knnImpute and add in
#medianImpute as a control measure
base_options <- c("center", "scale", "YeoJohnson", "medianImpute", "knnImpute",
                  "corr", "nzv", "zv", "pca")

#Get all possible unique combinations with variable lengths of those
#pre-processing options
pp_options_all <- do.call(c, lapply(seq_along(base_options),
                                    function(x)
                                      combn(base_options, x, simplify = FALSE)))

#Add in names to the pre-processing list 
names(pp_options_all) <- sapply(pp_options_all, paste, collapse = " + ")


#There are some combinations that I don't want.  For example, I don't want more
#than one imputation method together, I don't want nzv and zv to be separated,
#and I don't want centering and scaling to be separated.  Update the
#pre-processing list to include only those elements that I want.
keep_pp_options <- sapply(pp_options_all, function(x){
  keep_cs <- sum(c("center", "scale") %in% x) != 1
  keep_var <- sum(c("zv", "nzv") %in% x) != 1
  keep_impute <- sum(grepl("Impute", x)) < 2
  
  keep_cs*keep_var*keep_impute
})

#Remove those elements to get a subset of the pre-processing options list
pp_options_subset <- pp_options_all[as.logical(keep_pp_options)]

#Finally, get a list of pre-processing options that contain an imputation
#function
pp_options_final <- pp_options_subset[sapply(pp_options_subset, function(x){
  any(grepl("Impute", x))
})]

#  06.2  Train models using the pre-processing options list ===================

#Run a simple logistic regression on all numeric variables (i.e., excluding the
#flag variables) in the training data set using the pre-processing options.  Add
#in an error catching function to keep this going in the event of an error.
#Time the function to see how long it takes (probably a looooong time!).
start_time <- Sys.time()

fits_pp_all <- lapply(pp_options_final, function(x){
  
  tryCatch({
    set.seed(1022)
    train(Group ~ ., 
          data = training %>% select(-contains("suppress")), 
          method = "glm", 
          family = "binomial", 
          preProcess = x, 
          na.action = na.pass)
  }, error = function(e) cat("ERROR :",conditionMessage(e), "\n"))
})
end_time <- Sys.time()

end_time - start_time

#Save the warnings
fits_pp_all_warnings <- warnings()

#Get the results
results <- resamples(fits_pp_all)

#  06.2. Compare pre-processing options ========================================

#Convert the statistics of the results into a tidy data frame
results_tidy <- results$values %>% 
  gather(model, value, -Resample) %>% 
  separate(col = model, 
           into = c("model", "metric"), 
           sep = "~") 

#Get a summary
results_summary <- results_tidy %>% 
  group_by(model, metric) %>% 
  summarize(mean = mean(value), 
            sem = sd(value)/sqrt(n()),
            median = median(value))

#Strangely, it looks like medianImpute performed slightly better than knn
#impute.  It also looks like removing highly correlated variables is worthwhile.
#PCA does not seem to increase accuracy.  Based on this, you should choose:

#knnImpute + corr + nzv + zv
set.seed(1022)
fit.final <- train(Group ~ ., 
      data = training %>% select(-contains("suppress")), 
      method = "glm", 
      family = "binomial", 
      preProcess = c("knnImpute", "zv", "nzv", "corr"),
      trControl = control, 
      na.action = na.pass)

fit.final$preProcess$method$remove

#  07.   Putting it all together