#==============================================================================#

#  The follow code performs an exploratory data analysis on my data set to get a
#  better idea of the distribution of the variables, the relationships between
#  the variables, and the relationships between the variables and the outcome
#  variable.

#==============================================================================#

#  01.   Load packages and establish defaults ==================================
library(caret)
library(broom)
library(tidyverse)
library(viridis)
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

#  03.   Data visualization ====================================================

#------------------------------------#
#                                    #
#     This is just a place holder    #
#                                    #
#------------------------------------#

#  03.1  Missing data  =========================================================

#Look at the amount of missing data per numeric variable
g_missing <- Final_Sample %>% 
  select_if(negate(is.factor)) %>% 
  summarise_all(function(x) round(sum(is.na(x))/length(x), 2)) %>% 
  gather(variable, missing) %>% 
  ggplot(aes(x = reorder(variable, missing), y = missing, fill = missing)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 1), 
                     labels = scales::percent) +
  coord_flip() +
  labs(x = NULL, 
       y = "Percent Missing",
       title = "Percent Missing Data per Variable", 
       subtitle = "Numeric variables only") +
  scale_fill_viridis(labels = scales::percent)

g_missing

#There is a lot of missing data, up to ~75% missing for some variables.  Only
#visualize variables with less than 16% missing data.  I chose 16% to keep some
#opioid variables.
df_final <- Final_Sample %>% 
  select_if(function(x) sum(is.na(x))/length(x) <= 0.16)

#There are 37 variables left, including the outcome variable (i.e., "Group).
#According to the documentation, count data are suppressed if a value is between
#1 and 10 or counter suppressed if you can mathematically determine a value
#between 1 and 10.  For those, you can impute a value of 5 following the
#documentation's recommendation.  For the rest, the missing data can  be imputed
#in a few different ways:
#  1.  A simple median imputation
#  2.  A simple mean imputation
#  3.  A combination of mean and median imputation 
#  4.  caret's knnImpute function
#  3.  caret's bagImpute function.
#Of the options, I'll likely choose one of the latter 3

#  03.2  Univariate Analysis - Numeric =========================================

# The numeric data in the Part D PUF can be divided into certain 'groups' (in no
# particular order): 
# 01.  Claim counts
# 02.  Beneficiary counts
# 03.  Fill counts
# 04.  Beneficiary demographics
# 05.  Claim costs
# 06.  Day's supply
# 07.  Other

#Visualize the data based on these different groups, starting with claims data
g_claims <- df_final %>% 
  select(Group, contains("claim")) %>% 
  gather(variable, value, -Group) %>% 
  ggplot(aes(x = value, fill = Group)) +
  geom_density(alpha = 0.75, color = "black") +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Claim Count Data by Group", 
       subtitle = "Variables with <= 16% Missing")

g_claims

#For the most part, all of the claim variables are highly right skewed and
#follow a Poisson distribution.  For these, imputing with the median would be
#more appropriate than imputing with the mean

#Now beneficiary data
g_bene <- df_final %>% 
  select(Group, contains("bene"), -contains("suppress")) %>% 
  gather(variable, value, -Group) %>% 
  ggplot(aes(x = value, fill = Group)) +
  geom_density(alpha = 0.75, color = "black") +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Beneficiary Data by Group", 
       subtitle = "Variables with <= 16% Missing")

g_bene

#bene_count looks fairly similar to the claims data.
#average_age_of_beneficiaries is slightly more symmetric but with a left skew.
#beneficiary_average_risk_score is slightly more symmetric than the claims data
#but still right skewed.  I might impute with the mean for
#average_age_of_beneficiaries and beneficiary_average_risk_score

#Now fill data
g_fill <- df_final %>% 
  select(Group, contains("fill"), -contains("suppress")) %>% 
  gather(variable, value, -Group) %>% 
  ggplot(aes(x = value, fill = Group)) +
  geom_density(alpha = 0.75, color = "black") +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Fill Data by Group", 
       subtitle = "Variables with <= 16% Missing")

g_fill

#Similar to claims, they're highly right skewed.

#Now cost data
g_cost <- df_final %>% 
  select(Group, contains("cost"), -contains("suppress")) %>% 
  gather(variable, value, -Group) %>% 
  ggplot(aes(x = value, fill = Group)) +
  geom_density(alpha = 0.75, color = "black") +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Cost Data by Group", 
       subtitle = "Variables with <= 16% Missing")

g_cost

#Again, more skewed.

#Now day's supply
g_supply <- df_final %>% 
  select(Group, contains("day"), -contains("suppress")) %>% 
  gather(variable, value, -Group) %>% 
  ggplot(aes(x = value, fill = Group)) +
  geom_density(alpha = 0.75, color = "black") +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Day's Supply Data by Group", 
       subtitle = "Variables with <= 16% Missing")

g_supply

#Not to sound like a broken record, but these are also skewed too!

#Finally, the other variable(s)
g_other <- df_final %>% 
  select(-contains("suppress")) %>% 
  gather(variable, value, -Group) %>% 
  filter(!grepl("claim|bene|fill|cost|day", variable)) %>% 
  ggplot(aes(x = value, fill = Group)) +
  geom_density(alpha = 0.75, color = "black") +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Other Variables by Group", 
       subtitle = "Variables with <= 16% Missing")

g_other

#Um yeah, basically everything looks the same

#  03.3  Univariate Analysis - Factors =========================================

#The previous section looked at numeric variables.  Next, look at factors.

g_factors <- df_final %>% 
  select_if(is.factor) %>% 
  gather(variable, value, -Group) %>% 
  count(Group, variable, value) %>% 
  ggplot(aes(x = value, y = n, fill = Group)) +
  geom_col(position = "dodge") +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Flag Variables by Group")

g_factors

#There are some differences between groups across variables.  NPDB physicians
#tend to have more complete data than the controls.  This could be because NPDB
#physicians tend to have more beneficiaries than controls, on average.  Just as
#a refresher, look at this to make sure it's true
qplot(x = bene_count, fill = Group, data = df_final, 
      geom = "density", alpha = 0.75)

df_final %>% 
  group_by(Group) %>% 
  summarize(mean = mean(bene_count, na.rm = TRUE), 
            median = median(bene_count, na.rm = TRUE), 
            max = max(bene_count, na.rm = TRUE))

#While the control group has the maximum value, NPDB has a higher median and
#mean than control.

#  03.4  Bivariate Analysis ===================================================

#There are a lot of variables here so make a correlation matrix plot.  First,
#you need to dummy code your factors in order to get correlations with them.
#For now, use pairwise complete obs, although this isn't the best method
cor_matrix_all <- df_final %>% 
  dummyVars(" ~ .", data = ., fullRank = TRUE) %>% 
  predict(df_final) %>% 
  cor(use = "pairwise.complete.obs") 

#Now, graph the data.
g_cor_all <- cor_matrix_all %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "var1") %>% 
  gather(var2, value, -var1) %>% 
  ggplot(aes(x = reorder(var1, value), 
             y = reorder(var2, value), 
             fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = NULL, 
       title = "Correlation Matrix - All", 
       caption = "Correlation uses pairwise complete obs")

g_cor_all

#There are some clusters of highly correlated variables.  For the most part,
#flag variables are weakly negatively correlated to everything.  Group is not
#really strongly correlated with anything

#Do another matrix using only numeric variables
cor_matrix_numeric <- df_final %>% 
  select_if(is.numeric) %>% 
  cbind(Group.NPDB = ifelse(df_final$Group == "NPDB", 1, 0)) %>% 
  cor(use = "pairwise.complete.obs") 

#Now, graph the data.
g_cor_numeric <- cor_matrix_numeric %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "var1") %>% 
  gather(var2, value, -var1) %>% 
  ggplot(aes(x = reorder(var1, value), 
             y = reorder(var2, value), 
             fill = value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = NULL, 
       title = "Correlation Matrix - Numeric", 
       caption = "Correlation uses pairwise complete obs")

g_cor_numeric

#Everything is fairly positively correlated with everything else, except for
#beneficiary risk score and opioid prescriber rate.  The NPDB group has a weak
#positive correlation to most variables except for risk score and age

#Finally, do one with the factors
cor_matrix_factors <- df_final %>% 
  select_if(is.factor) %>% 
  dummyVars(" ~ .", data = ., fullRank = TRUE) %>% 
  predict(df_final) %>% 
  cor() 

#Now, graph the data.
g_cor_factors <- cor_matrix_factors %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "var1") %>% 
  gather(var2, value, -var1) %>% 
  ggplot(aes(x = reorder(var1, value), 
             y = reorder(var2, value), 
             fill = value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = NULL, 
       title = "Correlation Matrix - Factors")

g_cor_factors
#Nothing really interesting stands out that wasn't seen before.

#  04.   Save graphs ===========================================================

#Put all the graphs into a list
graph_list <- mget(ls(pattern = "g_"))

#Make a pdf_filename that includes the location for the pdf 
pdf_location <- paste0(getwd(), "/Plots/EDA.pdf")

#Save the graphs to a pdf.  I'm not trying to make them look pretty, just saving
#them to see later on
pdf(pdf_location, onefile = TRUE, width = 12, height = 9)
for (i in seq(length(graph_list))) {
  print(graph_list[[i]])
}
dev.off()
