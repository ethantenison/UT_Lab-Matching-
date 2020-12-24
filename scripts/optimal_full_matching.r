#===========================================
#code for Chapter 5 "Propensity Score Matching" of book:
#Leite, W. L. (2017). Practical propensity score methods using R. 
#Thousand Oaks, CA: Sage. 

#PART 7 - OPTIMAL FULL MATCHING

# 
#this is the code that was used to generate the example results in the book
#As the R software and the R packages used in this example are updated frequently
#some incompatibilities between the current code and new R versions or package versions
#may appear
#Any updates to the code will be posted at:
# http://www.practicalpropensityscore.com


#This example estimates the effect of mothers having a job 
#that provides or subsidizes child care 
#on the length that they breastfeed their children 
#National Longitudinal Survey of Youth 1979 (NLSY79) 
#and the NLSY79 Children and Youth
setwd("C:/Users/tenis/Desktop/Data_Projects/UT_Lab-Matching")
#load data
load(file="data/external/Chapter5_data_with_propensity_scores_and_formula.rData")

#Perform optimal full matching 
library(MatchIt) #library for propensity score matching
library(optmatch)
fullMatching <- matchit(psFormula,distance=data$logitPScores, 
                        data = data, method = "full")
#the code above gives a warning that does not apply

#if you get an error that the maximum size of the problem was exceeded
#run this code: options("optmatch_max_problem_size" = Inf)
#before you call matchit


#diagnose covariate balance
balance.fullMatching <- summary(fullMatching, standardize=T)
#extract the table of balance after matching
table.balance <- balance.fullMatching$sum.matched


#------------------------------------------------------
#Estimate ATT with optimal full matched data
#using regression

#obtain matched data
data.fullMatching <- match.data(fullMatching)
#check the number of subclasses created
table(data.fullMatching$subclass)
#check the weights
table(data.fullMatching$weights)
sum(data.fullMatching$weights)

#estimate the treatment effect
library(survey)
design.fullMatching <- svydesign(ids=~1, weights=~weights,
                                 data=data.fullMatching)
#fit regression model
model.fullMatching <- svyglm(C0338600~childCare, design.fullMatching, family=gaussian())
summary(model.fullMatching)

#estimate treatment effects adjusting for possible violation of independence 
#due to matching of observations (cluster effects)
design.fullMatching2 <- svydesign(ids=~subclass, weights=~weights,
                                  data=data.fullMatching)
design.fullMatching2 <- as.svrepdesign(design.fullMatching2, type="bootstrap",
                                       replicates=1000)
model.fullMatching2 <- svyglm(C0338600~childCare, design.fullMatching2, family=gaussian())
summary(model.fullMatching2)