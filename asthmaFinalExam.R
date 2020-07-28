##### FINAL EXAM SUMMER 2020 #####

library(psych)
library(summarytools)
library(e1071)
library(tidyverse)


# Create the function to get MODE value.
# Source: https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(na.omit(v), uniqv)))]
}


asthmaLungFunctionData = read.csv("Lung_Function_Final.csv")

#### OVERVIEW of the dataset #####

describe(asthmaLungFunctionData)

str(asthmaLungFunctionData)


#### Adding columns to the dataset for later analysis #####
#### Adding columns to the dataset for later analysis #####
#### Adding columns to the dataset for later analysis #####
#### Adding columns to the dataset for later analysis #####
#### Adding columns to the dataset for later analysis #####


asthmaLungFunctionData$GenderMaleFemale <- 
    with(asthmaLungFunctionData, ifelse(is.na(Sex), NA,  ifelse(Sex==0, "Female", "Male")))

asthmaLungFunctionData$LungFunctionStatus <- 
  with(asthmaLungFunctionData, ifelse(is.na(Status), NA,  ifelse(Status==0, "Active", "Remission")))

asthmaLungFunctionData$TreatmentGroupDrugName <- 
  with(asthmaLungFunctionData, ifelse(is.na(Group), NA,  ifelse(Group==0, "Drug A/0", "Drug B/1")))

asthmaLungFunctionData$Week.0.to.12.difference <- 
  with(asthmaLungFunctionData, Week.12 - Week.0)

asthmaLungFunctionData$Week.0.to.60.difference <- 
  with(asthmaLungFunctionData, Week.60 - Week.0)




############################## DESCRIPTIVE STATISTICS ####################################
############################## DESCRIPTIVE STATISTICS ####################################
############################## DESCRIPTIVE STATISTICS ####################################
############################## DESCRIPTIVE STATISTICS ####################################
############################## DESCRIPTIVE STATISTICS ####################################

summarytools::freq(asthmaLungFunctionData$Age)
base::summary(asthmaLungFunctionData$Age)
getmode(asthmaLungFunctionData$Age)
e1071::skewness(asthmaLungFunctionData$Age)
histo <- graphics::hist(asthmaLungFunctionData$Age)
table(cut(asthmaLungFunctionData$Age, breaks=seq(0, 25, 1)))
histo$density = histo$counts/sum(histo$counts)*100


min(asthmaLungFunctionData$Age)
max(asthmaLungFunctionData$Age)
range(asthmaLungFunctionData$Age)
mean(asthmaLungFunctionData$Age)
mean(asthmaLungFunctionData$Age, na.rm = TRUE)
median(asthmaLungFunctionData$Age)
sd(asthmaLungFunctionData$Age)

####### AGE BY TREATMENT GROUP #######

asthmaLungFunctionData %>% group_by(Group) %>% 
  summarise(mean = mean(Age), 
                sd = sd(Age), 
                mode=getmode(Age), 
                min=min(Age), 
                max=max(Age), 
                median=median(Age),
                n = n())

outlierAges <- boxplot(asthmaLungFunctionData$Age, plot=FALSE)$out
outlierAges

shapiro.test(asthmaLungFunctionData$Age)
qqnorm(asthmaLungFunctionData$Age)
qqline(asthmaLungFunctionData$Age, col = "red")
outlierAges <- boxplot(asthmaLungFunctionData$Age, plot=TRUE)

##################### t-test for AGE BY TREATMENT GROUP ###################
##################### t-test for AGE BY TREATMENT GROUP ###################
##################### t-test for AGE BY TREATMENT GROUP ###################
##################### t-test for AGE BY TREATMENT GROUP ###################
##################### t-test for AGE BY TREATMENT GROUP ###################
##################### t-test for AGE BY TREATMENT GROUP ###################


library(car)

#http://www.sthda.com/english/wiki/f-test-compare-two-variances-in-r

res.ftest <- var.test(Age ~ Group, data = asthmaLungFunctionData)
res.ftest$p.value

bartlett.test(Age ~ Group, data=asthmaLungFunctionData)
lvtest <- leveneTest(Age ~ Group, data=asthmaLungFunctionData)
fligner.test(Age ~ Group, data = asthmaLungFunctionData)
plot(Age ~ Group, data = asthmaLungFunctionData)
t.test (Age ~ Group , var.equal=FALSE, data = asthmaLungFunctionData)
asthmaLungFunctionData %>% group_by(Group) %>% summarise(mean = mean(Age, na.rm=TRUE), sd = sd(Age, na.rm=TRUE), 
                                                 median = median(Age, na.rm=TRUE) , min=min(Age, na.rm=TRUE),
                                                 max=max(Age, na.rm=TRUE),n = n(), non_na_count = sum(!is.na(Age)))




######## TREATMENT GROUP BREAKDOWN/COUNT #########


freq(asthmaLungFunctionData$TreatmentGroupDrugName)
summary(asthmaLungFunctionData$TreatmentGroupDrugName)


########  GENDER ANALYSIS #############
########  GENDER ANALYSIS #############
########  GENDER ANALYSIS #############
########  GENDER ANALYSIS #############
########  GENDER ANALYSIS #############
########  GENDER ANALYSIS #############
########  GENDER ANALYSIS #############

freq(asthmaLungFunctionData$GenderMaleFemale)
summary(asthmaLungFunctionData$GenderMaleFemale)

by(asthmaLungFunctionData$GenderMaleFemale, asthmaLungFunctionData$Group, summary)

by(asthmaLungFunctionData$GenderMaleFemale, asthmaLungFunctionData$Group, freq)

xx <- table(asthmaLungFunctionData$GenderMaleFemale, asthmaLungFunctionData$Group)

xx

chiTreatmentGroup <- chisq.test(xx)
chiTreatmentGroup


phi(chiTreatmentGroup)

cramersV(chiTreatmentGroup)

test <- fisher.test(chiTreatmentGroup)
test




########  LUNG FUNCTION ANALYSIS #############
########  LUNG FUNCTION ANALYSIS #############
########  LUNG FUNCTION ANALYSIS #############
########  LUNG FUNCTION ANALYSIS #############
########  LUNG FUNCTION ANALYSIS #############
########  LUNG FUNCTION ANALYSIS #############


freq(asthmaLungFunctionData$LungFunctionStatus)
summary(asthmaLungFunctionData$LungFunctionStatus)

by(asthmaLungFunctionData$LungFunctionStatus, asthmaLungFunctionData$Group, summary)

by(asthmaLungFunctionData$LungFunctionStatus, asthmaLungFunctionData$Group, freq)

xx <- table(asthmaLungFunctionData$LungFunctionStatus, asthmaLungFunctionData$Group)

xx

chiTreatmentGroup <- chisq.test(xx)
chiTreatmentGroup


phi(chiTreatmentGroup)

cramersV(chiTreatmentGroup)

test <- fisher.test(chiTreatmentGroup)
test



################# WEEK ZERO #################
################# WEEK ZERO #################
################# WEEK ZERO #################
################# WEEK ZERO #################
################# WEEK ZERO #################



#summarytools::freq(asthmaLungFunctionData$Week.0)
base::summary(asthmaLungFunctionData$Week.0)
getmode(asthmaLungFunctionData$Week.0)
e1071::skewness(asthmaLungFunctionData$Week.0)
histo <- graphics::hist(asthmaLungFunctionData$Week.0)
table(cut(asthmaLungFunctionData$Week.0, breaks=seq(0, 100, 10)))

histo$density = histo$counts/sum(histo$counts)*100


min(asthmaLungFunctionData$Week.0)
max(asthmaLungFunctionData$Week.0)
range(asthmaLungFunctionData$Week.0)
mean(asthmaLungFunctionData$Week.0)
mean(asthmaLungFunctionData$Week.0, na.rm = TRUE)
median(asthmaLungFunctionData$Week.0)
sd(asthmaLungFunctionData$Week.0)

####### WEEK ZERO BY TREATMENT GROUP #######

asthmaLungFunctionData %>% group_by(Group) %>% 
  summarise(mean = mean(Week.0), 
            sd = sd(Week.0), 
            mode=getmode(Week.0), 
            min=min(Week.0), 
            max=max(Week.0), 
            median=median(Week.0),
            n = n())

outlierz <- boxplot(asthmaLungFunctionData$Week.0, plot=FALSE)$out

outlierz

shapiro.test(asthmaLungFunctionData$Week.0)
qqnorm(asthmaLungFunctionData$Week.0)
qqline(asthmaLungFunctionData$Week.0, col = "red")
outlierz <- boxplot(asthmaLungFunctionData$Week.0, plot=TRUE)

##################### t-test for WEEK ZERO BY TREATMENT GROUP ###################
##################### t-test for WEEK ZERO BY TREATMENT GROUP ###################
##################### t-test for WEEK ZERO BY TREATMENT GROUP ###################

res.ftest <- var.test(Week.0 ~ Group, data = asthmaLungFunctionData)
res.ftest$p.value

bartlett.test(Week.0 ~ Group, data=asthmaLungFunctionData)
lvtest <- leveneTest(Week.0 ~ Group, data=asthmaLungFunctionData)
fligner.test(Week.0 ~ Group, data = asthmaLungFunctionData)
plot(Week.0 ~ Group, data = asthmaLungFunctionData)
t.test (Week.0 ~ Group , var.equal=FALSE, data = asthmaLungFunctionData)
asthmaLungFunctionData %>% group_by(Group) %>% summarise(mean = mean(Week.0, na.rm=TRUE), sd = sd(Week.0, na.rm=TRUE), 
                                                         median = median(Week.0, na.rm=TRUE) , min=min(Week.0, na.rm=TRUE),
                                                         max=max(Week.0, na.rm=TRUE),n = n(), non_na_count = sum(!is.na(Week.0)))

shapiro.test(asthmaLungFunctionData$Week.0)
qqnorm(asthmaLungFunctionData$Week.0)
qqline(asthmaLungFunctionData$Week.0, col = "red")
outlierAges <- boxplot(asthmaLungFunctionData$Week.0, plot=FALSE)







################# WEEK TWELVE #################
################# WEEK TWELVE #################
################# WEEK TWELVE #################
################# WEEK TWELVE #################
################# WEEK TWELVE #################



#summarytools::freq(asthmaLungFunctionData$Week.12)
base::summary(asthmaLungFunctionData$Week.12)
getmode(asthmaLungFunctionData$Week.12)
e1071::skewness(asthmaLungFunctionData$Week.12, na.rm = TRUE)
histo <- graphics::hist(asthmaLungFunctionData$Week.12)
table(cut(asthmaLungFunctionData$Week.12, breaks=seq(0, 100, 10)))

histo$density = histo$counts/sum(histo$counts)*100


min(asthmaLungFunctionData$Week.12, na.rm = TRUE)
max(asthmaLungFunctionData$Week.12, na.rm = TRUE)
range(asthmaLungFunctionData$Week.12, na.rm = TRUE)
mean(asthmaLungFunctionData$Week.12, na.rm = TRUE)
getmode(asthmaLungFunctionData$Week.12)
median(asthmaLungFunctionData$Week.12, na.rm = TRUE)
sd(asthmaLungFunctionData$Week.12, na.rm = TRUE)

####### WEEK TWELVE BY TREATMENT GROUP #######

asthmaLungFunctionData %>% group_by(Group) %>% 
  summarise(mean = mean(Week.12, na.rm = TRUE), 
            sd = sd(Week.12, na.rm = TRUE), 
            mode=getmode(Week.12), 
            min=min(Week.12, na.rm = TRUE), 
            max=max(Week.12, na.rm = TRUE), 
            median=median(Week.12, na.rm = TRUE),
            n = n())

outlierAges <- boxplot(asthmaLungFunctionData$Week.12, plot=FALSE)$out

##################### t-test for WEEK TWELVE BY TREATMENT GROUP ###################
##################### t-test for WEEK TWELVE BY TREATMENT GROUP ###################
##################### t-test for WEEK TWELVE BY TREATMENT GROUP ###################

res.ftest <- var.test(Week.12 ~ Group, data = asthmaLungFunctionData)
res.ftest$p.value

bartlett.test(Week.12 ~ Group, data=asthmaLungFunctionData)
lvtest <- leveneTest(Week.12 ~ Group, data=asthmaLungFunctionData)
fligner.test(Week.12 ~ Group, data = asthmaLungFunctionData)
plot(Week.12 ~ Group, data = asthmaLungFunctionData)
t.test (Week.12 ~ Group , var.equal=FALSE, data = asthmaLungFunctionData)
asthmaLungFunctionData %>% group_by(Group) %>% summarise(mean = mean(Week.12, na.rm=TRUE), sd = sd(Week.12, na.rm=TRUE), 
                                                         median = median(Week.12, na.rm=TRUE) , min=min(Week.12, na.rm=TRUE),
                                                         max=max(Week.12, na.rm=TRUE),n = n(), non_na_count = sum(!is.na(Week.12)))

shapiro.test(asthmaLungFunctionData$Week.12)
qqnorm(asthmaLungFunctionData$Week.12)
qqline(asthmaLungFunctionData$Week.12, col = "red")
outlierAges <- boxplot(asthmaLungFunctionData$Week.12, plot=FALSE)







################# WEEK SIXTY #################
################# WEEK SIXTY #################
################# WEEK SIXTY #################
################# WEEK SIXTY #################
################# WEEK SIXTY #################



#summarytools::freq(asthmaLungFunctionData$Week.60)
base::summary(asthmaLungFunctionData$Week.60)
getmode(asthmaLungFunctionData$Week.60)
e1071::skewness(asthmaLungFunctionData$Week.60, na.rm = TRUE)
histo <- graphics::hist(asthmaLungFunctionData$Week.60)
table(cut(asthmaLungFunctionData$Week.60, breaks=seq(0, 100, 10)))

histo$density = histo$counts/sum(histo$counts)*100


min(asthmaLungFunctionData$Week.60, na.rm = TRUE)
max(asthmaLungFunctionData$Week.60, na.rm = TRUE)
range(asthmaLungFunctionData$Week.60, na.rm = TRUE)
mean(asthmaLungFunctionData$Week.60, na.rm = TRUE)
getmode(asthmaLungFunctionData$Week.60)
median(asthmaLungFunctionData$Week.60, na.rm = TRUE)
sd(asthmaLungFunctionData$Week.60, na.rm = TRUE)

####### WEEK SIXTY BY TREATMENT GROUP #######

asthmaLungFunctionData %>% group_by(Group) %>% 
  summarise(mean = mean(Week.60, na.rm = TRUE), 
            sd = sd(Week.60, na.rm = TRUE), 
            mode=getmode(Week.60), 
            min=min(Week.60, na.rm = TRUE), 
            max=max(Week.60, na.rm = TRUE), 
            median=median(Week.60, na.rm = TRUE),
            n = n())

outlierAges <- boxplot(asthmaLungFunctionData$Week.60, plot=FALSE)$out

##################### t-test for WEEK SIXTY BY TREATMENT GROUP ###################
##################### t-test for WEEK SIXTY BY TREATMENT GROUP ###################
##################### t-test for WEEK SIXTY BY TREATMENT GROUP ###################

res.ftest <- var.test(Week.60 ~ Group, data = asthmaLungFunctionData)
res.ftest$p.value

bartlett.test(Week.60 ~ Group, data=asthmaLungFunctionData)
lvtest <- leveneTest(Week.60 ~ Group, data=asthmaLungFunctionData)
fligner.test(Week.60 ~ Group, data = asthmaLungFunctionData)
plot(Week.60 ~ Group, data = asthmaLungFunctionData)
t.test (Week.60 ~ Group , var.equal=FALSE, data = asthmaLungFunctionData)
asthmaLungFunctionData %>% group_by(Group) %>% summarise(mean = mean(Week.60, na.rm=TRUE), sd = sd(Week.60, na.rm=TRUE), 
                                                         median = median(Week.60, na.rm=TRUE) , min=min(Week.60, na.rm=TRUE),
                                                         max=max(Week.60, na.rm=TRUE),n = n(), non_na_count = sum(!is.na(Week.60)))

shapiro.test(asthmaLungFunctionData$Week.60)
qqnorm(asthmaLungFunctionData$Week.60)
qqline(asthmaLungFunctionData$Week.60, col = "red")
outlierAges <- boxplot(asthmaLungFunctionData$Week.60, plot=TRUE)




######################   ##### ## MULIPLE LINEAR STARTS HERE REGRESSION  ####################  #############
######################   ##### ## MULIPLE LINEAR STARTS HERE REGRESSION  ####################  #############
######################   ##### ## MULIPLE LINEAR STARTS HERE REGRESSION  ####################  #############
######################   ##### ## MULIPLE LINEAR STARTS HERE REGRESSION  ####################  #############
######################   ##### ## MULIPLE LINEAR STARTS HERE REGRESSION  ####################  #############



library(rms)
library(car)
library(ggplot2)
library(ggpubr)

########################EVALUATE ALL PAIRS OF VARIABLE INTERACTIONS ##########


################ #AGE AND GENDER
#Levene's test for equality of variance
var.test(asthmaLungFunctionData$Age ~ asthmaLungFunctionData$Sex)

#unequal variances t-test
t.test(asthmaLungFunctionData$Age ~ asthmaLungFunctionData$Sex, var.equal=F)

#equal variances t-test
t.test(asthmaLungFunctionData$Age ~ asthmaLungFunctionData$Sex, var.equal=T)

################ #AGE AND GROUP
#Levene's test for equality of variance
var.test(asthmaLungFunctionData$Age ~ asthmaLungFunctionData$Group)

#unequal variances t-test
t.test(asthmaLungFunctionData$Age ~ asthmaLungFunctionData$Group, var.equal=F)

#equal variances t-test
t.test(asthmaLungFunctionData$Age ~ asthmaLungFunctionData$Group, var.equal=T)


###### AGE AND WEEK.0.to.12.difference

scatterplot(Week.0.to.12.difference ~ Age, data = asthmaLungFunctionData)
plot(x=asthmaLungFunctionData$Age,y=asthmaLungFunctionData$Week.0.to.12.difference)
Hmisc::rcorr(x=asthmaLungFunctionData$Age,y=asthmaLungFunctionData$Week.0.to.12.difference, type=c("spearman"))
Hmisc::rcorr(x=asthmaLungFunctionData$Week.0.to.12.difference,y=asthmaLungFunctionData$Age, type=c("spearman"))

Hmisc::rcorr(x=asthmaLungFunctionData$Age,y=asthmaLungFunctionData$Week.0.to.12.difference, type=c("pearson"))

#complete.obs means only complete rows, ignore NA values
cor(asthmaLungFunctionData$Age, asthmaLungFunctionData$Week.0.to.12.difference, use = "complete.obs", method = "pearson")

cor.test(asthmaLungFunctionData$Age, asthmaLungFunctionData$Week.0.to.12.difference)

ggs = ggscatter(asthmaLungFunctionData, x = "Age", y = "Week.0.to.12.difference", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Age", ylab = "Week.0.to.12.difference") 
ggs



###### AGE AND WEEK.0.to.60.difference

scatterplot(Week.0.to.60.difference ~ Age, data = asthmaLungFunctionData)
plot(x=asthmaLungFunctionData$Age,y=asthmaLungFunctionData$Week.0.to.60.difference)
Hmisc::rcorr(x=asthmaLungFunctionData$Age,y=asthmaLungFunctionData$Week.0.to.60.difference, type=c("spearman"))
Hmisc::rcorr(x=asthmaLungFunctionData$Week.0.to.60.difference,y=asthmaLungFunctionData$Age, type=c("spearman"))

Hmisc::rcorr(x=asthmaLungFunctionData$Age,y=asthmaLungFunctionData$Week.0.to.60.difference, type=c("pearson"))

#complete.obs means only complete rows, ignore NA values
cor(asthmaLungFunctionData$Age, asthmaLungFunctionData$Week.0.to.60.difference, use = "complete.obs", method = "pearson")

cor.test(asthmaLungFunctionData$Age, asthmaLungFunctionData$Week.0.to.12.difference)

ggs = ggscatter(asthmaLungFunctionData, x = "Age", y = "Week.0.to.60.difference", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Age", ylab = "Week.0.to.60.difference") 
ggs



######### GENDER AND TREATMENT GROUP Group

gender.drug.group.table = table(asthmaLungFunctionData$GenderMaleFemale, asthmaLungFunctionData$TreatmentGroupDrugName)
Xsq <- chisq.test(gender.drug.group.table)
library(vcd)
assocstats(gender.drug.group.table)
Xsq$expected


################ #gender  AND week0-to-12 scores
#Levene's test for equality of variance
var.test(asthmaLungFunctionData$Week.0.to.12.difference ~ asthmaLungFunctionData$GenderMaleFemale)

#unequal variances t-test
t.test(asthmaLungFunctionData$Week.0.to.12.difference ~ asthmaLungFunctionData$GenderMaleFemale, var.equal=F)

#equal variances t-test
t.test(asthmaLungFunctionData$Week.0.to.12.difference ~ asthmaLungFunctionData$GenderMaleFemale, var.equal=T)


################ #gender  AND week0-to-60 scores
#Levene's test for equality of variance
var.test(asthmaLungFunctionData$Week.0.to.60.difference ~ asthmaLungFunctionData$GenderMaleFemale)

#unequal variances t-test
t.test(asthmaLungFunctionData$Week.0.to.60.difference ~ asthmaLungFunctionData$GenderMaleFemale, var.equal=F)

#equal variances t-test
t.test(asthmaLungFunctionData$Week.0.to.60.difference ~ asthmaLungFunctionData$GenderMaleFemale, var.equal=T)



################ #treatment drug group  AND week0-to-12 scores
#Levene's test for equality of variance
var.test(asthmaLungFunctionData$Week.0.to.12.difference ~ asthmaLungFunctionData$TreatmentGroupDrugName)

#unequal variances t-test
t.test(asthmaLungFunctionData$Week.0.to.12.difference ~ asthmaLungFunctionData$TreatmentGroupDrugName, var.equal=F)

#equal variances t-test
t.test(asthmaLungFunctionData$Week.0.to.12.difference ~ asthmaLungFunctionData$TreatmentGroupDrugName, var.equal=T)


################ #treatment drug group  AND week0-to-60 scores
#Levene's test for equality of variance
var.test(asthmaLungFunctionData$Week.0.to.60.difference ~ asthmaLungFunctionData$TreatmentGroupDrugName)

#unequal variances t-test
t.test(asthmaLungFunctionData$Week.0.to.60.difference ~ asthmaLungFunctionData$TreatmentGroupDrugName, var.equal=F)

#equal variances t-test
t.test(asthmaLungFunctionData$Week.0.to.60.difference ~ asthmaLungFunctionData$TreatmentGroupDrugName, var.equal=T)


#################### WEEK 12 and WEEK 60  ####################################

scatterplot(Week.0.to.60.difference ~ Week.0.to.12.difference, data = asthmaLungFunctionData)
plot(x=asthmaLungFunctionData$Week.0.to.12.difference,y=asthmaLungFunctionData$Week.0.to.60.difference)
Hmisc::rcorr(x=asthmaLungFunctionData$Week.0.to.12.difference,y=asthmaLungFunctionData$Week.0.to.60.difference, type=c("spearman"))
Hmisc::rcorr(x=asthmaLungFunctionData$Week.0.to.60.difference,y=asthmaLungFunctionData$Week.0.to.12.difference, type=c("spearman"))

Hmisc::rcorr(x=asthmaLungFunctionData$Week.0.to.12.difference,y=asthmaLungFunctionData$Week.0.to.60.difference, type=c("pearson"))

#complete.obs means only complete rows, ignore NA values
cor(asthmaLungFunctionData$Week.0.to.12.difference, asthmaLungFunctionData$Week.0.to.60.difference, use = "complete.obs", method = "pearson")

cor.test(asthmaLungFunctionData$Week.0.to.12.difference, asthmaLungFunctionData$Week.0.to.12.difference)

ggs = ggscatter(asthmaLungFunctionData, x = "Week.0.to.12.difference", y = "Week.0.to.60.difference", 
                add = "reg.line", conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Week.0.to.12.difference", ylab = "Week.0.to.60.difference") 
ggs


################# MULTIPLE LINEAR REGRESSION EQUATION AND MODEL #############

#http://www.cookbook-r.com/Manipulating_data/Changing_the_order_of_levels_of_a_factor/
#RELEVEL: If the factor is unordered, then the levels will still appear in some order, 
#but the specific order of the levels matters 
#only for convenience (pen, pencil, brush) – it will determine, 
#for example, how output will be printed, or the arrangement of items on a graph.




mod12=ols   (Week.0.to.12.difference~TreatmentGroupDrugName+Age+Sex,data=asthmaLungFunctionData)

mod12.lms=lm(Week.0.to.12.difference~TreatmentGroupDrugName+Age+Sex,data=asthmaLungFunctionData)


mod12
mod12.lms
summary(mod12.lms)

confint(mod12)



mod60=ols   (Week.0.to.60.difference~TreatmentGroupDrugName+Age+Sex,data=asthmaLungFunctionData)

mod60.lms=lm(Week.0.to.60.difference~TreatmentGroupDrugName+Age+Sex,data=asthmaLungFunctionData)


mod60
mod60.lms
summary(mod60.lms)

confint(mod60)





mod60AgeGenderOnly = ols   (Week.0.to.60.difference~Age+Sex,data=asthmaLungFunctionData)

mod60AgeGenderOnly.lms = lm(Week.0.to.60.difference~Age+Sex,data=asthmaLungFunctionData)


mod60AgeGenderOnly
mod60AgeGenderOnly.lms
summary(mod60AgeGenderOnly.lms)

confint(mod60AgeGenderOnly)



