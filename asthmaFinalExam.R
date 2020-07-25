##### FINAL EXAM SUMMER 2020 #####

library(psych)
library(summarytools)
library(e1071)
library(tidyverse)


# Create the function to get MODE value.
# Source: https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


asthmaLungFunctionData = read.csv("Lung_Function_Final.csv")

#### OVERVIEW of the dataset #####

describe(asthmaLungFunctionData)

str(asthmaLungFunctionData)


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
table(cut(asthmaLungFunctionData$Age, breaks=seq(0, 100, 10)))
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

shapiro.test(asthmaLungFunctionData$Age)
qqnorm(asthmaLungFunctionData$Age)
qqline(asthmaLungFunctionData$Age, col = "red")
outlierAges <- boxplot(asthmaLungFunctionData$Age, plot=FALSE)$out



x<-asthmaLungFunctionData
x<- x[-which(asthmaLungFunctionData$Age %in% outlierAges),]
res.ftest <- var.test(Age ~ Group, data = x)
res.ftest$p.value
bartlett.test(Age ~ Group, data=x)
leveneTest(Age ~ Group, data=x)
fligner.test(Age ~ Group, data = x)
plot(Age ~ Group, data = x)
t.test (Age ~ Group , var.equal=TRUE, data = x)
x %>% group_by(Group) %>% summarise(mean = mean(Age, na.rm=TRUE), sd = sd(Age, na.rm=TRUE), 
                                        median = median(Age, na.rm=TRUE) , min=min(Age, na.rm=TRUE),
                                        max=max(Age, na.rm=TRUE),n = n(), non_na_count = sum(!is.na(Age)))







# trash and examples below this line


#age outliers
#https://www.r-bloggers.com/how-to-remove-outliers-in-r/
outlierAges <- boxplot(lungCancer$Age, plot=FALSE)
x<-lungCancer
x<- x[-which(lungCancer$Age %in% outlierAges),]
min(x$Age)
max(x$Age)
range(x$Age)
mean(x$Age)
mean(x$Age, na.rm = TRUE)
median(x$Age)
sd(x$Age)

getmode(lungCancer$Age)
skewness(lungCancer$Age, na.rm = FALSE)
histo <- hist(lungCancer$Age)

table(cut(lungCancer$Age, breaks=seq(0, 100, 10)))

h = hist(lungCancer$Age) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE)