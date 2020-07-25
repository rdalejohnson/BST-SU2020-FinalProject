##### FINAL EXAM SUMMER 2020 #####

library(psych)
library(summarytools)
library(e1071)


# Create the function to get MODE value.
# Source: https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


asthmaLungFunctionData = read.csv("Lung_Function_Final.csv")

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

asthmaLungFunctionData %>% group_by(Group) %>% summarise(mean = mean(Age), sd = sd(Age), mode=getmode(Age), n = n())




#age outliers
#https://www.r-bloggers.com/how-to-remove-outliers-in-r/
outlierAges <- boxplot(lungCancer$Age, plot=FALSE)$out
x<-lungCancer
x<- x[-which(lungCancer$Age %in% outlierAges),]
min(x$Age)
max(x$Age)
range(x$Age)
mean(x$Age)
mean(x$Age, na.rm = TRUE)
median(x$Age)
sd(x$Age)






# trash and examples below this line



getmode(lungCancer$Age)
skewness(lungCancer$Age, na.rm = FALSE)
histo <- hist(lungCancer$Age)

table(cut(lungCancer$Age, breaks=seq(0, 100, 10)))

h = hist(lungCancer$Age) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE)