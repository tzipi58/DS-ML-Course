
#Loading libraries:

library(Hmisc)
library(PerformanceAnalytics)
library(RcmdrMisc)
library(Rmisc)
library(GGally)
library(imputeTS)
library(pheatmap)
library(cocor)
library(mice)
library(psych)
library(moments)
library(ggplot2)
library(dplyr)
library(DescTools)
library(car)
library(e1071)
library(nortest)
library(corrplot)
library(purrr)
library(knitr)
library(rmarkdown)


#Import data:
  
df <- read.csv("C:/R exc2/flat_file.csv",header = T)

# 2.1 Descriptive analysis:

#Numerical variables:

df.numeric<-df[c(2,4,6,8,13,14,19:20,21,27:32,39:43,63:84)]
descriptive.num<-sapply(df.numeric, function(x) c(valid_N = sum(complete.cases(x)),
                                       NAs=7375-sum(complete.cases(x)),
                                       means = mean(na.omit(x)),
                                       medi = median(na.omit(x)),
                                       std = sd(na.omit(x)),
                                       CI = CI(na.omit(x), ci=0.95),
                                       min = min(na.omit(x)),
                                       max = max(na.omit(x)),
                                       range= max(na.omit(x))-min(na.omit(x)), 
                                       percent5 = quantile(na.omit(x), 0.05),
                                       percent95 = quantile(na.omit(x), 0.95),
                                       skews = skewness(na.omit(x)),
                                       kurt = kurtosis(na.omit(x)))
)

descriptive.num

##Descriptive -Categorical variables:
  
df.Categorical<-df[c(3,7,14:15,17)]
descriptive.Categorical<-sapply(df.Categorical, function(x) c(validN=sum(complete.cases(x)),
                                       NAs=7375-sum(complete.cases(x)),
                                       mods = Mode(x),
                                       Tabs=table(x))
)

descriptive.Categorical

##Dichotomous variables:
  
df.Dichotomous<-df[c(9:12,16,22:26,33:38,44:62)]
descriptive.Dichotomous<-sapply(df.Dichotomous, function(x) c(validN=sum(complete.cases(x)),
                                         NAs=7375-sum(complete.cases(x)),
                                         mods = Mode(x),
                                         Tabs=table(x))
)
descriptive.Dichotomous


#2.2 Graphs:

##Histograms for Continues variables:
par(mar = c(1, 1, 1, 1))  
df.numeric.matrix<-data.matrix(df.numeric)
for (x in 1:length(df.numeric)) 
{hist(df.numeric.matrix[,x], breaks=30, main = colnames(df.numeric.matrix)[x])
  plot(density(na.omit(df.numeric.matrix[,x])))}

## Normality tests for the numerical variables:

x_sample <- rnorm(3000)
sapply(df.numeric, function(x) c(print(ks.test(x_sample,na.omit(x),alternative = "two.sided")),
                             plot(ecdf(na.omit(x))))
)

# The conclusion is - that all continuous variables are not normally distributed and that there are many outliers.

# Bar plots for the categorical/dichotomous variables:
  
par(mar = c(4, 5, 3, 2)) 
par(oma = c(4, 5, 3, 2))
barplot(sapply(df.Dichotomous, function(x) table(-x)/nrow(df.Dichotomous)),col=c("grey","white"), las=2, main = "Dichtomous var, grey=Yes(NAs Rate)")


barplot(table(df.Categorical$original_language),main = "original_language")
barplot(table(df.Categorical$runtime_cat),main = "runtime_cat")
barplot(table(df.Categorical$release_day),main = "release_day")
barplot(table(df.Categorical$release_month),main = "release_month")
barplot(table(df.Categorical$release_year),main = "release_year")

#The length of most movies is medium.
#The popular day for movie release is the first day of the month. 
#In the 9-12 months most of the movies come out.
#Most of the main actors are men. 
#As the years go by there are more movies.

#2.3 Correlations:

#Scatter plots selected -  to describe all the correlations between all the continuous variables. numerical variables 

df.numeric.columns = colnames(df.numeric)

count=0
for(i in 1:(ncol(df.numeric)-1)) 
  for (j in i:ncol(df.numeric))
  {       # for-loop over columns
    X<- df.numeric[ , i]
    Y<- df.numeric[ , j]
    if (i!= j) {
      print(c(i,j))
      print(ggplot(df.numeric, aes(X, Y)) + geom_point() + geom_smooth(method = "lm") +
              xlab(as.character(df.numeric.columns[i])) + ylab(as.character(df.numeric.columns[j])))
      rm(X, Y)
    }
    count=count+1
  }

#Generate correlations:
  
#Since all the continuous variables were not found to be normally distributed - I performed a spearman correlation matrix 
# between all the continuous and dichotomous variables.
# Since there are variables with missing values  used a correlation command that allows pairwise.
df.correlations<-(df[-c(1,3,5,7,11,14:15,18)])

# correlation with P value and Valid numbers
rcorrelation<-rcorr.adjust(as.matrix(df.correlations),type="spearman",use="pairwise.complete.obs")
cat("\n Correlations:")
rcorrelation$R$r

cat("\n P values:")
rcorrelation$R$P

cat("\n Number of observations:")
rcorrelation$R$n

#2.4 The target variable

#Section 2.1 described all the continuous variables.
#including the revenue variable.
#See above for histogram, and normal distribution. 
#Only about 3,000 movies (out of 7375) have a value in the revenue variable. 
#this variable is not a normal distribution.
#the distribution is with a right tail.
# a few films have a very high revenues.

##Revenue by Categorical variables:

#max_revenue=max(df$revenue)

ggplot(data=df, mapping = aes(x=as.factor(original_language), y=revenue)) + 
  geom_boxplot() + geom_point() + theme_bw() +
  labs(y ="revenue", x="original_language", title = "revenue as a function of original_language") + 
  stat_boxplot(geom="errorbar",width=0.2) + coord_cartesian(ylim = c(0,1e8))

ggplot(data=df, mapping = aes(x=as.factor(runtime_cat), y=revenue)) + 
  geom_boxplot() + geom_point() + theme_bw() +
  labs(y ="revenue", x="runtime_cat", title = "revenue as a function of runtime_cat") + 
  stat_boxplot(geom="errorbar",width=0.2) + coord_cartesian(ylim = c(0,2e8))

ggplot(data=df, mapping = aes(x=as.factor(release_year), y=revenue)) + 
  geom_boxplot() + geom_point() + theme_bw() +
  labs(y ="revenue", x="release_year", title = "revenue as a function of release_year") + 
  stat_boxplot(geom="errorbar",width=0.2) + coord_cartesian(ylim = c(0,1e8))

ggplot(data=df, mapping = aes(x=as.factor(release_month), y=revenue)) + 
  geom_boxplot() + geom_point() + theme_bw() +
  labs(y ="revenue", x="release_month", title = "revenue as a function of release_month") + 
  stat_boxplot(geom="errorbar",width=0.2) + coord_cartesian(ylim = c(0,1e8))

ggplot(data=df, mapping = aes(x=as.factor(release_day), y=revenue)) + 
  geom_boxplot() + geom_point() + theme_bw() +
  labs(y ="revenue", x="release_day", title = "revenue as a function of release_day") + 
  stat_boxplot(geom="errorbar",width=0.2) + coord_cartesian(ylim = c(0,1e8))

#The variables most correlated with revenue are budget and leading actors previous movie count (total and even more 5 years). These trends are strongly affected by outliers.
#It is probable that correlation between leading actors and budget mediate the final correlation with revenue.
#Some common languages (e.g., English and Japanese) indicate a larger revenue but only compared with less common languages. It is not a clear trend (statistically significant).
#Later years suggest increased revenue but this is also not a clear trend. 

#2.5 Outliers

#For numeric variables outliers are found outside the confidence interval. 

lapply(1:ncol(df.numeric), function(i) boxplot(df.numeric.matrix[, i], main = colnames(df.numeric.matrix)[i]))

# Most variables have high outliers values - distribution with right tail distribution.
#"release year" variable is an example of a variable with low extremity values.
#in the first years there are fewer Movies per year compared to recent years.
# this is distribution with left tail. 
#The "runtime" variable is an example of a variable with high and low extremity values - two-tailed.
# with a slight tendency to right-tailed.

#2.6 Missing data

#2.6 - Missing values were calculated in section 2.1 
# The variable that has the most missing values is revenue (4382/7375).
#The variables that examined the historical revenue of the main actor and the two sub-actors 
# were built on the basis of this database.
# Therefore, it is not surprising that these variables also have many missing values.


#2.7 Missing data matrix

missing.matrix<-data.matrix(df)
missing.matrix[!is.na(missing.matrix)] = 0  #There is data available
missing.matrix[is.na(missing.matrix)] = 1   #NA value

par(mar = c(3, 2, 3, 2))  #bot, left, top, right
par(oma = c(3, 3, 3, 3))
heatmap(missing.matrix, scale = "none")

#3 Data cleansing

#3.1 Variables with and without outliers

## 1 Histograms with and without outliers:

df.numeric.matrix<-data.matrix(df.numeric)
for (x in 1:length(df.numeric)) 
{# Find the 25th and the 75th percentile of the dataset
  rm(subs, up, low, Q, iqr)
  Q<- quantile(df.numeric.matrix[,x], probs = c(0.25,0.75), na.rm = TRUE)
  # Find the difference of the 75th and 25th percentiles
  iqr <- IQR(df.numeric.matrix[,x], na.rm = TRUE)
  # Find the cut-off ranges
  up <-  Q[2]+1.5*iqr # Upper Range  
  low <- Q[1]-1.5*iqr # Lower
  subs<- subset(df.numeric.matrix[,x],
                      ((df.numeric.matrix[,x] > low) & (df.numeric.matrix[,x] < up)))
  #remove cases of empty subset
  if (length(subs) != 0) {
    print(x)
    hist(df.numeric.matrix[,x], breaks=30, main = colnames(df.numeric.matrix)[x])
    hist(subs, breaks=30, main = 'No outliers')
    plot(density(na.omit(df.numeric.matrix[,x])), main = colnames(df.numeric.matrix)[x])
    plot(density(subs), main = 'No outliers')
  }
}

#Based on the formula: Q [1,2] ± 1.5 * iqr - I cleared the outliers values
# in each of the continuous variables. 
# Graphs were made for the variables with and without the outliers values.
#No significant differences were seen between the distributions.


#Variables without outliers:

colnames(df.numeric.matrix)[c(9, 27, 30, 32:34,36:38, 40:42)]



## 2 Scatter plot with and without outliers

#Define matrix with variables having outliers
df.numeric.matrix1<-data.matrix(df.numeric)[,c(1:8,10:26,28:29,31,35,39)]

for (x in 1:ncol(df.numeric.matrix1))
{#Print scatter with outliers
  print(ggplot(df.numeric, aes(df.numeric.matrix1[ ,x], df.numeric.matrix1[ , 4])) + geom_point() + 
          geom_smooth(method = "lm")+ggtitle(colnames(df.numeric.matrix1)[x]))
  # Find the 25th and the 75th percentile of the dataset
  rm(subs, subs_rev, up, low, Q, iqr)
  Q<- quantile(df.numeric.matrix1[,x], probs = c(0.25,0.75), na.rm = TRUE)
  # Find the difference of the 75th and 25th percentiles
  iqr <- IQR(df.numeric.matrix1[,x], na.rm = TRUE)
  # Find the cut-off ranges
  up <-  Q[2]+1.5*iqr # Upper Range  
  low <- Q[1]-1.5*iqr # Lower
  
  subs<- subset(df.numeric.matrix1[,x],
                      ((df.numeric.matrix1[,x] > low) & (df.numeric.matrix1[,x] < up)))
  subs_rev<- subset(df.numeric.matrix1[,4],
                          ((df.numeric.matrix1[,x] > low) & (df.numeric.matrix1[,x] < up)))
  #remove cases of empty subset
  if (length(subs) != 0) {
    print(x)
    print(ggplot(data.frame(subs), aes(subs, subs_rev)) + geom_point() + 
            geom_smooth(method = "lm")+ggtitle('eliminated'))
    #plot(eliminated, eliminated_rev)
  }
}

#We can see differences between scatter plots containing and not containing original outliers.
#Outliers in independent variables affect the range of the dependent variable (revenue)
#Prominent changes are visible for:
#Budget, popularity, keyword_cnt, some of the depart_* variables

## 3 Comparing the correlations before and after removing outliers:

for (x in 1:ncol(df.numeric.matrix1))
{
  #Clean data
  rm(subs, subs_rev, up, low, Q, iqr, cor_with, cor_elim)
  #Find correlation with outliers
  cor_with<-cor.test(df.numeric.matrix1[,x],df.numeric.matrix1[,4], method = "spearman",
                     alternative = "two.sided", exact = FALSE)
  
  #Find correlation without outliers:
  # Find the 25th and the 75th percentile of the dataset
  Q<- quantile(df.numeric.matrix1[,x], probs = c(0.25,0.75), na.rm = TRUE)
  # Find the difference of the 75th and 25th percentiles
  iqr <- IQR(df.numeric.matrix1[,x], na.rm = TRUE)
  # Find the cut-off ranges
  up <-  Q[2]+1.5*iqr # Upper Range  
  low <- Q[1]-1.5*iqr # Lower
  
  subs<- subset(df.numeric.matrix1[,x],
                      ((df.numeric.matrix1[,x] > low) & (df.numeric.matrix1[,x] < up)))
  subs_rev<- subset(df.numeric.matrix1[,4],
                          ((df.numeric.matrix1[,x] > low) & (df.numeric.matrix1[,x] < up)))
  
  #remove cases of empty subset
  if (length(subs) != 0) {
    print(x)
    
    r1.jk <- cor_with$estimate  # Correlation between variable and revenue measured with outliers
    n1 <- length(df.numeric.matrix1[ ,x])-sum(is.na(df.numeric.matrix1[ ,x]))    # Size of full group
    
    #Find correlation without outliers
    
    cor_elim<-cor.test(subs, subs_rev, method = "spearman",
                       alternative = "two.sided", exact = FALSE)
    r2.hm <- cor_elim$estimate  # Correlation between variable and revenue measured without outliers
    n2 <- length(subs)-sum(is.na(subs))    # Size of subset group
    
    #Assuming a group after removal of outliers is considered independed by COCOR
    print(colnames(df.numeric.matrix1)[x])
    print(cocor.indep.groups(r1.jk, r2.hm, n1, n2, data.name=c("Full", "Subset"),
                             var.labels=c("Variable", "revenue", "Variable", "revenue")))
    
  }
}

#The decision is not to delete the outliers values
# because the omission of the outliers values - 
#did not materially change the findings.



## 4 generate data set without outliers
#In accordance with the above decision the outliers values were not removed

#3.2 Missing data
## 1 Missing values among variables

hist(df$actor0_prev_revenue, breaks=30, main = 'full actor0_prev_revenue')
plot(density(na.omit(df$actor0_prev_revenue)), main = 'full actor0_prev_revenue')
rm(subs)
subs<- subset(df,(is.na(df$actor0_prev_revenue) == FALSE) & (is.na(df$actor1_prev_revenue) == FALSE))
hist(subs$revenue, breaks=30, main = 'actor0_prev_revenue subset by actor1_prev_revenue')
plot(density(na.omit(subs$revenue)), main = 'actor0_prev_revenue subset by actor1_prev_revenue')

hist(df$actor1_prev_revenue, breaks=30, main = 'full actor1_prev_revenue')
plot(density(na.omit(df$actor1_prev_revenue)), main = 'full actor1_prev_revenue')
rm(eliminated)
subs<- subset(df,(is.na(df$actor1_prev_revenue) == FALSE) & (is.na(df$actor2_prev_revenue) == FALSE))
hist(subs$actor1_prev_revenue, breaks=30, main = 'actor1_prev_revenue subset by actor2_prev_revenue')
plot(density(na.omit(subs$actor1_prev_revenue)), main = 'actor1_prev_revenue subset by actor2_prev_revenue')

## 2 Potential mechanism of missing data

#Sub-setting by leading actors previous revenues causes data to be missing from the other leading actors but from the higher paid actors more (right tail reduced, more for 0 segmented by 1 than for 1 segmented by 2). This means that when revenue data is missing, it is simultaneously missing from all leading actors and the revenue.

hist(df$revenue, breaks=30, main = 'full revenue')
plot(density(na.omit(df$revenue)), main = 'full revenue')
rm(eliminated)
subs<- subset(df,((is.na(df$actor0_prev_revenue) == FALSE) & is.na(df$actor1_prev_revenue) == FALSE) & (is.na(df$actor2_prev_revenue) == FALSE))
hist(subs$revenue, breaks=30, main = 'revenue subset by leading actors revenue')
plot(density(na.omit(subs$revenue)), main = 'revenue subset by leading actors revenue')

#As I wrote in section 2.6 - the variable that has 
#the most missing values is revenue. 
#As well as the variables that examined the historical
#revenue of the main actor and the two sub-actors 
#(actor0_prev_revenue actor1_prev_revenue; actor1_prev_revenue;)
#- these variables were constructed by examining the historical
#revenue(revenue from previous movies based on the current database) 
#for each player. 
#The reliance of these variables on a revenue-variable 
#that has missing values creates the mechanism of the missing values.
## 3 Imputation

#I will use the mice package for the imputation because it appears the variables are missing completely at random.

imputate <- mice(df, method = "cart")
print(imputate$imp$revenue)

#4 Imputing the database

df.matrix.impute<-data.matrix(complete(imputate))
df.matrix.impute[!is.na(df.matrix.impute)] = 0  #There is data available
df.matrix.impute[is.na(df.matrix.impute)] = 1   #NA value

par(mar = c(3, 2, 3, 2))  #bot, left, top, right
par(oma = c(3, 3, 3, 3))
heatmap(df.matrix.impute, scale = "none")

#Sorry, but I did not manage to finish 
#and repeat section 2 for the revised data set

















