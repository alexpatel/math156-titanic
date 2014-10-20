## Who Gets to Survive the Titanic Disaster?
## Chang Liu and Alexander Patel
## Math 156
## October 20, 2014

## The R Environment
# For graphs and charts, we will use the 'ggplot2' plotting library
#install.packages("ggplot2")
library("ggplot2")
#install.packages("scales")
library("scales")

## The Dataset
#  Our dataset comprises personal and logistical data on the
#  passengers on the Titanic. 
#  The data can be found at:
#   http://lib.stat.cmu.edu/S/Harrell/data/descriptions/titanic.html. 
titanic <- read.csv("titanic3.csv"); str(titanic)
# The data is a sample of 1310 passengers from the population size of 1317
#  so it approximates well the population
# There are 14 columns, but we will be looking at 5:
#  * 3 categorial/logical: passenger class, sex, survival
#  * 2 numerical: age, fare
titanic <- data.frame(titanic$survived, titanic$age, titanic$sex, titanic$fare, titanic$pclass)
names(titanic) <- c("survived", "age", "sex", "fare", "pclass"); head(titanic)

## PART I: AGE
# The data set contains age data on ~80% of the passengers (1046 / 1310)
age.nna <- titanic[!is.na(titanic$age),] # strip data of rows with age=NULL
summary(age.nna$age) 
# Notice the min/max: 2 months versus 80 years!
# Let's overlay a PDG on a histogram of age
ggplot(age.nna, aes(x=age)) + 
    ggtitle("Passenger Age") + 
    xlab("Age") + 
    ylab("Density") + 
    geom_histogram(aes(y=..density..), binwidth=1)+
    geom_density(alpha=.5, fill="#FFFFFF")
# We can see the discrepancies at extreme ages with a normal quantile plot
# The data is noticeably right-skewed, although there is clear dip in the number
#  of pre-teen and teenage passengers (for good reason, even in 1912)
qqnorm(age.nna$age, main="Passenger Age: Normal Quantile Plot")
qqline(age.nna$age)

# Let's do a boxplot of age vs. gender and add in as a reference the 1912 U.S. Life Expectancy
#  from: http://demog.berkeley.edu/~andrew/1918/figure2.html
# Turns out, there are more "old" passengers than meets the eye!
p <- qplot(x=age.nna$sex, y=age.nna$age, data=age.nna, 
           geom=c("boxplot", "jitter"), main="Age vs. Gender", xlab="Gender", 
           ylab="Age") + coord_flip()
p + geom_hline(yintercept = 51.5, color="blue", label="Life Exp. (M)") + 
    geom_hline(yintercept = 55.9, colour="red", label="Life Exp. (F)")

# Now, let's look at on particular category of the age group--children--and test 
#  whether the second part of "women and children first" (a code of conduct most famously associated with the Titanic) holds. 
# There are 154 children in the data set.
length(which(titanic$age < 18))
# Let's add a new categorical variable for child/adult.
titanic$age_group <- "adult"
titanic$age_group[titanic$age < 18] <- "child"
# 52% of children and only 36% of adults survived.
# But the child population is much smaller than the adult one. 
age_group.survived <- table(titanic$age_group, titanic$survived); age_group.survived
age_group.survived.prop <- prop.table(age_group.survived, 1); age_group.survived.prop
# Is the difference in survival rates significant? Let's see with a permutation test.
adults.survived <- age_group.survived.prop[3]
children.survived <- age_group.survived.prop[4]
observed <-  children.survived / adults.survived; observed # children are 1.45x more likely to survive 
N=10^4-1 ; result<-numeric(N)  
for (i in 1:N) {
    index <- sample(nrow(titanic), size=154, replace=FALSE)
    child.sample <- length(which(titanic$survived[index] == 1)) / length(index)
    adult.sample <- length(which(titanic$survived[-index] == 1)) / (nrow(titanic) - length(index))
    result[i] <- child.sample / adult.sample 
}
qplot(result, binwidth=.05) + 
    geom_vline(xintercept = observed, color="red", label="Observed") +
    ggtitle("Permutation Test: Child Survival / Adult Survival") + 
    xlab("Ratio") + 
    ylab("Count")
pvalue = (sum (result >= observed) + 1)/(N+1); 2*pvalue 
# A near-zero p-value indicates that the evidence supports the hypothesis that 
#  children are more likely to have survived

## PART II: Gender
# extract passengers with non-null sex/survival data
index<-which((!is.na(titanic$sex) )&(!is.na(titanic$survived))); 
sex<-data.frame(titanic$survived[index],titanic$sex[index])
names(sex) <- c("survive ", "sex"); head(sex)
# 466 females and 843 males have survival data
nrow(subset(sex, sex == "female")); nrow(subset(sex, sex == "male"))
# a contingency table for gender
counts<-table(sex)[,c(0,2,3)]; counts
# Women's survival rate 
women.survived<-counts[2]/(counts[1]+counts[2]);women.survived
# Men's survival rate
men.survived <-counts[4]/(counts[3]+counts[4]);men.survived
# Women are almost 4 times more likely to survive than men!
observed <- women.survived / men.survived; observed 
# Visualizing gender vs. survival
barplot(counts, ylim=c(0, 1000), xlab="Gender",ylab="Count",main="Survival by Sex")
text(.71, 520, paste(as.character(round(women.survived, 3) * 100),"% Survived"))
text(1.9, 890, paste(as.character(round(men.survived, 3 ) * 100),"% Survived"))
legend("bottomright", fill=c("black", "grey"), legend=c("Perished", "Survived"))
# Is the survival ratio of women over men statistically significant? 
# Let's do two tests. First, a permutation test for the ratio of women and men who survived
N=10^3-1 ; result<-numeric(N)  
for (i in 1:N) {
    per.sex<-sample(titanic$sex)
    counts2<-table(titanic$survived,per.sex);counts2<-counts2[,c(0,2,3)]
    women<-counts2[2]/(counts2[1]+counts2[2]) # women's survival rate 
    men<-counts2[4]/(counts2[3]+counts2[4]) # men's survival rate
    result[i]<-women/men  
}
qplot(result, binwidth=.05) + 
    geom_vline(xintercept = observed, color="red", label="Observed") + 
    xlim(0, 4) +
    ggtitle("Permutation Test: Female Survival / Male Survival") + 
    xlab("Ratio") + 
    ylab("Count")
pValue = (sum (result >= observed) + 1)/(N+1); 2*pValue #double for 2-sided test
# Far below 1% pvalue level: the observed ratio is extremely unlikely to occur by chance! 
# We have reason now to belive that women are saved first

# Now let's perform a chi-square test to further verify our conviction: 
# NUll hypothesis: sex and survival are independent VS
# Alternative hypothesis - women more likely to survive  
chisq.test(counts) 
# first glance: two variables highly dependent with statistical significance
chisq1 <-function(Obs){
    Expected <- rep(sum(Obs)/length(Obs),length(Obs))
    sum((Obs-Expected)^2/Expected)
}
#Calculate chi square for the observed data
Chi1<-chisq1(counts);Chi1
# calculate its p-value 
pchisq(Chi1,1,lower.tail=FALSE) 
# extremely small at 1% p-value level
# strongly suggesting the relationship is not indepedent 

# BONUS point: advantage of simulation over classical chisquare test

# Although in agreement on the inference, however, the large discrepany 
# in values between the built-in test and our calculation makes us nervous 
# e.g.p-values: 2.2e-16 for built-in; 9.304668e-131 for calculation
# The same issue appears in the analysis of class and survival!
# But we have no easy way to find out why! 
# Also the degree of power in the values (esp. -131?!) are just way too high
# Let's see if simulation can remedy this issue. 

# simulate the results
N = 10^4-1 ; sex.result <- numeric(N)
for (i in 1:N) {
    per_sex<- sample(titanic$sex);
    counts3<-table(titanic$survived,per_sex)
    counts3<-counts3[,c(0,2,3)]
    sex.result[i]<-chisq1(counts3)
}
qplot(sex.result, binwidth=1) + 
    geom_vline(xintercept = Chi1, color="red", label="Observed") + 
    xlim(100, 600) +
    ggtitle("Simulation: Sex Survival") + 
    xlab("Chi-squared") + 
    ylab("Count")
pVal <-(sum (sex.result >= Chi1) +1)/(N+1); pVal;pVal*2 

# Not only the pvalue from simulation clearly is more reasonable
# but also it confirms the same inference without giving 
# the unknown discrepany and extremeness in values in the classical method.

## PART III: Passenger Class and Fare
# In our long script, we also looked at passenger class and ticket fare. The evidence 
# supports the hypothesis that passengers in lower-numbered passenger classes are
# more likely to have survived.the sinking of the Titanic.

# Conclusion:
# Women, children, and the first class are more likely to survive than others 