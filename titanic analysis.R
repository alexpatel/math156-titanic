
titanic<-read.csv("titanic2.csv")

head(titanic)

# define our variables
age<-titanic$Age
fare<-titanic$Fare
embark<-titanic$Embarked
parch<-titanic$Parch
sibsp<-titanic$SibSp
Pclass<-titanic$Pclass
sex<-titanic$Sex
survive<-titanic$Survived



# plot some visualizations(I couldn't disregard missing entries)

plot(density(titanic$Age, na.rm = TRUE)) # looks quite normal except the bump and long tail
hist(age,20,freq=F)
boxplot(age)
# overlay a pdf
curve(pnorm(x,30,1),add=TRUE)



plot(density(titanic$Fare, na.rm = TRUE))
#highly skewed! what does this mean?

plot(density(titanic$Parch, na.rm = TRUE))
plot(density(sibsp, na.rm = TRUE))
plot(density(titanic$Pclass, na.rm = TRUE))

?density


# normal quantile test: how normal does these variable look?
qqnorm(age)
qqline(age) # age is quite nicely distributed except for the tail

qqnorm(Pclass)
qqline(Pclass) # not a good fit at all

qqnorm(fare)
qqline(fare) # fare is not close to normal distribution at all


# our first question of interest lies in the correlation of
# sex, socio-economic class, and age with survival

# Let's start with sex!
counts<-table(sex,survive); counts
233/(81+233)
109/(109+468)
women<-counts[3]/(counts[3]+counts[1]);women # women's survival rate 
men<-counts[4]/sum(counts[c(2,4)]);men # men's survival rate
women/men # women are 4 times more likely to survive than men!

# plot a graph to visualize our intuition
barplot(counts, xlab="gender",ylab="number of people",main="survival by sex")

# testing two statistics: chisq and F-ratio of survival by sex
# chi-square test: NUll- women more likely to survive 
#   Alternative- sex and survival are independent

chisq.test(counts) # first glance: highly dependent with statistical significance

chisq1 <-function(Obs){
  Expected <- rep(sum(Obs)/length(Obs),length(Obs))
  sum((Obs-Expected)^2/Expected)
}
#Calculate chi square for the observed data
Chi1<-chisq1(counts);Chi1
# calculate its p-value
pchisq(Chi1,1,lower.tail=FALSE)


# we can also simulate the results

N = 10^4-1 ; sex.result <- numeric(N)
for (i in 1:N) {
  per_sex<- sample(sex)
  sex.result[i]<-chisq1(table(per_sex,survive))
}
hist(sex.result, breaks = "FD")  
abline(v = Chi1, col = "red") 
pVal <-(sum (sex.result >= Chi1) +1)/(N+1); pVal;pVal*2 
# we can safely assert that sex and survival are not independent
# confirming our observation from the graph that women are more likely to survive


# Now turn to age and survival; we will have age groups of 10 years apart
counts<-table(survive,age);counts


hist(counts,200,break="FD",probability=TRUE)
233/(81+233)
109/(109+468)
women<-counts[3]/(counts[3]+counts[1]);women # women's survival rate 
men<-counts[4]/sum(counts[c(2,4)]);men # men's survival rate
women/men # women are 4 times more likely to survive than men!

# plot a graph to visualize our intuition
barplot(counts, xlab="gender",ylab="number of people",main="survival by sex")


# testing hypothesis: 

chisq.test(counts) # highly dependent with statistical significance

chisq1 <-function(Obs){
  Expected <- rep(sum(Obs)/length(Obs),length(Obs))
  sum((Obs-Expected)^2/Expected)
}
#Calculate chi square for the observed data
Chi1<-chisq1(counts);Chi1
# calculate its p-value
pchisq(Chi1,1,lower.tail=FALSE)


# we can also simulate the results

N = 10^4-1 ; sex.result <- numeric(N)
for (i in 1:N) {
  per_sex<- sample(sex)
  sex.result[i]<-chisq1(table(per_sex,survive))
}
hist(sex.result, breaks = "FD")  
abline(v = Chi1, col = "red") 
pVal <-(sum (sex.result >= Chi1) +1)/(N+1); pVal;pVal*2 
# we can safely assert that sex and survival are not independent
# confirming our observation from the graph that women are more likely to survive


# Now turn to class and survival
counts<-table(survive,class);counts
chisq.test(counts) # 1% pvalue level suggests survival are not independent as well

chisq.test(counts) # highly dependent with statistical significance

# plot a graph to visualize our intuition
barplot(counts, xlab="gender",ylab="number of people",main="survival by sex")

# testing hypothesis: do rich people get saved first? 

chisq1 <-function(Obs){
  Expected <- rep(sum(Obs)/length(Obs),length(Obs))
  sum((Obs-Expected)^2/Expected)
}
#Calculate chi square for the observed data
Chi1<-chisq1(counts);Chi1
# calculate its p-value
pchisq(Chi1,2,lower.tail=FALSE) # highly significant


# we can also simulate the results

N = 10^4-1 ; class.result <- numeric(N)
for (i in 1:N) {
  per_class<- sample(class)
  class.result[i]<-chisq1(table(per_class,survive))
}
hist(sex.result, breaks = "FD",xlim=c(50,500))  
abline(v = Chi1, col = "red") # too far from the graph
pVal <-(sum (sex.result >= Chi1) +1)/(N+1); pVal;pVal*2 
# we can safely assert that class and survival are not independent
# confirming our observation from the graph that the rich are more likely to survive




