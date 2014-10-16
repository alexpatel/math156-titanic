



# Source of data: http://lib.stat.cmu.edu/S/Harrell/data/descriptions/titanic.html

titanic<-read.csv("titanic3.csv")

head(titanic)

nrow(titanic)
# a sample of 1310 passengers drawn from the population size of 1317
# so it nears the popolation

#------------------------SECTION ONE------------------------------
# To get a feel of the data, let's do some explotory analysis; we'll
# visualize the variables with different plots 



# define our variables
age<-titanic$age
fare<-titanic$fare
embark<-titanic$embarked
parch<-titanic$parch
sibsp<-titanic$sibsp
pclass<-titanic$pclass
sex<-titanic$sex
survive<-titanic$survived


# First let's get rid of the NA entries
length(age)
index=which(!is.na(age)); age<-age[index]; length(age)
summary(age) 

# histogram-ALEX, plz overlay a pdf)
hist(age,20,freq=FALSE,col="red") 
# This looks like a normal distribution except for the bump at age of 0
# and the long tail towards age of 80.

# we have a ship of passengers with quite a wide range of ages
boxplot(age)


# ALEX- this looks not nice. Can you fix this?
barplot(table(pclass),xlab="Passenger Class",ylab="Number of People",main="Distribution of Passengers by Class")
# the ship has slight more 3rd class passengers than 1st and 2nd class


# let's look at the distribution of their fare 

length(fare); index=which(!is.na(fare)); fare<-fare[index]; length(fare)
hist(fare,50,col="blue",freq=FALSE)
# highly skewed with a long tail! In fact:
summary(fare)
# This means most fares are less than 50 Pre-1970 British Pounds 
# but there are seats as expensive as 500 Pounds!



# WHAT INFERENCE CAN WE DRAW?



# normal quantile test: how normal does these variables look?
qqnorm(age)
qqline(age) # age is quite nicely distributed except for the tail

qqnorm(pclass)
qqline(pclass) # not a good fit at all, meaning it's not normal

qqnorm(fare)
qqline(fare) # fare is not close to normal distribution at all


# WHAT INFERENCE CAN WE DRAW?



# ----------------------------SECTION TWO----------------------------
# After some exploration, our main interest lies in 
# the correlation of sex, socio-economic class, and age with survival


#ALEX -can you do the analysis for age and survival? 
# You can refer to my code but first group the age into intervals of 10.



# Let's start with examining sex and survival
counts<-table(titanic$survive,titanic$sex); counts
# ALEX - could you get rid of the awkward extra column of "0"
women<-counts[4]/(counts[3]+counts[4]);women # women's survival rate 
men<-counts[6]/sum(counts[c(5,6)]);men # men's survival rate
women/men # women are almost 4 times more likely to survive than men!


# plot a graph to visualize our statitistics 
barplot(counts, xlab="gender",ylab="number of people",main="survival by sex")
#ALEX - it would be nice to have a numbers of percentages on the bars 


# Let's test two statistics: chi-square and F-ratio of survival by sex
# chi-square test: NUll- sex and survival are independent VS
#   Alternative- women more likely to survive  

chisq.test(counts) # first glance: highly dependent with statistical significance

chisq1 <-function(Obs){
  Expected <- rep(sum(Obs)/length(Obs),length(Obs))
  sum((Obs-Expected)^2/Expected)
}
#Calculate chi square for the observed data
Chi1<-chisq1(counts);Chi1
# calculate its p-value
pchisq(Chi1,2,lower.tail=FALSE) # very small at 1% p-value level


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




# Age and Survival Analysis: (ALEX)




# Now we can turn to class and survival
counts<-table(survive,pclass);counts
chisq.test(counts) # far below 1% pvalue level suggests high dependence for a large chisq

# plot a graph to support our inference
barplot(counts, xlab="gender",ylab="number of people",main="survival by sex")
# it seems first class has more survival than other classes
# could this be due to some chance event?

# testing hypothesis: do rich passengers get saved more often? 

chisq1 <-function(Obs){
  Expected <- rep(sum(Obs)/length(Obs),length(Obs))
  sum((Obs-Expected)^2/Expected)
}

Chi1<-chisq1(counts);Chi1 
#a large chi-square avlue for the observed data
pchisq(Chi1,2,lower.tail=FALSE) # highly significant
# again, a small p-value with a large chisq suggests high dependence

# we can also simulate the results

N = 10^4-1 ; class.result <- numeric(N)
for (i in 1:N) {
  per_class<- sample(pclass)
  class.result[i]<-chisq1(table(per_class,survive))
}
hist(class.result, breaks = "FD",xlim=c(300,600))  
abline(v = Chi1, col = "red") # too far away from the graph
pVal <-(sum (sex.result >= Chi1) +1)/(N+1); pVal;pVal*2 
# we can safely assert that class and survival are not independent
# confirming our visualization that the rich are more likely to survive




