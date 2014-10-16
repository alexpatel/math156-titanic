     ### Who Gets to Surive the Titanic Disaster? ###
            # Chang Liu and Alexander Patel
                    # Math 156
                # October 20, 2014

## Table of Contents

##############################################################
#   I    #  Exploratory analysis: Who are on the ship?   
##############################################################
#        #  Basic demographics (sex, age)                   #
#        #  Social-economic status                          #
#  ALEX  #  Summary                                         #
##############################################################
#   II   #  Independence testing: Who get to survive?
##############################################################
#        #  Testing for sex and survival                    #
#  ALEX  #  Testing for age and survival                    # 
#        #  Testing for passenger class and survival        #
#        #  Conclusion                                      #                                                
##############################################################


# A checklist of criteria
##############################################################
#  Line  #      Element       #
##############################################################
#        #  Techical  (1): a data frame                      #
#        #  Techical  (2): two catgorical/logical cols       # 
#        #  Techical  (3): two numeric cols                  #
#        #  Techical  (4): at least 20 rows                  #
#        #  Graphical (1): a barplot                         #
#        #  Graphical (2): a histogram                       #
#  ALEX  #  Graphical (3): a pdg overaid on a histogram      # 
#  ALEX  #  Graphical (4): a contingency table               # 
#        #  Analysis  (4): a permutation test                #
#        #  Analysis  (4): a p-value                         #
#        #  Analysis  (4): analysis of a contingency tables  #
#        #  Analysis  (4): at least 20 rows                  #
#        #  Creativity/Complexity (1)                        #              
#        #  Creativity/Complexity (2)                        #                                                
#        #  Creativity/Complexity (3)                        #                                                  
#        #  Creativity/Complexity (4)                        #         
#        #  Creativity/Complexity (5)                        #                                                 
#        #  Creativity/Complexity (6)                        #                                                 
##############################################################

## The Dataset

#  Our dataset comprises personal and logistical data on the
#  passengers on the Titanic. Our questions about
#  this data are:
#      * What are the leading factors associating with survival from the sinking of  
#         the Titanic: cabin, ticket price, sex or age?
#     * Are these relationships, if any, significant? 
#     * Does a family get to survive together most often? 
#
#  The data can be found at http://lib.stat.cmu.edu/S/Harrell/data/descriptions/titanic.html. 

setwd("data/")
titanic.csv <- read.csv("titanic3.csv"); head(titanic.csv)
ncol(titanic.csv); nrow(titanic.csv)





# Source of data: http://lib.stat.cmu.edu/S/Harrell/data/descriptions/titanic.html

titanic<-read.csv("titanic3.csv")

head(titanic)


nrow(titanic)
# a sample of 1310 passengers' data drawn from the population size of 1317
# so it approximates well the population


#------------------------SECTION ONE------------------------------
# To get a sense of the sample, let's do some explotory analysis
# First we'll visualize the variables with different plots 



# define our variables
age<-titanic$age # a numerical column
fare<-titanic$fare # a numerical column
pclass<-titanic$pclass # a categorical column
sex<-titanic$sex # a categorical column
survive<-titanic$survived # a numerical column with survived=1 or deceased=0


# First let's looks at the distribution of age 
length(age)
index=which(!is.na(age)); age<-age[index]; length(age)
summary(age) 

# ALEX TO DO: plz divide into 3 groups: children, adults, elderly 
# Then do a barplot and comment on the demographics 

# ALEX TO DO: plz overlay a pdf on the following histogram
hist(age,20,freq=FALSE,col="red") 
# This looks like a normal distribution except for the bump at age of 0
# and the long tail towards age of 80.

# we have a ship of passengers with quite a wide range of ages
boxplot(age)

# What is the demographics of gender?
table(sex)


# Then we look at its passenger class - prox for social-economic status

# ALEX- this looks not nice. Can you fix this?
barplot(table(pclass),xlab="Passenger Class",ylab="Number of People",main="Distribution of Passengers by Class")
# the ship has slight more 3rd class passengers than the sum of 1st and 2nd class


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


# WHAT INFERENCE CAN WE DRAW from the normal distribution of age group?


# ALEX after you have analyzed the age group, can you comment on the relationship 
# between age, parents and class? I know for a fact from wikipedia that most children are on the 
# 3rd class cabin and most of them are with less than 1 parents accompanying them
# Then we infer that many of them are accompanied by relatives, nannies or alike

# Analyze a contingency table if you can. 

# This should complicate our conclusion that while children and 1st class get saved
# yet children are mostly in the 3rd class. 



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


# Conclusion:
# Women, children (TBD?), and the first class are most likely to survive than others 


