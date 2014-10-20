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
#        #  Graphical (3): a pdg overaid on a histogram      # 
#        #  Graphical (4): a contingency table               # 
#        #  Analysis  (4): a permutation test                #
#        #  Analysis  (4): a p-value                         #
#        #  Analysis  (4): analysis of a contingency tables  #
#        #  Analysis  (4): at least 20 rows                  #
#        #  Creativity/Complexity: large data set            #             
#        #  Creativity/Complexity: many columns              #                                                
#        #  Creativity/Complexity: novel statistics(ratio)   #                                                  
#        #  Creativity/Complexity: software engineering      # ALEX?        
#        #  Creativity/Complexity: use of quantile           #                                                  
#        #  Creativity/Complexity: beautiful display         # ALEX(ggplots)? 
#        #  Creativity/Complexity: more                     #     
#        #  Creativity/Complexity (6)                        #     
##############################################################

## THE DATASET
#  Our dataset comprises personal and logistical data on the
#  passengers on the Titanic. 
#  The data can be found at:
#   http://lib.stat.cmu.edu/S/Harrell/data/descriptions/titanic.html. 
setwd("data/")
titanic <- read.csv("titanic3.csv"); head(titanic, n=10)
# The data is a sample of 1310 passengers from the population size of 1317
#  so it approximates well the population
nrow(titanic)  

## THE COLUMNS
# define our variables
age     <- titanic$age      # age, a numerical column
fare    <- titanic$fare     # ticket fare, a numerical column
pclass  <- titanic$pclass   # cabin class, a categorical column
sex     <- titanic$sex      # gender, a categorical column
survive <- titanic$survived # survived/not survived (0, 1), a numerical column 

#------------------------SECTION ONE------------------------------
# To get a sense of the sample, let's do some explotory analysis
# Let's visualize the variables with different plots.
# For graphs and charts, we will use the 'ggplot2' plotting library
#install.packages("ggplot2")
#install.packages("scales")
library("ggplot2")
library("scales")
dev.off()

## AGE 
# The data set contains age data on ~80% of the passengers (1046 / 1310)
length(age); length(which(!is.na(age))) 
age.nna <- titanic[!is.na(age),] # strip data of rows with age=NULL
summary(age.nna$age) 
# Notice the min/max: 2 months versus 80 years!
# Let's do a boxplot of age vs. gender
p <- qplot(x=age.nna$sex, y=age.nna$age, data=age.nna, 
      geom=c("boxplot", "jitter"), main="Age vs. Gender", xlab="Gender", 
      ylab="Age") + coord_flip()
# Add in the 1912 U.S. Life Expectancy
#  From: http://demog.berkeley.edu/~andrew/1918/figure2.html
p + geom_hline(yintercept = 51.5, color="blue", label="Life Exp. (M)") + 
    geom_hline(yintercept = 55.9, colour="red", label="Life Exp. (F)")
# Overlay a PDG on a histogram of age
ggplot(age.nna, aes(x=age)) + 
    ggtitle("Passenger Age") + 
    xlab("Age") + 
    ylab("Density") + 
    geom_histogram(aes(y=..density..), binwidth=1)+
    geom_density(alpha=.5, fill="#FFFFFF")


# Then we look at its passenger class - prox for social-economic status

# ALEX- this looks not nice. Can you fix this? surely ggplots will do much better
barplot(table(pclass),xlab="Passenger Class",ylab="Number of People",main="Distribution of Passengers by Class")
# the ship has slight more 3rd class passengers than the sum of 1st and 2nd class


# let's look at the distribution of their fare 

length(fare); index=which(!is.na(fare)); fare<-fare[index]; length(fare)
hist(fare,50,col="blue",freq=FALSE)
# highly skewed with a long tail! In fact:
summary(fare)
# Most fares are less than 50 Pre-1970 British Pounds 
# but there are seats as expensive as 500 Pounds!



# normal quantile test: how comparable are these variables to normal distr.?
qqnorm(age)
qqline(age) # unsatifactory fit

qqnorm(pclass)
qqline(pclass) # not a good fit at all, meaning it's not normal

qqnorm(fare)
qqline(fare) # fare is not close to normal distribution at all

# there is no indication that we can apply normal distribtion to them  





# ALEX after you have analyzed the age group, can you comment on the relationship 
# between age, parents and class? 

# for example, look at the distribution of children by pclass, by the number of parents, and by both

#I know for a fact from wikipedia that most children are on the 
# 3rd class cabin and most of them are with less than 1 parents accompanying them
# Then we infer that many of them are accompanied by relatives, nannies or alike

# This should complicate our conclusion that while children and 1st class get saved first
# children are mostly in the 3rd class, suggesting these kids get saved less often. 



# ----------------------------SECTION TWO----------------------------
# After some exploration, our main interest lies in 
# the correlation of sex, socio-economic class, and age with survival

# Let's start with examining sex and survival
counts<-table(survive2,sex2); counts<-counts[,c(0,2,3)]; counts
# ALEX - could you get rid of the awkward extra column of "0"
women<-counts[2]/(counts[1]+counts[2]);women # women's survival rate 
men<-counts[4]/(counts[3]+counts[4]);men # men's survival rate
observed<-women/men;observed # women are almost 4 times more likely to survive than men!


# plot a graph to visualize our statitistics 
barplot(counts, xlab="gender",ylab="number of people",main="survival by sex")
#ALEX - it would be nice to have a numbers of percentages on the bars 




#Is this survival ratio statistically significant? Let's do two tests: 
# A permutation test for the ratio of women and men who survived
N=10^4-1 ; result<-numeric(N)  
for (i in 1:N) {
  per.sex<-sample(sex2)
  counts<-table(survive2,per.sex);counts<-counts[,c(0,2,3)]; counts
  women<-counts[2]/(counts[1]+counts[2]);women # women's survival rate 
  men<-counts[4]/(counts[3]+counts[4]);men # men's survival rate
  result[i]<-women/men  
}
hist(result, breaks = "FD", prob = TRUE)
hist(result, breaks = "FD", prob = TRUE,xlim=c(0,4))
abline(v = observed, col = "red") # too far from the distribution
pValue = (sum (result >= observed) + 1)/(N+1); 2*pValue #double for 2-sided test
# Far below 1% pvalue level: the observed ratio is extremely unlikely to occur by chance! 
# We have reason now to belive that women are saved first



# Now let's performa a chi-square test to further support our conviction: 
# NUll hypothesis: sex and survival are independent VS
# Alternative hypothesis - women more likely to survive  

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

# clear out the empty values!
counts<-table(survive,pclass);counts
chisq.test(counts) # far below 1% pvalue level suggests high dependence for a large chisq

# plot a graph to support our inference
barplot(counts, xlab="passenger class",ylab="number of people",main="survival by class")
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
table(per_class,survive)

table(pclass, age)

hist(class.result, breaks = "FD",xlim=c(300,600),freq=FALSE)  
abline(v = Chi1, col = "red") # too far away from the graph
pVal <-(sum (sex.result >= Chi1) +1)/(N+1); pVal;pVal*2 
# we can safely assert that class and survival are not independent
# confirming our visualization that the rich are more likely to survive


# Conclusion:
# Women, children (TBD?), and the first class are most likely to survive than others 


