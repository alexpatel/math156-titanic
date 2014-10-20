     ### Who Gets to Surive the Titanic Disaster? ###
            # Chang Liu and Alexander Patel
                    # Math 156
                # October 20, 2014

## Table of Contents

##############################################################
#   I    #  Exploratory analysis: Who are on the ship?   
##############################################################
#  ALEX  #  Basic demographics (sex, age)                   #
#  ALEX  #  Social-economic status                          #
#  ALEX  #  Summary                                         #
##############################################################
#   II   #  Independence testing: Who get to survive?
##############################################################
#        #  Testing for sex and survival                    #
#  ALEX  #  Testing for age and survival                    # 
#        #  Testing for passenger class and survival        #
#  AlEX  #  Conclusion                                      #                                                
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
#        #  Analysis  (4): Simulation vs Classical           #
#        #  Creativity/Complexity: large data set            #             
#        #  Creativity/Complexity: many columns              #                                                
#        #  Creativity/Complexity: novel statistics          #                                                  
#        #  Creativity/Complexity: software engineering      # ALEX?        
#        #  Creativity/Complexity: use of quantile           # not sure if it qualifies                                                  
#        #  Creativity/Complexity: beautiful display         # ALEX, use ggplots? 
#        #  Creativity/Complexity: Simulation over Classical # Alex, any insights from age analysis?     
#        #  Creativity/Complexity  backup?                   #     
##############################################################

## The R Environment
# For graphs and charts, we will use the 'ggplot2' plotting library
#install.packages("ggplot2")
#install.packages("scales")
library("ggplot2")
library("scales")

## The Dataset
#  Our dataset comprises personal and logistical data on the
#  passengers on the Titanic. 
#  The data can be found at:
#   http://lib.stat.cmu.edu/S/Harrell/data/descriptions/titanic.html. 
titanic <- read.csv("titanic3.csv"); head(titanic, n=10)
# The data is a sample of 1310 passengers from the population size of 1317
#  so it approximates well the population
nrow(titanic)  

## THE COLUMNS
# define our variables
age     <- titanic$age      # age, a numerical column
pclass  <- titanic$pclass   # cabin class, a categorical column
sex     <- titanic$sex      # gender, a categorical column
survive <- titanic$survived # survived/not survived (0, 1), a numerical column 

#------------------------SECTION ONE------------------------------
# To get a sense of the sample, let's do some explotory analysis
# Let's visualize the variables with different plots.

## Age ## 
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
# We can see the discrepancies at extreme ages with a QQ-plot
qqnorm(age.nna$age)
qqline(age.nna$age)

# Now, let's look at the children on board
age.children <- titanic[which(titanic$age < 18),]; nrow(age.children) # 154 children
summary(age.children$pclass) 
# Split children by passenger class
age.children.pclass <- split(age.children, age.children$pclass)
# 15 kids in 1st class, 33 in 2nd; 106 in 3rd
counts <- c(nrow(age.children.pclass$"1"), nrow(age.children.pclass$"2"), nrow(age.children.pclass$"3"))
counts
# Plot number of children in each passenger class
ggplot(data=age.children, aes(x=pclass, y=age)) +
    ggtitle("Children Passengers: Passenger Class") +
    xlab("Passenger Class") +
    ylab("Count") +
    geom_bar(stat="identity") +

## Gender ##
index<-which((!is.na(titanic$sex) )&(!is.na(titanic$survived))); 
sex<-data.frame(titanic$survived[index],titanic$sex[index])
names(sex) <- c("survive ", "sex"); head(sex)
# 466 females and 843 males have survival data
nrow(subset(sex, sex == "female")); nrow(subset(sex, sex == "male"))
# a contingency table for gender
counts<-table(sex)[,c(0,2,3)]; counts
# Women's survival rate 
wome.survivedn<-counts[2]/(counts[1]+counts[2]);women 
# Men's survival rate
men.survived <-counts[4]/(counts[3]+counts[4]);men 
# Women are almost 4 times more likely to survive than men!
observed <- women.survived / men.survived; observed 
# Visualizing gender vs. survival
barplot(counts, ylim=c(0, 1000), xlab="Gender",ylab="Count",main="Survival by Sex")
text(.71, 510, paste(as.character(round(women, 3) * 100),"% Survived"))
text(1.9, 890, paste(as.character(round(men, 3 ) * 100),"% Survived"))
legend("right", fill=c("black", "grey"), legend=c("Perished", "Survived"))

# Then we look at passenger class (a proxy for social-economic status)
# ALEX- this looks not nice. Can you fix this? surely ggplots will do much better
barplot(table(titanic$pclass),xlab="Passenger Class",ylab="Number of People",main="Distribution of Passengers by Class")
ggplot(data=titanic, aes(x=pclass)
ggplot(data=age.children, aes(x=pclass, y=age)) +
    ggtitle("Children Passengers: Passenger Class") +
    xlab("Passenger Class") +
    ylab("Count") +
    geom_bar(stat="identity") +
    theme_bw()
# the ship has slight more 3rd class passengers than the sum of 1st and 2nd class
qqnorm(pclass)
qqline(pclass) # not a good fit at all, meaning it's not normal

# Fare
fare <- titanic$fare
# we have fare data for most of the passengers in the data set
length(fare); index<-which(!is.na(fare)); fare<-fare[index]; length(fare)
hist(fare,50,col="blue",freq=FALSE)
# highly skewed with a long tail
summary(fare)
# Most fares are less than 34 Pre-1970 British Pounds 
# but there are seats as expensive as 512 Pounds and as cheap as free
qqnorm(fare)
qqline(fare) # fare is not close to normal distribution at all

# ----------------------------SECTION TWO----------------------------
# After some exploration, our main interest lies in 
# the correlation of sex, socio-economic class, and age with survival


# Let's start with examining sex and survival

#Is the survival ratio of women over men statistically significant? 
# Let's do two tests: 
# A permutation test for the ratio of women and men who survived
N=10^4-1 ; result<-numeric(N)  
for (i in 1:N) {
  per.sex<-sample(sex2)
  counts2<-table(survive2,per.sex);counts2<-counts2[,c(0,2,3)]
  women<-counts2[2]/(counts2[1]+counts2[2]) # women's survival rate 
  men<-counts2[4]/(counts2[3]+counts2[4]) # men's survival rate
  result[i]<-women/men  
}
hist(result, breaks = "FD", prob = TRUE)
hist(result, breaks = "FD", prob = TRUE,xlim=c(0,4))
abline(v = observed, col = "red") # too far from the distribution
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
  per_sex<- sample(sex2);
  counts3<-table(survive2,per_sex)
  counts3<-counts3[,c(0,2,3)]
  sex.result[i]<-chisq1(counts3)
}

hist(sex.result, freq=FALSE,breaks = "FD",xlim=c(100,600))  
abline(v = Chi1, col = "red") # way too far
pVal <-(sum (sex.result >= Chi1) +1)/(N+1); pVal;pVal*2 

# Not only the pvalue from simulation clearly is more reasonable
# but also it confirms the same inference without giving 
# the unknown discrepany and extremeness in values in the classical method.


# Now we can safely conclude that sex and survival are not independent
# confirming our observation from the graph that women are more likely to survive




# Age and Survival Analysis: (ALEX)







# Now we can turn to class and survival


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
# A small p-value with a large chisq value suggests high dependence
# bewteen survivalibity and class

# Again, the unknown discrepany and extremeness in values appear in the classical method. 

# But we can also simulate the results to verify the same thing for comparison.


N = 10^4-1 ; class.result <- numeric(N)
for (i in 1:N) {
  per_class<- sample(pclass)
  class.result[i]<-chisq1(table(per_class,survive))
}
hist(class.result, breaks = "FD",freq=FALSE)  
hist(class.result, breaks = "FD",freq=FALSE, xlim=c(300,600))  
abline(v = Chi1, col = "red") # too far away from the graph
pVal <-(sum (class.result >= Chi1) +1)/(N+1); pVal;pVal*2 # below 1% level
# Simulation clearly verifies the same conclusion without giving the mystery.
# It works practically better.

# Now we can safely assert that class and survival are not independent,
# confirming our visualization that the rich are more likely to survive


# Conclusion:
# Women, children, and the first class are more likely to survive than others 


