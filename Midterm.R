     ### Who Gets to Surive the Titanic Disaster? ###
            # Chang Liu and Alexander Patel
                    # Math 156
                # October 20, 2014

## Table of Contents

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
#        #  Creativity/Complexity (1)                        #              
#        #  Creativity/Complexity (2)                        #                                                
#        #  Creativity/Complexity (3)                        #                                                  
#        #  Creativity/Complexity (4)                        #         
#        #  Creativity/Complexity (5)                        #                                                 
#        #  Creativity/Complexity (6)                        #                                                 
##############################################################

## The Dataset

#  Our dataset comprises personal and logistical data on the
#  passengers on the Titanic. Our initial questions about
#  this data are:
#      * What are the leading factors associating with survival from the sinking of  
#         the Titanic: cabin, ticket price, sex or age?
#     * Are these relationships, if any, significant? 
#     * Does point of embarkment matter? Does a family get to survive together most often? 
#
#  The data can be found at http://lib.stat.cmu.edu/S/Harrell/data/descriptions/titanic.html. 

setwd("data/")
titanic.csv <- read.csv("titanic.csv"); head(titanic.csv)
ncol(titanic.csv); nrow(titanic.csv)
