getwd()
# Case Problem 1
# Pelican Stores- Pelican stores is a part of a Nation Clothing. They ran a promotion in
# which they gave discount coupon to customers of National clothing. 
# Objectives: Develop customer porfile and evaluate promotional campaign.

# Reading the data
ps<-read.csv("PelicanStores.csv", header = TRUE, sep = ",")
dim(ps)
str(ps)
head(ps)
# Variable Type
# Qualitative: 1) Type of customer 2) Method of Payment 3) Gender 4) Marital status
# Quantitative: 1) Customer 2) Items 3) Net.sales 4) Age

# UNIVARIATE ANALYSIS
# ANALYSING QUALITATIVE VARIABLES ONE BY ONE
# VARIABLE: Type of customer
library(plyr)
y<-count(ps$Type.of.Customer) # Finding the frequencies & Plotting
y

n<-sum(y$freq) # Calculating relative frequencies
n
relativefrequency<- y$freq/n
relativefrequency
 
percentrelfreq<- relativefrequency*100 # Converting relative frequencies into pecentages.
percentrelfreq

tc<- data.frame(y,relativefrequency,percentrelfreq)# Creaing dataframe of freq, relative & percent.
tc

barplot(tc$freq,names.arg = tc$x, main = "Var 1.Customer Type \n based on purchases", 
        col = c("Red","Blue"), # barplot varible 1
        xlab = "Customer Type", ylab = "Frequency")

pie(tc$freq[order(tc$freq, decreasing = TRUE)], init.angle = 90, # Lets make a pie chart 
    clockwise = TRUE,
    col = c("seashell","cadetblue2"),
    main = "Pie chart of Variable 1 Customer Type")

## FINDINGS for variable 1: Most of the customers who purchased where of Promotional type.
# Looks like the promotion was effective as most customers where promotional type

# VARIABLE: Method of Payment
# Frequencies
y<-count(ps$Method.of.Payment) # Finding the frequencies & Plotting
y

n<-sum(y$freq) # Calculating relative frequencies
n
relativefrequency<- y$freq/n
relativefrequency

percentrelfreq<- relativefrequency*100 # Converting relative frequencies into pecentages.
percentrelfreq

mp<- data.frame(y,relativefrequency,percentrelfreq)# Creaing dataframe of freq, relative & percent.
mp
# Bar & Pie
barplot(mp$freq,names.arg = mp$x, main = "Var 2.Method of Payment \n based on purchases", 
        col = c("beige","blanchedalmond","bisque1","bisque2","bisque3","bisque4"),
                        xlab ="Method of Payment", ylab = "Frequency") # barplot varible 2
        
pie(mp$freq, init.angle = 90,
    clockwise = TRUE,labels = mp$x,
    col = c("seashell","cadetblue2","lightpink","lightcyan","plum1","papayawhip"),
    main = "Pie chart of Method of Payment Employed\n for purchases")
## Findings: 70% of payments were made using the Proprietary card of NAtional Clothing.
## This again indicates that the promotion has done reasonably well. 

#VARIABLE3: Gender
# Frequencies
y<-count(ps$Gender) # Finding the frequencies & Plotting
y

n<-sum(y$freq) # Calculating relative frequencies
n
relativefrequency<- y$freq/n
relativefrequency

percentrelfreq<- relativefrequency*100 # Converting relative frequencies into pecentages.
percentrelfreq

g<- data.frame(y,relativefrequency,percentrelfreq)# Creaing dataframe of freq, relative & percent.
g
# Bar & Pie
barplot(g$freq,names.arg = g$x, main = "Var 3.Gender of Customers \n based on purchases", 
        col = c("gray","darkgray"),
        xlab ="Gender", ylab = "Frequency") # barplot varible 2

pie(g$freq, init.angle = 90,
    clockwise = TRUE,labels = g$x,
    col = c("seashell","cadetblue2"),
    main = "Pie chart of Genderwise purchases")
## Findings 93% overwhelming number of buyers are ladies. 

