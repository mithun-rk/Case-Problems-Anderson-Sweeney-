install.packages("rsconnect")
library(RJSONIO)
library(rsconnect)
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

#VARIABLE4: Marital status
# Frequencies
y<-count(ps$Marital.Status) # Finding the frequencies & Plotting
y

n<-sum(y$freq) # Calculating relative frequencies
n
relativefrequency<- y$freq/n
relativefrequency

percentrelfreq<- relativefrequency*100 # Converting relative frequencies into pecentages.
percentrelfreq

ms<- data.frame(y,relativefrequency,percentrelfreq)# Creaing dataframe of freq, relative & percent.
ms
# Bar & Pie
barplot(ms$freq,names.arg = ms$x, main = "Var 4.Marital Status of Customers \n based on purchases", 
        col = c("gray","darkgray"),
        xlab ="Marital Status", ylab = "Frequency") # barplot varible 2

pie(ms$freq, init.angle = 90,
    clockwise = TRUE,labels = ms$x,
    col = c("seashell","cadetblue2"),
    main = "Pie chart of Marital status of Customers")
## Findings; 84% overwhelming number of our customers are Married. 

# UNIVARIATE ANALYSIS
# ANALYSING QUANTITATIVE VARIABLES ONE BY ONE
# Variable 1. Items 
# Frequencies
summary(ps$Items)
range(ps$Items)
#Lets define some values to create bins
low_value<- 0
High_value<- 18
step_value<- 2
x_breaks<- seq(low_value,High_value,step_value)
x_breaks

x<-cut(ps$Items, breaks = x_breaks)
x
y<- table(x)
y

# Cumulative frequency, relative frequency, Percent frequency
A<-count(x)
A

cumulativefreq<- cumsum(A$freq)
cumulativefreq
n<-sum(A$freq)
n
relativefrequency<- A$freq/n
relativefrequency
percentrelfreq<- relativefrequency*100
percentrelfreq

i<- data.frame(A,cumulativefreq,relativefrequency,percentrelfreq)
i
# Making the Histogram
h<- hist(ps$Items,
         prob = TRUE,
         ylim = c(0,0.3),
         xlim = c(0,20),
         breaks = x_breaks,
         col = "darkgray",
         border = 0,
         main = "Histogram of number of Items\nPurchased by customers")
curve(dnorm(x, mean=mean(ps$Items),sd=sd(ps$Items)), col="red", lwd=3,add = TRUE)

# Dot Plot 
stripchart(ps$Items,method = "stack", at = c(0.2),
           pch = 20, cex = 1, las =1,frame.plot = F, xlim = c(0,20),
           main= "Dot Plot No. of Items purchased")
# Stem & leaf
stem(x= ps$Items)
## Findings: Most customers buy 0-2 Items 

# Variable 2. Net Sales 
# Frequencies
summary(ps$Net.Sales)
range(ps$Net.Sales)
#Lets define some values to create bins
low_value<- 0
High_value<- 300
step_value<- 60
x_breaks<- seq(low_value,High_value,step_value)
x_breaks

x<-cut(ps$Net.Sales, breaks = x_breaks)
x
y<- table(x)
y
# Cumulative frequency, relative frequency, Percent frequency
A<-count(x)
A

cumulativefreq<- cumsum(A$freq)
cumulativefreq
n<-sum(A$freq)
n
relativefrequency<- A$freq/n
relativefrequency
percentrelfreq<- relativefrequency*100
percentrelfreq

ns<- data.frame(A,cumulativefreq,relativefrequency,percentrelfreq)
ns
# Making the Histogram
h<- hist(ps$Net.Sales,
         prob = TRUE,
         ylim = c(0,0.01),
         xlim = c(0,300),
         breaks = x_breaks,
         col = "darkgray",
         border = 0,
         main = "Histogram of Net Sales\nbased on purchases made by customers")
curve(dnorm(x, mean=mean(ps$Net.Sales),sd=sd(ps$Net.Sales)), col="red", lwd=3,add = TRUE)

# Dot Plot 
stripchart(ps$Net.Sales,method = "stack", at = c(0.1),
           pch = 20, cex = 1, las =1,frame.plot = F, xlim = c(0,300),
           main= "Dot plot of Net sales")
# Stem & leaf
stem(x= ps$Net.Sales)
# Findings: Most purchases are in the bucket of 0 to 50$ followed by 50-100$.

# Variable 3. Age
# Frequencies
summary(ps$Age)
range(ps$Age)
#Lets define some values to create bins
low_value<- 20
High_value<- 80
step_value<- 12
x_breaks<- seq(low_value,High_value,step_value)
x_breaks

x<-cut(ps$Age, breaks = x_breaks)
x
y<- table(x)
y
# Cumulative frequency, relative frequency, Percent frequency
A<-count(x)
A

cumulativefreq<- cumsum(A$freq)
cumulativefreq
n<-sum(A$freq)
n
relativefrequency<- A$freq/n
relativefrequency
percentrelfreq<- relativefrequency*100
percentrelfreq

Age<- data.frame(A,cumulativefreq,relativefrequency,percentrelfreq)
Age
# Making the Histogram
h<- hist(ps$Age,
         prob = TRUE,
         ylim = c(0,0.03),
         xlim = c(20,80),
         breaks = x_breaks,
         col = "darkgray",
         border = 0,
         main = "Histogram of Age groups\nbased on purchases made by customers")
curve(dnorm(x, mean=mean(ps$Age),sd=sd(ps$Age)), col="red", lwd=3,add = TRUE)

# Dot Plot 
stripchart(ps$Age,method = "stack", at = c(0.03),
           pch = 20, cex = 1, las =1,frame.plot = F, xlim = c(20,80),
           main= "Dot plot of Age Groups")
# Stem & leaf
stem(x= ps$Age)
# Findings; Age group 32-44 is the most populated group. 

## BIVARIATE ANALYSIS
## Lets analysis items sold with other categorical variables as a Cross tabulation.
## Lets convert the 3 quantitative variables into categorical
# Coverting Items purchased into 5 categories
summary(ps$Items)
range(ps$Items)
#Lets define some values to create bins
low_value<- 0
High_value<- 20
step_value<- 4
x_breaks<- seq(low_value,High_value,step_value)
x_breaks

ps$ic<-cut(ps$Items, breaks = x_breaks,labels = c("0-3","4-7","8-11","12-15","16-20"))
ps$ic
y<- table(ps$ic)
y
# Cross tab- Type of customer | No. of Items purchased
ttab1<-table(ps$Type.of.Customer,ps$ic)
ttab1<-round(prop.table(ttab1,1),2)*100
ttab1
# Cross tab - Method of payment | No. of Items purchased
ttab2<-table(ps$Method.of.Payment,ps$ic)
ttab2<-round(prop.table(ttab2,1),2)*100
ttab2
# Cross tab - Gender | No. of Items purchased
ttab3<-table(ps$Gender,ps$ic)
ttab3<-round(prop.table(ttab3,1),2)*100
ttab3
# Cross tab - Method of payment | No. of Items purchased
ttab4<-table(ps$Marital.Status,ps$ic)
ttab4<-round(prop.table(ttab4,1),2)*100
ttab4

# Coverting Net sales into 5 categories
summary(ps$Net.Sales)
range(ps$Net.Sales)
#Lets define some values to create bins
low_value<- 0
High_value<- 300
step_value<- 60
x_breaks<- seq(low_value,High_value,step_value)
x_breaks

ps$nsc<-cut(ps$Net.Sales, breaks = x_breaks,labels = c("0$-59$","60$-119$","120$-179$","180$-239$","240$-300$"))
ps$nsc
y<- table(ps$nsc)
y
# Cross tab- Type of customer | Net sales 
ttab5<-table(ps$Type.of.Customer,ps$nsc)
ttab5<-round(prop.table(ttab5,1),2)*100
ttab5
# Cross tab - Method of payment | Net sales
ttab6<-table(ps$Method.of.Payment,ps$nsc)
ttab6<-round(prop.table(ttab6,1),2)*100
ttab6
# Cross tab - Gender | Net sales
ttab7<-table(ps$Gender,ps$nsc)
ttab7<-round(prop.table(ttab7,1),2)*100
ttab7
t(ttab7)
# Cross tab - Method of payment | Net sales
ttab8<-table(ps$Marital.Status,ps$nsc)
ttab8<-round(prop.table(ttab8,1),2)*100
ttab8

# Coverting Age into 5 categories
summary(ps$Age)
range(ps$Age)
#Lets define some values to create bins
low_value<- 20
High_value<- 80
step_value<- 10
x_breaks<- seq(low_value,High_value,step_value)
x_breaks

ps$ac<-cut(ps$Age, breaks = x_breaks,labels = c("20-29Yrs","30-39Yrs","40-49Yrs","50-59Yrs","60-69Yrs","70-79Yrs"))
ps$ac
y<- table(ps$ac)
y
# Cross tab- Type of customer | Age
ttab9<-table(ps$Type.of.Customer,ps$ac)
ttab9<-round(prop.table(ttab9,1),2)*100
ttab9
# Cross tab - Method of payment | Age
ttab10<-table(ps$Method.of.Payment,ps$ac)
ttab10<-round(prop.table(ttab10,1),2)*100
ttab10
# Cross tab - Gender | Age
ttab11<-table(ps$Gender,ps$ac)
ttab11<-round(prop.table(ttab11,1),2)*100
ttab11
t(ttab11)
# Cross tab - Method of payment | Age
ttab12<-table(ps$Marital.Status,ps$ac)
ttab12<-round(prop.table(ttab12,1),2)*100
ttab12

# Some Scatter plots 
library(car)
scatterplot(ps$Net.Sales~ps$Age, pch=16, col="darkblue",
            main="Net sales acrosss Age",
            xlab= "Age in Years", ylab="Sales in $")

scatterplot(ps$Net.Sales~ps$Items,pch=16, col="darkblue",
            main="Net sales againts No. of items sold",
            xlab= "Number of Items Purchased", ylab="Sales in $")
