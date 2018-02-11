#Case Problem 2 The Motion picture industry.
# The data consist of 100 movies released in the year 2011. We want to analyse the performance
# the variables made availabe include 1) Collections in the opening week. 2) Total Collections
# 3) No. theaters released in 4) No. of weeks running

movies<- read.csv("2011Movies.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
View(movies)
str(movies)
head(movies)
# All the 4 variables are Quantitative. 
# UNIVARIATE ANALYSIS
# ANALYSING QUANTITATIVE VARIABLES ONE BY ONE
# Variable 1. Opening Gross sales
# Frequencies
summary(movies$Opening.Gross.Sales...millions.)
range(movies$Opening.Gross.Sales...millions.)
#Lets define some values to create bins
low_value<- 0
High_value<- 175
step_value<- 25
x_breaks<- seq(low_value,High_value,step_value)
x_breaks

Opensalescat<-cut(movies$Opening.Gross.Sales...millions., breaks = x_breaks,
                  labels = c("0-24$millions","25-49$millions","50-74$millions",
                  "75-99$millions","100-124$millions","125-149$millions",
                  "150-175$millions"))
Opensalescat

y<- table(Opensalescat)
y

# Cumulative frequency, relative frequency, Percent frequency
A<-count(Opensalescat)
A

cumulativefreq<- cumsum(A$freq)
cumulativefreq
n<-sum(A$freq)
n
relativefrequency<- A$freq/n
relativefrequency
percentrelfreq<- relativefrequency*100
percentrelfreq

Openingweek<- data.frame(A,cumulativefreq,relativefrequency,percentrelfreq)
Openingweek
# Making the Histogram
h<- hist(movies$Opening.Gross.Sales...millions.,
         prob = TRUE,
         ylim = c(0,0.035),
         xlim = c(0,180),
         col = "black",
         border = 0,
         main = "Histogram of Collection in $millions\nduring the opening week of release")
curve(dnorm(x, mean=mean(movies$Opening.Gross.Sales...millions.),sd=sd(movies$Opening.Gross.Sales...millions.)), col="red", lwd=3,add = TRUE)

# Dot Plot 
stripchart(movies$Opening.Gross.Sales...millions.,method = "stack", at = c(0.2),
           pch = 20, cex = 1, las =1,frame.plot = F, xlim = c(0,180),
           main= "Dot Plot opeing week collections")
# Stem & leaf
stem(x= movies$Opening.Gross.Sales...millions.)
## Findings: almost 80% movies collect less than 50$million. 

# Variable 2. Total Gross sales
# Frequencies
summary(movies$Total.Gross.Sales...millions.)
range(movies$Total.Gross.Sales...millions.)
#Lets define some values to create bins
low_value<- 0
High_value<- 400
step_value<- 50
x_breaks<- seq(low_value,High_value,step_value)
x_breaks

Tolasalescat<-cut(movies$Total.Gross.Sales...millions., breaks = x_breaks,
                  labels = c("0-49$millions","50-99$millions","100-149$millions",
                             "150-199$millions","200-249$millions","250-299$millions",
                             "300-349$millions","350-400$millions"))
Tolasalescat
y<- table(Tolasalescat)
y

# Cumulative frequency, relative frequency, Percent frequency
A<-count(Tolasalescat)
A

cumulativefreq<- cumsum(A$freq)
cumulativefreq
n<-sum(A$freq)
n
relativefrequency<- A$freq/n
relativefrequency
percentrelfreq<- relativefrequency*100
percentrelfreq

Totalsales<- data.frame(A,cumulativefreq,relativefrequency,percentrelfreq)
Totalsales
# Making the Histogram
h<- hist(movies$Total.Gross.Sales...millions.,
         prob = TRUE,
         ylim = c(0,0.01),
         xlim = c(0,400),
         col = "darkgray",
         border = 0,
         main = "Histogram of Total Collection in $millions")
curve(dnorm(x, mean=mean(movies$Opening.Gross.Sales...millions.),sd=sd(movies$Opening.Gross.Sales...millions.)), col="red", lwd=3,add = TRUE)

# Dot Plot 
stripchart(movies$Total.Gross.Sales...millions.,method = "stack", at = c(0.2),
           pch = 20, cex = 1, las =1,frame.plot = F, xlim = c(0,400),
           main= "Dot Plot Total collections")
# Stem & leaf
stem(x= movies$Total.Gross.Sales...millions.)
## Findings: Most movies close 70% earn less than 100 million.
## Most movies make about 37 make anywhere between 51 to 100 million. 

# Variable 3. Number of Theaters
# Frequencies
summary(movies$Number.of.Theaters)
is.character(movies$Number.of.Theaters)
movies$Number.of.Theaters<- as.numeric(gsub(",","",movies$Number.of.Theaters, fixed = TRUE))

range(movies$Number.of.Theaters)

#Lets define some values to create bins
low_value<- 1000
High_value<- 4500
step_value<- 500
x_breaks<- seq(low_value,High_value,step_value)
x_breaks

No.ofTheaters<-cut(movies$Number.of.Theaters, breaks = x_breaks,
                  labels = c("1000-1499","1500-1999","2000-2499",
                             "2500-2999","3000-3499","3500-3999",
                             "4000-4499"))
No.ofTheaters
y<- table(No.ofTheaters)
y

# Cumulative frequency, relative frequency, Percent frequency
A<-count(No.ofTheaters)
A

cumulativefreq<- cumsum(A$freq)
cumulativefreq
n<-sum(A$freq)
n
relativefrequency<- A$freq/n
relativefrequency
percentrelfreq<- relativefrequency*100
percentrelfreq

Totalno.oftheater<- data.frame(A,cumulativefreq,relativefrequency,percentrelfreq)
Totalno.oftheater
# Making the Histogram
h<- hist(movies$Number.of.Theaters,
         prob = TRUE,
         ylim = c(0,0.001),
         xlim = c(0,4500),
         col = "darkgray",
         border = 0,
         main = "Histogram of Number of Theater movie was released")
curve(dnorm(x, mean=mean(movies$Opening.Gross.Sales...millions.),sd=sd(movies$Opening.Gross.Sales...millions.)), col="red", lwd=3,add = TRUE)

# Dot Plot 
stripchart(movies$Number.of.Theaters,method = "stack", at = c(0.2),
           pch = 20, cex = 1, las =1,frame.plot = F, xlim = c(0,4500),
           main= "Dot Number of theater")
# Stem & leaf
stem(x= movies$Number.of.Theaters)
## Finding: most movies are released in 3000-3499 theaters. 

# Variable 4. Weeks in Release
# Frequencies
summary(movies$Weeks.in.Release)
range(movies$Weeks.in.Release)
#Lets define some values to create bins
low_value<- 5
High_value<- 45
step_value<- 5
x_breaks<- seq(low_value,High_value,step_value)
x_breaks

Tolalweeksinreleas<-cut(movies$Weeks.in.Release, breaks = x_breaks,
                  labels = c("5-9 weeks","10-14 weeks","15-19 weeks",
                             "20-24 weeks","25-29 weeks","30-34 weeks",
                             "35-39 weeks","40-44 weeks"))
Tolalweeksinreleas
y<- table(Tolalweeksinreleas)
y

# Cumulative frequency, relative frequency, Percent frequency
A<-count(Tolalweeksinreleas)
A

cumulativefreq<- cumsum(A$freq)
cumulativefreq
n<-sum(A$freq)
n
relativefrequency<- A$freq/n
relativefrequency
percentrelfreq<- relativefrequency*100
percentrelfreq

Totalweeksinrelease<- data.frame(A,cumulativefreq,relativefrequency,percentrelfreq)
Totalweeksinrelease
# Making the Histogram
h<- hist(movies$Weeks.in.Release,
         prob = TRUE,
         ylim = c(0,0.1),
         xlim = c(5,45),
         col = "darkgray",
         border = 0,
         main = "Histogram of Weeks Running")
curve(dnorm(x, mean=mean(movies$Opening.Gross.Sales...millions.),sd=sd(movies$Opening.Gross.Sales...millions.)), col="red", lwd=3,add = TRUE)

# Dot Plot 
stripchart(movies$Weeks.in.Release,method = "stack", at = c(0.2),
           pch = 20, cex = 1, las =1,frame.plot = F, xlim = c(5,45),
           main= "Dot Plot weeks running")
# Stem & leaf
stem(x= movies$Weeks.in.Release)
#Finding Most movies run for 10-14 weeks 


## QUESTION 2
# Exploring relationship between Total Gross sales & Opening weekend Gross sales. 
scatterplot(movies$Total.Gross.Sales...millions.~movies$Opening.Gross.Sales...millions.,pch=16, col="darkblue",
            main="Scatter plot exploring relationship\n of Total colletction & opening week collections",
            xlab= "Opening weeks Gross sales", ylab="Total Gross Sales")

cor(movies$Total.Gross.Sales...millions.,movies$Opening.Gross.Sales...millions.)
## Finding: mostly linear relationship, a good opening ensures good total collection.

## Question 3
# Exploring relationship between Total Gross sales & Number of Theaters
scatterplot(movies$Total.Gross.Sales...millions.~movies$Number.of.Theaters,pch=16, col="darkblue",
            main="Scatter Plot\n Relationship - Total Collection & No.of Theaters",
            xlab= "Number of Theaters", ylab="Collection in Millions")
cor(movies$Opening.Gross.Sales...millions.,movies$Number.of.Theaters)
## Findings: linear post 3000 theaters

## Question 4
# Exploring relationship between Total Gross sales & Number of weeks in release
scatterplot(movies$Total.Gross.Sales...millions.~movies$Weeks.in.Release,pch=16, col="darkblue",
            main="Scatter Plot\n Relationship- Total collection & No. of weeks in Release",
            xlab= "Number of Weeks running", ylab="Collection in Millions")
cor(movies$Total.Gross.Sales...millions.,movies$Weeks.in.Release)
## Findings; Running more than 20 weeks is counter productive. 


## Question 5
# Descriptive statistics for each of the 4 variable. 
summary(movies$Opening.Gross.Sales...millions.) 
fivenum(movies$Opening.Gross.Sales...millions.) # Min,1st & 3rd quartile, median, Max.
boxplot.stats(movies$Opening.Gross.Sales...millions.) # This function also gives you the outliers. 

library(psych)
describe(movies$Opening.Gross.Sales...millions.)
## Plotting
boxplot(movies$Opening.Gross.Sales...millions.,horizontal = TRUE, col = "brown")
## Findings: Most movies made less equal to 19 million. There are some real outstanding
## performerers, one of them collected as high as 169 million.

boxplot.stats(movies$Total.Gross.Sales...millions.)
describe(movies$Total.Gross.Sales...millions.)
boxplot(movies$Total.Gross.Sales...millions.,horizontal = TRUE, col = "blue")


boxplot.stats(movies$Number.of.Theaters)
describe(movies$Number.of.Theaters)
boxplot(movies$Number.of.Theaters,horizontal = TRUE, col = "blue")


boxplot.stats(movies$Weeks.in.Release)
describe(movies$Weeks.in.Release)
boxplot(movies$Weeks.in.Release,horizontal = TRUE, col = "blue")
