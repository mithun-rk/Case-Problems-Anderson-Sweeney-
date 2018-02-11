# Case 3 Business schools of Asia-Pacific
# Across the region thousands of Asian show an increasing willingness to temporarilt shelve
# their careers and spend 2 years in pursuit of theoritical business qualification. 
# Lets analyse some charteristics of the leading Asia-Pacific business schools. 

getwd()
# Reading the data
MBA<- read.csv("Asian.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
View(MBA)
dim(MBA)
str(MBA)

# Converting 3 Variables viz 1) Local.tution, 2) Foreign.Tution and 3) Starting salary
# into numeric from character. As they are numeric in nature. 

# Descriptive Statistics
library(psych)
describe(MBA)

# Let look at each one of the variable one at a time. 
# Varialbe 1
summary(MBA$Business.School) # A nominal variable. 

# Variable 2
summary(MBA$Full.Time.Enrollment)
boxplot.stats(MBA$Full.Time.Enrollment)# No outliers 

sd(MBA$Full.Time.Enrollment) # standard deviation
coefofvariaion<- sd(MBA$Full.Time.Enrollment)/mean(MBA$Full.Time.Enrollment)*100
coefofvariaion
# Finding Maimum erollement seen IIM Calcutta. Minimum a Macquire school sydney.
# Mean enrollment 165.2 with standard dev of 140.
# At 85% Coef of variation we see Between schools we see a lot of variationin the in takes. 

# Variable 3
summary(MBA$Students.per.Faculty)
boxplot.stats(MBA$Students.per.Faculty) # No outliers

sd(MBA$Students.per.Faculty) # Standard deviation
sd(MBA$Students.per.Faculty)/mean(MBA$Students.per.Faculty)*100
# Findings, Minimum students to manage per faculty is 2 at Australia National University
# Maximum 19 us at University of Adeliade. 
# we can say average student per faculity is 8.5 with standard dev of 5.
# Resonablly high variation. 

# Variable 4
summary(MBA$Local.Tuition....)# local tution fee is beeing read as text.
# lets convert from text to numeric.
MBA$Local.Tuition....<- as.numeric(gsub(",","",MBA$Local.Tuition...., fixed = TRUE))# Need to replace , by blank for as. numeric to work. 
# Hence gsub is used to replace , by nothing.
# Now lets call summary
summary(MBA$Local.Tuition....)
boxplot.stats(MBA$Local.Tuition....) # No outliers

sd(MBA$Local.Tuition....) # Standard deviation
sd(MBA$Local.Tuition....)/mean(MBA$Local.Tuition....)*100
# Findings: Minimum fee of 1000 is at Jamnalal Bajaj bombay. Maximum 33060 is at Internation 
# university of Japan. 
# we can say Average fee for local students is 12375 with a standard dev of 7778.
# High coefficient of variation. 

# Variable 5
summary(MBA$Foreign.Tuitiion....) # Again foreign fee is read as text.
# lets convert from text to numeric.
MBA$Foreign.Tuitiion....<- as.numeric(gsub(",","",MBA$Foreign.Tuitiion...., fixed = TRUE))# Need to replace , by blank for as. numeric to work. 
# Hence gsub is used to replace , by nothing.
# Now lets call summary
summary(MBA$Foreign.Tuitiion....)
boxplot.stats(MBA$Foreign.Tuitiion....) # No outliers

sd(MBA$Foreign.Tuitiion....) # Standard deviation
sd(MBA$Foreign.Tuitiion....)/mean(MBA$Foreign.Tuitiion....)*100
# Findings: Mininum fee for foreign students is 1000 at Jamnalal bajaj and maximum of 33060
# at National Institute in Japan.

# Varialbe 6
summary(MBA$Age)
boxplot.stats(MBA$Age) # No outliers

sd(MBA$Age) # Standard deviation
# Findings: Minimum age of enrollment is 22 and maximum has been 37.
# Median age is 29 and standard deviation is 3.78 years. 

# Variable 7
summary(MBA$X.Foreign) # Looks like this variable tells % of foreign enrollement.
# We can create artifical variable with actual counts of local and forign students, if required. 
boxplot.stats(MBA$X.Foreign) # No outliers

sd(MBA$X.Foreign) # Standard deviation
# Findings : There are some colleges with no foreign students and some colleges with as high as 90
# % of students who are foreigners. 
# On an average 28 % students for any college are foreigner with a standard deviation of 25% 

#Variable 8
summary(MBA$GMAT) 
# Converting into factor
MBA$GMAT<- as.factor(MBA$GMAT)
summary(MBA$GMAT)
prop.table(table(MBA$GMAT))
# Findings 56% students enrolled took GMAT and 44% did not. 

# Variable 9
summary(MBA$English.Test)
# Coverting into factor
MBA$English.Test<- as.factor(MBA$English.Test)
summary(MBA$English.Test)
prop.table(table(MBA$English.Test))
# Findings: 68% enrolled students have not takes a maths test. 

# Variable 10
summary(MBA$Work.Experience)
# Coverting into factor
MBA$Work.Experience<- as.factor(MBA$Work.Experience)
summary(MBA$Work.Experience)
prop.table(table(MBA$English.Test))
# Findings: 76% enrolled students have some kind of work experience. 

# Variable 11
summary(MBA$Starting.Salary....) # Again foreign fee is read as text.
# lets convert from text to numeric.
MBA$Starting.Salary....<- as.numeric(gsub(",","",MBA$Starting.Salary...., fixed = TRUE))# Need to replace , by blank for as. numeric to work. 
# Hence gsub is used to replace , by nothing.
# Now lets call summary
summary(MBA$Starting.Salary....)
boxplot.stats(MBA$Starting.Salary....) # No outliers

sd(MBA$Starting.Salary....) # Standard deviation
sd(MBA$Starting.Salary....)/mean(MBA$Starting.Salary....)*100
# Findings: The minimum starting salary is 7000 and the max is 87000.
# The median salary is 41400 with a standard deviation of 23459/-

# One noteworth and obvious observation is that coefficient of variation for all the 
# 11 variables is really high. This is kind of intutive cause all are different B schools. 

