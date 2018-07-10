
#Session 1 #####################################################

# Using **R** as a calculator ####

1 + 2

1 + 3^2

log(15) - sqrt(3.4)

pnorm(1.96)

# Variable assignment ####

x <- 2
y <- 3
x^2 - 3*y + 5

x = 2
y = 3
x^2 - 3*y + 5

# Getting help -- in **R**  ####

?mean

??mean

RSiteSearch("mean")


# Getting data into **R** ####

patient.df <- read.csv("Data\\Patient.csv")
patient.df <- read.csv("Data/Patient.csv")

head(patient.df)
tail(patient.df)
names(patient.df)
dim(patient.df)
str(patient.df)

patient.df <- read.csv("Data/Patient.csv", 
                       stringsAsFactors = FALSE)
str(patient.df)


# Different types of data objects in **R**

#character
"hello world"

#numeric
3.14159

#integer
256

#logical
1 == 1
2 <= 0
3 != 2

# Vectors ####

# A character vector contains strings
c("hello", "world")

# A numeric vector contains numbers
c(1, 2, 3, 4, 5, 6)

# We can easily produce sequences using ':'
1:6

# Matrices ####
# Create a matrix with 2 rows
matrix(1:6, nrow = 2)

# Create a matrix with 2 columns
matrix(1:6, ncol = 2)

# Data frames ####
my_characters = c("one", "two", "three")
my_numbers = 1:3
my_logicals = c(TRUE, FALSE, F)

data.frame(my_characters, my_numbers, my_logicals)


#Session 2 #####################################################

# Getting data into **R** ####
patient.df <- read.csv("Data/Patient.csv", 
                       stringsAsFactors = FALSE)
str(patient.df)


# Descriptive statistics ####
mean(Height)

mean(patient.df$Height)

# Calculating averages with missing values ####

mean(patient.df$Height, na.rm = TRUE)


# Table of counts ####
# One-way frequency table

table(patient.df$Sex)

# Total count
total <- sum(table(patient.df$Sex))
total

# Proportions of total
table(patient.df$Sex)/total

Sex.table <- with(patient.df, table(Sex))
Sex.table
total <- sum(Sex.table)
Sex.table/total

# One-way tables `with`######
#Convert to percentages
Sex.pct <- 100*Sex.table/total
Sex.pct

# Round to 1 decimal place ####
round(Sex.pct, 1)

Sex.Race.tab <- with(patient.df, table(Sex, Race))
Sex.Race.tab

#  Two-way frequency tables ####

Sex.Race.tab <- with(patient.df, table(Sex, Race))
Sex.Race.tab


#  Two-way proportion tables ####

# Calculate proportion with respect to 'margin' total
# margin = 1 (row total) or 2 (column total) 
perc.Sex.Race <- prop.table(Sex.Race.tab, margin=2)
perc.Sex.Race

# Tabulate as percentages
round(100*perc.Sex.Race, 1)


# `which` individual does Smoke? #####
index <- which(patient.df$Smoke == 1)
index


# How many are there? #####

# Use length() to count the number of elements in "index".
length(index)

sum(patient.df$Smoke == 1)

sum(patient.df$Smoke == 1, na.rm = TRUE)


# Who are they?####

# Use square brackets to extract IDs corresponding
# to the cases numbers contained in "index"
index <- which(patient.df$Smoke == 1)
patient.df$Patient.ID[index]


# Subsetting #####

# First element only
patient.df$Height[1]

# All but the first element
patient.df$Height[-1]

# Elements 3 through 8
patient.df$Height[3:8]

# Elements 3 and 8
patient.df$Height[c(3,8)]

#First row or record
patient.df[1, ]

#Second column or variable
patient.df[, 2]

#Some rows and columns
patient.df[2:5, 4:7]

#Rows by number, columns by name
patient.df[1:10, c("Race", "Smoke")]


# Missing values #####

sum(is.na(patient.df$Smoke))

table(patient.df$Smoke)

table(patient.df$Smoke, useNA = "always")


# Cleaning up the Smoke variable #####

patient.df$Smoke.group <-
  with(patient.df, ifelse(Smoke == 1, "Yes", "No"))

table(patient.df$Smoke.group)


# Cleaning up the Race variable ####

patient.df$Race.group <-
  with(patient.df,
       ifelse(Race == 1, "Caucasian",
              ifelse(Race == 2, "African", "Other")))

table(patient.df$Race.group)


# Our clean data set #####

head(patient.df)

str(patient.df)

names(patient.df)


# Removing the Smoke and Race variables ####

Smoke.index <- which(names(patient.df) == "Smoke")
Smoke.index

Race.index <- which(names(patient.df) == "Race")
Race.index

names(patient.df[,- c(Smoke.index, Race.index)])


# Removing the `Smoke` and `Race` variables with `%in%` ####

names(patient.df)

names(patient.df) %in% c("Smoke", "Race")

!names(patient.df) %in% c("Smoke", "Race")


names(patient.df[, !names(patient.df) %in% c("Smoke", "Race")])

patient.df <- 
  patient.df[, !names(patient.df) %in% c("Smoke", "Race")]


# Your turn (BMI.group) ######



# Subsetting in calculations ####

with(patient.df, table(Race.group, BMI.group))


exclude.rows <-
  with(patient.df,
       which(Race.group == "Other" | BMI.group == "normal"))


head(patient.df[-exclude.rows, c("Race.group", "BMI.group")])


r.b.tab <- with(patient.df[-exclude.rows,], 
                table(Race.group, BMI.group))
r.b.tab


round(prop.table(r.b.tab)*100, 1)


# Different way ####
r.b.tab1 <- with(patient.df,
                 table(Race.group, BMI.group))
dim(r.b.tab1)

row.names(r.b.tab1)

colnames(r.b.tab1)

colnames(r.b.tab1)

which(row.names(r.b.tab1) == "Other")

which(colnames(r.b.tab1) == "normal")

r.b.tab1[-3, -1]

round(prop.table(r.b.tab1[-3, -1])*100, 1)


# Your turn ##########




#Session 3 #####################################################

load("RData/Session3.RData")

# Working with functions ####
mystder <- function(x){
  mysd <- sd(x, na.rm = TRUE) # Calc std. deviation
  n <- length(x)              # Calc sample size
  mysd/sqrt(n)                # Definition of SEM
}
mystder(patient.df$Height)



# Getting data into R ####

library(readxl)
excel <- read_excel("data.xlsx", sheet = 1)

library(haven)
stata <- read_dta("data.dta")
spss <- read_sav("data.sav")
sas <- read_sas("data.sas7bdat")
sasxport <- read_xpt("data.xpt")


# Install R Packages from Bioconductor #####

source("https://bioconductor.org/biocLite.R")
biocLite()

source("https://bioconductor.org/biocLite.R")
biocLite(c("GenomicFeatures", "AnnotationDbi"))


# Install R Packages from Github #####

install.packages("devtools")

devtools::install_github("kcha193/infoDecompuTE")


# Loading **R** Packages #####

library(dplyr)

stats::filter()


# `factor` #####

class(patient.df$Sex)

patient.df$Sex <- factor(patient.df$Sex)

class(patient.df$Sex)

levels(patient.df$Sex)


patient.df$BMI.group <- factor(patient.df$BMI.group)
levels(patient.df$BMI.group)


patient.df$BMI.group <- factor(patient.df$BMI.group,
                               levels = c("normal", "overweight", "obese"))
levels(patient.df$BMI.group)


# Which other variables should also be factors? ####

str(patient.df)


patient.df$Race.group <- factor(patient.df$Race.group)
patient.df$Smoke.group <- factor(patient.df$Smoke.group)


# Converting numbers to factors ####

test <- factor(c(0,1,2))
test


test 

as.numeric(test)

as.numeric(as.character(test))


# Relational data ####

cholesterol.df <- read.csv("Data/CholesterolNA.csv")
head(cholesterol.df)
names(cholesterol.df)
dim(cholesterol.df)
str(cholesterol.df)


# Combining data frame by columns (`cbind()`) ####

combined.df <- cbind(patient.df, cholesterol.df[,-1])

cholesterol.df <- read.csv("Data/Cholesterol.csv")
head(cholesterol.df)

combined.df <- cbind(patient.df, cholesterol.df)

# Solution ####

sum(table(patient.df$Patient.ID) > 1)
sum(table(cholesterol.df$Patient.ID) > 1)


# `dplyr` **R** package #####

library(dplyr)


x <- data.frame(key = c(1,2,3), val.x = c("x1","x2","x3"))
y <- data.frame(key = c(1,2,4), val.y = c("y1","y2","y4"))

x
y


# left_join()
left_join(x, y, by = "key")


# right_join()
right_join(x, y, by = "key")


# full_join()
full_join(x, y, by = "key")


# inner_join()
inner_join(x, y, by = "key")


# Combining two data frames

combined.df <- left_join(patient.df, cholesterol.df)

combined.df <- left_join(patient.df, cholesterol.df, 
                         by =  "Patient.ID")

names(combined.df)

str(combined.df)

# Renaming variables

names(combined.df)[c(1,11,12,13)] <- 
  c("ID", "Baseline", "PreTrt", "PostTrt")

names(combined.df)


# Session 4 #####################################################


load("RData/Session4.RData")

# Combining two data frames and renaming variables ####

library(dplyr)
combined.df <- left_join(patient.df, cholesterol.df)

names(combined.df)[c(1,11,12,13)] <- 
  c("ID", "Baseline", "PreTrt", "PostTrt")

str(combined.df)


# Categorical Variables

table(combined.df$Age.group) 

table(combined.df$Sex)


# `for` loops to get column summary statistics ####


for (i in c("Age.group", "Sex", "Smoke.group",
            "Race.group", "BMI.group")){
  
  print(i)
  
  print(table(combined.df[,i]))
  
}




# Continous Variables

mean(combined.df$Age)


# `for` loop to get column summary statistics ####

for (i in c("Age", "Height", "Weight", "BMI", 
            "Baseline", "PreTrt", "PostTrt")){
  
  print(i)
  
  print(mean(combined.df[,i]))
  
}


for (i in c("Age", "Height", "Weight", "BMI", 
            "Baseline", "PreTrt", "PostTrt")){
  
  print(i)
  
  print(mean(combined.df[,i], na.rm = TRUE))
  
}



# Easier way (First ID column is removed)


summary(combined.df[,-1])



categorical.df <- combined.df[,c("Age.group", "Sex", 
                                 "Smoke.group", "Race.group", "BMI.group")]

for (i in 1:ncol(categorical.df)){
  print(i)
  print(table(categorical.df[,i]))
}


continous.df <- combined.df[,c("Age", "Height", "Weight", 
                               "BMI", "Baseline", "PreTrt", "PostTrt")]

for (i in 1:ncol(continous.df)){
  print(i)
  print(mean(continous.df[,i], na.rm = TRUE))
}


# Better way: `apply` to get column summary statistics

apply(categorical.df, 2, table)

apply(continous.df, 2, mean, na.rm = TRUE)

apply(continous.df, 2, sd, na.rm = TRUE)


# `apply()` using a self-defined **R** function ####

na.check <- function(someinput){
  test.na <- is.na(someinput) 
  sum(test.na)
}

test1 <- continous.df$BMI[1:10]
test1 

test.na <- is.na(test1)
test.na

sum(test.na)


apply(continous.df, 2, na.check)

apply(continous.df, 2, function(x) sum(is.na(x)))

# A slightly more complicated function

mytab <- function(someinput){
  n <- length(someinput)
  n.missing <- na.check(someinput)
  n.complete <- n - n.missing
  mymean <- round(mean(someinput, na.rm = TRUE), 2)
  mysd <- round(sd(someinput, na.rm = TRUE), 2)
  mystder <- round(mysd/sqrt(n.complete), 2)
  Lower.CI <- round(mymean - 1.96*mystder, 2)
  Upper.CI <- round(mymean + 1.96*mystder, 2)
  c(Complete.obs = n.complete, Missing.obs = n.missing, 
    Mean = mymean, Std.Error = mystder, 
    Lower.CI = Lower.CI, Upper.CI = Upper.CI)
}



apply(continous.df, 2, mytab)  


# More descriptive stats ####

with(combined.df, mean(Baseline[Sex == "Male"], 
                       na.rm = TRUE))
with(combined.df, mean(Baseline[Sex == "Female"], 
                       na.rm = TRUE))


# Better way `tapply()` #####

with(combined.df, tapply(Baseline, Sex, mean, na.rm = TRUE))


with(combined.df, tapply(Baseline, Race.group, mean, na.rm = TRUE))


with(combined.df, tapply(Baseline, Race.group, mytab))


# Data cleaning  ######
str(combined.df[,11:13])


# `tidyr` **R** package 
library(tidyr)


# `gather()` ####
combined.long.df <- gather(combined.df, key = Time, value= Cholesterol,
                           Baseline, PreTrt, PostTrt)

combined.long.df <- 
  gather(combined.df, key = Time,  value = Cholesterol, 
         -ID, -Age, -Age.group, -Sex, -Weight, -Height,
         -Smoke.group, -Race.group, -BMI, -BMI.group)


head(combined.long.df)

str(combined.long.df)


# `spread()` ####

combined.wide.df <- spread(combined.long.df,
                           key = Time,
                           value = Cholesterol)



# `dplyr` **R** package 

patient.df <- read.csv("Data/Patient.csv", stringsAsFactors = FALSE)

step1 <- mutate(patient.df, 
                Sex = as.factor(Sex), 
                Smoke.group = factor(ifelse(Smoke == 1, "Yes", "No")),
                Race.group =  factor(ifelse(Race == 1, "Caucasian",
                                            ifelse(Race == 2, "African", "Other"))), 
                BMI =  (Weight/Height^2)*703)

step2 <- select(step1, -Smoke, -Race)

step3 <- mutate(step2, 
                BMI.group =   factor(ifelse(BMI >= 30, "obese", 
                                            ifelse(BMI >= 25, "overweight",
                                                   "normal")), 
                                     levels = c("normal", "overweight", "obese")))

step4 <- mutate(step3, 
                Age.group =   factor( ifelse(Age<=35, "Under 35", 
                                             ifelse(Age <= 60, "36 to 60", "Over 61")), 
                                      levels = c("Under 35", "36 to 60", "Over 61")))

Cholesterol.df <- read.csv("Data/Cholesterol.csv")

step5 <- left_join(step4, Cholesterol.df)

final.df <- rename(step5, ID = Patient.ID, 
                   Baseline = Day1, 
                   PreTrt = Day5, 
                   PostTrt = Day10)

str(final.df)

# Piping operator `%>%`  ###########

final.df <- patient.df %>%
  mutate(
  Sex = as.factor(Sex),
  Smoke.group = factor(ifelse(Smoke == 1, "Yes", "No")),
  Race.group =  factor(ifelse(
    Race == 1, "Caucasian",
    ifelse(Race == 2, "African", "Other")
  )),
  BMI =  (Weight / Height ^ 2) * 703
) %>%
  select(-Smoke,-Race) %>%
  mutate(BMI.group = factor(
    ifelse(BMI >= 30, "obese",
           ifelse(BMI >= 25, "overweight",
                  "normal")),
    levels = c("normal", "overweight", "obese")
  )) %>%  
  mutate(Age.group =   factor(
    ifelse(Age <= 35, "Under 35",
           ifelse(Age <= 60, "36 to 60", "Over 61")),
    levels = c("Under 35", "36 to 60", "Over 61")
  )) %>%
  left_join(Cholesterol.df) %>% rename(
    ID = Patient.ID,
    Baseline = Day1,
    PreTrt = Day5,
    PostTrt = Day10
  )


final.long.df <- final.df %>% 
  gather(key = Time, value= Cholesterol, 
         Baseline, PreTrt, PostTrt)
 


# `tapply()` #####


with(combined.df, tapply(Baseline, 
                         Race.group, mean, 
                         na.rm = TRUE))


# `group_by()` and `summarise()` ####


combined.df %>% 
  group_by(Race.group) %>% 
  summarise(BaseMean = mean(Baseline, na.rm = TRUE))
 

# `tapply()` #####
with(combined.df, tapply(Baseline, Race.group, mytab))


# `group_by()` and `summarise()` ####

combined.df %>% 
  group_by(Race.group) %>% 
  summarise(Complete.obs = mytab(Baseline)[1],
            Missing.obs = mytab(Baseline)[2],
            Mean = mytab(Baseline)[3],
            Std.Error = mytab(Baseline)[4],
            Lower.CI = mytab(Baseline)[5],
            Upper.CI = mytab(Baseline)[6]
  )

# `group_by()` and `count()`

combined.df %>% 
  group_by(Age.group, Race.group, BMI.group) %>% 
  count() 

# `arrange()`

combined.df %>% 
  group_by(Age.group, Race.group, BMI.group) %>% 
  count() %>% 
  arrange(desc(n))

# `spread()`

combined.df %>% 
  group_by(Age.group, Race.group, BMI.group) %>% 
  count()  %>% 
  spread(key = Age.group, value = n) 

# `filter()`

combined.df %>% 
  group_by(Age.group, Race.group, BMI.group) %>% 
  count()  %>% 
  spread(Age.group, n) %>% 
  filter(!is.na(BMI.group), !is.na(Race.group)) 



