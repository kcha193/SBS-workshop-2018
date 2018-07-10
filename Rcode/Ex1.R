
#Session 1 #####################################################


# 1. Using **R** as a calculator #######
1+4

2^3 + 4/sqrt(34)

log(30)

abs(-2)



# 2. Reading data into **R** #######


lake.df <- read.csv("Data/Lake.csv", 
                    stringsAsFactors = FALSE)

head(lake.df)
tail(lake.df)
dim(lake.df)
str(lake.df)



#Session 2 #####################################################


# 1. Descriptive statistics ####

mean(lake.df$pH, na.rm = TRUE)
mean(lake.df$Chlorophyll, na.rm = TRUE)

sd(lake.df$pH, na.rm = TRUE)
sd(lake.df$Chlorophyll, na.rm = TRUE)


summary(lake.df$pH)


table(lake.df$Calcium)


round(prop.table(table(lake.df$Calcium)) * 100, 2)


# 2. Subsetting #########

lake.df$pH[1]


lake.df$pH[53]
#or:
lake.df$pH[nrow(lake.df)]


lake.df$pH[c(1, 53)]


lake.df[3, ]


lake.df[, "pH"]


with(lake.df, mean(pH[Calcium == "Low"], na.rm = TRUE))


with(lake.df, mean(pH[Calcium == "Low" & Chlorophyll < 10], na.rm = TRUE))


# 3. `ifelse()` function ####

lake.df$pHtype <-
  with(lake.df, 
       ifelse(pH > 7, "basic", 
              ifelse(pH < 7, "acidic", "natural")))


with(lake.df, table(Calcium, pHtype))

save(lake.df, file = "RData/Ex3.RData")


#Session 3 #####################################################

load("RData/Ex3.RData")



# 1. Installing an **R** package #### 

std.error(lake.df$pH)

library(plotrix)

std.error(lake.df$pH)

with(lake.df, sd(pH, na.rm = TRUE)/sqrt(length(pH)))


# 2. Write your own function

mystder <- function(x){
  mysd <- sd(x, na.rm = TRUE)
  n <- length(x)
  round(mysd/sqrt(n), 2)
}


mystder <- function(x){
  mysd <- sd(x, na.rm = TRUE)
  n <- sum(!is.na(x))
  round(mysd/sqrt(n), 2)
}


mystder(lake.df$pH)



# 3. `factor`

lake.df$pHtype <- factor(lake.df$pHtype, levels = c( "acidic", "natural", "basic"))

lake.df$Calcium <- factor(lake.df$Calcium, levels = c("Low", "Medium", "High"))

with(lake.df, table(Calcium, pHtype))

# 4. Combine two data frames

mercury.df <- read.csv("Data/Mercury.csv", 
                       stringsAsFactors = FALSE)

library(dplyr)
joined.df <- left_join(lake.df, mercury.df, by = "ID")

dim(joined.df)

names(joined.df)

str(joined.df)

# 5. Challenge  ####

sem_ci <- function(input){
  avg <- mean(input, na.rm = TRUE)
  mysd <- sd(input, na.rm = TRUE)
  n <- sum(!is.na(input))
  sem <- mysd / sqrt(n)
  upperCI <- round(avg + 1.96 * sem, 2)
  lowerCI <- round(avg - 1.96 * sem, 2)
  c(lowerCI, upperCI)
}

sem_ci(joined.df$pH)


save(joined.df, file = "RData/Ex4.RData")


# Session 4 #####################################################

load("RData/Ex4.RData")


# 1. `for` loop ########### 

for (i in c("pHtype", "Calcium")){
  print(i)
  print(table(joined.df[,i]))
}

for (i in c("pH", "Chlorophyll", "Day1", "Day2", "Day3")){
  print(i)
  print(mean(joined.df[,i], na.rm = TRUE))
  print(sd(joined.df[,i], na.rm = TRUE))
}

# 2. `apply()` and `tapply()` functions ######


apply(joined.df[, c("pHtype", "Calcium")], 2, table)


apply(joined.df[, c("pH", "Chlorophyll", "Day1", "Day2", "Day3")], 2,
      mean, na.rm=TRUE)

apply(joined.df[, c("pH", "Chlorophyll", "Day1", "Day2", "Day3")], 2,
      sd, na.rm=TRUE)

round(apply(joined.df[, c("pH", "Chlorophyll", "Day1", "Day2", "Day3")], 2, 
            mean, na.rm = TRUE), 2)

round(apply(joined.df[, c("pH", "Chlorophyll", "Day1", "Day2", "Day3")], 2, 
            sd, na.rm = TRUE), 2)


apply(joined.df[, -c(1,2)], 2, function(x) sum(!is.na(x)))



mytab1 <- function(someinput){
  n <- length(someinput)
  n.missing <- length(which(is.na(someinput))) # <- !!!
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


apply(joined.df[, c("pH", "Chlorophyll", "Day1", "Day2", "Day3")], 2, mytab1)


with(joined.df, tapply(pH, Calcium, mytab1))


library(tidyr)
joined.long.df <- gather(joined.df, "Day1", "Day2", "Day3", key = Time, value= Mercury)


round(with(joined.long.df, 
           tapply(Mercury, list(pHtype, Calcium, Time), mean, na.rm = TRUE)), 2)


# 3. Challenge  ######

lake.df <- read.csv("Data/Lake.csv", 
                    stringsAsFactors = FALSE)
mercury.df <- read.csv("Data/Mercury.csv", 
                       stringsAsFactors = FALSE)

joined.df <- lake.df %>%
  mutate(
    pHtype = factor(
      ifelse(pH > 7, "basic", ifelse(pH < 7, "acidic", "natural")),
      levels = c("acidic", "natural", "basic")
    ),
    Calcium = factor(lake.df$Calcium, levels = c("Low", "Medium", "High"))
  ) %>%
  left_join(mercury.df, by = "ID")


joined.long.df <- joined.df %>%
  gather("Day1", "Day2", "Day3", key = Time, value= Mercury)


joined.long.df %>% 
  group_by(pHtype, Calcium, Time) %>% 
  summarise(Mean = round(mean(Mercury, na.rm = TRUE)))

save(joined.df, joined.long.df, file = "RData/Ex5.RData")


