
#R scripts for each Session 

#k.chang@auckland.ac.nz

load("Rdata/Session5.Rdata")
ls()

# Session 5 ####

# plot()  ####
plot(combined.df)

#Box plots ####
with(combined.long.df, plot(Time, Cholesterol))

class(combined.long.df$Time)

combined.long.df$Time <- factor(combined.long.df$Time)

class(combined.long.df$Time)

with(combined.long.df, plot(Time, Cholesterol))

combined.long.df$Time <- factor(combined.long.df$Time,
                                levels = c("Baseline", "PreTrt", "PostTrt"))

with(combined.long.df, plot(Time, Cholesterol))

with(combined.long.df, plot(Cholesterol ~ Time))

#Box plots ####
with(combined.long.df, plot(Sex, Cholesterol))

with(combined.long.df,
     plot(Sex, Cholesterol, xlab = "Gender",
          ylab = "Cholesterol level (mg/100ml)"))


with(combined.long.df,
     plot(Sex, Cholesterol, xlab = "Gender",
          ylab = "Cholesterol level (mg/100ml)",
          cex.lab = 1.5))


with(combined.long.df,
     plot(Sex, Cholesterol, xlab = "Gender",
          ylab = "Cholesterol level (mg/100ml)",
          cex.lab = 1.5,
          main = "Cholesterol level for male and female"))


# `boxplot()` works as well ####

boxplot(Cholesterol~Sex, data = combined.long.df,
        xlab = "Gender",
        ylab = "Cholesterol level (mg/100ml)",
        cex.lab = 1.5, 
        main = "Cholesterol level for male and female")

#Bar plots ####
with(combined.df, table(Smoke.group, Age.group))


smoke.age <- with(combined.df, 100 * 
                    prop.table(table(Smoke.group, Age.group), 
                               margin =  2))

round(smoke.age, 2)


barplot(smoke.age, xlab = "Age group by years",
        ylab = "Percentage")

barplot(smoke.age, beside = TRUE,
        xlab = "Age group by years",
        ylab = "Percentage")


barplot(smoke.age, beside = TRUE,
        xlab = "Age group by years",
        ylab = "Percentage")
abline(h = 0)

barplot(smoke.age, beside = TRUE,
        xlab = "Age group by years",
        ylab = "Percentage",
        ylim = c(0, 100),
        space = c(0.2, 1.5))
abline(h = 0)

barplot(smoke.age, beside = TRUE,
        xlab = "Age group by years",
        ylab = "Percentage",
        ylim = c(0, 100),
        space = c(0.2, 1.5),
        density = c(0, 100))
abline(h = 0)

barplot(smoke.age, beside = TRUE, 
        xlab = "Age group by years", 
        ylab = "Percentage", 
        ylim = c(0, 100),
        space = c(0.2, 1.5), 
        density = c(0, 10), 
        col = "black")
abline(h = 0)

barplot(smoke.age, beside = TRUE,
        xlab = "Age group by years", 
        ylab = "Percentage", 
        ylim = c(0, 100),
        space = c(0.2, 1.5), 
        col = c("blue", "green"))
abline(h = 0)

barplot(smoke.age, beside = TRUE,
        xlab = "Age group by years", 
        ylab = "Percentage", 
        ylim = c(0, 100),
        space = c(0.2, 1.5), 
        legend.text = TRUE)
abline(h = 0)

barplot(smoke.age, beside = TRUE,
        xlab = "Age group by years",
        ylab = "Percentage",
        ylim = c(0, 100),
        space = c(0.2, 1.5),
        legend.text = TRUE,
        args.legend = list(bty = "n", cex = 1.3))
abline(h = 0)


#Histgrams #####

hist(combined.long.df$Cholesterol)

hist(combined.long.df$Cholesterol, breaks = 5)

hist(combined.long.df$Cholesterol, breaks = 50)

hist(combined.long.df$Cholesterol, main = NULL,
     xlab = "Cholesterol level (mg/100ml)")


# Scatter plots #####

with(combined.long.df, plot(Age, Cholesterol))


with(combined.long.df, plot(Age, Cholesterol,
                            col = c("black", "red")[Sex],
                            pch = c(19, 24)[Smoke.group]))

legend("topright", pch = c(19, 19, 24, 24), 
       col = c("black", "red", "black", "red"),
       legend = c("Non-Smokers:Female", "Non-Smokers:Male",
                  "Smokers:Female", "Smokers:Male"))

with(combined.long.df, plot(Age, Cholesterol,
                            col = c("black", "red")[Sex],
                            pch = c(19, 24)[Smoke.group]))

legend("topleft", pch = c(19, 19, 24, 24), 
       col = c("black", "red", "black", "red"),
       legend = c("Non-Smokers:Female", "Non-Smokers:Male",
                  "Smokers:Female", "Smokers:Male"))

table(combined.long.df$Smoke.group, combined.long.df$Sex)

# Graphical Parameters #####

?par
op <- par(mfrow = c(2, 2))       
with(combined.long.df, boxplot(Cholesterol~Age.group,
                               xlab = "Age Group", ylab = "Total score"))
barplot(smoke.age, beside = TRUE, ylab = "Percentage",
        xlab = "Age group in years", ylim = c(0, 100),
        space = c(0.2, 1.5), legend.text = TRUE)
abline(h = 0)
hist(combined.long.df$Age)
with(combined.long.df, plot(Age, Cholesterol))
## At end of plotting, reset to previous settings:
par(op)


# Extra: pairs()  #####
pairs(combined.long.df[, c("Weight", "Height", "BMI", "Cholesterol")])

plot(combined.long.df[, c("Weight", "Height", "BMI", "Cholesterol")])

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(na.omit(combined.long.df[1:1000,
                       c("Weight", "Height", "BMI", "Cholesterol")]), 
      lower.panel = panel.smooth, upper.panel = panel.cor)


### Session 6 #####

library(ggplot2)

ggplot(data = combined.long.df,
       mapping = aes(x = Age, y = Cholesterol))

g <- ggplot(data = combined.long.df,
            mapping = aes(x = Age, y = Cholesterol))

g + geom_point()

ggplot(data = combined.long.df,
       mapping = aes(x = Age, y = Cholesterol)) + geom_point()

ggplot(data = combined.long.df) +
  geom_point(mapping = aes(x = Age, y = Cholesterol))

ggplot() +
  geom_point(data = combined.long.df,
             mapping = aes(x = Age, y = Cholesterol))

g <- ggplot(data = combined.long.df,
            mapping = aes(x = Age, y = Cholesterol))

g + geom_point()

g + geom_point(aes(colour = Sex, shape = Smoke.group))

(g <- g + geom_point(aes(colour = Sex, shape = Smoke.group))+
    labs(title = "Cholesterol level versus Age",
         x = "Age", y = "Cholesterol level (mg/100ml)"))

g + theme(plot.title = element_text(size=8, hjust = 0.5),
          axis.title = element_text(size=8),
          legend.title = element_text(size=8),
          legend.text = element_text(size=8) )

g <- ggplot(
  data = na.omit(combined.long.df[,c("Age", "Cholesterol",
                                     "Sex", "Smoke.group")]),
  mapping = aes(x = Age, y = Cholesterol,
                color = Sex, shape = Smoke.group)) +
  geom_point() +
  labs(title ="Total score versus Age",
       x = "Age", y = "Total score")

myTheme <- theme_bw() +
  theme(plot.title = element_text(size=8, hjust = 0.5),
        axis.title=element_text(size=8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8) )

g + myTheme

g + geom_smooth() + myTheme

#Box plot ####

ggplot(na.omit(combined.long.df[,c("Age.group",
                                   "Cholesterol")]),
       aes(x = Age.group, y = Cholesterol)) +
  geom_boxplot()  +
  labs(title = "Cholesterol level versus Age",
       x = "Age Group", y = "Cholesterol level (mg/100ml)") + 
  myTheme


ggplot(na.omit(combined.long.df[,c("Age.group", "Cholesterol",
                                   "BMI.group")]),
       aes(x = Age.group, y = Cholesterol, fill = BMI.group)) +
  geom_boxplot() +
  labs(title = "Cholesterol level versus Age and BMI",
       x = "Age Group", y = "Cholesterol level (mg/100ml)") +
  myTheme


g <- ggplot(na.omit(combined.long.df[,c("Age.group",
                                        "Cholesterol", "Race.group")]),
            aes(x = Age.group, y = Cholesterol)) +
  geom_boxplot()  + labs(title = "Total Score versus Age",
                         x = "Age Group", y = "Total score")

g + facet_wrap(~Race.group) + myTheme 


g + facet_wrap(~Race.group) + myTheme + 
  theme(axis.text.x = 
          element_text(angle=30, vjust=1,  hjust = 1))



# Histogram ####

ggplot(combined.long.df, aes(x = Age)) +
  geom_histogram() + myTheme


ggplot(combined.long.df, aes(x = Age)) +
  geom_histogram(binwidth = 10)+
  labs(x = "Age") + myTheme



ggplot(combined.long.df, aes(x = Age)) +
  geom_histogram(binwidth = 1)+
  labs(x = "Age")  + myTheme


ggplot(combined.df, aes(x = Age.group)) + geom_bar() + 
  myTheme


ggplot(combined.long.df, aes(x = Cholesterol)) +
  geom_density()+
  labs(x = "Cholesterol level (mg/100ml)") + myTheme

ggplot(combined.long.df, aes(x = Cholesterol, fill = Age.group)) +
  geom_density(alpha = .4)+
  labs(x = "Cholesterol level (mg/100ml)") + myTheme


# Bar chart ####
 
ggplot(combined.df, aes(x = Age.group)) + geom_bar() + 
  myTheme

ggplot(combined.df, aes(x = Age.group, y = ..prop.., group = "x")) + 
  geom_bar() + 
  myTheme


ggplot(na.omit(combined.df[,c("Age.group", "Smoke.group")]),
       aes(x = Age.group , fill = Smoke.group)) +
  geom_bar() + myTheme


g <- ggplot(na.omit(combined.df[,c("Age.group", "Smoke.group")]),
            aes(x = Age.group, y = ..prop.., group = Smoke.group,
                fill = Smoke.group))
g + geom_bar() + myTheme

g + geom_bar(position = "dodge") +
  labs( x = "Age group in years", y = "Percentage")+
  myTheme


with(combined.df, prop.table(table(Age.group, Smoke.group), 2))


library(dplyr)

age.smoke.tab <- combined.df %>% 
  filter(!is.na(Smoke.group))%>% 
  group_by(Age.group, Smoke.group) %>% 
  count() 

age.smoke.tab

age.tab <- combined.df %>% filter(!is.na(Smoke.group)) %>%
  group_by(Age.group) %>% count() %>% rename(Total = n)

age.tab


final.tab <- age.smoke.tab %>% 
  left_join(age.tab) %>% 
  mutate(Prop = n/Total)
final.tab

ggplot(final.tab, aes(x = Age.group, y = Prop, 
                      fill = Smoke.group)) +
  geom_col()+
  labs( x = "Age group in years", y = "Percentage")+
  myTheme


age.smoke.tab %>% 
  left_join(age.tab) %>% 
  mutate(Prop = n/Total) %>% 
  ggplot(aes(x = Age.group, y = Prop, 
             fill = Smoke.group)) +
  geom_col()+
  labs( x = "Age group in years", 
        y = "Percentage")+
  myTheme

# Plot means in context ####

combined.long.df %>%
  group_by(Age.group) %>%
  summarise(Mean = mean(Cholesterol, na.rm = TRUE))

combined.long.df %>%
  group_by(Age.group) %>%
  summarise(Mean = mean(Cholesterol, na.rm = TRUE),
            SD = sd(Cholesterol, na.rm = TRUE),
            N = sum(!is.na(Cholesterol)),
            SE = SD/sqrt(N),
            CI.upr = Mean + 1.96 * SE,
            CI.lwr = Mean - 1.96 * SE)

age.tab <-
  combined.long.df %>%
  group_by(Age.group) %>%
  summarise(Mean = mean(Cholesterol, na.rm = TRUE),
            SD = sd(Cholesterol, na.rm = TRUE),
            N = sum(!is.na(Cholesterol)),
            SE = SD/sqrt(N),
            CI.upr = Mean + 1.96 * SE,
            CI.lwr = Mean - 1.96 * SE)

ggplot(age.tab, aes(x = Age.group, y = Mean)) + 
  geom_point() +
  geom_errorbar(aes(ymax = CI.upr, ymin = CI.lwr),
                width = 0.1)+
  xlab("Age Group")+
  ylab("Cholesterol level (mg/100ml)") +
  myTheme


GA.tab <-
  combined.long.df %>%
  group_by(Age.group, Sex) %>%
  summarise(Mean = mean(Cholesterol, na.rm = TRUE),
            SD = sd(Cholesterol, na.rm = TRUE),
            N = sum(!is.na(Cholesterol)),
            SE = SD/sqrt(N),
            CI.upr = Mean + 1.96 * SE,
            CI.lwr = Mean - 1.96 * SE)
GA.tab

g <- ggplot(GA.tab, aes(x = Age.group, y = Mean,
                        color = Sex)) +
  xlab("Age Group")+
  ylab("Cholesterol level (mg/100ml)")

g + geom_point() +
  geom_errorbar(aes(ymax = CI.upr, ymin = CI.lwr),
                width = 0.1) +
  myTheme

dodge <- position_dodge(width=0.2)

g + geom_point(position = dodge) +
  geom_errorbar(aes(ymax = CI.upr, ymin = CI.lwr),
                width = 0.1, position = dodge) +
  myTheme


dodge <- position_dodge(width=0.2)

g + geom_point(position = dodge) +
  geom_errorbar(aes(ymax = CI.upr, ymin = CI.lwr),
                width = 0.1, position = dodge) +
  coord_flip() +
  myTheme


g + geom_point(position = dodge) +
  geom_errorbar(aes(ymax = CI.upr, ymin = CI.lwr),
                width = 0.1, position = dodge) +
  geom_path(aes(x = as.numeric(Age.group)),
            position = dodge) +
  myTheme

#Save a ggplot ####

ggsave("myplot.pdf")
ggsave("myplot.png")
ggsave("myplot.pdf", width = 4, height = 4)
ggsave("myplot.pdf", width = 20, height = 20, units = "cm")


### Session 7 ####


#T-test ####

ggplot(combined.long.df, aes(Sex, Cholesterol)) +
  geom_boxplot() +
  theme_bw()

ggplot(combined.long.df, aes(Sex, log(Cholesterol))) +
  geom_boxplot() +
  labs(y = "Log cholesterol") +
  theme_bw()


t.test(Cholesterol ~ Sex, data = combined.long.df)


#One-way ANOVA #####

ggplot(combined.long.df, aes(Age.group, Cholesterol)) +
  geom_boxplot() +
  theme_bw()

tryaov = aov(Cholesterol ~ Age.group, data = combined.long.df)

summary(tryaov)


model.tables(tryaov, "means")

TukeyHSD(tryaov)

comp <- TukeyHSD(tryaov)
comp$Age.group


pp <- data.frame(comp$Age.group)
pp$Age.group <- factor(rownames(comp$Age.group))

library(ggplot2)
ggplot(pp, aes(x = Age.group, y = diff)) + geom_point() + 
  geom_errorbar(aes(ymax = upr, ymin = lwr), width = 0.25) + 
  theme_bw() +
  labs(x = "Age group", y = "Difference",
       title = "Tukey HSD intervals for each age group comparision") +
  theme(text = element_text(size = 14))



# Two-way ANOVA in **R** #####

ggplot(combined.long.df, aes(Age.group, Cholesterol)) +
  geom_boxplot() +
  facet_wrap(~Sex) +
  theme_bw()

combined.long.df$Sex <- factor(combined.long.df$Sex)

try2way <-  aov(Cholesterol ~ Sex * Age.group, combined.long.df)


summary(try2way)

model.tables(try2way, "means")

model.tables(try2way, "means")$table$'Sex:Age.group'

TukeyHSD(try2way)

TukeyHSD(try2way)$`Sex:Age.group`

try3way <-  aov(Cholesterol ~ Sex * Age.group * Time, combined.long.df)


summary(try3way)


# Test of independence  ####

smoke.age.tab <- with(combined.df, 
                      table(Smoke.group, Age.group))
smoke.age.tab


chisq.test(smoke.age.tab)


mytest <- 1:4
chisq.test(mytest)


fisher.test(smoke.age.tab)



# Simple linear regression

with(combined.long.df, plot(Age, Cholesterol))

hist(combined.long.df$Cholesterol)

trylm <- lm(Cholesterol ~ Age, data = combined.long.df)

qqnorm(resid(trylm))
qqline(resid(trylm), col = "red")

plot(predict(trylm), residuals(trylm))
abline(h = 0, col = 2, lwd = 2)

trylmLog <- lm(log(Cholesterol) ~ Age, data = combined.long.df)
qqnorm(resid(trylmLog))
qqline(resid(trylmLog), col = "red")

plot(predict(trylmLog), residuals(trylmLog))
abline(h = 0, col = 2, lwd = 2)

summary(trylm)

summary(trylmLog)

with(combined.long.df, plot(Age, log(Cholesterol)))
abline(trylmLog, col = 2, lwd = 2)


tryquad <- lm(log(Cholesterol) ~ Age + I(Age^2), data = combined.long.df)

summary(tryquad)



### Session 8 ####


# Logistic regression

# class of Smoke.group?
class(combined.df$Smoke.group)

# Convert Smoke.group to a variable of type 'numeric'
# Assigned to a new variable Smoke.group1 
combined.df$Smoke.group1 <- 
  ifelse(combined.df$Smoke.group == "Yes", 1, 0)

# Numeric values of Smoke.group1?
class(combined.df$Smoke.group1)



try.glm <-  glm(Smoke.group1~Age,family = binomial, data = combined.df)

summary(try.glm)

lp <- predict(try.glm, data.frame(Age = 70))
lp

exp(lp)


predict(try.glm, data.frame(Age = 70), type = "response")


predict(try.glm, data.frame(Age = 70),
        type = "response", se.fit = TRUE)


try.glm2 <-  glm(Smoke.group1 ~ Age.group, family = binomial, data = combined.df)
                   
anova(try.glm2, test = "Chisq")

summary(try.glm2)$coef

Age.refac <- factor(as.character(combined.df$Age.group),
                    levels = c("Over 61", "36 to 60", "Under 35"))

try.glm3 <- glm(Smoke.group1 ~ Age.refac,
                family = binomial,
                data = combined.df)


summary(try.glm3)













