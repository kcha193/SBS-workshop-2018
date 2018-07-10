
# Session 5#########


load("Rdata/Ex5.Rdata")

#Q1
with(joined.long.df, 
     plot(as.factor(Calcium), Mercury,
    xlab = "Calcium level", cex.lab = 1.5,
    ylab = "Mercury concentration",
    main = "Mercury contaminations in Florida lakes"))


with(joined.long.df, plot(as.factor(Calcium), Mercury,
                          xlab = "Calcium level", cex.lab = 1.5,
                          ylab = "Mercury concentration",
                          main = "Mercury contaminations in Florida lakes"))


joined.long.df$Calcium <- 
  factor(joined.long.df$Calcium, 
         level = c("Low", "Medium", "High"))

with(joined.long.df, plot(Calcium, Mercury,
        xlab = "Calcium level", cex.lab = 1.5,
        ylab = "Mercury concentration",
        main = "Mercury contaminations in Florida lakes"))


#Q2
with(joined.long.df, plot(pH, Mercury, 
          col = c(1,2,4)[Calcium], 
          pch = c(20, 22,3)[Calcium], 
          xlab = "pH value", 
          ylab = "Mercury concentration"))

legend("topright", pch = c(20, 22, 3), 
       col = c(1, 2, 4),
       cex = 1,
       legend = c("Low", "Medium", "High"))


#Q3

trt.mean <- with(joined.long.df, tapply(Mercury,
                  list(pHtype, Calcium), mean))


barplot(trt.mean, beside = TRUE, space = c(0.1, 1),
        legend.text= c("Acidity", "Neutral", "Alkalinity"),
        ylab = "Mean mercury concentration", 
        xlab = "Calcium level",
        args.legend = list(bty = "n", cex = 1.5))


# Session 6 #########

#Q1

joined.long.df %>% filter(!is.na(Calcium)) %>% 
  ggplot( aes(Calcium, Mercury)) + 
  geom_boxplot()+
  labs(title ="Mercury contaminations in Florida lakes",  
       x = "Calcium level", y = "Mercury concentration") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5))


#Q2

joined.long.df %>% filter(!is.na(Calcium) & !is.na(pH)) %>% 
  ggplot( aes(x = pH, y = Mercury, 
              col = Calcium, shape = Calcium)) + 
  geom_point() +
  labs(title ="Mercury contaminations in Florida lakes",  
       x = "pH value", y = "Mercury concentration") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5))


#Q3


joined.long.df %>% filter(!is.na(Calcium) &
                            !is.na(pHtype)) %>% 
  group_by(Calcium, pHtype) %>% 
  summarise(Mean = mean(Mercury)) %>% 
  ggplot(aes(x = Calcium, y = Mean, 
             fill = pHtype)) + 
  geom_col(position = "dodge")  +
  labs(x = "Calcium level",
       y = "Mean Mercury concentration")


#Q4

g.m <- with(joined.long.df, 
            tapply(Mercury, 
                   Calcium, mean, na.rm = TRUE))
g.sd <- with(joined.long.df, 
             tapply(Mercury, 
                    Calcium, sd, na.rm = TRUE))
g.n <- with(joined.long.df, 
            tapply(Mercury, 
                   Calcium, 
                   function(x)sum(!is.na(x))))
g.stder <- g.sd/sqrt(g.n)

G.df <- data.frame(
  Calcium = factor(names(g.m), 
    levels = c("Low", "Medium", "High")),
  Mean = g.m,
  Upper = g.m + 1.96 * g.stder,
  Lower = g.m - 1.96 * g.stder
)


G.df <- 
  joined.long.df %>% 
  filter(!is.na(Calcium)) %>% 
  group_by(Calcium) %>%
  summarise(Mean = mean(Mercury, na.rm = TRUE),
            SD = sd(Mercury, na.rm = TRUE),
            N = sum(!is.na(Mercury)),
            SE = SD/sqrt(N),
            CI.upr = Mean + 1.96 * SE,
            CI.lwr = Mean - 1.96 * SE)



ggplot(G.df, aes(x = Calcium, y = Mean)) + 
  geom_point() +
  geom_errorbar(aes(ymax = CI.upr, ymin = CI.lwr),
                width = 0.1)  +
  labs(x = "Calcium level", y = "Mean Mercury concentration")+
  theme_bw()





ga.m <- with(joined.long.df, tapply(Mercury, 
   list(Calcium, Time), mean, na.rm = TRUE))

ga.sd <- with(joined.long.df, tapply(Mercury, 
   list(Calcium, Time), sd, na.rm = TRUE))

ga.n <- with(joined.long.df, tapply(Mercury, 
   list(Calcium, Time), function(x)sum(!is.na(x))))

ga.stder <- ga.sd/sqrt(ga.n)
ga.upper <-ga.m + 1.96*ga.stder
ga.lower <-ga.m - 1.96*ga.stder

GA.df = data.frame(
  Time = factor(rep(colnames(ga.m), 3), 
                levels = colnames(ga.m)),
  Calcium = rep(rownames(ga.m), c(3, 3, 3)),
  Mean = c(ga.m[1, ], ga.m[2, ], ga.m[3, ]),
  Upper = c(ga.upper[1, ], ga.upper[2, ], 
            ga.upper[3, ]),
  Lower = c(ga.lower[1, ], ga.lower[2, ],
            ga.lower[3, ])
)

dodge <- position_dodge(width=0.2)

ggplot(GA.df, aes(x = Time, y = Mean,
                  color = Calcium)) +
  xlab("Age Group")+
  ylab("Mean total Nerdy scores") +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower),
                width = 0.1, position = dodge) +
  geom_path(aes(x = as.numeric(Time)), 
            position = dodge)  +
  labs(x = "Time", y = "Mean Mercury concentration")+
  theme_bw()



joined.long.df %>% 
  group_by(Calcium, Time) %>% 
  summarise(Mean = mean(Mercury, na.rm = TRUE),
            SD = sd(Mercury, na.rm = TRUE ), 
            N = sum(!is.na(Mercury))) %>% 
  mutate(Upper = Mean  + 1.96*SD/sqrt(N), 
         Lower = Mean  + 1.96*SD/sqrt(N)) %>% 
  filter(!is.na(Calcium)) %>% 
ggplot( aes(x = Time, y = Mean,
                  color = Calcium)) +
  xlab("Age Group")+
  ylab("Mean total Nerdy scores") +
  geom_point(position = dodge) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower),
                width = 0.1, position = dodge) +
  geom_path(aes(x = as.numeric(Time)), 
            position = dodge)  +
  labs(x = "Time", y = "Mean Mercury concentration")+
  theme_bw()



#Q5

joined.long.df %>% filter(!is.na(Calcium) & 
                            !is.na(pHtype)) %>% 
  group_by(Calcium, pHtype) %>% 
  summarise(Mean = mean(Mercury)) %>% 
  ggplot(aes(x = pHtype, y = Calcium)) + 
  geom_tile(aes(fill = Mean)) + 
  scale_fill_gradient(low = "red",
                      high = "green") +
  theme_bw()  +
  labs(y = "Calcium level", x = "pH level")


#Session 7 ##############


#Q1
joined.long.df$pHtypeAcidic <- 
  with(joined.long.df, ifelse(pH < 7, 
                              "acidity",
                              "Others"))

with(joined.long.df,
     t.test(Mercury~pHtypeAcidic,
            var.equal = TRUE))



with(joined.long.df,
     t.test(Mercury~pHtypeAcidic, 
            var.equal = FALSE))


acid.mer <- with(joined.long.df, 
                 Mercury[pHtype == "acidity"])

not.acid.mer <- with(joined.long.df,
                     Mercury[pHtype != "acidity"])

t.test(acid.mer, not.acid.mer)

#Q2

myaov <- with(joined.long.df, 
              aov(Mercury ~ Calcium))

myaov <-  aov(Mercury ~ Calcium, 
              data = joined.long.df)



summary(myaov)


model.tables(myaov, type = "means")



TukeyHSD(myaov)


#Q3 

myaov <- with(joined.long.df, 
              aov(Mercury ~ Calcium*Time))



summary(myaov)




model.tables(myaov, type = "means")






TukeyHSD(myaov)





# Q4

ca.ph.tab <- with(joined.df, 
                  table(Calcium, pHtype))
ca.ph.tab

chisq.test(ca.ph.tab)


fisher.test(ca.ph.tab)




# Q5

with(joined.long.df, plot(pH, Mercury))


mylm <- with(joined.long.df, 
             lm(Mercury ~ pH))


summary(mylm)


plot(predict(mylm), residuals(mylm))
abline(h = 0, lwd = 2, col = 2)




#Session 8 ##############


cont <- ifelse(joined.long.df$Mercury > 0.53, 1, 0)


table(cont)


joined.long.df$cont <- cont


myglm <- glm(cont~pH, family = binomial, 
             data = joined.long.df)



anova(myglm, test = "Chisq")

summary(myglm)

myglm1 <- glm(cont~Calcium,
              data = joined.long.df,
              family = binomial)

anova(myglm1, test = "Chisq")

summary(myglm1)


exp(1.0726 )/(1 + exp(1.0726 ))

exp(1.0726-1.6116 )/(1 + exp(1.0726-1.6116 ))

exp(1.0726-2.9444)/(1 + exp(1.0726-2.9444))


newdata = 
  data.frame(Calcium = 
               c("High", "Medium", "Low"))
predict(myglm1, 
        newdata, type = "response")













