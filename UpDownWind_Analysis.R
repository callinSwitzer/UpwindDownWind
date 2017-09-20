## Callin Switzer
## 20 Sept 2017
## Analysis of upwind / downwind data



# load packages
library(ggplot2)
library(plyr)


# set ggplot theme
theme_set(theme_bw())



# import data
updn <- read.csv("datasets/Upwind Downwind Data.csv")
head(updn)


# visualize data to gain intuition
# plot counts
ggplot(updn, aes(x = interaction(side, flow.category, flight.direction), fill = bee.wind.orientation)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot proportions
aa <- ggplot(updn, aes(x = interaction(side, flow.category, flight.direction), fill = bee.wind.orientation)) + 
  geom_bar(position = "fill") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_hline(aes(yintercept =  0.5))
aa

# Generalized Linear Model, after removing the no wind category
updn_sm <- updn[updn$bee.wind.orientation != "No wind'", ]

# code the orientation as a factor
updn_sm$bee.wind.orientation <- mapvalues(updn_sm$bee.wind.orientation, 
                                                    from = c("Downwind'", "Upwind'"), 
                                                    to = c(0, 1))

# drop unused levels of factors in new dataset
updn_sm <- droplevels(updn_sm)

m1 <- glm(bee.wind.orientation ~ side  + flow.category + flight.direction, family = binomial("logit"), data = updn_sm)
summary(m1)

# check Variance inflation factors -- a rule of thumb is that GVIF^(1/(2*Df)) should be less than sqrt(10)
car::vif(lm(as.numeric(as.factor(bee.wind.orientation)) ~ side + flow.category + flight.direction, data = updn))

# start with model that includes interactions (all)
m1.1 <- glm(bee.wind.orientation ~ side * flight.direction * flow.category, family = binomial("logit"), data = updn_sm)
summary(m1.1)




# see which, if any, predictors can be dropped
drop1(m1.1, test = "LRT") 

m2 <- update(m1.1, .~. - side:flight.direction:flow.category)

drop1(m2, test = "LRT") 

m3 <- update(m2, .~. - side:flow.category)

drop1(m3, test = "LRT")

summary(m3)

preds <- predict(m3, type = 'response', se = TRUE)


preddf <- cbind(updn_sm[, c("bee.wind.orientation", "side", "flight.direction", "flow.category")], fit = preds$fit,se =  preds$se.fit)
head(preddf)

preddf <- preddf[!(duplicated(preddf)), ]


# plot with 
ggplot(preddf, aes(y = fit, x = interaction(side, flow.category, flight.direction), fill = bee.wind.orientation)) + 
  geom_point() + 
  ylim(c(0, 1)) + 
  ylab("predicted probablity of flying upwind") +   
  geom_errorbar(aes(ymin = fit-2*se, ymax = fit + 2*se), width = 0.1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# visualize predictions along with original data
aa + geom_point(data = preddf, aes(y = fit, x = interaction(side, flow.category, flight.direction), fill = bee.wind.orientation))


# make 95% bootstrap CI's for proportions

bootInd <- sample(1:nrow(updn), replace = TRUE)
bootSamp <- updn[bootInd, ]

countDF <- as.data.frame(xtabs(~ bee.wind.orientation + side + flow.category + flight.direction, data = bootSamp))
head(countDF)

xtabs(~ bee.wind.orientation + side + flow.category + flight.direction, data = bootSamp)

bootdf <- as.data.frame(prop.table(xtabs(~  bee.wind.orientation +side + flow.category + flight.direction, data = bootSamp), margin = c(2,3,4)))

bootdf <- bootdf[bootdf$bee.wind.orientation == "Upwind'", ]

ggplot(bootdf, aes(y = Freq, x = interaction(side, flow.category, flight.direction), color = bee.wind.orientation)) + 
  geom_point() + 
  ylim(c(0, 1)) + 
  ylab("predicted probablity of flying upwind") 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

