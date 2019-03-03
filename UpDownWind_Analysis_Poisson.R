## Callin Switzer
## 20 Sept 2017
## Analysis of upwind / downwind data
## 2 March 2019
##



# load packages
library(ggplot2)
library(plyr)
library(viridis)
library(car)
library(multcomp)
library(tidyr)

# set ggplot theme
theme_set(theme_bw())




# import data
updn <- read.csv("datasets/Upwind Downwind Data.csv")
head(updn)

#________________________________________________________________________ 
# visualize data to gain intuition
#________________________________________________________________________ 
# plot counts
ggplot(updn, aes(x = interaction(side, flow.category, flight.direction), fill = bee.wind.orientation)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(updn, aes(x = interaction(side, flow.category, flight.direction), fill = bee.wind.orientation)) + 
  geom_bar() + 
  facet_wrap(~day) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(updn, aes(x = interaction(flight.direction, side), fill = bee.wind.orientation)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# this is the cause of "simpson's paradox"
ggplot(updn, aes(x = interaction(flight.direction, side), fill = bee.wind.orientation)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(updn[updn$bee.wind.orientation != "No wind'",], aes(x = interaction(flight.direction, side), fill = bee.wind.orientation)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




ggplot(updn[updn$bee.wind.orientation != "No wind'",], aes(x = interaction(side), 
                                                           fill = bee.wind.orientation)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(updn[updn$bee.wind.orientation != "No wind'",], aes(x = interaction(flight.direction, side, flow.category), fill = bee.wind.orientation)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


aa <- ggplot(updn, aes(x = interaction(side, flow.category, flight.direction), fill = bee.wind.orientation)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_hline(aes(yintercept =  0.5))
aa



# plot proportions
aa <- ggplot(updn, aes(x = interaction(side, flow.category, flight.direction), fill = bee.wind.orientation)) + 
  geom_bar(position = "fill") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_hline(aes(yintercept =  0.5))
aa

updn$ttrt <- with(updn, interaction(bee.wind.orientation, flow.category,  flight.direction))

tb <- table(updn$ttrt)
updn$trt <-  factor(updn$ttrt,
                    levels = names(tb[order(tb, decreasing = TRUE)]))


bb <- ggplot(updn, aes(x = trt, fill  = bee.wind.orientation)) + 
  geom_bar(position = "identity", alpha = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  geom_hline(aes(yintercept =  0.5)) + 
  scale_fill_viridis(discrete = TRUE) 
bb

bb + facet_wrap(~day)


# find the maximum number of minutes per day
tapply(updn$minute, INDEX = updn$day, max)

# find the sample sizes for each day
xtabs(~updn$day) # looks like some days were quite small -- like 6/12/17

# find unique experiments, in case we want to analyze separately
trts <- as.data.frame((tapply(as.character(updn$trt),updn$day, FUN = function(x) paste(sort(unique(x)) , collapse = " " ))))
trts$date = row.names(trts)
colnames(trts) = c("experiment", "date")
trts
unique(trts$experiment)

# label experiments in dataset
updn$experiment <- mapvalues(updn$day, from = trts$date, to = trts$experiment)

# visualize different experiments

cc <- ggplot(updn, aes(x = trt, fill = bee.wind.orientation)) + 
  geom_bar(position = "identity", alpha = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  geom_hline(aes(yintercept =  0.5)) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid(experiment ~ side  + flight.direction)
cc







#________________________________________________________________________ 
# Analyze each experiment separately
#________________________________________________________________________ 

exp1 <- updn[updn$experiment == levels(updn$experiment)[1], ]
# exp1 <- updn[updn$experiment == levels(updn$experiment)[1] & updn$flight.direction == "Nectar'", ]
colnames(exp1)

# visualize speed of bees in two conditions
ggplot(exp1, aes(x = flow.category, y = abs(y.velocity.avg), fill = day)) +
  facet_grid(side ~ flight.direction) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(exp1, aes(x = bee.wind.orientation, y = abs(y.velocity.avg), fill = day)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(updn, aes(x = bee.wind.orientation, y = abs(y.velocity.avg))) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(updn, aes(x = day, y = abs(y.velocity.avg), fill = (bee.wind.orientation))) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


ggplot(updn, aes(x = day, y = abs(y.velocity.avg), fill = (bee.wind.orientation))) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

modVel <- lm((abs(y.velocity.avg)) ~ bee.wind.orientation, data = updn)
summary(modVel)

par(mfrow = c(2,3))
plot(modVel, which = 1:6)



bb <- ggplot(exp1, aes(x = trt, fill  = bee.wind.orientation)) + 
  geom_bar(position = "identity", alpha = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  geom_hline(aes(yintercept =  0.5)) + 
  scale_fill_viridis(discrete = TRUE) 
bb

bb + facet_wrap(~day)



#Simpson's paradox
# plot #1 (slices data differently than below)
ggplot(exp1, aes(x = flight.direction)) + 
  geom_bar(alpha = 0.5, aes(fill = bee.wind.orientation)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid(~side, labeller = labeller(.rows = label_both, .cols = label_both))+
  labs(y = "Count of flights")

# if you look at each day, then the trend seems to be reversed
ggplot(exp1, aes(x = flight.direction)) + 
  geom_bar(alpha = 0.5, aes(fill = bee.wind.orientation)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid(day~side, labeller = labeller(.rows = label_both, .cols = label_both))+
  labs(y = "Count of flights")

ggplot(exp1, aes(x = flight.direction)) + 
  geom_bar(alpha = 0.5, aes(fill = bee.wind.orientation)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid(~side, labeller = labeller(.rows = label_both, .cols = label_both))+
  labs(y = "Count of flights")




ggplot(exp1, aes(x = side)) + 
  geom_bar(alpha = 0.5, aes(fill = bee.wind.orientation)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid(~bee.wind.orientation, labeller = labeller(.rows = label_both, .cols = label_both))+
  labs(y = "Count of flights")

ggplot(exp1, aes(x = flight.direction)) + 
  geom_bar(alpha = 0.5, aes(fill = bee.wind.orientation)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid(~day, labeller = labeller(.rows = label_both, .cols = label_both))+
  labs(y = "Count of flights")

# if you don't 
ggplot(exp1, aes(x= interaction(flight.direction))) + 
  geom_bar(alpha = 0.5, aes(fill = bee.wind.orientation)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid(day~side ,space = 'free', labeller = labeller(.cols = label_both))+
  labs(y = "Count of flights")

# if you don't
ggplot(exp1[exp1$flight.direction == "Nectar'", ], aes(x= interaction(flight.direction))) + 
  geom_bar(alpha = 0.5, aes(fill = bee.wind.orientation)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid(day~side ,space = 'free', labeller = labeller(.cols = label_both))+
  labs(y = "Count of flights")


ggplot(exp1, aes(x = day)) + 
  geom_bar(alpha = 0.5, aes(fill = bee.wind.orientation)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid(flight.direction ~side + day, labeller = labeller(.rows = label_both, .cols = label_both))+
  labs(y = "Count of flights")

# seemingly contradictory plots (doesn't account for right vs. left side and flight direction at the same time)
ggplot(exp1, aes(x = flight.direction, fill  = bee.wind.orientation)) + 
  geom_bar( alpha = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  scale_fill_viridis(discrete = TRUE) 

ggplot(exp1, aes(x = interaction(flight.direction, bee.wind.orientation), fill  = bee.wind.orientation)) + 
  geom_bar( alpha = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_wrap(~day)


cc <- ggplot(exp1, aes(x = side, fill = bee.wind.orientation)) + 
  geom_bar(position = "identity", alpha = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  geom_hline(aes(yintercept =  0.5)) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid(bee.wind.orientation  ~ flight.direction + flow.category, labeller = labeller(.rows = label_both, .cols = label_both))
cc


cc <- ggplot(exp1, aes(x = interaction(side, bee.wind.orientation), fill = bee.wind.orientation)) + 
  geom_bar(position = "identity", alpha = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  geom_hline(aes(yintercept =  0.5)) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid( flight.direction ~  flow.category, labeller = labeller(.rows = label_both, .cols = label_both))
cc

cc <- ggplot(exp1, aes(x = side, fill = bee.wind.orientation)) + 
  geom_bar(position = "fill", alpha = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  geom_hline(aes(yintercept =  0.5), linetype = 2) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid( flight.direction ~  flow.category, labeller = labeller(.rows = label_both, .cols = label_both))
cc



# glm for experiment 1
exp1$fly_upwind <- mapvalues(exp1$bee.wind.orientation, from = c("Downwind'", "No wind'", "Upwind'"), 
                                           to = c(0, 999, 1))

exp1$fly_upwind <- droplevels(exp1$fly_upwind)

table(exp1$fly_upwind)


# check VIF
car::vif(lm(as.numeric(fly_upwind) ~ side  +  flight.direction + day, data = exp1))

# model full interaction
m1 <- glm(fly_upwind ~ (side  +  flight.direction)^2, data = exp1, family = binomial("logit"))
m1 <- glm(fly_upwind ~ ( flight.direction + day)^2 + side, data = exp1, family = binomial("logit"))
summary(m1)

alias((lm(as.numeric(fly_upwind) ~ (side  +  flight.direction + day)^2, data = exp1)), partial = TRUE)
car::vif((lm(as.numeric(fly_upwind) ~ (side  +  flight.direction + day)^2, data = exp1)), partial = TRUE)

drop1(m1, test = "LRT")

m2 <- update(m1, .~. -flight.direction:day)
summary(m2)
drop1(m2, test = "LRT")

m3 <- update(m2, .~. - day)
drop1(m3, test = "LRT")
summary(m3)

m4 <- update(m3, .~. - flight.direction)
summary(m4)



# diagnostics
plot(m1, which = 4)


# visualize results
# calculate confidence intervals for predictions
preddf <- exp1[, c("side", "flight.direction", "flow.category", "day", "bee.wind.orientation")]
preddf <- expand.grid(side = unique(exp1$side), flight.direction = unique(exp1$flight.direction), 
                      day = unique(exp1$day), bee.wind.orientation = unique(exp1$bee.wind.orientation))



pdns <- data.frame(predict(m3, newdata = preddf, type = "link", se.fit = TRUE))
pdns$fit_prob = plogis(pdns$fit)
pdns$lower <- plogis(pdns$fit - 1.96* pdns$se.fit)
pdns$higher <- plogis(pdns$fit + 1.96* pdns$se.fit)

pdns <- data.frame(predict(m1, newdata = preddf, type = "link", se.fit = TRUE))
pdns$fit_prob = plogis(pdns$fit)
pdns$lower <- plogis(pdns$fit - 1.96* pdns$se.fit)
pdns$higher <- plogis(pdns$fit + 1.96* pdns$se.fit)

preddf <- cbind(preddf, pdns)




preddf

ggplot(exp1, aes(x = day)) + 
  geom_bar(alpha = 0.5, position = "identity", aes(fill = bee.wind.orientation)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  scale_fill_viridis(discrete = TRUE) + 
  labs(y = "Count of flights")

ab <- ggplot(preddf, aes(x = interaction(day) )) + 
  geom_point(aes(y = fit_prob, color = day)) + 
  facet_grid(side ~ flight.direction) + 
  ylim(c(0,1)) + 
  labs(y = 'predicted probability of flying upwind') + 
  geom_errorbar(aes(ymin = lower, ymax = higher, color = day), width = 0.1) + 
  geom_hline(aes(yintercept = 0.5), linetype = 2)
ab 
colnames(exp1)

# calculate averages from dataset
poisDF <- as.data.frame(xtabs(~bee.wind.orientation + flight.direction + day +  side, droplevels(exp1)))


daySums <- as.data.frame(tapply(posDF2$Freq, INDEX = list(posDF2$day, posDF2$side), sum))
daySums$day <- row.names(daySums)
  
daySums_l <- as.data.frame(gather(daySums, key = day))
colnames(daySums_l) <- c("day", "side", "day_side_total")


posdf2 <- merge(poisDF, daySums_l, by.x = c("day", "side"), by.y = c("day", "side"))




posdf2$dayPercents <- posdf2$Freq / posdf2$day_side_total

posdf2 <- posdf2[posdf2$Freq > 0, ]
  
  
m1.1 <- glm(Freq ~ (bee.wind.orientation + flight.direction + side)^2 + offset(log(day_side_total)), data = posdf2, family = poisson("log"))
summary(m1.1)

drop1(m1.1, test = 'LRT')

m1.2 <- update(m1.1, .~. - bee.wind.orientation:flight.direction)
summary(m1.2)
drop1(m1.2, test = 'LRT')



m1.3 <- update(m1.2, .~. - bee.wind.orientation:side)
summary(m1.3)
drop1(m1.3, test = 'LRT')

m1.4 <- update(m1.3, .~. - flight.direction:side)
summary(m1.4)
drop1(m1.4, test = 'LRT')

m1.5 <- update(m1.4, .~. - side)
summary(m1.5)
drop1(m1.5, test = 'LRT')


m1.6 <- update(m1.5, .~. - flight.direction)
summary(m1.6)

preddf2 <- posdf2
preddf2$day_side_total <- 1



pds <- predict(m1.6,newdata = preddf2, type = "link", se.fit = TRUE)

posdf2$predds <- exp(pds$fit)
posdf2$lower <- exp(pds$fit - 1.96*pds$se.fit)
posdf2$higher <- exp(pds$fit + 1.96*pds$se.fit)



# plot actual data and predictions

ggplot(posdf2, aes(x = interaction(side, flight.direction), y = dayPercents))+ 
  geom_point() + 
  facet_grid(day~bee.wind.orientation) + 
  geom_errorbar(aes(ymin = lower, ymax = higher), width = 0.1) + 
  geom_hline(aes(yintercept = 0.5), linetype = 2) + 
  geom_point(aes(y = predds), position = position_jitter(width = 0.1, height = 0), color = 'grey')


# plot actual data vs. predictions (again)
set.seed(123)
ggplot(posdf2, aes(x = bee.wind.orientation, y = dayPercents))+ 
  geom_point(position = position_jitter(width = 0.03), alpha = 0.2) + 
  geom_errorbar(aes(ymin = lower, ymax = higher), width = 0.05, color = 'black') + 
  geom_hline(aes(yintercept = 0.5), linetype = 2) + 
  geom_point(aes(y = predds), color = 'black') + 
  labs(x = "bee wind orientation", y = "probability of flight") +
  ylim(c(0,1)) + 
  ggtitle("Flight probablility for experiment 1 (fast both sides)")


#________________________________________________________________________ 
# Experiment 2
# this incorporates, fast/slow, left/right/ and box/nectar
#________________________________________________________________________ 

exp2 <- updn[updn$experiment %in% levels(updn$experiment)[2:3], ]
colnames(exp2)



# calculate averages from dataset
poisDF3 <- as.data.frame(xtabs(~bee.wind.orientation + flight.direction + day +  side + flow.category, droplevels(exp2)))


daySums <- as.data.frame(tapply(poisDF3$Freq, INDEX = list(poisDF3$day, poisDF3$side), sum))
daySums$day <- row.names(daySums)

daySums_l <- as.data.frame(gather(daySums, key = day))
colnames(daySums_l) <- c("day", "side", "day_side_total")


p3 <- merge(poisDF3, daySums_l, by.x = c("day", "side"), by.y = c("day", "side"))




p3$dayPercents <- p3$Freq / p3$day_side_total

p3 <- p3[p3$Freq > 0, ]
p3

m2.1 <- glm(Freq ~ (bee.wind.orientation + flight.direction + side + flow.category)^2 + offset(log(day_side_total)), data = p3, family = poisson("log"))
summary(m2.1)

drop1(m2.1, test = 'LRT')

m2.2 <- update(m2.1, .~. - side:flow.category)
summary(m2.2)
drop1(m2.2, test = 'LRT')



m2.3 <- update(m2.2, .~. - flight.direction:flow.category)
summary(m2.3)
drop1(m2.3, test = 'LRT')

m2.4 <- update(m2.3, .~. - bee.wind.orientation:flight.direction)
summary(m2.4)
drop1(m2.4, test = 'LRT')

m2.5 <- update(m2.4, .~. - flight.direction:side)
summary(m2.5)
drop1(m2.5, test = 'LRT')


m2.6 <- update(m2.5, .~. - bee.wind.orientation:side)
summary(m2.6)
drop1(m2.6, test = "LRT")

m2.7 <- update(m2.6, .~. - bee.wind.orientation:flow.category)
summary(m2.7)
drop1(m2.7, test = "LRT")

m2.8 <- update(m2.7, .~. - flow.category)
drop1(m2.8, test = "LRT")

m2.9 <- update(m2.8, .~. - side)
drop1(m2.9, test = "LRT")
m2.91 <- update(m2.9, .~. - flight.direction)
summary(m2.91)


preddf2 <- p3
preddf2$day_side_total <- 1



pds <- predict(m2.91,newdata = preddf2, type = "link", se.fit = TRUE)

p3$predds <- exp(pds$fit)
p3$lower <- exp(pds$fit - 1.96*pds$se.fit)
p3$higher <- exp(pds$fit + 1.96*pds$se.fit)



# plot actual data and predictions

ggplot(p3, aes(x = interaction(side, flight.direction), y = dayPercents))+ 
  geom_point() + 
  facet_grid(day~bee.wind.orientation) + 
  geom_errorbar(aes(ymin = lower, ymax = higher), width = 0.1) + 
  geom_hline(aes(yintercept = 0.5), linetype = 2) + 
  geom_point(aes(y = predds), position = position_jitter(width = 0.1, height = 0), color = 'grey')


# plot actual data vs. predictions (again)
set.seed(123)
ggplot(p3, aes(x = bee.wind.orientation, y = dayPercents))+ 
  geom_point(position = position_jitter(width = 0.03), alpha = 0.2) + 
  geom_errorbar(aes(ymin = lower, ymax = higher), width = 0.05, color = 'black') + 
  geom_hline(aes(yintercept = 0.5), linetype = 2) + 
  geom_point(aes(y = predds), color = 'black') + 
  labs(x = "bee wind orientation", y = "probability of flight") +
  ylim(c(0,1)) + 
  ggtitle("Flight probablility for experiment 2 (fast/slow wind)")


#________________________________________________________________________ 
# Experiment 3
# Control
#________________________________________________________________________ 


exp3 <- updn[updn$experiment %in% levels(updn$experiment)[4], ]
colnames(exp3)

ggplot(exp3, aes(x = interaction(side, flow.category, flight.direction), fill = side)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(exp3, aes(x = interaction(flight.direction), fill = side)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# calculate averages from dataset
poisDF3 <- as.data.frame(xtabs(~bee.wind.orientation + flight.direction + day +  side + flow.category, droplevels(exp3)))


daySums <- as.data.frame(tapply(poisDF3$Freq, INDEX = list(poisDF3$day, poisDF3$side), sum))
daySums$day <- row.names(daySums)

daySums_l <- as.data.frame(gather(daySums, key = day))
colnames(daySums_l) <- c("day", "side", "day_side_total")


p3 <- merge(poisDF3, daySums_l, by.x = c("day", "side"), by.y = c("day", "side"))




p3$dayPercents <- p3$Freq / p3$day_side_total

p3 <- p3[p3$Freq > 0, ]
p3

m2.1 <- glm(Freq ~ (flight.direction + side)^2 + offset(log(day_side_total)), data = p3, family = poisson("log"))
summary(m2.1)

car::vif(lm(Freq ~ (flight.direction + side)^2 + offset(log(day_side_total)), data = p3))

drop1(m2.1, test = 'LRT')

m2.2 <- update(m2.1, .~. - flight.direction:side)
summary(m2.2)
drop1(m2.2, test = 'LRT')



m2.3 <- update(m2.2, .~. - side)
summary(m2.3)

preddf2 <- p3
preddf2$day_side_total <- 1



pds <- predict(m2.3,newdata = preddf2, type = "link", se.fit = TRUE)

p3$predds <- exp(pds$fit)
p3$lower <- exp(pds$fit - 1.96*pds$se.fit)
p3$higher <- exp(pds$fit + 1.96*pds$se.fit)
p3


# plot actual data and predictions

ggplot(p3, aes(x = flight.direction, y = dayPercents))+ 
  geom_point() + 
  facet_grid(~day) + 
  geom_errorbar(aes(ymin = lower, ymax = higher), width = 0.1) + 
  geom_hline(aes(yintercept = 0.5), linetype = 2) + 
  geom_point(aes(y = predds), position = position_jitter(width = 0.1, height = 0), color = 'grey')


# plot actual data vs. predictions (again)
set.seed(123)
ggplot(p3, aes(x = flight.direction, y = dayPercents))+ 
  geom_point(position = position_jitter(width = 0.03), alpha = 0.2) + 
  geom_errorbar(aes(ymin = lower, ymax = higher), width = 0.05, color = 'black') + 
  geom_hline(aes(yintercept = 0.5), linetype = 2) + 
  geom_point(aes(y = predds), color = 'black') + 
  labs(x = "bee wind orientation", y = "probability of flight") +
  ylim(c(0,1)) + 
  ggtitle("Flight probablility for experiment 3 (no wind)")
