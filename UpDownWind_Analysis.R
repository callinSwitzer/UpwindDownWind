## Callin Switzer
## 20 Sept 2017
## Analysis of upwind / downwind data



# load packages
library(ggplot2)
library(plyr)
library(viridis)
library(car)
library(multcomp)


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
exp1 <- updn[updn$experiment == levels(updn$experiment)[1] & updn$flight.direction == "Nectar'", ]
colnames(exp1)


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
m1 <- glm(fly_upwind ~ (side  +  flight.direction + day)^2, data = exp1, family = binomial("logit"))
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
  geom_bar(alpha = 0.5, position = "identity" aes(fill = bee.wind.orientation)) + 
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
xtabs(~bee.wind.orientation + flight.direction + day +  side, droplevels(exp1))

ab + 
  geom_bar(data = exp1, aes(x = day, color = ), position = 'fill', alpha = 0.1)

# post-hoc test to see if pref is greater than 0.5 in all categories

exp1$intVar <- with(exp1, interaction(flight.direction, day))
exp1$intVar <- gsub("[[:punct:]]", "", exp1$intVar) 


m1.1 <- glm(fly_upwind ~ intVar + 0, data = exp1, family = binomial("logit"))
summary(m1.1)


# calculate confidence intervals for proportions
CIs <- plogis(cbind(coef(m1.1), confint(m1.1)))

CIs <- data.frame(CIs, coefs = row.names(CIs))
colnames(CIs) <- c("mean", "low", "high", "coefs")

CIs$side = mapvalues(CIs$coefs, from = c("intVarLeftBox", "intVarLeftNectar", "intVarRightBox", "intVarRightNectar"), 
                     to = c("Left'", "Left'", "Right'", "Right'"))

CIs$flight.direction = mapvalues(CIs$coefs, from = c("intVarLeftBox", "intVarLeftNectar", "intVarRightBox", "intVarRightNectar"), 
                     to = c("Box'", "Nectar'", "Box'", "Nectar'"))


K <- diag(length(coef(m1.1)))
rownames(K) <- names(coef(m1.1))

summary(glht(m1.1, linfct = K),test = adjusted("none") ) # same as above

# plot mean and CI's
cc <- ggplot(exp1, aes(x = side)) + 
  geom_bar(position = "fill", alpha = 0.5, aes(fill = bee.wind.orientation)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  geom_hline(aes(yintercept =  0.5), linetype = 2) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid( flight.direction ~  flow.category, labeller = labeller(.rows = label_both, .cols = label_both))
cc + geom_point(data = CIs, aes(x = side, y = mean)) + 
  geom_errorbar(data = CIs, aes(ymin = low, ymax = high), width = 0.1) + 
  labs(y = "Proportion of flights upwind")


# double check to make sure prediction confidence intervals are the same
# calculate confidence intervals for predictions
preddf <- exp1[, c("side", "flight.direction", "flow.category")]
preddf <- preddf[!(duplicated(preddf)), ]


pdns <- data.frame(predict(m1, newdata = preddf, type = "link", se.fit = TRUE))
pdns$fit_prob = plogis(pdns$fit)
pdns$lower <- plogis(pdns$fit - 1.96* pdns$se.fit)
pdns$higher <- plogis(pdns$fit + 1.96* pdns$se.fit)

pdns <- data.frame(predict(m1, newdata = preddf, type = "link", se.fit = TRUE))
pdns$fit_prob = plogis(pdns$fit)
pdns$lower <- plogis(pdns$fit - 1.96* pdns$se.fit)
pdns$higher <- plogis(pdns$fit + 1.96* pdns$se.fit)

preddf <- cbind(preddf, pdns)


preddf
CIs



#________________________________________________________________________ 
# Experiment 2
# this incorporates, fast/slow, left/right/ and box/nectar
#________________________________________________________________________ 

exp2 <- updn[updn$experiment %in% levels(updn$experiment)[2:3], ]
colnames(exp2)

dd <- ggplot(exp2, aes(x = side, fill = bee.wind.orientation)) + 
  geom_bar(position = "identity", alpha = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  geom_hline(aes(yintercept =  0.5)) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid(bee.wind.orientation  ~ flight.direction + flow.category, labeller = labeller(.rows = label_both, .cols = label_both))
dd



dd <- ggplot(exp2, aes(x = side, fill = bee.wind.orientation)) + 
  geom_bar(position = "fill", alpha = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  geom_hline(aes(yintercept =  0.5), linetype = 2) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid(flow.category~flight.direction, labeller = labeller(.rows = label_both, .cols = label_both))
dd



# glm for experiment 2
exp2$fly_upwind <- mapvalues(exp2$bee.wind.orientation, from = c("Downwind'", "No wind'", "Upwind'"), 
                             to = c(0, 999, 1))

exp2$fly_upwind <- droplevels(exp2$fly_upwind)

table(exp2$fly_upwind)


# check VIF
car::vif(lm(as.numeric(fly_upwind) ~ side  +  flight.direction + flow.category, data = exp2))

# model full interaction
m1 <- glm(fly_upwind ~ side  *  flight.direction * flow.category , data = exp2, family = binomial("logit"))
summary(m1)

drop1(m1, test = "LRT")

m2 <- update(m1, .~. - side:flight.direction:flow.category)
summary(m2)

drop1(m2, test = "LRT")

m3 <- update(m2, .~. - side:flow.category)
summary(m3)

drop1(m3, test = "LRT") # can't drop any more

# calculate confidence intervals for predictions
preddf <- exp2[, c("side", "flight.direction", "flow.category")]
preddf <- preddf[!(duplicated(preddf)), ]


# predictions can be from m1 or m3
pdns <- data.frame(predict(m1, newdata = preddf, type = "link", se.fit = TRUE))
pdns$fit_prob = plogis(pdns$fit)
pdns$lower <- plogis(pdns$fit - 1.96* pdns$se.fit)
pdns$higher <- plogis(pdns$fit + 1.96* pdns$se.fit)

preddf <- cbind(preddf, pdns)

dd <- ggplot(exp2, aes(x = side)) + 
  geom_bar(position = "fill", alpha = 0.5, aes(fill = bee.wind.orientation)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  geom_hline(aes(yintercept =  0.5), linetype = 2) + 
  scale_fill_viridis(discrete = TRUE) + 
  facet_grid(flow.category~flight.direction, labeller = labeller(.rows = label_both, .cols = label_both))
dd
dd + geom_point(data = preddf, aes(x = side, y = fit_prob)) + 
  geom_errorbar(data = preddf, aes(ymin = lower, ymax = higher), width = 0.1) + 
  labs(y = "Proportion of flights upwind")

# diagnostics
plot(m1, which = 4)
summary(m1)


#________________________________________________________________________ 
# Experiment 3
# control experiment, incorporates left/right, and box/nectar, no wind
#________________________________________________________________________ 
exp3 <- updn[updn$experiment == levels(updn$experiment)[4], ]

ee <- ggplot(exp3, aes(x = side)) + 
  geom_bar(position = "identity", alpha = 0.5, aes(fill = side)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  geom_hline(aes(yintercept =  0.5)) + 
  scale_fill_viridis(discrete = TRUE, option = "C") + 
  facet_grid(bee.wind.orientation  ~ flight.direction + flow.category, labeller = labeller(.rows = label_both, .cols = label_both))
ee

ee <- ggplot(exp3, aes(x = flight.direction)) + 
  geom_bar(position = "fill", alpha = 0.5, aes(fill = side)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  geom_hline(aes(yintercept =  0.5)) + 
  scale_fill_viridis(discrete = TRUE, option = "C") + 
  facet_grid(bee.wind.orientation  ~  flow.category, labeller = labeller(.rows = label_both, .cols = label_both))
ee



# glm for experiment 3
exp3$fly_right <- mapvalues(exp3$side, from = c("Left'", "Right'"), 
                             to = c(0, 1))

exp3$fly_right <- droplevels(exp3$fly_right)

table(exp3$fly_right)


# check VIF

# model
m1 <- glm(fly_right ~ flight.direction, data = exp3, family = binomial("logit"))
summary(m1)

drop1(m1, test = "LRT")

m2 <- update(m1, .~. - flight.direction)
summary(m2)


# calculate confidence intervals for predictions
preeef <- exp3[, c("side", "flight.direction", "flow.category")]
preeef <- preeef[!(duplicated(preeef)), ]

pdns <- data.frame(predict(m1, newdata = preeef, type = "link", se.fit = TRUE))
pdns$fit_prob = plogis(pdns$fit)
pdns$lower <- plogis(pdns$fit - 1.96* pdns$se.fit)
pdns$higher <- plogis(pdns$fit + 1.96* pdns$se.fit)

preeef <- cbind(preeef, pdns)

ee <- ggplot(exp3, aes(x = flight.direction)) + 
  geom_bar(position = "fill", alpha = 0.5, aes(fill = side)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) + 
  geom_hline(aes(yintercept =  0.5)) + 
  scale_fill_viridis(discrete = TRUE, option = "C") + 
  facet_grid(bee.wind.orientation  ~  flow.category, labeller = labeller(.rows = label_both, .cols = label_both))
ee
ee + geom_point(data = preeef, aes(x = flight.direction, y = fit_prob)) + 
  geom_errorbar(data = preeef, aes(ymin = lower, ymax = higher), width = 0.1) + 
  labs(y = "Proportion of flights upwind")

# diagnostics
plot(m1, which = 4)

#________________________________________________________________________ 
# Generalized Linear Model, after removing the no wind category
#________________________________________________________________________ 






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

bootfun <- function(o){
  bootInd <- sample(1:nrow(updn), replace = TRUE)
  bootSamp <- updn[bootInd, ]
  
  bootdf <- as.data.frame(prop.table(xtabs(~  bee.wind.orientation +side + 
                                             flow.category + flight.direction, 
                                           data = bootSamp), margin = c(2,3,4)))
  
  bootdf <- bootdf[bootdf$bee.wind.orientation == "Upwind'", ]
  return(bootdf)
}

# boostrap 
boots <- lapply(1:1000, bootfun)

# combine bootstrap samples and do calculations
combboot <- do.call(rbind, boots)

bootMeans <- as.data.frame(tapply(X = combboot$Freq, INDEX = list(combboot$side, combboot$flow.category, combboot$flight.direction), mean))

bootMeans$side = row.names(bootMeans)

bootlo <- as.data.frame(tapply(X = combboot$Freq, INDEX = list(combboot$side, combboot$flow.category, combboot$flight.direction), quantile, 0.025))
bootlo$side = row.names(bootlo)

boothi <- as.data.frame(tapply(X = combboot$Freq, INDEX = list(combboot$side, combboot$flow.category, combboot$flight.direction), quantile, 0.975))
boothi$side = row.names(boothi)

bootdf2<- cbind(gather(boothi, side, hi), gather(bootlo, side, lo), gather(bootMeans,side, mean))

# removed duplicated columns
bootdf3 <- bootdf2[!duplicated(as.list(bootdf2))]


# plot bootstrap mean proportions and 95% CI's for each mean
ggplot(bootdf3, aes(y = mean, x = interaction(side, side.1))) + 
  geom_point() + 
  ylim(c(0, 1)) + 
  ylab("predicted probablity of flying upwind + 95% bootstrap CI's") +   
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# plot bootstrap on top of raw data
ggplot(updn, aes(x = interaction(side, flow.category, flight.direction))) + 
  geom_bar(position = "fill", aes(fill = bee.wind.orientation)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_hline(aes(yintercept =  0.5), linetype = 2) + 
  geom_point(data = bootdf3, aes(y = mean, x = interaction(side, side.1))) + 
  ylab("predicted probablity of flying upwind + 95% bootstrap CI's") +   
  geom_errorbar(data = bootdf3, aes(x = interaction(side, side.1), 
                                    ymin = bootdf3$lo, ymax = bootdf3$hi), width = 0.1) 

# plot without control treatment
updn2 <- updn[updn$flow.category != "None'", ]
boot3df2 <- bootdf3[bootdf3$hi != 0, ]

ggplot(updn2, aes(x = interaction(side, flow.category, flight.direction))) + 
  geom_bar(position = "fill", aes(fill = bee.wind.orientation), alpha = 0.6) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_hline(aes(yintercept =  0.5), linetype = 2) + 
  geom_point(data = boot3df2, aes(y = mean, x = interaction(side, side.1))) + 
  ylab("predicted probablity of flying upwind + 95% bootstrap CI's") + 
  xlab("treatment") + 
  geom_errorbar(data = boot3df2, aes(x = interaction(side, side.1), 
                                    ymin = boot3df2$lo, ymax = boot3df2$hi), width = 0.1)  + 
  scale_fill_viridis(discrete = TRUE, begin = 0.5, end = 0.8, option = "B")



#________________________________________________________________________  
############################ another approach ###########################
# Generalized Linear Model, where right or left is the thing of interest
#________________________________________________________________________ 



# code the side 1 = right, 0 = left
updn$side_int <- mapvalues(updn$side, 
                           from = c("Left'", "Right'"), 
                           to = c(0, 1))


# shows a problem with aliased coefficients
car::vif(lm(rnorm(nrow(updn)) ~ flow.category + bee.wind.orientation  +  flight.direction,  data = updn))
alias(lm(rnorm(nrow(updn)) ~ flow.category + bee.wind.orientation  +  flight.direction,  data = updn))

# bee.wind.orientation, "No Wind" is aliased with flow.category, "None".
# we can get around this problem by making a new variable, that combines these into a single group

updn$treatment <- interaction(updn$flow.category, updn$bee.wind.orientation)

# now VIF is fine
car::vif(lm(rnorm(nrow(updn)) ~ treatment  +  flight.direction,  data = updn))


m1 <- glm(side_int ~ treatment * flight.direction, family = binomial("logit"), data = updn)
summary(m1)


drop1(m1, test = 'LRT') #can't drop any variables

# visualize this model
# re-order the levels for plotting









