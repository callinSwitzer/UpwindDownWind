
#install packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "lubridate")
ipak(packages)

# set ggplot theme
theme_set(theme_classic() + 
            theme(axis.text=element_text(colour="black"), 
                  text=element_text(size=10)))

# set  directories
dataDir <- file.path(getwd(), "data")
figDir <- file.path(getwd(), "figures")
dataOut <- file.path(getwd(), "dataOutput")

print(paste("last run ", Sys.time()))
print(R.version)


# Import data

updn1 <- read_csv(file.path(dataDir, "Data2D_UpwindDownwind_withVelocity.csv"))
updn2 <- read_csv(file.path(dataDir, "Data2D_UpwindDownwind.csv"))

# Tidy and Transform data

# merge datasets
updn1 <- updn1 %>% mutate(id = row_number())
updn2 <- updn2 %>% mutate(id = row_number())
updn_full = full_join(updn1, updn2, by =  c("id", "minute", "day", "side", "flight direction"))

# define function to convert strings to title case
titleCase = function(vect){
  gsub("(^|[[:space:]])([[:alpha:]])", 
       "\\1\\U\\2", tolower(vect), perl=TRUE)
}


# data tidying
updn_full <- updn_full %>%
  
  # rename columns to replace punctuation with "_"
  rename_all(list(~gsub('[[:punct:] ]+','_',colnames(updn_full)))) %>%
  
  # remove single quote from strings
  mutate_all(list(~gsub("\'", "", .))) %>%
  rename(MedVel = starts_with("med")) %>%
  
  # convert to datetime
  mutate(datetime = as_date(.$day, format = "%m_%d_%y", tz = "UTC"), 
         
         # convert to numeric
         minute = as.numeric(.$minute),
         MedVel = as.numeric(.$MedVel),
         flow_category = as.numeric(.$flow_category_x)) %>%
  
  # change factors
  mutate(side = titleCase(.$side), 
         flight_direction = recode(.$flight_direction, 
                                   "FEED" = "Towards feeder", 
                                   "NEST" = "Towards colony"), 
         fast_slow = titleCase(.$fast_slow),
         bee_wind_orientation = recode(bee_wind_orientation_x, 
                                       "DW" = "Downwind", 
                                       "UW" = "Upwind", 
                                       "NW" = "No wind"), 
         side = recode(side, "Rite" = "Right")) %>%
  mutate(trt_interaction = interaction(bee_wind_orientation_x, 
                                       fast_slow,  
                                       flight_direction))

# identify experiments
tb <- table(updn_full$trt_interaction)

# order by count
updn_full$trt <-  factor(updn_full$trt_interaction,
                         levels = names(tb[order(tb, decreasing = TRUE)]))

# find unique experiments, in case we want to analyze separately
trts <- as.data.frame((tapply(as.character(updn_full$trt),
                              updn_full$day, 
                              FUN = function(x) paste(sort(unique(x)) , collapse = " " ))))
trts$date = row.names(trts)
colnames(trts) = c("experiment", "date")

# label experiments in dataset
updn_full$experiment <- plyr::mapvalues(updn_full$day, 
                                        from = trts$date, 
                                        to = trts$experiment) %>% as.factor()


# remove data from exp 2 (slow/fast speed conditions)
updn <- updn_full[!(updn_full$experiment %in% levels(updn_full$experiment)[2:3]), ] %>% 
  droplevels()

# look at data
updn



#########################################################
#### Simulation
################################################################

# make figure 1
{
  lengthOfTunnel = 1.0 # meters
  exposureTime = 1.0 # seconds
  timeBWExposures = 59.0 # seconds
  numExposures = 2
  numBees = 100
  
  cameraStarts = seq(0, (numExposures -1)*(exposureTime + timeBWExposures), length.out = numExposures)
  cameraStops = cameraStarts + exposureTime
  camData = data.frame(cameraStarts, cameraStops)
  
  
  beeID = seq(1, numBees, 1)
  beeStartTimes = runif(numBees, min = 0 - max(beeSpeeds), max = max(cameraStarts + exposureTime)) # starts between 0 and 60 seconds
  
  # sample bee speeds from original dataset (even though it may be biased)
  qs = quantile(updn$MedVel, probs = seq(0.1, 99.9, length.out = 25)/100)
  beeSpeeds = sample(qs, size = numBees, replace = TRUE)
  
  # calculate end times
  beeEndTimes = beeStartTimes + 1/beeSpeeds*lengthOfTunnel
  
  
  df1 <- data.frame(beeID, beeStartTimes, beeEndTimes, beeSpeeds) %>% as.tbl()
  beeCaught = rep(FALSE, length(beeStartTimes))
  for (ii in 1:length(cameraStarts)){
    
    # three cases
    # bee starts after video starts
    case1 <- beeStartTimes > cameraStarts[ii] & beeStartTimes < cameraStops[ii]
    
    # bee is in frame when camera starts and ends before camera does
    case2 <- beeEndTimes > cameraStarts[ii] & beeEndTimes < cameraStops[ii]
    
    # bee starts before camera and ends after camera turns off
    case3 <- beeStartTimes < cameraStarts[ii] & beeEndTimes > cameraStops[ii]
    
    
    beeCaught = case1 | case2 | case3 | beeCaught
    
  }
  
  df1$beeCaught = beeCaught
  
  
}

df2 <-  df1 %>% gather(beeStartTimes, beeEndTimes, key = "startStop", value = "times") %>%
  arrange(beeID) %>%
  mutate(beeID =  fct_reorder(as.factor(.$beeID), .x = .$times, .fun = min))


ggplot(df2, aes(x = times, y = beeID, color = beeCaught)) +
  geom_line() +
  labs(x = "Time (s)", y= "Bee ID") +
  geom_rect(data = camData, inherit.aes = FALSE,
            aes(xmin=cameraStarts, xmax=cameraStops, ymin=0, ymax=Inf, fill = 'Recording'),
            color = NA, alpha = 0.8) +
  scale_fill_manual('Camera status',
                    values = 'yellow',  
                    guide = guide_legend(override.aes = list(alpha = 1))) + 
  theme(axis.text.y=element_blank()) +
  scale_color_grey(name = "Bee caught on camera", start = 0.7, end = 0.1) + 
  theme(legend.position = c(0.2, 0.8))

ggsave(file.path(figDir, "beeSimulationTimes.png"), width = 8, height = 4)








###################################################################################
### Fig 2
###################################################################################




# estimate num bees per experiment
nrow(updn[updn$day == unique(updn$day)[3], ]) ## ~ 500 bees in 120 seconds
# 250 bees per minute
250*60 # bees per hour
250 * 60 * 2 # bees per two hours



for(simNum in 1:20){
  lengthOfTunnel = 1 # meters
  exposureTime = 1.0 # seconds
  timeBWExposures = 59.0 # seconds
  numExposures = 120
  numBees = 250 * 60 * 2
  
  cameraStarts = seq(0, (numExposures -1)*(exposureTime + timeBWExposures), length.out = numExposures)
  cameraStops = cameraStarts + exposureTime
  camData = data.frame(cameraStarts, cameraStops)
  
  
  beeID = seq(1, numBees, 1)
  beeStartTimes = runif(numBees, min = 0 - max(beeSpeeds), max = max(cameraStarts + exposureTime)) # starts between 0 and 60 seconds
  
  # sample bee speeds from original dataset (even though it may be biased)
  qs = quantile(updn$MedVel, probs = seq(0.1, 99.9, length.out = 55)/100)
  
  # this basically gives the same info
  #qs = seq(min(updn$MedVel), max(updn$MedVel), length.out = 55)
  
  beeSpeeds = sample(qs, size = numBees, replace = TRUE)
  
  # calculate end times
  beeEndTimes = beeStartTimes + 1/beeSpeeds*lengthOfTunnel
  
  
  df1 <- data.frame(beeID, beeStartTimes, beeEndTimes, beeSpeeds) %>% as.tbl()
  beeCaught = rep(FALSE, length(beeStartTimes))
  for (ii in 1:length(cameraStarts)){
    
    # three cases
    # bee starts after video starts
    case1 <- beeStartTimes > cameraStarts[ii] & beeStartTimes < cameraStops[ii]
    
    # bee is in frame when camera starts and ends before camera does
    case2 <- beeEndTimes > cameraStarts[ii] & beeEndTimes < cameraStops[ii]
    
    # bee starts before camera and ends after camera turns off
    case3 <- beeStartTimes < cameraStarts[ii] & beeEndTimes > cameraStops[ii]
    
    
    beeCaught = case1 | case2 | case3 | beeCaught
    
  }
  
  df1$beeCaught = beeCaught
  
  probs = xtabs(df1$beeCaught ~ df1$beeSpeeds) / xtabs( ~ df1$beeSpeeds)
  relprobs = probs / max(probs)
  if (simNum == 1){
    smdf = data.frame(speed = qs, prob = probs, relprobs = relprobs)
  }
  else{
    smdf = rbind(smdf, data.frame(speed = qs, prob = probs, relprobs = relprobs))
  }
  
  
}



# create figure 2
firstPlt = ggplot(smdf, aes(x = speed, y = prob.Freq)) + 
  stat_smooth(method = "lm", aes(color = "Estimate"), formula = y ~ I(1/x), span = 0.2, se = FALSE, size = 0.5) + 
  geom_point(alpha = 0.2, stroke = 0, size = 1, aes(fill = "Simulated data")) + 
  labs(x = "Bee speed (m/s)", y = "Probability of catching bee with a camera\n(based on simulated data)") + 
  lims(y = c(0, 0.12), 
       x = c(0, 2)) + 
  scale_color_grey(name = "", start = 0, end = 0) + 
  theme(legend.position = c(0.7, 0.7), 
        legend.spacing.y = unit(-0.3, "cm"), 
        legend.background = element_blank()) + 
  scale_fill_manual(name = "", values = 1) 

firstPlt 

ggsave(file.path(figDir, "beeCatchProbability_1sec.png"), width = 8, height = 5)



ggplot(updn, aes(x = MedVel)) + 
  geom_histogram(aes( y=..density..), alpha = 0.2, color= 'black') + 
  labs(y = "Density", x = "Median Velocity (px/s)")



#### Evaluate bias in speed estimates

# estimate num bees per experiment
nrow(updn[updn$day == unique(updn$day)[3], ]) ## ~ 500 bees in 120 seconds
# 250 bees per minute
250*60 # bees per hour
250 * 60 * 2 # bees per two hours



{
  lengthOfTunnel = 1 # meters
  exposureTime = 1 # seconds
  timeBWExposures = 59 # seconds
  numExposures = 2
  numBees = 250 * 60 * 200
  
  cameraStarts = seq(0, (numExposures -1)*(exposureTime + timeBWExposures), length.out = numExposures)
  cameraStops = cameraStarts + exposureTime
  camData = data.frame(cameraStarts, cameraStops)
  
  
  beeID = seq(1, numBees, 1)
  beeStartTimes = runif(numBees, min = 0 - max(beeSpeeds), max = max(cameraStarts + exposureTime)) # starts between 0 and 60 seconds
  
  # sample bee speeds from original dataset (even though it may be biased)
  beeSpeeds = sample(updn$MedVel, size = numBees, replace = TRUE)
  
  # calculate end times
  beeEndTimes = beeStartTimes + 1/beeSpeeds*lengthOfTunnel
  
  
  df1 <- data.frame(beeID, beeStartTimes, beeEndTimes, beeSpeeds) %>% as.tbl()
  beeCaught = rep(FALSE, length(beeStartTimes))
  for (ii in 1:length(cameraStarts)){
    
    # three cases
    # bee starts after video starts
    case1 <- beeStartTimes > cameraStarts[ii] & beeStartTimes < cameraStops[ii]
    
    # bee is in frame when camera starts and ends before camera does
    case2 <- beeEndTimes > cameraStarts[ii] & beeEndTimes < cameraStops[ii]
    
    # bee starts before camera and ends after camera turns off
    case3 <- beeStartTimes < cameraStarts[ii] & beeEndTimes > cameraStops[ii]
    
    
    beeCaught = case1 | case2 | case3 | beeCaught
    
  }
  
  df1$beeCaught = beeCaught
  

  
  
}


# caught bees (from simulation)
hist(df1$beeSpeeds[df1$beeCaught], freq = FALSE, 
     breaks = 50, col = rgb(1,0,0,0.1), lty="blank", main = "", 
     xlab = "Bee speed (m/s)")
lines(density(df1$beeSpeeds[df1$beeCaught], width = 0.2, from = 0.2, to = 1.5), col = 'red')

# # all bees from simulation
hist(df1$beeSpeeds, freq = FALSE, breaks = 50, add = TRUE, col = rgb(0,0,1,0.2), lty = "blank")
lines(density(df1$beeSpeeds, width = 0.2, from =  0.2, to = 1.5), col = 'blue')

# # original data
# hist(updn$MedVel, freq = FALSE, breaks = 50, col = rgb(0,0,0,0.2), add = TRUE)
# lines(density(updn$MedVel, width = 0.2), col = 'grey20')
legend("topright", legend = c("Observed bee speeds", "Actual bee speeds"), col = c("red", "blue"), lty = 1)



ks.test(df1$beeSpeeds[df1$beeCaught], updn$MedVel)
ks.test(df1$beeSpeeds, updn$MedVel)

hist(updn$MedVel, freq = FALSE, breaks = 50, col = rgb(1,0,0,0.2))
lines(density(updn$MedVel, width = 0.2), col = 'red')
hist(df1$beeSpeeds, freq = FALSE, breaks = 50, col = rgb(1,0,0,0.2), add = TRUE)
lines(density(df1$beeSpeeds, width = 0.2), col = 'blue')

d1 = density(updn$MedVel,from = 0.2, to = 2, width = 0.25, n = 999)


samples = sample(x = d1$x, replace = TRUE, prob = d1$y, size = 100000)
min(samples)

hist(samples, breaks = 50, freq = FALSE)
lines(density(samples, width = 0.2))
hist(updn$MedVel, freq = FALSE, breaks = 50, col = rgb(1,0,0,0.2), add = TRUE)
lines(density(updn$MedVel, width = 0.2), col = 'red')


ks.test(samples, updn$MedVel)
