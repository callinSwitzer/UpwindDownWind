
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
#########################################################

lengthOfTunnel = 1.0 # meters
exposureTime = 1.0 # seconds
timeBWExposures = 59.0 # seconds
numBees = 10000

cameraStarts = c(0, 60, 120, 180, 240)
cameraStarts = c(60)
cameraStops = cameraStarts + exposureTime
camData = data.frame(cameraStarts, cameraStops)


beeID = seq(1, numBees, 1)
beeStartTimes = runif(numBees, min = 0, max = max(cameraStarts + 1)) # starts between 0 and 60 seconds
beeSpeeds = sample(updn$MedVel, replace = TRUE, size = numBees) # meters per second
beeEndTimes = beeStartTimes + 1/beeSpeeds


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

df2 <-  df1 %>% gather(beeStartTimes, beeEndTimes, key = "startStop", value = "times") %>%
  arrange(beeID) %>%
  mutate(beeID =  fct_reorder(as.factor(.$beeID), .x = .$times, .fun = min))


ggplot(df2, aes(x = times, y = beeID, color = beeCaught)) + 
  geom_line() + 
  labs(x = "Time (s)", y= "Bee ID") + 
  geom_rect(data = camData, inherit.aes = FALSE,  
            aes(xmin=cameraStarts, xmax=cameraStops, ymin=0, ymax=Inf), 
            color = NA, fill = 'yellow', alpha = 0.8) + 
  theme(axis.text.y=element_blank()) + 
  scale_color_grey(start = 0.7, end = 0.1)



# linear model
mod1 <- glm(beeCaught ~ beeSpeeds, data = df1, family = binomial)
df1$predProb = predict(mod1, type = "response")

# calculate probabilities without using a linear model (see if they are the same)




plot(x = df1$beeSpeeds, y = df1$predProb)
abline(v = 0.5)
abline(v = 0.3)


ggplot(df1, aes(x = beeSpeeds, y = predProb)) + 
  geom_line() + 
  labs(x = "bee speed (m/s)", y = "Probability of catching bee with a camera")
