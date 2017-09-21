


carcount <- function(o, plot = FALSE){
  # slow cars
  timestart1 <- sort(runif(n= 10, 0,30))
  
  # fast car leave times
  timestart2 <- sort(runif(n = 10, 0,30))
  
  speedSlow = 25
  speedFast = 80
  
  timeLeaveRoad1 <- timestart1 + 60/speedSlow
  timeLeaveRoad2 <- timestart2 + 60/speedFast
  
  
  # distance of observation
  dobs <- 1 #mile
  
  # you observe cars for 1 minute, every 5 minutes
  timeObsStart <- seq(0,30, by = 5)
  timeObsEnd <- timeObsStart + 1
  
  if(plot){
    par(mfrow = c(2,1))
    plot(x = timestart2, y = seq(1:length(timestart2)), xlim = c(0, 35), type = 'n')
    rect(xleft = timeObsStart, ybottom = 0, xright = timeObsEnd, ytop = 15, density = 100, col = 'grey')
    segments(x0 = timestart2, x1 = timeLeaveRoad2, y0 = seq(1:length(timestart2)), y1 = seq(1:length(timestart2)))
    
    
    plot(x = timestart1, y = seq(1:length(timestart1)), xlim = c(0, 35), type = 'n')
    rect(xleft = timeObsStart, ybottom = 0, xright = timeObsEnd, ytop = 15, density = 100, col = 'grey')
    segments(x0 = timestart1, x1 = timeLeaveRoad1, y0 = seq(1:length(timestart1)), y1 = seq(1:length(timestart1)))
  }
  
  
  
  # count observations
  (countFast <- sum(sapply(1:length(timeObsStart), FUN = function(x) {
    timestart2 < timeObsEnd[x] &  timeLeaveRoad2 > timeObsStart[x]
  })))
  
  (countSlow <- sum(sapply(1:length(timeObsStart), FUN = function(x) {
    timestart1 < timeObsEnd[x] &  timeLeaveRoad1 > timeObsStart[x]
  })))
  
  return(c(fast = countFast, slow = countSlow))

}

carcount(plot = TRUE)


cts <- t(as.data.frame(sapply(1:1000, carcount)))
colMeans(cts)
