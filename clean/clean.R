setwd("/Users/didi/src/modern-agriculture");
library(gdata);
mydata = read.xls("./data/data.xlsx");
data <- mydata
head(mydata, 20);

# calculate water level
heightBox <- 17
data$Tank.Level <- heightBox - data$Tank.Level

### 1. Tank Level ###

# Boxplot and Histogram of Tank Level
par(mfrow=c(2,2))

hist(data$Tank.Level, main = "Histogram of Tank.Level", xlab = "Tank Level")
boxplot(data$Tank.Level, main = "Boxplot of Tank.Level", xlab = "Tank Level")

# a. delete out value
repeat {
  outvalue <- boxplot.stats(data$Tank.Level)$out
  data$Tank.Level[data$Tank.Level %in% outvalue ] <- NA
  
  if (length(outvalue) == 0) break
}

hist(data$Tank.Level, main = "Histogram of Tank.Level", xlab = "Tank Level")
boxplot(data$Tank.Level, main = "Boxplot of Tank.Level", xlab = "Tank Level")

### 2. Conductivity ###

# Boxplot and Histogram of Conductivity
par(mfrow=c(2,2))

# remove zero values
data$Conductivity[data$Conductivity == 0] <- NA

hist(data$Conductivity, main = "Histogram of Conductivity")
boxplot(data$Conductivity, main = "Boxplot of Conductivity")

# delete out value
repeat {
  outvalue <- boxplot.stats(data$Conductivity)$out
  data$Conductivity[data$Conductivity %in% outvalue ] <- NA
  
  if (length(outvalue) == 0) break
}

hist(data$Conductivity, main = "Histogram of Conductivity")
boxplot(data$Conductivity, main = "Boxplot of Conductivity")

### 3. Tank Level and Conductivity

# delete NA rows
#data <- na.omit(data)

# scale Tank Level
data$Tank.Level <- scale(data$Tank.Level)
data$Conductivity <- scale(data$Conductivity)

# plot
par(mfrow=c(1,1))
x <- c(1:length(data$Time))
plot(x, data$Tank.Level, col="blue")
points(x, data$Conductivity, col="red")

smoothScatter(x, data$Tank.Level, main="Scatterplot Colored by Smoothed Densities")
### 3. focus on the middle section data ###

lo <- 10603
#hi <- 18558
hi <- 14830

ID <- mydata$ID[lo:hi]
Time <- mydata$Time[lo:hi]
Water.Temperature <- mydata$Water.Temperature[lo:hi]
Air.Humidity <- mydata$Air.Humidity[lo:hi]
Temperature <- mydata$Temperature[lo:hi]
Tank.Level <- mydata$Tank.Level[lo:hi]
Conductivity <- mydata$Conductivity[lo:hi]
Natural.Light <- mydata$Natural.Light[lo:hi]
LED.Status <- mydata$LED.Status[lo:hi]
LED.Intensity <- mydata$LED.Intensity[lo:hi]

smpl <- data.frame(ID, Time, Water.Temperature, Air.Humidity, Temperature, Tank.Level, Conductivity, Natural.Light, LED.Status, LED.Intensity)

# a. calculate water level
heightBox <- 17
smpl$Tank.Level <- heightBox - smpl$Tank.Level

# b. delete out value
repeat {
  outvalue <- boxplot.stats(smpl$Tank.Level)$out
  smpl$Tank.Level[smpl$Tank.Level %in% outvalue ] <- NA
  
  if (length(outvalue) == 0) break
}

repeat {
  outvalue <- boxplot.stats(smpl$Conductivity)$out
  smpl$Conductivity[smpl$Conductivity %in% outvalue ] <- NA
  
  if (length(outvalue) == 0) break
}

repeat {
  outvalue <- boxplot.stats(smpl$Air.Humidity)$out
  smpl$Air.Humidity[smpl$Air.Humidity %in% outvalue ] <- NA
  
  if (length(outvalue) == 0) break
}

repeat {
  outvalue <- boxplot.stats(smpl$Temperature)$out
  smpl$Temperature[smpl$Temperature %in% outvalue ] <- NA
  
  if (length(outvalue) == 0) break
}

repeat {
  outvalue <- boxplot.stats(smpl$Water.Temperature)$out
  smpl$Water.Temperature[smpl$Water.Temperature %in% outvalue ] <- NA
  
  if (length(outvalue) == 0) break
}

# c. remove zero values
smpl$Conductivity[smpl$Conductivity == 0] <- NA

# d. scale Tank Level
smpl$Water.Temperature <- scale(smpl$Water.Temperature)
smpl$Air.Humidity <- scale(smpl$Air.Humidity)
smpl$Temperature <- scale(smpl$Temperature)
smpl$Tank.Level <- scale(smpl$Tank.Level)
smpl$Conductivity <- scale(smpl$Conductivity)
smpl$Natural.Light <- scale(smpl$Natural.Light)


smplcor <- data.frame(smpl$Water.Temperature, smpl$Air.Humidity, smpl$Temperature, smpl$Tank.Level, smpl$Conductivity)
cor(smplcor, use="complete.obs")
cor.test(smpl$Tank.Level, smpl$Conductivity, use="complete.obs") # Tank.Level 和 Conductivity显著相关
cor.test(smpl$Air.Humidity, smpl$Conductivity) # Air.Humidity 和 Conductivity显著相关
cor.test(smpl$Air.Humidity, smpl$Temperature) # Air.Humidity 和 Tank.Level显著相关
library(psych)
corr.test(smplcor, use="complete")

# plot
library(TTR)
par(mfrow=c(1,1))
plot(smpl$Temperature, smpl$Air.Humidity)
plot(smpl$Air.Humidity, smpl$Conductivity)
plot(c(1:length(smpl$Tank.Level)), smpl$Tank.Level, col="blue" , xlab = "Minute", ylab = "Value")
points(c(1:length(smpl$Conductivity)), smpl$Conductivity, col="red")
#abline(lm(smpl$Tank.Level ~ c(1:length(smpl$Tank.Level))))
#abline(lm(smpl$Conductivity ~ c(1:length(smpl$Conductivity))))

# I. handle miss value
# 1. show stat of missing value
head(smpl)
library(mice)
md.pattern(smpl)
library(VIM)
aggr(smpl, prop=FALSE, numbers=TRUE)

# 2. MI
aggr(smplcor, prop=FALSE, numbers=TRUE)
imp <- mice(smplcor, seed=1234)
fit <- with(imp, lm(smplcor$smpl.Conductivity ~ smplcor$smpl.Air.Humidity))
pooled <- pool(fit)
summary(pooled)

smplcor <- complete(imp, action=3)

# 3. 散点矩阵图
pairs(~smplcor$smpl.Tank.Level+smplcor$smpl.Conductivity+smplcor$smpl.Water.Temperature+smplcor$smpl.Temperature+smplcor$smpl.Air.Humidity,
      main="Basic Scatter Plot Matrix")

library(car)
scatterplotMatrix(~smplcor$smpl.Tank.Level+smplcor$smpl.Conductivity+smplcor$smpl.Water.Temperature+smplcor$smpl.Temperature+smplcor$smpl.Air.Humidity,
                  data=smplcor, lty.smooth=2, main="Scatter Plot Matrix via car Package")


# II. filter
par(mfrow=c(1,1))
smplcor$Tank.Level.SMA <- SMA(smplcor$smpl.Tank.Level, 1500)
plot(c(1:length(smplcor$Tank.Level)), smplcor$smpl.Tank.Level, col="blue")
points(c(1:length(smplcor$Tank.Level)), smplcor$Tank.Level.SMA, col="green")

smoothScatter(c(1:length(smplcor$Tank.Level)), 
              smplcor$smpl.Tank.Level,
              main="Scatterplot Colored by Smoothed Densities")

# filter
size <- length(smpl$Tank.Level)
for (num in seq(266, 10)) {
  intervel <- seq.int(1, size, length.out = num)
  for (i in seq(1, length(intervel) - 1)) {
    j = i + 1
    
    lo <- intervel[i]
    hi <- intervel[j]
    
    tank.level <- smpl$Tank.Level[lo:hi]
    #print(tank.level)
    
    # delete out value
    repeat {
      outvalue <- boxplot.stats(tank.level)$out
      tank.level[tank.level %in% outvalue ] <- NA
      
      if (length(outvalue) == 0) break
    }
    
    smpl$Tank.Level[lo:hi] <- tank.level
  }
}

points(c(1:length(smpl$Tank.Level)), smpl$Tank.Level, col="green")
x <- c(1:length(smpl$Tank.Level))
abline(lm(smpl$Tank.Level ~ x))
abline(lm(smpl$Conductivity ~ x))

## 1. 什么原因导致电导率会有断点，进行分段分析？
## 2. 


# plot the middle section
lo <- 15000
hi <- 16000
plot(c(lo:hi), mydata$Tank.Level[lo:hi], col="blue")
plot(c(lo:hi), mydata$Conductivity[lo:hi], col="red")

mydata[14825:14835,]
