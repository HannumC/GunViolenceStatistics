#*****************************************************************************************
# One Sample t test
#*****************************************************************************************


# Null hypothesis: Gun death by state is equal to New Jersey's mean
x = c(17.79,19.59,14.2,16.93,7.89,11.75,4.48,10.8,12.49,12.63,2.71,14.08,8.67,13.04,8.19,11.44,14.15,19.15,11.89,9.75,3.18,12.03,7.88,17.55,14.56,16.94,8.99,14.16,7.03,5.69,15.63,4.39,12.42,11.89,11.14,16.41,11.76,11.36,5.33,15.6,9.47,15.86,10.5,11.69,10.37,10.46,9.07,15.1,9.93,17.51)
t.test(x, y = NULL, alternative = c("two.sided", "less", "greater"), mu = 5.69, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

#*****************************************************************************************
# Two Sample t test
#*****************************************************************************************

# Null hypothesis: Gun death is equal for states that allow versus prohibit open gun carry
x = c(7.89,12.49,2.71,8.67,4.39,5.33,15.6)
y = c(17.79,19.59,14.2,16.93,11.75,4.48,10.8,12.63,14.08,13.04,8.19,11.44,14.15,19.15,11.89,9.75,3.18,12.03,7.88,17.55,14.56,16.94,8.99,14.16,7.03,5.69,15.63,12.42,11.89,11.14,16.41,11.76,11.36,9.47,15.86,10.5,11.69,10.37,10.46,9.07,15.1,9.93,17.51)
t.test(x, y, alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)



#*****************************************************************************************
# Linear Regression Graph
#*****************************************************************************************

# Scatter Plot of average houseold incoming vs gun violence per 100,000 people
y = c(17.55,16.93,15.1,17.79,14.15,15.63,19.15,15.6,15.86,12.42,14.08,16.41,12.49,16.94,14.56,13.04,11.14,12.03,12.63,14.2,11.89,14.16,9.47,11.44,11.76,8.19,8.99,9.93,10.5,11.36,10.37,5.33,8.67,17.51,11.89,4.39,10.8,11.69,7.88,11.75,9.07,7.89,10.46,7.03,3.18,4.48,5.69,19.59,2.71,9.75)
x = c(40593 ,41995 ,42019 ,44765 ,45215 ,45382 ,45727 ,47238 ,47275 ,47830 ,48275 ,48568 ,49426 ,49509 ,50238 ,50532 ,51075 ,51084 ,51244 ,51492 ,51494 ,52431 ,53017 ,53906 ,54148 ,54736 ,54996 ,55638 ,55653 ,55702 ,56990 ,58073 ,59588 ,60214 ,60557 ,60850 ,61255 ,62912 ,63488 ,63909 ,64129 ,64500 ,66262 ,70303 ,70628 ,71346 ,72222 ,73355 ,73486 ,75847)
z = c(1,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,0,1)
plot(x,y,col=c("red","blue"),pch=16,cex=2)
cor(x, y) # Correlation is -0.65 / Slight negative correlation
(cor(x,y))^2  # Coefficient of Determination


model <- lm(y~x) # Create the linear regression model
summary(model) # Provides information like the Signif, Error, t value, etc.
xslope <- round(as.numeric(model$coefficients[2]),2)
intercept <- round(as.numeric(model$coefficients[1]),2)
r2 <- round(summary(model)$r.squared,3)
plot(x,y,col=c("red","blue"),pch=16,cex=2)
abline(model,lwd=2)
text(60,180,labels=paste("y = ",xslope,
                         "x + (",intercept,")\nR-squared: ",r2))

# Residuals graph
residuals <- resid(model)
par(mfrow=c(1,2))
# plot(x,y,col="black",pch=16,
#      cex=2,main="Weight vs Height")
abline(model, lwd=3)
plot(x,residuals,
     main="Residuals")
abline(h=0)

# Multiple Regression Lines
xslope <- round(as.numeric(model$coefficients[2]),2)
intercept <- round(as.numeric(model$coefficients[1]),2)
r2 <- round(summary(model)$r.squared,3)
plot(x,y,col=c("red","blue"),pch=16,cex=2)
model1 <- lm( y[which(z==1)] ~ x[which(z==1)] )
model2 <- lm( y[which(z==0)] ~ x[which(z==0)] )
summary(model) # Provides information like the Signif, Error, t value, etc.
abline(model1,col="blue")
abline(model2,col="red")
text(60,180,labels=paste("y = ",xslope,
                         "x + (",intercept,")\nR-squared: ",r2))


# Scatter Plot of average houseold incoming vs gun violence per 100,000 people
abline(model1,col="blue")
abline(model2,col="red")
text(60,180,labels=paste("y = ",xslope,
                         +                          "x + (",intercept,")\nR-squared: ",r2))
y = c(17.55,16.93,15.1,17.79,14.15,15.63,19.15,15.6,15.86,12.42,14.08,16.41,12.49,16.94,14.56,13.04,11.14,12.03,12.63,14.2,11.89,14.16,9.47,11.44,11.76,8.19,8.99,9.93,10.5,11.36,10.37,5.33,8.67,17.51,11.89,4.39,10.8,11.69,7.88,11.75,9.07,7.89,10.46,7.03,3.18,4.48,5.69,19.59,2.71,9.75)
x1 = c(40593 ,41995 ,42019 ,44765 ,45215 ,45382 ,45727 ,47238 ,47275 ,47830 ,48275 ,48568 ,49426 ,49509 ,50238 ,50532 ,51075 ,51084 ,51244 ,51492 ,51494 ,52431 ,53017 ,53906 ,54148 ,54736 ,54996 ,55638 ,55653 ,55702 ,56990 ,58073 ,59588 ,60214 ,60557 ,60850 ,61255 ,62912 ,63488 ,63909 ,64129 ,64500 ,66262 ,70303 ,70628 ,71346 ,72222 ,73355 ,73486 ,75847)
x2 = c(1,1,1,1,1,1,1,0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,0,1)
mult.reg.model <- lm(y ~ x1+x2)
summary(mult.reg.model)



#*****************************************************************************************
# One Way ANOVA test 
#*****************************************************************************************
# If .txt tab file, use this
my_data <- read.csv('/hus11f01.csv')
my_data$Decade <- ordered(my_data$Decade,
                          levels = c("60", "70", "80","90", "0"))
library(dplyr)
group_by(my_data, Decade) %>%
  summarise(
    count = n(),
    mean = mean(Homicide, na.rm = TRUE),
    sd = sd(Homicide, na.rm = TRUE)
  )

# Box plots
# ++++++++++++++++++++
# Plot Homicide by Decade and color by Decade
library("ggpubr")
ggboxplot(my_data, x = "Decade", y = "Homicide", 
          color = "Decade", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#118833", "#AA66FF"),
          order = c("60", "70", "80","90", "0"),
          ylab = "Homicide", xlab = "Decade")

# Compute the analysis of variance
res.aov <- aov(Homicide ~ Decade, data = my_data)
# Summary of the analysis
summary(res.aov)




#*****************************************************************************************
# Chi-square tests. 
#*****************************************************************************************

# chi square test for states with or without required background checks for private sales
# H0: the laws for background checks are independant of the region the state is in
# Ha: the laws for background checks are dependant on the region the state is in

northEast <- c(2,7)
south <- c(13,3)
midwest <- c(8,4)
west <- c(6,5)
rows <- 4

backgroundMatrix <- matrix(c(northEast, south, midwest, west), nrow = rows, byrow = TRUE)

rownames(backgroundMatrix) <- c("North East", "South", "Midwest", "West")
colnames(backgroundMatrix) <- c("No", "Yes")
backgroundMatrix

backgroundResult <- chisq.test(backgroundMatrix, simulate.p.value = TRUE)
backgroundResult

backgroundResult$observed

round(backgroundResult$expected, 2)



# Chi-square test for states with unrestricted concealed carry vs. states with shall-issue
# permits vs. states with may-issue permits.
# Shall-issue: the granting of the license is subject only to meating requirements of the law
# May-issue: the granting of the license is partially subject to local authorities.

NorthEast <- c(3,1,4)
South <- c(2,12,2)
Midwest <- c(3,9,0)
West <- c(4,7,1)
rows <- 4

carryMatrix <- matrix(c(NorthEast, South, Midwest, West), nrow = rows, byrow = TRUE)
carryMatrix

rownames(carryMatrix) <- c("North East", "South", "Midwest", "West")
colnames(carryMatrix) <- c("Unrestricted","Shall-issue","May-issue")

carryResult <- chisq.test(carryMatrix, simulate.p.value = TRUE)
carryResult

carryResult$observed

round(carryResult$expected)


#*****************************************************************************************
# Histogram
#*****************************************************************************************
sampledata=read.csv(file="/OpenCarryStats.csv", header=TRUE, sep=",")
hist(sampledata$Rate.Per.100000,breaks =10 ,main="Distribution of Gun Violence incidents by State\n Per 100,000",ylab="Frequency Gun Violence",xlab = "Gun Violence Incidents",col="grey")


