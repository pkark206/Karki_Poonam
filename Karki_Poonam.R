# Reading data
bd <- read.csv('fat.dat.txt', sep = "", header = F)

# Checking the column names
head(bd)

# Checking the structure of the dataframe
str(bd)

# Checking how many columns in the dataset
ncol(bd)

# Changing the default column names with the original column names
colnames(bd) <- c('#case', 'bfBrozek', 'bfSiri','density','age','weight','height',
                  'adiposity','ffwt','neckcir','chestcir','abdmcir','hipcir','thighcir',
                  'kneecir','anklecir','extBicep','forearmcir','wristcir')

#Fixing errors

bd$density[bd$density == 1.0665] <- NA
bd$density[bd$density == 1.0666] <- NA
bd$density[bd$density == 1.0991] <- NA
bd$`bfBrozek`[bd$`bfBrozek`==0] <- NA
bd$`bfSiri`[bd$`bfSiri`==0] <- NA
bd$height[bd$height == 69.5] <- 29.5


# Calculate correlation matrix
cor_matrix <- cor(bd)

# mean(bd$bfBrozek)
bd$bodyfat <-apply(bd[,2:3],1,mean)
head(bd)

#all subset regression
library(leaps)
leaps <-regsubsets(bodyfat ~ density+age+ weight+ height+adiposity+ffwt+neckcir+chestcir+abdmcir+hipcir+thighcir+kneecir+anklecir+extBicep+forearmcir+wristcir, data=bd,nbest=4)
plot(leaps, scale="adjr2",na.rm = TRUE)

subsTable <- function(obj, scale){
  x <- summary(leaps)
  m <- cbind(round(x[[scale]],3), x$which[,-1])
  colnames(m)[1] <- scale
  m[order(m[,1]), ]
}

subsTable(leaps, scale="adjr2")


#Using regression

fit3 <- lm(bodyfat ~density+age+ weight+ height+adiposity+ffwt+neckcir+chestcir+abdmcir+hipcir+thighcir+kneecir+anklecir+extBicep+forearmcir+wristcir, data=bd)
summary(fit3)

# Global Validation of Linear Model Assumption (gvlma)
library (gvlma)
gvmodel <- gvlma(fit3)
summary(gvmodel)

#finding the best fit

fit <- lm(bodyfat ~ weight+age+height+neckcir+chestcir+thighcir+kneecir+anklecir+extBicep+forearmcir+wristcir,  data=bd)
summary(fit)

# Global Validation of Linear Model Assumption (gvlma)
library (gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)

par(mfrow=c(2,2))
# Assessing normality
qqPlot(fit, simulate=TRUE, labels=row.names(wage),
       id=list(method="identify"), main="Q-Q Plot")

# Assessing linearity
library(car)
crPlots(fit)

# Independence of errors
durbinWatsonTest(fit)  # result from Bootstrap
durbinWatsonTest(fit, simulate=F)


# Assessing homoscedasticity
library(car)
ncvTest(fit)
spreadLevelPlot(fit)


# Evaluating multi-collinearity
library(car)
vif(fit) 
vif(fit) > 10 # problem?


#Outlier Test

outlierTest(fit)

#  Identifying high leverage points
# average hat value=p/n 
# where p=# of parameters estimated in the model including the incercept
# n=the sample size
# a hat value > 3*average hat value should be examined
hat.plot <- function(fit, bd) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  text(hatvalues(fit), labels=rownames(fit), cex=0.9, font=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit, bd)


# Identifying influential observations
# Cooks Distance D
# identify D values > 4/(n-k-1) 
# where n=sample size
# k=# of predictor variables

( cutoff <- 4/(nrow(bd)-length(fit$coefficients)-2) )
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

#Removal of outliers
bd <- bd[-c(39, 86, 216), ]







