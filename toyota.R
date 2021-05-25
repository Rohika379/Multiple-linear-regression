
# Load the car dataset
car <- read.csv(file.choose())

#Choosing the right columns for our model

#Price,Age_08_04, KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight

car <-car[,c(3,4,7,9,13,14,16,17,18)]
View(car)
str(car)
attach(car)

# Normal distribution
qqnorm(Price)
qqline(Price)

# Exploratory data analysis:
# 1. Measures of central tendency
# 2. Measures of dispersion
# 3. Third moment business decision
# 4. Fourth moment business decision
# 5. Probability distributions of variables 
# 6. Graphical representations (Histogram, Box plot, Dot plot, Stem & Leaf plot, Bar plot, etc.)

summary(car)

# Scatter plot
plot(Age_08_04, Price) # Plot relation ships between each X with Y
plot(KM, Price)
plot(HP,Price)
plot(cc, Price)
plot(Doors,Price)
plot(Gears, Price)
plot(Quarterly_Tax,Price)
plot(Weight,Price)

# Or make a combined plot
pairs(car)   # Scatter plot for all pairs of variables
plot(car)

cor(Age_08_04, Price)
cor(KM, Price)
cor(HP,Price)
cor(cc, Price)
cor(Doors,Price)
cor(Gears, Price)
cor(Quarterly_Tax,Price)
cor(Weight,Price)

cor(car)
# The Linear Model of interest
model.car <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = car) # lm(Y ~ X)
summary(model.car)

model.carA <- lm( Price~ Age_08_04)
summary(model.carA)

model.carK <- lm(Price ~ KM)
summary(model.carK)

model.carH <-lm(Price~HP)
summary(model.carH)

model.carc <- lm( Price~ cc)
summary(model.carc)

model.carD <- lm(Price ~ Doors)
summary(model.carD)

model.carG <-lm(Price~Gears)
summary(model.carG)

model.carQ <- lm( Price~ Quarterly_Tax)
summary(model.carQ)

model.carW <- lm(Price ~ Weight)
summary(model.carW)

model.carALL <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight )
summary(model.carALL)

#### Scatter plot matrix with Correlations inserted in graph
install.packages("GGally")
library(GGally)
ggpairs(car)


### Partial Correlation matrix
install.packages("corpcor")
library(corpcor)
cor(car)
?car
cor2pcor(cor(car))

plot(model.car)# Residual Plots, QQ-Plot, Std. Residuals vs Fitted, Cook's distance

qqPlot(model.car, id.n = 5) # QQ plots of studentized residuals, helps identify outliers

library(car)
# Deletion Diagnostics for identifying influential obseravations
influenceIndexPlot(model.car, id.n = 3) # Index Plots of the influence measures
influencePlot(model.car, id.n = 3) # A user friendly representation of the above

# Regression after deleting the 77th observation
model.car1 <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = car[-810, ])
summary(model.car1)

### Variance Inflation Factors
vif(model.cor)  # VIF is > 10 => collinearity

#### Added Variable Plots ######
avPlots(model.car, id.n = 2, id.cex = 0.8, col = "red")

# Linear Model without cc
model.final <- lm(Price ~ Age_08_04+KM+HP+Doors+Gears+Quarterly_Tax+Weight, data = car)
summary(model.final)

# Linear model without SPEED and influential observation
model.final1 <- lm(Price ~Age_08_04+KM+HP+Doors+Gears+Quarterly_Tax+Weight , data = car[-810, ])
summary(model.final1)

# Added Variable Plots
avPlots(model.final1, id.n = 2, id.cex = 0.8, col = "red")

# Variance Influence Plot
vif(model.final1)

# Evaluation Model Assumptions
plot(model.final1)
plot(model.final1$fitted.values, model.final1$residuals)

qqnorm(model.final1$residuals)
qqline(model.final1$residuals)

# Subset selection
# 1. Best Subset Selection
# 2. Forward Stepwise Selection
# 3. Backward Stepwise Selection / Backward Elimination

install.packages("leaps")
library(leaps)
lm_best <- regsubsets(Price ~ ., data = car, nvmax = 15)
summary(lm_best)
?regsubsets
summary(lm_best)$adjr2
which.max(summary(lm_best)$adjr2)
coef(lm_best, 8)

lm_forward <- regsubsets(Price ~ ., data = car, nvmax = 15, method = "forward")
summary(lm_forward)

# Data Partitioning
n <- nrow(car)
n1 <- n * 0.7
n2 <- n - n1
train <- sample(1:n, n1)
test <- car[-train, ]

# Model Training
model <- lm(Price ~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, car[train, ])
summary(model)


pred <- predict(model, newdata = test)
actual <- test$Price
error <- actual - pred

test.rmse <- sqrt(mean(error**2))
test.rmse

train.rmse <- sqrt(mean(model$residuals**2))
train.rmse

# Step AIC
install.packages("MASS")
library(MASS)
stepAIC(model.car)
