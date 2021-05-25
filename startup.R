# Load the staertup dataset
startup <- read.csv(file.choose())
startup <-startup[,-c(4)]
View(startup)

attach(startup)

# Normal distribution
qqnorm(R.D.Spend)
qqline(R.D.Spend)

# Exploratory data analysis:
# 1. Measures of central tendency
# 2. Measures of dispersion
# 3. Third moment business decision
# 4. Fourth moment business decision
# 5. Probability distributions of variables 
# 6. Graphical representations (Histogram, Box plot, Dot plot, Stem & Leaf plot, Bar plot, etc.)

summary(startup)

# Scatter plot
plot(R.D.Spend, Profit) # Plot relation ships between each X with Y
plot(Administration, Profit)
plot(Marketing.Spend,Profit)

# Or make a combined plot
pairs(startup)   # Scatter plot for all pairs of variables
plot(startup)

cor(R.D.Spend, Profit)
cor(Administration,Profit)
cor(Marketing.Spend,Profit)
cor(startup) # correlation matrix

# The Linear Model of interest
model.star <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend, data = startup) # lm(Y ~ X)
summary(model.star)

model.starR <- lm( Profit~ R.D.Spend)
summary(model.starR)

model.starA <- lm(Profit ~ Administration)
summary(model.starA)

model.starM <-lm(Profit~Marketing.Spend)
summary(model.starM)

model.starRAM <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend)
summary(model.starRAM)

#### Scatter plot matrix with Correlations inserted in graph
install.packages("GGally")
library(GGally)
ggpairs(startup)


### Partial Correlation matrix
install.packages("corpcor")
library(corpcor)
cor(startup)
?car
cor2pcor(cor(startup))

plot(model.star)# Residual Plots, QQ-Plot, Std. Residuals vs Fitted, Cook's distance

qqPlot(model.star, id.n = 5) # QQ plots of studentized residuals, helps identify outliers

# Deletion Diagnostics for identifying influential obseravations
influenceIndexPlot(model.star, id.n = 3) # Index Plots of the influence measures
influencePlot(model.star, id.n = 3) # A user friendly representation of the above

# Regression after deleting the 77th observation
model.star1 <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend , data = startup[-50, ])
summary(model.star1)


### Variance Inflation Factors
vif(model.star)  # VIF is > 10 => collinearity

# Regression model to check R^2 on Independent variales
VIFRD <- lm(R.D.Spend ~ Administration + Marketing.Spend)
VIFAD <- lm(Administration ~R.D.Spend+Marketing.Spend)
VIFMR <- lm(Marketing.Spend~R.D.Spend+Administration)


summary(VIFRD)
summary(VIFAD)
summary(VIFMR)


#### Added Variable Plots ######
avPlots(model.star, id.n = 2, id.cex = 0.8, col = "red")

# Linear Model without WT
model.final <- lm(Profit ~ Administration+Marketing.Spend+R.D.Spend, data = startup)
summary(model.final)

# Linear model without WT and influential observation
model.final1 <- lm(Profit ~Administration+Marketing.Spend+R.D.Spend , data = startup[-50, ])
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
lm_best <- regsubsets(Profit ~ ., data = startup, nvmax = 15)
summary(lm_best)
?regsubsets
summary(lm_best)$adjr2
which.max(summary(lm_best)$adjr2)
coef(lm_best, 3)

lm_forward <- regsubsets(Profit ~ ., data = startup, nvmax = 15, method = "forward")
summary(lm_forward)

# Data Partitioning
n <- nrow(startup)
n1 <- n * 0.7
n2 <- n - n1
train <- sample(1:n, n1)
test <- startup[-train, ]

# Model Training
model <- lm(Profit ~ Administration+Marketing.Spend+R.D.Spend, startup[train, ])
summary(model)


pred <- predict(model, newdata = test)
actual <- test$Profit
error <- actual - pred

test.rmse <- sqrt(mean(error**2))
test.rmse

train.rmse <- sqrt(mean(model$residuals**2))
train.rmse

# Step AIC
install.packages("MASS")
library(MASS)
stepAIC(model.star)
