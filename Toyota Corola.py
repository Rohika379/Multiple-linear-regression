
# Multilinear Regression
import pandas as pd
import numpy as np

# loading the data
cars = pd.read_csv("C:/Users/Bibhuti/OneDrive/Desktop/360digiTMG assignment/MLR/ToyotaCorolla.csv",encoding='unicode_escape')
cars.info()
cars.rename(columns={'Age_08_04':'Age'},inplace=True)

# Exploratory data analysis:
# 1. Measures of central tendency
# 2. Measures of dispersion
# 3. Third moment business decision
# 4. Fourth moment business decision
# 5. Probability distributions of variables 
# 6. Graphical representations (Histogram, Box plot, Dot plot, Stem & Leaf plot, Bar plot, etc.)

cars.describe()

#Graphical Representation
import matplotlib.pyplot as plt # mostly used for visualization purposes 

# Age
plt.bar(height = cars.Weight, x = np.arange(1, 1437, 1))
plt.hist(cars.Weight) #histogram
plt.boxplot(cars.Weight) #boxplot

# Price
plt.bar(height = cars.Price, x = np.arange(1, 1437, 1))
plt.hist(cars.Price) #histogram
plt.boxplot(cars.Price) #boxplot

# Jointplot
import seaborn as sns
sns.jointplot(x=cars['Age'], y=cars['Price'])
sns.jointplot(x=cars['KM'], y=cars['Price'])
sns.jointplot(x=cars['cc'], y=cars['Price'])
sns.jointplot(x=cars['Weight'], y=cars['Price'])
sns.jointplot(x=cars['Doors'], y=cars['Price'])
sns.jointplot(x=cars['Quarterly_Tax'], y=cars['Price'])
# Countplot
plt.figure(1, figsize=(16, 10))
sns.countplot(cars['Age'])

# Q-Q Plot
from scipy import stats
import pylab
stats.probplot(cars.Price, dist = "norm", plot = pylab)
plt.show()
                             
# Correlation matrix 
cars.corr()
# preparing model considering all the variables 
import statsmodels.formula.api as smf # for regression model
         
ml1 = smf.ols('Price ~Age+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight', data = cars).fit() # regression model

# Summary
ml1.summary()
# p-values for Doors are more than 0.05

# Checking whether data has any influential values 
# Influence Index Plots
import statsmodels.api as sm

sm.graphics.influence_plot(ml1)
# Studentized Residuals = Residual/standard deviation of residuals
# index 221 is showing high influence so we can exclude that entire row

cars_new = cars.drop(cars.index[[601,221,960]])

# Preparing model                  
ml_new = smf.ols('Price ~ Age+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight', data = cars_new).fit()    

# Summary
ml_new.summary()

# Check for Colinearity to decide to remove a variable using VIF
# Assumption: VIF > 10 = colinearity
# calculating VIF's values of independent variables
#Price ~Age+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight
rsq_age = smf.ols('Age~KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight', data = cars).fit().rsquared  
vif_age = 1/(1 - rsq_age) 

rsq_km = smf.ols('KM ~ Age+HP+cc+Doors+Gears+Quarterly_Tax+Weight', data = cars).fit().rsquared  
vif_km = 1/(1 - rsq_km)

rsq_hp = smf.ols('HP ~ Age+KM+cc+Doors+Gears+Quarterly_Tax+Weight', data = cars).fit().rsquared  
vif_hp = 1/(1 - rsq_hp)

rsq_cc = smf.ols('cc ~ Age+KM+HP+Doors+Gears+Quarterly_Tax+Weight', data = cars).fit().rsquared  
vif_cc = 1/(1 - rsq_cc) 

rsq_doors = smf.ols('Doors~ Age+KM+HP+cc+Gears+Quarterly_Tax+Weight', data = cars).fit().rsquared  
vif_doors = 1/(1 - rsq_doors) 

rsq_gears = smf.ols('Gears ~ Age+KM+HP+cc+Doors+Quarterly_Tax+Weight', data = cars).fit().rsquared  
vif_gears = 1/(1 - rsq_gears) 

rsq_quar = smf.ols('Quarterly_Tax ~ Age+KM+HP+cc+Doors+Gears+Weight', data = cars).fit().rsquared  
vif_quar = 1/(1 - rsq_quar)

rsq_weight = smf.ols('Weight ~ Age+KM+HP+cc+Doors+Gears+Quarterly_Tax', data = cars).fit().rsquared  
vif_weight = 1/(1 - rsq_weight) 

# Storing vif values in a data frame
d1 = {'Variables':['Age','KM','HP','cc','Doors','Gears','Quarterly_Tax','Weight'], 'VIF':[vif_age, vif_km,vif_hp, vif_cc, vif_doors,vif_gears,vif_quar,vif_weight]}
Vif_frame = pd.DataFrame(d1)  
Vif_frame
# As WT is having highest VIF value, we are going to drop this from the prediction model

# Final model
final_ml = smf.ols('Price ~ Age+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight', data = cars).fit()
final_ml.summary() 

# Prediction
pred = final_ml.predict(cars)

# Q-Q plot
res = final_ml.resid
sm.qqplot(res)
plt.show()

# Q-Q plot
stats.probplot(res, dist = "norm", plot = pylab)
plt.show()

# Residuals vs Fitted plot
sns.residplot(x = pred, y = cars.Price, lowess = True)
plt.xlabel('Fitted')
plt.ylabel('Residual')
plt.title('Fitted vs Residual')
plt.show()

sm.graphics.influence_plot(final_ml)


### Splitting the data into train and test data 
from sklearn.model_selection import train_test_split
cars_train, cars_test = train_test_split(cars, test_size = 0.3) # 20% test data

# preparing the model on train data 
model_train = smf.ols("Price ~ Age+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight", data = cars_train).fit()

# prediction on test data set 
test_pred = model_train.predict(cars_test)

# test residual values 
test_resid = test_pred - cars_test.Price
# RMSE value for test data 
test_rmse = np.sqrt(np.mean(test_resid * test_resid))
test_rmse


# train_data prediction
train_pred = model_train.predict(cars_train)

# train residual values 
train_resid  = train_pred - cars_train.Price
# RMSE value for train data 
train_rmse = np.sqrt(np.mean(train_resid * train_resid))
train_rmse

