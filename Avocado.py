

# Multilinear Regression
import pandas as pd
import numpy as np

# loading the data
Avocado = pd.read_csv("C:/Users/Bibhuti/OneDrive/Desktop/360digiTMG assignment/MLR/Avacado_Price.csv",encoding='unicode_escape')
Avocado.info()
Avocado.rename(columns={'AveragePrice':'Price','Total_Volume':'Volume','tot_ava1':'ava1','tot_ava2':'ava2','tot_ava3':'ava3'},inplace=True)

# Exploratory data analysis:
# 1. Measures of central tendency
# 2. Measures of dispersion
# 3. Third moment business decision
# 4. Fourth moment business decision
# 5. Probability distributions of variables 
# 6. Graphical representations (Histogram, Box plot, Dot plot, Stem & Leaf plot, Bar plot, etc.)

Avocado.describe()

#Graphical Representation
import matplotlib.pyplot as plt # mostly used for visualization purposes 

# Total_bags
plt.bar(height = Avocado.Total_Bags, x = np.arange(1, 18250, 1))
plt.hist(Avocado.Total_Bags) #histogram
plt.boxplot(Avocado.Total_Bags) #boxplot

# Price
plt.bar(height = Avocado.Price, x = np.arange(1, 18250, 1))
plt.hist(Avocado.Price) #histogram
plt.boxplot(Avocado.Price) #boxplot

# Jointplot
import seaborn as sns
sns.jointplot(x=Avocado['Volume'], y=Avocado['Price'])
sns.jointplot(x=Avocado['ava1'], y=Avocado['Price'])
sns.jointplot(x=Avocado['ava2'], y=Avocado['Price'])
sns.jointplot(x=Avocado['ava3'], y=Avocado['Price'])
sns.jointplot(x=Avocado[' Total_Bags'], y=Avocado['Price'])

# Countplot
plt.figure(1, figsize=(16, 10))
sns.countplot(Avocado['Volume'])

# Q-Q Plot
from scipy import stats
import pylab
stats.probplot(Avocado.Price, dist = "norm", plot = pylab)
plt.show()
                             
# Correlation matrix 
Avocado.corr()
# preparing model considering all the variables 
import statsmodels.formula.api as smf # for regression model
         
ml1 = smf.ols('Price ~Volume+ava1+ava2+ava3+Total_Bags', data = Avocado).fit() # regression model

# Summary
ml1.summary()
# p-values for Doors are more than 0.05

# Checking whether data has any influential values 
# Influence Index Plots
import statsmodels.api as sm

sm.graphics.influence_plot(ml1)
# Studentized Residuals = Residual/standard deviation of residuals
# index 221 is showing high influence so we can exclude that entire row


# Preparing model                  
ml_new = smf.ols('Price ~ Volume+ava1+ava2+ava3+Total_Bags ', data = Avocado).fit()    

# Summary
ml_new.summary()

# Check for Colinearity to decide to remove a variable using VIF
# Assumption: VIF > 10 = colinearity
# calculating VIF's values of independent variables
#Price ~Volume+ava1+ava2+ava3+Total_Bags
rsq_vol = smf.ols('Volume ~ ava1+ava2+ava3+Total_Bags', data = Avocado).fit().rsquared  
vif_vol = 1/(1 - rsq_vol) 

rsq_1= smf.ols('ava1 ~ Volume+ava2+ava3+Total_Bags', data = Avocado).fit().rsquared  
vif_1 = 1/(1 - rsq_1)

rsq_2 = smf.ols('ava2 ~ Volume+ava1+ava3+Total_Bags', data = Avocado).fit().rsquared  
vif_2 = 1/(1 - rsq_2)

rsq_3 = smf.ols('ava3 ~ Volume+ava1+ava2+Total_Bags', data = Avocado).fit().rsquared  
vif_3 = 1/(1 - rsq_3) 

rsq_bags = smf.ols('Total_Bags~ Volume+ava1+ava2+ava3', data = Avocado).fit().rsquared  
vif_bags = 1/(1 - rsq_bags) 

# Storing vif values in a data frame
d1 = {'Variables':['Volume','ava1','ava2','ava3','Total_Bags'], 'VIF':[vif_vol, vif_1,vif_2, vif_3, vif_bags]}
Vif_frame = pd.DataFrame(d1)  
Vif_frame
# As WT is having highest VIF value, we are going to drop this from the prediction model

# Final model
final_ml = smf.ols('Price ~Volume+ava1+ava2+ava3+Total_Bags ', data = Avocado).fit()
final_ml.summary() 

# Prediction
pred = final_ml.predict(Avocado)

# Q-Q plot
res = final_ml.resid
sm.qqplot(res)
plt.show()

# Q-Q plot
stats.probplot(res, dist = "norm", plot = pylab)
plt.show()

# Residuals vs Fitted plot
sns.residplot(x = pred, y = Avocado.Price, lowess = True)
plt.xlabel('Fitted')
plt.ylabel('Residual')
plt.title('Fitted vs Residual')
plt.show()

sm.graphics.influence_plot(final_ml)


### Splitting the data into train and test data 
from sklearn.model_selection import train_test_split
Avocado_train, Avocado_test = train_test_split(Avocado, test_size = 0.2) # 20% test data

# preparing the model on train data 
model_train = smf.ols("Price ~ Volume+ava1+ava2+ava3+Total_Bags", data = Avocado_train).fit()

# prediction on test data set 
test_pred = model_train.predict(Avocado_test)

# test residual values 
test_resid = test_pred - Avocado_test.Price
# RMSE value for test data 
test_rmse = np.sqrt(np.mean(test_resid * test_resid))
test_rmse


# train_data prediction
train_pred = model_train.predict(Avocado_train)

# train residual values 
train_resid  = train_pred - Avocado_train.Price
# RMSE value for train data 
train_rmse = np.sqrt(np.mean(train_resid * train_resid))
train_rmse



