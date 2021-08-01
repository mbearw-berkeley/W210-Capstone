
import pandas as pd
from sklearn.metrics import r2_score
from sklearn.base import clone
import numpy as np
import matplotlib.pyplot as plt
import os
import seaborn as sns
from sklearn.model_selection import train_test_split,GridSearchCV
#from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from xgboost import XGBRegressor
#from sklearn.neighbors import KNeighborsRegressor




from RegressionModel import RegressionModel

train=pd.read_csv("../data/TrainingData/train.csv")
test=pd.read_csv("../data/TestData/test.csv")
val=pd.read_csv("../data/ValidationData/val.csv")

d = RegressionModel("Random Forest Regressor",RandomForestRegressor())
d.get_predictions("Unemployment")