import pandas as pd,numpy as np
from sklearn.model_selection import train_test_split
def main():
	data = pd.read_csv("./data/CleanedData/fullData.csv")

	relevant = data[data["YEAR FORECAST MADE"]>=1992][['YEAR BEING FORECAST', 'INDICATOR', 'actual', 'MAX', 'ACTUAL_CONF',
       'HIT', 'pred_average', 'pred_var', 'banana', 'beef', 'bread', 'chicken',
       'coffee', 'eggs', 'electricity', 'flour', 'iceCream', 'rice',
       'unleadedGasoline']]
	relevant.dropna(inplace=True)

	train,test = train_test_split(relevant,test_size=.2,random_state=42,stratify=relevant["INDICATOR"])
	train,val = train_test_split(train,test_size=0.25, random_state=42,stratify = train["INDICATOR"])
	train.to_csv("./data/TrainingData/train.csv",index=False)
	test.to_csv("./data/TestData/test.csv",index=False)
	val.to_csv("./data/ValidationData/val.csv",index=False)
main()