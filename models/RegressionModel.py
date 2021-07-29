import pandas as pd
from sklearn.metrics import r2_score
import numpy as np
import matplotlib.pyplot as plt
import os
import seaborn as sns
from sklearn.model_selection import train_test_split,GridSearchCV





def MeasureSplit(df,measure):
    return df[df["INDICATOR"]==measure].drop(columns = ["actual","INDICATOR","YEAR BEING FORECAST"]),df[df["INDICATOR"]==measure]["actual"]



class RegressionModel:
    measures = ["Unemployment","RealGDP","Core CPI","Core PCE"]
    
    def __init__(self,name,model):
        self.name = name
        self.model = model
        self.rmse_scores = []
        self.optimized_rmse_scores = []
        self.optimized_r2_scores = []
        self.r2_scores = []
        self.validation = True
        self.best_models = dict.fromkeys(self.measures,None)
        self.optimized=False
        self.store_scores()  
    
    def set_validation(setting):
        self.validation=setting
        
    def clear_scores(self):
        self.rmse_scores = []
        self.r2_scores = []
        
    def store_scores(self):
        if self.optimized==True:
            rmse = self.optimized_rmse_scores
            r2 = self.optimized_r2_scores
        else:
            rmse = self.rmse_scores
            r2 = self.r2_scores
        for measure in self.measures:
            if self.validation:
                X_test,y_test = MeasureSplit(val,measure)
            else: 
                X_test,y_test = MeasureSplit(test,measure)
            predictions = self.get_predictions(measure)
            rmse.append(np.sqrt(np.mean((y_test-predictions)**2)))
            r2.append(r2_score(y_test,predictions))
        return 
    
    def plot_scores(self):
        fig,axes = plt.subplots(2,1,figsize=(10,20))
        for i,score,s in zip([0,1],['RMSE Scores','R^2 Scores'],[self.rmse_scores,self.r2_scores]):
            sns.barplot(x=self.measures,y=s,ax=axes[i])
            axes[i].set_title(score)
            axes[i].set_xlabel("Economic Measures")
            axes[i].set_ylabel("Score")
        return plt.show()
    
    def get_predictions(self,variable_name):
        #calculates and stores scores
        #returns predictions for specified measure
        if self.optimized==True:
            model = self.best_models[variable_name]
        else:
            model = self.model
        X_train,y_train = MeasureSplit(train,variable_name)
        if self.validation:
            X_test,y_test = MeasureSplit(val,variable_name)
        else: 
            X_test,y_test = MeasureSplit(test,variable_name)
        model.fit(X_train,y_train)
        return model.predict(X_test)
    
    def create_results(self):
        data = {}
        for measure in self.measures:
            data[measure] = self.get_predictions(measure)
        return pd.DataFrame(dict([ (self.name+" "+k,pd.Series(v)) for k,v in data.items()]))
    
    def annual_performance(self, variable_name, measure, plot=True):
        X_train,y_train = MeasureSplit(train,variable_name)
        if self.validation:
            X_test,y_test = MeasureSplit(val,variable_name)
        else: 
            X_test,y_test = MeasureSplit(test,variable_name)
            
        preds = self.get_predictions(measure)
        actuals = val[val["INDICATOR"]==measure]['actual']
        spf_preds = val[val["INDICATOR"]==measure]['pred_average']
        years = val[val["INDICATOR"]==measure]['YEAR BEING FORECAST']
        frame = pd.DataFrame({"Actual": actuals, "SPF": spf_preds, "Model": preds, "Year": years})
        grouped = frame.groupby("Year").mean()
        
        if plot == True:
            plt.plot(grouped['Actual'])
            plt.plot(grouped['SPF'])
            plt.plot(grouped['Model'])
            labels = ["Actual", "SPF", "Model"]
            plt.legend(labels)
            plt.show()
            
        return grouped
    
    def perform_gridsearch(self,parameters):
        
        def find_optimal_model(variable_name):
            X_train,y_train = MeasureSplit(train,variable_name)
            gridsearch = GridSearchCV(estimator=self.model,param_grid = parameters,scoring='neg_root_mean_squared_error',n_jobs=5,cv=5,verbose=True)
            gridsearch.fit(X_train,y_train)
            return gridsearch.best_estimator_
        
        for measure in self.measures:    
            self.best_models[measure]=find_optimal_model(measure)
        self.optimized=True
        return


def main():
