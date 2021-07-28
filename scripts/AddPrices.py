
import pandas as pd,time,numpy as np,os,json,requests,prettytable
from datetime import date


item_dict = {}
item_dict["unleadedGasoline"] = "APU000074714"
item_dict["milk"] = "APU0000709112"
item_dict["bread"] = "APU0000702111"
item_dict["eggs"] = "APU0000708111"
item_dict["bacon"] = "APU0000704111"
item_dict["electricity"] = "APU000072610"
item_dict["rice"] = "APU0000701312"
item_dict["flour"] = "APU0000701111"
item_dict["chicken"] = "APU0000706111"
item_dict["banana"] = "APU0000711211"
item_dict["coffee"] = "APU0000717311"
item_dict["beef"] = "APU0000703112"
item_dict["iceCream"] = "APU0000710411" 

def update_items(filename):
    data = pd.read_csv('./data/RawData/BLS/'+filename)
    seriesID = item_dict[filename.split(".")[0]]
    data = data[["Year","Period","Value"]]
    new = get_items(filename.split(".")[0],seriesID)
    merged = data.merge(new,how="outer")
    return merged

def get_items(item_name,ID):
    headers = {'Content-type': 'application/json'}
    data = json.dumps({"seriesid": [ID],"startyear":"2020", "endyear":date.today().year+1})
    p = requests.post('https://api.bls.gov/publicAPI/v2/timeseries/data/', data=data, headers=headers)
    json_data = json.loads(p.text)
    returnFrame = pd.DataFrame(json_data["Results"]["series"][0]["data"])
    returnFrame.rename(columns={"year":"Year","period":"Period","value":"Value"},inplace=True)
    returnFrame["Year"] = returnFrame["Year"].astype(int)
    returnFrame["Value"] = returnFrame["Value"].astype(float)
    return returnFrame
# for filename in os.listdir("../data/RawData/BLS"):
#     print(filename.split(".")[0])



def toQuarter(month):
    if month>=1 and month<=3:
        return 1
    elif month > 3 and month <=6:
        return 2
    elif month > 6 and month <= 9:
        return 3
    else: 
        return 4



def formatData(filename):
    data = pd.read_csv('./data/RawData/BLS/'+filename,usecols=["Year","Period","Value"])
    
    data.Period = data.Period.apply(lambda x: x[1:]).astype("int")
    data["Quarter"] = data.Period.apply(toQuarter)
    data = data.drop(columns=["Period"])
    data = data.groupby(["Year","Quarter"]).mean()
    return data

def combine(df,left_on_col = ["YEAR FORECAST MADE","QUARTER"]):
    returnFrame = df.copy()
    for filename in os.listdir("./data/RawData/BLS"):
        to_merge = formatData(filename).reset_index(level=[0,1])
        to_merge.rename(columns={"Value":filename.split(".")[0]},inplace=True)
        returnFrame = returnFrame.merge(to_merge,how="left",left_on=left_on_col,right_on=["Year","Quarter"],suffixes=('', '_drop')).fillna(method="ffill")
        #returnFrame = returnFrame.merge(to_merge,how="left",left_on=left_on_col,right_on=["Year","Quarter"],suffixes=('', '_drop')).fillna(method="ffill")
    returnFrame = returnFrame[returnFrame.columns.drop(returnFrame.filter(regex='drop').columns)]
    return returnFrame.drop(columns=["Year","Quarter"])
 






def main():


    for filename in os.listdir("./data/RawData/BLS"):
        name = filename.split(".")[0]
        print(name)
        df = update_items(filename)
        df.to_csv("./data/RawData/BLS/"+name+".csv",index=False)    
        time.sleep(5)

    training = pd.read_csv("./data/TrainingData/fulldata.csv")

    newTraining = combine(training)
    newTraining.to_csv("./data/TrainingData/trainingWithItems.csv",index=False)
    print("Completed Adding Prices to Training Data! ")



main()






# quarterFred= pd.read_csv("../data/CleanData/QuarterlyFred.csv",index_col=0)
# quarterFred["DATE"] = quarterFred["DATE"].apply(lambda x: datetime.strptime(x, '%Y-%m-%d'))
# quarterFred["Year"] = quarterFred["DATE"].apply(lambda x:x.year)
# quarterFred["Month"] = quarterFred["DATE"].apply(lambda y:y.month)
# quarterFred["Quarter"] = quarterFred["Month"].apply(toQuarter)
# quarterFred.drop(columns=["Month"],inplace=True)
# quarterFred.head()


# # In[97]:


# newQuarterly = combine(quarterFred,["Year","Quarter"])
# newQuarterly.head()
# #newQuarterly.to_csv("../data/CleanData/QuarterlyFredWithItems.csv",index=False)


# # In[ ]:




