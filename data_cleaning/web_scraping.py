import csv, pandas as pd, time, requests,urllib
from bs4 import BeautifulSoup

PhillyFedURL = 'https://www.philadelphiafed.org'

measures = ['corecpi',"corepce","unemp","rgdp"]
link_dict = dict.fromkeys(measures)
def getUpdateLink(measure):
    url = PhillyFedURL +'/surveys-and-data/'+ measure
    userAgent = {"User-agent":"Mozilla/5.0"}
    response = requests.get(url,headers=userAgent)
    soup = BeautifulSoup(response.text,"html.parser")
    #get the correct link for download
    body = soup.find_all("a",{'rel': "external"})
    link_dict[measure] = PhillyFedURL+body[0].get("href")
    time.sleep(5)
    return

for measure in measures:
    getUpdateLink(measure)


def createDataFramesFromURL(measure,url):
    xl_df = pd.read_excel(url,skiprows=8460,converters={"YEAR":pd.to_datetime})
    return xl_df

returned  = createDataFramesFromURL("unemp",link_dict["unemp"])



