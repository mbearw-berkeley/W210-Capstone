import csv, pandas as pd, time, requests,urllib,datetime,openpyxl,mock
from openpyxl.reader import excel
from bs4 import BeautifulSoup
import warnings

warnings.filterwarnings('ignore')

actualsGDPURL = "https://fred.stlouisfed.org/graph/fredgraph.xls?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=748&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDPC1&scale=left&cosd=1947-01-01&coed=2021-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Annual&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=pch&vintage_date=2021-07-26&revision_date=2021-07-26&nd=1947-01-01"
actualsUnempURL = "https://fred.stlouisfed.org/graph/fredgraph.xls?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=UNRATE&scale=left&cosd=1948-01-01&coed=2021-06-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2021-07-26&revision_date=2021-07-26&nd=1948-01-01"
actualsCCPIURL = "https://fred.stlouisfed.org/graph/fredgraph.xls?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPILFESL&scale=left&cosd=1957-01-01&coed=2021-06-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2021-07-26&revision_date=2021-07-26&nd=1957-01-01"
actualsCPCEURL = "https://fred.stlouisfed.org/graph/fredgraph.xls?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=PCEPILFE&scale=left&cosd=1959-01-01&coed=2021-05-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2021-07-26&revision_date=2021-07-26&nd=1959-01-01"
actualsURLs = [actualsGDPURL,actualsUnempURL,actualsCCPIURL,actualsCPCEURL]


def getUpdateLink():
    url = 'https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/individual-forecasts'
    userAgent = {"User-agent":"Mozilla/5.0"}
    response = requests.get(url,headers=userAgent)
    soup = BeautifulSoup(response.text,"html.parser")
    #get the correct link for download
    body = soup.find_all("a",class_="")
    link = 'https://www.philadelphiafed.org'+body[8].get("href")
    return link 

def createDataFramesFromURL(measure,url):
    with mock.patch.object(excel.ExcelReader, 'read_properties', lambda self: None):
        xl_df =  pd.read_excel(url,sheet_name=measure, dtype={'YEAR': str, 'QUARTER': str})
    return xl_df

def saveActuals(measure,url):
    output = "./data/RawData/ScrapedData/Actuals/"+measure+".xls"
    urllib.request.urlretrieve(url,output)
    return

def main():
    print("Pulling new SPF survey data~")
    microSPF = getUpdateLink()

    for measure in ["PRGDP","PRUNEMP","PRCCPI","PRCPCE"]:
        export = createDataFramesFromURL(measure,microSPF)
        export.to_csv("./data/RawData/ScrapedData/"+measure+".csv",index=False)

    for url,measure in zip(actualsURLs,["GDP","UNEMP","CCPI","CPCE"]):
        saveActuals(measure,url)
    print("Completed pulling new SPF data!")





main()
# edit R page to update files in directory in place 
# append price data to training 
# retrain model on new data 
# produce predictions vector that we use for final visualizations
# make Yaml file
# Flask server
# 
# 
