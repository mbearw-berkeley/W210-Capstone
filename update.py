import rpy2.robjects as robjects,sys

sys.path.insert(0,"scripts")

import UpdateSPF
import CPI_cleaning
import PCE_cleaning
import GDP_cleaning
import Unemp_cleaning


r = robjects.r
r['source']('data_cleaning/preprocess.R')

import AddPrices

print("Data update completed! ")
