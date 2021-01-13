NIPSdata.R : Rcode for data preprocessing 
NIPS.RData : Original dataset from data preprocessing 
             tnsNIPS: a tensor whose tensor size = (2482,14036,17), tensor mode = (papers, words, years)
             NIPS: a dataframe which has the same information with tnsNIPS
             papers,words,years: name of the tensor dimension
             
rNIPS.RData: Reduced dataset from NIPS.RData
             rtnsNIPS: a tensor whose tensor size = (510,500,17), tensor mode = (papers, words, years)
             rwords,rpapers,ryears: name of the tensor dimension
