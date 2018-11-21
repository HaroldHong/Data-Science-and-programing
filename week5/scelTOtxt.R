library(devtools)
library(cidian)
library(stringi)
library(pbapply)
library(Rcpp)
library(RcppProgress)
dir.path <- "C:\\Users\\Harold\\Documents\\GitHub\\Data-Science-and-programing\\week5\\FinanceCorpusCH.scel"
decode_scel(scel = dir.path,
            output = paste0(dir.path,".txt"),
            cpp = TRUE,
            progress = TRUE)
