library(dplyr)
library(bigrquery)
library(ggplot2)
library(tidyr)
library(flextable)
library(shiny)
library(plotly)
library(readr)
projectid = "petlove-dataeng-prod-01"

query <- readChar("/home/matheus/matheuspetlove/Threshold/query_threshold.txt", file.info("/home/matheus/matheuspetlove/Threshold/query_threshold.txt")$size)

tb <- bq_project_query(projectid,query)
df = bq_table_download(tb) %>% data.frame()
