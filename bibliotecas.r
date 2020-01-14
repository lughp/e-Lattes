library(tinytex)
library(tidyverse)
library(jsonlite)
library(listviewer)
library(scales)
library(igraph)

#Definir o local onde est?oo os arquivos json
setwd("C:/refinal")

#Arquivo com funcionalidades que transformam o arquivo formato list em DataFrames
source("elattes.ls2df.R")