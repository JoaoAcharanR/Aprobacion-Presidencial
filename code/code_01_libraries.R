#####Cargar librerias####

library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)
library(tidyr)
library(ggrepel)

#####Cargar datos#####

encuestas <- read_xlsx("raw/encuestas.xlsx")

