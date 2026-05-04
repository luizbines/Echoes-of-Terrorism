# This code identifies which localities in Israel are within or intersecting the West Bank.

# Library
library(sf)
library(dplyr)

# Directory
wd = '/home/luiz/Documentos/GitHub/Echoes-of-Terrorism/Voting/'
setwd(wd);

israel_panel = read.csv('cleaning/Israel/Output/3_israel_panel_west_bank.csv')