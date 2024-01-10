# title:    pre_proc
# purpose:  Pre-process ADaM datasets, to be used seamlessly in PowerBI
# Author:   Malan Bosman
# Date:     11JUL2023

# 1. Converts NA to null for numerics and consumption by PowerBI
# 2. 

# load libraries -----------------------------------------------------------
library(tidyverse)
library(haven)


csv_path <- "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_csv/"

# NA to null for Numerics -------------------------------------------------

library(readr)

ADADAS = read.csv(paste0(csv_path,"ADADAS.csv"))
ADSL = read.csv(paste0(csv_path,"ADSL.csv"))
ADAE = read.csv(paste0(csv_path,"ADAE.csv"))
ADCIBC = read.csv(paste0(csv_path,"ADCIBC.csv"))
ADLBC = read.csv(paste0(csv_path,"ADLBC.csv"))
ADLBCPV = read.csv(paste0(csv_path,"ADLBC.csv"))
ADLBH <- read.csv(paste0(csv_path,"ADLBH.csv"))
ADLBHPV <- read.csv(paste0(csv_path,"ADLBHPV.csv"))
ADLBHY <- read.csv(paste0(csv_path,"ADLBHY.csv"))
ADNPIX <- read.csv(paste0(csv_path,"ADNPIX.csv"))
ADTTE <- read.csv(paste0(csv_path,"ADTTE.csv"))
ADVS <- read.csv(paste0(csv_path,"ADVS.csv"))

# function to convert NA to null in numeric variables

NA_to_null <- function(ds){
  df <- ds %>% 
    #select(where(is.numeric)) %>% 
    mutate_all(~replace(., is.na(.), "null")) # replace NA
  
  arg_name <- deparse(substitute(ds)) # Get argument name
  var_name <- paste(arg_name, "NEW", sep="_") # Construct the name
  assign(var_name, df, env=.GlobalEnv) # Assign values to variable
  }

NA_to_null(ADADAS)
NA_to_null(ADSL)
NA_to_null(ADAE)
NA_to_null(ADCIBC)
NA_to_null(ADLBC)
NA_to_null(ADLBCPV)
NA_to_null(ADLBH)
NA_to_null(ADLBHPV)
NA_to_null(ADLBHY)
NA_to_null(ADNPIX)
NA_to_null(ADTTE)
NA_to_null(ADVS)


# write out ---------------------------------------------------------------
csv_path_new <- "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/New_csv/"

write_csv(ADADAS_NEW, paste0(csv_path_new,"ADADAS.csv"))
write_csv(ADSL_NEW, paste0(csv_path_new,"ADSL.csv"))
write_csv(ADAE_NEW, paste0(csv_path_new,"ADAE.csv"))
write_csv(ADCIBC_NEW, paste0(csv_path_new,"ADCIBC.csv"))
write_csv(ADLBC_NEW, paste0(csv_path_new,"ADLBC.csv"))
write_csv(ADLBC_NEW, paste0(csv_path_new,"ADLBC.csv"))
write_csv(ADLBH_NEW, paste0(csv_path_new,"ADLBH.csv"))
write_csv(ADLBHPV_NEW, paste0(csv_path_new,"ADLBHPV.csv"))
write_csv(ADLBHY_NEW, paste0(csv_path_new,"ADLBHY.csv"))
write_csv(ADNPIX_NEW, paste0(csv_path_new,"ADNPIX.csv"))
write_csv(ADTTE_NEW, paste0(csv_path_new,"ADTTE.csv"))
write_csv(ADVS_NEW, paste0(csv_path_new,"ADVS.csv"))
  
dup <- ADSL %>% 
  group_by(USUBJID) %>% 
  filter(n()>1)
  
  