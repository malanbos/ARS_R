# title:    xpt_to_csv
# purpose:  create a data model consisting of ADaM datasets, and load into PBI for data visualisation.
# Author:   Malan Bosman
# Date:     09MAY2023

# Load libraries
library(tidyverse)
library(haven)

#paths
xpt_path <- "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_xpt/"
csv_path <- "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_csv/"

# import ADaM datasets
ADADAS = read_xpt("C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_xpt/adadas.xpt")
ADAE = read_xpt("C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_xpt/adae.xpt")
ADCIBC = read_xpt("C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_xpt/adcibc.xpt")
ADLBC = read_xpt("C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_xpt/adlbc.xpt")
ADLBCPV = read_xpt("C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_xpt/adlbcpv.xpt")
ADLBH = read_xpt("C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_xpt/adlbh.xpt")
ADLBHPV = read_xpt("C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_xpt/adlbhpv.xpt")
ADLBHY = read_xpt("C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_xpt/adlbhy.xpt")
ADNPIX = read_xpt("C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_xpt/adnpix.xpt")
ADSL = read_xpt("C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_xpt/adsl.xpt")

ADTTE = read_xpt("C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_xpt/adtte.xpt")
ADVS = read_xpt("C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_xpt/advs.xpt")

# write to csv
write_csv(ADSL, "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_csv/ADSL.csv")
write_csv(ADAE, "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_csv/ADAE.csv")
write_csv(ADADAS, "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_csv/ADADAS.csv")
write_csv(ADCIBC, "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_csv/ADCIBC.csv")
write_csv(ADLBC, "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_csv/ADLBC.csv")
write_csv(ADLBCPV, "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_csv/ADLBCPV.csv")
write_csv(ADLBH, "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_csv/ADLBH.csv")
write_csv(ADLBHPV, "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_csv/ADLBHPV.csv")
write_csv(ADLBHY, "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_csv/ADLBHY.csv")
write_csv(ADNPIX, "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_csv/ADNPIX.csv")
write_csv(ADTTE, "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_csv/ADTTE.csv")
write_csv(ADVS, "C:/Users/bosmanm/Desktop/PowerBI/Project_CDISC/ADaM datasets/Original_csv/ADVS.csv")
