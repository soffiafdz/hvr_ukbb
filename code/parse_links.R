#!/usr/bin/env Rscript

library(here)
library(data.table)

# Read csv
l <- fread(here("lists/links_ukbb.csv"))

## 35_350 present: All good!
l[, .N, c("t1_source", "mask_source", "t1_link", "mask_link")]

## Create list of sub/visit
ls <- l[t1_link == 1 & mask_link == 1, .(sub, sess)]
fwrite(ls, here("lists/ukbb_list.lst"), col.names = FALSE)
