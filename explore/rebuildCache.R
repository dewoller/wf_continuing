rm(list=ls())
options(width = 200)
show_code = FALSE
setwd("~/mydoc/research/mofi/continuing/R")
source("functions.R")
source("get_data.R")
source("generate_data_frames.R")
system("rm -rf cache/*")
data.cache( generate_data_frames, frequency='yearly')
system("notify-send 'done rebuilding cache'")

