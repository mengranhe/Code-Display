#!/usr/local/bin/Rscript
source("tasks.R")
suppressMessages(library(argparse))

#create parser object
parser <- ArgumentParser()

parser$add_argument("--songname", default = NULL, type = "character", help = "Name of the song")

args <- parser$parse_args()

list_song_info(args$songname)