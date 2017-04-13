#!/usr/local/bin/Rscript
source("tasks.R")
suppressMessages(library(argparse))

#create parser object
parser <- ArgumentParser()

parser$add_argument("--snippet", default = NULL, type = "character", help = "file name of the snippet")

args <- parser$parse_args()

identify(args$snippet)
