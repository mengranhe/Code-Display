#!/usr/local/bin/Rscript
source("tasks.R")
suppressMessages(library(argparse))

#create parser object
parser <- ArgumentParser()

parser$add_argument("--songname", default = NULL, type = "character", help = "Name of the song")
parser$add_argument("--artist", default = NULL, type = "character", help = "Singer of the song")
parser$add_argument("--year", default = NULL, type = "integer", help = "release year of the song")
parser$add_argument("--file", default = NULL, type = "character", help = "file name of the song")
parser$add_argument("--numPeak", default = 10, type = "integer", help = "Number of signatures generated from each window of the song")
parser$add_argument("--snippet", default = NULL, type = "character", help = "file name of the snippet")
args <- parser$parse_args()

add_song(args$songname, args$artist, args$year, args$file, args$numPeak)


