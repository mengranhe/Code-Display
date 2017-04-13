#!/usr/local/bin/Rscript
suppressMessages(library(testthat))
####### first close all 16 connections to prevent failing connection ######
allconn <- dbListConnections(PostgreSQL())
for (con in allconn) {
  dbDisconnect(con)
}
source("tasks.R")

test_that("If adding a song which is already in the database, return error", {
  expect_error(add_song("Yellow", "Coldplay", 2000, "Yellow.wav"), "The song has been stored in the database")
})

test_that("Try to list song contents but the song doesn't exist in the database, return error", {
  expect_error(list_song_info("ABC"), "The song doesn't exist")
})

test_that("If the audio file is not identifiable, neither .mp3 nor .wav, return error", {
  expect_error(identify("ABC.mp4"), "invalid audio file")
})

test_that("If the snippet/song defined by user doesn't exist, cannot identify this snippet/song", {
  expect_error(identify("ABC.wav"), "File 'ABC.wav' does not exist.")
})

test_that("Test if song_info table exists in database", {
  song_info_database_check <- dbExistsTable(conn, "song_info")
  expect_true(song_info_database_check)
})

test_that("Test if song_data table exists in database", {
  song_data_check <- dbExistsTable(conn, "song_data")
  expect_true(song_data_check)
})

test_that("test if a song that user defined exists in song_info table and if it has signatures in song_data table",{
    songname <- "Closer"
    result <- list_song_info(songname)
    candidate_songname <- result$song_information[1,"song_name"]
    expect_equal(candidate_songname, songname) #check if user defined song is in the database
    
    expect_true(dim(result$signature)[1]>0)
    expect_true(dim(result$signature)[2]>=3) #columns contain song_id, window and at least one signature
  }
)

#this test takes some time, because snippet is entire song
test_that("identify snippet/song defined by user, return song information once found", {
  songfile <- "Yellow.wav" 
  songname <- sub("\\..*", "", songfile)  #cut off suffix ".wav"
  matchresult <- identify(songfile)[1,"song_name"]
  expect_equal(matchresult, songname)
})
