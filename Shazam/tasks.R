#!/usr/local/bin/Rscript

source("psqlConnet.R") #load library(RPostgreSQL) and log into server, connection name is "conn"
source("signature.R")
source("matching.R")

############################################
#Task 1: song analyzer, add new song schema#
############################################
#1. check if song_info table exists in database, if not, create table.
#2. insert song information into song_info table
#3. read audio file, return its window centers and signatures, and insert signatures into song_data table

add_song <- function(songname, artist, year, file, numPeak = 10) {
  #check if song_info table exist in database, return true if table song_info exists in database
  database_check <- dbExistsTable(conn, "song_info")
  
  if (isTRUE(database_check)) {
    #insert song information (store song_name, singer, release year) into table "song_info"
    song_info_text <- paste(c("'", songname, "', ", "'", artist, "',", year), collapse = "")
    song_name_text <- paste0("'", songname, "'", collapse = "") #create text string '<songname>'
    
    #check if the song is already present, if true, stop adding information to the table, if false insert song information into table
    info_check_text <- paste("select exists (select * from song_info where song_name like", song_name_text,");", collapse = "") 
    info_check <- dbGetQuery(conn, info_check_text) 
    if (isTRUE(info_check[[1]])) {
      stop("The song has been stored in the database")
    }
    else {
      psql_info_text <- paste("insert into song_info (song_name, singer, song_year) values(",song_info_text,");",collapse = "")
      dbSendQuery(conn, psql_info_text) #execute sql command
    } 
    
    #create dataframe for each song, containing windows and corresponding signatures
    #call song_signature to calculate signatures of song, the number of peaks for each window = 10 by default
    result <- song_signature(file, numPeak)
    #create command line to find out song id
    psql_id_text <- paste("select song_id from song_info where song_name like", song_name_text,";",collapse = "")
    id <- dbGetQuery(conn, psql_id_text)[[1]] #return song id
    
    #make dataframe in R called "song_data"
    song_data <- data.frame(cbind(rep(id, nrow(result$signature)), seq(1:nrow(result$signature)), result$signature))
    names(song_data) <- c("song_id", "window", paste("signature", 1:numPeak, sep = ""))
    #append new song's signatures to existing table
    dbWriteTable(conn, "song_data", value = song_data, append = TRUE, row.names = FALSE)
  }
  
  else { #if song_info doesn't exist, create new table named "song_info"
    dbSendQuery(conn, "create table song_info (song_id serial primary key, song_name text, singer text, song_year int);")
  }
} 

# add_song("Closer", "The Chainsmokers", 2016, "Closer.wav")
# add_song("Scientist", "Coldplay", 2002,"Scientist.wav")
# add_song("Send My Love", "Adele", 2016, "Send My Love.wav")
# add_song("Starving","2Hailee Steinfeld",2015, "Starving.wav")
# add_song("Viva La Vida","Coldplay", 2008, "Viva La Vida.wav")
# add_song("We Dont Talk Anymore", "Charlie Puth", 2016, "We Dont Talk Anymore.wav")
# add_song("Yellow", "Coldplay", 2000, "Yellow.wav")


################################################
#Task 2: list the contents of the song database#
################################################
#1. use psql to locate song information and return song information from batabase
#2. locate song's signatures and window centers by song id and return song signature from batabase
#3. by calling list_song_info() function, you will get song id,
#name of the song, artisit, release year of song, its windows and signatures

list_song_info <- function(songname){ 
  title_text <- paste0("'", songname, "'", collapse= "") #create text string '<songname>'
  song_check_text <- paste("select exists (select * from song_info where song_name like", title_text, ");", collapse = "") 
  song_check <- dbGetQuery(conn, song_check_text) #if file doesn't exist, return FALSE
  if(isTRUE(song_check[[1]])){
    psql_song_text <- paste("select * from song_info where song_name like", title_text, ";", collapse = "") 
    song_information <- dbGetQuery(conn, psql_song_text)
  }
  else{
    stop("The song doesn't exist/ Exclude file suffix") #can't read nonexistent file/argument must exclude suffix
  }
  
  #create command line to find out song id
  data_id_text <- paste("select song_id from song_info where song_name like", title_text, ";", collapse = "") 
  id <- dbGetQuery(conn, data_id_text)[[1]] #return song id. If song doesn't exist, the function will stop at previous step
  signature_check <- dbGetQuery(conn, paste("select exists (select * from song_data where song_id = ", id, ");", collapse = ""))
  if(isTRUE(signature_check[[1]])){
    signature_text <- paste("select * from song_data where song_id =", id, ";", collapse = "")
    signature_information <- dbGetQuery(conn, signature_text)
  }
  else {
    stop("The song's signatures don't exist")
  }
  return(list(song_information = song_information, signature = signature_information))
} 

#####################################################################
# Task 3: identify .mp3 or .wav snippet and return song information #
#####################################################################
#1. source("matching.R") will give you global dataframe song_data, 
#search_matrix and default snippet (if snippet is not assigned by user)

#2. source("shazam.R") will convert .wav audio file into signals and return its signatures

#3. by calling identify() function, you will get the matched song id, 
#song name, artist, and release year of the song. 

identify <- function(snippet){
  checkmp3 <- regexpr("\\.mp3", snippet)
  checkwav <- regexpr("\\.wav", snippet)
  
  #if file is in .mp3 format, convert it to .wav and save as file:
  if (checkmp3[1] != -1) {
    filemp3 <- readMP3(snippet)
    filewav <- sub("\\.mp3", "\\.wav", snippet) #change name from "*.mp3" to "*.wav"
    writeWave(filemp3, filename = filewav) #take .mp3 signal to convert as .wav file, named as "<filewave>"
  }
  # if file is in .wav format: 
  else if (checkwav[1] != -1) {
    filewav <- snippet
  }
  else{
    stop("invalid audio file/Need file suffix") #if the file is neither mp3 nor wav format, or user doesn't include suffix, return error
  }  
  #calculate snippet signatures by calling song_signature():
  snippet_signal <- song_signature(filewav) #snippet_signal as an object, contains its signatures and snippet file name
  snippet_signature <- snippet_signal$signature 
  
  #match snippet's signatures with database signatures
  matched_song <- match_signature(snippet_signature,threshold = 0.01, N = 2)
  return(matched_song)
}

