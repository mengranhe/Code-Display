#!/usr/local/bin/Rscript
suppressMessages(library(tuneR))
suppressMessages(library(signal))
suppressMessages(library(xts))

####################
# factor functions #
####################

#normalize periodogram by the max element
normPower <- function(x) {
  value <-  x / max(x)
  return(value)
}

#factor function of song_signature():
#windowing: use specgram() source code, but changed a line of code inside, in order to prevent integer overflow 
specgram_new <- function(x, n = min(256, length(x)), Fs = 2, window = hanning(n), overlap = length(window) / 2)  {
  ## if only the window length is given, generate hanning window
  if (length(window) == 1){
    window = hanning(window)
  }
  ## should be extended to accept a vector of frequencies at which to
  ## evaluate the fourier transform (via filterbank or chirp
  ## z-transform)
  if (length(n) > 1)
    stop("specgram does not handle frequency vectors yet")
  
  ## compute window offsets
  win_size = length(window)
  if (win_size > n) {
    n = win_size
    warning("specgram fft size adjusted to", n)
  }
  step = win_size - overlap
  
  ## build matrix of windowed data slices
  if (length(x) > win_size){
    offset = seq(1, length(x) - win_size, by = step)
  }
  else{
    offset = 1
    S = matrix(0, n, length(offset))
  }
  
  for (i in 1:length(offset)) {
    S[1:win_size, i] = x[offset[i]:(offset[i] + win_size - 1)] * window
  }
  
  ## compute fourier transform
  S = mvfft(S)
  
  ## extract the positive frequency components
  if (n %% 2 == 1)
    ret_n = (n + 1) / 2
  else
    ret_n = n / 2
  S = S[1:ret_n,]
  
  f = ((0:(ret_n - 1)) / n) * Fs   #fixed code, prevent integer overflow
  t = offset / Fs
  
  res = list(S = S, f = f, t = t)
  class(res) = "specgram"
  res
}


#factor function of make_peak_signature(): find all the peaks along the periodogram and return the indices of peaks
find_peak_indices <- function(powerspec, thresh = 5){
  shape <- diff(sign(diff(powerspec, na.pad = FALSE)))
  peaks_index <- sapply(which(shape < 0),
                        FUN = function(i) {
                          z <- i - thresh + 1
                          z <- ifelse(z > 0, z, 1)
                          w <- i + thresh + 1
                          w <- ifelse(w < length(powerspec), w, length(powerspec))
                          if (all(powerspec[c(z:i, (i + 2):w)] <= powerspec[i + 1])) {
                            return(i + 1)
                          } else {
                            return(numeric(0))
                          }
                        })
  peaks_index <- unlist(peaks_index)
  return(peaks_index)
}


#factor function of make_signature(): return indices of frequencies at which power is at peak 
#the number of peaks determines the length of indices of frequencies for each window
make_peak_signature <- function(numPeak, powerspec) {
  peak_index <- find_peak_indices(powerspec)
  chosen_power <- sort(powerspec[peak_index], decreasing = TRUE)[1:numPeak]
  freq_index <- match(chosen_power, powerspec)
  return(freq_index)
}

###############################################
#     main function for making signature      #
###############################################
song_signature <- function(file, numPeak = 10) { #song.signature() takes arguments of file name and the number of signatures drawn from each window
  song <- readWave(file)
  songl <- song@left
  # each channel contains length(song@left) sample points, 
  # with duration of length(song@left)/song@samp.rate seconds 
  
  #16 bit means that the sound pressure values are mapped to integer values
  #that can range from -2^15 to (2^15)-1. Convert it to floating value ranging from -1 to 1
  songl <- songl / 2 ^ (song@bit - 1)
  
  Fs <- song@samp.rate #sampling rate
  step <- Fs # one spectral slice every 1s
  window <- 10 * Fs # 10s data window
  
  fftn <- 2 ^ ceiling(log2(window)) # next highest power of 2
 
  spg <- specgram_new(songl, n = fftn, Fs, window, window - step)
  #spg$S: complex output of the FFT, one row per slice.
  #spg$f:the frequency indices corresponding to the rows of S.
  #spg$t:the time indices corresponding to the columns of S.
  
  ###################################
  # local periodogram and signature #
  ###################################
  Periodogram <- abs(spg$S) ^ 2
  Periodogram1 <- apply(Periodogram, 2, normPower) #normalized periodogram
  
  #normalize frequency by Nyquist frequency (half the sampling rate of the time series)
  normfreq <- spg$f / (song@samp.rate / 2)
  
  #return signatures of song, which are the frequencies at which the powers are at peaks
  make_signature <- function(powerspec) { 
    sig_freq_index <- make_peak_signature(numPeak, powerspec) #10 is number of peaks by default, it determines the number of signatures
    signature_frequency <- normfreq[sig_freq_index]
    return(signature_frequency)
  } 
  
  signature <- t(apply(Periodogram1, 2, make_signature))
  songname <- sub("\\..*", "", file)
  return(list(signature = signature, song_name = songname)) #info about id from database
} 


###################### Copyright of original specgram() ############################
## Copyright (C) 1999-2000 Paul Kienzle
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

## usage: [S [, f [, t]]] = specgram(x [, n [, Fs [, window [, overlap]]]])
##
## Generate a spectrogram for the signal. This chops the signal into
## overlapping slices, windows each slice and applies a Fourier
## transform to determine the frequency components at that slice.
##
## x: vector of samples
## n: size of fourier transform window, or [] for default=256
## Fs: sample rate, or [] for default=2 Hz
## window: shape of the fourier transform window, or [] for default=hanning(n)
##    Note: window length can be specified instead, in which case
##    window=hanning(length)
## overlap: overlap with previous window, or [] for default=length(window)/2
##
## Return values
##    S is complex output of the FFT, one row per slice
##    f is the frequency indices corresponding to the rows of S.
##    t is the time indices corresponding to the columns of S.
##    If no return value is requested, the spectrogram is displayed instead.
##
## Example
##    x = chirp([0:0.001:2],0,2,500);  # freq. sweep from 0-500 over 2 sec.
##    Fs=1000;                  # sampled every 0.001 sec so rate is 1 kHz
##    step=ceil(20*Fs/1000);    # one spectral slice every 20 ms
##    window=ceil(100*Fs/1000); # 100 ms data window
##    specgram(x, 2^nextpow2(window), Fs, window, window-step)
##
##    ## Speech spectrogram
##    [x, Fs] = auload(file_in_loadpath("sample.wav")); # audio file
##    step = fix(5*Fs/1000);     # one spectral slice every 5 ms
##    window = fix(40*Fs/1000);  # 40 ms data window
##    fftn = 2^nextpow2(window); # next highest power of 2
##    [S, f, t] = specgram(x, fftn, Fs, window, window-step)
##    S = abs(S(2:fftn*4000/Fs,:)); # magnitude in range 0<f<=4000 Hz.
##    S = S/max(S(:));           # normalize magnitude so that max is 0 dB.
##    S = max(S, 10^(-40/10));   # clip below -40 dB.
##    S = min(S, 10^(-3/10));    # clip above -3 dB.
##    imagesc(flipud(log(S)));   # display in log scale
##
##
##     imagesc(flipud(log(S(idx,:))))
## 2001-07-05 Paul Kienzle <pkienzle@users.sf.net>



