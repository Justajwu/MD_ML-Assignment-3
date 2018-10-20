#library(memisc)
#library(foreign)
#library(Hmisc)
#library(doParallel)
#library(foreach)
#library(data.table)
#library(ggmap)
#library(proj4)
#library(maptools) 
#library(gpclib)
#library(sp)
#library(spatstat)
#library(rgdal)
#library(randomForest)
library(tidyverse)
library(lubridate)
library(ROCR)

#gpclibPermit()

# use the maximum number of cores available for parallel computation
#num.cores <- detectCores()
#registerDoParallel(cores=8)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# read.por tries two different methods for reading in an SPSS file.
# NOTE: for compatibility, column names are converted to lowercase
read.por <- function(filename) {
	df <- data.frame()
	try(df <- data.frame(read.spss(filename), stringsAsFactors=FALSE), silent = TRUE)
	if (nrow(df) == 0) {
		try(df <- data.frame(as.data.set(spss.portable.file(filename)), stringsAsFactors=FALSE), silent = TRUE)
	}
	names(df) <- tolower(names(df))
	df
}

# functions to recode factor levels
recode.factor <- function(f, old.levels, new.levels) {
	f.new <- as.factor(f)
	level.map <- new.levels
	names(level.map) <- old.levels
	levels(f.new) <- level.map[levels(f.new)]
  f.new <- factor(f.new, levels = unique(new.levels))
	f.new
}

recode.yn <- function(f) {
	f.new <- factor(f, levels=c('N','Y'))
	f.new <- as.logical(as.integer(f.new) - 1)
	f.new
}

recode.yesno <- function(f) {
  f.new <- factor(f, levels=c('NO','YES'))
  f.new <- as.logical(as.integer(f.new) - 1)
  f.new
}

# this function is solely for the stop being inside or outside
recode.io <- function(f) {
  f.new <- factor(f, levels=c('O','I'))
  f.new <- as.logical(as.integer(f.new) - 1)
  f.new
}

# this function is solely for whether the officer provided a verbal statement when not in uniform
recode.0V <- function(f) {
  f.new <- factor(f, levels=c('0','V'))
  f.new <- as.logical(as.integer(f.new) - 1)
  f.new
}

# this function is solely for whether the officer provided a shield when not in uniform
recode.0S <- function(f) {
  f.new <- factor(f, levels=c('0','S'))
  f.new <- as.logical(as.integer(f.new) - 1)
  f.new
}


#convert offense codes to human-readable names
offense.codes <- read.delim('offense-codes.tsv', header=FALSE, col.names=c('code','offense'))
offense.codes$offense <- tolower(offense.codes$offense)

convert.offense.code <- function(codes) {
offenses <- offense.codes$offense[as.integer(codes)]
offenses <- factor(offenses, levels=offense.codes$offense)
offenses
}


# combine and standardize top 100 reasons for arrest.
arrest.offenses <- read.delim('Lab 6/src/arrest.offenses.tsv', header=FALSE, col.names=c('real.offense','nominal.offense'))
arrest.offenses$real.offense <- trim(arrest.offenses$real.offense)
arrest.offenses$nominal.offense <- trim(arrest.offenses$nominal.offense)
convert.arrest.reasons <- function(rawlist) {
ndx <- match(rawlist, arrest.offenses$nominal.offense)
responses <- arrest.offenses$real.offense[ndx]
ndx <- is.na(responses) & (rawlist!="")
responses[ndx] <- "not.top.100"
responses <- factor(responses)
responses
}

# logit function
logit <- function(rawlist) {
  output <- exp(rawlist)/(1+exp(rawlist))
  output
}

# inverse logit function
inv.logit <- function(x) {
	p <- exp(x)/(1+exp(x))
	p
}

# list of drug-related suspected crimes
druglist <- c("criminal possession of marihuana", "criminal possesion of controlled substance", "criminal sale of controlled substance", "criminal sale of marihuana")

# proj4 strings
epsg.2263 = "+proj=lcc +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
nyc.proj = "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"

# standardize
standardize <- function(x) {
x.std <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
x.std
}