# Script by Byron Ahn (byron@ucla.edu)
# Feb 8, 2023
#	Attribution-ShareAlike 2.5 license
#
#
# This script depends on three things
#   (1) syllabify-core.py in the same directory
#   (2) textgrids in a folder called "input" in the same directory
#   (3) those textgrids MUST have a "Words" tier and a "Phones" tier, with the "Phones" tier containing ARPABET transcriptions
#         (ARPABET Phones can be added through a forced aligner like the Montreal Forced Aligner: https://montreal-forced-aligner.readthedocs.io/)
#
# This script then creates two things
#   (1) *modified* textgrids in a subdirectory called "syllabified"
#   (2) an R dataframe called `syllableDF` that contains all the syllable information
#

## check if relevant libraries are installed; if not, install them
if (!"devtools" %in% installed.packages()) install.packages("devtools")
if (!"PraatR" %in% installed.packages()) devtools:::install_github("usagi5886/PraatR")
if (!"rPraat" %in% installed.packages()) install.packages("rPraat")
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if (!"reticulate" %in% installed.packages()) install.packages("reticulate")

## load up those libraries
library(tidyverse)
# `PraatR` lets you call (pre-existing) Praat binaries to do Praat functions, from R (http://www.aaronalbin.com/praatr/index.html)
library(PraatR)
# `rPraat` lets you manipulate Praat files (https://cran.r-project.org/web/packages/rPraat/index.html)
library(rPraat)
# `reticulate` lets you pass variables between python and R (https://rstudio.github.io/reticulate/)
library(reticulate)
# `here` lets you get the directory path of the present script
library(here)

## load up the Python script written by Kyle Gorman (https://github.com/kylebgorman/syllabify/)
#	(modified to not have the "if __name__ == '__main__':" block)
#	this script does the heavy-lifting for doing English syllabification
source_python("syllabify-core.py")


## set up some variables
# set the working directory
WorkingDirectoryFullPath = file.path(here(),"input")

# create a sub-directory named "syllabified"
dir.create(file.path(here(),"syllabified"))

# start by setting up a dataframe
syllableDF <- data.frame(file = character(), word = character(), syllable = character(), syllStartTime = double(), syllEndTime = double())


## the core logic of the script

# a for loop, going through all the .TextGrid files in `WorkingDirectoryFullPath`
for (tg_file in list.files(path=WorkingDirectoryFullPath, pattern=".+\\.TextGrid")){
	
	# read in the textgrid file, store it as `theTg`:
	theTg = tg.read(file.path(here(),"input",tg_file), encoding = "UTF-8")
	
	# create a new ties, "Syllables", before the first tier whose name is "Phones"
	theTg <- tg.insertNewIntervalTier(theTg, 
										which(sapply(theTg, FUN=function(X) "Phones" %in% X))[1],
										"Syllables"
	)
	
	# a for loop, going through all the Word intervals in the .TextGrid files
	for (n in 1:tg.getNumberOfIntervals(theTg, "Words")){
		
		# check to see if the label of the word has any word characters:
		if (grepl("\\w+",theTg$Words$label[n])) {
			
			# chop up `theTg` to isolate just the time-frame of the word
			theWord <- tg.cut(theTg, theTg$Words$t1[n], theTg$Words$t2[n]) 
			
			# store the phones of that word, as an array `phones`
			phones <- theWord$Phones$label
			
			# the row number for the next syllable in the syllableDF is `syllableNum`
			syllableNum <- dim(syllableDF)[1] + 1
			
			# if there is more than one phone listed, pass it to the syllabify function (which doesn't work if there's only one phone)
			if (length(phones) > 1){
				# syllabify the `phones` array, and store it as a list called `syllables`
				# each list in `syllables` (e.g., `syllables[[1]]` being the first syllable) has 3 members ---onset, nucleus, coda--- which are each stored as lists
				syllables <- reticulate::py$syllabify(phones)
				
				# a counter for keeping track of which phone we're looking at (for start/end times)
				xPhone <- 0
				
				# a for loop, going through all the syllables (i.e. members of the list named `syllables`)
				for (xSyll in 1:length(syllables)){
					
					#advance the syllable counter to the next phone (the 1st phone in the 1st pass; in subsequent passes, the phone after the last phone of the previous syllable)
					xPhone <- xPhone + 1
					
					# add to `syllableDF`… 
					syllableDF[syllableNum, 1] <- tg_file #the filename
					syllableDF[syllableNum, 2] <- theTg$Words$label[n] #the label of the nth Word
					syllableDF[syllableNum, 3] <- paste(unlist(syllables[[xSyll]]), collapse="") # the string corresponding to the phones in the xSyll-th syllable
					syllableDF[syllableNum, 4] <- theWord$Phones$t1[xPhone] # the start time of the first phone of the xSyll-th syllable
					syllableDF[syllableNum, 5] <- theWord$Phones$t2[xPhone+ length(syllables[[xSyll]][[1]]) + length(syllables[[xSyll]][[2]]) + length(syllables[[xSyll]][[3]]) - 1] # the end time of the last phone of the xSyll-th syllable
					
					# add syllable to .textgrid
					theTg <- tg.insertInterval(theTg,
												which(sapply(theTg, FUN=function(X) "Syllables" %in% X))[1], # the number of the first tier whose label is "Syllables"
												theWord$Phones$t1[xPhone], # the start time of the first phone of the xSyll-th syllable
												theWord$Phones$t2[xPhone+ length(syllables[[xSyll]][[1]]) + length(syllables[[xSyll]][[2]]) + length(syllables[[xSyll]][[3]]) - 1], # the end time of the last phone of the xSyll-th syllable
												label = paste(unlist(syllables[[xSyll]]), collapse="") # the string corresponding to the phones in the xSyll-th syllable
												)
					
					# move the counter `xPhone` to the last phone in this syllable
					xPhone <- xPhone + length(syllables[[xSyll]][[1]]) + length(syllables[[xSyll]][[2]]) + length(syllables[[xSyll]][[3]]) - 1
					
					# the row number for the next syllable in the syllableDF is `syllableNum`
					syllableNum <- dim(syllableDF)[1] + 1
				} # end forloop through syllables
			} else if (length(phones) == 1) {
				# in case there is exactly 1 phone in the word, use the start/end time of that phone interval
				syllableDF[syllableNum, 1] <- tg_file #the filename
				syllableDF[syllableNum, 2] <- theTg$Words$label[n] #the label of the nth Word
				syllableDF[syllableNum, 3] <- paste(phones, collapse="") # the string corresponding to the phones in the syllable
				syllableDF[syllableNum, 4] <- theWord$Phones$t1[1] # the start time of the first (and only) phone of the syllable
				syllableDF[syllableNum, 5] <- theWord$Phones$t2[1] # the end time of the last (and only) phone of the syllable
				
				# add syllable to .textgrid
				theTg <- tg.insertInterval(theTg,
											which(sapply(theTg, FUN=function(X) "Syllables" %in% X))[1], # the number of the first tier whose label is "Syllables"
											theWord$Phones$t1[1], # the start time of the first (and only) phone of the syllable
											theWord$Phones$t2[1], # the end time of the last (and only) phone of the syllable
											label = paste(phones, collapse="") # the string corresponding to the phones in the syllable
											)
			} else {
				# in case there are no phones in the word, use the start/end time of that word interval
				syllableDF[syllableNum, 1] <- tg_file #the filename
				syllableDF[syllableNum, 2] <- theTg$Words$label[n] #the label of the nth Word
				syllableDF[syllableNum, 3] <- character() # a blank label
				syllableDF[syllableNum, 4] <- theWord$Words$t1[1] # the start time of the word
				syllableDF[syllableNum, 5] <- theWord$Words$t2[1] # the end time of the word
			} # end ifelse for number of phones
		} # end ifelse for Word interval having content
	} # end forloop through Words
	tg.write(theTg, file.path(here(),"syllabified",tg_file))
} # end forloop through files

# clear out the environment, except for the `syllableDF` dataframe
rm(syllables, theTg, theWord, phones, syllableNum, n, tg_file, xPhone, xSyll, O2, O3, r, SLAX, VOWELS, chain, destress, pprint, syllabify, WorkingDirectoryFullPath)