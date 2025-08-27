#### R code for data wrangling to prodto produce tidy data for further analyses
###as described in Ehret, Katharina. Assessing the role of socio-demographic
###triggers on Kolmogorov-based complexity in spoken English varieties.
###Submitted to: Entropy.


## libraries

library(tidyverse)
library(data.table)

## extract relevant complexity ratios

# load complexity ratios (output extracted from the complexity analysis results; see https://github.com/katehret/measuring-language-complexity)

fredRatios <- fread("data/FRED_complexityRatios.csv") 

ICERatios <- fread("data/ICE_complexityRatios.csv")

SBRatios <- fread("data/SBCSAE_complexityRatios.csv")

# get filenames and extract mean ratios for morphological and syntactic complexity

fred <- data.table(fredRatios$V1, fredRatios$meansynratio, fredRatios$meanmorphratio, fredRatios$syn.sd, fredRatios$morph.sd)

ICE <- data.table(ICERatios$V1, ICERatios$meansynratio, ICERatios$meanmorphratio, ICERatios$syn.sd, ICERatios$morph.sd)

SB <- data.table(SBRatios$V1, SBRatios$meansynratio, SBRatios$meanmorphratio, SBRatios$syn.sd, SBRatios$morph.sd)


# create nice column labels

colnames(fred) <- c("filename", "meansynratio", "meanmorphratio", "syn.sd", "morph.sd")

colnames(ICE) <- c("filename", "meansynratio", "meanmorphratio", "syn.sd", "morph.sd")

colnames(SB) <- c("filename", "meansynratio", "meanmorphratio", "syn.sd", "morph.sd")


# join

myls <- list(fred, ICE, SB)

mydata <- rbindlist(myls, use.names=T)

# extract and add area code

mydata[, ":=" (area = gsub( "([A-Z]*)_*([0-9])", "\\1", mydata$filename))]

# add language ID based on area code; IDs are taken from eWAVE/DOVE

mydata <- mydata |> 
mutate (ID = as.factor(area), .before = 1) |>
mutate (#FRED
	ID = ifelse(area == "INV", "2", ID), #ScH
	ID = ifelse(area == "ROC", "2", ID),
	ID = ifelse(area == "SUT", "2", ID),
	ID = ifelse(area == "ELN", "2", ID), #ScL
	ID = ifelse(area == "MLN", "2", ID),
	ID = ifelse(area == "ANS", "2", ID), 
	ID = ifelse(area == "BAN", "2", ID),
	ID = ifelse(area == "DFS", "2", ID),
	ID = ifelse(area == "FIF", "2", ID),
	ID = ifelse(area == "KCD", "2", ID),
	ID = ifelse(area == "KRS", "2", ID),
	ID = ifelse(area == "LKS", "2", ID),
	ID = ifelse(area == "PEE", "2", ID),
	ID = ifelse(area == "PER", "2", ID),
	ID = ifelse(area == "SEL", "2", ID),
	ID = ifelse(area == "WLN", "2", ID),
	ID = ifelse(area == "KEN", "9", ID), #Southeast
	ID = ifelse(area == "SFK", "9", ID),
	ID = ifelse(area == "LND", "9", ID),
	ID = ifelse(area == "MDX", "9", ID),
	ID = ifelse(area == "LEI", "80", ID), #Midlands
	ID = ifelse(area == "SAL", "80", ID),
	ID = ifelse(area == "WAR", "80", ID),
	ID = ifelse(area == "NTT", "80", ID),
	ID = ifelse(area == "DUR", "6", ID), #North
	ID = ifelse(area == "NBL", "6", ID),
	ID = ifelse(area == "LAN", "6", ID),
	ID = ifelse(area == "WES", "6", ID),
	ID = ifelse(area == "YKS", "6", ID),
	ID = ifelse(area == "DEN", "5", ID), #Wales
	ID = ifelse(area == "GLA", "5", ID),
	ID = ifelse(area == "IOM", "4", ID), #Man
	ID = ifelse(area == "CON", "7", ID), #Southwest
	ID = ifelse(area == "DEV", "7", ID),
	ID = ifelse(area == "OXF", "7", ID),
	ID = ifelse(area == "SOM", "7", ID), 
	ID = ifelse(area == "WIL", "7", ID),
	ID = ifelse(area == "HEB", "82", ID), #Heb
	#ICE
	ID = ifelse(area == "AU", "59", ID),
	ID = ifelse(area == "CAN", "79", ID),
	ID = ifelse(area == "GB", "78", ID), 
	ID = ifelse(area == "GH", "39", ID),
	ID = ifelse(area == "HK", "56", ID),
	ID = ifelse(area == "IN", "52", ID),
	ID = ifelse(area == "IRE", "3", ID),
	ID = ifelse(area == "JAM", "26", ID), #Jam46 excluded due to sparsity
	ID = ifelse(area == "NG", "41", ID),
	ID = ifelse(area == "NZ", "64", ID),
	ID = ifelse(area == "PHI", "75", ID),
	ID = ifelse(area == "SIN", "57", ID),
	ID = ifelse(area == "SL", "55", ID),
	ID = ifelse(area == "TT", "81", ID),
	ID = ifelse(area == "UG", "47", ID),
	ID = ifelse(area == "EA", "46", ID), #only Kenya 
	#SBCSAE	
	ID = ifelse(area == "SBC", "14", ID)
	)



## merge with demographic data from DOVE

# load demographic data

langs <- fread("data/languages_DOVE_v1.0.csv") #taken from https://github.com/Morphosyntactic-Variation-in-Englishes/DOVE; note the individual copyright restrictions of the demographic data (cf. sources and copyright files in DOVE)


# add corpus information for languages; currently not in DOVE

langs$corpus <- c("FRED", "ICE", "FRED", "FRED", "FRED", "FRED", "FRED", "SBCSAE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "ICE", "FRED", "ICE", "FRED")


# join demographic data with complexity data

mydata[, ID := as.integer(ID)] #because ID is an integer in langs; required for join

mydata <- full_join(mydata, langs, by= "ID", multiple="all", relationship="many-to-many")


# rename columns for clarity

mydata <- rename(mydata, language_ID = ID)

mydata <- rename(mydata, language_name = name)

# save final dataset for further analysis

fwrite(mydata, "data/k-complexity_data.csv")


