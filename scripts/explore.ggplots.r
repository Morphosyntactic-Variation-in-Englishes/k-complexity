#### R code for visual exploration of Kolmogorov complexity in English
#### varieties as described in Ehret, Katharina. Assessing
###the role of socio-demographic triggers on Kolmogorov-based complexity in
###spoken English varieties. Submitted to: Entropy.

### set-up 

# libraries

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(viridis) #for colour-blind friendly palette
library(scales)
library(data.table)
library(rstatix)


### load data

df <- fread("data/k-complexity_data.csv")


### look at data

## morphratio

boxplot(df$meanmorphratio)

## determine outliers

morph_out <- boxplot.stats(df$meanmorphratio)$out

length(morph_out) #285

out_mind <- which(df$meanmorphratio %in% morph_out) # get index of outliers

morph_outliers <- df[out_mind, ]

# check areal distribution of outliers

morph_outliers$area # a lot of SBC but also other areas in various FRED dialects

length(which(morph_outliers$area == "SBC")) # 59 out of 60

# fwrite(morph.outliers, "morph_outliers.csv")


## synratio

boxplot(df$meansynratio)

syn_out <- boxplot.stats(df$meansynratio)$out

length(syn_out) # 82

out_synind <- which(df$meansynratio %in% syn_out)

syn_outliers <- df[out_synind, ]


# check areal distribution of outliers

syn_outliers$area  # mostly only SBC

length(which(syn_outliers$area == "SBC")) # 57 out of 60

# fwrite(syn_outliers, "syn_outliers.csv")



### remove SBCSAE as the entire corpus is an outlier; keep the others as there is no clear pattern matching a whole subcorpus

df <- df |> filter(language_ID !='14') 


### file level visualisation

# morphological vs. syntactic complexity by language type  

ggplot(data = df, aes(x = meansynratio , y = meanmorphratio , col=language_type)) +
        geom_point() + 
        geom_smooth(method = "lm") +
	scale_color_viridis(discrete = TRUE, option="D") +
        labs(
             x = "Mean syntactic comlexity ratio",
             y = "Mean morphological complexity ratio",
	     col="Type") +
	theme_minimal()


ggsave("pics/scatter_byType.jpeg", dpi=300, unit="mm", width=180)


### variety-level visualisation

# aggregate by variety

byVar <- df %>%  
   group_by(abbr) %>%  # group data by variety
  summarise_at(vars(meansynratio, meanmorphratio), list(aggregate = ~mean(., na.rm=F), deviations = ~sd(., na.rm=F)))

types <- c("L1c", "L1c", "L1c", "L1c",  "L2", "L2", "L1t", "L2", "L1c",
	   "L2", "L2", "L1c", "L1t", "L1c", "L2", "L1t", "L1c", "L1t", "L2",
	   "L1t", "L1t", "L2", "L2", "L1c")

byVar$language_type <- as.factor(types)


# plot complexity ratios by variety

  ggplot(byVar, aes(x = meansynratio_aggregate, y = meanmorphratio_aggregate, col=language_type)) +
        geom_point() + 
        scale_color_viridis(discrete = TRUE, option="D") +
      	labs(
             x = "Aggregate mean syntactic complexity ratio",
             y = "Aggregate mean morphological complexity ratio",
	     col = "Type") +
	guides(colour=guide_legend(override.aes=list(alpha=1, size=3))) +
  	geom_text_repel(aes(label=abbr), size=4, max.overlaps = 20) + theme_minimal()


ggsave("pics/scatter_byVariety.jpeg", dpi=300, unit="mm", width=180)


