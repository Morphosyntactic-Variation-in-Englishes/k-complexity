#### R code to load data and implement modelling transformations for the
#### subsequent regression analyses as described in Ehret, Katharina. Assessing
###the role of socio-demographic triggers on Kolmogorov-based complexity in
###spoken English varieties. Submitted to: Entropy.


## load dataset with raw values

df <- read.csv("data/k-complexity_data.csv")


## remove SBCSAE because the entire corpus is an extreme outlier

df <- df %>% filter(language_ID !='14') 


## apply transformations for modelling

# convert character strings to factors

df <- df %>% 
  mutate_at(c("macro_region", "contact_languages", "corpus", "language_type"), as.factor)


# add log transformed numeric predictor natives; use log10 following Sinnemäki and Di Garbo (2018)

df <- df %>% 
mutate(log10_natives = log10(natives), .after ="non_natives")  

# add scaled (centered and scaled) numeric variables

df <- df %>% 
mutate(z_spread = scale(spread), .after ="spread")  

df <- df %>% 
mutate(z_migration = scale(migration), .after ="migration")  

# calculate population density and z-score

df <- df |> mutate(z_density = scale((natives+non_natives)/spread), .after = z_migration)

# calculate the proportion of non-native speakers following Bentz and Winter (2013), as the proportion of non-native speakers in the total population of native and non-native speakers. 

df <- df |> mutate(prop_non_natives = non_natives/(natives+non_natives), .after = non_natives)



## aggregate complexity ratios for variety-level analysis


df <- df %>%  group_by(language_ID) %>% mutate(agg_meansynratio =
mean(meansynratio), agg_meanmorphratio= mean(meanmorphratio), agg_morph.sd =
	sd(meanmorphratio), agg_syn.sd = sd(meansynratio))

 df <- df %>%  
select(!c(filename, meansynratio, meanmorphratio, syn.sd, morph.sd, area)) %>%
	distinct()



