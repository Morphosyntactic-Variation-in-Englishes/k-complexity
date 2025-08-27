#### R code for regression analyses as described in Ehret, Katharina. Assessing
###the role of socio-demographic triggers on Kolmogorov-based complexity in
###spoken English varieties. Submitted to: Entropy.

### setting the stage

# libraries

library(tidyverse)
library(lme4)
library(performance)
library(ggeffects)
library(broom.mixed)
library(viridis) #for colour-blind friendly palette
library(scales)
library(ggcorrplot) #for correlation plot
library(ggpubr)

# load data and apply modelling transformations

source("scripts/transform.variables.r")


### morphological complexity

# formula including all theoretically motivated triggers and varying intercept for region and for corpus

m1 <- lmer(scale(agg_meanmorphratio) ~ log10_natives + prop_non_natives + z_spread + z_density +
   		z_migration  + contact_languages + (1|macro_region) + (1| corpus), 
	data = df, REML=F
	)
		
## check assumptions

check_normality(m1) #OK: residuals appear as normally distributed (p = 0.733).

check_heteroscedasticity(m1) #OK: Error variance appears to be homoscedastic (p = 0.744).

check_collinearity(m1) #low correlation


## look at model

summary(m1)

## get significance of fixed effects via LRT

drop1(m1, test="Chisq")

## get significance of random effects via LRT using REML

m1_REML <- lmer(scale(agg_meanmorphratio) ~ log10_natives + prop_non_natives + z_spread + z_density 		+ z_migration  + contact_languages + (1|macro_region) + (1| corpus), 
	data = df, REML=TRUE
	)
	
# test significance of var intercept for region

m1R_REML <- lmer(scale(agg_meanmorphratio) ~ log10_natives + prop_non_natives + z_spread + z_density 		+ z_migration  + contact_languages +  (1| corpus), 
	data = df, REML=TRUE
	)

anova(m1R_REML, m1_REML, test = 'Chisq', refit = FALSE) 


# test significance of var intercept for corpus

m1C_REML <- lmer(scale(agg_meanmorphratio) ~ log10_natives + prop_non_natives + z_spread + z_density 		+ z_migration  + contact_languages +  (1| region), 
	data = df, REML=TRUE
	)

anova(m1C_REML, m1_REML, test = 'Chisq', refit = FALSE) 


###  plot fixed effects

## dots and whisker plot with confidence intervals based on code from Winter (2020: 183)

m1_fixefs <-  c("z_spread", "z_density", "z_migration","contact_languages1", "contact_languages2", "contact_languages3", "prop_non_natives", "log10_natives")

# get estimates and confidence intervals

m1_coefs <- tidy(m1, conf.int = TRUE) |>
		filter(term %in% m1_fixefs)

#write.csv(m1_coefs, "stats/m1_coefs_confint.csv", row.names=F)

# order coefficients by estimate and relevel

m1_pred.ordered <- arrange(m1_coefs, estimate)$term

m1_coefs <- mutate(m1_coefs, term = factor(term, levels = m1_pred.ordered))


# plot 

m1_coefs |> ggplot(aes(x = term, y = estimate)) +
	 geom_point(color="#1fa187", size=2, show.legend=F) + #viridis green
	 geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
	 width = 0.3, color="#1fa187") +
	 geom_hline(yintercept = 0, linetype = 2) +
	 coord_flip() + labs(title = NULL, x = "fixed effects", y="coefficient estimates") +
	 ylim(-1.5, 2) +
 	 theme_bw(base_family="Arial")

ggsave("pics/m1_dotswhiskers.jpeg", dpi=600, unit="mm", width=180)


### plot random effects

## extract random effect variances

# for region

m1_ranefRegion <-  as.data.frame(ranef(m1))[1:6,]

m1_ranefRegion$region <- c("Africa", "America", "Asia", "British Isles", "Caribbean", "Oceania")
	
m1_ranefRegion |> ggplot(aes(x = region, y = condval)) +
		geom_point(aes(colour = factor(term)), shape=1, size=3, stroke=1, show.legend=T) +
	 	geom_hline(yintercept = 0, linetype = 3) +
	 	coord_flip() + 
	 	labs(title = NULL, y = "random effect variance", x="region") +
		scale_color_viridis(discrete = TRUE) +
 	 	theme_bw(base_family="Arial") + 
		theme(legend.position = "top", legend.title=element_blank())


ggsave("pics/m1_randomVarRegion.jpeg", dpi=600, unit="mm", width=180)


# for corpus (not used in paper)

m1_ranefCorpus <-  as.data.frame(ranef(m1))[7:8,]

m1_ranefCorpus$corpus <- c("FRED", "ICE")

m1_ranefCorpus |> ggplot(aes(x = corpus, y = condval)) +
		geom_point(aes(colour = factor(term)), shape=1, size=3, stroke=1, show.legend=T) +
	 	geom_hline(yintercept = 0, linetype = 3) +
	 	coord_flip() + 
	 	labs(title = NULL, y = "random effect variance", x="corpus") +
		scale_color_viridis(discrete = TRUE) +
 	 	theme_bw(base_family="Arial") + 
		theme(legend.position = "top", legend.title=element_blank())

ggsave("pics/m1_randomVarCorpus.jpeg", dpi=600, unit="mm", width=180)


#############################################################

### syntactic complexity

# formula including all theoretically motivated triggers and varying intercept for region; varying intercept for corpus

s1 <- lmer(scale(agg_meansynratio) ~ log10_natives + prop_non_natives + z_spread + z_density +
   		z_migration  + contact_languages + (1|macro_region) + (1| corpus), data = df, REML=F	)

# check assumptions

check_normality(s1) #OK: residuals appear as normally distributed (p = 0.974).

check_heteroscedasticity(s1) #OK: Error variance appears to be homoscedastic (p = 0.738).

check_collinearity(s1) #moderate and high correlations


## address collinearity

# check correlations of predictors and visualise

cor.df <- as.data.frame(df) |> select(c("z_spread", "contact_languages",
				   "log10_natives", "z_density",
				  "prop_non_natives", "z_migration"
				  ))

cor.df <- cor.df %>%
  	mutate_at(vars(contact_languages), as.numeric)


# correlation matrix

cor.mat <- cor(cor.df)

#write.csv(cor.mat, "stats/corr.csv")

# calculate matrix with p-values

p.mat <- cor_pmat(cor.df)

#write.csv(p.mat, "stats/pvalues_corr.csv")


ggcorrplot(cor.mat, hc.order = TRUE, type = "upper",
     	outline.col = "white", p.mat = p.mat,  sig.level = 0.05, insig = "blank") +
	scale_fill_viridis(option="D") +
	labs(x = "", y = "") +
	guides(x= guide_axis(angle = 45))

ggsave("pics/correlations.jpeg", dpi=300, unit="mm", width=180)


# remove contact_languages as highly positively correlated with prop_non_natives

s2 <- lmer(scale(agg_meansynratio) ~ log10_natives + prop_non_natives + z_spread + z_density +
   		z_migration + (1|macro_region) + (1| corpus), data = df, REML=F)


check_collinearity(s2) # low correlation


## address singularity

# check random effect variances

summary(s2) # zero estimate and deviance

s2_ranefs <- ranef(s2) # zero estimates

#write.csv(s2_ranefs, "stats/ranefEstimates_s2.csv")


## extract random effect variances and plot

# for region

s2_ranefRegion <-  as.data.frame(ranef(s2))[1:6,]

s2_ranefRegion$region <- c("Africa", "America", "Asia", "British Isles", "Caribbean", "Oceania")

s2_ranefRegion$term <- c("Intercept", "Intercept", "Intercept", "Intercept", "Intercept", "Intercept")

rvRegion <- s2_ranefRegion |> ggplot(aes(x = region, y = condval)) +
		geom_point(aes(colour = factor(term)), shape=1, size=3, stroke=1, show.legend=T) +
	 	geom_hline(yintercept = 0, linetype = 3) +
	 	coord_flip() + 
	 	labs(title = NULL, y = "random effect variance", x="region") +
		scale_color_viridis(discrete = TRUE) +
 	 	theme_bw() + 
		theme(legend.position = "top", legend.title=element_blank())


#ggsave("pics/s2_randomVarRegion.jpeg", dpi=600, unit="mm", width=180)


# for corpus 

s2_ranefCorpus <-  as.data.frame(ranef(s2))[7:8,]

s2_ranefCorpus$term <- c("Intercept", "Intercept")

s2_ranefCorpus$corpus <- c("FRED", "ICE")

rvCorpus <- s2_ranefCorpus |> ggplot(aes(x = corpus, y = condval)) +
		geom_point(aes(colour = factor(term)), shape=1, size=3, stroke=1, show.legend=T) +
	 	geom_hline(yintercept = 0, linetype = 3) +
	 	coord_flip() + 
	 	labs(title = NULL, y = "random effect variance", x="corpus") +
		scale_color_viridis(discrete = TRUE) +
 	 	theme_bw() + 
		theme(legend.position = "top", legend.title=element_blank())

#ggsave("pics/s2_randomVarCorpus.jpeg", dpi=600, unit="mm", width=180)


# combine

ggarrange(rvRegion, rvCorpus, ncol=2, common.legend=T)

ggsave("pics/s2_randomVarEstimates.jpeg", dpi=600, unit="mm", width=180)


## remove varying intercept for corpus but keep region

s3 <- lmer(scale(agg_meansynratio) ~ log10_natives + prop_non_natives + z_spread + z_density +
   		z_migration + (1|region), data = df, REML=F)


# remove varying intercept for region

s4 <- lm(scale(agg_meansynratio) ~ log10_natives + prop_non_natives + z_spread + z_density +
   		z_migration, data = df)


## check model assumptions of final model

check_normality(s4) #OK: residuals appear as normally distributed (p = 0.850)

check_heteroscedasticity(s4) # Warning: Heteroscedasticity (non-constant error variance) detected (p = 0.047).

check_collinearity(s4)  #low correlation


############################################################################

### post-hoc Kruskal-Wallis test between migration rate and language type

kruskal.test(df$z_migration, df$language_type)



