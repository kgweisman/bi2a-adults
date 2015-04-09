# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lubridate)

# clear environment
rm(list=ls())

# --- READ IN DATA ------------------------------------------------------------

# RUN 01
# d_tidy = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/data/run-india-and-us_01.csv", fileEncoding = "latin1")

# RUN 02
d_tidy = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/data/run-india-and-us_02_hand-coded.csv", fileEncoding = "latin1")

# filter out by comp_filter
d_tidy = d_tidy %>%
  filter(comp_filter == "keep")

# --- DEMOGRAPHICS ------------------------------------------------------------
# count observations
n_india = d_tidy %>%
  filter(country == "india") %>%
  count(worker_id) %>%
  nrow()
n_india

n_us = d_tidy %>%
  filter(country == "us") %>%
  count(worker_id) %>%
  nrow()
n_us

# gender
d_tidy %>% 
  select(country, worker_id, gender) %>%
  distinct() %>%
  count(country, gender)

# age
d_tidy %>% 
  select(-trialNum, -swatch, -response, -responseCoded, -rt) %>%
  distinct() %>%
  group_by(country) %>%
  summarise(mean = mean(age, na.rm = T),
            sd = sd(age, na.rm = T))

# ethnicity
d_tidy %>% 
  select(country, worker_id, ethnicity) %>%
  distinct() %>%
  count(country, ethnicity)

# religion
d_tidy %>% 
  select(country, worker_id, religion) %>%
  distinct() %>%
  count(country, religion)

# education
d_tidy %>% 
  select(country, worker_id, education) %>%
  distinct() %>%
  count(country, education)

# english_native
d_tidy %>% 
  select(country, worker_id, english_native) %>%
  distinct() %>%
  count(country, english_native)

# # commments
# View(d_tidy %>%
#   select(country, condition, comments) %>%
#   distinct())

# --- DATAFRAMES & SUMMARIES -----------------------------------

# summarize by condition
condition_summary = d_tidy %>%
  group_by(country, condition) %>%
  summarise(mean = mean(responseCoded, na.rm = T),
            sd = sd(responseCoded, na.rm = T),
            n = length(responseCoded)/48)
# View(condition_summary)

# summarize by swatch and condition
swatch_summary = d_tidy %>%
  group_by(country, swatch, condition) %>%
  summarise(mean = mean(responseCoded, na.rm = T),
            sd = sd(responseCoded, na.rm = T),
            n = length(responseCoded))
# View(swatch_summary)

# add animal ratings to swatch_summary
animal_ratings = swatch_summary %>%
  select(-sd, -n) %>%
  spread(condition, mean) %>%
  arrange(animal) %>%
  select(country, swatch, animal) %>%
  full_join(swatch_summary) %>%
  select(-animal)
View(animal_ratings)

# animalrat_us = animal_ratings %>%
#   filter(country == "us") %>%
#   select(swatch, animal_rating_us = animal) %>%
#   distinct()
# # View(animalrat_us)
# write.csv(animalrat_us, "/Users/kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/data/animal_ratings_us_adults.csv")
# animalrat_us = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/data/animal_ratings_us_adults.csv")

# 
# animalrat_ind = animal_ratings %>%
#   filter(country == "india") %>%
#   select(swatch, animal_rating_ind = animal) %>%
#   distinct()
# # View(animalrat_ind)
# write.csv(animalrat_ind, "/Users/kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/data/animal_ratings_india_adults.csv")

# animalrat_ind = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/data/animal_ratings_india_adults.csv")

temp = animal_ratings %>%
  select(-sd, -n) %>%
  spread(country, mean) %>%
  rename(animal_rating_us = us,
         animal_rating_india = india) %>%
  select(-condition) %>%
  mutate(animal_ranking_us = rank(animal_rating_us),
         animal_ranking_india = rank(animal_rating_india))
View(temp)

animal_ratings2 = animal_ratings %>%
  full_join(temp) %>% 
  select(-condition) %>%
  mutate(selfrat = ifelse(country == "india", animal_rating_india,
                          ifelse(country == "us", animal_rating_us,
                                 "NA")),
         selfrank = ifelse(country == "india", animal_ranking_india,
                           ifelse(country == "us", animal_ranking_us,
                                  "NA"))) %>%
  mutate(selfrat = as.numeric(as.character(selfrat)),
         selfrank = as.numeric(as.character(selfrank)))
  
View(animal_ratings2)

# # look at correlations of means (odd)
# d_swatches = d_tidy %>%
#   select(country, condition, worker_id, swatch, responseCoded) %>%
#   #   spread(condition, responseCoded) %>%
#   group_by(country, swatch, condition) %>%
#   summarise(mean = mean(responseCoded)) %>%
#   spread(condition, mean)
# 
# cor_india = d_swatches %>% 
#   filter(country == "india") %>%
#   select (-swatch, -country) %>% 
#   cor()
# cor_india
# 
# cor_us = d_swatches %>% 
#   filter(country == "us") %>%
#   select (-swatch, -country) %>% 
#   cor()
# cor_us

# --- PLOTS -----------------------------------

# overal mean ratings by condition and country
condsum_plot = condition_summary %>%
  ggplot(aes(x = condition, y = mean, fill = condition)) +
  facet_wrap(~ country) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean - 2*sd/n,
                    ymax = mean  + 2*sd/n,
                    width = 0.1)) +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Mean rating by condition and country\nError bars: 95% CI\n")
print(condsum_plot)

condsum_plot2 = d_tidy %>%
  ggplot(aes(x = condition, y = responseCoded, fill = condition)) +
  facet_wrap(~ country) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Mean rating by condition and country\n")
print(condsum_plot2)

# mean rating by swatch and condition, per country
  # ... india
  swatchsum_plot_ind = swatch_summary %>%
    filter(country == "india") %>%
    ggplot(aes(x = condition, y = mean, fill = condition)) +
    facet_wrap( ~ swatch, ncol = 6) +
    geom_bar(stat = "identity", position = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = mean - sd/sqrt(n),
                      ymax = mean  + sd/sqrt(n),
                      width = 0.1)) +
    theme_bw() +
    scale_fill_brewer(palette = "Set2") +
    theme(text = element_text(size = 14),
          axis.text.x = element_text(size = 0)) +
    labs(title = "Mean rating by swatch and condition: INDIA \n")
  print(swatchsum_plot_ind)
  # ... us
  swatchsum_plot_us = swatch_summary %>%
    filter(country == "us") %>%
    ggplot(aes(x = condition, y = mean, fill = condition)) +
    facet_wrap( ~ swatch, ncol = 6) +
    geom_bar(stat = "identity", position = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = mean - sd/sqrt(n),
                      ymax = mean  + sd/sqrt(n),
                      width = 0.1)) +
    theme_bw() +
    scale_fill_brewer(palette = "Set2") +
    theme(text = element_text(size = 14),
          axis.text.x = element_text(size = 0)) +
    labs(title = "Mean rating by swatch and condition: US \n")
  print(swatchsum_plot_us)

# histograms by picture (faceted by condition)
for(swatch in levels(d_tidy$swatch)) {
  this_pic = swatch
  g = d_tidy %>%
    filter(swatch == this_pic) %>% 
    ggplot(aes(x = responseCoded, fill = country)) +
      facet_grid(condition ~ country) +
      geom_histogram(binwidth = 0.5) +
#       coord_cartesian(xlim = c(-3:4),
#                       ylim = c(0, 10)) +
      theme_bw() +
      scale_fill_brewer(palette = "Set2") +
      theme(text = element_text(size = 20),
            legend.position = "none") +
      labs(title = paste0(this_pic, "\n"))
  print(g)
}

# relationship between mean and sd by picture
swatch_summary %>% 
  ggplot(aes(x = mean, y = sd, colour = condition)) +
  facet_grid(country ~ condition) +
  geom_jitter() +
  geom_smooth(method = loess) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Mean vs. standard deviation for individual swatches,\nby country and condition\n")

# histograms for means and sds by picture
  # ... means
  ggplot(filter(swatch_summary, condition == "animal"), aes(x = mean)) +
    geom_histogram() +
    coord_cartesian(xlim = c(-3:3)) +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none") +
    labs(title = "Histogram of mean animal\nratings across pictures\n")
  # ... sds
  ggplot(filter(swatch_summary, condition == "animal"), aes(x = sd)) +
    geom_histogram() +
    coord_cartesian() +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none") +
    labs(title = "Histogram of sds of animal ratings\nacross pictures\n")

# look at all ratings sorted by animal ratings
  # ... us (sorted by us rating)
  ratings_us = animal_ratings %>% filter(country == "us") %>%
    ggplot(aes(x = reorder(swatch, animal_rating_us), y = mean, fill = condition)) +
    facet_wrap(~ condition, ncol = 2) +
    geom_bar(stat = "identity", position = "identity", width = 0.5) +
  #   geom_errorbar(aes(ymin = mean - 2*sd/sqrt(n),
  #                     ymax = mean + 2*sd/sqrt(n),
  #                     width = 0.1)) +
    coord_cartesian(ylim = c(-3, 3)) +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none",
          axis.text.x = element_blank()) +
#           axis.text.x = element_text(angle = 60,
#                                      hjust = 1)) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Mean ratings by picture (sorted by US animal rating): US\n",
         x = "Pictures (sorted by US animal rating)")
  ratings_us

# ... india (sorted by us rating)
ratings_ind2 = animal_ratings %>% filter(country == "india") %>%
  ggplot(aes(x = reorder(swatch, animal_rating_us), y = mean, fill = condition)) +
  facet_wrap(~ condition, ncol = 2) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  #   geom_errorbar(aes(ymin = mean - 2*sd/sqrt(n),
  #                     ymax = mean + 2*sd/sqrt(n),
  #                     width = 0.1)) +
  coord_cartesian(ylim = c(-3, 3)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_blank()) +
  #             axis.text.x = element_text(angle = 60,
  #                                        hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Mean ratings by picture (sorted by US animal rating): INDIA\n",
       x = "Pictures (sorted by US animal rating)")
ratings_ind2

  # ... india (sorted by indian rating)
  ratings_ind1 = animal_ratings %>% filter(country == "india") %>%
    ggplot(aes(x = reorder(swatch, animal_rating_ind), y = mean, fill = condition)) +
    facet_wrap(~ condition, ncol = 2) +
    geom_bar(stat = "identity", position = "identity", width = 0.5) +
    #   geom_errorbar(aes(ymin = mean - 2*sd/sqrt(n),
    #                     ymax = mean + 2*sd/sqrt(n),
    #                     width = 0.1)) +
    coord_cartesian(ylim = c(-3, 3)) +
    theme_bw() +
    theme(text = element_text(size = 20),
          legend.position = "none",
          axis.text.x = element_blank()) +
#             axis.text.x = element_text(angle = 60,
#                                        hjust = 1)) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Mean ratings by picture (sorted by Indian animal rating): INDIA\n",
         x = "Pictures (sorted by Indian animal rating)")
  ratings_ind1

# # --- REGRESSIONS -----------------------------------
# 
# # look at straight-up contrasts
# contrasts(d_tidy$condition) -> contrasts_default
# 
# contrasts_orth = cbind(bio.psych = c(4, -3, -3, 4, 4, -3, -3),
#                        animal.hungrpain = c(2, 0, 0, -1, -1, 0, 0),
#                        hungr.pain = c(0, 0, 0, 1, -1, 0, 0),
#                        experien.think = c(0, 1, 1, 0, 0, 1, -3),
#                        affect.sense = c(0, 1, 1, 0, 0, -2, 0),
#                        happy.feel = c(0, -1, 1, 0, 0, 0, 0))
# contrasts(d_tidy$condition) = contrasts_orth
# 
# r1 = lmer(responseCoded ~ condition + (1 | worker_id) + (1 | swatch), d_tidy); summary(r1)
# r2 = lmer(responseCoded ~ country + condition + (1 | worker_id) + (1 | swatch), d_tidy); summary(r2)
# r3 = lmer(responseCoded ~ country * condition + (1 | worker_id) + (1 | swatch), d_tidy); summary(r3)
# anova(r3, r2, r1)
# 
# # look at animal rating as covariate
# 
# d_noanim = d_tidy %>% 
#   full_join(animalrat_us) %>%
#   full_join(animalrat_ind) %>%
#   filter(condition != "animal") %>%
#   mutate(condition = factor(condition),
#          animal_rating_own = ifelse(country == "us",
#                                     animal_rating_us,
#                                     animal_rating_ind))
# View(d_noanim)
# 
# contrasts_default2 = contrasts(d_noanim$condition); contrasts_default2
# 
# contrasts_orth2 = cbind(bio.psych = c(-1, -1, 2, 2, -1, -1),
#                        hungr.pain = c(0, 0, 1, -1, 0, 0),
#                        experien.think = c(1, 1, 0, 0, 1, -3),
#                        affect.sense = c(1, 1, 0, 0, -2, 0),
#                        happy.feel = c(-1, 1, 0, 0, 0, 0))
# contrasts(d_noanim$condition) = contrasts_orth2
# 
# s1 = lmer(responseCoded ~ country + condition + animal_rating_own + (1 | worker_id) + (1 | swatch), d_noanim); summary(s1)
# s2 = lmer(responseCoded ~ country + (condition * animal_rating_own) + (1 | worker_id) + (1 | swatch), d_noanim); summary(s2)
# s3 = lmer(responseCoded ~ country  * condition * animal_rating_own + (1 | worker_id) + (1 | swatch), d_noanim); summary(s3)
# s4 = lmer(responseCoded ~ country  * condition * poly(animal_rating_own, 2) + (1 | worker_id) + (1 | swatch), d_noanim); summary(s4)
# anova(s1, s2, s3, s4)
# 
# d_tidy %>% 
#   group_by(country, condition) %>%
#   summarise(mean = mean(responseCoded))
# 
# # us
# us_bio = mean(-1.106, -0.497)
# us_psych = > mean(-0.432, -0.588, -0.656, -0.743)
# us_bio.psych = us_bio - us_psych


r1 = lm(mean ~ poly(selfrank, 1) + country, animal_ratings2); summary(r1)
r2 = lm(mean ~ poly(selfrank, 2) + country, animal_ratings2); summary(r2)
r3 = lm(mean ~ poly(selfrank, 3) + country, animal_ratings2); summary(r3)
r4 = lm(mean ~ poly(selfrank, 1) * country, animal_ratings2); summary(r4)
r5 = lm(mean ~ poly(selfrank, 2) * country, animal_ratings2); summary(r5)
r6 = lm(mean ~ poly(selfrank, 3) * country, animal_ratings2); summary(r6)

anova(r1, r4, r5, r6)


