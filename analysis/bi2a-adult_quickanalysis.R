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
d_tidy_01 = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/data/run-india-and-us_01.csv", fileEncoding = "latin1")

# RUN 02
d_tidy_02 = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/data/run-india-and-us_02_hand-coded.csv", fileEncoding = "latin1")

# filter out by comp_filter
d_tidy_02 = d_tidy_02 %>%
  filter(comp_filter == "keep")

# RUN 03
d_tidy_03 = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/BI2A/bi2a-adults/data/run-india-and-us_03_hand-coded.csv", fileEncoding = "latin1")

# filter out by comp_filter
d_tidy_03 = d_tidy_03 %>%
  filter(comp_filter == "keep")

# SET WHICH DATA TO ANALYZE
# d_tidy = d_tidy_01
# d_tidy = d_tidy_02
# d_tidy = d_tidy_03
d_tidy = full_join(d_tidy_02, d_tidy_03) %>%
  mutate(condition = factor(condition))

# --- DEMOGRAPHICS ------------------------------------------------------------
# country
d_tidy %>%
  select(country, worker_id) %>%
  distinct() %>%
  count(country)

# condition
d_tidy %>% 
  select(country, worker_id, condition) %>%
  distinct() %>%
  count(country, condition)

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

# add ratings to swatch_summary
all_ratings = swatch_summary %>%
  select(-sd, -n) %>%
  spread(condition, mean) %>%
  arrange(animal) %>%
  select(country, swatch, animal) %>%
  full_join(swatch_summary) %>%
  select(-animal)
# View(all_ratings)

temp_us = all_ratings %>%
  filter(country == "us") %>%
  select(-sd, -n) %>%
  spread(condition, mean) %>%
  transmute(swatch = swatch,
            animal_rat_us = animal,
            emotions_rat_us = emotions,
            hungry_rat_us = hungry,
            pain_rat_us = pain,
            sense_rat_us = sense,
            think_rat_us = think) %>%
  mutate(animal_rank_us = rank(animal_rat_us),
         emotions_rank_us = rank(emotions_rat_us),
         hungry_rank_us = rank(hungry_rat_us),
         pain_rank_us = rank(pain_rat_us),
         sense_rank_us = rank(sense_rat_us),
         think_rank_us = rank(think_rat_us))

temp_india = all_ratings %>%
  filter(country == "india") %>%
  select(-sd, -n) %>%
  spread(condition, mean) %>%
  transmute(swatch = swatch,
            animal_rat_india = animal,
            emotions_rat_india = emotions,
            hungry_rat_india = hungry,
            pain_rat_india = pain,
            sense_rat_india = sense,
            think_rat_india = think) %>%
  mutate(animal_rank_india = rank(animal_rat_india),
         emotions_rank_india = rank(emotions_rat_india),
         hungry_rank_india = rank(hungry_rat_india),
         pain_rank_india = rank(pain_rat_india),
         sense_rank_india = rank(sense_rat_india),
         think_rank_india = rank(think_rat_india))

temp = full_join(temp_us, temp_india)

d_analysis = all_ratings %>%
  full_join(temp) %>% 
  mutate(animal_selfrat = ifelse(country == "india", animal_rat_india,
                                 ifelse(country == "us", animal_rat_us,
                                        "NA")),
         emotions_selfrat = ifelse(country == "india", emotions_rat_india,
                                 ifelse(country == "us", emotions_rat_us,
                                        "NA")),
         hungry_selfrat = ifelse(country == "india", hungry_rat_india,
                                 ifelse(country == "us", hungry_rat_us,
                                        "NA")),
         pain_selfrat = ifelse(country == "india", pain_rat_india,
                                   ifelse(country == "us", pain_rat_us,
                                          "NA")),
         sense_selfrat = ifelse(country == "india", sense_rat_india,
                                 ifelse(country == "us", sense_rat_us,
                                        "NA")),
         think_selfrat = ifelse(country == "india", think_rat_india,
                                   ifelse(country == "us", think_rat_us,
                                          "NA"))) %>%
  mutate(animal_selfrank = ifelse(country == "india", animal_rank_india,
                                 ifelse(country == "us", animal_rank_us,
                                        "NA")),
         emotions_selfrank = ifelse(country == "india", emotions_rank_india,
                                   ifelse(country == "us", emotions_rank_us,
                                          "NA")),
         hungry_selfrank = ifelse(country == "india", hungry_rank_india,
                                 ifelse(country == "us", hungry_rank_us,
                                        "NA")),
         pain_selfrank = ifelse(country == "india", pain_rank_india,
                               ifelse(country == "us", pain_rank_us,
                                      "NA")),
         sense_selfrank = ifelse(country == "india", sense_rank_india,
                                ifelse(country == "us", sense_rank_us,
                                       "NA")),
         think_selfrank = ifelse(country == "india", think_rank_india,
                                ifelse(country == "us", think_rank_us,
                                       "NA"))) %>%
  mutate(animal_selfrat = as.numeric(animal_selfrat),
         emotions_selfrat = as.numeric(emotions_selfrat),
         hungry_selfrat = as.numeric(hungry_selfrat),
         pain_selfrat = as.numeric(pain_selfrat),
         sense_selfrat = as.numeric(sense_selfrat),
         think_selfrat = as.numeric(think_selfrat),
         animal_selfrank = as.numeric(animal_selfrank),
         emotions_selfrank = as.numeric(emotions_selfrank),
         hungry_selfrank = as.numeric(hungry_selfrank),
         pain_selfrank = as.numeric(pain_selfrank),
         sense_selfrank = as.numeric(sense_selfrank),
         think_selfrank = as.numeric(think_selfrank))

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

condsum_plot2 = d_analysis %>%
  ggplot(aes(x = condition, y = mean, fill = condition)) +
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
#   # ... us (sorted by us rating)
#   ratings_us = animal_ratings %>% filter(country == "us") %>%
#     ggplot(aes(x = reorder(swatch, animal_rating_us), y = mean, fill = condition)) +
#     facet_wrap(~ condition, ncol = 2) +
#     geom_bar(stat = "identity", position = "identity", width = 0.5) +
#   #   geom_errorbar(aes(ymin = mean - 2*sd/sqrt(n),
#   #                     ymax = mean + 2*sd/sqrt(n),
#   #                     width = 0.1)) +
#     coord_cartesian(ylim = c(-3, 3)) +
#     theme_bw() +
#     theme(text = element_text(size = 20),
#           legend.position = "none",
#           axis.text.x = element_blank()) +
# #           axis.text.x = element_text(angle = 60,
# #                                      hjust = 1)) +
#     scale_fill_brewer(palette = "Set2") +
#     labs(title = "Mean ratings by picture (sorted by US animal rating): US\n",
#          x = "Pictures (sorted by US animal rating)")
#   ratings_us
# 
# # ... india (sorted by us rating)
# ratings_ind2 = animal_ratings %>% filter(country == "india") %>%
#   ggplot(aes(x = reorder(swatch, animal_rating_us), y = mean, fill = condition)) +
#   facet_wrap(~ condition, ncol = 2) +
#   geom_bar(stat = "identity", position = "identity", width = 0.5) +
#   #   geom_errorbar(aes(ymin = mean - 2*sd/sqrt(n),
#   #                     ymax = mean + 2*sd/sqrt(n),
#   #                     width = 0.1)) +
#   coord_cartesian(ylim = c(-3, 3)) +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         legend.position = "none",
#         axis.text.x = element_blank()) +
#   #             axis.text.x = element_text(angle = 60,
#   #                                        hjust = 1)) +
#   scale_fill_brewer(palette = "Set2") +
#   labs(title = "Mean ratings by picture (sorted by US animal rating): INDIA\n",
#        x = "Pictures (sorted by US animal rating)")
# ratings_ind2
# 
#   # ... india (sorted by indian rating)
#   ratings_ind1 = animal_ratings %>% filter(country == "india") %>%
#     ggplot(aes(x = reorder(swatch, animal_rating_ind), y = mean, fill = condition)) +
#     facet_wrap(~ condition, ncol = 2) +
#     geom_bar(stat = "identity", position = "identity", width = 0.5) +
#     #   geom_errorbar(aes(ymin = mean - 2*sd/sqrt(n),
#     #                     ymax = mean + 2*sd/sqrt(n),
#     #                     width = 0.1)) +
#     coord_cartesian(ylim = c(-3, 3)) +
#     theme_bw() +
#     theme(text = element_text(size = 20),
#           legend.position = "none",
#           axis.text.x = element_blank()) +
# #             axis.text.x = element_text(angle = 60,
# #                                        hjust = 1)) +
#     scale_fill_brewer(palette = "Set2") +
#     labs(title = "Mean ratings by picture (sorted by Indian animal rating): INDIA\n",
#          x = "Pictures (sorted by Indian animal rating)")
#   ratings_ind1

# # --- ANALYSIS -----------------------------------
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

# with rankings
r1a = lm(mean ~ poly(animal_selfrank, 1) + condition + country, d_analysis); summary(r1a)
r2a = lm(mean ~ poly(animal_selfrank, 2) + condition + country, d_analysis); summary(r2a)
r3a = lm(mean ~ poly(animal_selfrank, 3) + condition + country, d_analysis); summary(r3a)
r4a = lm(mean ~ poly(animal_selfrank, 1) * condition * country, d_analysis); summary(r4a)
r5a = lm(mean ~ poly(animal_selfrank, 2) * condition * country, d_analysis); summary(r5a)
r6a = lm(mean ~ poly(animal_selfrank, 3) * condition * country, d_analysis); summary(r6a)

anova(r1a, r4a, r5a, r6a)

# look at correlations between countries for rankings and ratings
# with(d_analysis, cor.test(animal_rank_us, animal_rank_india, method = "spearman")) # equivalent to below
with(d_analysis, cor.test(animal_rat_us, animal_rat_india, method = "spearman"))
with(d_analysis, cor.test(animal_rat_us, animal_rat_india, method = "pearson"))

sorted_us = d_analysis %>%
  filter(country == "us") %>%
  select(swatch, animal_rat_us) %>%
  distinct() %>%
  arrange(animal_rat_us)
View(sorted_us)

sorted_india = d_analysis %>%
  filter(country == "india") %>%
  select(swatch, animal_rat_india) %>%
  distinct() %>%
  arrange(animal_rat_india)
View(sorted_india)



# look at correlations between conditions within each country
# look at correlations of means (odd)

cor_india = d_analysis %>% 
  filter(country == "india") %>%
  transmute(animal = as.numeric(animal_selfrat),
            emotions = as.numeric(emotions_selfrat),
            hungry = as.numeric(hungry_selfrat),
            pain = as.numeric(pain_selfrat),
            sense = as.numeric(sense_selfrat),
            think = as.numeric(think_selfrat)) %>%
  cor() %>%
  round(3) %>%
  as.dist()
cor_india

cor_us = d_analysis %>% 
  filter(country == "us") %>%
  transmute(animal = as.numeric(animal_selfrat),
            emotions = as.numeric(emotions_selfrat),
            hungry = as.numeric(hungry_selfrat),
            pain = as.numeric(pain_selfrat),
            sense = as.numeric(sense_selfrat),
            think = as.numeric(think_selfrat)) %>%
  cor() %>%
  round(3) %>%
  as.dist()
cor_us