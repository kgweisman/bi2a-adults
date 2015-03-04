# --- TABLES & SUMMARIES -----------------------------------

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

# summarize by condition
condition_summary = d_tidy %>%
  group_by(country, condition) %>%
  summarise(mean = mean(responseCoded, na.rm = T),
            sd = sd(responseCoded, na.rm = T),
            n = length(responseCoded)/48)
View(condition_summary)

# summarize by swatch and condition
swatch_summary = d_tidy %>%
  group_by(country, swatch, condition) %>%
  summarise(mean = mean(responseCoded, na.rm = T),
            sd = sd(responseCoded, na.rm = T),
            n = length(responseCoded))
View(swatch_summary)

# add animal ratings to swatch_summary
animal_ratings = swatch_summary %>%
  select(-sd, -n) %>%
  spread(condition, mean) %>%
  arrange(animal) %>%
  select(country, swatch, animal) %>%
  full_join(swatch_summary)
View(animal_ratings)

animalrat_us = animal_ratings %>%
  filter(country == "us") %>%
  select(swatch, animal_rating_us = animal) %>%
  distinct()
View(animalrat_us)

animalrat_ind = animal_ratings %>%
  filter(country == "india") %>%
  select(swatch, animal_rating_ind = animal) %>%
  distinct()
View(animalrat_ind)

animal_ratings = animal_ratings %>%
  full_join(animalrat_us) %>% 
  full_join(animalrat_ind) %>%
  select(-animal)
View(animal_ratings)

# look at correlations of means (odd)
d_swatches = d_tidy %>%
  select(country, condition, worker_id, swatch, responseCoded) %>%
  #   spread(condition, responseCoded) %>%
  group_by(country, swatch, condition) %>%
  summarise(mean = mean(responseCoded)) %>%
  spread(condition, mean)

cor_india = d_swatches %>% 
  filter(country == "india") %>%
  select (-swatch, -country) %>% 
  cor()
cor_india

cor_us = d_swatches %>% 
  filter(country == "us") %>%
  select (-swatch, -country) %>% 
  cor()
cor_us

# --- PLOTS -----------------------------------

# overal mean ratings by condition and country
condsum_plot = condition_summary %>%
  ggplot(aes(x = condition, y = mean, fill = condition)) +
  facet_wrap(~ country) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean - sd,
                    ymax = mean  + sd,
                    width = 0.1)) +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Mean rating by condition and country\nError bars: standard deviation\n")
print(condsum_plot)

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



# --- REGRESSIONS -----------------------------------

# look at straight-up contrasts
contrasts(d_tidy$condition) -> contrasts_default

contrasts_orth = cbind(bio.psych = c(4, -3, -3, 4, 4, -3, -3),
                       animal.hungrpain = c(2, 0, 0, -1, -1, 0, 0),
                       hungr.pain = c(0, 0, 0, 1, -1, 0, 0),
                       experien.think = c(0, 1, 1, 0, 0, 1, -3),
                       affect.sense = c(0, 1, 1, 0, 0, -2, 0),
                       happy.feel = c(0, -1, 1, 0, 0, 0, 0))
contrasts(d_tidy$condition) = contrasts_orth

r1 = lmer(responseCoded ~ condition + (1 | worker_id) + (1 | swatch), d_tidy); summary(r1)
r2 = lmer(responseCoded ~ country + condition + (1 | worker_id) + (1 | swatch), d_tidy); summary(r2)
r3 = lmer(responseCoded ~ country * condition + (1 | worker_id) + (1 | swatch), d_tidy); summary(r3)
anova(r3, r2, r1)

# look at animal rating as covariate

d_noanim = d_tidy %>% 
  full_join(animalrat_us) %>%
  full_join(animalrat_ind) %>%
  filter(condition != "animal") %>%
  mutate(condition = factor(condition),
         animal_rating_own = ifelse(country == "us",
                                    animal_rating_us,
                                    animal_rating_ind))
View(d_noanim)

contrasts_default2 = contrasts(d_noanim$condition); contrasts_default2

contrasts_orth2 = cbind(bio.psych = c(-1, -1, 2, 2, -1, -1),
                       hungr.pain = c(0, 0, 1, -1, 0, 0),
                       experien.think = c(1, 1, 0, 0, 1, -3),
                       affect.sense = c(1, 1, 0, 0, -2, 0),
                       happy.feel = c(-1, 1, 0, 0, 0, 0))
contrasts(d_noanim$condition) = contrasts_orth2

s1 = lmer(responseCoded ~ country + condition + animal_rating_own + (1 | worker_id) + (1 | swatch), d_noanim); summary(s1)
s2 = lmer(responseCoded ~ country + (condition * animal_rating_own) + (1 | worker_id) + (1 | swatch), d_noanim); summary(s2)
s3 = lmer(responseCoded ~ country  * condition * animal_rating_own + (1 | worker_id) + (1 | swatch), d_noanim); summary(s3)
s4 = lmer(responseCoded ~ country  * condition * poly(animal_rating_own, 2) + (1 | worker_id) + (1 | swatch), d_noanim); summary(s4)
anova(s1, s2, s3, s4)


