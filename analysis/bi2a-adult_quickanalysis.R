# count observations
n_india = d_tidy %>%
  filter(country == "india") %>%
  count(worker_id) %>%
  nrow()

n_us = d_tidy %>%
  filter(country == "us") %>%
  count(worker_id) %>%
  nrow()

# summarize by condition
condition_summary = d_tidy %>%
  group_by(country, condition) %>%
  summarise(mean = mean(responseCoded, na.rm = T),
            sd = sd(responseCoded, na.rm = T),
            n = length(responseCoded)/48)
View(condition_summary)

# plot
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

# summarize by swatch and condition
swatch_summary = d_tidy %>%
  group_by(country, swatch, condition) %>%
  summarise(mean = mean(responseCoded, na.rm = T),
            sd = sd(responseCoded, na.rm = T),
            n = length(responseCoded)/48)
View(swatch_summary)

# plots
# # ... india
# swatchsum_plot_ind = swatch_summary %>%
#   filter(country == "india") %>%
#   ggplot(aes(x = condition, y = mean, fill = condition)) +
#   facet_wrap( ~ swatch, ncol = 6) +
#   geom_bar(stat = "identity", position = "identity", width = 0.5) +
#   geom_errorbar(aes(ymin = mean - sd/sqrt(n),
#                     ymax = mean  + sd/sqrt(n),
#                     width = 0.1)) +
#   theme_bw() +
#   scale_fill_brewer(palette = "Set2") +
#   theme(text = element_text(size = 14),
#         axis.text.x = element_text(size = 0)) +
#   labs(title = "Mean rating by swatch and condition: INDIA \n")
# print(swatchsum_plot_ind)
# 
# # ... us
# swatchsum_plot_us = swatch_summary %>%
#   filter(country == "us") %>%
#   ggplot(aes(x = condition, y = mean, fill = condition)) +
#   facet_wrap( ~ swatch, ncol = 6) +
#   geom_bar(stat = "identity", position = "identity", width = 0.5) +
#   geom_errorbar(aes(ymin = mean - sd/sqrt(n),
#                     ymax = mean  + sd/sqrt(n),
#                     width = 0.1)) +
#   theme_bw() +
#   scale_fill_brewer(palette = "Set2") +
#   theme(text = element_text(size = 14),
#         axis.text.x = element_text(size = 0)) +
#   labs(title = "Mean rating by swatch and condition: US \n")
# print(swatchsum_plot_us)

# plot histos by picture (faceted by condition)
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

# look at correlations of means (odd)
d_swatches = d_tidy %>%
  select(country, condition, worker_id, swatch, responseCoded) %>%
#   spread(condition, responseCoded) %>%
  group_by(country, swatch, condition) %>%
  summarise(mean = mean(responseCoded)) %>%
  spread(condition, mean)

View(d_swatches)

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

# score pictures for selection

# look at relationship between mean and sd by picture
swatch_summary %>% 
  ggplot(aes(x = mean, y = sd, colour = condition)) +
  facet_grid(country ~ condition) +
  geom_jitter() +
  geom_smooth(method = loess) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Mean vs. standard deviation for individual swatches,\nby country and condition\n")

# # look at histograms for means and sds
# # ... means
# ggplot(filter(swatch_summary, condition == "animal"), aes(x = mean)) +
#   geom_histogram() +
#   coord_cartesian(xlim = c(-3:3)) +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         legend.position = "none") +
#   labs(title = "Histogram of mean animal\nratings across pictures\n")
# 
# # ... sds
# ggplot(filter(swatch_summary, condition == "animal"), aes(x = sd)) +
#   geom_histogram() +
#   coord_cartesian() +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         legend.position = "none") +
#   labs(title = "Histogram of sds of animal ratings\nacross pictures\n")

# add animal ratings to swatch_summary
animal_ratings = swatch_summary %>%
  select(-sd, -n) %>%
  spread(condition, mean) %>%
  arrange(animal) %>%
  select(country, swatch, animal) %>%
  full_join(swatch_summary)
View(animal_ratings)

# look at all ratings sorted by animal ratings
ratings_us = animal_ratings %>% filter(country == "us") %>%
  ggplot(aes(x = reorder(swatch, animal), y = mean, fill = condition)) +
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
#         axis.text.x = element_text(angle = 60,
#                                    hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Mean ratings by picture (sorted): US\n",
       x = "Pictures (sorted by animal rating)")
ratings_us

# look at all ratings sorted by animal ratings
ratings_ind = animal_ratings %>% filter(country == "india") %>%
  ggplot(aes(x = reorder(swatch, animal), y = mean, fill = condition)) +
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
  #         axis.text.x = element_text(angle = 60,
  #                                    hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Mean ratings by picture (sorted): INDIA\n",
       x = "Pictures (sorted by animal rating)")
ratings_ind
