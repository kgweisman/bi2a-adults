n = 39

# summarize by condition
condition_summary = d_tidy %>%
  group_by(condition) %>%
#   group_by(swatch) %>%
  summarise(mean = mean(responseCoded, na.rm = T),
            sd = sd(responseCoded, na.rm = T),
            n = length(responseCoded)/48)
View(condition_summary)

# summarize by condition and swatch
swatch_summary = d_tidy %>%
  group_by(swatch, condition) %>%
  summarise(mean = mean(responseCoded, na.rm = T),
            sd = sd(responseCoded, na.rm = T),
            n = length(responseCoded)/48)
View(swatch_summary)

# make wide version
swatch_summary_wide = swatch_summary %>%
  select(-sd, -n) %>%
  spread(condition, mean) %>%
  arrange(animal)
View(swatch_summary_wide)

# plot histos by picture (faceted by condition)
for(swatch in levels(d_tidy$swatch)) {
  this_pic = swatch
  g = d_tidy %>%
    filter(swatch == this_pic) %>% 
    ggplot(aes(x = responseCoded, fill = swatch)) +
      facet_wrap(~ condition) +
      geom_histogram(binwidth = 0.5) +
      coord_cartesian(xlim = c(-3:4),
                      ylim = c(0, 10)) +
      theme_bw() +
      scale_fill_brewer(palette = "Set1") +
      theme(text = element_text(size = 20),
            legend.position = "none") +
      labs(title = paste0(this_pic, "\n"))
  print(g)
}

# look at correlations of means (odd)
d_swatches = d_tidy %>%
  select(condition, worker_id, swatch, responseCoded) %>%
#   spread(condition, responseCoded) %>%
  group_by(swatch, condition) %>%
  summarise(mean = mean(responseCoded)) %>%
  spread(condition, mean)

View(d_swatches)

d_swatches %>% select (-swatch) %>% cor()

# score pictures for selection

# look at relationship between mean and sd by picture
ggplot(swatch_summary, aes(x = mean, y = sd)) +
  geom_jitter() +
  geom_smooth(method = loess) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Animal ratings by picture:\nmean vs. standard deviation\n")

# look at histograms for means and sds
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

# look at mean animal ratings, sorted
animal = swatch_summary_wide %>%
  ggplot(aes(x = reorder(swatch, animal), y = animal, fill = "a")) +
#   facet_wrap(~ condition) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
#   geom_errorbar(aes(ymin = mean - sd/sqrt(n),
#                     ymax = mean  + sd/sqrt(n),
#                     width = 0.1)) +
  coord_cartesian(ylim = c(-3, 3)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Mean ANIMAL ratings by picture (sorted)\n",
       x = "Pictures (sorted by animal rating)")
animal

think = swatch_summary_wide %>%
  ggplot(aes(x = reorder(swatch, animal), y = think, fill = "a")) +
  #   facet_wrap(~ condition) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  #   geom_errorbar(aes(ymin = mean - sd/sqrt(n),
  #                     ymax = mean  + sd/sqrt(n),
  #                     width = 0.1)) +
  coord_cartesian(ylim = c(-3, 3)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Mean THINK ratings by picture (sorted)\n",
       x = "Pictures (sorted by animal rating)")
think

happy = swatch_summary_wide %>%
  ggplot(aes(x = reorder(swatch, animal), y = happy, fill = "a")) +
  #   facet_wrap(~ condition) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  #   geom_errorbar(aes(ymin = mean - sd/sqrt(n),
  #                     ymax = mean  + sd/sqrt(n),
  #                     width = 0.1)) +
  coord_cartesian(ylim = c(-3, 3)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Mean HAPPY ratings by picture (sorted)\n",
       x = "Pictures (sorted by animal rating)")
happy

feelings = swatch_summary_wide %>%
  ggplot(aes(x = reorder(swatch, animal), y = feelings, fill = "a")) +
  #   facet_wrap(~ condition) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  #   geom_errorbar(aes(ymin = mean - sd/sqrt(n),
  #                     ymax = mean  + sd/sqrt(n),
  #                     width = 0.1)) +
  coord_cartesian(ylim = c(-3, 3)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Mean FEELINGS ratings by picture (sorted)\n",
       x = "Pictures (sorted by animal rating)")
feelings

sense = swatch_summary_wide %>%
  ggplot(aes(x = reorder(swatch, animal), y = sense, fill = "a")) +
  #   facet_wrap(~ condition) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  #   geom_errorbar(aes(ymin = mean - sd/sqrt(n),
  #                     ymax = mean  + sd/sqrt(n),
  #                     width = 0.1)) +
  coord_cartesian(ylim = c(-3, 3)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Mean SENSE ratings by picture (sorted)\n",
       x = "Pictures (sorted by animal rating)")
sense

hungry = swatch_summary_wide %>%
  ggplot(aes(x = reorder(swatch, animal), y = hungry, fill = "a")) +
  #   facet_wrap(~ condition) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  #   geom_errorbar(aes(ymin = mean - sd/sqrt(n),
  #                     ymax = mean  + sd/sqrt(n),
  #                     width = 0.1)) +
  coord_cartesian(ylim = c(-3, 3)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Mean HUNGRY ratings by picture (sorted)\n",
       x = "Pictures (sorted by animal rating)")
hungry

pain = swatch_summary_wide %>%
  ggplot(aes(x = reorder(swatch, animal), y = pain, fill = "a")) +
  #   facet_wrap(~ condition) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  #   geom_errorbar(aes(ymin = mean - sd/sqrt(n),
  #                     ymax = mean  + sd/sqrt(n),
  #                     width = 0.1)) +
  coord_cartesian(ylim = c(-3, 3)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Mean PAIN ratings by picture (sorted)\n",
       x = "Pictures (sorted by animal rating)")
pain
