# plot, sorted by own ratings
animal_india = animal_ratings %>%
  filter(country == "india" & condition == "animal") %>%
  ggplot(aes(x = reorder(swatch, mean), y = mean, label = n)) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean - 2*sd/sqrt(n),
                    ymax = mean + 2*sd/sqrt(n),
                    width = 0.1)) +
#   geom_text(vjust = -1,
#             colour = "red") +
  theme_bw() +
  coord_cartesian(ylim = c(-3,3)) +
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  labs(title = "Mean scaled responses to ANIMAL, by picture: Indian adults\n",
       x = "Pictures (sorted by mean Indian adult response)")
animal_india

# plot, sorted by own ratings
animal_us = animal_ratings %>%
  filter(country == "us" & condition == "animal") %>%
  ggplot(aes(x = reorder(swatch, mean), y = mean, label = n)) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean - 2*sd/sqrt(n),
                    ymax = mean + 2*sd/sqrt(n),
                    width = 0.1)) +
  #   geom_text(vjust = -1,
  #             colour = "red") +
  theme_bw() +
  coord_cartesian(ylim = c(-3,3)) +
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  labs(title = "Mean scaled responses to ANIMAL, by picture: US adults\n",
       x = "Pictures (sorted by mean US adult response)")
animal_us