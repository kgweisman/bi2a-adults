# plot, sorted by own animal ratings
plot_india = d_analysis %>%
  filter(country == "india") %>%
  ggplot(aes(x = animal_selfrank, y = mean, label = n)) +
  facet_wrap(~ condition) +
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
  labs(title = "Mean scaled responses, by condition: Indian adults\n",
       x = "Pictures (sorted by mean Indian adult response to ANIMAL)") +
  stat_smooth(aes(group = 1))
plot_india
  

# plot, sorted by own animal ratings
plot_us = d_analysis %>%
  filter(country == "us") %>%
  ggplot(aes(x = animal_selfrank, y = mean, label = n)) +
  facet_wrap(~ condition) +
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
  labs(title = "Mean scaled responses, by condition: US adults\n",
       x = "Pictures (sorted by mean US adult response to ANIMAL)") +
  stat_smooth(aes(group = 1))
plot_us







#############



plot_india = d_analysis %>%
  filter(country == "india") %>%
  filter(condition == "animal") %>%
  ggplot(aes(x = reorder(swatch, animal_selfrank), y = mean, label = n)) +
  #   facet_wrap(~ condition) +
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
  labs(title = "Mean scaled responses, by condition: Indian adults\n",
       x = "Pictures (sorted by mean Indian adult response to ANIMAL)") +
  stat_smooth(aes(group = 1))
plot_india

plot_us = d_analysis %>%
  filter(country == "us") %>%
  filter(condition == "animal") %>%
  ggplot(aes(x = reorder(swatch, animal_selfrank), y = mean, label = n)) +
  #   facet_wrap(~ condition) +
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
  labs(title = "Mean scaled responses, by condition: usn adults\n",
       x = "Pictures (sorted by mean usn adult response to ANIMAL)") +
  stat_smooth(aes(group = 1))
plot_us
