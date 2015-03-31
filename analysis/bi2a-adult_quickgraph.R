# plot, sorted by US adult ratings
ratings2_animal_scaled = animal_ratings %>%
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
  labs(title = "Mean binary responses to ANIMAL, by picture: Adults\n",
       x = "Pictures (sorted by mean US adult response)")
ratings2_animal_scaled