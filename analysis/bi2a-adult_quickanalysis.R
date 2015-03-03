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

# plot histos by picture (faceted by condition)
for(swatch in levels(d_tidy$swatch)) {
  this_pic = swatch
  g = d_tidy %>%
    filter(swatch == this_pic) %>% 
    ggplot(aes(x = responseCoded, fill = swatch)) +
      facet_wrap(~ condition) +
      geom_histogram(binwidth = 0.5) +
      coord_cartesian(xlim = c(1:8),
                      ylim = c(0, 30)) +
      theme_bw() +
      scale_fill_brewer(palette = "Set2") +
      theme(text = element_text(size = 20),
            legend.position = "none") +
      labs(title = paste0(this_pic, "\n"))
  print(g)
}

# plot regressions
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
ggplot(ratings_summary, aes(x = mean, y = sd)) +
  geom_jitter() +
  geom_smooth(method = loess) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Animal ratings by picture:\nmean vs. standard deviation\n")

# look at histograms for means and sds
# ... means
ggplot(filter(summary_table, dimension == "animal"), aes(x = mean_rating)) +
  geom_histogram() +
  coord_cartesian(xlim = c(1:7)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Histogram of mean animal\nratings across pictures\n")

# ... sds
ggplot(filter(summary_table, dimension == "animal"), aes(x = sd_rating)) +
  geom_histogram() +
  coord_cartesian() +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  labs(title = "Histogram of sds of animal ratings\nacross pictures\n")

# look at mean animal ratings, sorted
m = summary_table %>%
  filter(dimension == "animal") %>%
  ggplot(aes(x = reorder(picture, mean_rating), y = mean_rating, fill = "a")) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_rating - sd_rating/sqrt(30),
                    ymax = mean_rating + sd_rating/sqrt(30),
                    width = 0.1)) +
  coord_cartesian(ylim = c(1, 7)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none",
        axis.text.x = element_text(angle = 60,
                                   hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Mean ratings by picture (sorted)\n",
       xlab = "Pictures (sorted)")
m
