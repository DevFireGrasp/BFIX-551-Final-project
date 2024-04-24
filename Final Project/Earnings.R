topearning_states <- acs_data %>%
  arrange(desc(median_earnings))


topearning_states %>%
  slice(1:25) %>%  
  ggplot(aes(x = reorder(state, median_earnings), y = median_earnings)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Top Earning States",
       x = "State",
       y = "Median Earnings")