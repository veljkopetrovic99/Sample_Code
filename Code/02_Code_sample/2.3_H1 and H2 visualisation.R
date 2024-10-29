#VISUALISATION of H1 and H2

# Plot for Average Absolute Deviation
plot_absolute_deviation <- ggplot(data_summary, aes(x = Condition, y = Average_Absolute_Deviation, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Average_Absolute_Deviation - SE_Absolute_Deviation, ymax = Average_Absolute_Deviation + SE_Absolute_Deviation), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Average Absolute Deviation by Condition", x = "Condition", y = "Average Absolute Deviation") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 20), 
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 20), 
    legend.text = element_text(size = 20)
  )

# Plot for Average Confidence
plot_confidence <- ggplot(data_summary, aes(x = Condition, y = Average_Confidence, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Average_Confidence - SE_Confidence, ymax = Average_Confidence + SE_Confidence), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Average Confidence by Condition", x = "Condition", y = "Average Confidence") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 20), 
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 20), 
    legend.text = element_text(size = 20)
  )

# Print the plots
print(plot_absolute_deviation)
print(plot_confidence)


