# Load the necessary library
library(ggplot2)
library(dplyr)

df_orig <- read.csv("resultados.csv")

#df <- df_orig[df_orig$rank == 1, ]
df<-df_orig

# Calculate the average score for each cut
avg_scores <- df %>%
  group_by(corte) %>%
  summarise(avg_score = mean(publicScore, na.rm = TRUE))

# Create the plot
#p <- ggplot(df, aes(x = corte, y = publicScore, color = as.factor(semilla))) +
p <- ggplot(df, aes(x = corte, y = publicScore, color = interaction(rank, semilla))) +
  geom_line() +
  labs(title = "",
       x = "Corte",
       y = "Puntaje",
       color = "Semilla") +
  theme_minimal()

#p <- p + geom_line(data = avg_scores, aes(x = corte, y = avg_score), 
#                   color = "black", linetype = "dashed", size = 1.2)

# Save the plot as a PDF
ggsave("puntajes.pdf", plot = p)
