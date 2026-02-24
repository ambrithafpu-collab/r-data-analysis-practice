# Oral Microbiome Diversity Analysis
# Comparing Healthy vs HNSCC groups

library(ggplot2)
library(dplyr)

set.seed(123)

# Create structured abundance dataset
data <- data.frame(
  Group = rep(c("Healthy", "HNSCC"), each = 30),
  Fusobacterium = c(rpois(30, 40), rpois(30, 70)),
  Prevotella = c(rpois(30, 35), rpois(30, 60)),
  Streptococcus = c(rpois(30, 65), rpois(30, 40)),
  Rothia = c(rpois(30, 50), rpois(30, 30))
)

# Calculate total counts
data <- data %>%
  mutate(Total = Fusobacterium + Prevotella + Streptococcus + Rothia)

# Calculate relative abundance
data <- data %>%
  mutate(
    Fusobacterium_rel = Fusobacterium / Total,
    Prevotella_rel = Prevotella / Total,
    Streptococcus_rel = Streptococcus / Total,
    Rothia_rel = Rothia / Total
  )

# Shannon diversity function
shannon <- function(x) {
  -sum(x * log(x), na.rm = TRUE)
}

data$Shannon <- apply(
  data[, c("Fusobacterium_rel", "Prevotella_rel",
           "Streptococcus_rel", "Rothia_rel")],
  1,
  shannon
)

# T-test for diversity difference
t.test(Shannon ~ Group, data = data)

# Boxplot of Shannon diversity
ggplot(data, aes(x = Group, y = Shannon, fill = Group)) +
  geom_boxplot() +
  labs(title = "Shannon Diversity Comparison: Healthy vs HNSCC",
       y = "Shannon Index")

