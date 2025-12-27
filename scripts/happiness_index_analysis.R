---
title: "The Impact of Lifestyle Factors on Mental Well-being"
output: pdf_document
date: "2025-12-21"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r}
install.packages("readr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")
library(readr)
library(ggplot2)
library(dplyr)
library(corrplot)
df <- read_csv("Mental_Health_and_Social_Media_Balance_Dataset.csv")

```
```{r}
library(ggplot2)

df1 <- data.frame(
  Platform = c('X (Twitter)', 'LinkedIn', 'TikTok', 'Facebook', 'YouTube', 'Instagram'),
  Score = c(8.65, 8.52, 8.38, 8.35, 8.31, 7.99)
)


ggplot(df1, aes(x = reorder(Platform, Score), y = Score)) +
  geom_bar(stat = "identity", fill = "#74c476") +
  geom_text(aes(label = Score), hjust = -0.2, color = "#238b45", size = 4) +
  coord_flip() +
  ylim(0, 10) +
  labs(title = "Platform-Based Net Happiness Rates", x = "", y = "Happiness Rates (1-10)") +
  theme_minimal()
# ... (önceki grafik kodların) ...
  theme_minimal() +  # Parantez içini boşalt
  theme(
    plot.background = element_rect(fill = "#fefefe", colour = NA),
    panel.background = element_rect(fill = "#fefefe", colour = NA)
  )
```
```{r}

library(ggplot2)

df <- read.csv("Mental_Health_and_Social_Media_Balance_Dataset.csv", check.names = TRUE)

x_min <- floor(min(df$Happiness_Index.1.10., na.rm = TRUE)) 
y_max <- max(df$Daily_Screen_Time.hrs., na.rm = TRUE)

ggplot(df, aes(x = Happiness_Index.1.10., y = Daily_Screen_Time.hrs.)) +

  geom_point(alpha = 0.5, color = "#4c72b0", size = 2.5, position = "jitter") +
  

  geom_smooth(method = "lm", color = "#e41a1c", fill = "#e41a1c", alpha = 0.15) +
  
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  
  coord_cartesian(xlim = c(x_min, 10), ylim = c(0, y_max + 1)) +
  
  labs(
    title = "Happiness Index vs Daily Screen Time",
    subtitle = paste("Focused Happiness Range:", x_min, "-", 10),
    x = "Happiness Index (1-10)",
    y = "Daily Screen Time (hrs)"
  ) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92"),
    plot.title = element_text(face = "bold", size = 14),

 plot.background = element_rect(fill = "#fefefe", colour = NA),
  panel.background = element_rect(fill = "#fefefe", colour = NA),
  legend.background = element_rect(fill = "#fefefe", colour = NA)
)
```
```{r}
library(ggplot2)

df4 <- data.frame(
  Group = factor(c('Low (0-3 hrs)', 'Medium (3-6 hrs)', 'High (6+ hrs)'),
                 levels = c('Low (0-3 hrs)', 'Medium (3-6 hrs)', 'High (6+ hrs)')),
  Sleep_Quality = c(8.38, 6.87, 5.19)
)

ggplot(df4, aes(x = Group, y = Sleep_Quality, group = 1)) +
  geom_line(color = "#2b5b84", size = 1.5) +
  geom_point(color = "#2b5b84", size = 4, shape = 21, fill = "white", stroke = 2) +
  geom_text(aes(label = Sleep_Quality), vjust = -1) +
  ylim(0, 10) +
  labs(title = "Impact of Daily Screen Time on Sleep Quality",
       x = "Daily Screen Time Group", y = "Average Sleep Quality (1-10)") +
theme_minimal() +  # Parantez içini boşalt
  theme(
    plot.background = element_rect(fill = "#fefefe", colour = NA),
    panel.background = element_rect(fill = "#fefefe", colour = NA)
  )
```
```{r}
library(ggplot2)
library(dplyr)

# Veriyi hazırlarken sıralamayı bozmamak için direkt factor yapıyoruz
df_exercise <- df %>%
  group_by(Exercise_Frequency.week.) %>%
  summarise(Average_Happiness = mean(Happiness_Index.1.10., na.rm = TRUE))

ggplot(df_exercise, aes(y = factor(Exercise_Frequency.week.), # Reorder'ı kaldırdık, sadece factor yaptık
                        x = Average_Happiness)) +
  
  geom_bar(stat = "identity", fill = "#4c72b0") +

  geom_text(aes(label = round(Average_Happiness, 2)), hjust = -0.2, color = "#4c72b0") +
  scale_x_continuous(limits = c(0, 11), expand = c(0, 0)) +
  labs(
    title = "Average Happiness Index by Exercise Frequency",
    x = "Average Happiness Index (1-10)", 
    y = "Exercise Frequency (days/week)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    # Senin istediğin o özel renk kodu:
    plot.background = element_rect(fill = "#fefefe", colour = NA),
    panel.background = element_rect(fill = "#fefefe", colour = NA),
    legend.background = element_rect(fill = "#fefefe", colour = NA)
  )
```
```{r}

if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")
if (!require("dplyr")) install.packages("dplyr")

library(ggplot2)
library(reshape2)
library(dplyr)

# Veriyi oku
df <- read.csv("Mental_Health_and_Social_Media_Balance_Dataset.csv")

# Veriyi hazırla
df_metrics <- df %>%
  select(where(is.numeric)) %>%
  rename(
    "Screen\nTime" = Daily_Screen_Time.hrs.,
    "Sleep" = Sleep_Quality.1.10.,
    "Stress" = Stress_Level.1.10.,
    "Days\nOff" = Days_Without_Social_Media,
    "Exers" = Exercise_Frequency.week.,
    "Haps" = Happiness_Index.1.10.
  )

cor_data <- cor(df_metrics, use = "complete.obs")
melted_cor <- melt(cor_data)

ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.8) + 
  scale_fill_gradientn(
    colors = c("#313695", "#4575b4", "#abd9e9", "#ffffbf", "#fdae61", "#f46d43", "#a50026"),
    limit = c(min(melted_cor$value), max(melted_cor$value)),
    name = "Correlation"
  ) +
  geom_text(aes(label = round(value, 2)), size = 4, fontface = "bold", color = "black") + 
  labs(title =  "Correlation Matrix: Mental Health & Usage Metrics", x = "", y = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, color = "black", face = "bold", size = 9, lineheight = 0.8),
    axis.text.y = element_text(color = "black", face = "bold", size = 9),
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.background = element_rect(fill = "#fefefe", colour = NA),
    panel.background = element_rect(fill = "#fefefe", colour = NA),
    legend.background = element_rect(fill = "#fefefe", colour = NA)
  ) + 
  coord_fixed()
```
