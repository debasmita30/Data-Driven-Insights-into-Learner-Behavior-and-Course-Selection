suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

df <- read_csv("C:/Users/Diya/Downloads/funnel/tableau_hierarchy_for_RiseUpp.csv")

top_10_streams <- df %>%
  filter(level == "Sub-Category") %>%
  group_by(parentid) %>%
  summarise(
    course_count = n(),
    avg_engagement = mean(engagement_score, na.rm = TRUE)
  ) %>%
  arrange(desc(course_count))

platform_engagement <- df %>%
  group_by(preferred_platform) %>%
  summarise(
    avg_engagement = mean(engagement_score, na.rm = TRUE),
    student_count = n()
  ) %>%
  arrange(desc(avg_engagement))

cat("--- Analysis 1: Platform Engagement ---\n")
print(platform_engagement)

top_stream_id <- top_10_streams$parentid[1]

top_stream_persona <- df %>%
  filter(parentid == top_stream_id) %>%
  summarise(
    avg_age = mean(avg_age, na.rm = TRUE),
    dominant_platform = names(which.max(table(preferred_platform))),
    avg_engagement = mean(engagement_score, na.rm = TRUE)
  )

cat("\n--- Analysis 2: Persona for Top Stream (ID:", top_stream_id, ") ---\n")
print(top_stream_persona)

opportunity_stream <- top_10_streams %>%
  slice_head(n = 5) %>%
  arrange(avg_engagement) %>%
  slice_head(n = 1)

cat("\n--- Analysis 3: Optimization Opportunity ---\n")
print(opportunity_stream)

ggplot(platform_engagement, aes(x = reorder(preferred_platform, -avg_engagement), y = avg_engagement, fill = preferred_platform)) +
  geom_col() +
  geom_text(aes(label = round(avg_engagement, 1)), vjust = -0.5) +
  labs(
    title = "Average Student Engagement by Preferred Platform",
    x = "EdTech Platform",
    y = "Average Engagement Score"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")