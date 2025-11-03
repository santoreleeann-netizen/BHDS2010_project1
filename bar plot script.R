##############################################################################
##Visualization #2: Creates stratified bar charts of text messages Group 
##and Time (Hint: Faceted Bar Charts).
###############################################################################

# Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

#To make our bar plot we will first convert our data from wide to long format
#using melt.

df_long <- melt(df,
                id.vars = "Group",
                measure.vars = c("Baseline", "Six_months"),
                variable.name = "Time",
                value.name = "Messages")

#Next, since our bar plot will compare the means between groups, we will 
#calculate the mean text messages and 95% CI for each group and timepoint.
# Compute summary statistics for mean and 95% CI per group/time. We will
#put these results in their own data frame called df_summary

df_summary <- df_long %>%
  group_by(Group, Time) %>%
  summarize(
    mean_count = mean(Messages, na.rm = TRUE),
    sd = sd(Messages, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci_lower = mean_count - 1.96 * se,
    ci_upper = mean_count + 1.96 * se,
    .groups = "drop"
  )

#Now that we have our values, I will  rename them so that they look more
#aesthetically pleasing in our plot. 
df_summary <- df_summary %>%
  mutate(
    Time = recode(Time,
                  "Baseline" = "Baseline",
                  "Six_months" = "Six months"),
    Group = recode_factor(as.factor(Group),
                          "1" = "Group 1",
                          "2" = "Group 2")
  )

#we can now plot our bar plot. We will use ggplot to
#make a bar plot that compares the mean text messages at each time point and
#we will facet by groups so that we can visualize the differences in time
#points between groups as well

ggplot(df_summary, aes(x = Time, y = mean_count, fill = Time)) +
  geom_col(position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, position = position_dodge(0.7)) +
  facet_wrap(~ Group) +
  labs(
    title = "Average Number of Text Messages at Baseline and Six Months",
    x = "Time Point",
    y = "Mean Number of Messages",
    fill = "Time Point"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold")
  )

#Here we can see the mean and 95% CI plotted at each time point for each group
#Baseline values are pink and six month follow up values are blue. The color
#designations are describe din the legend on the right. The panel
#on the left show the results of group 1 while the one on the right shows the
#results of group 2.The error bars on the top of each bar represent the 95%
#CI. We can clearly see that both group one and group 2 experience a decrease
#In the mean number of messages at follow up compared to baseline. The 95% CI
#do not appear to overlap in group one, but they do appear to overlap in group
#2 suggesting that the decrease in messages was statistically significant for
#group 1 but not for group 2. 