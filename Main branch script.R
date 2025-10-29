#Libraries
library(readr)
library(tidyverse)
library(reshape2)
#First I will import the dataset we will be working from and call it raw
#raw <- read_csv("C:/Users/Owner/Downloads/TextMessages.csv")

#Then I will recreate that data here so that my partner and I can work on it
#without having to worry about it being located in different spots on each
#of our computers. This function will output the code needed to recreate the raw
#data frame
#dput(as.data.frame(raw))

#Then I can take that code and turn it into the df here
df <- structure(list(Group = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                               1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                               2,2), Baseline = c(52,68,85,47,73,57,63,50,66,
                              60,51,72,77,57,79,75,53,72,62,71,53,64,79,75
                              ,60,65,57,66,71,75,61,80,66,53,62,61,77,66,52
                              ,60,58,54,72,71,87,75,57,59,46,89), Six_months
                     = c(32,48,62,16,63,53,59,58,59,57,60,56,61,52,9,76,38,63,53
                         ,61,50,78,33,68,59,62,50,62,61,70,64,64,55,47,61,56,64,
                         62,47,56,78,74,61,61,78,62,71,55,46,79), Participant = 
                       c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
                         22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
                         40,41,42,43,44,45,46,47,48,49,50)), 
                row.names = c(NA,-50L), class = "data.frame")

#Now that we both have the df available, I will make sure all of our
#variables are named appropriately and are of the correct classes
names(df)
#These names are of reasonable lengths and descriptions so we can keep them as
#is
class (df$Group)
class (df$Baseline)
class (df$Six_months)
class (df$Participant)
#Currently all of our variables are numeric. This is appropriate for Baseline,
#Six_months, and Participants variables. Group however should be a factor.
#We will do that here
df$Group <- as.factor(df$Group)
#And check that it worked here
is.factor(df$Group)
#It is TRUE confirming we successfully converted it to a factor
#Now that our data is imported and has the correct names and classes, we can
#begin to create our figures and start our analysis. 

#(JR) I'll start by reordering the columns and then shifting to a long "tidy"
#data set by "melting" the Baseline and Six_months columns. I create an object,
#pipe df into select to reorder my columns (just a little OCD), pipe into
#rename(this is so the six months variable appears without the underscore in the graph),
#and pipe into melt to create a tidy data frame.

df_long <- df %>% select(Participant, Group, Baseline, Six_months) %>% 
  rename("Six months" = Six_months) %>% 
    melt(id.vars = c("Participant", "Group"), variable.name = "Visit", 
          value.name = "Text_Count") 

#Next, I'll create the stratified boxplot by group. I assign an object, pipe the 
#new data frame into ggplot, assign aesthetics, add a boxplot layer (with an argument
#or the outliers, but maybe we should just remove?), set a scale for the y-axis 
#with scale_y_continuous, facet by group with a labeller argument for the two groups,
#add a color scheme by visit, and in the themes layer, I remove the legend, and 
#(just for fun) change the background color.

text_count_boxplot <- df_long %>% 
  ggplot(aes(x = Visit, y = Text_Count, fill = Visit)) +
  geom_boxplot(outlier.size = .8) + scale_y_continuous(limits = c(0, 100), 
                      breaks = seq(from = 0, to = 100, by = 10)) +
    facet_grid(~Group, labeller = label_both) + 
      scale_fill_manual(values = c("tomato", "forestgreen")) +
        theme(legend.position = "none", 
                  panel.background = element_rect(fill = "lightblue")) +
  labs(title = "Text messages by Group", y = "Text Count")
    
text_count_boxplot

#Just to see what it would look like, I removed the outliers and the 
#scale_y_continuous argument, replacing it with "free_y" in facet_grid; this
#allows R to handle how to assign the y-axis. I like this graph better, but would
#love input!!

text_count_boxplot2 <- df_long %>% 
  ggplot(aes(x = Visit, y = Text_Count, fill = Visit)) +
  geom_boxplot(outliers = FALSE)  +
  facet_grid(~Group, labeller = label_both, scales = "free_y") + 
  scale_fill_manual(values = c("tomato", "forestgreen")) +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "lightblue")) +
  labs(title = "Text messages by Group", y = "Text Count")

text_count_boxplot2
