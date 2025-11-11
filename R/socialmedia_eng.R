# Analysis of the dataset "Students' Social Media Addiction.csv"

# Objective:
# Examine students' behavior regarding social media usage,
# comparing the average daily time spent on social media
# between Single and In Relationship students.
# Additionally, investigate possible relationships between social media use,
# addiction levels, sleep quality, and mental health.

# Key variables in this dataset include:
# Gender
# Relationship_Status 
# Avg_Daily_Usage_Hours 
# Mental_Health_Score 
# Sleep_Hours_Per_Night
# Addicted_Score
# Conflicts_Over_Social_Media 
# These are the main variables of interest.
#####################################################################
# Basic import
setwd("~/Desktop/Dataset_R")  
getwd()

library(tidyverse)

social <- read_csv("Students Social Media Addiction.csv")   
social                             

# Check dimensions and structure
dim(social)    
names(social)  
head(social)
str(social)  
glimpse(social)

#####################################################################
# Descriptive statistics for key variables

social |> 
  filter(!is.na(Avg_Daily_Usage_Hours)) |> 
  summarise(
    mean = mean(Avg_Daily_Usage_Hours),
    median = median(Avg_Daily_Usage_Hours),
    sd = sd(Avg_Daily_Usage_Hours),
    min = min(Avg_Daily_Usage_Hours),
    max = max(Avg_Daily_Usage_Hours))

social |> 
  filter(!is.na(Mental_Health_Score)) |> 
  summarise(
    mean = mean(Mental_Health_Score),
    median = median(Mental_Health_Score),
    sd = sd(Mental_Health_Score),
    min = min(Mental_Health_Score),
    max = max(Mental_Health_Score))

social |> 
  filter(!is.na(Sleep_Hours_Per_Night)) |> 
  summarise(
    mean = mean(Sleep_Hours_Per_Night),
    median = median(Sleep_Hours_Per_Night),
    sd = sd(Sleep_Hours_Per_Night),
    min = min(Sleep_Hours_Per_Night),
    max = max(Sleep_Hours_Per_Night))

social |> 
  filter(!is.na(Addicted_Score)) |> 
  summarise(
    mean = mean(Addicted_Score),
    median = median(Addicted_Score),
    sd = sd(Addicted_Score),
    min = min(Addicted_Score),
    max = max(Addicted_Score))

social |> 
  filter(!is.na(Conflicts_Over_Social_Media)) |> 
  summarise(
    mean = mean(Conflicts_Over_Social_Media),
    median = median(Conflicts_Over_Social_Media),
    sd = sd(Conflicts_Over_Social_Media),
    min = min(Conflicts_Over_Social_Media),
    max = max(Conflicts_Over_Social_Media))

#####################################################################
# Comparison between Single and In Relationship

social |> 
  group_by(Relationship_Status) |> 
  filter(!is.na(Relationship_Status)) |>
  summarise(
    mean = mean(Avg_Daily_Usage_Hours),
    sd = sd(Avg_Daily_Usage_Hours),
    N = n())

# Boxplot: visual comparison between Single and In Relationship
social |> 
  ggplot(aes(x = Relationship_Status, y = Avg_Daily_Usage_Hours, fill = Relationship_Status)) +
  geom_boxplot() +
  xlab("Relationship Status") +
  ylab("Average Daily Usage Hours") +
  theme_minimal()

# The boxplot shows the distribution of hours used for each group.

#####################################################################
# Analysis 1: Relationship between Avg Daily Usage Hours and Mental Health Score

# Scatterplot with regression line
social |> 
  ggplot(aes(x = Avg_Daily_Usage_Hours, y = Mental_Health_Score, color = Gender)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  xlab("Average Daily Usage Hours") +
  ylab("Mental Health Score") +
  theme_bw()

out.lm1 <- lm(Mental_Health_Score ~ Avg_Daily_Usage_Hours, data = social)

out.lm1 |>
  summary()

social |>
  select(Avg_Daily_Usage_Hours, Mental_Health_Score) |>
  mutate(Mental_Health_Score_hat = predict(out.lm1)) |>
  arrange(Avg_Daily_Usage_Hours)

social |>
  summarise(
    Mx  = mean(Avg_Daily_Usage_Hours),
    Mx2 = mean(Avg_Daily_Usage_Hours^2),
    My  = mean(Mental_Health_Score),
    My2 = mean(Mental_Health_Score^2),
    Mxy = mean(Avg_Daily_Usage_Hours * Mental_Health_Score),
    S2x = Mx2 - Mx^2,
    S2y = My2 - My^2,
    Sxy = Mxy - Mx * My,
    b   = Sxy / S2x,            
    a   = My - b * Mx,         
    R2  = Sxy^2 / (S2x * S2y))

# Comment:
# The analysis shows a negative relationship between average daily social media use
# and mental health scores. Results indicate that as daily usage increases,
# psychological well-being tends to decrease.
# On average, each additional hour on social media is associated with a reduction
# of about 0.70 points in the mental health score. The model explains
# approximately 64% of the observed variability, indicating a strong and
# statistically significant relationship (p < 0.001).
# These findings suggest that excessive social media use can negatively impact
# students’ mental health, reducing their emotional and psychological balance.

#####################################################################
# Analysis 2: Relationship between Avg_Daily_Usage_Hours and Sleep_Hours_Per_Night

social |> 
  ggplot(aes(x = Avg_Daily_Usage_Hours, y = Sleep_Hours_Per_Night, color = Gender)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkgreen") +
  xlab("Average Daily Usage Hours") +
  ylab("Sleep Hours Per Night") +
  theme_bw()

out.lm2 <- lm(Sleep_Hours_Per_Night ~ Avg_Daily_Usage_Hours, data = social)

out.lm2 |>
  summary()

social |>
  select(Avg_Daily_Usage_Hours, Sleep_Hours_Per_Night) |>
  mutate(Sleep_Hours_Pred = predict(out.lm2)) |>
  arrange(Avg_Daily_Usage_Hours)

social |>
  summarise(
    Mx  = mean(Avg_Daily_Usage_Hours),
    Mx2 = mean(Avg_Daily_Usage_Hours^2),
    My  = mean(Sleep_Hours_Per_Night),
    My2 = mean(Sleep_Hours_Per_Night^2),
    Mxy = mean(Avg_Daily_Usage_Hours * Sleep_Hours_Per_Night),
    S2x = Mx2 - Mx^2,
    S2y = My2 - My^2,
    Sxy = Mxy - Mx * My,
    b   = Sxy / S2x,            
    a   = My - b * Mx,          
    R2  = Sxy^2 / (S2x * S2y))

# Predicting sleep hours for 5.5 hours of social media usage
social |>
  summarise(
    Mx  = mean(Avg_Daily_Usage_Hours),
    Mx2 = mean(Avg_Daily_Usage_Hours^2),
    My  = mean(Sleep_Hours_Per_Night),
    My2 = mean(Sleep_Hours_Per_Night^2),
    Mxy = mean(Avg_Daily_Usage_Hours * Sleep_Hours_Per_Night),
    S2x = Mx2 - Mx^2,
    S2y = My2 - My^2,
    Sxy = Mxy - Mx * My,
    b   = Sxy / S2x,            
    a   = My - b * Mx,          
    R2  = Sxy^2 / (S2x * S2y),
    y_5_5 = a + b * 5.5)        # predicted sleep hours for 5.5h of social media use

# Comment: 
# The analysis shows a negative relationship between daily social media use
# and sleep hours. On average, each extra hour of social media reduces sleep by
# approximately 0.71 hours.
# The model explains about 62.5% of observed variability in sleep.
# A student using social media for 5.5 hours daily is predicted to sleep 6.46 hours per night.
# This confirms a consistent negative correlation between social media use and sleep quantity.

#####################################################################
# Analysis 3: Relationship between Addicted_Score and Conflicts_Over_Social_Media

social |> 
  ggplot(aes(x = Addicted_Score, y = Conflicts_Over_Social_Media, color = Gender)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  xlab("Addiction Score") +
  ylab("Conflicts Over Social Media") +
  theme_bw()

out.lm3 <- lm(Conflicts_Over_Social_Media ~ Addicted_Score, data = social)

out.lm3 |>
  summary()

social |>
  select(Addicted_Score, Conflicts_Over_Social_Media) |>
  mutate(Conflicts_Pred = predict(out.lm3)) |>
  arrange(Addicted_Score)

social |>
  summarise(
    Mx  = mean(Addicted_Score),
    Mx2 = mean(Addicted_Score^2),
    My  = mean(Conflicts_Over_Social_Media),
    My2 = mean(Conflicts_Over_Social_Media^2),
    Mxy = mean(Addicted_Score * Conflicts_Over_Social_Media),
    S2x = Mx2 - Mx^2,
    S2y = My2 - My^2,
    Sxy = Mxy - Mx * My,
    b   = Sxy / S2x,            
    a   = My - b * Mx,         
    R2  = Sxy^2 / (S2x * S2y))  

# Comment:
# The analysis shows a strong positive relationship between social media addiction
# levels and the frequency of conflicts caused by social media use. 
# On average, each additional point in addiction score corresponds to an
# increase of ~0.56 points in relationship conflicts.
# The model explains about 87% of the observed variability, indicating a
# very strong and statistically significant relationship (p < 0.001).
# These results highlight that higher social media addiction not only affects
# personal well-being but may also generate tension and disputes in relationships,
# negatively impacting interpersonal quality.

#####################################################################
# Credits
# This analysis was inspired by the work and teaching materials of Luigi Augugliaro, Full Professor at the University of Palermo.
# The dataset and concepts were used for educational purposes only.
