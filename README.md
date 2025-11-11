# Social-Media-Students-Analysis
Exploration and visualization of students' social media usage using **R** and the **tidyverse** package.
The goal of this project is to examine students' behavior regarding social media, compare usage between Single and In Relationship students, and investigate relationships with addiction levels, sleep quality, and mental health.

# Project Structure

* **data/Students Social Media Addiction.csv** → Dataset containing students' social media usage and related variables.
* **R/socialmedia_eng.R** → R script that:
  1. Imports and explores the dataset
  2. Computes descriptive statistics for key variables
  3. Analyzes relationships between social media usage, mental health, sleep, and addiction
  4. Creates visualizations (bar plots, boxplots, scatterplots with regression lines, facets)
* **output/** → Folder for saving generated graphs

# Dataset Overview
This dataset contains information about students’ social media usage and its effects on well-being.
**Main variables:**
* **Gender** → Student’s gender
* **Relationship_Status** → Student’s relationship status: Single / Complicated / In Relationship
* **Avg_Daily_Usage_Hours** → Average daily time spent on social media (hours)
* **Mental_Health_Score** → Mental health assessment score
* **Sleep_Hours_Per_Night** → Average sleep hours per night
* **Addicted_Score** → Social media addiction score
* **Conflicts_Over_Social_Media** → Frequency of conflicts caused by social media usage

# Analysis Overview
The analysis includes:
1. **Comparison between Single and In Relationship students**
   * Mean and standard deviation of daily social media usage
   * Boxplots to visualize usage distributions

2. **Relationship between Avg_Daily_Usage_Hours and Mental Health Score**
   * Scatterplots with regression lines
   * Linear regression model showing negative association (more social media → lower mental health score)

3. **Relationship between Avg_Daily_Usage_Hours and Sleep_Hours_Per_Night**
   * Scatterplots with regression lines
   * Linear regression model showing negative association (more social media → fewer sleep hours)
   * Prediction example for 5.5 hours of daily social media usage

4. **Relationship between Addicted_Score and Conflicts_Over_Social_Media**
   * Scatterplots with regression lines
   * Linear regression model showing positive association (higher addiction → more conflicts)
