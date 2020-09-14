# Library Installations & Working Directory
library(tidyverse)
library(rstudioapi)
library(gganimate)
library(ggthemes)
library(extrafont)
setwd(dirname(getActiveDocumentContext()$path))
font_import()

# Data Import
data <- read_csv("../data/data.csv")

# EDA
summary(data)
data <- data %>%
  rename("gdp" = `gdp_for_year ($)`, "gdp/capita" = `gdp_per_capita ($)`) %>%
  mutate(year = as.numeric(year),
         sex = factor(sex, levels = c("male", "female"), labels = c("Male", "Female")),
         age = factor(age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"),
                      labels = c("5 - 14", "15 - 24", "25 - 34", "35 - 54", "55 - 74", "75+"))) %>%
  select(-c(`country-year`, `HDI for year`, generation))

# Suicides per Country by Year
# Pre Plot Formatting
suic_year_tidy <- data %>%
  group_by(year, country) %>%
  summarize(suicides = sum(suicides_no)) %>%
  filter(year > 1986 & year < 2016)
suic_year_tidy <- suic_year_tidy[ , c(2, 1, 3)]

suic_year_formatted <- suic_year_tidy %>%
  group_by(year) %>%
  mutate(rank = rank(-suicides),
         prop = suicides/suicides[rank == 1],
         label = paste0(" ", round(suicides))) %>%
  group_by(country) %>%
  filter(rank <= 10) %>%
  ungroup()

# Static Plot
staticplot_suic <- ggplot(suic_year_formatted, aes(rank, suicides, group = country, 
                                                  fill = country, color = country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = suicides, label = label, hjust = 0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = .1, color = "grey"),
        panel.grid.minor.x = element_line(size = .1, color = "grey"),
        plot.title = element_text(size = 25, hjust = 0.5, face = "bold", colour = "grey", vjust = 1),
        plot.caption = element_text(size = 8, hjust = 0.5, face = "italic", color = "grey"),
        plot.background = element_blank(),
        plot.margin = margin(2, 2, 2, 4, "cm"))

# Animated Plot
anim_pop_suic = staticplot_suic + transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Suicides per Year: {closest_state}',
       caption  = "Data Source: Kaggle")

# Animate the Plot
animate(anim_pop_suic, 200, fps = 10,  width = 1200, height = 1000, end_pause = 30,
        renderer = gifski_renderer("suic.gif"))

# Suicides/100k population per Country by Year
# Pre Plot Formatting
suic_100_tidy <- data %>%
  group_by(year, country) %>%
  summarize("Suicides Per 100K" = (sum(suicides_no)/sum(population)) * 100000) %>%
  filter(year > 1986 & year < 2016)
suic_100_tidy <- suic_100_tidy[ , c(2, 1, 3)]

suic_100_formatted <- suic_100_tidy %>%
  group_by(year) %>%
  mutate(rank = rank(-`Suicides Per 100K`),
         prop = `Suicides Per 100K`/`Suicides Per 100K`[rank == 1],
         label = paste0(" ", round(`Suicides Per 100K`))) %>%
  group_by(country) %>%
  filter(rank <= 10) %>%
  ungroup()

# Static Plot
staticplot_100 <- ggplot(suic_100_formatted, aes(rank, `Suicides Per 100K`, group = country, 
                                                   fill = country, color = country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = `Suicides Per 100K`, label = label, hjust = 0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse()  +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = .1, color = "grey"),
        panel.grid.minor.x = element_line(size = .1, color = "grey"),
        plot.title = element_text(size = 25, hjust = 0.5, face = "bold", colour = "grey", vjust = 1),
        plot.caption = element_text(size = 8, hjust = 0.5, face = "italic", color = "grey"),
        plot.background = element_blank(),
        plot.margin = margin(2, 2, 2, 4, "cm"))

# Animated Plot
anim_100 = staticplot_100 + transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Suicides per 100K Population: {closest_state}',
       caption  = "Data Source: Kaggle")

# Animate the Plot
animate(anim_100, 300, fps = 10,  width = 1200, height = 1000, end_pause = 30,
        renderer = gifski_renderer("100.gif"))

# Suicides per Age Group by Year
# Pre Plot Formatting
suic_ag_tidy <- data %>%
  group_by(year, age) %>%
  summarize("Suicides Per Age Group Per 100K Population" = (sum(suicides_no)/sum(population)) * 100000) %>%
  filter(year > 1986 & year < 2016)
suic_ag_tidy <- suic_ag_tidy[ , c(2, 1, 3)]

suic_ag_formatted <- suic_ag_tidy %>%
  group_by(year) %>%
  rename("Age" = age) %>%
  mutate(rank = rank(-`Suicides Per Age Group Per 100K Population`),
         prop = `Suicides Per Age Group Per 100K Population`/`Suicides Per Age Group Per 100K Population`[rank == 1],
         label = paste0(" ", round(`Suicides Per Age Group Per 100K Population`))) %>%
  group_by(Age) %>%
  ungroup()

# Static Plot
suic_ag_plot <- ggplot(suic_ag_formatted, aes(year, `Suicides Per Age Group Per 100K Population`, color = Age)) +
  geom_line(size = 1, alpha = .7) +
  labs(title = "Suicides per Age Group",
      subtitle = "How do yearly suicide deaths differ across age groups?",
      x = "Year", 
      y = "Suicides per 100,000 People",
      caption = "Source: Kaggle",
      color = "Age Group") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), legend.position = "right",
        legend.direction = "vertical", text = element_text(family = "Constantia")) 
  
# Animated Plot
suic_ag_plot_anim <- suic_ag_plot +
  transition_reveal(as.integer(year))

# Animate the Plot
animate(suic_ag_plot_anim, 200, fps = 30,  width = 1200, height = 1000, duration = 10, end_pause = 60, res = 100,
        renderer = gifski_renderer("age.gif"))

# Suicides per Gender by Year
# Pre Plot Formatting
suic_g_tidy <- data %>%
  group_by(year, sex) %>%
  summarize("Suicides Per Gender Per 100K Population" = (sum(suicides_no)/sum(population)) * 100000) %>%
  filter(year > 1986 & year < 2016)
suic_g_tidy <- suic_g_tidy[ , c(2, 1, 3)]

suic_g_formatted <- suic_g_tidy %>%
  group_by(year) %>%
  rename("Sex" = sex) %>%
  mutate(rank = rank(`Suicides Per Gender Per 100K Population`),
         prop = `Suicides Per Gender Per 100K Population`/`Suicides Per Gender Per 100K Population`[rank == 1],
         label = paste0(" ", round(`Suicides Per Gender Per 100K Population`))) %>%
  group_by(Sex) %>%
  ungroup()

# Static Plot
suic_g_plot <- ggplot(suic_g_formatted, aes(year, `Suicides Per Gender Per 100K Population`, color = Sex)) +
  geom_line(size = 1, alpha = .7) +
  labs(title = "Suicides per Gender",
       subtitle = "How do yearly suicide deaths differ by gender?",
       x = "Year", 
       y = "Suicides per 100,000 People",
       caption = "Source: Kaggle",
       color = "Gender") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), legend.position = "right",
        legend.direction = "vertical", text = element_text(family = "Constantia")) 


# Animated Plot
suic_g_plot_anim <- suic_g_plot +
  transition_reveal(as.integer(year))

# Animate the Plot
animate(suic_g_plot_anim, 200, fps = 30,  width = 1200, height = 1000, duration = 10, end_pause = 60, res = 100,
        renderer = gifski_renderer("g.gif"))

# Suicides in the US vs ROW
ROW_tidy <- data %>%
  group_by(year, country_group = ifelse(country == "United States", "USA", "Non-USA")) %>%
  summarize(suicides = (sum(suicides_no)/sum(population)) * 100000)

# Plot
ROW_plot <- ggplot(ROW_tidy, aes(year, suicides, color = country_group)) +
  geom_line(size = 1, alpha = .7) +
  labs(title = "Suicides: USA vs Non-USA",
       subtitle = "How do yearly suicide deaths differ across the U.S. and other countries?",
       x = "Year",
       y = "Suicides per 100,000 People",
       caption = "Source: Kaggle",
       color = "Country") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), legend.position = "right", legend.direction = "vertical", text = element_text(family = "Constantia")) 

# Suicides per Age Group - US
# Pre-plot Formatting
suic_ag_us_tidy <- data %>%
  group_by(year, age, country) %>%
  filter(country == "United States") %>%
  summarize("Suicides Per Age Group Per 100K Population" = (sum(suicides_no)/sum(population)) * 100000) %>%
  filter(year > 1986 & year < 2016)

suic_ag_us_formatted <- suic_ag_us_tidy %>%
  group_by(year) %>%
  rename("Age" = age) %>%
  group_by(Age) %>%
  ungroup()

# Plot
suic_ag_us_plot <- ggplot(suic_ag_us_formatted, aes(year, `Suicides Per Age Group Per 100K Population`, color = Age)) +
  geom_line(size = 1, alpha = .7) +
  labs(title = "Suicides per Age Group in the U.S.",
        subtitle = "How do yearly suicide deaths differ across age groups in the U.S.?",
        x = "Year",
        y = "Suicides per 100,000 People",
        caption = "Source: Kaggle",
        color = "Age Group") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), legend.position = "right", legend.direction = "vertical", text = element_text(family = "Constantia"))

# Female Teenage Suicides 
# Pre-plot Formatting
f_teen_tidy <- data %>%
  group_by(year, age, sex) %>%
  filter(year > 1986, year < 2016) %>%
  filter(age == "5 - 14" | age == "15 - 24") %>%
  filter(sex == "Female") %>%
  droplevels %>%
  summarize("Suicides Per Age Group Per 100K Population" = (sum(suicides_no)/sum(population)) * 100000) 

# Plot
f_teen_plot <- ggplot(f_teen_tidy, aes(year, `Suicides Per Age Group Per 100K Population`, color = age, group = age)) +
  geom_line(size = 1, alpha = .7) +
  labs(title = "Young Female Suicides",
       subtitle = "How have young female suicides changed over the years?",
       x = "Year",
       y = "Suicides per 100,000 People",
       caption = "Source: Kaggle",
       color = "Age Group") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), legend.position = "right", legend.direction = "vertical", text = element_text(family = "Constantia"))

# Male Teenage Suicides
# Pre-plot Formatting
m_teen_tidy <- data %>%
  group_by(year, age, sex) %>%
  filter(year > 1986, year < 2016) %>%
  filter(age == "5 - 14" | age == "15 - 24") %>%
  filter(sex == "Male") %>%
  droplevels %>%
  summarize("Suicides Per Age Group Per 100K Population" = (sum(suicides_no)/sum(population)) * 100000) 


# Plot
m_teen_plot <- ggplot(m_teen_tidy, aes(year, `Suicides Per Age Group Per 100K Population`, color = age, group = age)) +
  geom_line(size = 1, alpha = .7) +
  labs(title = "Young Male Suicides",
       subtitle = "How have young male suicides changed over the years?",
       x = "Year",
       y = "Suicides per 100,000 People",
       caption = "Source: Kaggle",
       color = "Age Group") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), legend.position = "right", legend.direction = "vertical", text = element_text(family = "Constantia"))

# Data Exploration, Cleaning & Further Descriptive Analysis
head(data)
str(data)
data <- data %>%
  uncount(weights = suicides_no) 

# Variation
# Variation of Categorical Variables
# Sex Variation
variation_sex <- ggplot(data) +
  geom_bar(mapping = aes(x = sex, color = sex, fill = sex)) +
  labs(title = "Total Suicides per Gender from 1985 - 2016",
       x = "Sex",
       y = "Suicides") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), legend.position = "right", legend.direction = "vertical", text = element_text(family = "Constantia")) +
  scale_y_continuous(labels = scales::comma)
count(data, sex)

variation_age <- ggplot(data) +
  geom_bar(mapping = aes(x = age, color = age, fill = age)) +
  labs(title = "Total Suicides per Age Group from 1985 - 2016",
       x = "Age Group",
       y = "Suicides") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), legend.position = "right", legend.direction = "vertical", text = element_text(family = "Constantia")) +
  scale_y_continuous(labels = scales::comma)
count(data, age)

# Inferential Statistics
# Summing the Data back up
data <- read_csv("../data/data.csv")
summary(data)
data <- data %>%
  rename("gdp" = `gdp_for_year ($)`, "gdp/capita" = `gdp_per_capita ($)`) %>%
  mutate(year = as.numeric(year),
         sex = factor(sex, levels = c("male", "female"), labels = c("Male", "Female")),
         age = factor(age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"),
                      labels = c("5 - 14", "15 - 24", "25 - 34", "35 - 54", "55 - 74", "75+"))) %>%
  select(-c(`country-year`, `HDI for year`, generation))

# Regression Analysis
# GDP/Capita
# Pre_Analysis Data Formatting
gdpcap <- data %>%
  group_by(year, country, `gdp/capita`) %>%
  summarize(suicides = sum(suicides_no))
# Model
summary(lm(suicides ~ `gdp/capita`, data = gdpcap))
ggplot(gdpcap, aes(`gdp/capita`, suicides)) +
  stat_smooth(method = lm) +
  scale_x_continuous(name = "GDP per Capita", labels = scales::comma) +
  scale_y_continuous(name = "Suicides per Country") +
  labs(title = "Linear Regression Analysis: GDP per Capita and Suicides per Country") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), legend.position = "right", legend.direction = "vertical", text = element_text(family = "Constantia"))





