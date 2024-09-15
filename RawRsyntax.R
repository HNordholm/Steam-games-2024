#LIBRARIES 


#Data analysis
library(tidyverse)
library(correlationfunnel)
library(skimr)
library(lubridate)

#DATA IMPORT 

steam_2024_tbl <- read_csv("Steam_2024_bestRevenue_1500.csv")

#DATA EXAMINATION 

steam_2024_tbl %>% skim()

steam_2024_tbl %>% View()

# NA removal -- 3 OBS. 

steam_2024_tbl <- steam_2024_tbl %>% drop_na()

#Feature enginering -- date -> monthcolumn

steam_2024_tbl <- steam_2024_tbl %>%
  mutate(releasemonth = month(dmy(releaseDate),label=TRUE,abbr=TRUE)) %>% 
  mutate(releasemonth=as.factor(releasemonth))

# EXPLORATORY DATA ANALYSIS (GGPLOT) --

# Reviewscore - price | relationship

steam_2024_tbl %>%
  ggplot(aes(reviewScore,price))+
  geom_point(size=1.5,color="black")+
  geom_smooth()+
  theme_minimal()+
  labs(title="Gameprice and reviewscore relationship",x="Reviewscore")

# Log(copiessold) - log(price) | relationship

steam_2024_tbl %>%
  ggplot(aes(x = log(copiesSold), y = log(price))) +
  geom_point(size = 1.8, color = "skyblue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "darkblue") +
  labs(
    x = "Log(Copies Sold)",
    y = "Log(Price)",
    title = "Relationship Between log(copies sold) and log(price)")+
  theme_minimal()

# PRICE HISTOGRAM -- 

steam_2024_tbl %>% 
  ggplot(aes(price))+
  geom_histogram(binwidth =5,fill="skyblue",color="black")+
  theme_minimal()+
  labs(title="Distribution of gameprices",x="Price",y=NULL)

# REVIEWSCORE HISTOGRAM --

steam_2024_tbl %>% 
  ggplot(aes(reviewScore))+
  geom_histogram(binwidth=10,fill="darkblue",color="black")+
  theme_minimal()

# REVENUE HISTOGRAM -- 

steam_2024_tbl %>% 
  ggplot(aes(log(revenue)))+
  geom_histogram(binwidth =1,fill="skyblue",color="black")+
  theme_minimal()

# Total Q sold by releasemonth 

steam_2024_tbl %>% 
  group_by(releasemonth) %>% 
  summarise(total_q_sold = sum(copiesSold)) %>% 
  mutate(releasemonth=fct_reorder(releasemonth, total_q_sold)) %>% 
  ggplot(aes(x = total_q_sold, y = releasemonth, fill = total_q_sold))+
  geom_col()+
  scale_fill_gradient(low="lightblue",high="darkblue")+  
  labs(x="Total Copies sold",y="Release month",fill ="Total copies sold",
       title ="Quantity sold by releasemonth")+
  theme_minimal()+
  scale_x_continuous(labels=scales::label_comma())

# CORRELATION ANALYSIS 

steam_2024_tbl %>% 
  binarize() %>% 
  correlate(revenue__457163_Inf) %>% 
  plot_correlation_funnel()

steam_2024_tbl %>% 
  binarize() %>% 
  correlate(copiesSold__38313_Inf) %>% 
  plot_correlation_funnel()

# -- Hypothesis testing -- 

# Hypothesis: does games released during first quarter tend to be sold more then
# games released in second quarter? -- 

# New feature: first or. second quarter release --

steam_quarter_tbl <- steam_2024_tbl %>% 
  filter(releasemonth %in% c("jan","feb","mar","apr","maj","jun")) %>% 
  mutate(Quarter=case_when(releasemonth %in% c("jan","feb","mar")~"First",
                           releasemonth %in% c("apr","maj","jun")~"Second"))

# Avg. copies sold first vs. second quarter -- 

steam_quarter_tbl %>% 
  group_by(Quarter) %>% 
  summarise(Avg_q_sold=mean(copiesSold))

# GGPLOT VISUAL -- 

steam_quarter_tbl %>% 
  group_by(Quarter) %>% 
  summarise(Avg_q_sold = mean(copiesSold)) %>% 
  ggplot(aes(x =Quarter,y=Avg_q_sold,fill=Avg_q_sold))+
  geom_col()+
  scale_fill_gradient(low="lightblue",high="darkblue")+
  theme_minimal()+
  labs(title="Avg. steam-games sold",subtitle="By first and second quarter",
       x=NULL,y="Avg Q(copies) sold")+
  geom_text(aes(label=round(Avg_q_sold,0)),vjust=-0.6)

# Testing for normal distribution by quarter 1  -- 

steam_quarter_tbl %>% 
  filter(Quarter=="First") %>% 
  pull(copiesSold) %>% 
  shapiro.test()

# First quarter not normal distributed -> non-parametric test 

# Wilcox hypothesis testing -- 

wilcox.test(copiesSold~Quarter,data=steam_quarter_tbl)