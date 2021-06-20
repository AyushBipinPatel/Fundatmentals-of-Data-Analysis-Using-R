#### To show how everyone needs help while coding ##########


# Libraries ---------------------------------------------------------------

library(here)
library(tidyverse)
library(AER)

# explore data ------------------------------------------------------------

starwars
View(starwars)
?starwars

head(starwars$films)

starwars %>% 
  rowwise() %>% 
  mutate(
    number_of_films = length(films)
  ) %>% 
  arrange(desc(number_of_films)) %>% 
  view()


starwars %>% 
  filter(mass <160) %>% 
  ggplot(aes(height,mass))+
  geom_jitter(size = 3, alpha = 0.8,
              colour = "steelblue")+
  labs(
    title = "Height and Weight",
    x = "Height in cm",
    y = "Mass in Kg"
  )+
  theme(
    panel.background = element_rect(fill = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff"),
    panel.grid = element_line(colour = "grey"),
    plot.title.position  = "plot"
  )

unique(starwars$skin_color)


data("CPS1988")

view(CPS1988)

CPS1988 %>% 
  mutate(
    edu_classification = case_when(
      education == 0 ~ "NO education",
      education >0 & education <=5 ~ "Primary Education",
      education >5 & education <=8 ~ "Secondary Education",
      education >8 & education <=12 ~ "Higer Secondary Education",
      education >12 & education <=15 ~ "Graduate Education",
      education >15 ~ "Post Graduate Education"
      
    ),
    edu_classification = fct_reorder(
      as.factor(edu_classification),
      education
    )
  ) %>% 
  filter(wage <= 2500) %>% 
  ggplot(aes(edu_classification,wage))+
  geom_boxplot(
               alpha= 0.5)+
  geom_jitter(alpha = 0.5)+
  theme(
    panel.background = element_rect(fill = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff"),
    panel.grid = element_line(colour = "grey"),
    plot.title.position  = "plot"
  )+
  facet_wrap(~ethnicity)

CPS1988 %>% 
  mutate(
    edu_classification = case_when(
      education == 0 ~ "NO education",
      education >0 & education <=5 ~ "Primary Education",
      education >5 & education <=8 ~ "Secondary Education",
      education >8 & education <=12 ~ "Higer Secondary Education",
      education >12 & education <=15 ~ "Graduate Education",
      education >15 ~ "Post Graduate Education"
      
    ),
    edu_classification = fct_reorder(
      as.factor(edu_classification),
      education
    )
  ) %>% 
  count(edu_classification,ethnicity)

CPS1988 %>% 
  mutate(
    edu_classification = case_when(
      education == 0 ~ "NO education",
      education >0 & education <=5 ~ "Primary Education",
      education >5 & education <=8 ~ "Secondary Education",
      education >8 & education <=12 ~ "Higer Secondary Education",
      education >12 & education <=15 ~ "Graduate Education",
      education >15 ~ "Post Graduate Education"
      
    ),
    edu_classification = fct_reorder(
      as.factor(edu_classification),
      education
    )
  ) %>% 
  filter(wage <= 2500) %>% 
  ggplot(aes(experience,wage))+
  geom_jitter(aes(colour = ethnicity),
              alpha= 0.5)+
  theme(
    panel.background = element_rect(fill = "#ffffff"),
    plot.background = element_rect(fill = "#ffffff"),
    panel.grid = element_line(colour = "grey"),
    plot.title.position  = "plot"
  )+
  facet_wrap(~parttime)





data("CreditCard")

view(CreditCard)



CreditCard %>% 
  filter(income<=7.5& expenditure <=1000 ) %>% 
  ggplot(aes(owner,expenditure))+
  geom_boxplot(aes(fill = owner), alpha = 0.8)

CreditCard %>% 
  mutate(
    income_group = case_when(
      income < 2.24375 ~ "LIG",
      income >= 2.24375 & income < 2.90 ~ "MIG",
      income >= 2.9 & income < 4 ~ "UMIG",
      income >= 4.0 & income < 13.5 ~ "HIG",
    )
  ) %>% 
  filter(income<=7.5 | expenditure <=1000) %>% 
  ggplot(aes(age, expenditure))+
  geom_jitter(aes(color = income_group),
              alpha =0.5, size = 3)


CreditCard %>% 
  mutate(
    income_group = case_when(
      income < 2.24375 ~ "LIG",
      income >= 2.24375 & income < 2.90 ~ "MIG",
      income >= 2.9 & income < 4 ~ "UMIG",
      income >= 4.0 & income < 13.5 ~ "HIG",
    )
  ) %>% 
  ggplot()+
  geom_density(aes(share, 
                   fill = income_group),
               alpha = 0.5)


data("Affairs")
view(Affairs)


Affairs %>%
  filter(affairs!=0) %>% 
  count(religiousness,gender, children,) %>% 
  ggplot()+
  geom_col(aes(religiousness,n, fill = children))+
  facet_wrap(~gender)

table(Affairs$gender)






