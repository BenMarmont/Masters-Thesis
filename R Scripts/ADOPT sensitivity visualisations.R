library(tidyverse)
library(readxl)
library(readr)
library(ggthemes)
library(patchwork)


#import-------------------------------------------------------------------------
zeddy_sensitivity <- read_csv("machine_1_sensitivity_zeddy.csv") %>% 
  rename(`Predicted adoption rate in population (%)`=`Peak adoption (%)`)

c_lock_sensitivity <- read_csv("machine_2_sensitivities.csv")%>% 
  rename("Predicted adoption rate in population (%)" = "Peak adoption (%)") 

#graphing------------------------------------------------------------------------

patch_1 <- zeddy_sensitivity %>% 
  mutate(`Technology performance` = 
        fct_relevel(`Technology performance`, "Low", "Medium", "High")) %>% 
  ggplot(mapping = aes(x = `Predicted adoption rate in population (%)`, y = `Technology performance`)) +
      geom_col() +
      coord_flip() +
      theme_bw() +   
      labs(title = "Technology 1") +
      xlim(0, 100)

patch_2 <- c_lock_sensitivity %>% 
  mutate(`Technology performance` = 
           fct_relevel(`Technology performance`, "Low", "Medium", "High")) %>% 
  ggplot(mapping = aes(x = `Predicted adoption rate in population (%)`, y = `Technology performance`)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  labs(title = "Technology 2") +
  xlim(0, 100) 

patch_1 + patch_2 + plot_annotation(title = "Modelled Peak Adoption", subtitle = "Higher percentage is preferable")

#Looking at time to peak adoption-----------------------------------------------
  
patch_3 <- zeddy_sensitivity %>%
mutate(`Technology performance` =
         fct_relevel(`Technology performance`, "Low", "Medium", "High")) %>%
  ggplot(mapping = aes(y = `Time to peak adoption (years)`, x = `Technology performance`)) +
      geom_col() +
      theme_bw() +   
      labs(title = "Technology 1") +
      ylim(0, 20)



patch_4 <- c_lock_sensitivity %>%
  mutate(`Technology performance` = 
           fct_relevel(`Technology performance`, "Low", "Medium", "High")) %>%
  ggplot(mapping = aes(y = `Time to peak adoption (years)`, x = `Technology performance`)) +
  geom_col() +
  theme_bw() +   
  labs(title = "Technology 2") +
  ylim(0, 20)

patch_3 + patch_4 +plot_annotation(title = "Modelled time till peak adoption", subtitle = "Lower time is preferable")


