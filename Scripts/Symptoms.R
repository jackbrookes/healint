library(ggplot2)
library(readr)
library(tidyverse)
library(ggprism)

# read in data and filter out non-default symptoms 

symptom_data <- read_csv("Data/migraine_symptoms.csv") %>% 
  filter(is_default == TRUE)

# Recode symptoms

symptom_data$symptom[symptom_data$symptom == "blurred look"] <- "blurred vision"
symptom_data$symptom[symptom_data$symptom == "Duizeligheid"] <- "Dizziness"
symptom_data$symptom[symptom_data$symptom == "Zawroty g\U0142owy"] <- "Dizziness"
symptom_data$symptom[symptom_data$symptom == "no."] <- "none"
symptom_data$symptom[symptom_data$symptom == "no"] <- "none"
symptom_data$symptom[symptom_data$symptom == "no symptoms"] <- "none"
symptom_data$symptom[symptom_data$symptom == "depressive mood"] <- "Depressed mood"
symptom_data$symptom[symptom_data$symptom == "sensitivity to noise"] <- "sensitivity to sound"
symptom_data$symptom[symptom_data$symptom == "haze eyes"] <- "blurred vision"
symptom_data$symptom[symptom_data$symptom == "pain worsens with movement"] <- "worse by physical activity"
symptom_data$symptom[symptom_data$symptom == "tinnitus (ringing)"] <- "tinnitus"
symptom_data$symptom[symptom_data$symptom == "dizziness"] <- "Dizziness"
symptom_data$symptom[symptom_data$symptom == "zoeira in the ears / buzzing"] <- "tinnitus"
symptom_data$symptom[symptom_data$symptom == "dizziness / fainting"] <- "Dizziness"
symptom_data$symptom[symptom_data$symptom == "diarrhea"] <- "Diarrhoea"
symptom_data$symptom[symptom_data$symptom == "pounding pain"] <- "throbbing pain"
symptom_data$symptom[symptom_data$symptom == "hammering pain"] <- "throbbing pain"
symptom_data$symptom[symptom_data$symptom == "sensitivity to odors"] <- "sensitivity to odours"
symptom_data$symptom[symptom_data$symptom == "sensitivity to smell"] <- "sensitivity to odours"
symptom_data$symptom[symptom_data$symptom == "worse by physical activity"] <- "worse pain if moving"
symptom_data$symptom[symptom_data$symptom == "embarrassing"] <- "Mood Changes"
symptom_data$symptom[symptom_data$symptom == "tired"] <- "fatigue"
symptom_data$symptom[symptom_data$symptom == "depressed mood"] <- "Mood Changes"
symptom_data$symptom[symptom_data$symptom == "Mood Changes"] <- "Mood Changes"
symptom_data$symptom[symptom_data$symptom == "emotional"] <- "Mood Changes"
symptom_data$symptom[symptom_data$symptom == "melancholy"] <- "Mood Changes"
symptom_data$symptom[symptom_data$symptom == "moody"] <- "Mood Changes"
symptom_data$symptom[symptom_data$symptom == "temperamental"] <- "Mood Changes"
symptom_data$symptom[symptom_data$symptom == "body heat"] <- "Hot Flashes"
symptom_data$symptom[symptom_data$symptom == "hot flashes"] <- "Hot Flashes"
symptom_data$symptom[symptom_data$symptom == "unstable mood"] <- "Mood Changes"
symptom_data$symptom[symptom_data$symptom == "pulsating pain"] <- "throbbing pain"
symptom_data$symptom[symptom_data$symptom == "Depressed mood"] <- "Mood Changes"
symptom_data$symptom[symptom_data$symptom == "neck pain"] <- "cervical pain"
symptom_data$symptom[symptom_data$symptom == "delusion / confusion"] <- "confusion"
symptom_data$symptom[symptom_data$symptom == "confusion / dizziness"] <- "Dizziness"
symptom_data$symptom[symptom_data$symptom == "confusion / light headed"] <- "confusion"
symptom_data$symptom[symptom_data$symptom == "confusion / disorientation"] <- "confusion"

# group and filter data to remove non-english symptoms and recode as factors

symptom_data <- symptom_data %>% 
  group_by(symptom) %>% 
  tally() %>% 
  filter(!row_number() %in% c(1:22)) %>% 
  filter(!row_number() %in% c(36:36)) %>% 
  arrange(n) %>% 
  mutate(symptom = fct_reorder(symptom, n))

# plot symptoms
  
ggplot(symptom_data, aes(x = symptom, y = n, fill = symptom))+
  geom_col()+
  theme_prism()+
  ylab("Number of Reports")+
    coord_flip()+
  xlab("Symptom")+
  theme(legend.position = "NULL")

ggsave("Figures/symptoms.png", dpi = 300, type = "cairo", units = "in", width = 6, height = 5.5)


