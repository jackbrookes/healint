

# plot symptoms overall
  
ggplot(symptoms_recoded, aes(x = replacement, y = n, fill = n))+
  geom_col()+
  theme_prism()+
  ylab("Number of Reports")+
    coord_flip()+
  xlab(NULL)+
  scale_y_continuous(expand = c(0,0))+
  theme(legend.position = "NULL",
        text = element_text(size = 8))+
  scale_fill_gradientn(colours = c("#F7F4F9", "#E7E1EF", "#D4B9DA", "#C994C7", "#DF65B0", "#E7298A", "#CE1256"))

ggsave("Figures/symptoms.png", dpi = 300, type = "cairo", units = "in", width = 5, height = 4)


# plot categories of symptoms
symptoms_recoded %>% 
  filter(classification != "None") %>% 
  group_by(classification) %>% 
  summarise(n = sum(n)) %>% 
  ggplot(aes(x = classification, y = n, fill = classification))+
  geom_col(colour = "black")+
  theme_prism()+
  ylab("Number of Reports")+
  xlab(NULL)+
  scale_y_continuous(expand = c(0,0))+
  theme(legend.position = "NULL",
        text = element_text(size = 8))+
  scale_fill_manual(values = c("#f27993" ,"#ff7d73", "#ff8d49", "#ffa600"))

ggsave("Figures/symptoms.png", dpi = 300, type = "cairo", units = "in", width = 4, height = 3)
