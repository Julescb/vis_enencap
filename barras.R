#establecer el ambiente
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen=999)

#cargar paquetes
library(pacman)
p_load(tidyverse, scales, ggrepel, readxl, janitor, ggthemes, hrbrthemes, magick, ggalt, treemapify)


#cargar base 
bd3 <-read_excel("01_datos/datos enecap.xlsx", sheet = "graf3")

### gráficas de barra ----

bd3 %>% 
  filter(ambgob == "federal",
         delito != "Estados Unidos Mexicanos",
         delito != "Otro") %>% 
  arrange(tot_atn) %>% 

  ggplot()+
  geom_col(aes(reorder(delito, tot_atn), tot_atn))+
  coord_flip()+
  scale_y_continuous(expand =  c(0,0))+
  theme(text=element_text(family="Arial", color = "grey10"), 
        plot.title=element_text(size=18, hjust = 0),
        plot.subtitle = element_text(size=45,hjust = 0, vjust =1),
        plot.caption = element_text(size=20,hjust = 0),
        axis.line.x = element_line(size = 1, colour = "grey70"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=13, margin = margin(t = 15, r = 0, b = 0, l = 0),face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.y=element_text(size=12, margin = margin(t = 15, r = 0, b = 0, l = 0),face = "bold"),
        
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.grid.major.y = element_line(colour = "gray90"),
        panel.ontop = FALSE, 
        legend.position="top", 
        legend.text = element_text(size=18),
        legend.key = element_rect(fill = "transparent", color = NA))




##### PORCENTAJEEESSSS -----

##misma gráfica pero con porcentajes
(g_fed <- bd3 %>% 
  filter(ambgob == "federal",
         delito != "Estados Unidos Mexicanos",
         delito != "Otro", delito != "Otros tipos de robo") %>% 
  arrange(porc_atn) %>% 
  
  ggplot()+
  geom_col(aes(reorder(delito, porc_atn), (porc_atn/100), fill = delito), fill = "steelblue4")+
  coord_flip()+
  scale_y_continuous(expand =  c(0,0), labels = scales::percent))+
  labs(title="Delitos más atendidos a nivel federal, 2017.",
       subtitle = "Porcentaje de agentes con funciones operativas (no investigación) que los atendió.")+
  theme(text=element_text(family="Arial", color = "grey10"), 
        plot.title=element_text(size=18, hjust = -.7),
        plot.subtitle = element_text(size=16,hjust = -1.6, vjust =1),
        plot.caption = element_text(size=20,hjust = 0),
        axis.line.x = element_line(size = 1, colour = "grey70"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=13, margin = margin(t = 15, r = 0, b = 0, l = 0),face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.y=element_text(size=12, margin = margin(t = 15, r = 0, b = 0, l = 0),face = "bold"),
        
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.grid.major.y = element_line(colour = "gray90"),
        panel.ontop = FALSE, 
        legend.position="none", 
        legend.text = element_text(size=18),
        legend.key = element_rect(fill = "transparent", color = NA))
)

ggsave(filename = "03_graficas/barrasfed.png", 
       width = 15,    #Ancho
       height = 7, 
       dpi = 200)

#estatal

(bd3 %>% 
  filter(ambgob == "estatal",
         delito != "Estatal",
         delito != "Otro", delito != "Otros tipos de robo") %>% 
  arrange(porc_atn) %>% 
  
  ggplot()+
  geom_col(aes(reorder(delito, porc_atn), (porc_atn/100), fill = delito), fill = "steelblue4")+
  coord_flip()+
  scale_y_continuous(expand =  c(0,0), labels = scales::percent)+
    labs(title="Delitos más atendidos a nivel estatal, 2017.",
         subtitle = "Porcentaje de agentes con funciones operativas (no investigación) que los atendió.")+
  theme(text=element_text(family="Arial", color = "grey10"), 
        plot.title=element_text(size=18, hjust = -.7),
        plot.subtitle = element_text(size=16,hjust = -1.6, vjust =1),
        plot.caption = element_text(size=20,hjust = 0),
        axis.line.x = element_line(size = 1, colour = "grey70"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=13, margin = margin(t = 15, r = 0, b = 0, l = 0),face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.y=element_text(size=12, margin = margin(t = 15, r = 0, b = 0, l = 0),face = "bold"),
        
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.grid.major.y = element_line(colour = "gray90"),
        panel.ontop = FALSE, 
        legend.position="none", 
        legend.text = element_text(size=18),
        legend.key = element_rect(fill = "transparent", color = NA))
)

ggsave(filename = "03_graficas/barrasest.png", 
       width = 15,    #Ancho
       height = 7, 
       dpi = 200)  
  
#municipal
(g_mun <- bd3 %>% 
  filter(ambgob == "municipal",
         delito != "Municipal",
         delito != "Otro", delito != "Otros tipos de robo") %>% 
  arrange(porc_atn) %>%
  
  ggplot()+
  geom_col(aes(reorder(delito, porc_atn), (porc_atn/100), fill = delito), fill = "steelblue4")+
  coord_flip()+
  scale_y_continuous(expand =  c(0,0), labels = scales::percent)+
  labs(title="Delitos más atendidos a nivel municipal, 2017.",
       subtitle = "Porcentaje de agentes con funciones operativas (no investigación) que los atendió.")+
  theme(text=element_text(family="Arial", color = "grey10"), 
        plot.title=element_text(size=18, hjust = -.7),
        plot.subtitle = element_text(size=16,hjust = -1.6, vjust =1),
        plot.caption = element_text(size=20,hjust = 0),
        axis.line.x = element_line(size = 1, colour = "grey70"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(size=13, margin = margin(t = 15, r = 0, b = 0, l = 0),face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.y=element_text(size=12, margin = margin(t = 15, r = 0, b = 0, l = 0),face = "bold"),
        
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.grid.major.y = element_line(colour = "gray90"),
        panel.ontop = FALSE, 
        legend.position="none", 
        legend.text = element_text(size=18),
        legend.key = element_rect(fill = "transparent", color = NA))
)

ggsave(filename = "03_graficas/barrasmun.png", 
       width = 15,    #Ancho
       height = 7, 
       dpi = 200)  

