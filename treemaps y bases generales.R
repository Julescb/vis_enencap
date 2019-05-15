#establecer el ambiente
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen=999)

#cargar paquetes
library(pacman)
p_load(tidyverse, scales, ggrepel, readxl, janitor, ggthemes, hrbrthemes, magick, ggalt, treemapify, data.table)


#LIMPIEZA PARA BASE CORRECTA TREEMAPS
#cargar base para delitos
bd<-read_excel("01_datos/datos enecap.xlsx", sheet = "graf1")

## comenzar con la separación prevención, reacción, investigación

#1 prevención
bd_prev <- bd %>% 
  select(delito, elementos, tot_atn, porc_atn, tot_prev, porc_prev) %>% 
  rename(frecuencia =  tot_prev,
         porcentaje = porc_prev)

bd_prev$cat <- "prevención"

openxlsx::write.xlsx(as.data.frame(bd_prev), "01_datos/base prevención.xlsx")


#2 reacción
bd_reac <- bd %>% 
  select(delito, elementos, tot_atn, porc_atn, tot_reac, porc_reac) %>% 
  rename(frecuencia =  tot_reac,
         porcentaje = porc_reac)

bd_reac$cat <- "reacción"

openxlsx::write.xlsx(as.data.frame(bd_reac), "01_datos/base reacción.xlsx")


#3 investigación
bd_inv <- bd %>% 
  select(delito, elementos, tot_atn, porc_atn, tot_inv, porc_inv) %>% 
  rename(frecuencia =  tot_inv,
         porcentaje = porc_inv)

bd_inv$cat <- "investigación"

openxlsx::write.xlsx(as.data.frame(bd_inv), "01_datos/base investigación.xlsx")

#4. Juntas por si acaso

bd_delitos <- rbindlist(list(bd_prev, bd_reac, bd_inv))

openxlsx::write.xlsx(as.data.frame(bd_delitos), "01_datos/base_delitos.xlsx")



###base incidentes ----
bd2<-read_excel("01_datos/datos enecap.xlsx", sheet = "graf2")

#1 prevención
ibd_prev <- bd2 %>% 
  select(incidente, elementos, tot_atn, porc_atn, tot_prev, porc_prev) %>% 
  rename(frecuencia =  tot_prev,
         porcentaje = porc_prev)

ibd_prev$cat <- "prevención"

openxlsx::write.xlsx(as.data.frame(ibd_prev), "01_datos/base prevención_i.xlsx")


#2 reacción
ibd_reac <- bd2 %>% 
  select(incidente, elementos, tot_atn, porc_atn, tot_reac, porc_reac) %>% 
  rename(frecuencia =  tot_reac,
         porcentaje = porc_reac)

ibd_reac$cat <- "reacción"

openxlsx::write.xlsx(as.data.frame(ibd_reac), "01_datos/base reacción_i.xlsx")


#3 investigación
ibd_inv <- bd2 %>% 
  select(incidente, elementos, tot_atn, porc_atn, tot_inv, porc_inv) %>% 
  rename(frecuencia =  tot_inv,
         porcentaje = porc_inv)

ibd_inv$cat <- "investigación"

openxlsx::write.xlsx(as.data.frame(ibd_inv), "01_datos/base investigación_i.xlsx")

#4. Juntas por si acaso

bd_incidentes <- rbindlist(list(ibd_prev, ibd_reac, ibd_inv))

openxlsx::write.xlsx(as.data.frame(bd_incidentes), "01_datos/base_incidentes.xlsx")


###Treemap revuelto ----

(treemap_rev<- bd_delitos %>% 
   filter(delito != "Estados Unidos Mexicanos") %>% 
   mutate(porcentaje = round(porcentaje, 2)) %>% 
   ggplot(aes(area = frecuencia, 
              fill = cat,
              alpha = frecuencia)) +
   geom_treemap(col = "white") +
   geom_treemap_text(aes(area = frecuencia, label = delito),
                     fontface = "bold", 
                     fill = "white", 
                     color = "white",
                     alpha = 1) +
   geom_treemap_text(aes(area = frecuencia, 
                         label = paste0("Total:", comma(frecuencia))), 
                     color = "white", 
                     padding.y = unit(8, "mm"), 
                     size = 16,
                     alpha = 1) +
   geom_treemap_text(aes(area = frecuencia, label = paste(porcentaje, 
                                                          " % del total nacional", 
                                                          sep = "")), 
                     color = "white", 
                     padding.y = unit(14.5, "mm"), 
                     size = 15,
                     alpha = 1) +
   scale_fill_manual(values = c("deepskyblue4", "darkgreen", "darkorange3"),
                     breaks = c("investigación", "prevención", "reacción"),
                     labels = c(" Investigación  ", " Prevención  ", " Reacción  "),
                     guide = guide_legend(title.position = "bottom")) + 
   scale_alpha(range = c(0.5, 1), guide = F) +
   theme(legend.position="bottom",
         legend.text = element_text(size = 15),
         legend.title = element_blank(),
         strip.background = element_rect(fill="gray99", 
                                         linetype="blank"),
         panel.border = element_rect(color = "gray98",
                                     fill=NA),
         panel.background = element_rect(fill = "grey70", 
                                         size = 2,
                                         linetype = "solid"),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank())
)


ggsave(filename = "03_graficas/treemap_gral.png", 
       treemap_rev,
       bg = "transparent",
       width = 16,    #Ancho
       height = 9, 
       dpi = 200)

## Treemap incidentes

(treemap_rev_i<- bd_incidentes %>% 
    filter(incidente != "Estados Unidos Mexicanos") %>% 
    mutate(porcentaje = round(porcentaje, 2)) %>% 
    ggplot(aes(area = frecuencia, 
               fill = cat,
               alpha = frecuencia)) +
    geom_treemap(col = "white") +
    geom_treemap_text(aes(area = frecuencia, label = incidente),
                      fontface = "bold", 
                      fill = "white", 
                      color = "white",
                      alpha = 1) +
    geom_treemap_text(aes(area = frecuencia, 
                          label = paste0("Total:", comma(frecuencia))), 
                      color = "white", 
                      padding.y = unit(8, "mm"), 
                      size = 16,
                      alpha = 1) +
    geom_treemap_text(aes(area = frecuencia, label = paste(porcentaje, 
                                                           " % del total nacional", 
                                                           sep = "")), 
                      color = "white", 
                      padding.y = unit(14.5, "mm"), 
                      size = 15,
                      alpha = 1) +
    scale_fill_manual(values = c("deepskyblue4", "darkgreen", "darkorange3"),
                      breaks = c("investigación", "prevención", "reacción"),
                      labels = c(" Investigación  ", " Prevención  ", " Reacción  "),
                      guide = guide_legend(title.position = "bottom")) + 
    scale_alpha(range = c(0.5, 1), guide = F) +
    theme(legend.position="bottom",
          legend.text = element_text(size = 15),
          legend.title = element_blank(),
          strip.background = element_rect(fill="gray99", 
                                          linetype="blank"),
          panel.border = element_rect(color = "gray98",
                                      fill=NA),
          panel.background = element_rect(fill = "grey70", 
                                          size = 2,
                                          linetype = "solid"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
)


ggsave(filename = "03_graficas/treemap_gral_i.png", 
       treemap_rev_i,
       bg = "transparent",
       width = 16,    #Ancho
       height = 9, 
       dpi = 200)


