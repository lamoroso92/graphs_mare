# Script - Gráfico de Perfil relativo ---------------------------------------------------------

require(ggplot2)
require(dplyr)
require(tidyr)
require(stringr)
require(reshape2)
require(RColorBrewer)
require(plyr)

setwd("D:/Unesp/cursos/curso_MetagMaRe/Aval/scripts/")

set.seed(1)

# Gerando dados mock --------------------------------------------------------------------------

dt_barplot = data.frame("id" = "x")

for (i in c("T0", "T1", "T2", "T3")) {
  text = paste0("SAMPLE_", i)
  dt_barplot[[text]] = 0
}

for (i in 1:10) {
  
  dt_barplot[i,1] = paste0("Táxon ",i)
  
  dt_barplot[i,2:5] = 0
}

{
  
  dt_barplot[1:4,2] = rnorm(4, 50, 30)
  dt_barplot[5:7,2] = rnorm(3, 150, 30)
  dt_barplot[8:10,2] = rnorm(3, 250, 10)
  
  dt_barplot[1,3] = rnorm(1, 250, 20)
  dt_barplot[2:4,3] = rnorm(3, 50, 20)
  dt_barplot[5,3] = rnorm(1, 250, 20)
  dt_barplot[6:7,3] = rnorm(2, 50, 30)
  dt_barplot[8,3] = 0
  dt_barplot[9:10,3] = rnorm(2, 20, 8)
  
  dt_barplot[1,4] = rnorm(1, 250, 20)
  dt_barplot[2:4,4] = rnorm(3, 50, 20)
  dt_barplot[5,4] = rnorm(1, 250, 20)
  dt_barplot[6:7,4] = rnorm(2, 50, 30)
  dt_barplot[8,4] = 0
  dt_barplot[9:10,4] = rnorm(2, 10, 2)
  
  dt_barplot[1,5] = rnorm(1, 250, 20)
  dt_barplot[2:4,5] = rnorm(3, 50, 20)
  dt_barplot[5,5] = rnorm(1, 250, 20)
  dt_barplot[6:7,5] = rnorm(2, 50, 30)
  dt_barplot[8,5] = 0
  dt_barplot[9:10,5] = rnorm(2, 50, 10)
  
}

dt_barplot_comp = dt_barplot

for (i in seq(2,5)) {
  soma = sum(as.numeric(dt_barplot_comp[[i]]))
  dt_barplot_comp[i] = (as.numeric(dt_barplot_comp[[i]])/soma)
}

melt_barplot_comp = melt(dt_barplot_comp)

melt_barplot_comp[["id"]] = factor(melt_barplot_comp$id, levels = c(unique(melt_barplot_comp$id)))

melt_barplot_comp[["time"]] = 
  factor(rep(c(
    rep("Controle (pré-incêndio)", 10), rep("1 mês", 10), rep("6 meses", 10), rep("12 meses", 10))), 
    levels = c("Controle (pré-incêndio)", "1 mês", "6 meses", "12 meses"))

# Gráfico -------------------------------------------------------------------------------------

g1 <- ggplot(melt_barplot_comp, aes(fill=id, y=value, x=time)) + 
  geom_bar(position= position_stack(reverse = F), stat="identity", width = .9) +
  theme_classic() +
  scale_fill_manual(values = c("#c44536", "#f3722c", "#f8961e", "#f9c74f", "#90be6d","#43aa8b",
                               "#577590", "#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")) +
  xlab("") +
  ylab("Abundância Relativa") +
  
  theme(legend.position = "right",
        legend.text = element_text(size = 10),
        legend.key.width = unit(10, "pt"),
        legend.key.height = unit(10, "pt"),
        legend.title = element_blank(),
        
        axis.title.y = element_text(size = 12, face = "bold", colour = "black"),
        axis.title.x = element_text(size = 12, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 8, colour = "black", angle = 90, vjust = 0.2, hjust = 1),
        axis.text.y = element_text(size = 8, colour = "black"),
        
        strip.text.y.left = element_blank(),
        strip.text.x = element_text(face = "bold", size = 10, hjust = .5),
        strip.placement = "outside",
        #strip.background.y = element_blank(),
        
        panel.spacing.x = unit(5, "pt"),
        panel.spacing.y = unit(2, "pt"),
        panel.grid.major = element_line(colour = 'gray75', linetype = 'dotted'), 
        panel.grid.minor = element_blank(),
        
        aspect.ratio = 1.75)  + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  scale_x_discrete(expand = c(0.125, 0.125))

g1

ggsave("./figures/abundrel.png", 
       plot = g1, 
       dpi = 1200, 
       device = "png",
       width = 6, 
       height = 5)

# Fim -----------------------------------------------------------------------------------------
