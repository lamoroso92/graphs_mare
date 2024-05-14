# Script - Gráfico de Beta div ----------------------------------------------------------------

require(reshape2)
require(ggplot2)
require(ggpubr)
require(artyfarty)
require(sjPlot)
require(ggforce)

setwd("D:/Unesp/cursos/curso_MetagMaRe/Aval/scripts/")

set.seed(1)

# Gerando dados mock --------------------------------------------------------------------------

dt_dotplot = data.frame(x=0,y=0,time="a")

dt_dotplot[1:30,  ] = c(rnorm(30, mean = -0.2, sd = 0.2), rnorm(30, mean = -0.3, sd = .15), rep("Controle (pré-incêndio)", 30))
dt_dotplot[31:60, ] = c(rnorm(30, mean = 0.25, sd = 0.25), rnorm(30, mean = 0.5, sd = .1), rep("1 mês", 30))
dt_dotplot[61:90, ] = c(rnorm(30, mean = 0.1, sd = 0.25), rnorm(30, mean = 0.5, sd = .1), rep("6 meses", 30))
dt_dotplot[91:120,] = c(rnorm(30, mean = 0.1, sd = 0.25), rnorm(30, mean = 0.35, sd = .1), rep("12 meses", 30))

dt_dotplot[,1] = as.numeric(dt_dotplot[,1])
dt_dotplot[,2] = as.numeric(dt_dotplot[,2])
dt_dotplot[,3] = factor(dt_dotplot[,3], levels = c("Controle (pré-incêndio)", "1 mês", "6 meses", "12 meses"))


# Gráfico -------------------------------------------------------------------------------------

g1 = 
  ggplot(dt_dotplot, aes(x=x ,y=y, fill = time)) +
  geom_point(shape = 21, size = 3) +
  scale_fill_manual(values=c("#5e5b52", "#fab430", "#fc6462", "#dc3b20"))  +
  xlab("PCoA 2") + 
  ylab("PCoA 1") + 
  theme_bw() +
  
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.key.width = unit(10, "pt"),
        legend.key.height = unit(10, "pt"),
        legend.title = element_blank(),
        
        axis.title.y = element_text(size = 12, face = "bold", colour = "black"),
        axis.title.x = element_text(size = 12, face = "bold", colour = "black"),
        axis.text.x = element_text(size = 8, colour = "black"),
        axis.text.y = element_text(size = 8, colour = "black"),
        
        strip.text.y.left = element_blank(),
        strip.text.x = element_text(face = "bold", size = 10, hjust = .5),
        strip.placement = "outside",
        #strip.background.y = element_blank(),
        
        panel.spacing.x = unit(5, "pt"),
        panel.spacing.y = unit(2, "pt"),
        panel.grid.major = element_line(colour = 'gray75', linetype = 'dotted'), 
        panel.grid.minor = element_blank(),
        
        aspect.ratio = 1) 

g1

ggsave("./figures/betadiv.png", 
       plot = g1, 
       dpi = 1200, 
       device = "png",
       width = 5, 
       height = 4)

# Fim -----------------------------------------------------------------------------------------
