# Script - Gráfico de Alfa div ----------------------------------------------------------------

require(reshape2)
require(ggplot2)
require(ggpubr)
require(artyfarty)
require(sjPlot)

setwd("D:/Unesp/cursos/curso_MetagMaRe/Aval/scripts/")

# Gerando dados mock --------------------------------------------------------------------------

dt_boxplot = data.frame("dummy"=0)

for (i in c("T0", "T1", "T2", "T3")) {
  for (j in 1:30) {
    text = paste0("SAMPLE_", i,"_",j)
    dt_boxplot[[text]] = 0
  }
}

dt_boxplot = dt_boxplot[-1]

dt_boxplot[1, 1:30   ] = ceiling(rnorm(30, mean = 300, sd = 35))
dt_boxplot[1, 31:60  ] = ceiling(rnorm(30, mean = 80 , sd = 35))
dt_boxplot[1, 61:90  ] = ceiling(rnorm(30, mean = 100, sd = 35))
dt_boxplot[1, 91:120 ] = ceiling(rnorm(30, mean = 145, sd = 35))

melt_dt_boxplot = melt(dt_boxplot)

melt_dt_boxplot[["time"]] = 
  factor(rep(c(
    rep("Controle (pré-incêndio)", 30), rep("1 mês", 30), rep("6 meses", 30), rep("12 meses", 30))), 
    levels = c("Controle (pré-incêndio)", "1 mês", "6 meses", "12 meses"))


# Gráfico -------------------------------------------------------------------------------------

g1 = 
  ggplot(melt_dt_boxplot, aes(x=time, y=value, fill=time))+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(shape=21, position=position_jitter(0.4), size = 2, alpha = .25) +
  xlab(NULL) + 
  ylab("Riqueza Observada") + 
  theme_classic() +
  scale_fill_manual(values=c("#5e5b52", "#fab430", "#fc6462", "#dc3b20")) +
  
  theme(legend.position = "none",
        legend.text = element_text(size = 10),
        legend.key.width = unit(10, "pt"),
        legend.key.height = unit(10, "pt"),
        legend.title = element_blank(),
        
        axis.title.y = element_text(size = 14, face = "bold", colour = "black"),
        axis.title.x = element_text(face = "bold", colour = "black"),
        axis.text.x = element_text(size = 8, colour = "black", angle = 90, vjust = 0.2, hjust = 1),
        axis.text.y = element_text(size = 8, colour = "black"),
        
        strip.text.y = element_text(face = "bold", size = 10, hjust = .5),
        strip.text.x = element_text(face = "bold", size = 10, hjust = .5),
        strip.placement = "outside",
        #strip.background.y = element_blank(),
        
        panel.spacing.x = unit(5, "pt"),
        panel.spacing.y = unit(2, "pt"),
        panel.grid.major = element_line(colour = 'gray75', linetype = 'dotted'), 
        panel.grid.minor = element_blank(),
        
        aspect.ratio = 1) 
g1

ggsave("./figures/alfadiv.png", 
       plot = g1, 
       dpi = 1200, 
       device = "png",
       width = 5, 
       height = 4)

# Fim -----------------------------------------------------------------------------------------
