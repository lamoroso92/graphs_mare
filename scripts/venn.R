# Script - Gráfico Venn -----------------------------------------------------------------------

require(ggplot2)
require(ggvenn)
require(dplyr)
require(tidyr)
require(stringr)
require(reshape2)
require(RColorBrewer)
require(plyr)

setwd("D:/Unesp/cursos/curso_MetagMaRe/Aval/scripts/")

set.seed(1)

# Gerando dados mock --------------------------------------------------------------------------

t0 = sample(75:200, 100, replace = F)
t1 = sample(125:300, 100, replace = F)
t2 = sample(125:275, 100, replace = F)
t3 = sample(125:250, 100, replace = F)

ls.venn <- list(
  `Controle` = t0, 
  `1 mês` = t1, 
  `6 meses` = t2, 
  `12 meses` = t3
)

# Gráfico -------------------------------------------------------------------------------------

g1 <- 
  ggvenn(ls.venn, 
       fill_color = c("#5e5b52", "#fab430", "#fc6462", "#dc3b20"), 
       stroke_size = 0.5, 
       set_name_size = 5, 
       text_size = 4.5, 
       show_percentage = F) + 
  scale_x_discrete(expand = c(0.25, 0.25)) + 
  scale_y_discrete(expand = c(0.15, 0.15)) + 
  theme(legend.position = "none",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5, colour = 'black'),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

g1

ggsave("./figures/venn.png", 
       plot = g1, 
       dpi = 1200, 
       device = "png",
       width = 6, 
       height = 5)

# Fim -----------------------------------------------------------------------------------------
