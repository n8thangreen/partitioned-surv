rm(list=ls())
library(magrittr)
library(data.table)
library(ggplot2)
library(gghighlight)

path = '/Users/nico/OneDrive - University College London/FDA-partitioned-surv/'
mx <- read.table(file = paste0(path,'/Data/mx_2021-2025.txt'))
lx <- read.table(file = paste0(path,'/Data/lx_2021-2025.txt'))/1000

rownames(mx) = rownames(lx) = 1922:2025
colnames(mx) = 0:110; colnames(lx) = 0:110
forecast = 2021:2025
years = 1922:2020
x = 0:110

data <- t(mx)
aux.mx <- data %>% 
as.data.table %>% 
  .[, id := 1:nrow(.)] %>% 
  melt(id = c('id'), variable.name = "t")%>% 
  .[t %in% 1922:2025] %>%
  .[, color := fifelse(t %in% 1922:2020, "1922-2022", as.character(t))] %>% 
  .[]
# aux.mx <- data %>%
#   as.data.table %>%
#   cbind(x) %>%
#   melt(id.vars = 'x') %>%
#   .[, pred := variable %in% forecast] %>%
#   .[]

data <- t(lx)
aux.lx <- data %>% 
  as.data.table %>% 
  .[, id := 1:nrow(.)] %>% 
  melt(id = c('id'), variable.name = "t")%>% 
  .[t %in% 1922:2025] %>%
  .[, color := fifelse(t %in% 1922:2020, "1922-2022", as.character(t))] %>% 
  .[]
  #.[, pred := variable %in% forecast] %>% 
  # .[]

col <- c("gray", scales::hue_pal()(5))
names(col) <- unique(aux.lx$color)


# aux.lx %>% 
#   ggplot(aes(id, value, col = color, group = t)) + 
#   theme_bw() + 
#   scale_color_manual(values = col)

aux.mx[, type := rep('mx',nrow(aux.mx))]
aux.lx[, type := rep('lx',nrow(aux.lx))]

aux <- bind_rows(aux.mx,aux.lx)

pdf(file = paste0(path,'/Figures/forecasts.pdf'),width=8,height=4,paper='special')
print({ 
aux %>%
  ggplot(aes(id, value, group = t, col = color, alpha = color)) +
  geom_line(alpha = .8) +
  theme_bw() +
  #scale_color_manual(values = c('gray', scales::hue_pal()(1))) +
  scale_color_manual(values = col)+
  #scale_alpha_manual(values = c(.3, 1)) +
  xlab('Age cohorts') +
  facet_wrap(~type, scales = "free_y", strip.position = "left", 
             labeller = as_labeller(c(lx = "Persons (in thousands) surviving to age-cohort", mx = "Log-Mortality rate") ) ) +
  ylab(NULL) +
  theme(strip.background = element_rect(fill = "white",color = "white")) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  theme(strip.placement = "outside") +
  theme(axis.text.y = element_text(size=10, color="black"),
        axis.text.x = element_text(size=10, color="black")) +
  theme(strip.text.y = element_text(size = 10))+
  theme(legend.text = element_text(face='bold'))+
  guides(color=guide_legend(nrow=1, byrow=TRUE))
  })  
dev.off()
