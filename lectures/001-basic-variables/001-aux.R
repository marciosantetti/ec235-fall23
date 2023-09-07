library(tidyverse)
library(ggthemes)
library(patchwork)
library(ggrepel)

theme_ms <- function() {
  theme_minimal(base_family = "IBM Plex Sans") +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = "grey80", color = NA),
          #axis.title.x = element_text(hjust = 0),
          #axis.title.y = element_text(hjust = 1),
          legend.title = element_text(face = "bold"))
}

theme_set(theme_ms())


####


dat <- read_csv("GDPC1.csv")
cpi <- read_csv("CPILFESL.csv")
un <- read_csv("UNRATE.csv")

dat <- dat %>% 
  add_column(cpi$CPILFESL_PC1) %>% 
  add_column(un$UNRATE) %>% 
  rename(gdp = GDPC1_PC1,
         core_cpi = `cpi$CPILFESL_PC1`,
         unrate = `un$UNRATE`,
         date = DATE)

dattx <- dat %>% 
  mutate(ch_unrate = c(NA, diff(unrate)))


dattx %>% 
  ggplot(aes(x = gdp, y = ch_unrate)) +
  geom_point(shape = 21, size = 2.2, fill = "#5c7d81", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "GDP growth",
       y = "Change in unemployment",
       title = "Okun's law, United States: 2000Q1–2023Q2")




###


dattx %>% 
  ggplot(aes(x = unrate, y = core_cpi)) +
  geom_point(shape = 21, size = 2.2, fill = "#bc202c", alpha = 0.5) +
  geom_vline(xintercept = 3, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Unemployment rate",
       y = "Core inflation rate",
       title = "Phillips curve, United States: 2000Q1–2023Q2")
