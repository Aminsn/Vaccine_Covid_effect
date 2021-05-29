rm(list = ls())
library(tidyverse)
library(zoo)
library(colorspace)
library(ggfx)
library(ggtext)
library(ragg)

vaccine_fits <- read_csv("~/Desktop/vaccine_fits.csv")
vaccine_counterfactual <- read_csv("~/Desktop/vaccine_counterfactual.csv")

df_wide <- data.frame(Time = vaccine_fits$date ,Fit = vaccine_fits$`United States`,  Counterfactual = vaccine_counterfactual$`United States`)
df_long <- df_wide %>% pivot_longer(values_to = "Turbidity",
                                         names_to = "Type",
                                         -Time) 

df <- df_long
df$Turbidity[which(df$Turbidity < 0)] <- 0
# df <- Fit_fit %>% 
#   mutate(Turbidity = replace(Turbidity, is.na(Turbidity) == TRUE, 0))

# df_ribbon <- df %>% 
#   mutate(Turbidity = replace(Turbidity, is.na(Turbidity) == TRUE, 0)) %>% 
#   group_by(Time) %>% 
#   summarize(
#     min = min(Turbidity, na.rm = T),
#     max = max(Turbidity, na.rm = T),
#     diff = max - min,
#     Type = max == Turbidity
#   ) %>% 
#   slice(1) %>% 
#   mutate(
#     Type = if_else(isTRUE(Type), "Counterfactual", "Counterfactual"),
#     Type = fct_rev(Type)
#   ) 

df_ribbon <-
  df %>% 
  group_by(Time) %>% 
  summarize(
    min = min(Turbidity),
    max = max(Turbidity),
    diff = max - min,
    Type = max == Turbidity
  ) %>% 
  slice(1) %>% 
  mutate(
    Type = if_else(isTRUE(Type), "Fit", "Counterfactual"),
    Type = fct_rev(Type)
  )



cols <- c("#a34fde","#fbc200")

ggplot(df, aes(Time, Turbidity, color = Type)) +
  geom_ribbon(
    data = df_ribbon,
    aes(x = Time, ymin = min, ymax = max),
    alpha = .7, inherit.aes = FALSE, fill = cols[1]
  ) +
  with_blur(
    geom_line(data = filter(df, Type == "Fit"), 
              color = cols[2], size = 2),
    colour = lighten(cols[2], .1), sigma = 3
  ) +
  labs(x = "time",
       caption = "By: Amin Shoari Nejad  |  Data source: Johns Hopkins University, University of Oxford • Counterfactual line shows how the number of cases would be different without vaccination") + 
  geom_line(data = filter(df, Type == "Counterfactual"), 
            color = cols[1], size = 2)+
  geom_line(size = 1.5) +
  geom_richtext(
    aes(x = as.Date("2021-02-15"), y = 80, 
        label = "Vaccination effect against COVID-19 in United States  <br><b style='color:#a34fde;font-size:20pt;'>Counterfactual</b> vs <b style='color:#fbc200;font-size:20pt;'>Observed</b></span>"),
    color = "grey80", size = 7, lineheight = 1.5,
    stat = "unique", fill = NA, label.color = NA
  ) +
  scale_x_date(date_breaks = "month", date_labels = "%Y-%m", expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,90)) +
  scale_color_manual(values = cols, name = "", labels = c("Counterfactual","Observed")) +
  theme_void() +
  labs(y = "Number of Cases (per 100k residents)") + 
  theme(
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    plot.background = element_rect(fill = "grey25"),
    axis.text.x = element_text(color = "grey80",
                               size = 10, face = "bold", margin = margin(t = 6)),
    axis.text.y = element_text(color = "grey80",
                               size = 10, face = "bold", margin = margin(t = 6)),
    panel.grid.major.x = element_line(color = "grey50", linetype = "13", size = .4),
    plot.margin = margin(15, 30, 10, 30),
    plot.caption = element_text( color = "grey50", size = 8,
                                 hjust = .5, margin = margin(t = 30, b = 0)),
    legend.text = element_text(color = "grey80", size = 10, face = "bold"),
    legend.title = element_text(color = "grey80", size = 15, face = "bold"),
    axis.title.y = element_text(color = "grey80", size = 10, face = "bold", angle = 90,
                                margin = margin(t = 0, r = 20, b = 0, l = 0)))
  ) 


# file <- here::here("04_magical", "04_magical.pdf")
# 
# ggsave(file, width = 12, height = 8, device = cairo_pdf)
# 
# pdftools::pdf_convert(
#   pdf = file, 
#   filenames = glue::glue("{str_remove(file, '.pdf')}.png"),
#   format = "png", dpi = 200
# )



