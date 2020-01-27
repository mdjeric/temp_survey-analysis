index_distribution <- function(dt, section, ...)  {
  
  dt <- schdl_index(gpss, section, ...)
  vrbl <- sym(colnames(dt[1]))
  section <- str_to_lower(section)
  
  ggplot(dt, aes(x = !!vrbl, y = ..count..)) +
    geom_density(na.rm = TRUE,
                 fill = base_color,
                 color = NA,
                 alpha = 0.9) + 
    scale_x_continuous(limits = c(1, 5), expand = c(0, 0)) +
    theme_gpss(grid = "Y") +
    labs(title = paste("Distribution of the index on", section),
         subtitle = "Density function",
         x = "Value",
         y = "Count",
         caption = gpsc_caption
         )
}


# cdbk[7,1] <- "PS0"



# COMPARATIVE -------------------------------------------------------------



# temp_gpss <- gpss %>% 
#   filter(!is.na(DQ3)) %>% 
#   select(AC0, DQ3) 
# 
# ggplot(temp_gpss, aes(x = AC0, y = ..count..)) +
#   geom_density(data = select(temp_gpss, -DQ3),
#                aes(fill = "all people surveyed"),
#                na.rm = TRUE, color = NA) +
#   geom_density(aes(fill = DQ3), na.rm = TRUE, color = NA) +
#   facet_wrap( ~ DQ3, nrow = 1) +
#   scale_x_continuous(limits = c(1, 5), expand = c(0, 0)) +
#   scale_fill_manual(values = c("#bdbdbda0", likert_colors_v2[c(1, 5)], NA),
#                     guide = FALSE
#   ) +
#   theme_ipsum(base_family = "Fira Sans",
#               plot_margin = margin(5, 5, 5, 5),
#               grid = "Y",
#               base_size = 8,
#               plot_title_size = 12,
#               subtitle_size = 10,
#               caption_size = 7) +
#   labs(title = "Distribution of the index on courses",
#        subtitle = "All surveyed students are reference category",
#        x = "Value",
#        y = "Count",
#        caption = gpsc_caption
#   ) 

# strip.text = element_text(margin = margin(0, 0, 0.2, 0, "cm")),
