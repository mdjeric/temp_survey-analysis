
theme_gpss <- function(grid = "XxYy", ...)  {
  theme_ipsum(base_family     = "Fira Sans",
              plot_margin     = margin(5, 5, 5, 5),
              grid            = grid,
              base_size       = 8,
              plot_title_size = 12,
              subtitle_size   = 10,
              caption_size    = 7,
              ...
              )  %+replace%
    theme(legend.text = element_text(family = "Fira Sans Light",
                                     size   = 7.2,
                                     vjust  = 1
                                     # margin = margin(1, 0, 0, 0)
                                     )
          )
}


likert_colors_v2 <- c("#440154FF", "#623D70FF", "NA", "#76DF7CFF", "#5DC863FF")
base_color       <- c("#008B8B")
base_grey        <- c("#BDBDBDA0")
three_groups_gyp <- c("#5DC863FF", "#FDE725FF", "#440154FF")
three_groups_ytp <- c("#FDE725FF", "#21908CFF", "#440154FF")
