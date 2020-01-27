# Libraries for analysis --------------------------------------------------

library(foreign)
library(tidyverse)
library(Amelia)
library(car)
library(hrbrthemes)
library(viridis)
library(scales)
library(readxl)
library(Cairo)
library(corrr)
library(ggridges)
library(janitor)
library(cowplot)
library(ggrepel)
library(ggbeeswarm)
library(colorspace)
library(gtable)
library(broom)

# Libraries for reports ---------------------------------------------------

library(knitr)
library(kableExtra)
library(grid)
library(gridExtra)
library(ggiraph)
library(extrafont)


# select likert questions from specified schedule

schdl <- function(dt, schedule)  {
  dt %>%
    select(subset(cdbk, schdl == schedule & likert)$code)
}

# Likert tables -----------------------------------------------------------

likert_percent_table <- function(dt)  {
  ndt <- dt %>%
    gather() %>%
    group_by(key) %>%
    count(value) %>%
    na.omit() %>%
    mutate(pct = round(n / sum(n) * 100, 2)) %>%
    select(key, value, pct) %>%
    spread(value, pct)
  
  # reorder columns to be in the order as levels in factor
  # WHEN DECIDING TO SHORTEN TO "NEITHER" DO IT HERE
  # IN BOTH PLACES, AND ALSO IN SIMILAR WAY IN PRINTING OF THE PLOT!
  # NOT NECESSARY TO DO IT IN DATA
  # - maybe this is not the smartest idea, we'll see.
  # ndt <- ndt[, c("key", levels(dt[,1]))]
  ndt <- ndt[, c("key", levels(pull(dt, 1)))]
  
  return(ndt)
}

likert_freq_table <- function(dt) {
  ndt <- dt %>%
    gather() %>%
    group_by(key) %>%
    count(value) %>%
    na.omit() %>%
    select(key, value, n) %>%
    spread(value, n)
  
  # reorder columns to be in the order as levels in factor
  # ndt <- ndt[, c("key", levels(dt[,1]))]
  ndt <- ndt[, c("key", levels(pull(dt, 1)))]
  
  return(ndt)
}


likert_stat_table <- function(dt) {
  informacije <- sapply(dt,
                        function(x) cbind(mean(as.numeric(x),
                                               na.rm = TRUE),
                                          sd(as.numeric(x),
                                             na.rm = TRUE),
                                          sum(is.na(x)),
                                          length(x) - sum(is.na(x))
                        )
  )
  
  data.frame(mean = round(informacije[1, ], 2),
             SD = round(informacije[2, ], 2),
             N = informacije[4, ],
             NAs = informacije[3, ]
  )
}



likert_full_table <- function(dt)  {
  lik_freq <- likert_freq_table(dt)
  
  # reverse column indexes for selection, so the two tables
  # are "zipped", for whatever the number of columns is
  # i.e. from 1, 2, 3, 4, 5, 6, 7, 8, 9 make it be:
  #           1, 2, 6, 3, 7, 4, 8, 5, 9 
  lvls <- ncol(lik_freq) - 1 
  a  <- c(0, rep(seq(lvls + 2,lvls * 2 + 1), each = 2))
  a1 <- c(0, rep(0:1, lvls))
  b  <- c(1, rep(seq(2, lvls + 1), each = 2))
  b1 <- c(1, rep(1:0, lvls))
  reorder <- (a * a1) + (b * b1)
  
  # combine all tables - first frequencies and percentages
  # then add statistics in the end
  lik_perc <- likert_percent_table(dt)
  likrt <- inner_join(lik_freq, lik_perc, by = "key" )
  likrt <- likrt[, reorder] 
  bind_cols(likrt, likert_stat_table(dt))
}



print_likert_table_html <- function(dt, tbl_caption = "default")  {
  dt_print <- likert_full_table(dt)
  
  # select column names, and prepare them for upper row in table
  n_cat <- (ncol(dt_print) - 5) / 2
  lvlnms <- colnames(dt_print)[seq(2, n_cat*2, 2)]
  lvlnms <- gsub("\\..*", "", lvlnms)
  uprow <- c(" ", lvlnms, "Stats")
  hdr <- c(1, rep(2, n_cat), 4)
  names(hdr) <- uprow
  
  # prepare lower level of column names
  col_names <- c(" ", rep(c("f", "%"), n_cat), "M", "SD", "N", "NAs")
  
  # change `key` with question text from columns
  dt_print <- inner_join(dt_print, cdbk[, c("code", "question")],
                         by = c("key" = "code"))
  dt_print$key <- dt_print$question
  dt_print <- select(dt_print, -question)
  
  # print table
  kable(dt_print,
        col.names = col_names,
        caption = tbl_caption) %>%
    kable_styling("striped") %>%
    add_header_above(hdr)
}

print_likert_table_latex <- function(dt, tbl_caption = "default")  {
  dt_print <- likert_full_table(dt)
  
  # select column names, and prepare them for upper row in table
  n_cat <- (ncol(dt_print) - 5) / 2
  lvlnms <- colnames(dt_print)[seq(2, n_cat*2, 2)]
  lvlnms <- gsub("\\..*", "", lvlnms)
  # make a break in "Strongly.." 
  lvlnms <- gsub("Strongly ", "Strongly\n", lvlnms)
  lvlnms <- gsub("Very ", "Very\n", lvlnms)
  lvlnms <- gsub("Neither agree nor disagree", "Neither", lvlnms)
  uprow <- c(" ", lvlnms, "Stats")
  hdr <- c(1, rep(2, n_cat), 4)
  names(hdr) <- uprow
  
  # prepare lower level of column names % has to be double escaped
  col_names <- c(" ", rep(c("f", "\\%"), n_cat), "M", "SD", "N", "NAs")
  dt_print <- inner_join(dt_print, cdbk[, c("code", "question")],
                         by = c("key" = "code"))
  dt_print$key <- dt_print$question
  dt_print <- select(dt_print, -question)
  
  # print latex table
  kable(dt_print, "latex",
        booktabs = T,
        escape = F,
        col.names = col_names,
        valign = "middle", # for some reason this is not working
        caption = tbl_caption
  ) %>%
    column_spec(1, width = "6.5cm") %>%
    kable_styling(latex_options = c("striped", "scale_down", "HOLD_position"),
                  font_size = 7) %>%
    add_header_above(hdr)
}


print_likert_table <- function(dt, tbl_caption = "default"){
  if (knitr::is_html_output()) {
    print_likert_table_html(dt, tbl_caption)
  } else {
    print_likert_table_latex(dt, tbl_caption)
  }
}


# Likert plots ------------------------------------------------------------

plot_likert <- function(dt)  {
  means <- rownames_to_column(likert_stat_table(dt), "rowid")
  vairable_levels <- levels(pull(dt[, 1])) # to keep order of values
  
  # calculate counts and percentages
  dt <- dt %>%
    gather() %>%
    group_by(key) %>%
    count(value) %>%
    na.omit() %>%
    mutate(pct = n / sum(n))
  
  # join with full question texts
  dt <- inner_join(dt, means, by = c("key" = "rowid"))
  dt <- inner_join(dt, cdbk[, c("code", "question")],
                   by = c("key" = "code"))
  dt$question <- str_wrap(dt$question, width = 40)
  dt$value <- factor(dt$value, vairable_levels) # return order of values
  levels(dt$value) <- gsub(" agree nor disagree", "", levels(dt$value))
  plt <- ggplot(dt) +
    geom_bar(aes(x = reorder(question, mean),
                 y = pct,
                 fill = value
                 ),
             stat = "identity"
             ) +
    coord_flip() +
    scale_y_continuous(sec.axis = sec_axis(~(.-1)/(-1),
                                           labels = percent
    ),
    trans = "reverse",
    labels = percent
    ) +
    scale_fill_manual(values = likert_colors_v2) +
    guides(fill = guide_legend(keywidth = 3.8,
                               keyheight = 0.5,
                               label.hjust = 0.5,
                               label.vjust = 1,
                               label.position = "bottom"
                               )
    ) +
    theme_gpss(grid = "Xx") + 
    # theme_ipsum(plot_margin = margin(5, 5, 5, 5),
    #             grid = "Xx",
    #             base_family = "Fira Sans",
    #             base_size = 8,
    #             plot_title_size = 12,
    #             caption_size = 7
    #             ) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_blank(),
          legend.text = element_text(margin = margin(1, 0, 0, 0),
                                     vjust = 0
                                     # family = "Fira Sans Light",
                                     # size = 7.2,
                                     )
    ) +
    labs(caption = gpsc_caption)
}


# PRINTING
# first must print first plot using plot_likert function, to get all
# the dimensions set up, than grob it and proceede with just this 
# function for all other barplots
print_likert_plot_size <- function(dt, grob_first_plot) {
  likert_plot <- plot_likert(dt)
  grob_likert_plot <- ggplotGrob(likert_plot)
  grob_likert_plot$widths <- grob_first_plot$widths
  grob_likert_plot
}

#sample: 
# plot_1 <- print_likert_plot(smp)
# plot_1
# gr_plot_1 <- ggplotGrob(plot_1)
# print_likert_plot_size(smp2, gr_plot_1)

# TRY GRID.DRAW in the function directly, might work. 

# Tables with comparison categories ---------------------------------------




plot_scolarship <- function()  {
  
  plt_uni_financ <- ggplot(subset(dta_multi, mult_q == "Recieves"),
                           aes(x = reorder(question, percent),
                               y = percent,
                               fill = 0.5
                           )
  ) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis(limits = c(0,1)) + 
    theme_ipsum(plot_margin = margin(5, 5, 5, 5),
                grid = "Xx") +
    theme(legend.position = "none") +
    scale_y_percent(limits = c(0,1)) +
    labs(title = "Students recieveing university financing",
         subtitle = "Multiple selections possible",
         x = element_blank())
  
  plt_uni_financ}

plot_scholarship_colaboration <- function()  {
  #scolarship-colaboration-plot, fig.height=2.2, fig.width=9}
  plt_clbor <- ggplot(subset(dta_multi,
                             mult_q %in% c("With faculty",
                                           "With students")
  ),
  aes(x = reorder(question, percent),
      y = percent,
      fill = 0.5)
  ) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis(limits = c(0,1)) + 
    facet_grid(~ mult_q) +
    theme_ipsum(plot_margin = margin(5, 5, 5, 5),
                grid = "Xx") +
    theme(legend.position = "none",
          panel.spacing = unit(5, "lines")) +
    scale_y_percent(limits = c(0,1)) +
    labs(title = "Colaborative activities",
         subtitle = "Multiple selections possible",
         x = element_blank())
  plt_clbor
}

plot_finance <- function()  {
  
  dta_mult_test <- subset(dta_multi, mult_q == "Financial support")
  dta_mult_test$type <- "loans-foodstamps"
  dta_mult_test$type[c(2,5,7,8,12)] <- "univ"
  dta_mult_test$type[c(3,4,10,11,13)] <- "other"
  dta_mult_test$type <- as.factor(dta_mult_test$type)
  
  plt_gen_financ_test <- ggplot(dta_mult_test,
                                aes(x = reorder(question, percent),
                                    y = percent,
                                    fill = type)
  ) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("#440154FF", "#3B528BFF", "#21908CFF"),
                      guide = "none") + 
    theme_ipsum(plot_margin = margin(30, 30, 5, 5),
                grid = "Xx") +
    theme(legend.position = "none") +
    scale_y_percent(limits = c(0,1)) +
    labs(title = "How students support their studies",
         subtitle = "Multiple selections possible",
         x = element_blank())
  
  plt_gen_financ_test
}

# DESIGN OF FUNCTION FOR MULTIPLE COMPARISION, CREATE THE NOTION OF MULTIPLE COMPARISON, DEFAULT NONE, AND THEN
# IF NO COMPARISON IS USED, PRINT THESE PLOTS, IF A COMPARISION CATEOGORY IS ADDED, THEN PRINT THAT.