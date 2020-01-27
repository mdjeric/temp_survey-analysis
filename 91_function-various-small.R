
## puls tibble of data frames with one index/modul and other variables

schdl_index <- function(dt, section, ..., modul = FALSE){
  if (modul) {
         dt %>% select(subset(cdbk, (module == section) & modul_index)$code, ...)
  }  else  {
         dt %>% select(subset(cdbk, (schdl  == section) & schdl_index)$code, ...)
  }
  
}

# schdl_index(gpss, "Courses", RR00)
# schdl_index(gpss, "Academics", RR00, modul = TRUE, DQ3)


# Calculate some statistics -----------------------------------------------

mean_index <- function(dt, section, modul = FALSE)  {
  round(mean(pull(schdl_index(dt, section, modul = modul)), na.rm = TRUE), 1)
}

# mean_index(gpss, "Courses")

sd_index <- function(dt, section, modul = FALSE)  {
  round(sd(pull(schdl_index(dt, section, modul = modul)), na.rm = TRUE), 1)
}

# sd_index(gpss, "Courses")


n_index <- function(dt, section, modul = FALSE)  {
  schdl_index(dt, section, modul = modul) %>%
    drop_na() %>% count() %>% pull()

}

# n_index(gpss, "Courses")


