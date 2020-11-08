library(stringr)


ends_in_form <- function(string) {
  string %>% 
    grepl(pattern = "[w, d, l]{5}$")
}



remove_form <- function(string) {
  str_sub(
    string = string,
    1,
    end = str_length(string) - 5 * ends_in_form(string)
  )
}

"Hondurasddlww" %>% 
  remove_form()
