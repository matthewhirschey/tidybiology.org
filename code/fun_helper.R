#fun_helper
filename_splitter <- function(file_name, 
                              var = "title"){
  file_name <- stringr::str_remove(file_name, "\\.Rmd")
  filename_info <- stringr::str_split(file_name,
                                      pattern = "-")
  output <- switch(var, 
                   module = filename_info[[1]][1], 
                   level = filename_info[[1]][2],
                   title = filename_info[[1]][3] %>% 
                     stringr::str_replace_all(pattern = "_", replacement = " ") %>% 
                     str_to_title_special(),
                   language = filename_info[[1]][4],
                   type = filename_info[[1]][5], 
                   stop("variable not found"))
  
  if(var == "level"){ 
    output <- dplyr::case_when(
      output == "a" ~ "beginner", 
      output == "b" ~ "intermediate", 
      output == "c" ~ "advanced", 
      TRUE ~ "super awesome"
    )}
  
  return(output)
}
#testing
# filename_splitter(file_name = "1-a-Introduction_to_Data_Science-multiple-module.Rmd")

str_to_title_special <- function(string){
  new_string <- stringr::str_to_title(string)
  package_names <- c(
    "Ggplot" = "ggplot", #omit 2
    "Dplyr" = "dplyr",
    "Tidyr" = "tidyr",
    "Readr" = "readr",
    "Purrr" = "purrr",
    "Tibble" = "tibble",
    "Stringr" = "stringr",
    "Forcats" = "forcats"
  )
new_title <- stringr::str_replace_all(new_string, package_names)  
return(new_title)
}

next_module <- function(module = "..."){
  if(module == "..."){
    
    link <- glue::glue("## Continue... 
                         \nGreat job, go back [home](http://www.tidybiology.org)")
  } else {
    module <- stringr::str_remove(module, "\\.Rmd")
    module_difficulty <- filename_splitter(file_name = module,
                                           var = "level")
    module_title <- filename_splitter(file_name = module,
                                      var = "title")
    link <- glue::glue("## Continue... 
                         \nThe next module in this {module_difficulty} pathway is 
                         \n[{module_title}]({module}.html)")
  }
  return(link)
}

#testing
#next_module()
#next_module(module = "1-a-Introduction_to_Data_Science-multiple-module")

find_modules <- function(module_filename = "1-a-Introduction_to_Data_Science-multiple-module.Rmd"){ #this takes all modules and gets the child modules out
  lines <- readLines(here::here("code", "www", module_filename))
  
  files <- str_subset(lines, pattern = "\\.Rmd") %>% 
    str_remove(pattern = "([[:symbol:]]+)[[:punct:]]r[[:space:]]child[[:symbol:]][[:punct:]]objects[[:punct:]]") %>%  #remove leader
    str_remove(pattern = "[[:punct:]]+$")
  
  #censor
  censor <- c("setup") #removes setup chunk
  files <- str_subset(files, censor, negate = TRUE)
  
  file_list <- list(
    module = rep_along(files, module_filename), 
    object = files
  )
  return(file_list)
}
#tmp <- find_modules() %>% as_tibble()

build_module_table <- function(){ #this builds a list of modules and the objects they contain
  module_path <- here::here("code", "www")
  
  module_names <- list.files(path = module_path, 
                             pattern = "module\\.Rmd")
  module_table <- tibble(
    module = character(), 
    object = character()
  )
  for (i in module_names) {
    single_table <- find_modules(i) %>% as_tibble()
    
    module_table <- 
      module_table %>% 
      bind_rows(single_table)
  }
  module_table %>% 
    #group_by(object) %>% 
    arrange(object)
  
  return(module_table)
}

module_linker <- function(module_filename){ #takes full file name in, returns html link out
  title <- filename_splitter(module_filename)
  
  name <- stringr::str_remove(module_filename, "\\.Rmd")
  url <- glue::glue("/{name}.html")
  
  markdown_final <- glue::glue("<a href='www/{url}'>{title}</a>")
  return(markdown_final)
}
# mod_name <- "3-a-data_visualization_with_ggplot-r-module.Rmd"
# module_linker(mod_name)

#take object and returns html string of modules it in
extract_module_table <- function(object_file){ #= "0-a-introduction_to_data_science-multiple-slides.Rmd"
  module_table <- build_module_table()
  
  module_files <- 
    module_table %>% #make using build_module_table() above
    dplyr::filter(object == object_file) %>% 
    dplyr::pull(module) 
  
  if(is_empty(module_files) == TRUE){return("None")}
  
  module_links <- 
    map_chr(module_files, module_linker) %>% 
    glue::glue_collapse(", ", last = " and ")
  return(module_links)
}
#test
#extract_module_table()

build_object_table <- function(){
  #get files
  object_path <- here::here("code", "www", "objects")
  
  object_names <- list.files(path = object_path, 
                             pattern = "Rmd")
  object_table <- 
    tibble(
      object_file = object_names, 
      title = map_chr(.x = object_names, .f = filename_splitter, var = "title"), 
      level = map_chr(.x = object_names, .f = filename_splitter, var = "level"), 
      module = map_chr(.x = object_names, .f = filename_splitter, var = "module"), 
      language = map_chr(.x = object_names, .f = filename_splitter, var = "language"), 
      type = map_chr(.x = object_names, .f = filename_splitter, var = "type"), 
      included_in = map_chr(.x = object_names, .f = extract_module_table)
    )
  
  #censor these objects that you don't want to navigate to directly
  censor <- c("1-rmarkdown_activity_file.Rmd", 
              "1-intro_exercise_file.Rmd", 
              "6-rmarkdown_activity_file.Rmd", 
              "6-rmarkdown_activity_file_answers.Rmd")
  
  object_table <- 
    object_table %>% 
    dplyr::filter(!object_file %in% censor, 
           !is.na(title))
  
  return(object_table)
}
#testing
# saveRDS(object_table, here::here("data", "object_table.Rds"))
# module_table <- build_module_table()
# object_table <- build_object_table()

#https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
numbers2words <- function(x){
  ## Function by John Fox found here: 
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  helper <- function(x){
    
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and", 
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }  
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
  #Disable scientific notation
  opts <- options(scipen=100) 
  on.exit(options(opts)) 
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine") 
  names(ones) <- 0:9 
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9 
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety") 
  names(tens) <- 2:9 
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")     
  if (length(x) > 1) return(trim(sapply(x, helper)))
  helper(x)
}
number_to_word <- function(x){
  wordy_num <- stringr::str_replace_all(numbers2words(x), " ", "-")
  return(wordy_num)
}

