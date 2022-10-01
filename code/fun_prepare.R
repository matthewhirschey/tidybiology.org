#these functions are used to prepare files for serving, but not used in the apps
#LIBRARIES-----
library(tidyverse)
library(here)
source(here::here("code", "fun_helper.R")) #needed for filename splitter

#BUILD TABLE-----
find_modules <- function(module_filename){ #this takes all modules and gets the child modules out
  lines <- readLines(here::here("code", module_filename))
  
  files <- str_subset(lines, pattern = "\\.Rmd") %>% 
    str_remove(pattern = "([[:symbol:]]+)[[:punct:]]r[[:space:]]child[[:symbol:]][[:punct:]][[:digit:]]+[[:punct:]]objects[[:punct:]]") %>%  #remove leader
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
  module_path <- here::here("code")
  
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

module_linker <- function(module_filename){ #takes full file name in, returns html link out (must be html for table)
  module_title <- filename_splitter(module_filename)
  module_num <- stringr::str_extract(module_filename, pattern = "[[:digit:]]+")
  url <- glue::glue("http://module.tidybiology.org/{module_num}")
  link_final <- glue::glue("<a href='{url}'>{module_title}</a>")
  return(link_final)
}
# module_filename <- "3-a-data_visualization_with_ggplot-r-module.Rmd"
# module_linker(module_filename)

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
# module_table <- build_module_table()
#extract_module_table()

build_object_table <- function(){
  #get dirs
  object_dirs <- list.dirs(path = here::here("code"),
                           recursive = FALSE) %>% 
    stringr::str_subset("objects")
  
  #get files
  object_names <- character()
  for (i in object_dirs) {
    object_files <- list.files(path = i, 
                               pattern = "Rmd")
    object_names <- c(object_names, object_files)
  }
  
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
  # censor <- c("1-rmarkdown_activity_file.Rmd", 
  #             "1-intro_exercise_file.Rmd", 
  #             "6-rmarkdown_activity_file.Rmd", 
  #             "6-rmarkdown_activity_file_answers.Rmd")
  
  object_table <- 
    object_table %>% 
    #censor these words so it doesn't get objects that contain them
    dplyr::filter(str_detect(object_file, pattern = "file", negate = TRUE), #!object_file %in% censor, 
                  !is.na(title))
  saveRDS(object_table, here::here("object_table.Rds"))
  return(object_table)
}
#testing
# build_object_table()
# object_table <- build_object_table()

#https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r

#BUILD MAP-----
make_onclick <- function(module_filename){ #makes url from module_filename, = "1-a-Introduction_to_Data_Science-multiple-module.Rmd"
  
  module_num <- filename_splitter(file_name = module_filename, 
                                  var = "module")
  module_section <- filename_splitter(file_name = module_filename, 
                                      var = "section")
  onclick <- glue::glue('window.open("http://module.tidybiology.org/{module_num}#section-{module_section}")')
  return(onclick)
}

#update object table with map stuff
build_map <- function(){
  if(!exists("object_table")){object_table <- readRDS(here::here("object_table.Rds"))}
  object_table <-
    object_table %>%
    mutate(station = title, 
           x = row_number()/5,
           y = 1,
           onclick = map_chr(.x = object_file, .f = make_onclick),
           track = map_chr(.x = object_file, .f = filename_splitter, var = "module"))
  
  #update object table
  saveRDS(object_table, here::here("object_table.Rds"))
  return(object_table)
}

# build_map()


#PUBLISH-----
#prepare files before publishing
publish_tidybiology_index <- function(){
  build_object_table()
  rsconnect::deployApp(account = "computationalthinking", 
                       appFiles = c("index.Rmd", 
                                    "object_table.Rds", 
                                    "www/"), 
                       forceUpdate = TRUE)
  build_map()
  rsconnect::deployApp(account = "computationalthinking", 
                       appDir = here::here("code"), 
                       appPrimaryDoc = "map.Rmd",
                       appName = glue::glue("tidybiology-map"),
                       appFiles = c("map.Rmd", "object_table.Rds"), 
                       forceUpdate = TRUE)
  
  rsconnect::deployApp(account = "computationalthinking", 
                       appDir = here::here("code"), 
                       appPrimaryDoc = "quiz.Rmd",
                       appName = glue::glue("tidybiology-quiz"),
                       appFiles = c("quiz.Rmd"), 
                       forceUpdate = TRUE)
  
  rsconnect::deployApp(account = "computationalthinking", 
                       appDir = here::here("code"), 
                       appPrimaryDoc = "example.Rmd",
                       appName = glue::glue("tidybiology-example"),
                       appFiles = c("example.Rmd", "final_project.Rmd", "final_project_template.Rmd"), 
                       forceUpdate = TRUE)
  
  # rsconnect::deployApp(account = "computationalthinking", 
  #                      appDir = here::here("code"), 
  #                      appPrimaryDoc = "feedback.Rmd",
  #                      appName = glue::glue("tidybiology-feedback"),
  #                      appFiles = c("feedback.Rmd", "fun_helper.R", ".Renviron"), 
  #                      forceUpdate = TRUE)
}
# publish_tidybiology_index()

publish_tidybiology<- function(module_name){
  if(is.numeric(module_name)){
    module_tibble <- tibble(
      mod_name = list.files(path = here::here("code"), 
                                 pattern = "module\\.Rmd")) %>% 
      dplyr::mutate(mod_num = stringr::str_extract(mod_name, pattern = "[[:digit:]]+"))
    file_name <- 
      module_tibble %>% 
      dplyr::filter(mod_num == as.character(module_name)) %>% 
      dplyr::pull(mod_name)
    if(length(file_name) == 0){return("no file found")}
  } else {
    file_name <- module_name}
  module_num <- stringr::str_extract(file_name, pattern = "[[:digit:]]+")
  object_dir <- glue::glue("{module_num}-objects")
  rsconnect::deployApp(account = "computationalthinking", 
                       appDir = here::here("code"), 
                       appPrimaryDoc = file_name,
                       appName = glue::glue("tidybiology-{module_num}"),
                       appFiles = c("setup.Rmd", "fun_helper.R", file_name, object_dir), 
                       forceUpdate = TRUE)
  
}
# publish_tidybiology(module_name = "1-a-introduction_to_data_science-multiple-module.Rmd")
# publish_tidybiology(module_name = "2-a-introduction_to_dplyr-r-module.Rmd")
# publish_tidybiology(7)
# 6:7 %>% purrr::walk(~publish_tidybiology(.x))

#republish all
publish_tidybiology_all <- function(index = TRUE){
  if(index == TRUE){ publish_tidybiology_index() }
  module_names <- list.files(path = here::here("code"), 
                             pattern = "module\\.Rmd")
  purrr::walk(.x = module_names, .f = publish_tidybiology)
}
# publish_tidybiology_all()
