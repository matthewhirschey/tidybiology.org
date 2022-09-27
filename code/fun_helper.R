#HELPER-----
filename_splitter <- function(file_name, 
                              var = "title"){
  file_name <- stringr::str_remove(file_name, "\\.Rmd")
  filename_info <- stringr::str_split(file_name,
                                      pattern = "-")
  output <- switch(var, 
                   module = filename_info[[1]][1], 
                   level = filename_info[[1]][2],
                   section = filename_info[[1]][3] %>% 
                     stringr::str_replace_all(pattern = "_", replacement = "-") %>% 
                     str_to_lower(),
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
# filename_splitter(file_name = "1-a-Introduction_to_Data_Science-multiple-module.Rmd", var = "section")

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

next_module <- function(module_name = "..."){
  if(module_name == "..."){
    
    link <- glue::glue("## Continue... 
                         \nGreat job, go back [home](http://www.tidybiology.org)")
  } else {
    module <- stringr::str_remove(module_name, "\\.Rmd")
    module_difficulty <- filename_splitter(file_name = module,
                                           var = "level")
    module_title <- filename_splitter(file_name = module,
                                      var = "title")
    module_num <- stringr::str_extract(module_name, pattern = "[[:digit:]]+")
    
    link <- glue::glue("## Continue...
    <br>The next module in this {module_difficulty} pathway is<br>
    <br><a href = 'http://module.tidybiology.org/{module_num}' class='btn btn-primary btn-default' role='button'>{module_title}</a>")
  }
  return(link)
}
#testing
#next_module()
#next_module(module = "1-a-Introduction_to_Data_Science-multiple-module.Rmd")

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
  wordy_num <- stringr::str_replace_all(numbers2words(x), " ", "_")
  return(wordy_num)
}

make_button <- function(file_name = "1-intro_exercise_file_subcell.Rmd", 
                        label = NULL,
                        button_style = "danger"){
  #get module from file name
  module <- stringr::str_extract(file_name, pattern = "[[:digit:]]+")
  #get label from file name, can override
  if(is.null(label)){label <- stringr::str_extract(file_name, pattern = "[[:alpha:]]+(?=\\.Rmd)")}
  
  #the file you want to attach is in a different repo, so this grabs it
  temp_dir <- tempdir()
  file_path <- glue::glue('{temp_dir}/{file_name}')
  
  #check for internet connection
  if(curl::has_internet() == TRUE){ 
    #download file to temp file
    download.file(url = glue::glue('https://raw.githubusercontent.com/matthewhirschey/tidybiology.org/main/code/{module}-objects/{file_name}'), 
                  destfile = file_path)
  } else {
    file.copy(from = glue::glue('code/{module}-objects/{file_name}'), 
              to = file_path)
  }
  
  #button sytles
  #https://fmmattioni.github.io/downloadthis/articles/button_types.html
  
  #download button
  button <- 
    downloadthis::download_file(
      path = file_path,
      button_label = glue::glue("Download {label}"),
      button_type = button_style,
      has_icon = TRUE,
      icon = "fa fa-save",
      self_contained = TRUE
    )
  return(button)
}

#make_button()

get_names <- function(class_dir){
  url <- paste0("https://raw.githubusercontent.com/matthewhirschey/tidybiology_admin/main/alumni.csv?token=", Sys.getenv("GITHUB_TOKEN"))
  full_names <- 
    readr::read_csv(url) %>% 
    dplyr::filter(class == class_dir) %>% 
    dplyr::arrange(full_name) %>% 
    dplyr::pull(full_name)
  return(full_names)
}

#get_names(class_dir = "2022_duke-nus")

get_sessions <- function(){
  url <- paste0("https://raw.githubusercontent.com/matthewhirschey/tidybiology_admin/main/alumni.csv?token=", Sys.getenv("GITHUB_TOKEN"))
  session <- 
    readr::read_csv(url) %>% 
    dplyr::distinct(class) %>% 
    dplyr::arrange(desc(class)) %>% 
    dplyr::pull(class)
  return(session)
}

#get_sessions()
