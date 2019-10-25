library(tidyverse)
library(fs)

folder <- "/Users/sefaozalp/Desktop/dash_comparison" #without forward slash


# read the csv file to be classified
data_raw <- dir_ls(folder, type = "file") %>% 
  read_csv(col_types = cols(id_str = col_character())) 


# read the csv files from classification directories

csv_files <- dir_info(folder,recurse = 1) %>% 
  filter(type=="file" ) 

csv_files

# rename classified files

classified_files <- classified_files %>% 
  mutate(parent_dir= dirname(path)) %>% 
  mutate(parent_dir_name= str_remove(parent_dir, paste0(folder, "/")  )) %>%
  mutate(filename= path_file(path)) %>% 
  select(path, parent_dir,parent_dir_name, filename) %>% 
  filter(!parent_dir_name %in% folder) %>% 
  mutate(new_file_path=paste0(parent_dir, "/", parent_dir_name, "-", filename)) 


pmap( classified_files, ~file_move(path = ..1, # this is the path variable
                    new_path =  ..5)) # this is the new path variable
