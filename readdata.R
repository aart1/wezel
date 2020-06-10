
library(readxl)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

# dit is een test file


excel_to_longDF <- function(path, tab){
  # get basic info about this experiment, based on the path
  filename <- str_split(path,"/") %>% unlist()
  filename <- filename[length(filename)]
  experiment_ID <- str_split(filename,"_") %>% unlist()
  experiment_ID <- experiment_ID[3]
  # get the concentration based on the sheet name
  conc <- str_remove(tab,"uM")
  # try reading this, if the tab does not exist, exit
  df <- read_excel(
    path=path,
    sheet=tab,
    skip=1)
  
  # select the relevant data
  tryCatch({
    df <- df %>% select(...1,`rel. to Baseline`:starts_with("Normalized")) %>%
      select(-starts_with("Normalized"))
  }, error = function(e){
    print(e)
  }
  )
  # hacky way to ensure that in an alternative scenario, namely a control, only the right columns are selected
  # somehow I do not manage to get this coded into trCatch
  tryCatch({
    df <- df %>% select(...1,`rel. to Baseline`:rel) %>%
      select(-rel)   
  }, error = function(e){
    print(e)
  }
  )
  
  # get the right column names
  colns <- df[1,] %>% as.vector() %>% as.character()
  colns[is.na(colns)] <- "Parameter"
  colnames(df) <- colns
  
  # drop the first row
  df <- df[2:nrow(df),]
  
  # pivot to a long df
  df <- df %>% pivot_longer(-Parameter, names_to="Well",values_to="Measurement", values_drop_na = TRUE)
  
  # add source information
  df <- df %>% mutate(Source_file = filename, Concentration=conc, Experiment=experiment_ID)
  
  return(df)
}

read_all_sheets <- function(path, conclist=concentrations){
  sheets <- excel_sheets(path)
  sheets <- sheets[sheets%in%conclist]
  df <- sheets %>%
    map_df(~ excel_to_longDF(path = path, tab = .x))
  return(df)
}



# relevant possible sheet names include controls (EtOH, DMSO),
# and concentration levels with and without units (uM)
concentrations <- c("0.01","0.03","0.1","0.3","1","3","10","30")
concentrations <- c("EtOH","DMSO",concentrations,paste0(concentrations,"uM"))

# collect all the files in the experiments folder
exp_folder <- "data/experiment_results/"
path <- exp_folder %>% dir(pattern =".xlsx")
path <- paste0(exp_folder,path)

# map the read function over all files
df <- path %>%
  map_df(~ read_all_sheets(path=.x))

# save the resulting dataframe
write_csv(df,"data/clean_data/parameter_measurements_all.csv")