install.packages("svDialogs")

library(svDialogs)





set_up_directories <- function(){
  
  
  
  # Change this to the directory path that leads up to the alias
  
  base_directory <- "C:/Users/"
  
  
  
  # Print out all child folders of "base_directory". One of them should be their alias.
  
  get_alias <- function(){dlg_list(list.files(base_directory), multiple = FALSE, title = "\n\nSelect which one is your alias...\nType the number that corresponds and then hit enter...\n\n" ,
                                   
                                   gui = .GUI)$res}
  
  alias <- get_alias()
  
  
  
  # Create path for new folder to be created
  
  path <- paste(base_directory,alias,"/state_farm_project",sep="")
  
  
  
  dir.create(path = path, showWarnings = FALSE)
  
  dir.create(path = paste(path,"_demo",sep=""), showWarnings = FALSE)
  
  
  
  cat("\n\nYou should now have two new folders on your desktop named the following:\n\tstate_farm_project\n\tstate_farm_project_demo\n\nIf you do not see these folders on your desktop, run the code again and be sure to type the number that corresponds to your alias. If the folders still aren't being created, ask a State Farm person for help.")
  
  
  
}



set_up_directories()