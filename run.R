files_sources <- list.files("code", full.names = TRUE)

source_echo <- 
  function(filepath) {
    
    print(filepath)
    source(filepath)
    
  }

sapply(files_sources, source_echo)