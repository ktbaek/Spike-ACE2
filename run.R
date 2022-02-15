files_sources <- list.files("code", full.names = TRUE)
sapply(files_sources, source)