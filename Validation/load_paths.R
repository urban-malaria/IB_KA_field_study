

## -----------------------------------------
### Paths
## -----------------------------------------
user <- Sys.getenv("USER") 


if ("ifeomaozodiegwu" %in% user) {
  user_path <- file.path("/Users", user, "Library", "CloudStorage")
  Drive <- file.path(user_path, "OneDrive-NorthwesternUniversity")
  
} else {
  
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("OneDrive"))))
}

NuDir <- file.path(Drive, "urban_malaria")
DataDir <-file.path(NuDir, "data", "nigeria")
ProjDir <- file.path(NuDir, "projects")
SampDir <- file.path(ProjDir, "sampling")
DHS_CDir <- file.path(DataDir, 'nigeria_dhs', 'data_analysis', 'data', 'DHS')
Raster_Dir <- file.path(NuDir,'data', 'Raster_files')
mod_file = file.path(ProjDir, "mathematical_model")


