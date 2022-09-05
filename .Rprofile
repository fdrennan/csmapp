source("renv/activate.R")
options(cache = TRUE)
options(development = TRUE)
options(file_regex = "csm[0-9]{6}[a|b|c]/datamisc$")
options(datamisc_cache_path = "./datamisc")
options(bmrn_base_dir = "/sassys/cdm/cdmdev/bmn111/ach")
options(base_directory = getOption("bmrn_base_dir"))
options(cache_path = "./cache/data.rda")
options(analysis_filter = {
  if (getOption("development")) {
    c("aei", "rgv")
  } else {
    NULL
  }
})
