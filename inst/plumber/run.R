# Run this with the following command from the terminal and check run.Rout for
# the port:
#
# R CMD BATCH run.R &
#
# Note that plumber depends on sodium, which depends on a system library that
# can be installed with:
#
# apt install libsodium-dev

if(!require(plumber)){
  install.packages("plumber")
  library(plumber)
}

root <- pr("api.R")
root %>% pr_run(host = "0.0.0.0", port = 6001)
