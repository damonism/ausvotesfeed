library(plumber)
root <- pr("api.R")
root %>% pr_run(host = "0.0.0.0")
