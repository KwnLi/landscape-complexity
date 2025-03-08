library(tidyverse)

hu109 <- readRDS("scripts/hu109.rds")
hu218 <- readRDS("scripts/hu218.rds")
hu327 <- readRDS("scripts/hu327.rds")
hu437 <- readRDS("scripts/hu437.rds")

hu.all <- bind_rows(hu109, hu218, hu327, hu437)

saveRDS(hu.all, "scripts/hu_onefield.rds")
