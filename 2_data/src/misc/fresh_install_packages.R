# fresh install package loader

freshinstall <- function(){

cat("------ installing packages ------\n")

packages<- c(
  "reshape2",
  "plm",
  "xts",
  "dynlm",
  "bbmle",
  "urca",
  "quantmod"
)

install.packages(packages)
rm(packages)
cat("------ package install done ------\n")
}