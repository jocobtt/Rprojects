#run python in rstudio:
library(reticulate)
#find version of python etc on system
py_config()

#start python instance in r
repl_python()
#call out dataset in r and describe it
r.mydata.describe
exit
#if want to call out python object put py in front ie
summary(py$t)

#check if module is available
py_module_available('pandas')

modell <- lm('speed ~ dist',data=mydata)
