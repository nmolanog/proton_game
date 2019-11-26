#######################
###load data
#######################
rm(list=ls())
options(max.print=999999)
library(pacman)
p_load(here)
p_load(openxlsx)
p_load(tidyverse)

library(bueri)
oldir<-getwd()

library(proton)

proton()
#Problem 1: Find the login of John Insecure.
# Bit has scrapped 'employees' data (names and logins) from the www web page of Technical University of Warsaw. The data is in the data.frame `employees`. 
# Now, your task is to find John Insecure's login.
# When you finally find out what John's login is, use `proton(action = "login", login="XYZ")` command, where XYZ is Insecure's login.
data('employees')
employees[employees$name %in% "John" & employees$surname %in% "Insecure","login"]
jhon_login<-employees[employees$name %in% "John" & employees$surname %in% "Insecure","login"]
proton(action = "login", login=jhon_login)
# Congratulations! You have found out what John Insecure's login is!
# It is highly likely that he uses some typical password.
# Bit downloaded from the Internet a database with 1000 most commonly used passwords.
# You can find this database in the `top1000passwords` vector.
# 
# Problem 2: Find John Insecure's password.
# 
# Use `proton(action = "login", login="XYZ", password="ABC")` command in order to log into the Proton server with the given credentials.
# If the password is correct, you will get the following message:
#   `Success! User is logged in!`.
# Otherwise you will get:
#   `Password or login is incorrect!`.
top1000passwords%>%length
data(top1000passwords)

res_atemp<-proton(action = "login", login=jhon_login, password="ABC")
res_atemp%>%str_detect("Success")
top1000passwords%>%map_lgl(~str_detect(proton(action = "login", login=jhon_login, password=.),"Success"))->res_search
jhon_pswrd<-top1000passwords[res_search]
proton(action = "login", login=jhon_login, password=jhon_pswrd)

# Well done! This is the right password!
#   Bit used John Insecure's account in order to log into the Proton server.
# It turns out that John has access to server logs.
# Now, Bit wants to check from which workstation Pietraszko is frequently logging into the Proton server. Bit hopes that there will be some useful data.  
# 
# Logs are in the `logs` dataset. 
# Consecutive columns contain information such as: who, when and from which computer logged into Proton.
# 
# Problem 3: Check from which server Pietraszko logs into the Proton server most often.
# 
# Use `proton(action = "server", host="XYZ")` command in order to learn more  about what can be found on the XYZ server.
# The biggest chance to find something interesting is to find a server from which Pietraszko logs in the most often.
# 
#             proton.login.pass 
# "Success! User is logged in!" 
data(logs)