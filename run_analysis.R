library(excel.link)
library(zoo)
library(plyr)
library(dplyr)
library(ggpubr)
library(binom)
library(gridExtra)
library(ggplot2)
library(viridis)
library(lubridate)
library(hipercow)
library(ggplot2)
library(coda)
library(patchwork)

windows_authenticate()

hipercow_init()
hipercow_configure('windows')

hipercow_provision()
hipercow_environment_create(packages=c('dplyr','ggplot2'))
resources <- hipercow_resources(cores=32)

id <- task_create_expr(2+2,
                       resources = hipercow_resources(cores=1))
id
task_status(id)
task_info(id)
task_log_show(id)
result <- task_result(id)
hipercow_purge(with_status = 'cancelled')
hipercow_purge(with_status = 'failure')

setwd('Y:/jth/tanz_smc')
id <- orderly2::orderly_run('data_quality')
id <- orderly2::orderly_run('incoming_data')
id <- task_create_expr(orderly2::orderly_run('incoming_data'),
                       resources=resources)
task_info(id)
task_status(id)
task_log_show(id)
names_list <- data.frame(name=names(final_data_list))
short_id <- task_create_bulk_expr(orderly2::orderly_run('run_pmcmc',parameters=list(name=name,length=1000)),data=names_list,
                       resources=resources)
#meteoropathologic_queenbee
task_status(short_id$ids)
task_log_show(short_id$ids[1])
task_result(short_id$ids[1])
names_list[19,]
short_id_19 <- task_create_expr(orderly2::orderly_run('run_pmcmc',parameters=list(name="Ruangwa District Council",length=1000,seed=1508L)),
                                  resources=hipercow_resources(cores=32))
task_status(short_id_19)
task_log_show(short_id_19)

##Make diagnostic plots
short_diag_id <- orderly2::orderly_run('run_diagnostics',parameters=list(length=1000))


test <- task_create_expr({
  print('this worked')
},resources = hipercow::hipercow_resources(cores=1))
task_status(test)
