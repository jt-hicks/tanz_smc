orderly2::orderly_shared_resource('create_diag_figs.R')
orderly2::orderly_dependency("incoming_data", "latest()",
                             c('tanz_data_smc_16to22_dist_list.RDS'))
orderly2::orderly_parameters(proposal_matrix=1,
                             length=NULL,
                             workers=1,
                             chain=1,
                             seed=1L)

source('create_diag_figs.R')

data_list <- readRDS('tanz_data_smc_16to22_dist_list.RDS')
names_list <- names(data_list)

for(name in names_list){
  orderly2::orderly_dependency("run_pmcmc", quote(latest(parameter:name == environment:name)),
                               c("data/${name}.rds" = "result.rds"))
  print(name)
  result <- readRDS(paste0('data/',name,".rds"))
  create_diag_figs(result,
                   country = 'Tanzania',
                   district = name,
                   name = name)
}
