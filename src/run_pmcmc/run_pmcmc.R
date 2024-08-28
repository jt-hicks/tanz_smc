orderly2::orderly_shared_resource('get_u5prev_fromanc.R')
orderly2::orderly_dependency("incoming_data", "latest()",
                             c('tanz_data_smc_16to22_dist_list.RDS'))
orderly2::orderly_parameters(name=NULL,
                             proposal_matrix=1,
                             length=NULL,
                             workers=1,
                             chain=1,
                             seed=1L)

source('get_u5prev_fromanc.R')
data_list <- readRDS('tanz_data_smc_16to22_dist_list.RDS')

##Calculate first year prevalence and convert to children under 5
first_annual_prev <- data_list[[name]]%>%
  slice_head(n=12)

annual_prev = sum(first_annual_prev$positive)/sum(first_annual_prev$tested)

targetprev <- get_u5prev_fromanc(avg_prev=annual_prev,comparison='ancall')
print(targetprev)

# hipercow::hipercow_init()
# hipercow::hipercow_configure('windows')
# hipercow::hipercow_provision()
# hipercow::hipercow_environment_create(packages=c('dplyr','ggplot2'))
# resources <- hipercow::hipercow_resources(cores=1)

result <- mamasante::run_pmcmc(data_raw=data_list[[name]],
                       init_EIR = 100,
                       n_particles=200,
                       proposal_matrix = matrix(proposal_matrix),
                       target_prev = targetprev,
                       target_prev_group = 'u5',
                       max_param=125,
                       prop_treated = 0.4,
                       n_steps = length,
                       n_threads = hipercow::hipercow_parallel_get_cores(),
                       n_chains = chain,
                       n_workers = workers,
                       state_check = 0,## Run equilibrium checks
                       seasonality_on = FALSE,  ## state_check = TRUE runs a deterministic seasonal model before running the stochastic model to get more realistic immunity levels
                       seasonality_check = FALSE,##If TRUE, saves values of seasonality equilibrium
                       seed = seed,
                       start_pf_time = 30*12,
                       particle_tune = FALSE,
                       comparison = 'ancall',
                       initial = 'informed')

saveRDS(result,'result.rds')
