%lapse_rate has range [-12, -5]
%refreeze has range [0, 15]
%refreeze_frac has range [0.2, 0.8]
%PDD_ice has range [4, 12]
%PDD_snow has range [0, 6]
%heat_flux_Burgard has range [1*10**-4, 10*10**-4]
%heat_flux_ISMIP6_nonlocal has range [1*10**4, 4*10**4]
%heat_flux_ISMIP6_nonlocal_slope has range [1*10**6, 4*10**6]
%heat_flux_PICO has range [0.1*10**-5, 10*10**-5]
%heat_flux_Plume has range [1*10**-4, 10*10**-4]

mcmc_params_output     = readmatrix("mh_output.csv");
mcmc_params_output_upd = readmatrix("mh_output_upd.csv");
% mapping as follows: 
% "GSAT_2300", "simoc", "init_atmos", "lapse_rate","refreeze","refreeze_frac"                  
% "PDD_ice", "PDD_snow", "melt_param", "heat_flux_PICO", "heat_flux_Plume", "heat_flux_Burgard"              
% "heat_flux_ISMIP6_nonlocal", "heat_flux_ISMIP6_nonlocal_slope"
colnames = ["lapse_rate", "refreeze", "refreeze_frac", "PDD_ice", "PDD_snow", ...
    "heat_flux_PICO", "heat_flux_Plume", "heat_flux_Burgard",...
    "heat_flux_ISMIP6_nonlocal", "heat_flux_ISMIP6_nonlocal_slope"];

mcmc_params_output = mcmc_params_output(:,[4,5,6,7,8,10,11,12,13,14]);
mcmc_params_output_upd = mcmc_params_output_upd(:,[4,5,6,7,8,10,11,12,13,14]);

priors_bounds = [-12,-5; %lapse rate
    0,15; %refreeze rate
    0.2,0.8; %refreeze_frac
    4,12; %PDD_ice
    0,6;  %PDD_snow
    1e-6, 1e-4; % heat_flux_PICO
    1e-4, 1e-3; % heat_flux_Plume
    1e-4, 1e-3; % heat_flux_Burgard
    1e4, 4e4;  % heat_flux_ISMIP6_nonlocal
    1e6, 4e6  % heat_flux_ISMIP6_nonlocal_slope
    ]; 



figure(2); clf; 

for i = 1:10
    axs(i)  = subplot(2,5,i);
    box(axs(i), 'on');
    hold(axs(i), 'on');
end

% add the priors
for i = 1:10
    plot(axs(i), priors_bounds(i,:),[1,1]*1/(priors_bounds(i,2) - priors_bounds(i,1)), 'k--', 'linewidth', 1.5)
    plot(axs(i), priors_bounds(i,1)*[1,1],[0,1]*1/(priors_bounds(i,2) - priors_bounds(i,1)), 'k--', 'linewidth', 1.5)
    plot(axs(i), priors_bounds(i,2)*[1,1],[0,1]*1/(priors_bounds(i,2) - priors_bounds(i,1)), 'k--', 'linewidth', 1.5)
    
end

% posteriors
for i = 1:10
    data = mcmc_params_output(:,i);
    data_upd = mcmc_params_output_upd(:,i);
    h = histogram(axs(i),data, 'Normalization','pdf', 'FaceColor','b');
    h.FaceAlpha = 0.5; % 50% transparent
    h = histogram(axs(i),data_upd, 'Normalization','pdf', 'FaceColor','r');
    h.FaceAlpha = 0.5; % 50% transparent
    axs(i).XLabel.String = colnames(i);
    axs(i).XLabel.Interpreter = 'none';
end

%%%..
    