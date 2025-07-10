% make figure x of the manuscript, showing the imbie timeseries alongside
% the prior timeseries and posterior timeseries

%% Preliminaries

fig = figure(2); clf; hold on

ax = gca;
ax.FontSize = 14;

ax.XLabel.String = 'year';
ax.YLabel.String = 'mass balance (Gt)';
box on


imbiecol = [1,0,0]; %colour for the IMBIE plot
priorcol = [0,0,0]; %colour for the prior
postcol  = [0,1,0]; %colour for the posterior
%% Load in and plot the IMBIE timeseries
fname =  "../calibration-data/IMBIE/imbie3_December24/imbie3_antarctica_partitioned_Gt.csv";


imbie_data = readmatrix(fname);
imbie_mass_balance             = imbie_data(:,4);
imbie_mass_balance_uncertainty = imbie_data(:,5);
imbie_time                     = imbie_data(:,1);


xf = [imbie_time; flip(imbie_time)];
yf = [imbie_mass_balance - imbie_mass_balance_uncertainty; flip(imbie_mass_balance + imbie_mass_balance_uncertainty)];
fill(xf, yf, imbiecol, 'LineStyle','none', 'FaceAlpha',0.3, 'HandleVisibility','off');

plot(imbie_time, imbie_mass_balance, 'LineWidth',  2, 'Color',imbiecol)

%% Add the prior 
% Prior calculated as the mean over all simulations (agnostic)
prior = readmatrix("../SLE_SIMULATIONS_AIS_final_230725.csv");
prior = prior(801:end,23:end); %start at 802 for only phase 2 sims

%rebase to 1979
prior_time = 1950:2300;
[~,idx] = min(abs(prior_time - 1979));
slr_1979 = repmat(prior(:,idx), [1,351]); %matrix with repeated entries corresponding to 1979 slr
prior = prior - slr_1979;

%means and errs
prior_errl = prctile(prior,33,1); 
prior_erru = prctile(prior,67,1); 
prior_median = prctile(prior,50,1);

%convert to mass balance
prior_mean_mb = -prior_median* (362.5*1000);
prior_errl_mb = -prior_errl*(362.5*1000);
prior_erru_mb = -prior_erru*(362.5*1000);

%plot
xf = [prior_time, flip(prior_time)];
yf = [prior_errl_mb, flip(prior_erru_mb)];
fill(xf, yf, priorcol, 'LineStyle','none', 'FaceAlpha',0.3, 'HandleVisibility','off');
plot(prior_time, prior_mean_mb,'color',  priorcol, 'LineWidth',2)


%% add the posterior
posterior = readmatrix("../outputs/mcmc_output_data/mcmc_output_posteriortrajectories_ssp119.csv");
posterior_time = 1955:5:2300;

% rebase
[~,idx] = min(abs(posterior_time - 1979)); %this will be 1980 bc outputs in 5 years
slr_1979 = repmat(posterior(:,idx), [1,70]); %matrix with repeated entries corresponding to 1979 slr
posterior = posterior - slr_1979;

posterior_mean  = prctile(posterior,50,1)
posterior_erru  = prctile(posterior,67,1); %sets the shaded area
posterior_errl  = prctile(posterior,33,1); %sets the shaded area

% 
posterior_mean_mb = -posterior_mean * (362.5*1000);
posterior_errl_mb = -posterior_errl*(362.5*1000);
posterior_erru_mb = -posterior_erru*(362.5*1000);
% 
% %plot
xf = [posterior_time, flip(posterior_time)];
yf = [posterior_errl_mb, flip(posterior_erru_mb)];
fill(xf, yf, postcol, 'LineStyle','none', 'FaceAlpha',0.3, 'HandleVisibility','off');
plot(posterior_time, posterior_mean_mb,'color',  postcol, 'LineWidth',2)

%% what if we weight the simulations bayesian weighting

%% Tidy stuff
ax.XLim = [1979, 2022];
ax.YLim = [  -5000       10000];