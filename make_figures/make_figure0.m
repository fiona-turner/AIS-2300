% make figure 0 of the manuscript, showing the imbie timeseries alongside
% the prior timeseries and posterior timeseries
%
% 16/07/25, ATB alex.bradley@kcl.ac.uk, MIT 

%% Preliminaries
addpath('..')
fig = figure(5); clf; hold on
fig.Position(3:4) = [680, 420];
ax = gca;
ax.FontSize = 14;

ax.XLabel.String = 'year';
ax.YLabel.String = 'mass balance (Gt)';
box on

aa = linspecer(4);

pism_col = aa(3,:);
kori_col = aa(4,:);
prior_col = aa(1,:);
post_col = aa(2,:);
imbiecol = [0,0,0]; %colour for the IMBIE plot

ub_level = 83;
lb_level = 17; %set levels for upper and lower bounds

show_sims = 0; %flag to show the individual simulations
% if ~show_sims
%     prior_col = [0,47,167]/255;
%     post_col  = [167,0,47]/255;
% 
% end

s_alpha = 0.2; %alpha for the prior/post shading
%% Add individual simulation lines
if show_sims
lalpha = 0.1; %set the alpha for these lines

simulations = readtable("../SLE_SIMULATIONS_AIS_final_230725.csv");
simulations_meta = simulations(801:end,1:22); %meta data
simulations = table2array(simulations(801:end,23:end)); %start at 802 for only phase 2 sims
simulations_time = 1950:2300;

%rebase
[~,idx] = min(abs(simulations_time - 1979));
slr_1979 = repmat(simulations(:,idx), [1,351]); %matrix with repeated entries corresponding to 1979 slr
simulations = simulations - slr_1979;
simulations_mb = -simulations*362.5*1000;

simulations_kori_mb = simulations_mb(1:800,:);
simulations_pism_mb = simulations_mb(801:end,:);



for i = 1:length(simulations_kori_mb)
    if i == 1
         p = plot(simulations_time, simulations_kori_mb(i,:), 'color', kori_col, 'Marker','none','LineStyle','-');
    else
         p = plot(simulations_time, simulations_kori_mb(i,:), 'color', kori_col, 'HandleVisibility','off', 'Marker','none', 'LineStyle','-');
    end
p.Color = [p.Color, lalpha]; %set the alpha
end

for i = 1:length(simulations_pism_mb)
    if i == 1
         p = plot(simulations_time, simulations_pism_mb(i,:), 'color', pism_col,'Marker','none','LineStyle','-');
    else
         p = plot(simulations_time, simulations_pism_mb(i,:), 'color', pism_col, 'HandleVisibility','off', 'Marker','none','LineStyle','-');
    end
p.Color = [p.Color, lalpha]; %set the alpha
end

end



%% Add the prior 
% % Prior calculated as the mean over all simulations (agnostic)
% prior = readmatrix("../SLE_SIMULATIONS_AIS_final_230725.csv");
% prior = prior(801:end,23:end); %start at 802 for only phase 2 sims
% 
% %rebase to 1979
% prior_time = 1950:2300;
% [~,idx] = min(abs(prior_time - 1979));
% slr_1979 = repmat(prior(:,idx), [1,351]); %matrix with repeated entries corresponding to 1979 slr
% prior = prior - slr_1979;
% 
% %means and errs
% prior_errl = prctile(prior,5,1); 
% prior_erru = prctile(prior,95,1); 
% prior_median = prctile(prior,50,1);
% 
% %convert to Gt
% prior_mean_mb = -prior_median*(362.5*1000);
% prior_errl_mb = -prior_errl*(362.5*1000);
% prior_erru_mb = -prior_erru*(362.5*1000);
% 
% %plot
% xf = [prior_time, flip(prior_time)];
% yf = [prior_errl_mb, flip(prior_erru_mb)];
% fill(xf, yf, priorcol, 'LineStyle','none', 'FaceAlpha',0.3, 'HandleVisibility','off');
% plot(prior_time, prior_mean_mb,'color',  priorcol, 'LineWidth',2)




%% Add the prior predictions from the emulator
prior = readmatrix("../outputs/mcmc_output_data/priortrajectories.csv");
prior_time = 1955:5:2300;

% rebase
[~,idx] = min(abs(prior_time - 1979)); %this will be 1980 bc outputs in 5 years
slr_1979 = repmat(prior(:,idx), [1,70]); %matrix with repeated entries corresponding to 1979 slr
prior = prior - slr_1979;

prior_mean  = prctile(prior,50,1);

prior_erru  = prctile(prior,ub_level,1); %sets the shaded area
prior_errl  = prctile(prior,lb_level,1); %sets the shaded area

% 
prior_mean_mb = -prior_mean * (362.5*1000);
prior_errl_mb = -prior_errl*(362.5*1000);
prior_erru_mb = -prior_erru*(362.5*1000);
% 
% %plot
xf = [prior_time, flip(prior_time)];
yf = [prior_errl_mb, flip(prior_erru_mb)];
fill(xf, yf, prior_col, 'LineStyle','none', 'FaceAlpha',0.25, 'HandleVisibility','off');
plot(prior_time, prior_mean_mb,'color',  prior_col, 'LineWidth',2)

%% add the posterior
posterior = readmatrix("../outputs/mcmc_output_data/mcmc_output_posteriortrajectories.csv");
posterior_time = 1955:5:2300;

nmax = length(posterior);
%nmax = 2000;
posterior = posterior(1:nmax, :); %remove final rows
% rebase

[~,idx] = min(abs(posterior_time - 1979)); %this will be 1980 bc outputs in 5 years
slr_1979 = repmat(posterior(:,idx), [1,70]); %matrix with repeated entries corresponding to 1979 slr
posterior = posterior - slr_1979;

posterior_mean  = prctile(posterior,50,1);
%posterior_mean  = mean(posterior,1);
posterior_erru  = prctile(posterior,ub_level,1); %sets the shaded area
posterior_errl  = prctile(posterior,lb_level,1); %sets the shaded area

% 
posterior_mean_mb = -posterior_mean * (362.5*1000);
posterior_errl_mb = -posterior_errl*(362.5*1000);
posterior_erru_mb = -posterior_erru*(362.5*1000);
% 
% %plot
xf = [posterior_time, flip(posterior_time)];
yf = [posterior_errl_mb, flip(posterior_erru_mb)];
fill(xf, yf, post_col, 'LineStyle','none', 'FaceAlpha',s_alpha, 'HandleVisibility','off');
plot(posterior_time, posterior_mean_mb,'color',  post_col, 'LineWidth',2)
xlim([1979, 2022])

%% Load in and plot the IMBIE timeseries
fname =  "../calibration-data/IMBIE/imbie3_December24/imbie3_antarctica_partitioned_Gt.csv";


imbie_data = readmatrix(fname);
imbie_mass_balance             = imbie_data(:,4);
imbie_mass_balance_uncertainty = imbie_data(:,5);
imbie_time                     = imbie_data(:,1);


xf = [imbie_time; flip(imbie_time)];
yf = [imbie_mass_balance - imbie_mass_balance_uncertainty; flip(imbie_mass_balance + imbie_mass_balance_uncertainty)];
fill(xf, yf, imbiecol, 'LineStyle','none', 'FaceAlpha',s_alpha, 'HandleVisibility','off');

plot(imbie_time, imbie_mass_balance, 'LineWidth',  2, 'Color',imbiecol)

%% Tidy stuff
axl = gca;
yll =  [  -80000       15000]; %for macro
yll =  [  -10000       5000]; %for micro
axl.YLim = yll;
yticks_left = get(gca, 'YTick');


% add right y-axis of SLR

yyaxis right
axr = gca;
axr.YLim = yll;

% Set right y-axis ticks to match left y-axis ticks (so positions align)
set(axr, 'YTick', yticks_left)


yticks_right = 1/(362.5) * yticks_left;
axr.YTickLabel = compose('%.1f', yticks_right);

axr.YLabel.String = 'SLE (mm)';
if show_sims
    legend({'PISM', "KORI",  "Prior", "Posterior","IMBIE"}, 'location', 'SouthWest')
else
    legend({"Prior", "Posterior","IMBIE"}, 'location', 'SouthWest')
end