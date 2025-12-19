% Make figure 4 of the manuscript, showing the prior and posterior
% alongside observations over the historical period. Show on (a) large y
% axis and (b) zoomed y axis.
%
% 23/7/25, ATB. alex.bradley@kcl.ac.uk. MIT licence.
%
%% Preliminaries
addpath('..')
fig = figure(1); clf;
fig.Position(3:4) = [1300, 400];
for i = 1:2
    ax(i) = subplot(1,2,i);
    ax(i).FontSize = 14;

    ax(i).XLabel.String = 'year';
    ax(i).YLabel.String = 'SLE (mm)';
    box on
    hold on
end

aa = linspecer(4);

prior_col = aa(1,:);
post_col = aa(2,:);
imbiecol = [0,0,0]; %colour for the IMBIE plot

ub_level = 83;
lb_level = 17; %set levels for upper and lower bounds
s_alpha = 0.2; %alpha for the prior/post shading

%% Compute prior calculated as mean over all simulations
% !!! Note that this is not the true prior because prior sample does not
% reflect the prior distribution. This is because some parameters are fixed
% in Kori/PISM simulations and so these parameters are over-represented.
%
%
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




%% Compute prior predictions from the emulator
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

%% Compute posterior predictions from the emulator
posterior = readmatrix("../outputs/mcmc_output_data/mcmc_output_posteriortrajectories.csv");
posterior_time = 1955:5:2300;

nmax = length(posterior);
%nmax = 2000;
nmin = 1;
posterior = posterior(nmin:nmax, :); %remove final rows
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


%% Observations
fname =  "../calibration-data/IMBIE/imbie3_December24/imbie3_antarctica_partitioned_Gt.csv";
fname =  "../calibration-data/IMBIE/imbie3_July25/imbie3_antarctica_Gt_partitioned.csv";

imbie_data = readmatrix(fname);
imbie_mass_balance             = imbie_data(:,4);
imbie_mass_balance_uncertainty = imbie_data(:,5);
imbie_time                     = imbie_data(:,1);

imbie_slr = -imbie_mass_balance /( 362.5);
imbie_slr_uncertainty = -imbie_mass_balance_uncertainty / (362.5);

%% Make plots

% Prior
xf = [prior_time, flip(prior_time)];
yf = [prior_errl, flip(prior_erru)]*1e3;
for i = 1:2
fill(ax(i), xf, yf, prior_col, 'LineStyle','none', 'FaceAlpha',0.25, 'HandleVisibility','off');
plot(ax(i), prior_time, prior_mean*1e3,'color',  prior_col, 'LineWidth',2)
end

% Posterior
xf = [posterior_time, flip(posterior_time)];
yf = [posterior_errl, flip(posterior_erru)]*1e3;
for i = 1:2
fill(ax(i), xf, yf, post_col, 'LineStyle','none', 'FaceAlpha',s_alpha, 'HandleVisibility','off');
plot(ax(i), posterior_time, posterior_mean*1e3,'color',  post_col, 'LineWidth',2)
end

% Observations
xf = [imbie_time; flip(imbie_time)];
yf = [imbie_slr - imbie_slr_uncertainty; flip(imbie_slr + imbie_slr_uncertainty)];
for i = 1:2
fill(ax(i),xf, yf, imbiecol, 'LineStyle','none', 'FaceAlpha',s_alpha, 'HandleVisibility','off');
plot(ax(i), imbie_time, imbie_slr, 'LineWidth',  2, 'Color',imbiecol)
end


%% Tidy stuff
axl = gca;
ax(1).YLim = [-20, 300]; %for macro
ax(2).YLim = [-5, 20];%for micro
for i = 1:2
    ax(i).XLim = [1979, 2022];
end


legend(ax(1), {"Prior", "Posterior","IMBIE"}, 'location', 'NorthWest', 'FontSize', 14)