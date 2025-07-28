% make figure 8 of the manuscript, showing when the ensemble mean of a
% given emissions scenario deviates more than 1 sd from another.
%
% 24/7/25, ATB. alex.bradley@kcl.ac.uk. MIT license
%
%% Preliminaries
addpath('..')
ssp = ["119", "126", "245", "370", "585"];

levu = [66,83, 95];
levl = [33,17, 5]; %levels against which to assess
tt = 1955:5:2300; %time output of the mcmc

%% Load in the trajectories
t_emerge = nan(3,5,5);
count = 1;
for ii = 1:3

for i = 1:length(ssp)
    for j = 1:length(ssp)
        mcmc_output_scen1 = readmatrix(strcat("../outputs/mcmc_output_data/mcmc_output_posteriortrajectories_ssp",ssp(i),".csv"));
        posterior_central_scen1 = prctile(mcmc_output_scen1,50 ,1);
        posterior_erru_scen1    = prctile(mcmc_output_scen1,levu(ii),1); %sets the shaded area
        posterior_errl_scen1    = prctile(mcmc_output_scen1,levl(ii),1); %sets the shaded area

        mcmc_output_scen2 = readmatrix(strcat("../outputs/mcmc_output_data/mcmc_output_posteriortrajectories_ssp",ssp(j),".csv"));
        posterior_central_scen2 = prctile(mcmc_output_scen2, 50,1);
        posterior_erru_scen2    = prctile(mcmc_output_scen2,levu(ii),1); %sets the shaded area
        posterior_errl_scen2    = prctile(mcmc_output_scen2,levl(ii),1); %sets the shaded area

        %find when/if they differ
        idx = find((((posterior_central_scen1 < posterior_errl_scen2) | (posterior_central_scen1 > posterior_erru_scen2) | (posterior_central_scen2 > posterior_erru_scen1) | (posterior_central_scen2 < posterior_errl_scen1) ) & (tt > 2020)), 1, 'first');

        if ~isempty(idx)
            t_emerge(ii,i,j) = tt(idx);

        end
        count = count + 1
    end
end
end


%% Make plot
fig = figure(1); clf;
fig.Position(3:4) = [1400, 380];
t = tiledlayout(1,3);
cmap =  flipud(cmocean('ice', 100));
for i = 1:3
    nexttile
    p = heatmap( 1:5,1:5,flipud(squeeze(t_emerge(i,:,:)))); %flip so that we have ssps in the right order
    %set(p, 'AlphaData', ~isnan(t_emerge));
    xlabel("SSP");
    ylabel("SSP");
    p.XDisplayLabels = ssp;
    p.YDisplayLabels = flipud(ssp');
    p.ColorLimits = [2150, 2300];
    p.Colormap =cmap;
    p.MissingDataColor = 0.8*[1 1 1];   
    p.FontSize = 12;
end