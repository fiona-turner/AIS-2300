%Make figure 5 of the ms showing the prior and posterior distributions of
%model parameters
%
% 18/06/25, ATB (alex.bradley@kcl.ac.uk). MIT license.
%% Preliminaries

col_headers = ["\Delta GSAT (C)","simoc","init atmos","lapse rate (C/km)","refreeze rate",...
               "refreeze fraction", "PDD ice (w.e. mm / PDD)","PDD snow (w.e. mm / PDD)","melt_param", "sliding exponent", "overturning PICO","PICO heat flux (m/s)"...
           	   "Plume heat flux (-)","ISMIP6 nonlocal heat flux (m/yr)","ISMIP6 nonlocal slope heat flux (m/yr)","ISMIP6 local slope heat flux (m/yr)" ];

%use indices to choose which ones we plot (basically the continuous ones)
isplot = [1,4,5,6,7,8,12,13,14,15,16]; 

% set up plot
fig = figure(1);clf;

for i = 1:11
    ax(i) = subplot(3,4,i);
    hold(ax(i),'on');
    box(ax(i), 'on');
    ax(i).FontSize = 14;
    ax(i).XLabel.String = col_headers(isplot(i));
    ax(i).YLabel.String = 'density';
   % ax(i).XLabel.Interpreter = 'none';
end
fig.Position(3:4) = [1300, 740];
falpha = 0.3; %alpha for the histogram



% %% add the prior histograms from simulations
% prior = readmatrix("../SLE_SIMULATIONS_AIS_final_230725.csv");
% prior = prior(801:end, :); %phase 2 only
% 
% 
% lapse_rate_prior = prior(:,13);
% histogram(ax(2), lapse_rate_prior,20, 'Normalization','pdf');
% 
% histogram(ax(3), prior(:,14),20, 'Normalization','pdf');
% histogram(ax(4), prior(:,15),20, 'Normalization','pdf');
% histogram(ax(5), prior(:,16),20, 'Normalization','pdf');
% histogram(ax(6), prior(:,17),20, 'Normalization','pdf');
% histogram(ax(7), prior(:,18),20, 'Normalization','pdf');
% histogram(ax(8), prior(:,19),20, 'Normalization','pdf');
% histogram(ax(9), prior(:,20),20, 'Normalization','pdf');
% histogram(ax(10), prior(:,21),20, 'Normalization','pdf');
% histogram(ax(11), prior(:,22),20, 'Normalization','pdf');


%% Add the sampled prior
prior_params = readmatrix("../outputs/mcmc_output_data/tmp/priorparameters.csv");
prior_params = prior_params(:,isplot); %remove the unwanted columns

pcols2 = [0, 0.5, 0.5];
for i = 1:11
   h(i) =  histogram(ax(i), prior_params(:,i),20, 'Normalization','pdf', 'FaceColor', pcols2);
   h(i).FaceAlpha = falpha;
end

%% Add the sampled posterior

mcmc_output = readmatrix("../outputs/mcmc_output_data/tmp/mcmc_output_posteriorsamples.csv");
mcmc_output = mcmc_output(:,isplot); %remove the unwanted columns

pcol = [0.5, 0, 0.5];
for i = 1:11
    gg(i) = histogram(ax(i), mcmc_output(:,i),20, 'Normalization','pdf', 'FaceColor', pcol);
    gg(i).FaceAlpha = 0.5;
   % gg(i).BinEdges = h(i).BinEdges;   

end
%%
%ax(3).XLim = [-0.375, 15.375];
%ax(2).XLim = [ -12.2900,   -4.710000];
% ax(5).XLim = [3.71, 12.29];
% ax(6).XLim = [-0.3, 6.3];
% ax(7).XLim = 1e-3*[-0.0034, 0.105];
% ax(8).XLim = [0, 0.0011];
% ax(9).XLim = [0.00005, 0.00105];
% ax(10).XLim = [8500, 41500];
% ax(11).XLim = 1e6*[0.85, 4.13];

%% Add the analytic prior 
range_GSAT       = [-0.297,12.042];
range_lapse_rate = [-12, -5];
range_refreeze   = [0, 15];
range_refreeze_frac = [0.2, 0.8];
range_PDD_ice = [4, 12];
range_PDD_snow  = [0, 6];
range_heat_flux_ISMIP6_local    =[3*10^3, 3.2*10^4];
range_heat_flux_ISMIP6_nonlocal = [1*1e4, 4*1e4];
range_heat_flux_ISMIP6_nonlocal_slope  = [1*1e6, 4*1e6];
range_heat_flux_PICO                   = [0.1*1e-5, 10*1e-5];
range_heat_flux_Plume                  = [1*1e-4, 10*1e-4];

ranges = [range_GSAT;range_lapse_rate;range_refreeze;range_refreeze_frac;range_PDD_ice;
    range_PDD_snow;range_heat_flux_PICO; range_heat_flux_Plume;range_heat_flux_ISMIP6_nonlocal;range_heat_flux_ISMIP6_nonlocal_slope;range_heat_flux_ISMIP6_local]; %must be in same order as above

heights_of_priors = 1./(ranges(:,2) - ranges(:,1));



for i = 1:11
    plot(ax(i), ranges(i,:), heights_of_priors(i)*[1,1], 'k', 'LineWidth',1.5);
    plot(ax(i), ranges(i,1)*[1,1], [0,heights_of_priors(i)], 'k--', 'linewidth', 1.5 );
    plot(ax(i), ranges(i,2)*[1,1], [0,heights_of_priors(i)], 'k--', 'linewidth', 1.5 );
end

%% Add the distributions of SSP in panel a


scen_col = [136, 189, 181;
    34, 50, 81;
    231, 222, 92;
    221, 52, 39;
    121, 26, 36]/255;
count = 1;
for ssp = ["119", "126", "245", "370", "585"]
    fairpath = strcat("../FAIR_data/ssp", ssp, ".temperature.fair.temperature_climate.nc");
    fair_years = ncread(fairpath, 'year');
    fair_temps = ncread(fairpath, strcat("/ssp",ssp,"/surface_temperature"));
    fair_temps_2300 = fair_temps(:,end);    
    [f, xi] = ksdensity(fair_temps_2300);
    plot(ax(1), xi, f/3, 'Color',scen_col(count, :), 'LineWidth',1.5);
    count = count + 1;
end
