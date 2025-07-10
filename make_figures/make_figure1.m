% Make figure 1 of the manuscript showing trajectories of SLR to (a) 2100
% and (c) 2300. Also show distributions of SLR at the respective times at
% the right hand side
%
% 18/06/25, ATB (alex.bradley@kcl.ac.uk). MIT license.
%% Preliminaries

% set up the figure
fig = figure(1); clf;
fig.Position(3:4) = [1000, 600];

positions = [0.05,0.1,0.65,0.35;
    0.78, 0.1,0.2,0.35;
    0.05,0.55,0.65,0.35;
    0.78, 0.55,0.2,0.35];

for i = 1:4
    ax(i) = subplot("Position", positions(i,:));
    box(ax(i), 'on');
    hold(ax(i), 'on');
    ax(i).FontSize = 14;
end

ax(1).XLabel.String = 'year';
ax(3).XLabel.String = 'year';

ax(1).YLabel.String = 'SLE (m)';
ax(3).YLabel.String = 'SLE (m)';

ax(2).XLabel.String = 'SLE (m)';
ax(4).XLabel.String = 'SLE (m)';

ax(2).YLabel.String = 'density';
ax(4).YLabel.String = 'density';

pcol =  [0, 0, 0]; %color for the unscenariod data

scen_col = [136, 189, 181;
    34, 50, 81;
    231, 222, 92;
    221, 52, 39;
    121, 26, 36]/255;
%% Non-scenario data
%load in the data
mcmc_output = readmatrix("../outputs/mcmc_output_data/mcmc_output_posteriortrajectories.csv");

tt = 1955:5:2300;

posterior_central = mean(mcmc_output, 1);
posterior_erru     = prctile(mcmc_output,95,1); %sets the shaded area
posterior_errl     = prctile(mcmc_output,5,1); %sets the shaded area

% plot timeseries
xf = [tt, flip(tt)];
yf = [posterior_errl, flip(posterior_erru)];

fill(ax(1), xf, yf, pcol, 'FaceAlpha',0.1, 'LineStyle','none', 'HandleVisibility','off');
plot(ax(1), tt, posterior_central,'Color',  pcol, 'LineWidth',1.5);

fill(ax(3), xf, yf, pcol, 'FaceAlpha',0.1, 'LineStyle','none', 'HandleVisibility','off');
plot(ax(3), tt, posterior_central,'Color',  pcol, 'LineWidth',1.5);

% plot the distributions
[~,idx1] = min(abs(tt - 2100));
[~,idx2] = min(abs(tt - 2300));

slr2100 = mcmc_output(:,idx1);
slr2300 = mcmc_output(:,idx2);

[f2300, xi2300] = ksdensity(slr2300);
[f2100, xi2100] = ksdensity(slr2100);

plot(ax(2),xi2300, f2300,"Color",pcol, 'LineWidth',1.5 );
plot(ax(4),xi2100, f2100,"Color",pcol, 'LineWidth',1.5 );

%% Add the scenarios
count = 1;
for ssp = ["119", "126", "245", "370", "585"]
    mcmc_output = readmatrix(strcat("../outputs/mcmc_output_data/mcmc_output_posteriortrajectories_ssp",ssp,".csv"));
    posterior_central = mean(mcmc_output, 1);
    posterior_erru     = prctile(mcmc_output,95,1); %sets the shaded area
    posterior_errl     = prctile(mcmc_output,5,1); %sets the shaded area


    xf = [tt, flip(tt)];
    yf = [posterior_errl, flip(posterior_erru)];

    fill(ax(1), xf, yf, scen_col(count,:), 'FaceAlpha',0.1, 'LineStyle','none', 'HandleVisibility','off');
    plot(ax(1), tt, posterior_central,'Color',  scen_col(count,:), 'LineWidth',1.5);

    fill(ax(3), xf, yf, scen_col(count,:), 'FaceAlpha',0.1, 'LineStyle','none', 'HandleVisibility','off');
    plot(ax(3), tt, posterior_central,'Color',  scen_col(count,:), 'LineWidth',1.5);

    slr2100 = mcmc_output(:,idx1);
    slr2300 = mcmc_output(:,idx2);

    [f2300, xi2300] = ksdensity(slr2300);
    [f2100, xi2100] = ksdensity(slr2100);

    plot(ax(2),xi2300, f2300,"Color", scen_col(count,:), 'LineWidth',1.5 );
    plot(ax(4),xi2100, f2100,"Color", scen_col(count,:), 'LineWidth',1.5 );

    count = count + 1;


end


%% add the prior -- no calibration
% prior = readmatrix("../SLE_SIMULATIONS_AIS_final_230725.csv");
% prior = prior(801:end,23:end); %start at 802 for only phase 2 sims
% 
% %rebase to 2000
% [~,idx] = min(abs(prior_time - 2000));
% slr_1979 = repmat(prior(:,idx), [1,351]); %matrix with repeated entries corresponding to 1979 slr
% prior = prior - slr_1979;
% 
% prior_central = mean(prior, 1);
% prior_erru     = prctile(prior,95,1); %sets the shaded area
% prior_errl     = prctile(prior,5,1); %sets the shaded area
% prior_time = 1950:2300;
% 
% 
% xf = [prior_time, flip(prior_time)];
% yf = [prior_errl, flip(prior_erru)];
% 
% % plot the distributions
% [~,idx1] = min(abs(prior_time - 2100));
% [~,idx2] = min(abs(prior_time - 2300));
% 
% 
% slr2100 = prior(:,idx1);
% slr2300 = prior(:,idx2);
% 
% [f2300, xi2300] = ksdensity(slr2300);
% [f2100, xi2100] = ksdensity(slr2100);
% 
% fill(ax(1), xf, yf, [0,1,0], 'FaceAlpha',0.1, 'LineStyle','none', 'HandleVisibility','off');
% plot(ax(1), prior_time, prior_central,'Color', [0,1,0], 'LineWidth',1.5);
% 
% fill(ax(3), xf, yf, [0,1,0], 'FaceAlpha',0.1, 'LineStyle','none', 'HandleVisibility','off');
% plot(ax(3),  prior_time, prior_central,'Color',  [0,1,0], 'LineWidth',1.5);
% 
% plot(ax(2),xi2300, f2300,"Color", [0,1,0], 'LineWidth',1.5 );
% plot(ax(4),xi2100, f2100,"Color", [0,1,0], 'LineWidth',1.5 );

%% Tidying

%add zero lines
plot(ax(3),[1950,2100], [0,0], 'k--', 'LineWidth',1.5, 'HandleVisibility','off')
plot(ax(1),[1950,2300], [0,0], 'k--', 'LineWidth',1.5, 'HandleVisibility','off')

plot(ax(3),[2000,2000], [-0.4, 1], 'k--', 'LineWidth',1.5, 'HandleVisibility','off')
plot(ax(1),[2000,2000], [-4, 8], 'k--', 'LineWidth',1.5, 'HandleVisibility','off')

plot(ax(2), [0,0], [0,0.6], 'k--', 'LineWidth',1.5, 'HandleVisibility','off')
plot(ax(4), [0,0], [0,2], 'k--', 'LineWidth',1.5, 'HandleVisibility','off')

ax(3).XLim = [1950, 2100];
ax(1).XLim = [1950, 2300];
ax(3).XTick = 1950:50:2100;
ax(1).XTick = 1950:50:2300;

ax(3).YLim = [-0.5,1];
ax(1).YLim = [-1,4];
ax(3).YTick = -0.5:0.5:1;
legend(ax(3), {"All scenarios", "SSP119","SSP126","SSP245","SSP370","SSP585"}, 'location', "NorthWest", "FontSize", 14);
