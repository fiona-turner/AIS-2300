% Make figure 3 of the manuscript, showing (a) the evolution of the posterior
% distribution over the historical period and (b) a plot of the 2022 SLR as
% a function of n_max.
%
% 23/07/25, ATB. alex.bradley@kcl.ac.uk. MIT licence

%% Preliminaries
addpath('..')
usetmp = 0; %use temporary filepath
fig = figure(1); clf;

positions = [0.06,0.13,0.55,0.8;
             0.76, 0.13, 0.23, 0.8];

for i = 1:2
    ax(i) = subplot('Position', positions(i,:));
    hold(ax(i), 'on');
    box(ax(i), 'on');
    ax(i).FontSize = 14;
    grid(ax(i), 'on')
end
ax(1).XLabel.String = 'year';
ax(1).YLabel.String = 'SLE (m)';
fig.Position(3:4) = [1000, 350];
ax(2).XLabel.String = 'n_{MCMC}';
ax(2).YLabel.String = 'SLR at 2300 (m)';

%% Load in the data
if ~usetmp
    posterior = readmatrix("../outputs/mcmc_output_data/mcmc_output_posteriortrajectories.csv");
else
    posterior = readmatrix("../outputs/mcmc_output_data/tmp/mcmc_output_posteriortrajectories.csv");

end

posterior_time = 1955:5:2300;

%% Make panel (a)
nmax = 2000:2000:length(posterior);
cmap = cmocean('thermal', length(nmax));

%cmap = parula(length(nmax));

slr_2020 = nan(1,length(nmax));
slr_2300 = nan(1,length(nmax));
 
for in = 1:length(nmax)
posterior_red = posterior(1:nmax(in), :); %remove final rows
% rebase

[~,idx] = min(abs(posterior_time - 2000)); %this will be 1980 bc outputs in 5 years
slr_2000 = repmat(posterior_red(:,idx), [1,70]); %matrix with repeated entries corresponding to 1979 slr
posterior_red = posterior_red - slr_2000;

posterior_mean  = prctile(posterior_red,50,1);
posterior_mean_mb = -posterior_mean * (362.5*1000);
[~,idxt] = min(abs(posterior_time - 2020));
slr_2020(in) = posterior_mean(idxt);
slr_2300(in) = posterior_mean(end);
% 
% %plot

plot(ax(1), posterior_time, posterior_mean,'color',  cmap(in,:), 'LineWidth',2) 
end

c = colorbar(ax(1));
c.Ticks = nmax(5:5:end)./max(nmax);
c.Position(1) = positions(1,3) + 0.065;
c.Position(3) = 0.01;
c.Colormap = cmap;
%c.Limits = [min(c.Ticks), max(c.Ticks)];
c.TickLabels = compose('%.0f', nmax(5:5:end));
c.Label.String = 'n_{MCMC}';
shg

%% Make panel (b)
plot(ax(2), nmax, slr_2300, 'k', 'LineWidth',2);


%% Tidy stuff
ax(1).YLim = [0, 4];
%ax(1).YLim = 1e-3*[-10, 2000];
%ax(1).XLim = [1979, 2022];
ax(2).YLim = ax(1).YLim;
ax(2).XLim = [min(nmax), max(nmax)];