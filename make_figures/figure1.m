% Make figure 1 of the manuscript, showing the trajectories of the
% simulations. Colour according to scenario (a) and model (b)
%
% 23/7/25, ATB. alex.bradley@kcl.ac.uk. MIT licence.
%
%% Preliminaries
fig = figure(1);clf; hold on;
fig.Position(3:4) = [1200, 360];
for i = 1:2
    ax(i) = subplot(1,2,i);
    xlabel('time');
    ylabel('SLE (m)');
    ax(i).FontSize = 14;
    hold on
    box on;
    grid on;
end

%% Load in simulations and preprocess
simulations = readtable("../SLE_SIMULATIONS_AIS_final_230725.csv");
simulations_meta = simulations(801:end,1:22); %meta data
simulations = table2array(simulations(801:end,23:end)); %start at 802 for only phase 2 sims

idx_585  = simulations_meta.scenario == "SSP585";
idx_126  = simulations_meta.scenario == "SSP126";
idx_kori = simulations_meta.model    == "Kori";
idx_pism = simulations_meta.model    == "PISM";


alpha_585 = 0.1;
col_126  = [34, 50, 81]/255;
col_585  = [121, 26, 36]/255;
col_pism = [255,152,51]/255;
col_kori = [0,153, 153]/255;

alphas = alpha_585 * idx_585 + alpha_126 * idx_126;
cols1   = zeros(length(idx_pism),3);
cols2   = zeros(length(idx_pism),3);
cols1(idx_kori,:) = repmat(col_kori,[sum(idx_kori),1]);
cols1(idx_pism,:) = repmat(col_pism,[sum(idx_pism),1]);
cols2(idx_126,:)  = repmat(col_126,[sum(idx_126),1]);
cols2(idx_585,:)  = repmat(col_585,[sum(idx_585),1]);

simulations_time = 1950:2300;

%rebase
[~,idx] = min(abs(simulations_time - 2000));
slr_2000 = repmat(simulations(:,idx), [1,351]); %matrix with repeated entries corresponding to 1979 slr
simulations = simulations - slr_2000;
simulations_mb = -simulations*362.5*1000;

%% Make plots
for i = 1:length(simulations)
    p = plot(ax(1), simulations_time, simulations(i,:), 'color', cols1(i,:), 'Marker','none','LineStyle','-');
    p.Color = [p.Color, 0.2]; %set the alpha



end

% do the scenario slightly differently so that 126 on top
for i = 1:length(simulations)
    if idx_585(i)
    p = plot(ax(2), simulations_time, simulations(i,:), 'color', cols2(i,:), 'Marker','none','LineStyle','-');
    p.Color = [p.Color, 0.2]; %set the alpha
    end
end

for i = 1:length(simulations)
    if idx_126(i)
    p = plot(ax(2), simulations_time, simulations(i,:), 'color', cols2(i,:), 'Marker','none','LineStyle','-');
    p.Color = [p.Color, 0.2]; %set the alpha
    end
end



