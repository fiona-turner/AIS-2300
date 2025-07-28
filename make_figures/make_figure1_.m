% Make figure 1 of the manuscript, showing the trajectories of sea level
% rise from (a) 1979-2022 (with observations) and (b) 1979-2300
%

%% Preliminaries
addpath('..')
fig = figure(1); clf; 
fig.Position(3:4) = [1400, 420];
for i = 1:2
    ax(i) = subplot(1,2,i);
    ax(i).FontSize = 14;
    hold(ax(i), 'on'); 
    ax(i).XLabel.String = 'year';
    ax(i).YLabel.String = 'SLE (m)';
    box on
end


aa = linspecer(4);

pism_col = aa(3,:);
kori_col = aa(4,:);
%% Add trajectories to both

lalpha = 0.2; %set the alpha for these lines

simulations = readtable("../SLE_SIMULATIONS_AIS_final_230725.csv");
simulations_meta = simulations(801:end,1:22); %meta data
simulations = table2array(simulations(801:end,23:end)); %start at 802 for only phase 2 sims
simulations_time = 1950:2300;

%rebase
[~,idx] = min(abs(simulations_time - 1979));
slr_1979 = repmat(simulations(:,idx), [1,351]); %matrix with repeated entries corresponding to 1979 slr
simulations = simulations - slr_1979;
simulations_mb = -simulations*362.5*1000;

simulations_kori    = simulations(1:800, :);
simulations_pism    = simulations(801:end, :);
simulations_kori_mb = simulations_mb(1:800,:);
simulations_pism_mb = simulations_mb(801:end,:);


for i = 1:length(simulations_kori_mb)
    if i == 1
        for j = 1:2
            p = plot(ax(j), simulations_time, simulations_kori(i,:), 'color', kori_col, 'Marker','none','LineStyle','-');
                p.Color = [p.Color, lalpha]; %set the alpha

        end
    else
        for j = 1:2
            p = plot(ax(j),simulations_time, simulations_kori(i,:), 'color', kori_col, 'HandleVisibility','off', 'Marker','none', 'LineStyle','-');
            p.Color = [p.Color, lalpha]; %set the alpha

        end
    end
end

for i = 1:length(simulations_pism_mb)
    if i == 1
        for j = 1:2
            p = plot(ax(j), simulations_time, simulations_pism(i,:), 'color', pism_col,'Marker','none','LineStyle','-');
                p.Color = [p.Color, lalpha]; %set the alpha

        end
    else
        for j = 1:2
            p = plot(ax(j),simulations_time, simulations_pism(i,:), 'color', pism_col, 'HandleVisibility','off', 'Marker','none','LineStyle','-');
                p.Color = [p.Color, lalpha]; %set the alpha

        end
    end
    p.Color = [p.Color, lalpha]; %set the alpha
end

legend(ax(1), {"Kori", "PISM"})

%% add the observations
fname =  "../calibration-data/IMBIE/imbie3_December24/imbie3_antarctica_partitioned_Gt.csv";

imbie_data = readmatrix(fname);
imbie_mass_balance             = imbie_data(:,4);
imbie_mass_balance_uncertainty = imbie_data(:,5);
imbie_time                     = imbie_data(:,1);

imbie_mass_balance = -imbie_mass_balance/(362.5*1000);
imbie_mass_balance_uncertainty = imbie_mass_balance_uncertainty/(362.5*1000);

%xf = [imbie_time; flip(imbie_time)];
%yf = [imbie_mass_balance - imbie_mass_balance_uncertainty; flip(imbie_mass_balance + imbie_mass_balance_uncertainty)];
%fill(xf, yf, imbiecol, 'LineStyle','none', 'FaceAlpha',s_alpha, 'HandleVisibility','off');

plot(ax(1), imbie_time, imbie_mass_balance, 'LineWidth',  2, 'Color','k')

%% tidy 
ax(1).XLim = [1950, 2025];
ax(1).YLim = [-0.1, 0.1];