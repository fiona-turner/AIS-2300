% Make figure 2 showing the simulated vs emulated predictions
% of SLR at 2100, 2150, 2200, 2300
%
% 26/7/25, ATB. alex.bradley@kcl.ac.uk. MIT license.
%% Preliminaries
%
fig = figure(1); clf;
fig.Position(3:4) = [900, 660];
for i = 1:4
    ax(i) = subplot(2,2,i);
    hold(ax(i), 'on');
    box(ax(i), 'on');
    ax(i).FontSize = 14;
    ax(i).XLabel.String = 'simulated SLE (m)';
    ax(i).YLabel.String = 'emulated SLE (m)';
    grid(ax(i), 'on')

end

times = [2100, 2150, 2200, 2300];

coveredcol = [43,102,136]/255;
notcoveredcol = [227,129,122]/255;

xl = [-0.7, 2.2;
       -1, 3.2;
       -1.2, 4.4;
       -2, 7];

%% Load in the data
loocv_mean  = readmatrix("../outputs/emulator_output_data/loocv_emulator_mean.csv");
loocv_std   = readmatrix("../outputs/emulator_output_data/loocv_emulator_sd.csv");
simulations = readmatrix("../outputs/emulator_output_data/loocv_simulation_data.csv");
years       = 1955:5:2300;

% remove the years row
% loocv_mean = loocv_mean(2:end, :);
% loocv_std  = loocv_std(2:end, :);
% simulations = simulations(2:end, :);


sz = size(loocv_std);
n = sz(1); %number of loocv points (100)

% initalize storage for RMSE etc
emu_pred_all      = nan(4,n);
sim_all = nan(4,n);
coverages = nan(4,n);

%% Loop over plot times
for i = 1:4
    [~,idx] = min(abs(years - times(i)));

    plot(ax(i), xl(i,:), xl(i,:), 'k--', 'LineWidth',1.5)

    for j = 1:n

        % get the prediction, error, and simulated value
        emu_pred = loocv_mean(j, idx);
        emu_err  = loocv_std(j, idx);
        sim      = simulations(j,idx);


        %work out coverage and error of this sim
        iscovered = ((emu_pred - 2 * emu_err) < sim) &  ((emu_pred + 2 * emu_err) > sim);
        coverages(i,j) = iscovered;
        emu_pred_all(i,j) = emu_pred;
        sim_all(i,j) = sim;


        if iscovered
            plot(ax(i), sim, emu_pred, 'o', 'MarkerEdgeColor',coveredcol, 'MarkerFaceColor',coveredcol)
            plot(ax(i), [sim, sim], [emu_pred-2*emu_err, emu_pred+2*emu_err], 'Color',coveredcol, 'LineWidth',1.5)
            

        else
           plot(ax(i),sim,emu_pred, 'o', 'MarkerEdgeColor',notcoveredcol, 'MarkerFaceColor',notcoveredcol)
           plot(ax(i),  [sim, sim],[emu_pred-2*emu_err, emu_pred+2*emu_err], 'Color',notcoveredcol, 'LineWidth',1.5)



        end
        


    end 
    ax(i).XLim = xl(i,:);
    ax(i).YLim = xl(i,:);
    
end


% compute RMSE and coverage and Kendall tau
errs = emu_pred_all - sim_all;
coverage = sum(coverages,2)/n * 100;
RMSE = sum(errs.^2,2)/n;

for i = 1:4
    ktau(i)  = corr(emu_pred_all(i,:)',sim_all(i,:)','type','Kendall');
    
end