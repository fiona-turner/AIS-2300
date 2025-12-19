% Make figure 8 showing the main effects of SLR with GSAT at (a) 2100, (b) 2200,
% and (c) 2300
%
% 18/06/25, ATB (alex.bradley@kcl.ac.uk). MIT license.
%
%
clear
%% Load in the data
meff_GSAT_samples = readmatrix("../outputs/meff/GSAT_2300/samples.csv");
years = readmatrix("../outputs/meff/GSAT_2300/years.csv");
meff_GSAT_nominal_mean = readmatrix("../outputs/meff/GSAT_2300/nominal_meff_meanx.csv");
meff_GSAT_nominal_std = readmatrix("../outputs/meff/GSAT_2300/nominal_meff_sdx.csv");
meff_GSAT_nominalpost_mean = readmatrix("../outputs/meff/GSAT_2300/nominalposterior_meff_meanx.csv");
meff_GSAT_nominalpost_std = readmatrix("../outputs/meff/GSAT_2300/nominalposterior_meff_sdx.csv");

% select the times
tt = [2100, 2200, 2300];

%% Make the plot
fig = figure(1);clf; hold on
fig.Position(3:4) = [1250,300];
pcol = [0.5, 0., 0.5];

for i = 1:3
    ax(i) = subplot(1,3,i);
    hold(ax(i), 'on');
    box(ax(i), 'on');
    ax(i).XLabel.String = '\Delta GSAT (C)';
    ax(i).YLabel.String = 'SLE (m)';
    ax(i).FontSize = 14;
    grid(ax(i), 'on')

    %get the right row for nominal
    [~,idx] = min(abs(years - tt(i)));
    main = meff_GSAT_nominalpost_mean(:,idx);
    err  = meff_GSAT_nominalpost_std(:,idx);


    xf = [meff_GSAT_samples;flip(meff_GSAT_samples)];
    yf = [main-err; flip(main+err)];
    fill(xf, yf, pcol, 'FaceAlpha',0.3, 'LineStyle','none');
    plot(ax(i), meff_GSAT_samples, main, 'Color',pcol, 'LineWidth',1.5);
    ax(i).XLim = [0,12];

    %compute the linear slope between 0 and ~8 GSAT
    [~,idx8c] = min(abs(meff_GSAT_samples - 8));
    [~,idx0c] = min(abs(meff_GSAT_samples - 0));
    slope = (main(idx8c) - main(idx0c))/(meff_GSAT_samples(idx8c) - meff_GSAT_samples(idx0c));

    plot([meff_GSAT_samples(idx0c),meff_GSAT_samples(idx8c)], [main(idx0c), main(idx8c)],'linewidth', 1.5, 'Color',pcol, 'LineStyle','--' )

    fprintf('linear slope at time %.0f is %.4f \n', tt(i), slope)

end
   


