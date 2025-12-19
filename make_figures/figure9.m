% Make figure 9 of the mansucript, showing the likely ranges of SLR at 2300
% from different modelling studies.
%
%% Preliminaries
fig = figure(1); clf; hold on
fig.Position(3:4) = [1200, 370];
for i = 1:3
    ax(i) = subplot(1,3,i);
    hold on
    box on
    ax(i).FontSize = 14;
    grid on
end

scen_col = [34, 50, 81; %1-2.6
    231, 222, 92; %2-4.5
    121, 26, 36]/255; %5-8.5

%% Data
sim_names = ["This study", "IPCC AR6", "Seroussi et al., 2024", "Turner et al., 2023", "Coulon et al., 2024", "Coulon et al., in review"];
ub = [2.40, 2.71, 3.76;
    0.78,   nan,  3.13;
    0.3635, nan,  3.41;
    1.4,    2.0,  nan;
    1.20,   nan,  3.72;
    1.16,   nan,  4.32];


lb = [-0.77, -0.48, 0.36;
    -0.14, nan, -0.28;
    -0.18,  nan,  0.38;
    0,     0.1,  nan;
    0.097, nan,  1.26;
    0.15,  nan, 1.41];

%% Make figure
for i = 1:3 %scenarios
    plot(ax(i), [0,0],[-1,6], 'k--', 'linewidth',1.5)


    for im = 1:6 %5 models

        range =[lb(im, i) ub(im, i)];
        yv    = 6 - im;
        plot( ax(i), range,[yv,yv], 'LineWidth',5, 'Color',scen_col(i, :));


    end %end loop over models



end %end loop over scenarios


%% tidy
for i = 1:3

    ax(i).YLim = [-0.5, 5.5];
    ax(i).YTick =0:5;

end

ax(1).YTickLabel = flipud(sim_names');
ax(2).YTickLabel = {};
ax(3).YTickLabel = {};

ax(1).XLim = [-1, 2.5];
ax(2).XLim = [-1, 3];
ax(3).XLim = [-0.5, 4.5];

