% Make a plot of the mass balance data with shaed uncertainty
% make a plot comparing the imbie timeseries for imbie 2 and imbie 3

fnames = ["./2022/imbie_antarctica_2022_Gt.csv", "./imbie3_December24/imbie3_antarctica_partitioned_Gt.csv","./imbie3_July25/imbie3_antarctica_Gt_partitioned.csv"];

tags = ["2022", "2024", "2025"];
pcol = [0.9, 0.3, 0; 
        0.3, 0, 0.9];
pcol = lines(length(fnames));

figure(1);clf; hold on;

for i = 1:length(fnames)

imbie_data = readmatrix(fnames(i));
imbie_mass_balance             = imbie_data(:,4);
imbie_mass_balance_uncertainty = imbie_data(:,5);
imbie_time                     = imbie_data(:,1);


xf = [imbie_time; flip(imbie_time)];
yf = [imbie_mass_balance - imbie_mass_balance_uncertainty; flip(imbie_mass_balance + imbie_mass_balance_uncertainty)];

fill(xf, yf, pcol(i,:), 'LineStyle','none', 'FaceAlpha',0.3, 'HandleVisibility','off');
plot(imbie_time, imbie_mass_balance, 'LineWidth',  2, 'Color',pcol(i,:))

end

ax = gca;
ax.FontSize = 14;
ax.XLim = [1979, 2025];
ax.XLabel.String = 'year';
ax.YLabel.String = 'mass balance (Gt)';
box on
legend(tags);