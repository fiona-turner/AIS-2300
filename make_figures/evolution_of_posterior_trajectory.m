% make a plot of the evolution of the posterior mean trajectory as a
% function of the number of mcmc members


posterior = readmatrix("../outputs/mcmc_output_data/mcmc_output_posteriortrajectories.csv");
posterior_time = 1955:5:2300;

nmax = 1000:1000:length(posterior);
%nmax = [100, 200, 500, 1000, 1200, 1300];
figure(1); clf; hold on;
box on
ax =gca;
ax.FontSize = 14;
xlabel('year');
ylabel('SLE (mm)')
cmap = parula(length(nmax));

slr_2020 = nan(1,length(nmax));
slr_2300 = nan(1,length(nmax));
 
for in = 1:length(nmax)
posterior_red = posterior(1:nmax(in), :); %remove final rows
% rebase

[~,idx] = min(abs(posterior_time - 1979)); %this will be 1980 bc outputs in 5 years
slr_1979 = repmat(posterior_red(:,idx), [1,70]); %matrix with repeated entries corresponding to 1979 slr
posterior_red = posterior_red - slr_1979;

posterior_mean  = prctile(posterior_red,50,1);
posterior_mean_mb = -posterior_mean * (362.5*1000);
[~,idxt] = min(abs(posterior_time - 2020));
slr_2020(in) = posterior_mean(idxt);
slr_2300(in) = posterior_mean(end);
% 
% %plot

plot(posterior_time, posterior_mean,'color',  cmap(in,:), 'LineWidth',2)
xlim([1979, 2022])
end

c = colorbar;
c.Ticks = nmax./max(nmax);
c.Colormap = cmap;
c.Limits = [min(c.Ticks), max(c.Ticks)];
c.TickLabels = compose('%.0f', nmax);
c.Label.String = 'n_{MCMC}';
grid on

% add the observations
fname =  "../calibration-data/IMBIE/imbie3_December24/imbie3_antarctica_partitioned_Gt.csv";

imbie_data = readmatrix(fname);
imbie_mass_balance             = -imbie_data(:,4)/(362.5*1000);
imbie_mass_balance_uncertainty = -imbie_data(:,5)/(362.5*1000);
imbie_time                     = imbie_data(:,1);
[~,idxt] = min(abs(imbie_time-2020));

imbiecol = [0,0,0];
xf = [imbie_time; flip(imbie_time)];
yf = [imbie_mass_balance - imbie_mass_balance_uncertainty; flip(imbie_mass_balance + imbie_mass_balance_uncertainty)];
fill(xf, yf, imbiecol, 'LineStyle','none', 'FaceAlpha',0.2, 'HandleVisibility','off');

plot(imbie_time, imbie_mass_balance, 'LineWidth',  2, 'Color',imbiecol)


figure(2);clf; 
subplot(1,2,1)
plot(nmax, slr_2020, 'k', 'LineWidth',2);
xlabel('n_{max}');
ax = gca;
ax.FontSize = 14;
ylabel('SLR at 2020 (m)');

hold on
plot(nmax, imbie_mass_balance(idxt)*ones(size(nmax)), 'k--', 'LineWidth',2)
ylim(1e-3*[7, 12]);
xlim([0, max(nmax)]);

subplot(1,2,2)
plot(nmax, slr_2300, 'k', 'LineWidth',2);
xlabel('n_{max}');
ax = gca;
ax.FontSize = 14;
ylabel('SLR at 2300 (m)');
xlim([0, max(nmax)]);
