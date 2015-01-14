#### ggplots for manuscript
library(ggplot2)
library(raster)
library(rgdal)
library(spacetime)
library(gridExtra)
library(plyr)
library(nlme)
library(rasterVis)

setwd('~/Google Drive/UNR/UNR-Thesis/Data')

#### DEFINE HEX COLORS
red = '#990000'
lightred = '#df3232'
blue = '#0f5bff' #'#5C92FA' ##0f5bff
orange = '#ff8b00'
darkblue = '#093065'
sage_color = '#417e3b'
purple = '#8974bd'
white = 'white'
## for temp_cols
temp_red = '#cc0d0d'
temp_orange = '#ff8100'
yellow = '#eae011'
off_white = '#ffffff'
temp_cols = colorRampPalette(c(temp_red, temp_orange, 
                               yellow, off_white))
min_temp = colorRampPalette(c(darkblue, blue, off_white))
max_temp = colorRampPalette(c(red, lightred, off_white))

#### figure 3 and figure 4
## laod in the EOF analysis and the master tmn tmx data
load('/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/Modeling_R-Images/2014-10-30_EOF-scaled.RData')
# source('~/Google Drive/UNR/UNR-Thesis/Data/Thesis-Code/load_and_melt.R')

#### CREATE FIGURE 4
# library(rasterVis)
seof = stack(eof)
plot_seof = stack(seof[[1:4]])

## load in the political boundaries
states = readOGR(dsn='/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/GIS-Data/states_21basic',
                 layer='states')
canada = readOGR(dsn='/home/vitale232/Google Drive/UNR/UNR-Thesis/Data/GIS-Data/Canada',
                 layer='Canada')

states = spTransform(states, CRS(projection(plot_seof)))
canada = spTransform(canada, CRS(projection(plot_seof)))

states = crop(states, plot_seof)
canada = crop(canada, plot_seof)


## raster package version
cols = colorRampPalette(c(blue, white, lightred))
# x11(height=9, width=9)
# 
# par(mfrow=c(2,2),
#     mar=c(5, 4, 4, 5))

## Format the device for plotting
pdf('~/Google Drive/UNR/UNR-Thesis/Manuscript/JAMC/figure02_EOFs.pdf',
    height=7.5, width=11)
par(mfrow=c(2,2),
    mar=c(1, 4, 4, 10))

## Plot the EOF rasters
plot(plot_seof[[1]], col=cols(255), main='EOF1',axes=FALSE)
plot(states, add=TRUE, border='gray42')
plot(canada, add=TRUE, border='gray42')
contour(plot_seof[[1]], add=TRUE)

plot(plot_seof[[2]], col=cols(255), main='EOF2',axes=FALSE)
plot(states, add=TRUE, border='gray42')
plot(canada, add=TRUE, border='gray42')
contour(plot_seof[[2]], add=TRUE)

plot(plot_seof[[3]], col=cols(255), main='EOF3',axes=FALSE)
plot(states, add=TRUE, border='gray42')
plot(canada, add=TRUE, border='gray42')
contour(plot_seof[[4]], add=TRUE)

plot(plot_seof[[4]], col=cols(255), main='EOF4',axes=FALSE)
plot(states, add=TRUE, border='gray42')
plot(canada, add=TRUE, border='gray42')
contour(plot_seof[[4]], add=TRUE)

dev.off()

#### Load in the stats dataset for tmn and tmx
source('~/Google Drive/UNR/UNR-Thesis/Data/Thesis-Code/load_and_melt.R')




# ## GGPLOT2 version
# dfeof = as.data.frame(eof[ , 1:4])
# 
# cols = colorRampPalette(c(darkblue, blue, lightred, red))
# 
# dfeof = data.frame(x=rep(dfeof$x, 4),
#                    y=rep(dfeof$y, 4),
#                    z=c(dfeof$EOF1, dfeof$EOF2, 
#                        dfeof$EOF3, dfeof$EOF4),
#                    EOF=factor(c(rep('EOF1', length(dfeof$x)),
#                                 rep('EOF2', length(dfeof$x)),
#                                 rep('EOF3', length(dfeof$x)),
#                                 rep('EOF4', length(dfeof$x)))))
# fig4 = ggplot(data=dfeof, aes(x, y, z=z)) +
#   geom_tile(aes(fill=z)) +
#   facet_wrap(~ EOF) +
#   stat_contour(aes(x, y, z)) +
#   scale_fill_gradient2(low=blue, mid=white, high=lightred) +
#   theme_gray(base_size=20) +
#   theme(axis.text.x=element_text(color='black'),
#         axis.text.y=element_text(color='black'),
#         strip.text=element_text(size=22))
# print(fig4)

#### CREATE FIGURE 3
fig3_df = teof[, c('date', 'PC1', 'PC2', 'PC3', 'PC4')]
fig3_df$date = as.Date(fig3_df$date)
fig3_df[ , 2:5] = apply(fig3_df[ , 2:5], 2, scale)
start = as.Date('2010-01-01')
finish = as.Date('2014-09-24')

## Subset to the dates we have SRSN readings
fig3_ss = fig3_df[fig3_df$date >= start & fig3_df$date <= finish, ]
srsn = fig3_df[fig3_df$date >= as.Date('2013-06-17') & 
               fig3_df$date <= as.Date('2014-06-24'), ]
# 
# g1 = ggplot(aes(x=date, y=PC1), data=fig3_ss) + geom_line(color='blue') + 
#      geom_hline(aes(yintercept=0), linetype=2) + theme_gray(base_size=20) +
#      ggtitle('(a)') + theme(plot.title=element_text(hjust=0),
#                             axis.text.y=element_text(color='black'),
#                             axis.text.x=element_blank(),
#                             axis.title.x=element_blank()) 
# g2 = ggplot(aes(x=date, y=PC2), data=fig3_ss) + geom_line(color='blue') +
#   geom_hline(aes(yintercept=0), linetype=2) + theme_gray(base_size=20) +
#   ggtitle('(a)') + theme(plot.title=element_text(hjust=0),
#                          axis.text.y=element_text(color='black'),
#                          axis.text.x=element_blank(),
#                          axis.title.x=element_blank())
# g3 = ggplot(aes(x=date, y=PC3), data=fig3_ss) + geom_line(color='blue') +
#   geom_hline(aes(yintercept=0), linetype=2) + theme_gray(base_size=20) +
#   ggtitle('(a)') + theme(plot.title=element_text(hjust=0),
#                          axis.text.y=element_text(color='black'),
#                          axis.text.x=element_blank(),
#                          axis.title.x=element_blank())
# g4 = ggplot(aes(x=date, y=PC4), data=fig3_ss) + geom_line(color='blue') + xlab('Date') +
#   geom_hline(aes(yintercept=0), linetype=2) + theme_gray(base_size=20) +
#   ggtitle('(a)') + theme(plot.title=element_text(hjust=0),
#                          axis.text.x=element_text(color='black'),
#                          axis.text.y=element_text(color='black'))

# # pdf(file='~/Google Drive/UNR/UNR-Thesis/Manuscript/JAMC/figure03_PCs.pdf',
# #     width=10, height=8)
# grid.arrange(g1, g2, g3, g4, nrow=4)
# # dev.off()
# 

#### RUN THE STUFF ABOVE THIS TO CREATE FIGURE 3 ####

df3 = melt(fig3_ss, id.vars='date')
fig3_ann = data.frame('Date'=rep(start, 4),
                      'PC'=factor(c('PC1', 'PC2', 'PC3', 'PC4')),
                      'Score'=c(1.9, 3, 3.8, 2.25),
                      'lab'=c('(a)', '(b)', '(c)', '(d)'))
names(df3) = c('Date', 'PC', 'Score')

why = c('blue'=blue)
fig3 = qplot(Date, Score, data=df3, geom=c('line'), color='blue') + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  facet_grid(PC ~ ., scales='free') 
fig3 = fig3 + theme_gray(base_size=20) +
   theme(axis.text.x=element_text(color='black'),
         axis.text.y=element_text(color='black'),
         strip.text=element_text(size=22))
fig3 = fig3 + scale_color_manual(values=why, guide=FALSE)
fig3 = fig3 + geom_text(data=fig3_ann, aes(Date, Score, label=lab), 
                        color='black', size=10)
fig3 = fig3 + geom_vline(xintercept=as.numeric(as.Date('2013-06-17')),
                         linetype=4, color=red) +
              geom_vline(xintercept=as.numeric(as.Date('2014-06-24')),
                         linetype=4, color=red)
# fig3 = fig3 + geom_vline(aes(x=as.Date('2013-06-17')), color='black')
print(fig3)

ggsave(fig3, filename='~/Google Drive/UNR/UNR-Thesis/Manuscript/JAMC/figure03_PCs.pdf',
       width=11, height=7)




#### CREATE FIGURE 4
m_tmn$merge = paste(m_tmn$date, m_tmn$site)
m_tmx$merge = paste(m_tmx$date, m_tmx$site)
m = merge(m_tmn, m_tmx[, c('merge', 'tmx')], by='merge')

# red = 'firebrick1'
# blue = 'dodgerblue2'

cols = c('SRSN Maximum Temperature'=red,
         'SRSN Minimum Temperature'=blue,
         'Regional Average Temperature'='black')
tp = ggplot(m) +
     geom_point(aes(date, tmx, color='SRSN Maximum Temperature'), alpha=0.5) +
     geom_point(aes(date, tmn, color='SRSN Minimum Temperature'), alpha=0.5) +
     geom_line(aes(date, tair, color='Regional Average Temperature'), size=1.25) +
     xlab('Date') + ylab('Temperature (°C)') +
#      ggtitle('Daily Air Temperature in the Snake Range, NV') +
     scale_color_manual(name='', values=cols) +
     theme_gray(base_size=20) +
     theme(legend.position='bottom',
           axis.text.x=element_text(color='black'),
           axis.text.y=element_text(color='black')) +
     guides(color=guide_legend(override.aes = list(shape=c(NA, 16, 16),
                                                   linetype=c(1, 0, 0),
                                                   size=c(2, 3, 3))))
# x11(height=7, width=11)
print(tp)

ggsave(tp, filename='~/Google Drive/UNR/UNR-Thesis/Manuscript/JAMC/figure04_raw-t-data.pdf',
       width=11, height=7)



srsn$tair = ra$tair
ra_tair = ggplot(srsn) +
  geom_line(aes(date, tair, color='green')) +
  geom_line(aes(date, scale(PC4)*10, color='orange'))
print(ra_tair)
qplot(tair, PC4, data=srsn) + geom_smooth(aes(tair, PC4), data=srsn, method='lm')




#### Figure 5

m_tmn$month = factor(format(m_tmn$date, '%b'),
                     levels=c('Jan', 'Feb' ,'Mar',
                              'Apr', 'May', 'Jun',
                              'Jul', 'Aug', 'Sep',
                              'Oct', 'Nov', 'Dec'))
df = m_tmn
df$site = as.character(df$site)
tmn_month = ddply(df, .(site, month),
                  summarize,
                  tmn=round(mean(tmn, na.rm=TRUE), 2),
                  sd=round(sd(tmn, na.rm=TRUE), 2))

tmn_month = merge(tmn_month, snake[, c('ID', 'elev', 'tci', 'cc_nlcd')],
                  by.x='site', by.y='ID', all.x=TRUE, all.y=FALSE)
pp = qplot(elev, tmn, data=tmn_month)
pp + facet_wrap( ~ month, nrow=3)

mod = lme(tmn ~ elev, data=m_tmn,
          random=~1|month, na.action=na.exclude)
df$resid = resid(mod)

resid_month = ddply(df, .(site, month),
                    summarize,
                    resid=round(mean(resid, na.rm=TRUE), 2),
                    sd=round(sd(resid, na.rm=TRUE), 2))

resid_month = merge(resid_month, snake[, c('ID', 'elev', 'tci', 'cc_nlcd', 'gis.slope')],
                    by.x='site', by.y='ID', all.x=TRUE, all.y=FALSE)

pp = ggplot(resid_month) + geom_point(aes(tci, resid), color=blue)
pp = pp + facet_wrap( ~ month, nrow=3) + 
  stat_smooth(aes(tci, resid), data=resid_month[!(resid_month$month %in% c('Jan', 'Nov', 'Dec')) ,],
              method='lm', se=TRUE, color='darkgray', size=1.2) +
  stat_smooth(aes(tci, resid), data=resid_month[resid_month$month %in% c('Jan', 'Nov', 'Dec') ,],
              method='lm', se=TRUE, color=red, size=1.2) + 
  stat_smooth(aes(tci, resid), data=resid_month[resid_month$month == 'Jun',],
              method='lm', se=TRUE, color=orange, size=1.2)
pp = pp + theme_gray(base_size=20) + 
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        strip.text=element_text(size=22)) + 
  xlab('Terrain Convergence Index (TCI)') +
  ylab(expression(paste("Residual Minimum Temperature (", degree, "C)")))
print(pp)


ggsave('~/Google Drive/UNR/UNR-Thesis/Manuscript/JAMC/figure05_tci-elev-resid.pdf',
       height=8, width=9)



#### FIGURE 6
rad = raster('./GIS-Data/Irradiance/annual_rad.grd')
snake$rad = extract(rad, snake)
m_tmx$month = factor(format(m_tmn$date, '%b'),
                     levels=c('Jan', 'Feb' ,'Mar',
                              'Apr', 'May', 'Jun',
                              'Jul', 'Aug', 'Sep',
                              'Oct', 'Nov', 'Dec'))
df2 = m_tmx

df2$site = as.character(df2$site)
tmx_month = ddply(df2, .(site, month),
                  summarize,
                  tmx=round(mean(tmx, na.rm=TRUE), 2),
                  sd=round(sd(tmx, na.rm=TRUE), 2),
                  irrad=round(mean(irrad, na.rm=TRUE), 2))

tmx_month = merge(tmx_month, snake[, c('ID', 'elev', 'tci', 'cc_nlcd', 'rad')],
                  by.x='site', by.y='ID', all.x=TRUE, all.y=FALSE)
pp = qplot(elev, tmx, data=tmx_month)
pp + facet_wrap( ~ month, nrow=3)

mod2 = lme(tmx ~ elev, data=m_tmx,
           random=~1|month, na.action=na.exclude)
df2$resid = resid(mod)

resid_month_tmx = ddply(df2, .(site, month),
                        summarize,
                        resid=round(mean(resid, na.rm=TRUE), 2),
                        sd=round(sd(resid, na.rm=TRUE), 2),
                        irrad=round(mean(irrad, na.rm=TRUE), 2))

resid_month_tmx = merge(resid_month_tmx, snake[, c('ID', 'elev', 'tci', 'cc_nlcd', 'gis.slope', 'rad')],
                        by.x='site', by.y='ID', all.x=TRUE, all.y=FALSE)


fig6 = ggplot(resid_month_tmx) + geom_point(aes(gis.slope, resid), color=blue)
fig6 = fig6 + facet_wrap( ~ month, nrow=3)
fig6 = fig6 + stat_smooth(aes(gis.slope, resid), 
                          method='lm', se=TRUE, color=red, size=1.2)
fig6 = fig6 + theme_gray(base_size=20) + 
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        strip.text=element_text(size=22)) + 
  xlab(expression(paste('Slope (', degree, ')'))) +
  ylab(expression(paste("Residual Maximum Temperature (", degree, "C)"))) +
  scale_x_continuous(labels=c('0', '10', '20', '30', '40', ''))
print(fig6)

ggsave('~/Google Drive/UNR/UNR-Thesis/Manuscript/JAMC/figure06_slope-elev-resid.pdf',
       height=8, width=9)


#### Figure 7 - tmn validation
source('~/Google Drive/UNR/UNR-Thesis/Data/Thesis-Code/validation_tmn.R')

cols = c('Sage'=sage_color,
         'PJ'=darkblue,
         'Montane'=orange,
         'Subalpine'=purple)
fig7 = ggplot(df) +
  geom_point(aes(date, sage, color='Sage'), size=3, alpha=0.8) +
  geom_point(aes(date, pj, color='PJ'), size=3, alpha=0.8) +
  geom_point(aes(date, montane, color='Montane'), size=3, alpha=0.8) +
  geom_point(aes(date, subalpine, color='Subalpine'), size=3, alpha=0.8) +
  geom_hline(aes(yintercept=0), linetype=2) +
  xlab('Date') +
#   ylim(c(-9, 14)) +
  ylab(expression(paste('Prediction - Observation (', degree, 'C)'))) +
  ggtitle('Minimum Daily Temperature Bias') +
  scale_color_manual(name='', values=cols) +
  theme_gray(base_size=20) +
  theme(legend.position='bottom',
        axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))

print(fig7)

ggsave('~/Google Drive/UNR/UNR-Thesis/Manuscript/JAMC/figure07_tmn-validation.pdf',
       height=7, width=11)


#### Figure 8 - tmx validation ####
source('~/Google Drive/UNR/UNR-Thesis/Data/Thesis-Code/validation_tmx.R')

cols = c('Sage'=sage_color,
         'PJ'=darkblue,
         'Montane'=orange,
         'Subalpine'=purple)
fig8 = ggplot(df2) +
  geom_point(aes(date, sage, color='Sage'), size=3, alpha=0.8) +
  geom_point(aes(date, pj, color='PJ'), size=3, alpha=0.8) +
  geom_point(aes(date, montane, color='Montane'), size=3, alpha=0.8) +
  geom_point(aes(date, subalpine, color='Subalpine'), size=3, alpha=0.8) +
  geom_hline(aes(yintercept=0), linetype=2) +
  xlab('Date') +
  ylab(expression(paste('Prediction - Observation (', degree, 'C)'))) +
  ggtitle('Maximum Daily Temperature Bias') +
  scale_color_manual(name='', values=cols) +
  theme_gray(base_size=20) +
  theme(legend.position='bottom',
        axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))

print(fig8)

ggsave('~/Google Drive/UNR/UNR-Thesis/Manuscript/JAMC/figure08_tmx-validation.pdf',
       height=7, width=11)

#### Figure 9 - minimum and maximum average temperature ####
dec_files = list.files('./Temperature-Maps/Tmn/tmn_mod/', 
                       pattern='201[3-4]-12-[0-9][0-9]_tmn.tif$',
                       full.names=TRUE)
dec_stack = stack(dec_files)
dec = mean(dec_stack)

jul_files = list.files('./Temperature-Maps/Tmx/tmx_mod/',
                       pattern='201[3-4]-07-[0-9][0-9]_tmx.tif$',
                       full.names=TRUE)
jul_stack = stack(jul_files)
jul = mean(jul_stack)

stations$lab = c('Sage', 'PJ', 'Montane', 'Subalpine', '', '', '')

# x11(width=8, height=9.5)
pdf('/home/vitale232/Google Drive/UNR/UNR-Thesis/Manuscript/JAMC/figure09_temperature-maps.pdf',
    width=8, height=9.425)
par(mfrow=c(2,1))
plot(dec, col=temp_cols(255), main='Average Minimum Temperature, December 2013',
     legend=FALSE, axes=FALSE)
plot(dec, legend.only=TRUE, axes=FALSE, col=temp_cols(255),
     legend.args=list(text=expression(paste('Minimum Temperature (', degree, 'C)')), 
                      side=4, line=2.5))
plot(stations, add=TRUE, pch=22, col='black', bg='gray88')
text(stations, labels=stations$lab, cex=1.25, pos=1)
mtext('(a)', side=3, line=0.5, adj=0, cex=2)


plot(jul, col=temp_cols(255), main='Average Maximum Temperature, July 2013',
     legend=FALSE, axes=FALSE)
plot(jul, legend.only=TRUE, axes=FALSE, col=temp_cols(255),
     legend.args=list(text=expression(paste('Maximum Temperature (', degree, 'C)')), 
                      side=4, line=2.5))
plot(stations, add=TRUE, pch=22, col='black', bg='gray88')
text(stations, labels=stations$lab, cex=1.25, pos=1)
mtext('(b)', side=3, line=0.5, adj=0, cex=2)
dev.off()



#### Figure 10, elevation quadratic tmn ####
fig10_df = m_tmn
if(fig10_df$elev[1] > 3) {
  fig10_df$elev = fig11_df$elev/1000
}
fig10_df = fig10_df[fig10_df$date == as.Date('2013-06-19') |
                      fig10_df$date == as.Date('2013-12-12'), ]
fig10_df$date = factor(fig10_df$date)

fig10_ann = data.frame('date'=factor(c(as.Date('2013-06-19'), as.Date('2013-12-12'))),
                       'elev'=c(3.3, 3.3),
                       'tmn'=c(11, 11),
                       'lab'=c('(a)', '(b)'))

fig10 = ggplot(fig10_df) +
  geom_point(aes(elev, tmn), color=blue, size=2.75) +
  stat_smooth(aes(elev, tmn), method=lm, 
              color=red, se=TRUE, size=1.25) +
  geom_text(data=fig10_ann, aes(elev, tmn, label=lab), size=10) +
  facet_grid(. ~ date) + 
  xlab('Elevation (km)') + 
  ylab(expression(paste('Minimum Temperature (', degree, 'C)'))) +
  theme_gray(base_size=20) +
  theme(legend.position='bottom',
        axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))


print(fig10)

ggsave('~/Google Drive/UNR/UNR-Thesis/Manuscript/JAMC/figure10_tmn-lapse.pdf', 
       height=7, width=9)


#### Figure 11, elevation quadratic tmx ####
fig11_df = m_tmx
if(fig11_df$elev[1] > 3) {
  fig11_df$elev = fig11_df$elev/1000
}
fig11_df = fig11_df[fig11_df$date == as.Date('2013-06-19') |
                      fig11_df$date == as.Date('2013-12-12'), ]
fig11_df$date = factor(fig11_df$date)

fig11_ann = data.frame('date'=factor(c(as.Date('2013-06-19'), as.Date('2013-12-12'))),
                       'elev'=c(3.3, 3.3),
                       'tmx'=c(27, 27),
                       'lab'=c('(a)', '(b)'))

fig11 = ggplot(fig11_df) +
  geom_point(aes(elev, tmx), color=blue, size=2.75) +
  stat_smooth(aes(elev, tmx), method=lm, 
              color=red, se=TRUE, size=1.25) +
  geom_text(data=fig11_ann, aes(elev, tmx, label=lab), size=10) +
  facet_grid(. ~ date) + 
  xlab('Elevation (km)') + 
  ylab(expression(paste('Maximum Temperature (', degree, 'C)'))) +
  theme_gray(base_size=20) +
  theme(legend.position='bottom',
        axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))


print(fig11)

ggsave('~/Google Drive/UNR/UNR-Thesis/Manuscript/JAMC/figure11_tmx-lapse.pdf', 
       height=7, width=9)
