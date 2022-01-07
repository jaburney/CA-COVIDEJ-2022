import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import cm
from census import Census
from us import states

#   Path to covid-pollution-master/
wd_path = '../'

#   Thanks to Alleo - function from here:
#   https://stackoverflow.com/questions/21844024/weighted-percentile-using-numpy/32216049
def weighted_quantile(values, quantiles, sample_weight=None, 
                      values_sorted=False, old_style=False):
    """ Very close to numpy.percentile, but supports weights.
    NOTE: quantiles should be in [0, 1]!
    :param values: numpy.array with data
    :param quantiles: array-like with many quantiles needed
    :param sample_weight: array-like of the same length as `array`
    :param values_sorted: bool, if True, then will avoid sorting of
        initial array
    :param old_style: if True, will correct output to be consistent
        with numpy.percentile.
    :return: numpy.array with computed quantiles.
    """
    values = np.array(values)
    quantiles = np.array(quantiles)
    if sample_weight is None:
        sample_weight = np.ones(len(values))
    sample_weight = np.array(sample_weight)
    assert np.all(quantiles >= 0) and np.all(quantiles <= 1), \
        'quantiles should be in [0, 1]'

    if not values_sorted:
        sorter = np.argsort(values)
        values = values[sorter]
        sample_weight = sample_weight[sorter]

    weighted_quantiles = np.cumsum(sample_weight) - 0.5 * sample_weight
    if old_style:
        # To be convenient with numpy.percentile
        weighted_quantiles -= weighted_quantiles[0]
        weighted_quantiles /= weighted_quantiles[-1]
    else:
        weighted_quantiles /= np.sum(sample_weight)
    return np.interp(quantiles, weighted_quantiles, values)


vname = 'SD_dwellHome'

#   Load safegraph data
dat = pd.read_csv(wd_path+'/data/SafeGraph_CA_SocialDistancing_cbg.csv')

cen = pd.read_csv('../data/CA_blockgroup_metadata_6.29.20.csv')
cen = cen.rename(columns={'race_white_block_group':'WhiteTot',
                          'race_black_block_group':'BlackTot',
                          'hisp_yes_block_group':'HispTot',
                          'race_asian_block_group':'AsianTot',
                          'race_total_block_group':'RaceTot',
                          'block_group':'fips_12'})

cen['PercWhite'] = cen['WhiteTot']/cen['RaceTot']
cen['PercBlack'] = cen['BlackTot']/cen['RaceTot']
cen['PercHisp'] = cen['HispTot']/cen['RaceTot']
cen['PercAsian'] = cen['AsianTot']/cen['RaceTot']

allpop = np.sum(cen['RaceTot'])

#   Combine census and safegraph
dat = pd.merge(dat,cen,on='fips_12')

#   Reformat into time series
dateidx = pd.DatetimeIndex(dat['date'])
udates = pd.DatetimeIndex(np.unique(dateidx[pd.notnull(dateidx)]))
udates.name = 'date'
tracts = np.unique(dat['fips_12'])

alltracts = []
for tract in tracts:
    tractdat = dat[dat['fips_12']==tract]
    tractdat.index = pd.DatetimeIndex(tractdat['date'])
    tractdat = tractdat[vname]
    tractdat = tractdat[pd.notnull(tractdat)]
    
    tractdat.name = tract
    alltracts.append(tractdat)

dist = pd.concat(alltracts,axis=1,sort=False)
dates = dist.index

#   Cut to 2020 data since 2019 is available now too
dist = dist[dist.index>'2019-12-31']
dates = dist.index

#   Now start splitting by quantiles of variables of interest and plotting
def dwell2homeperc(arr):
    """
    dwell2homeperc(arr) takes an array of minutes at home per day
        and converts it to a percent of time not at home
    """
    return (1-arr/(24*60))*100

#   Select variable for plotting
splitvars = ['PercHisp','PercAsian','PercBlack']
#   Set plotting names
fullvnames = {'NS':'CA North/South',
              'PercBlack':'% Black',
              'black':'% Black',
              'PercWhite':'% White',
              'PercHisp':'% Hispanic',
              'hisp':'% Hispanic',
              'PercAsian':'% Asian',
              'asian':'% Asian',
              'MedInc':'Median Income',
              'income':'Median Income'}
#   Initialize summary table
#   This is Table S1
tabcol = ['Pre-shutdown','Post-shutdown','% change']
tabind = ['Hispanic Low','Hispanic High','Asian Low','Asian High','Black Low','Black High']
summary = pd.DataFrame(index=tabind,columns=tabcol)

#   cmaps = [cm.plasma_r,cm.ocean,cm.viridis_r]*2
cmaps = [cm.bwr,cm.bwr,cm.bwr]
window = 1 #   Set window=1 to avoid any rolling averages
plt.figure(figsize=(10,10))
nct=0
#   Don't do any normalization
for normalize in [False]:
    nct+=1
    #   Run through each of the variables used in the subplots
    for sct,sv in enumerate(splitvars):
        plt.subplot(3,1,sct+1)
        quartiles=[[0,10],[90,100]] #   Plotting range
        colorfrac = [0.25,0.75]     #   Color fraction within colormap
        vmobility = []              
        vmobilitystd = []
        #   For each of the potting ranges, aggregate data
        for gi in range(len(quartiles)):
            vquart = np.nanpercentile(cen[sv],quartiles[gi])
            for i in range(len(quartiles)-1):
                fipsq = cen['fips_12'][(cen[sv]>=vquart[i])&(cen[sv]<=vquart[i+1])]
                fipsqi = [tr in fipsq.values for tr in dist.columns]
                vpop = np.array([cen[cen['fips_12']==tr]['RaceTot'].values[0] for tr in fipsq]) 
                vpopq = np.nansum(vpop)
                weight = vpop

                x = dwell2homeperc(dist.iloc[:,fipsqi])
                wquant = np.array([weighted_quantile(x.values[ri,:],[0.2,0.5,0.8],weight) 
                                for ri in range(x.shape[0])])
                wquant = pd.DataFrame(wquant,index=dates)
                vmeanq = np.sum(weight*x,1)/np.sum(weight)
                vstdq = np.sqrt((np.sum(weight*x*x,1)*np.sum(weight)-(np.sum(weight*x,1)**2))/
                                 np.sum(weight)**2)
                vmeanq = pd.DataFrame(vmeanq).rolling(window).mean()
                vmobility.append(wquant)
        
        #   Get index for colormap
        coli = np.where([sv==x for x in splitvars])[0][0]
        #   Draw line
        labsi = ['Lower {}%'.format(quartiles[0][1]),'Upper {}%'.format(quartiles[0][1])]
        for i in range(len(quartiles)):
            plt.plot(dates,vmobility[i][1],
                     color = cmaps[coli](colorfrac[i]),
                     label=labsi[i])
            plt.fill_between(dates,vmobility[i][0],
                                   vmobility[i][2],
                                   color = cmaps[coli](colorfrac[i]),
                                   alpha = 0.1)
        plt.legend(fontsize=13)
        plt.gca().annotate(fullvnames[sv],(0.03,0.03),xycoords='axes fraction',
                           ha='left',va='bottom',fontsize=15)
    
        #   Format axes
        pdates = ['2020-01-01','2020-01-15','2020-02-01','2020-02-15','2020-03-01',
                  '2020-03-15','2020-04-01','2020-04-15','2020-05-01']
        ax = plt.gca()
        emergdate = np.datetime64('2020-03-04')
        lockdate = np.datetime64('2020-03-19')
        if not normalize:
            if window == 7:
                ax.set_ylim([10,65])
            elif window == 14:
                ax.set_ylim([15,65])
            else:
                ax.set_ylim([0,80])
            plt.ylabel('% of day away from home',fontsize=12)
        else:
            if window == 7:
                ax.set_ylim([-65,20])
            elif window == 14:
                ax.set_ylim([-65,20])
            else:
                ax.set_ylim(-80,60)
            plt.ylabel('% change: % of day away from home',fontsize=12)
        ylims = ax.get_ylim()
        plt.plot([emergdate,emergdate],ylims,'k',linewidth=2)
        plt.plot([lockdate,lockdate],ylims,'k',linewidth=2)
    
        xtick = [np.datetime64(d) for d in pdates]
        ax.set_xticks(xtick)
        tlabs = [np.datetime64(x,'D') for x in pdates]
        if sct==(len(splitvars)-1):
            ax.set_xticklabels(tlabs,rotation=30,ha='right')
        else:
            ax.set_xticklabels('')
        plt.text(emergdate-2,ylims[0]+3,'Emergency',
                va='bottom',rotation=90,fontsize=11)
        plt.text(lockdate-2,ylims[0]+3,'Stay-home',
                va='bottom',rotation=90,fontsize=11)
        ax.set_xlim([np.datetime64(pdates[0]),np.datetime64(pdates[-1])])
        ax.tick_params(labelsize=11)
        ax.grid(linestyle='--',color=[0.8,0.8,0.8])

        if 'hisp' in sv.lower():
            ntmp = 'Hispanic'
        elif 'asia' in sv.lower():
            ntmp = 'Asian'
        elif 'black' in sv.lower():
            ntmp = 'Black'
        elif 'white' in sv.lower():
            ntmp = 'White'

        summary.at[ntmp+' Low','Pre-shutdown'] = np.mean(vmobility[0].loc[dates<emergdate,1])
        summary.at[ntmp+' High','Pre-shutdown'] = np.mean(vmobility[1].loc[dates<emergdate,1])
        summary.at[ntmp+' Low','Post-shutdown'] = np.mean(vmobility[0].loc[dates>lockdate,1])
        summary.at[ntmp+' High','Post-shutdown'] = np.mean(vmobility[1].loc[dates>lockdate,1])
        summary.at[ntmp+' Low','% change'] = ((summary.loc[ntmp+' Low','Post-shutdown'] - 
                                                  summary.at[ntmp+' Low','Pre-shutdown'])/
                                                  summary.at[ntmp+' Low','Pre-shutdown'])
        summary.at[ntmp+' High','% change'] = ((summary.loc[ntmp+' High','Post-shutdown'] - 
                                                  summary.at[ntmp+' High','Pre-shutdown'])/
                                                  summary.at[ntmp+' High','Pre-shutdown'])

plt.tight_layout()
#   plt.savefig(wd_path+'/figures/F2_mobility.pdf')
summary.to_csv(wd_path+'/figures/table_S1_mobility.csv')

