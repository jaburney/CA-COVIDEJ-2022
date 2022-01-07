import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import cm
import datetime
from census import Census
from us import states

#   Path to covid-pollution-master/
wd_path = '../'

vname = 'SD_dwellHome'

#   Load safegraph data
dat = pd.read_csv(wd_path+'/data/SafeGraph_CA_SocialDistancing_cbg.csv')
#   dat['fips_11'] = dat['fips_11'].astype(str)
dat['fips_12'] = dat['fips_12'].astype(str)
dat['fips_11'] = dat['fips_12'].astype(str).str[:-1]

#   Load census data and format
cen = pd.read_csv('../data/CA_blockgroup_metadata_6.29.20.csv')
cen = cen.rename(columns={'race_white_block_group':'WhiteTot',
                          'race_black_block_group':'BlackTot',
                          'hisp_yes_block_group':'HispTot',
                          'race_asian_block_group':'AsianTot',
                          'race_total_block_group':'TotPop',
                          'block_group':'fips_12'})
cen['fips_12'] = cen['fips_12'].astype(str)
cen['fips_11'] = cen['fips_12'].str[:-1]
#   cen = cen.groupby('fips_11').sum().reset_index().drop(columns='fips_12')
cen['county'] = cen['fips_11'].str[1:4]

allpop = np.sum(cen['TotPop'])

#   Combine census and safegraph
dat = pd.merge(dat,cen,on='fips_12')

#   Reformat into time series
dist = dat.pivot_table(values=vname, index=dat.fips_12, columns='date', aggfunc='first').T
pops = dat.pivot_table(values='TotPop', index=dat.fips_12, columns='date', aggfunc='first').T
dist = dist*pops
dist.index = pd.DatetimeIndex(dist.index)

dates = dist.index
dates = dates[dates>='2020-01-01']

#   Now start splitting by quantiles of variables of interest and plotting
def dwell2homeperc(arr):
    """
    dwell2homeperc(arr) takes an array of minutes at home per day
        and converts it to a percent of time not at home
    """
    return (1-arr/(24*60))*100


#   splitvars = ['NS','PercHisp','MedInc']
splitvars = ['Region']
fullvnames = {'Region':'Region',
              'PercBlack':'Percent Black',
              'PercWhite':'Percent White',
              'PercHisp':'Percent Hispanic',
              'MedInc':'Median Income'}
              
regcolors = {
            'N. California':        np.array([57,180,114])/255.,
            'N. Sac Valley':        np.array([153,251,153])/255.,
            'Greater Sac':          np.array([231,227,65])/255.,
            'Bay Area':             np.array([240,129,129])/255.,
            'Central Sierra':       np.array([211,181,141])/255.,
            'Central Coast':        np.array([255,167,0])/255.,
            'San Joaquin Valley':   np.array([155,206,46])/255.,
            'S. California':        np.array([174,217,231])/255.,
            'S. Border':            np.array([187,84,212])/255.,
            }
window = 1 #   Set window=1 to avoid any rolling averages
plt.figure(figsize=(11.6,7.2))
nct=0
sct=0
normalizelist = [True]
for normalize in normalizelist:
    nct+=1
    for sv in splitvars:
        sct+=1
        plt.subplot(len(normalizelist),len(splitvars),sct)

        if sv=='Region':
            Scounties = ['San Diego','Imperial','Riverside','Orange',
                        'San Bernardino','Los Angeles','Ventura',
                        'San Luis Obispo','Kern','Santa Barbara']
                        
            countyIDs = {
                '001': 'Alameda',
                '003': 'Alpine' ,
                '005': 'Amador' ,
                '007': 'Butte'  ,
                '009': 'Calaveras'  ,
                '011': 'Colusa' ,
                '013': 'Contra Costa'   ,
                '015': 'Del Norte'  ,
                '017': 'El Dorado'  ,
                '019': 'Fresno' ,
                '021': 'Glenn'  ,
                '023': 'Humboldt'   ,
                '025': 'Imperial'   ,
                '027': 'Inyo'   ,
                '029': 'Kern'   ,
                '031': 'Kings'  ,
                '033': 'Lake'   ,
                '035': 'Lassen' ,
                '037': 'Los Angeles',
                '039': 'Madera' ,
                '041': 'Marin'  ,
                '043': 'Mariposa'   ,
                '045': 'Mendocino'  ,
                '047': 'Merced' ,
                '049': 'Modoc'  ,
                '051': 'Mono'   ,
                '053': 'Monterey'   ,
                '055': 'Napa'   ,
                '057': 'Nevada' ,
                '059': 'Orange' ,
                '061': 'Placer' ,
                '063': 'Plumas' ,
                '065': 'Riverside'  ,
                '067': 'Sacramento' ,
                '069': 'San Benito' ,
                '071': 'San Bernardino' ,
                '073': 'San Diego'  ,
                '075': 'San Francisco'  ,
                '077': 'San Joaquin',
                '079': 'San Luis Obispo',
                '081': 'San Mateo' ,
                '083': 'Santa Barbara',
                '085': 'Santa Clara',
                '087': 'Santa Cruz' ,
                '089': 'Shasta' ,
                '091': 'Sierra' ,
                '093': 'Siskiyou',
                '095': 'Solano' ,
                '097': 'Sonoma' ,
                '099': 'Stanislaus',
                '101': 'Sutter' ,
                '103': 'Tehama' ,
                '105': 'Trinity',
                '107': 'Tulare' ,
                '109': 'Tuolumne',
                '111': 'Ventura',
                '113': 'Yolo'  ,
                '115': 'Yuba'
            }

            cname = np.array([countyIDs[k] for k in countyIDs.keys()])
            cnum  = np.array([k for k in countyIDs])
            sorti = np.argsort(cnum)
            
            cats = [['Del Norte','Siskiyou','Modoc','Lassen','Humboldt','Trinity',
                    'Plumas','Sierra','Nevada','Mendocino','Lake'],
                    ['Shasta','Tehama','Glenn','Butte','Colusa'],
                    ['Yolo','Sutter','Yuba','Sacramento','El Dorado','Placer'],
                    ['Sonoma','Napa','Marin','Solano','Contra Costa','Alameda','San Francisco',
                     'San Mateo','Santa Clara','Santa Cruz','San Benito'],
                    ['Amador','Calaveras','Alpine','Tuolumne','Mariposa','Mono','Inyo'],
                    ['Monterey','San Luis Obispo','Santa Barbara'],
                    ['San Joaquin','Stanislaus','Merced','Madera','Fresno','Kings','Tulare','Kern'],
                    ['Ventura','Los Angeles','San Bernardino','Orange','Riverside'],
                    ['San Diego','Imperial']]
            
            out = pd.DataFrame(np.vstack((cnum[sorti],cname[sorti])).T,columns=['county_fips','county_name'])
            
            #   Clunky way to figure out which category the county is in...
            catnum = np.zeros(len(cnum),dtype=int)
            for ci in range(len(out['county_name'])):
                county = out['county_name'][ci]
            
                for i in range(len(cats)):
                    if county in cats[i]:
                        catnum[ci] = i+1
                        break
            
            out['county_category'] = catnum
            num2reg = {1:'N. California',
                       2:'N. Sac Valley',
                       3:'Greater Sac',
                       4:'Bay Area',
                       5:'Central Sierra',
                       6:'Central Coast',
                       7:'San Joaquin Valley',
                       8:'S. California',
                       9:'S. Border',
                       }
            regions = [num2reg[n] for n in out['county_category'].values]
            out['region'] = regions

            #   Aggregate data for each region and make plot
            for reg in np.unique(regions):
                countynums = out.loc[out['region']==reg]['county_fips'].values
                ri = np.array([cen[cen['fips_12']==blk]['county'].values[0] in countynums 
                               for blk in dist.columns])
            
                #   region populations
                blkpops = np.sum([cen[cen['fips_12']==blk]['TotPop'].values[0] 
                                   for blk in dist.columns[ri]])
            
                #   weighted means (mutliplied by block pop earlier already)
                regmean = (dist.iloc[:,ri]/blkpops).sum(axis=1)
                regmean = regmean['2020-01-01':]
                #   Take average
                regmean = pd.DataFrame(regmean).rolling(window).mean()
                if normalize:
                    regmean = ((regmean[:'2020-01-31'].mean() - regmean)/
                                regmean[:'2020-01-31'].mean()) * 100
                else:
                    regmean = dwell2homeperc(regmean) 
                #   Draw line
                plt.plot(dates,regmean,color=regcolors[reg],label=reg)
            plt.legend(fontsize=13)

    
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
                ax.set_ylim([10,80])
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
        plt.text(emergdate-2,ylims[0]+2,'CA emergency',
                va='bottom',ha='left',rotation=90,fontsize=13)
        plt.text(lockdate+2,ylims[1]-2,'CA stay-home',
                va='top',ha='right',rotation=-90,fontsize=13)
    
        xtick = [np.datetime64(d) for d in pdates]
        ax.set_xticks(xtick)
        tlabs = [np.datetime64(x,'D') for x in pdates]
        ax.set_xticklabels(tlabs,rotation=30,ha='right')
        ax.set_xlim([np.datetime64(pdates[0]),np.datetime64(pdates[-1])])
        ax.tick_params(labelsize=11)
        ax.grid(linestyle='--',color=[0.8,0.8,0.8])
        plt.tight_layout()

plt.savefig(wd_path+'/figures/FS11_regional_mobility.pdf')    

