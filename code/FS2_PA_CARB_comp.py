import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
from math import radians, degrees, sin, cos, asin, acos, sqrt

#   Path to covid-pollution-master/
wd_path = '../'

#   Function form here:
#https://medium.com/@petehouston/calculate-distance-of-two-locations-on-earth-using-python-1501b1944d97
def great_circle(lon1, lat1, lon2, lat2):
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    return 6371e3 * (acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon1-lon2)))

dat = pd.read_csv(wd_path+'/data/CA_PM_site_daily_EJ.csv')

datcarb = dat[(dat['Source']=='CARB')&(dat['parameter']=='PM2.5')]
datpurp = dat[(dat['Source']=='PurpleAir')&(dat['parameter']=='PM2.5')]

#   Names of PurpleAir sensors that start with CARB
#   pacarbnames = [pan for pan in paname if 'CARB' in pan.upper()]
pacarbnames = np.unique(datpurp['name'])
carbnames = np.unique(datcarb['name'])

#   Calculate distances between sensors if they don't already exist
if not os.path.isfile(wd_path+'/data/distmat.npy'):
    #   Distance in degrees of each CARB PA sensor to each CARB sensor
    distmat = np.nan*np.zeros((len(pacarbnames),len(carbnames)))
    for pi,pan in enumerate(pacarbnames):
        purptmp = datpurp[datpurp['name']==pan].iloc[0,:]
       
        for ci,cn in enumerate(carbnames):
            carbtmp = datcarb[datcarb['name']==cn].iloc[0,:]
            
            distmat[pi,ci] = great_circle(purptmp['lon'],purptmp['lat'],carbtmp['lon'],carbtmp['lat'])
            #   Alternative simpler but marginally less accurate distance calculation
            #   distmat[pi,ci] = np.sqrt((purptmp['lat']-carbtmp['lat'])**2 + 
            #                            ((purptmp['lon']-carbtmp['lon'])*89./111)**2)

    np.save(wd_path+'/data/distmat', distmat)

else:
    distmat = np.load(wd_path+'/data/distmat.npy')

#   Find closest CARB sensor to PA sensors
pa2carb = np.argmin(distmat,1)
mindist = np.min(distmat,1)

#   Find sensor pairs located within 100m (and corresponding time series) 
dicts = []
for i in range(len(pa2carb)):
    if mindist[i]<100:
        #   Initialize dataframes with date indices
        out = pd.DataFrame(index=np.unique(dat['date_local']))
        outRH = pd.DataFrame(index=np.unique(dat['date_local']))
        outT = pd.DataFrame(index=np.unique(dat['date_local']))

        #   Get PurpleAir samples and RH
        PAvals = datpurp[datpurp['name']==pacarbnames[i]]
        PARH = PAvals.set_index(PAvals['date_local'])['RH_mean']
        PARH.name = 'PURP'
        PAT = PAvals.set_index(PAvals['date_local'])['Temp_mean']
        PAT.name = 'PURP'
        PAvals = PAvals.set_index(PAvals['date_local'])['sample_mean']
        PAvals.name = 'PURP'

        #   Get CARB samples and RH
        CARBvals = datcarb[datcarb['name']==carbnames[pa2carb[i]]]
        CARBvals = CARBvals.set_index(CARBvals['date_local'])['sample_mean']
        CARBvals.name = 'CARB'

        #   Align data
        out = pd.merge(out,CARBvals,how='left',left_index=True,right_index=True)
        out = pd.merge(out,PAvals,how='left',left_index=True,right_index=True)
        outRH = pd.merge(outRH,PARH/100.,how='left',left_index=True,right_index=True)
        outT = pd.merge(outT,PAT/100.,how='left',left_index=True,right_index=True)

        x = out['CARB']
        y = out['PURP']
        #   Calculate purpleair using two different corrections
        correctionRH = 1./(1+(0.25*outRH['PURP']**2)/(1-outRH['PURP']))
        yRH = out['PURP']*correctionRH
        yEPA = 0.39*out['PURP'] + 0.0024*(outT['PURP']*(9./5)+32) - 0.05*outRH['PURP']*100 + 5.19
        ni = np.isnan(x+y)
        N = np.sum(~ni)

        #   Keep only pairs with more than 2 data points (or else correlations makes no sense)
        if N>2:
            #   Calculate linear fits
            p = np.polyfit(x[~ni],y[~ni],1)
            pRH = np.polyfit(x[~ni],yRH[~ni],1)
            pEPA = np.polyfit(x[~ni],yEPA[~ni],1)
            r = np.corrcoef(x[~ni],y[~ni])[0,1]
            rRH = np.corrcoef(x[~ni],yRH[~ni])[0,1]
            rEPA = np.corrcoef(x[~ni],yEPA[~ni])[0,1]
        
            #   Save output
            dicts.append({'PURP_name':pacarbnames[i],
                          'CARB_name':carbnames[pa2carb[i]],
                          'distance':distmat[i,pa2carb[i]],
                          'slope':p[0],
                          'slopeRH':pRH[0],
                          'slopeEPA':pEPA[0],
                          'N':N,
                          'r':r,
                          'rEPA':rEPA,
                          'rRH':rRH})

#   Format into arrays
slopes = np.array([d['slope'] for d in dicts])
slopesRH = np.array([d['slopeRH'] for d in dicts])
slopesEPA = np.array([d['slopeEPA'] for d in dicts])
distances = np.array([d['distance'] for d in dicts])
rs = np.array([d['r'] for d in dicts])
rsRH = np.array([d['rRH'] for d in dicts])
rsEPA = np.array([d['rEPA'] for d in dicts])
Ns = np.array([d['N'] for d in dicts])

cnames = np.array([d['CARB_name'] for d in dicts])
pnames = np.array([d['PURP_name'] for d in dicts])

badi = rs<0.2

#   Make figure
plt.figure(figsize=(17,4));
plt.subplot(1,3,1)
plt.scatter(distances[~badi],slopes[~badi],c=rs[~badi],s=Ns[~badi]/10,vmin=0.4,vmax=1)
meany = np.median(slopes[~badi])
cbar = plt.colorbar()
cbar.set_label('Cor. coefficient')
xlims = plt.gca().get_xlim()
plt.plot(xlims,[meany,meany],'--',linewidth=1,color=[0.7]*3)
plt.ylim(0,3.5)
plt.xlabel('Distance [m]')
plt.ylabel('Regression slope [PurpleAir/CARB]')
plt.title('Without correction')

plt.subplot(1,3,2)
plt.scatter(distances[~badi],slopesRH[~badi],c=rsRH[~badi],s=Ns[~badi]/10,vmin=0.4,vmax=1)
meany = np.median(slopesRH[~badi])
cbar = plt.colorbar()
cbar.set_label('Cor. coefficient')
plt.plot(xlims,[meany,meany],'--',linewidth=1,color=[0.7]*3)
plt.ylim(0,3.5)
plt.xlabel('Distance [m]')
plt.ylabel('Regression slope [PurpleAir/CARB]')
plt.title('With RH correction as in Tryner et al. (2020)')

plt.subplot(1,3,3)
plt.scatter(distances[~badi],slopesEPA[~badi],c=rsEPA[~badi],s=Ns[~badi]/10,vmin=0.4,vmax=1)
meany = np.median(slopesEPA[~badi])
cbar = plt.colorbar()
cbar.set_label('Cor. coefficient')
plt.plot(xlims,[meany,meany],'--',linewidth=1,color=[0.7]*3)
plt.ylim(0,3.5)
plt.xlabel('Distance [m]')
plt.ylabel('Regression slope [PurpleAir/CARB]')
plt.title('With EPA correction')

plt.savefig(wd_path+'/figures/FS2_PA_Corrections.pdf')

