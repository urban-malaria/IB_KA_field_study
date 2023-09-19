import os
import pandas as pd
import numpy as np
import json
import requests
import time
from dtk.tools.demographics.DemographicsGeneratorConcern import WorldBankBirthRateConcern, EquilibriumAgeDistributionConcern, DefaultIndividualAttributesConcern
from dtk.tools.demographics.DemographicsGenerator import DemographicsGenerator
from dtk.tools.climate.ClimateGenerator import ClimateGenerator
from simtools.AssetManager.FileList import FileList
from simtools.Managers.WorkItemManager import WorkItemManager
from COMPS.Data.WorkItem import WorkItemState
from COMPS.Data import AssetCollection
from COMPS.Data import QueryCriteria
from simtools.Utilities.COMPSUtilities import download_asset_collection
from simtools.SetupParser import SetupParser
import sys
sys.path.append('../')
from load_paths import load_box_paths

home_path, data_path = load_box_paths()

location = "Ibadan"
dhs_year = "2003"
input = "2003"



if os.name == "posix":
    inputs_path = os.path.join(data_path, 'simulation_inputs')
else:
    home_path = os.path.join(home_path, 'mathematical_model')
    inputs_path = os.path.join(home_path, 'simulation_inputs', 'demographics')

if location =="Kano":
    inputs_path = os.path.join(home_path, 'simulation_inputs', 'demographics', 'Kano')
else:
    inputs_path = os.path.join(home_path, 'simulation_inputs', 'demographics', 'Ibadan')



def DHSBirthRateConcern(demo_df, country, dhs_year):
    """Extract birth rate at State level"""
    if country == "Nigeria":
        countr_code = 'NG'
    #hfca_state = demo_df['State'].item()
    #hfca_state_region = "North West" #use for Nigeria DHS 2010, 2008 and 2003  as no data at the state level
    hfca_state_region = "South West"  # use for Nigeria DHS 2010, 2008 and 2003  as no data at the state level
    #hfca_state_region = "Southwest - 1990" #use for Nigeria DHS 1990, make sure to change to geopolitical zone
    dhs_dic = requests.get(
        f"https://api.dhsprogram.com/rest/dhs/data?countryIds={countr_code}&"
        f"indicatorIds=FE_FRTR_W_CBR&breakdown=Subnational&"
        f"surveyYear={dhs_year}&"
        #f"CharacteristicLabel=..{hfca_state}&" 
        f"CharacteristicLabel={hfca_state_region}&" #use for Nigeria DHS 2010, 2008 and 2003  as no data at the state level 
        f"returnFields=CharacteristicLabel,Indicator,Region,SurveyId,Value").json()
    return dhs_dic['Data'][0]['Value']


def generate_demographics(demo_df, hfca, demo_fname, use_DHS_birth_rates=True) :
    if not SetupParser.initialized:
        SetupParser.init('HPC')

    demo_df = demo_df.loc[demo_df['Ward'] == hfca]
    #demo_df['geopode pop'] = demo_df['geopode pop']/100
    #demo_df['standard_pop'] = 2000

    br_concern = WorldBankBirthRateConcern(country="Nigeria", birthrate_year=2016)
    if use_DHS_birth_rates:
        #br_concern_country = br_concern.default_birth_rate
        br_concern.default_birth_rate = DHSBirthRateConcern(demo_df, country="Nigeria", dhs_year=dhs_year)

    chain = [
        DefaultIndividualAttributesConcern(),
        br_concern,
        EquilibriumAgeDistributionConcern(default_birth_rate=br_concern.default_birth_rate),
    ]

    current = DemographicsGenerator.from_dataframe(demo_df,
                                                   population_column_name='standard pop',
                                                   latitude_column_name='lat',
                                                   longitude_column_name='lon',
                                                   node_id_from_lat_long=False,
                                                   concerns=chain,
                                                   load_other_columns_as_attributes=True,
                                                   include_columns=['Ward', 'LGA', 'State', 'Archetype']
                                                   ) #computes both daily rate and annual rate here https://github.com/InstituteforDiseaseModeling/dtk-tools/blob/master/dtk/tools/demographics/DemographicsGeneratorConcern.py#L669
    current['Nodes'][0]['NodeID'] = 1
    with open(demo_fname, 'w') as fout :
        json.dump(current, fout, sort_keys=True,indent=4, separators=(',', ': '))


def generate_climate(demo_fname, hfca) :


    cg = ClimateGenerator(demographics_file_path=demo_fname, work_order_path='./wo.json',
                          climate_files_output_path=os.path.join(inputs_path, hfca),
                          climate_project='IDM-Nigeria',
                          start_year='2016', num_years='1')
    cg.generate_climate_files()


if __name__ == '__main__' :

    if location == "Kano":
        master_csv = os.path.join(home_path, 'Kano_ward_pop.csv')
    else:
         master_csv = os.path.join(home_path, 'Ibadan_ward_pop.csv')

    df = pd.read_csv(master_csv, encoding='latin')
    df['Ward'] = df['Ward'].str.normalize('NFKD').str.encode('ascii', errors='ignore').str.decode('utf-8')

    if input == "all":
        for hfca in df['Ward'].unique() :
            print(hfca)
            if not os.path.exists(os.path.join(inputs_path, hfca)):
                os.makedirs(os.path.join(inputs_path, hfca))
            demo_fname = os.path.join(inputs_path, hfca, '%s_demographics.json' % hfca)
            generate_demographics(df, hfca, demo_fname, True)

            if os.path.exists(os.path.join(inputs_path, hfca,  '%s_rainfall_daily_2016.bin' % hfca)) :
                continue
            generate_climate(demo_fname, hfca)

        #use this to change the names of the climate files
            for tag in ['air_temperature', 'rainfall', 'relative_humidity'] :
                os.replace(os.path.join(inputs_path, hfca, 'Nigeria_30arcsec_%s_daily.bin' % tag),
                       os.path.join(inputs_path, hfca, '%s_%s_daily_2016.bin' % (hfca, tag)))
                os.replace(os.path.join(inputs_path, hfca, 'Nigeria_30arcsec_%s_daily.bin.json' % tag),
                       os.path.join(inputs_path, hfca, '%s_%s_daily_2016.bin.json' % (hfca, tag)))
                my_file = os.path.join(inputs_path, hfca, 'Nigeria_2.5arcmin_demographics.json')
                if os.path.exists(os.path.join(inputs_path, hfca, 'Nigeria_2.5arcmin_demographics.json')):
                    os.remove(os.path.join(inputs_path, hfca, 'Nigeria_2.5arcmin_demographics.json'))

    else:
        for hfca in df['Ward'].unique():
            print(hfca)
            if not os.path.exists(os.path.join(inputs_path, hfca)):
                os.makedirs(os.path.join(inputs_path, hfca))
            demo_fname = os.path.join(inputs_path, hfca, '%s_demographics_%s.json' % (hfca, dhs_year))
            generate_demographics(df, hfca, demo_fname, True)
