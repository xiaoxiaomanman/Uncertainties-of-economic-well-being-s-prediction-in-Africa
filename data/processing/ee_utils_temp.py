from __future__ import annotations

from collections.abc import Mapping
from typing import Any, Optional

import ee
import pandas as pd
import time


def surveyyear_to_range(survey_year: int) -> tuple[str, str]:
    '''Returns the start and end dates for filtering satellite images for a year.

    Args
    - survey_year: int, year that survey was started

    Returns
    - start_date: str, start date for filtering satellite images (yyyy-mm-dd)
    - end_date: str, end date for filtering satellite images (yyyy-mm-dd)
    '''
    start_date = str(survey_year) + '-1-1'
    end_date = str(survey_year + 1) + '-1-1'
    return start_date, end_date


# def df_to_fc(country: str,
#              year: int,
#              sub_df: pd.DataFrame,
#              imageCollection: ee.ImageCollection,
#              # lat_colname: str = 'lat',
#              # lon_colname: str = 'lon',
#              start_date: Optional[str] = None,
#              end_date: Optional[str] = None,
#              ):
#     '''Create a ee.FeatureCollection from a pd.DataFrame.
#
#     Args
#     - csv_path: str, path to CSV file that includes at least two columns for
#         latitude and longitude coordinates
#     - lat_colname: str, name of latitude column
#     - lon_colname: str, name of longitude column
#
#     Returns: ee.FeatureCollection, contains one feature per row in the CSV file
#     '''
#     # convert values to Python native types
#     # see https://stackoverflow.com/a/47424340
#     sub_df = sub_df.astype('object')
#
#     ee_features = []
#     for i in range(len(sub_df)):
#         props = sub_df.iloc[i].to_dict()
#
#         # oddly EE wants (lon, lat) instead of (lat, lon)
#         geometry = ee.Geometry.Point([
#             props['lon'],
#             props['lat'],
#         ])
#         ic = imageCollection.filterBounds(geometry).filterDate(start_date,end_date).select('C')
#         max_temp = ic.reduce(ee.Reducer.max())
#         ee_feat  = ee.Feature(geometry, {'maximum_2m_air_temperature': max_temp.get('C'),
#                                                            'lonlat': geometry.coordinates(),
#                                                            'country': country, 'year': year})
#
#         ee_features.append(ee_feat)
#
#     return ee.FeatureCollection(ee_features)

def maxTempCal(image):
    mt_2m = image.reduceRegion(**{
        'reducer':ee.Reducer.mean(),
        'geometry':geometry,
        'bestEffort':True
    })
    # 转换为摄氏度
    mt_cel = ee.Number(mt_2m.get('mean_2m_air_temperature'))\
        .subtract(273.15)
    # 增加新的属性
    return image.set({'mean_air_temp':mt_cel})

def maxTempCal(image):
    mt_2m = image.reduceRegion(**{
        'reducer':ee.Reducer.mean(),
        'geometry':roi,
        'bestEffort':True
    })
    # 转换为摄氏度
    mt_cel = ee.Number(mt_2m.get('maximum_2m_air_temperature'))\
        .subtract(273.15)
    # 增加新的属性
    return image.set({'max_air_temp':mt_cel})

def temp(country: str,
             year: int,
             sub_df: pd.DataFrame,
             imageCollection: ee.ImageCollection,
             # lat_colname: str = 'lat',
             # lon_colname: str = 'lon',
             start_date: Optional[str] = None,
             end_date: Optional[str] = None,
             ):
    '''Create a ee.FeatureCollection from a pd.DataFrame.

    Args
    - csv_path: str, path to CSV file that includes at least two columns for
        latitude and longitude coordinates
    - lat_colname: str, name of latitude column
    - lon_colname: str, name of longitude column

    Returns: ee.FeatureCollection, contains one feature per row in the CSV file
    '''
    # convert values to Python native types
    # see https://stackoverflow.com/a/47424340
    sub_df = sub_df.astype('object')

    ee_features = []
    for i in range(len(sub_df)):
        props = sub_df.iloc[i].to_dict()

        # oddly EE wants (lon, lat) instead of (lat, lon)
        geometry = ee.Geometry.Point([
            props['lon'],
            props['lat'],
        ])

        Map = geemap.Map()
        Map.addLayer(ee.Image().paint(roi, 0, 2), {'palette': 'darkred'}, 'plateau')
        Map.centerObject(roi)
        Map
        era5_mt = imageCollection.filterDate(start_date, end_date).filterBounds(geometry)
        max_airTemp = era5_mt.map(meanTempCal).aggregate_array('mean_air_temp').getInfo()

        max_temp = ic.map(lambda img: ee.Image.reduceRegion(reducer = ee.Reducer.max(),
                                                            geometry = geometry))
        ee_feat  = ee.Feature(geometry, {'maximum_2m_air_temperature': max_temp.get('C'),
                                                           'lonlat': geometry.coordinates(),
                                                           'country': country, 'year': year})
        ee_features.append(ee_feat)

    return ee.FeatureCollection(ee_features)


def get_loc_temp(img: ee.Image, geometry, country, year):
    max_temp = ee.Image.reduceRegion(img,ee.Reducer.max(),geometry,country,year)
    return ee.Feature(geometry,{'maximum_2m_air_temperature': max_temp.get('C'),
                                'lonlat': geometry.coordinates(),
                                'country': country, 'year': year})

def get_loc_temp(img: ee.ImageCollection, geometry, country, year):
    max_temp = img.filterBounds(geometry)
    return ee.Feature(geometry,{'maximum_2m_air_temperature': max_temp.get('C'),
                                'lonlat': geometry.coordinates(),
                                'country': country, 'year': year})

def csvexporter(collection: ee.ImageCollection, export: str, prefix: str,
                fname: str, selectors: Optional[ee.List] = None,
                dropselectors: Optional[ee.List] = None,
                bucket: Optional[str] = None) -> ee.batch.Task:
    '''Creates and starts a task to export a ee.FeatureCollection to a TFRecord
    file in Google Drive or Google Cloud Storage (GCS).

    GCS:   gs://bucket/prefix/fname.tfrecord
    Drive: prefix/fname.tfrecord

    Args
    - collection: ee.FeatureCollection
    - export: str, 'drive' for Drive, 'gcs' for GCS
    - prefix: str, folder name in Drive or GCS to export to, no trailing '/'
    - fname: str, filename
    - selectors: None or ee.List of str, names of properties to include in
        output, set to None to include all properties
    - dropselectors: None or ee.List of str, names of properties to exclude
    - bucket: None or str, name of GCS bucket, only used if export=='gcs'

    Returns
    - task: ee.batch.Task
    '''
    if dropselectors is not None:
        if selectors is None:
            selectors = collection.first().propertyNames()

        selectors = selectors.removeAll(dropselectors)

    if export == 'gcs':
        task = ee.batch.Export.table.toCloudStorage(
            collection=collection,
            description=fname,
            bucket=bucket,
            fileNamePrefix=f'{prefix}/{fname}',
            fileFormat='CSV',
            selectors=selectors)

    # elif export == 'drive':
    #     task = ee.batch.Export.table.toDrive(
    #         collection=collection,
    #         description=fname,
    #         folder=prefix,
    #         fileNamePrefix=fname,
    #         fileFormat='CSV',
    #         selectors=selectors)

    elif export == 'drive':
        task = ee.batch.Export.table.toDrive(
            collection=collection,
            description=fname,
            folder=prefix,
            fileNamePrefix=fname,
            fileFormat='CSV')

    task.start()
    return task


def C(img: ee.Image):
    return img.addBands(img.subtract(273.15).rename("C"))


def ERA5(name, band, start_date, end_date, point):
    imageCollection = ee.ImageCollection(name).select(band).map(C)
    temp = ee.ImageCollection(name).filterBounds(point).filterDate(start_date, end_date)


class TEMP:
    def __init__(self, filterpoly: ee.Geometry, start_date: str,
                 end_date: str) -> None:
        '''
        Args
        - filterpoly: ee.Geometry
        - start_date: str, string representation of start date
        - end_date: str, string representation of end date
        '''
        self.filterpoly = filterpoly
        self.start_date = start_date
        self.end_date = end_date

        # self.ERA5 = self.init_coll('ECMWF/ERA5/DAILY').map(self.rename_l8).map(self.rescale_l8)
        self.ERA5 = self.init_coll('ECMWF/ERA5/DAILY').select('maximum_2m_air_temperature').map(self.C)

    def init_coll(self, name: str) -> ee.ImageCollection:
        '''
        Creates a ee.ImageCollection containing images of desired points
        between the desired start and end dates.

        Args
        - name: str, name of collection

        Returns: ee.ImageCollection
        '''
        return (ee.ImageCollection(name)
                .filterBounds(self.filterpoly)
                .filterDate(self.start_date, self.end_date))
