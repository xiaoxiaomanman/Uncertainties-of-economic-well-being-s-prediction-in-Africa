from __future__ import annotations

from collections.abc import Iterable
from glob import glob
from pprint import pprint
import os
from typing import Optional

import numpy as np
import pandas as pd
import tensorflow as tf
from tqdm.auto import tqdm

import sys
sys.path.append("../")
from batchers import batcher, tfrecord_paths_utils
from preprocessing.helper import (
    analyze_tfrecord_batch,
    per_band_mean_std,
    print_analysis_results)


REQUIRED_BANDS = [
    'BLUE', 'GREEN', 'LAT', 'LON', 'NIGHTLIGHTS', 'NIR', 'RED',
    'SWIR1', 'SWIR2', 'TEMP1']

BANDS_ORDER = [
    'BLUE', 'GREEN', 'RED', 'SWIR1', 'SWIR2', 'TEMP1', 'NIR',
    'DMSP', 'VIIRS']

DHS_EXPORT_FOLDER = 'data/dhs_tfrecords_raw'
DHSNL_EXPORT_FOLDER = 'data/dhsnl_tfrecords_raw'
LSMS_EXPORT_FOLDER = 'data/lsms_tfrecords_raw'

DHS_PROCESSED_FOLDER = 'data/dhs_tfrecords'
DHSNL_PROCESSED_FOLDER = 'data/dhsnl_tfrecords'
LSMS_PROCESSED_FOLDER = 'data/lsms_tfrecords'


def validate_individual_tfrecords(tfrecord_paths: Iterable[str],
                                  csv_path: str,
                                  label_name: Optional[str] = None) -> None:
    '''
    Args
    - tfrecord_paths: list of str, paths to individual TFRecord files in the same order as in the CSV
                      str列表，指向单个TFRecord文件的路径的顺序与CSV中的顺序相同
    - csv_path: str, path to CSV file with columns ['lat', 'lon', 'wealthpooled', 'year']
    '''
    df = pd.read_csv(csv_path, float_precision='high', index_col=False)
    iter_init, batch_op = batcher.Batcher(
        tfrecord_files=tfrecord_paths,
        label_name=label_name,
        ls_bands=None,
        nl_band=None,
        batch_size=128,
        shuffle=False,
        augment=False,
        clipneg=False,
        normalize=None).get_batch()

    locs, years = [], []
    if label_name is not None:
        labels = []

    num_processed = 0
    with tf.Session() as sess:
        sess.run(iter_init)
        while True:
            try:
                if label_name is not None:
                    batch_np = sess.run((batch_op['locs'], batch_op['years'], batch_op['labels']))
                    labels.append(batch_np[2])
                else:
                    batch_np = sess.run((batch_op['locs'], batch_op['years']))
                locs.append(batch_np[0])
                years.append(batch_np[1])
                num_processed += len(batch_np[0])
                print(f'\rProcessed {num_processed} images', end='')
            except tf.errors.OutOfRangeError:
                break
    print()

    locs = np.concatenate(locs)
    years = np.concatenate(years)
    # print(locs)
    # print(df[['lat', 'lon']].to_numpy(dtype=np.float32))

    assert (locs == df[['lat', 'lon']].to_numpy(dtype=np.float32)).all()
    assert (years == df['year'].to_numpy(dtype=np.float32)).all()
    if label_name is not None:
        labels = np.concatenate(labels)
        assert (labels == df['wealthpooled'].to_numpy(dtype=np.float32)).all()


validate_individual_tfrecords(
    tfrecord_paths=tfrecord_paths_utils.dhs(),
    csv_path='/data/zzw/africa_poverty_clean-main/data/dhs_clusters.csv',
    label_name='wealthpooled')

