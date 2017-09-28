
from os import listdir
from os.path import isfile, isdir, join, splitext, basename, dirname 
from collections import OrderedDict
import os
import argparse
import subprocess
import commands
import time
import pickle
import signal
import psutil
import shutil
import numpy as np
import types
import csv

import matplotlib
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.ticker as ticker
import os, sys
import math

from util import *

lanes = 16
bankSize = 32 * 1024 / 4
max_bw = 12.8*4

summary_headers = ['App', 'cycle', 'lavgbw', 'savgbw', 'pcuUtil', 'mcuUtil', 'scuUtil', 'ocuUtil', 'mcUtil', 'ocuUtil', 'mcUtil',
    'slinkUtil', 'vlinkUtil', 'clinkUtil', 'totalRegUtil', 'totalFUUtil', 'totalSBufUtil', 
    'totalVBufUtil']

def plot():
    cmap = matplotlib.cm.get_cmap('Oranges')
    for app in apps:
        if app==opts.app or opts.app=='ALL':
            for args in summary[app]:
                fig, ax = plt.subplots()
                for fullname in summary[app][args]:
                    tileSize = int(fullname.split('tileSize_')[1].split('_')[0])
                    used, total = summary[app][args][fullname]['pcuUtil']
                    pcu = futil(used, total)
                    used, total = summary[app][args][fullname]['mcuUtil']
                    mcu = futil(used, total)
                    used, total = summary[app][args][fullname]['mcUtil']
                    mc = futil(used, total)
                    lavgbw = summary[app][args][fullname]['lavgbw']
                    cycle = summary[app][args][fullname]['cycle']
                    # sp = ax.scatter(pcu, cycle, c = color_spd, s = dotSize, marker = marker_pd, edgecolors='none')
                    size = mcu * 200
                    # size = mc * 100
                    # size = mc * 100
                    # size = lavgbw
                    # size = tileSize 
                    bw = (lavgbw + 3) / 15
                    color = cmap(bw)
                    # color = cmap(max_bw - lavgbw/max_bw)
                    cycle = float(cycle) / (7.5 * (10 ** 7))
                    if tileSize==16:
                        ec = 'none'
                    else:
                        ec = 'k'
                    sp = ax.scatter(pcu * 100, cycle, c=color, s=size, edgecolors=ec)
                plot_path = '{}/apps/{}_{}.png'.format(SPATIAL_HOME,app, args)
                plt.grid(True)
                plt.xlabel('PCU Utilization (%)')
                plt.ylabel('Runtime')
                ax.set_ylim(0,1)
                fig.set_size_inches(6,4)
                plt.gcf().subplots_adjust(bottom=0.15)
                plt.savefig(plot_path, format='png', dpi=900)
                print('Generate {}'.format(plot_path))

def sargs(args):
    return '_'.join([str(a) for a in args])

def getUtil(line):
    used,total=line.split("Util(")[1].split(')')[0].split(',')
    used = int(used)
    total = int(total)
    return (used, total)

def cycleOf(app):
    if app in cycle_cache:
        return cycle_cache[app]
    log = logs(app, "RUN_SIMULATION")
    line = grep(log, ["Design ran for"])
    if line is None:
        return None
    else:
        cycle = int(line.split("Design ran for ")[1].split(" ")[0])
        cycle_cache[app] = cycle
        return cycle

def avgbw(app, args, params):
    lword = 0
    sword = 0
    if 'TPCHQ6' in app:
        N, = args
        lword += N * 4 # load 4 data stucture
    elif 'GDA' in app:
        R = args[0]
        C = 96
        lword += C * 2 # load mu0, mu1
        lword += R # load y
        lword += R * C # load x
        sword += C * C # store sigma
    elif 'BlackScholes' in app:
        N = args[0]
        lword += N * 6 # load 6 data stucture
        sword += N # store optpriceBlk
    elif 'GEMM_Blocked' in app:
        dim = 512
        tileSize = params['tileSize']
        i_tileSize = params['i_tileSize']
        # load b_sram
        lword += (tileSize * tileSize) * (dim / tileSize) * (dim / tileSize) * (dim / i_tileSize)
        # load a_sram
        lword += (tileSize) * (i_tileSize) * (dim / tileSize) * (dim / tileSize) * (dim / i_tileSize)
        # store c_col
        sword += i_tileSize * tileSize * (dim / tileSize) * (dim / i_tileSize)
    elif 'Kmeans_plasticine' in app:
        iters = 1
        D = 96
        K = 20
        N = 6144
        dim = 96
        tileSize = params['tileSize']
        BN = tileSize
        BD = dim
        lword += K * D # load orgCts
        lword += (BN * BD) * (N / BN) * iters # pts
        sword += (K * D) # store centroids
    elif 'PageRank_plasticine' in app:
        # lword += tileSize * 3 * (NP / tileSize) * iters # load initPR, edgesId, edgesLen
        # lword += numEdges * 2 * tileSize * (NP / tileSize) * iters # load edges, counts
        # lword += num2gather * tileSize * (NP / tileSize) * iters # gather
        # sword += tileSize * (NP / tileSize)  * iters # store currentPR
        pass
    elif 'SPMV_CRS' in app:
        MAGIC = 15162342 
        # lword += (tileSize + 1) * (N / tileSize) # load rowid_sram 
        # lword += (stop_id - start_id) * tileSize * 
        simlog = logs(app, "RUN_SIMULATION")
        with open(simlog, 'rb') as f:
            for line in f:
                if "Sum of size (#bursts) of dense loads:" in line:
                    burst = int(line.split(":")[1].split("(")[1].split(")")[0])
                    lword += (burst-MAGIC) * 16
                if "Sum of size (#bursts) of dense stores:" in line:
                    burst = int(line.split(":")[1].split("(")[1].split(")")[0])
                    sword += (burst-MAGIC) * 16
                if "Sparse reads: " in line:
                    lword += int(line.split("(")[1].split(")")[0])  
                if "Sparse write: " in line:
                    sword += int(line.split("(")[1].split(")")[0])  
        pass

    lbyte = lword * 4
    sbyte = sword * 4
    second = float(cycleOf(app)) / (10 ** 9)
    lgbps = lbyte / second / (10 ** 9)
    sgbps = sbyte / second / (10 ** 9)
    return (lgbps, sgbps)

def load_summary():
    global summary
    if not os.path.exists(SUMMARY_PATH):
        print("New app summary at {}".format(SUMMARY_PATH))
        summary = OrderedDict()
    else:
        summary = pickle.load(open(SUMMARY_PATH, 'rb'))
    for app in apps:
        if app not in summary:
            summary[app] = OrderedDict()

def summarize(app, args, params):
    global summary
    fullname = getFullName(app, args, params)
    # for p in passes:
        # print(p, progress(fullname,p))
    if success(fullname, "MAP_PIR") and success(fullname, "RUN_SIMULATION"):
        if fullname not in summary[app][sargs(args)]:
            print('summarize:{}'.format(fullname))
            summary[app][sargs(args)][fullname] = OrderedDict()
            table = summary[app][sargs(args)][fullname]
            cycle = cycleOf(fullname) 
            table['cycle'] = cycle
            table['lavgbw'], table['savgbw'] = avgbw(fullname, args, params)
            reslog = logs(fullname, "Utilization")
            with open(reslog, 'rb') as f:
                for line in f:
                    for header in summary_headers:
                        if header in line:
                            table[header] = getUtil(line)

def futil(used,total):
   return round(float(used) / float(total), 3)

def bestSummary():
    best = OrderedDict()
    best['TPCHQ6'] = ('TPCHQ6_96000_outerPar_8_tileSize_2000', '96000')
    best['BlackScholes'] = ('BlackScholes_1966080_outerPar_1_tileSize_32768', '1966080')
    best['Kmeans_plasticine'] = ('Kmeans_plasticine_1_6144_tileSize_512', '1_6144')
    best['SPMV_CRS'] = ('SPMV_CRS__tileSize_208_tile_par_2_pt_par_1', '')
    best['GEMM_Blocked'] = ('GEMM_Blocked__tileSize_256_i_tileSize_256_loop_jj_1_loop_ii_1_loop_kk_1_loop_i_4_loop_k_3', '')
    best['GDA'] = ('GDA_38400_tileSize_1600_outerPar_6_midPar_1', '38400')

    headers = ['App', 'lavgbw', 'savgbw', 'pcuUtil', 'mcuUtil', 'mcUtil', 'totalFUUtil']
    with open(BEST_SUMMARY_CSV_PATH, 'w') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=headers)
        writer.writeheader()
        for app in best:
            dict = {'App':app }
            for header in headers:
                fullname, args = best[app]
                if header == 'App':
                    passName
                elif header in ['lavgbw', 'savgbw']:
                    dict[header] = round(summary[app][args][fullname][header] / max_bw * 100, 3)
                elif header not in ['cycle']:
                    used, total = summary[app][args][fullname][header]
                    dict[header] = futil(used, total) * 100
                else:
                    dict[header] = summary[app][args][fullname][header]
            writer.writerow(dict)

def summaryToCsv():
    global summary

    pickle.dump(summary, open(SUMMARY_PATH, 'wb'))

    with open(SUMMARY_CSV_PATH, 'w') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=summary_headers)
        writer.writeheader()
        for app in summary:
            for arg in summary[app]:
                for fullname in summary[app][arg]: 
                    dict = {'App':fullname}
                    for header in summary[app][arg][fullname]:
                        if header not in ['App', 'cycle', 'lavgbw', 'savgbw']:
                            used, total = summary[app][arg][fullname][header]
                            dict[header] = futil(used, total)
                        elif header in ['lavgbw', 'savgbw']:
                            dict[header] = round(summary[app][arg][fullname][header], 3)
                        else:
                            dict[header] = summary[app][arg][fullname][header]
                    writer.writerow(dict)

