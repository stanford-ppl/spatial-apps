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
from task import *
from stats import *

def addArgs(app, args):
    if opts.summarize:
        summary = opts.summary
        if sargs(args) not in summary[app]: 
            summary[app][sargs(args)] = OrderedDict()

def dse(app, args, params):
    # print(params)
    for param in params:
        # print('  param:' + param + ' = ' + str(params[param]))
        if type(params[param]) is list:
            if len(params[param])==0:
                return 0
            newParams = params.copy()
            cnt = 0
            if opts.single:
                params[param] = [params[param][0]]
            for val in params[param]:
                newParams[param] = val
                cnt += dse(app, args, newParams)
            return cnt
        elif type(params[param]) is types.LambdaType:
            newParams = params.copy()
            newParams[param] = params[param](params)
            return dse(app, args, newParams)
        else:
            continue
    print('{}{} args={} and params=[{}]{}'.format(bcolors.UNDERLINE, app, str(args),
        ' '.join(['{}={}'.format(p,params[p]) for p in params]), bcolors.ENDC))
    fullname = getFullName(app, args, params)
    print(fullname)
    target(app, args, params)
    return 1

# tucson
def TPCHQ6():
    app = 'TPCHQ6'
    space = 0
    N = 96000
    args = [N]
    addArgs(app, args)

    # FINISHED
    # params = OrderedDict()
    # params['outerPar'] = irange(1, 10, 2) 
    # params['tileSize'] = lambda params: irange(3000, min(bankSize, N/params['outerPar']/4), 3000)
    # space += dse(app, args, params)

    # Finished
    params = OrderedDict()
    params['outerPar'] = irange(6, 10, 2) 
    params['tileSize'] = lambda params: irange(400, min(bankSize, N/params['outerPar']/4), 1600)
    space += dse(app, args, params)

    # Finished
    # params = OrderedDict()
    # params['outerPar'] = irange(2, 10, 2) 
    # params['tileSize'] = lambda params: irange(16, min(bankSize, N/params['outerPar']/200), 16*2)
    # space += dse(app, args, params)

    print('{} space: {}'.format(app, space))

# tucson
def GDA():
    app = 'GDA'
    space = 0

    # R = 192000
    # MAXC = 80
    # args = [R]
    # addArgs(app, args)

    # # Finished 
    # params = OrderedDict()
    # # params['tileSize'] = irange(16, 16*5, 16) 
    # params['tileSize'] = irange(16, bankSize*lanes/MAXC, 512) 
    # params['outerPar'] = lambda params: irange(1, min(10, R/params['tileSize']), 3) 
    # params['midPar'] = lambda params: irange(1, min(8, params['tileSize']), 3) 
    # space += dse(app, args, params)

    # # Finished 
    # params = OrderedDict()
    # params['tileSize'] = irange(16, 16*3, 16*2) 
    # params['outerPar'] = lambda params: irange(2, min(6, R/params['tileSize']), 2) 
    # params['midPar'] = lambda params: irange(2, min(6, params['tileSize']), 2) 
    # space += dse(app, args, params)

    # # portland
    # R = 19200
    # MAXC = 80
    # args = [R]
    # addArgs(app, args)

    # # Finished 
    # params = OrderedDict()
    # params['tileSize'] = irange(1024, bankSize*lanes/MAXC, 1024) 
    # params['outerPar'] = lambda params: irange(1, min(10, R/params['tileSize']), 3) 
    # params['midPar'] = lambda params: irange(1, min(8, params['tileSize']), 3) 
    # space += dse(app, args, params)

    # # Finished 
    # params = OrderedDict()
    # params['tileSize'] = irange(1024, bankSize*lanes/MAXC, 1024) 
    # params['outerPar'] = lambda params: irange(2, min(6, R/params['tileSize']), 2) 
    # params['midPar'] = lambda params: irange(2, min(6, params['tileSize']), 2) 
    # space += dse(app, args, params)

    R = 1024*10
    MAXC = 96
    args = [R]
    addArgs(app, args)
    params = OrderedDict()
    params['tileSize'] = irange(512, bankSize*lanes/MAXC, 512) 
    params['outerPar'] = 6
    space += dse(app, args, params)
    print('{} space: {}'.format(app, space))

# london
def GEMM_Blocked():
    app = 'GEMM_Blocked'
    space = 0
    dim = 512
    args = [dim]
    addArgs(app, args)

    #best['GEMM_Blocked'] = ('GEMM_Blocked__tileSize_256_i_tileSize_256_loop_jj_1_loop_ii_1_loop_kk_1_loop_i_4_loop_k_3', '')

    # params = OrderedDict()
    # params['tileSize']   = [16, 256] # irange(16, dim, 16)
    # params['i_tileSize'] = [16, 256] # irange(16, dim, 16)
    # params['loop_jj']    = lambda params: irange(1 , min(dim/params['tileSize']  , 2), 1)
    # params['loop_ii']    = lambda params: irange(1 , min(dim/params['i_tileSize'], 2), 1)
    # params['loop_kk']    = lambda params: irange(1 , min(dim/params['tileSize']  , 2), 1)
    # params['loop_i']     = lambda params: irange(1 , min(params['i_tileSize']    , 2), 1)
    # params['loop_k']     = lambda params: irange(1 , min(params['tileSize']      , 2), 1)
    # space += dse(app, args, params)

    params = OrderedDict()
    params['tileSize']   = [256] # irange(16, dim, 16)
    params['i_tileSize'] = [256] # irange(16, dim, 16)
    params['loop_jj']    = 1 
    params['loop_ii']    = 1
    params['loop_kk']    = 1
    params['loop_i']     = [3,4,5]
    params['loop_k']     = [3,4]
    space += dse(app, args, params)
    print('{} space: {}'.format(app, space))

# london
def BlackScholes():
    app = 'BlackScholes'
    space = 0
    N = 960000
    args = [N]
    addArgs(app, args)

    # Finished
    # params = OrderedDict()
    # params['outerPar'] = irange(1, 4, 1) 
    # params['tileSize'] = lambda params: irange(1024, min(bankSize, N/params['outerPar']), 1024)
    # space += dse(app, args, params)

    params = OrderedDict()
    params['outerPar'] = 1 
    # params['tileSize'] = lambda params: irange(1024*16, min(scratchpadCapacity,N/params['outerPar']),1024*16)
    params['tileSize'] = scratchpadCapacity 
    space += dse(app, args, params)

    print('{} space: {}'.format(app, space))

def PageRank_plasticine():
    app = 'PageRank_plasticine'
    space = 0
    iters = 1
    NP = 491520 # 2048 in simulation
    damp = 0.125
    args = [iters, NP, damp]
    addArgs(app, args)

    params = OrderedDict()
    maxTile = min(bankSize*lanes,NP)
    params['tileSize'] = [16, 1024, 1024*6, maxTile/4, maxTile/2, maxTile] 
    space += dse(app, args, params)

    print('{} space: {}'.format(app, space))

# london
def Kmeans_plasticine():
    app = 'Kmeans_plasticine'
    space = 0
    iter = 1
    N = 6144
    dim = 96
    args = [1, N]
    addArgs(app, args)

    # Finished 
    params = OrderedDict()
    params['tileSize'] = irange(16, 16, 1) + irange(128, 512, 128) + irange(1024,min(bankSize*lanes/dim, N), 1024)
    space += dse(app, args, params)

    print('{} space: {}'.format(app, space))

def Kmeans23():
    app = 'Kmeans23'
    space = 0
    iter = 1
    N = 320
    dim = 32
    args = [1, N]
    addArgs(app, args)

    # Finished 
    params = OrderedDict()
    params['P3'] = [2, 4, 6, 8]
    params['P4'] = [4, 8, 16]
    space += dse(app, args, params)

    print('{} space: {}'.format(app, space))

# tucson
def SPMV_CRS():
    app = 'SPMV_CRS'
    space = 0
    N = 494
    args = []
    addArgs(app, args)

    params = OrderedDict()
    maxTile = min(bankSize*lanes, N)
    # Finished
    params['tileSize'] = irange(16, maxTile, 64) 
    params['tile_par'] = lambda params: irange(1, min(5, N/params['tileSize']), 1)
    params['pt_par'] = lambda params: irange(1, min(5, params['tileSize']), 1)
    space += dse(app, args, params)

    print('{} space: {}'.format(app, space))

def runExp():
    #################################################################################################################
    ###                                                ASPLOS 2018                                           ########
    #################################################################################################################

    ### TO RUN        ######################################################################################################
    if "TPCHQ6" in opts.apps:
        TPCHQ6()
    if "GDA" in opts.apps:
        GDA()
    if "GEMM_Blocked" in opts.apps:
        GEMM_Blocked()
    if "BlackScholes" in opts.apps:
        BlackScholes()
    if "Kmeans_plasticine" in opts.apps:
        Kmeans_plasticine()
    if "Kmeans23" in opts.apps:
        Kmeans23()
    if "PageRank_plasticine" in opts.apps:
        PageRank_plasticine()
    if "SPMV_CRS" in opts.apps:
        SPMV_CRS()

def target(app, args, params):
    if opts.run:
        launchJob(app, args, params)
    if opts.status:
        status(app, args, params)
    if opts.summarize:
        summarize(app, args, params)
