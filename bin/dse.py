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

def addArgs(app, args):
    if opts.summary:
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
    print('{}{} params=[{}]{}'.format(bcolors.UNDERLINE, app,
        ' '.join(['{}={}'.format(p,params[p]) for p in params]), bcolors.ENDC))
    target(app, args, params)
    return 1

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

def parseParams(app):
    params = OrderedDict()
    path = '{0}{1}.scala'.format(APP_DIR, app)

    with open(path, 'r') as f:
        for line in f:
            if "param" in line:
                line = line.split("#")[0]
                head, tail = line.split("//")
                tail = tail.split("param")[1]
                param, value = head.split("val")[1].split("=")
                param = param.strip()
                value = value.strip()
                range = tail.strip()
                if range == "":
                    range = "[{}]".format(value)
                if "(" in range:
                    range = range.replace("(", "irange(")
                if "|" in range:
                    r, cond = range.split("|")
                    range = "filter(lambda p: {}, {})".format(cond, r)
                if ("<" in range):
                    range = range.replace("<", "params[\"").replace(">", "\"]")
                    range = "lambda params: " + range
                assign = "params[\"{}\"] = {}".format(param, range)
                exec(assign)

    print("parsed params:")
    for param in params:
        print("{} = {}".format(param, params[param]))
    return params

def runExp():
    for app in opts.apps:
    
        addArgs(app, args) # TODO: remove?

        params = parseParams(app)
        space = dse(app, args, params)
        print("space size: {}".format(space))

