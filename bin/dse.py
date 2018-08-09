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
import traceback
import re

import matplotlib
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.ticker as ticker
import os, sys
import math

from util import *
from task import *

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

def matchRange(range):
    if "(" in range:
        patterns = re.findall("\([^\(^)]*,[^\(^)]*,[^\(^)]*\)",range)
        for pattern in patterns:
            range = range.replace(pattern, "irange" + pattern)
    return range
def matchCondition(range):
    if "|" in range:
        r, cond = range.split("|")
        range = "filter(lambda p: {}, {})".format(cond, r)
    return range
def matchParam(range):
    if ("<" in range):
        patterns = re.findall("\<[\w\s]*\>",range)
        for pattern in patterns:
            range = range.replace(pattern, pattern.replace("<","params[\"").replace(">","\"]"))
        range = "lambda params: " + range
    return range
def matchEmpty(range, value):
    if range == "":
        range = "[{}]".format(value)
    return range

def parseParams(app):
    params = OrderedDict()
    path = '{0}{1}.scala'.format(APP_DIR, app)

    with open(path, 'r') as f:
        for line in f:
            if line.strip().startswith("//"):
                continue
            if "param " in line:
                line = line.split("#")[0]
                head, tail = line.split("//")
                tail = tail.split("param")[1]
                param, value = head.split("val")[1].split("=")
                param = param.strip()
                value = value.strip()
                range = tail.strip()
                range = matchEmpty(range, value)
                range = matchRange(range)
                range = matchCondition(range)
                range = matchParam(range)
                assign = "params[\"{}\"] = {}".format(param, range)
                try:
                  exec(assign)
                except Exception as e:
                  print(assign)
                  traceback.print_exc()
                  exit(-1)

    print("parsed params:")
    for param in params:
        print("{} = {}".format(param, params[param]))
    return params

def runExp():
    for app in opts.apps:
    
        if opts.dse:
            params = parseParams(app)
        else:
            params = OrderedDict()
        space = dse(app, args, params)
        print("space size: {}".format(space))

