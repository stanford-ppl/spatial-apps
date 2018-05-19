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

SPATIAL_HOME = os.environ['SPATIAL_HOME']
PIR_HOME = os.environ['PIR_HOME']

passes=["GEN_PIR","FIT_PIR","GEN_CHISEL","MAKE_VCS","MAP_PIR","RUN_SIMULATION"]
apps = ['DotProduct', 'TPCHQ6', 'GDA', 'BlackScholes', 'Kmeans_plasticine', 'Kmeans23', 'PageRank_plasticine', 'SPMV_CRS', 'GEMM_Blocked']

APP_DIR='{}/apps/src/'.format(SPATIAL_HOME)
JOB_PATH="{}/gen/job_list.pickle".format(SPATIAL_HOME)
SUMMARY_PATH="{}/apps/summary.pickle".format(SPATIAL_HOME)
SUMMARY_CSV_PATH="{}/apps/summary.csv".format(SPATIAL_HOME)
BEST_SUMMARY_CSV_PATH="{}/apps/best.csv".format(SPATIAL_HOME)

cycle_cache = {}

dependency = {
        "GEN_PIR":[],
        "FIT_PIR":["GEN_PIR"],
        "GEN_CHISEL":[],
        "MAKE_VCS":["GEN_CHISEL"],
        "MAP_PIR":["GEN_PIR"],
        "RUN_SIMUlATION":["MAKE_VCS"]
        }

class bcolors:
    HEADER    = '\033[95m'
    OKBLUE    = '\033[94m'
    OKGREEN   = '\033[92m'
    WARNING   = '\033[93m'
    FAIL      = '\033[91m'
    ENDC      = '\033[0m'
    BOLD      = '\033[1m'
    UNDERLINE = '\033[4m'
    BLUE      = '\033[0;34m'
    YELLOW    = '\033[1;33m'
    GREEN     = '\033[0;32m'
    RED       = '\033[0;31m'
    CYAN      = '\033[0;36m'
    ORANGE    = '\033[038;5;202m'
    NC        = '\033[0m'

colors = {
        "SUCCESS" : bcolors.GREEN,
        "KILLED"  : bcolors.RED,
        "FAILED"  : bcolors.RED,
        "RUNNING" : bcolors.ORANGE,
        "NOTRUN"  : bcolors.YELLOW,
        }

def rm(path):
    if os.path.exists(path):
        # print("rm {}".format(path))
        os.remove(path)

def cat(path):
    with open(path, "r") as f:
        for line in f:
            sys.stdout.write(line)

def vim(path):
    subprocess.call("vim {}".format(path), shell=True)

def mkdir(path):
    if not os.path.exists(path):
        os.makedirs(path)

def grep(path, patterns):
    found = []
    if type(patterns)!=list:
        patterns = [patterns]
    with open(path, 'r') as f:
        for line in f:
            for pattern in patterns:
                if pattern in line:
                    found.append(line)
    return found

def openfile(path, flag):
    try:
        return open(path, flag)
    except e:
        time.sleep(1)
        return openfile(path, flat)
        
def check_pid(pid):        
    """ Check For the existence of a unix pid. """
    # return os.path.exists("/proc/{}".format(pid))
    try:
        os.kill(pid, 0)
    except OSError:
        return False
    else:
        return True

def checkProcess():
    if not os.path.exists(JOB_PATH):
        job_list = {}
        print("New job_list!")
    else:
        job_list = pickle.load(openfile(JOB_PATH, 'rb'))
    for key in job_list.keys():
        pid = job_list[key]
        if not check_pid(pid):
            del job_list[key]
    pickle.dump(job_list, openfile(JOB_PATH, 'wb'))
    return job_list

def waitProcess():
    job_list = checkProcess()
    if len(job_list) > opts.parallel:
        print("Number of active process={}. Waiting....".format(len(job_list)))
        time.sleep(10)
        waitProcess()

def getpid(fullapp, passName):
    job_list = checkProcess()
    if (fullapp, passName) in job_list:
        pid = job_list[(fullapp, passName)]
        return pid
    else:
        return None

def setpid(fullapp, passName, pid):
    job_list = pickle.load(openfile(JOB_PATH, 'rb'))
    job_list[(fullapp, passName)] = pid
    pickle.dump(job_list, openfile(JOB_PATH, 'wb'))

def sargs(args):
    return '_'.join([str(a) for a in args])

def getFullName(app, args, params):
    postfix = sargs(args) 
    postfix = postfix + ''.join(["_{}_{}".format(k,str(params[k])) for k in params ]) 
    if postfix != "":
        fullname = '{}_{}'.format(app, postfix)
    else:
        fullname = app
    fullname = fullname.replace('.','d')
    return fullname

def irange(start, stop, step):
    r = range(start, stop, step)
    if (len(r)>0):
        last = r[-1]
    else:
        last = start
    if(last+step==stop):
        r.append(stop)
    return r

def logs(app, passName):
    if passName=="GEN_PIR":
        return '{}/gen/{}/gen_pir.log'.format(SPATIAL_HOME, app)
    elif passName=="FIT_PIR":
        return '{}/out/{}/fit_pir.log'.format(PIR_HOME,app)
    elif passName=="GEN_CHISEL":
        return '{}/gen/{}/gen_chisel.log'.format(SPATIAL_HOME,app)
    elif passName=="MAKE_VCS":
        return '{}/gen/{}/vcs.log'.format(SPATIAL_HOME,app)
    elif passName=="MAP_PIR":
        return '{}/out/{}/map_pir.log'.format(PIR_HOME,app)
    elif passName=="RUN_SIMULATION":
        return '{}/gen/{}/sim.log'.format(SPATIAL_HOME, app)
    elif passName=="Utilization":
        return '{}/out/{}/ResourceAnalysis.log'.format(PIR_HOME, app)

def write(log, msg):
    with open(log, 'a') as f:
        f.write(msg)

parser = argparse.ArgumentParser(description='Run experiments')
parser.add_argument('--parallel', dest='parallel', nargs='?', default=1, type=int)
parser.add_argument('--single', dest='single', action='store_true', default=False) 
parser.add_argument('--run', dest='run', action='store_true', default=False) 
parser.add_argument('--status', dest='status', action='store_true', default=False) 
parser.add_argument('--dse', dest='dse', action='store_true', default=False) 
parser.add_argument('--app', dest='app', action='store', default='ALL',help='App name')
parser.add_argument('--regen', dest='regen', action='store', default='false',
    help='force pass to rerun' )
parser.add_argument('--torun', dest='torun', action='store', default='ALL',
    help='Pass to run')
parser.add_argument('--regression', dest='regression', action='store_true', default=False) 
parser.add_argument('--summary', dest='summary', action='store_true', default=False) 
parser.add_argument('--best', dest='best', action='store_true', default=False) 
parser.add_argument('--plot', dest='plot', action='store_true', default=False) 

global opts
(opts, args) = parser.parse_known_args()
