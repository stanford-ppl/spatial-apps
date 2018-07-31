from os.path import join
import argparse
import subprocess
import time
import pickle
from collections import OrderedDict

import os, sys
import math

SPATIAL_HOME = os.environ['SPATIAL_HOME']
PIR_HOME = os.environ['PIR_HOME']
PAPER_HOME = os.environ['HOME'] + "/papers/"

APPS = ["lenet_loops"]
# APPS = ['DotProduct', 'OuterProduct', 'GDA', 'BlackScholes', 'TPCHQ6']
# APPS = ['DotProduct', 'OuterProduct', 'TPCHQ6', 'GDA', 'BlackScholes', 'GEMM_Blocked']
# APPS += ['LogReg', 'SGD_minibatch', 'SimpleP4']
# APPS += ['Kmeans', 'PageRank', 'SPMV_CRS', 'BFS']

dependency = OrderedDict()
dependency["gen_pir"] = []
dependency["psim_asic"] = ["gen_pir"]
dependency["psim_p2p"] = ["gen_pir"]
dependency["link_count"] = ["psim_p2p"]
dependency["psim_D_v1_s6"] = ["gen_pir"]
dependency["psim_D_v2_s6"] = ["gen_pir"]
dependency["psim_v3_s6"] = ["psim_p2p"]
dependency["psim_v2_s6"] = ["psim_p2p"]
dependency["psim_D_v0_s0"] = ["psim_p2p"]
dependency["psim_D_v1_s4"] = ["psim_p2p"]
dependency["psim_D_v2_s4"] = ["psim_p2p"]
dependency["psim_v3_s4"] = ["psim_p2p"]
dependency["psim_v2_s4"] = ["psim_p2p"]
passes=dependency.keys()

def getCommand(passName, fullapp):
    if passName=="gen_pir":
        command = "{}/apps/bin/{} {} {}".format(SPATIAL_HOME, passName, fullapp, opts.pirsrc)
    elif passName.startswith("psim_"):
        if "D" in passName:
            net = "dynamic"
            vlink = passName.split("_v")[1].split("_")[0]
            slink = passName.split("_s")[1]
        elif "v" in passName:
            net = "static"
            vlink = passName.split("_v")[1].split("_")[0]
            slink = passName.split("_s")[1]
        else:
            net = passName.split("psim_")[1]
            vlink = 0
            slink = 0
        command = "{}/apps/bin/psim_generic {} {} {} {}".format(SPATIAL_HOME, fullapp, net, vlink, slink)
    else:
        command = "{}/apps/bin/{} {}".format(SPATIAL_HOME, passName, fullapp)
    return command

LOG_DIR='{}/apps/log'.format(SPATIAL_HOME)
APP_DIR='{}/apps/src/'.format(SPATIAL_HOME)
JOB_PATH="{}/gen/job_list.pickle".format(SPATIAL_HOME)
SUMMARY_PATH="{}/apps/summary.pickle".format(SPATIAL_HOME)
SUMMARY_CSV_PATH="{}/apps/summary.csv".format(SPATIAL_HOME)
BEST_SUMMARY_CSV_PATH="{}/apps/best.csv".format(SPATIAL_HOME)

cycle_cache = {}

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

lanes = 16
bankSize = 32 * 1024 / 4
peak_bw = 12.8*4
pmuSize = 256 / 4 * 1024 # 256 kB / (4 word / Byte) = 64 * 1024 = 65536 word

def rm(path):
    if os.path.exists(path):
        # print("rm {}".format(path))
        os.remove(path)

def cat(path):
    with open(path, "r") as f:
        for line in f:
            sys.stdout.write(line)

def mkdir(path):
    if not os.path.exists(path):
        os.makedirs(path)

def vim(path):
    subprocess.call("vim {}".format(path), shell=True)

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
    return open(path, flag)
        
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
        try:
          job_list = pickle.load(openfile(JOB_PATH, 'rb'))
        except EOFError:
          time.sleep(1)
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
    job_list = checkProcess()
    job_list[(fullapp, passName)] = pid
    pickle.dump(job_list, openfile(JOB_PATH, 'wb'))

def sargs(args):
    return '_'.join([str(a) for a in args])

def getFullName(app, args, params):
    postfix = ''.join(["_{}_{}".format(k,str(params[k])) for k in params ]) 
    if postfix != "":
        fullname = '{}_{}'.format(app, postfix)
    else:
        fullname = app
    fullname = fullname.replace('.','d')
    return fullname

def logs(app, passName):
    return '{}/{}/{}.log'.format(LOG_DIR, app, passName)

def irange(start, stop, step):
    r = range(start, stop, step)
    if (len(r)>0):
        last = r[-1]
    else:
        last = start
    if(last+step==stop):
        r.append(stop)
    return r

def write(log, msg):
    with open(log, 'a') as f:
        f.write(msg)

parser = argparse.ArgumentParser(description='Run experiments')
parser.add_argument('--run', dest='run', action='store_true', default=False) 
parser.add_argument('--single', dest='single', action='store_true', default=False) 
parser.add_argument('--status', dest='status', action='store_true', default=False) 
parser.add_argument('--dse', dest='dse', action='store_true', default=False) 
parser.add_argument('--git', dest='git', action='store_true', default=False) 
parser.add_argument('--app', dest='app', action='store', default='ALL',help='App name')
parser.add_argument('--rerun', dest='rerun', action='store', default='false',
    help='force pass to rerun' )
parser.add_argument('--torun', dest='torun', action='store', default='ALL',
    help='Pass to run')
parser.add_argument('--regression', dest='regression', action='store_true', default=False) 
parser.add_argument('--best', dest='best', action='store_true', default=False) 
parser.add_argument('--summarize', dest='summarize', action='store_true', default=False) 

global opts
(opts, args) = parser.parse_known_args()

opts.apps = [] if len(args)==0 else APPS if args[0] == "ALL" else args

opts.pirsrc = '{}/pir/apps/src/gen'.format(PIR_HOME) if opts.dse else '{}/pir/apps/src'.format(PIR_HOME)
# opts.pirsrc = '{}/pir/apps/src/gen'.format(PIR_HOME)
if opts.summarize:
    opts.summary = OrderedDict()
