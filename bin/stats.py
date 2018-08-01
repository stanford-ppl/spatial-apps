
from os import listdir
from os.path import isfile, isdir, join, splitext, basename, dirname 
from collections import OrderedDict
import os
import pickle
import csv

import matplotlib
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.ticker as ticker
import os, sys
import math
import numpy as np

from util import *

def sargs(args):
    return '_'.join([str(a) for a in args])

def cycleOf(log):
    if not os.path.exists(log): return None
    lines = grep(log, ["Simulation complete at cycle"])
    if len(lines)==0:
        return None
    else:
        line = lines[0]
        cycle = int(line.split("Simulation complete at cycle:")[1].strip())
        return cycle

def usage(key, log):
    if not os.path.exists(log): return None
    line = grep(log, [key])
    if len(line) == 0:
      return None
    pct = float(line[0].split("(")[1].split("%")[0])
    return pct

def count(key, log):
    if not os.path.exists(log): return None
    line = grep(log, [key])
    if len(line) == 0:
      return None
    return int(line[0].split(key)[1])

def drambw(log):
    if not os.path.exists(log): return None
    line = grep(log, ["Total DRAM"])
    if len(line) == 0:
      return None
    return line[0].split("(")[1].split(")")[0].strip()

def loadbw(log):
    if not os.path.exists(log): return None
    line = grep(log, ["Total DRAM"])
    if len(line) == 0:
      return None
    return float(line[0].split("(")[1].split("GB")[0].strip()) / peak_bw * 100

def storebw(log):
    if not os.path.exists(log): return None
    line = grep(log, ["Total DRAM"])
    if len(line) == 0:
      return None
    return float(line[0].split("R,")[1].split("GB")[0].strip()) / peak_bw * 100

def numVC(log):
    if not os.path.exists(log): return None
    line = grep(log, ["Used "])
    if len(line) == 0:
      return None
    return int(line[0].split("Used ")[1].split("VCs")[0].strip())

def summarize(app, args, params):
    opts.summary['apps'][app] = {}
    summary = opts.summary['apps'][app]
    fullname = getFullName(app, args, params)
    summary["cycle"] = {}
    summary["vc"] = {}
    for passName in passes:
        log = logs(fullname, passName)
        if passName=="psim_D_v1_s4":
            summary["pcu"] = usage("PCU = ", log)
            summary["pmu-comp"] = usage("PMU-comp = ",log)
            summary["pmu-mem"] = usage("PMU-mem = ",log)
            summary["mc"] = usage("MC =", log)
        if passName=="psim_p2p":
            summary["loadbw"] = loadbw(log)
            summary["storebw"] = storebw(log)
        summary["cycle"][passName] = cycleOf(log)
        if "D" in passName:
            summary["vc"][passName] = numVC(log)

    with open(logs(fullname,"link_count"), 'r') as csvfile:
        reader = csv.DictReader(csvfile)
        counts = []
        for row in reader:
            count = row['count']
            if count is not None:
                counts.append(count)
        summary["link_count"] = counts
    return summary

def futil(used,total):
   return round(float(used) / float(total), 3)

