
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

from util import *

def plot_util_bw():
    cmap = matplotlib.cm.get_cmap('Oranges')
    fig, ax = plit.subplots()
    apps = []
    pcus = []
    pmus = []
    mcs = []
    laodbws = []
    storebws = []
    for app in summary:
        apps.append(app)
        pcus.append(summary["pcu"])
        pmus.append(summary["pmu"])
        mcs.append(summary["mc"])
        loadbws.append(summary["loadbw"])
        storebws.append(summary["storebws"])

    ind = range(len(apps))
    width = 0.35
    numbar = 5
    ax.bar(ind - width/numbar , pcus     , width , yerr=men_std , color='SkyBlue' , label='pcu')
    ax.bar(ind - width/numbar , pmus     , width , yerr=men_std , color='SkyBlue' , label='pmu')
    ax.bar(ind - width/numbar , mcs      , width , yerr=men_std , color='SkyBlue' , label='mc')
    ax.bar(ind - width/numbar , loadbws  , width , yerr=men_std , color='SkyBlue' , label='loadbw')
    ax.bar(ind - width/numbar , storebws , width , yerr=men_std , color='SkyBlue' , label='storebw')
    ax.set_xticklabels(apps)
    plot_path = '{}/apps/plots/util_bw.png'.format(SPATIAL_HOME)
    plt.grid(True)
    plt.xlabel('Apps')
    plt.ylabel('Percentage Utilization / Percentage to Peak Bandwith')
    ax.set_ylim(0,100)
    fig.set_size_inches(6,4)
    plt.gcf().subplots_adjust(bottom=0.15)
    plt.savefig(plot_path, format='png', dpi=900)
    print('Generate {}'.format(plot_path))

def sargs(args):
    return '_'.join([str(a) for a in args])

def cycleOf(log):
    lines = grep(log, ["Simulation complete at cycle"])
    if len(lines)==0:
        return None
    else:
        line = lines[0]
        cycle = int(line.split("Simulation complete at cycle:")[1].strip())
        return cycle

def pcuUsage(log):
    line = grep(log, ["PCU usage ="])
    if len(line) == 0:
      return None
    pct = float(line[0].split("(")[1].split("%")[0])
    return pct

def pmuUsage(log):
    line = grep(log, ["PMU usage ="])
    if len(line) == 0:
      return None
    pct = float(line[0].split("(")[1].split("%")[0])
    return pct

def mcUsage(log):
    line = grep(log, ["MC usage ="])
    if len(line) == 0:
      return None
    pct = float(line[0].split("(")[1].split("%")[0])
    return pct

def totalUsage(log):
    line = grep(log, ["Total usage ="])
    if len(line) == 0:
      return None
    pct = float(line[0].split("(")[1].split("%")[0])
    return pct

def drambw(log):
    line = grep(log, ["Total DRAM"])
    if len(line) == 0:
      return None
    return line[0].split("(")[1].split(")")[0].strip()

def loadbw(log):
    line = grep(log, ["Total DRAM"])
    if len(line) == 0:
      return None
    return float(line[0].split("(")[1].split("GB")[0].strip())

def storebw(log):
    line = grep(log, ["Total DRAM"])
    if len(line) == 0:
      return None
    return float(line[0].split("R,")[1].split("GB")[0].strip())

def summarize(app, args, params):
    opts.summary[app] = {}
    summary = opts.summary[app]
    fullname = getFullName(app, args, params)
    summary["cycle"] = {}
    for passName in passes:
        log = logs(fullname, passName)
        if passName=="psim_p2p":
            summary["pcu"] = pcuUsage(log)
            summary["pmu"] = pmuUsage(log)
            summary["mc"] = mcUsage(log)
            summary["loadbw"] = loadbw(log)
            summary["storebw"] = storebw(log)
        summary["cycle"][passName] = cycleOf(log)

def futil(used,total):
   return round(float(used) / float(total), 3)

