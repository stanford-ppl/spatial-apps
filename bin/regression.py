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

def regression():
    if opts.app=="ALL":
        apps = []
        apps.append("DotProduct")
        apps.append("OuterProduct")
        apps.append("Backprop")
        apps.append("Gibbs_Ising2D")
        apps.append("TPCHQ6")
        apps.append("SPMV_CRS")
        apps.append("BlackScholes")
        # apps.append("Kmeans_plasticine")
        # apps.append("PageRank_plasticine")
        apps.append("GEMM_Blocked")
        apps.append("GDA")
        apps.append("SYRK_col")
    else:
        apps = [opts.app]

    opts.torun = "GEN_PIR,MAP_PIR"
    for app in apps:
        target(app, [], {})

