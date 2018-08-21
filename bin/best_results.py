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
from stat import *
from task import *

bestapps = []

bestapps += ['DotProduct__N_1048576_ts_65536_op_1']
bestapps += ['TPCHQ6__N_1048576_ts_65536_op_1']
bestapps += ['OuterProduct__M_4096_N_4096_ts1_64_ts2_1024_op1_1_op2_2_ip2_16_ip1_1']
bestapps += ['BlackScholes__N_1048576_ts_65536_op_1']
bestapps += ['SPMV_ELL__L_64_N_16384_ts_1024_op_1_mp_2']
bestapps += ['GDA__C_128_R_4096_ts_512_op_1_mp1_8_mp2_1']
bestapps += ['LogReg__iters_4_D_128_N_8192_ts_512_op_1_mp_4']
bestapps += ['GEMM_Blocked__DIM_512_IDIM_256_ts_256_its_128_loop_ii_1_loop_jj_1_loop_kk_1_loop_i_1_loop_k_16']
bestapps += ['Kmeans__I_2_K_64_D_64_N_8192_ts_1024_op_1_mp1_8_mp2_1_mp3_1']
bestapps += ['lenet_loops__batch_par_1_conv1_par_2_conv2_par_4_mat1_par_2_mat2_par_1']
bestapps += ['SGD_minibatch__D_64_N_16384_E_2_ts_1024_mp1_8_mp2_16']

gen_pir()
psim_generic("psim_p2p_ideal"        , ["gen_pir"]        , "--net=p2p --vlink=0 --slink=0 --fifo-depth=20 --vfifo=4 --link-prop=db --flit-width=512")
psim_generic("psim_p2p"              , ["psim_p2p_ideal"] , "--net=p2p --vlink=0 --slink=0 --fifo-depth=4  --vfifo=4 --link-prop=db --flit-width=512" )
psim_generic("psim_v3_s4"            , ["psim_p2p_ideal"] , "--net=static --vlink=3 --slink=4 --fifo-depth=4 --vfifo=4 --link-prop=db --flit-width=512" )
psim_generic("psim_v2_s4"            , ["psim_p2p_ideal"] , "--net=static --vlink=2 --slink=4 --fifo-depth=4 --vfifo=4 --link-prop=db --flit-width=512 --proute-seed=2" )
psim_generic("psim_v2_s4_cd"         , ["psim_p2p_ideal"] , "--net=static --vlink=2 --slink=4 --fifo-depth=4 --vfifo=4 --link-prop=cd --flit-width=512" )
psim_generic("psim_v3_s4_cd"         , ["psim_p2p_ideal"] , "--net=static --vlink=3 --slink=4 --fifo-depth=4 --vfifo=4 --link-prop=cd --flit-width=512" )

psim_generic("psim_D_v1_s4"          , ["psim_p2p_ideal"] , "--net=dynamic --vlink=1 --slink=4 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=512 --proute-seed=2" )
psim_generic("psim_D_v1_s4_f256"     , ["psim_p2p_ideal"] , "--net=dynamic --vlink=1 --slink=4 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=256 --proute-seed=2" )
psim_generic("psim_D_v0_s4"          , ["psim_p2p_ideal"] , "--net=dynamic --vlink=0 --slink=4 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=512 --proute-seed=2" )
psim_generic("psim_D_v2_s4"          , ["psim_p2p_ideal"] , "--net=dynamic --vlink=2 --slink=4 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=512 --proute-seed=2" )
psim_generic("psim_D_v0_s0"          , ["psim_p2p_ideal"] , "--net=dynamic --vlink=0 --slink=0 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=512 --proute-seed=2" )
psim_generic("psim_D_v1_s4_rq1_q4"   , ["psim_p2p_ideal"] , "--net=dynamic --vlink=1 --slink=4 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=512 --proute-q=1 --psim-q=4 --proute-seed=2" )
psim_generic("psim_D_v1_s4_rq1_q8"   , ["psim_p2p_ideal"] , "--net=dynamic --vlink=1 --slink=4 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=512 --proute-q=1 --psim-q=8 --proute-seed=2" )
psim_generic("psim_D_v1_s4_rq1_q16"  , ["psim_p2p_ideal"] , "--net=dynamic --vlink=1 --slink=4 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=512 --proute-q=1 --psim-q=16 --proute-seed=2" )

# psim_generic("psim_D_v1_s4_rq4_q4"   , ["psim_p2p_ideal"] , "--net=dynamic --vlink=1 --slink=4 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=512 --proute-q=4 --psim-q=4" )
# psim_generic("psim_D_v1_s4_rq4_q8"   , ["psim_p2p_ideal"] , "--net=dynamic --vlink=1 --slink=4 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=512 --proute-q=4 --psim-q=8" )
# psim_generic("psim_D_v1_s4_rq4_q16"  , ["psim_p2p_ideal"] , "--net=dynamic --vlink=1 --slink=4 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=512 --proute-q=4 --psim-q=16" )
# psim_generic("psim_D_v1_s4_rq4"      , ["psim_p2p_ideal"] , "--net=dynamic --vlink=1 --slink=4 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=512 --proute-q=4" )
# psim_generic("psim_D_v1_s4_rq4_f256" , ["psim_p2p_ideal"] , "--net=dynamic --vlink=1 --slink=4 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=256 --proute-q=4" )
# psim_generic("psim_D_v0_s4_rq4"      , ["psim_p2p_ideal"] , "--net=dynamic --vlink=0 --slink=4 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=512 --proute-q=4" )
# psim_generic("psim_D_v2_s4_rq4"      , ["psim_p2p_ideal"] , "--net=dynamic --vlink=2 --slink=4 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=512 --proute-q=4" )
# psim_generic("psim_D_v0_s0_rq4"      , ["psim_p2p_ideal"] , "--net=dynamic --vlink=0 --slink=0 --fifo-depth=4 --vfifo=4 --proute-algo=route_min_directed_valient --link-prop=db --flit-width=512 --proute-q=4" )
link_count()

def runbest():
    opts.toclear = reduce(lambda a,b: "{},{}".format(a,b), [ p for p in passes() if
        p.startswith("psim")])
    # opts.toclear = reduce(lambda a,b: "{},{}".format(a,b), [ p for p in passes() if
        # p=='link_count'])
    if opts.run:
        opts.git=False

    for app in bestapps:
        target(app, [], {})
    #################################################################################################################
