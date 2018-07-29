from os import listdir
from os.path import isfile, isdir, join, splitext, basename, dirname 
from collections import OrderedDict
import subprocess
import time
import signal
import numpy as np
import types

import matplotlib
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.ticker as ticker
import os, sys
import math
import multiprocessing 

from stats import *
from util import *

def copyApp(app, args, params):
    path = '{0}/{1}.scala'.format(APP_DIR, app)
    fullname = getFullName(app, args, params) 
    if not os.path.exists('{}/gen'.format(APP_DIR)):
        os.makedirs('{}/gen'.format(APP_DIR))
    newpath = '{0}/gen/{1}.scala'.format(APP_DIR, fullname)
    paramFound = {}
    with open(newpath, 'w') as newapp:
        with open(path, 'r') as origapp :
            for line in origapp:
                if line.strip().startswith("//"):
                    continue
                found = False
                if 'object {}'.format(app) in line:
                    newapp.write(line.replace(app, fullname))
                    found = True
                for param in params:
                    if 'val {} = '.format(param) in line and "param " in line:
                        newapp.write('val {} = {}\n'.format(param, params[param]))
                        paramFound[param] = True
                        found = True
                if not found:
                    newapp.write(line)
    for param in params:
        if param not in paramFound:
            print('Param {} not found !!!'.format(param))
            exit()

def getCommand(passName, fullapp):
    if passName=="gen_pir":
        command = "{}/apps/bin/{} {} {}".format(SPATIAL_HOME, passName, fullapp, opts.pirsrc)
    else:
        command = "{}/apps/bin/{} {}".format(SPATIAL_HOME, passName, fullapp)
    return command

def runPass(fullname, passName):
    if not torun(passName):
        return
    if running(fullname, passName):
        return
    if progress(fullname, passName)!="NOTRUN" and not regenerate(passName):
        return
    for dep in dependency[passName]:
        if not success(fullname, dep):
            # print("{} {} not ran due to {} not succeeded".format(fullname, passName, dep))
            return

    if opts.parallel > 1:
        waitProcess()

    # print("runPass {} {}".format(fullname, passName))
    # clean log
    log = logs(fullname, passName)
    rm(log)
    mkdir(dirname(log))

    command = getCommand(passName, fullname)

    logFile = open(log, 'w')
    proc = subprocess.Popen(command.split(" "), stdout=logFile, stderr=logFile)
    setpid(fullname, passName, proc.pid)
    proc.wait()

def runJob(app, args, params):
    if args or params:
        copyApp(app, args, params)
    fullname = getFullName(app, args, params)
    for passName in passes:
        runPass(fullname, passName)

def launchJob(app, args, params):
    print('Running {} args={} and params=[{}]'.format(app, str(args),
        ' '.join(['{}={}'.format(p,params[p]) for p in params])))
    if opts.parallel > 1:
        pid = os.fork()
        if pid==0:
            runJob(app, args, params)
            exit()
        else:
            time.sleep(1)
    else:
        runJob(app, args, params)

def kill(fullname, passName):
    pid = getpid(fullname, passName)
    log = logs(fullname, passName)
    if pid is not None:
        resp = raw_input("{}Kill {} ({}){} for {}? ".format(bcolors.RED, passName, pid, bcolors.NC, fullname))
        if resp == "y":
            write(log, "-------------{}PASS (KILLED){}------------".format(bcolors.RED, bcolors.NC))
            os.kill(pid, signal.SIGTERM)
            print("Killed {}".format(pid))
    else:
        rm(log)

def act(fullname, resp):
    def getPass():
        ps = []
        for passName in passes:
            if passName in resp:
                ps.append(passName)
        if len(ps) == 0:
            for passName in reversed(passes):
                if running(fullname, passName): ps.append(passName); break
        if len(ps) == 0:
            for passName in reversed(passes):
                if failed(fullname, passName): ps.append(passName); break
        return ps
    def removeLog():
        for passName in getPass():
            log = logs(fullname, passName)
            rm(log)
            # del progress_cache[(fullname, passName)]
    def printlog():
        for passName in getPass():
            log = logs(fullname, passName)
            print("{}Show {} {}{}".format(bcolors.CYAN, passName, log, bcolors.NC))
            cat(log)
            print("{}-------------------------------------------------------{}".format(bcolors.CYAN, bcolors.NC))
    def open():
        for passName in getPass():
            log = logs(fullname, passName)
            vim(log)
    def killPass():
        for passName in getPass():
            kill(fullname, passName)

    if resp.startswith("k"):
        killPass()
    elif resp.startswith("r"):
        removeLog()
    elif resp.startswith("s"):
        printlog();
    elif resp.startswith("o"):
        open()
    elif resp.startswith("q"):
        exit(0)

def show(fullname):
    def printError(passName, log):
        if failed(fullname, passName):
            lines = grep(log, ["error"])
            for line in lines:
                sys.stdout.write('- ' + line)
    def passMessage(passName, log):
        msg = ""
        if success(fullname, passName) and passName=="psim_p2p":
            pcu = pcuUsage(log)
            pmu = pmuUsage(log)
            mc = mcUsage(log)
            msg += "pcu={}% pmu={}% mc={}%".format(pcu, pmu, mc)
        if success(fullname, passName) and (passName.startswith("psim_")):
            msg += " cycle={} dram={}".format(cycleOf(log), drambw(log))
            vc = numVC(log)
            if vc is not None: msg += " vc={}".format(vc)
        return msg
    for passName in passes:
        log = logs(fullname, passName)
        prog = progress(fullname, passName)
        msg = passMessage(passName, log)
        pid = getpid(fullname, passName)
        pid = " [{}]".format(pid) if pid is not None else ""
        print("{}{}({}){}{} {} {}".format(colors[prog], passName, prog, bcolors.NC, pid, log, msg))
        printError(passName, log)
    print("{}-------------------------------------------------------{}".format(bcolors.CYAN, bcolors.NC))
    return

def status(app, args, params):
    fullname = getFullName(app, args, params)
    show(fullname)
    resp = raw_input("")
    while resp != "":
        act(fullname, resp)
        show(fullname)
        resp = raw_input("")

# progress_cache = {}
def progress(fullname, passName):
    # if (fullname, passName) in progress_cache:
        # return progress_cache[(fullname, passName)]
    log = logs(fullname, passName)

    def checkFail(include, exclude, exists):
        return (len(exclude)!=0 and len(grep(log, exclude)) != 0) or  \
               (len(include)!=0 and len(grep(log, include)) == 0) or  \
               (len(exists)!=0 and any([not os.path.exists(path) for path in exists]))

    def isFailed():
        if passName=="run_simulation":
            include = ["Simulation ran for "]
            exclude = ["Hardware timeout after"]
            exists = []
        elif passName.startswith("psim_"):
            include = ["Simulation complete at cycle"]
            exclude = ["DEADLOCK"]
            exists = []
        elif passName=="gen_pir":
            include = ["success"]
            exclude = ['error','Error','ERROR','fail','Killed', 'KILLED']
            exists = ["{}/{}.scala".format(opts.pirsrc, fullname)]
            exists = []
        else:
            include = []
            exclude = ['error','Error','ERROR','fail','Killed', 'KILLED']
            exists = []
        return checkFail(include, exclude, exists)

    prog = "NONE"
    if os.path.exists(log):
        if (len(grep(log,"PASS (DONE)".format(passName))) != 0):
            prog = "FAILED" if isFailed() else "SUCCESS"
        else:
            pid = getpid(fullname, passName)
            prog = "NOTRUN" if pid is None else "RUNNING"
    else:
        prog = "NOTRUN"
    # progress_cache[(fullname,passName)] = prog
    return prog

def hasRun(fullname, passName):
    log = logs(fullname, passName)
    res = os.path.exists(log)
    if passName == "GEN_PIR":
        pirsrc = "{}/pir/apps/src/gen/{}.scala".format(PIR_HOME, fullname)
        res &= os.path.exists(pirsrc)
    return res

def failed(fullname, passName):
    return progress(fullname, passName)=="FAILED"

def success(fullname, passName):
    return progress(fullname, passName)=="SUCCESS"

def running(fullname, passName):
    return progress(fullname, passName)=="RUNNING"

def regenerate(passName):
    return passName in opts.regen or opts.regen == "ALL"

def torun(passName):
    return passName in opts.torun or opts.torun == "ALL"

def git_add(app, args, params):
    fullname = getFullName(app, args, params)
    for passName in passes:
        log = logs(fullname, passName)
        spatial_app = "{}/apps/src/gen/{}.scala".format(SPATIAL_HOME,fullname)
        # pir_app = "{}/pir/apps/gen/{}.scala".format(PIR_HOME,fullname)
        files = [log, spatial_app]
        for f in files:
          if os.path.exists(f):
            subprocess.call("git add -f {}".format(f), shell=True)

def target(app, args, params):
    if opts.run:
        launchJob(app, args, params)
    if opts.status:
        status(app, args, params)
    if opts.summarize:
        summarize(app, args, params)
    if opts.git:
        git_add(app, args, params)

