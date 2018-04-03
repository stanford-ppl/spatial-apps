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
                found = False
                if 'object {}'.format(app) in line:
                    newapp.write(line.replace(app, fullname))
                    found = True
                for param in params:
                    if 'val {} = '.format(param) in line:
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
    log = logs(fullapp, passName)
    if passName=="GEN_PIR":
        command = "{}/apps/bin/gen_pir {} {}".format(SPATIAL_HOME, fullapp, log)
        if opts.regression:
            command += ' {}/pir/apps/src'.format(PIR_HOME)
    elif passName=="FIT_PIR":
        command = "{}/apps/bin/fit_pir {} {}".format(SPATIAL_HOME, fullapp, log)
    elif passName=="GEN_CHISEL":
        command = "{}/apps/bin/gen_chisel {} {}".format(SPATIAL_HOME, fullapp, log)
    elif passName=="MAKE_VCS":
        command = "{}/apps/bin/make_vcs {} {}".format(SPATIAL_HOME, fullapp, log)
    elif passName=="MAP_PIR":
        command = "{}/apps/bin/map_pir {} {}".format(SPATIAL_HOME, fullapp, log)
    elif passName=="RUN_SIMULATION":
        command = "{}/apps/bin/run_sim {} {}".format(SPATIAL_HOME, fullapp, log)

    return command
    
def runPass(fullname, passName):
    if not torun(passName):
        return
    if success(fullname, passName) and not regenerate(passName):
        return
    if running(fullname, passName):
        return
    for dep in dependency[passName]:
        if not success(fullname, dep):
            print("{} {} not ran due to {} not succeeded".format(fullname, passName, dep))
            return

    # print("runPass {} {}".format(fullname, passName))
    # clean log
    log = logs(fullname, passName)
    rm(log)

    command = getCommand(passName, fullname)

    proc = subprocess.Popen(command.split(" "))
    setpid(fullname, passName, proc.pid)
    proc.wait()

def runJob(app, args, params):
    if args or params:
        copyApp(app, args, params)
    fullname = getFullName(app, args, params)
    for passName in passes:
        runPass(fullname, passName)

def launchJob(app, args, params):
    if opts.parallel > 1:
        waitProcess()
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
    for passName in passes:
        pid = getpid(fullname, passName)
        log = logs(fullname, passName)
        if pid is None:
            resp = raw_input("{}Kill {} ({}){} for {}?".format(bcolors.RED, passName, pid, bcolors.NC, fullname))
            if resp == "y":
                write(log, "-------------{}PASS (KILLED){}------------".format(bcolors.RED, bcolors.NC))
                os.kill(pid, signal.SIGTERM)
                print("Killed {}".format(pid))
        else:
            rm(log)

def act(fullname, resp):
    def removeLog():
        for passName in passes:
            if passName in resp:
                log = logs(fullname, passName)
                rm(log)
                # del progress_cache[(fullname, passName)]
    def printlog(passName):
        log = logs(fullname, passName)
        print("{}Show {} {}{}".format(bcolors.CYAN, passName, log, bcolors.NC))
        cat(log)
        print("{}-------------------------------------------------------{}".format(bcolors.CYAN, bcolors.NC))
    def open(passName):
        log = logs(fullname, passName)
        vim(log)

    if resp == "k":
        kill(fullname)
    elif "r" in resp:
        removeLog()
    elif resp == "s":
        for passName in passes:
            if running(fullname, passName) or failed(fullname, passName): printlog(passName); break
    elif "s " in resp:
        for passName in passes:
            if passName in resp: printlog(passName);
    elif resp == "o":
        for passName in passes:
            if running(fullname, passName) or failed(fullname, passName): open(passName); break
    elif "o " in resp :
        for passName in passes:
            if passName in resp: open(passName)

def show(fullname):
    for passName in passes:
        if passName=="GEN_PIR":
            keywords = ["Except"]
        elif passName=="MAP_PIR":
            keywords = ["error"]
        else:
            keywords = ["error"]

        log = logs(fullname, passName)
        prog = progress(fullname, passName)
        print("{}{}({}){} {}".format(colors[prog], passName, prog, bcolors.NC, log))
        if failed(fullname, passName):
            lines = grep(log, keywords)
            for line in lines:
                sys.stdout.write('- ' + line)
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
    prog = "NONE"
    if os.path.exists(log):
        isDone = (len(grep(log,"PASS (DONE)".format(passName))) != 0)
        if isDone:
	    isFailed = (len(grep(log, ['error','Error','ERROR','No rule to make', 'Killed', 'KILLED'])) != 0)
            if passName=="RUN_SIMULATION":
	        hasCycle=cycleOf(fullname) is not None
	        timeOut = len(grep(log, 'Hardware timeout after') != 0)
                if not hasCycle or timeOut:
                    isFailed = True
            if isFailed: 
	        prog = "FAILED"
            else:
	        prog = "SUCCESS"
        else:
            pid = getpid(fullname, passName)
            if pid is not None:
                prog = "RUNNING"
            else:
                prog = "NOTRUN"
    else:
        prog = "NOTRUN"
    # progress_cache[(fullname,passName)] = prog
    return prog

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
