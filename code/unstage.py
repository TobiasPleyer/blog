import sys
import os
from os import path
from shutil import rmtree
from argparse import ArgumentParser


def delete_subs(subs):
    for sub in subs:
        rmtree(sub)
            
def snd(tpl):
    return tpl[1]
    
def fst(tpl):
    return tpl[0]

def clean_dir(d, limit):
    subs = []
    for sub in os.listdir(d):
        sub = path.join(d, sub)
        try:
            mtime = path.getctime(sub)
        except OSError:
            print("Couldn't find mtime for {} - skipping...".format(sub))
            continue
        subs.append((mtime,sub))
    subs.sort(key=fst, reverse=True)
    subs = map(snd, subs)
    delete_subs(subs[limit:])

if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument('-n', '--number', type=int, nargs='?', metavar='N', default=5)
    parser.add_argument('root', nargs=1)
    
    args = parser.parse_args()
    limit = args.number
    root = args.root[0]
    if not path.exists(root):
        print("Folder does not exist: {}".format(root))
        sys.exit(1)
    
    for name in os.listdir(root):
        d = path.join(root, name)
        if path.isdir(d):
            clean_dir(d, limit)
