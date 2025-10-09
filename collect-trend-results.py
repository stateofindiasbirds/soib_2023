#!./py/bin/python3
#
# Relies on locally installed py interpreter with pyreadr
# package installed in it.
#
from argparse import ArgumentParser
import pyreadr
import os
import sys
import glob
import re
import shutil
from pathlib import Path

parser = ArgumentParser()
parser.add_argument('done_list',
    help="""Path to done.list, or output directory.
Will scan and collect trends if directory.""")
parser.add_argument('-v', '--verbose', action='store_true',
    help='Show names of src and destination files')
parser.add_argument('-d', '--dry_run', action='store_true',
    help="Dry run. Don't copy any files.")
args = parser.parse_args()

if os.path.isfile(args.done_list):
    base_dir = os.path.split(args.done_list)[0]
    # dir.list has list of all trends csv files
    filelist = list(map(lambda x:os.path.join(base_dir,x.strip()), open(args.done_list).readlines()))
    done_dir = os.path.split(args.done_list)[0]
else:
    # scan and get all trends csv files
    done_dir = args.done_list
    curdir = os.getcwd()
    os.chdir(done_dir)
    filelist = list(map(lambda x:os.path.join(done_dir,x), glob.glob("**/trends_*.csv", recursive=True)))
    os.chdir(curdir)
    if len(filelist)==0:
        print(f"Error: Didn't find any trends file in scan of {done_dir}")
        sys.exit(-1)

def get_mask(path):
    return re.sub('.*/output/([^/]*)/.*$','\\1',path)

masks = set(map(lambda path:get_mask(path), filelist))
md = pyreadr.read_r("00_data/analyses_metadata.RData")
metadata = md['analyses_metadata']
files_missing = 0
copy_list = []
for this_mask in masks:
    print('mask=',this_mask)
    tgt_mask_data = metadata[metadata['MASK']==this_mask]['DATA.PATH'].values[0]
    tgt_base_dir = os.path.split(tgt_mask_data)[0]
    src_base_dir = done_dir
    for filename in filelist:
        if get_mask(filename) != this_mask:
            continue
        f_dir, f_name = os.path.split(filename)
        src_file = filename
        dst_dir = os.path.join(tgt_base_dir, 'trends')
        dst_file = os.path.join(dst_dir, f_name)
        if not os.path.isfile(src_file):
            if args.verbose:
                print(f"Missing: {src_file}")
            files_missing += 1
        else:
            copy_list.append((src_file, dst_file))

if files_missing==0:
    print(f"Copying {len(copy_list)} files...")
    for src_file, dst_file in copy_list:
        if not args.dry_run:
            dst_dir, dst_fname = os.path.split(dst_file)
            dst_dir = Path(dst_dir)
            dst_dir.mkdir(parents=True, exist_ok=True)
            shutil.copy(src_file, dst_file)
            if args.verbose:
                print(f'{src_file} -> {dst_file}')
        else:
            print(f'DRY RUN: {src_file} -> {dst_file}')
else:
    print(f"{files_missing} output files are missing. So didn't copy any. Please check.")
    sys.exit(-1)
