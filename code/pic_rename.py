import sys
from pathlib import Path


folder = sys.argv[1]
pattern = sys.argv[2]
name_pattern = sys.argv[3]

folderpath = Path(folder)
filepaths = list(folderpath.glob(pattern))

pic_counter = 1
name_format = name_pattern + "{:03}{}"

for filepath in filepaths:
    if filepath.is_file():
        new_name = name_format.format(pic_counter, filepath.suffix)
        filepath.rename(folderpath/new_name)
        pic_counter += 1
