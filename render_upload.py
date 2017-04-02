from subprocess import check_output
from jinja2 import FileSystemLoader, Environment
from pprint import pprint

def replace_root_dir(filepath):
    return filepath.replace('output/', '.blog/', 1)

def filter_func(filepath):
    return (filepath
    and not filepath.endswith('README')
    and not filepath.endswith('.png')
    and not filepath.endswith('.css'))

env = Environment(loader=FileSystemLoader('.'), trim_blocks=True)
template = env.get_template('upload.sh.in')

find_output = check_output(['find', 'output/', '-type', 'f'])
#local file list
lfile_list = find_output.decode('utf-8').split('\n')
# filter unwanted stuff
lfile_list = list(filter(filter_func, lfile_list))
#remote file list on server
rfile_list = list(map(replace_root_dir, lfile_list))

sync_map = list(zip(lfile_list, rfile_list))

#pprint(sync_map)

with open('upload_all.sh', 'w') as fout:
    fout.write(template.render(sync_map=sync_map))
