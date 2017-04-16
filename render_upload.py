from subprocess import check_output
from jinja2 import FileSystemLoader, Environment
import argparse

def replace_root_dir(filepath):
    return filepath.replace('output/', '.blog/', 1)

def filter_func(filepath):
    return (filepath
    and not filepath.endswith('README')
    and not filepath.endswith('.png')
    and not filepath.endswith('.css'))


if __name__ == '__main__':
    #--------------------------------------------------
    # Argument parsing
    #--------------------------------------------------
    parser = argparse.ArgumentParser(description="Simple FTP upload script generator")
    parser.add_argument('-u', '--user', nargs=1, help="The user account used to log in")
    parser.add_argument('-s', '--host', nargs=1, help="The url of the FTP host")
    args = parser.parse_args()
    user = args.user[0]
    host = args.host[0]
    #--------------------------------------------------
    # Template environment initialization and rendering
    #--------------------------------------------------
    env = Environment(loader=FileSystemLoader('.'), trim_blocks=True)
    template = env.get_template('upload.sh.in')

    find_output = check_output(['find', 'output/', '-type', 'f'])  # use find to search
    lfile_list = find_output.decode('utf-8').strip().split('\n')           # local file list
    lfile_list = list(filter(filter_func, lfile_list))             # filter unwanted stuff
    rfile_list = list(map(replace_root_dir, lfile_list))           # remote file list on server
    sync_map = list(zip(lfile_list, rfile_list))                   # zip it for jinja

    find_output = check_output(['find', 'output/images/', '-type', 'f'])  # use find to search
    lfile_list = list(find_output.decode('utf-8').strip().split('\n'))
    rfile_list = list(map(replace_root_dir, lfile_list))
    sync_map += list(zip(lfile_list, rfile_list))

    with open('upload_all.sh', 'w') as fout:
        fout.write(template.render(ftp_host=host,
                                   user=user,
                                   sync_map=sync_map))
