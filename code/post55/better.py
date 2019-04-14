from subprocess import run, PIPE, STDOUT


def do_shell(cmd):
    print(f"Now running: {cmd}")
    process = run(cmd, stdout=PIPE, stderr=STDOUT, shell=True)
    print(process.stdout.decode('utf-8'))
    return process.returncode == 0

def shell(cmd):
    return lambda: do_shell(cmd)

def run_in_sequence(cmds):
    for cmd in cmds:
        ok = cmd()
        if not ok:
            return False
    return True

def main():
    ok = run_in_sequence([
           shell("echo 'OK'; exit 0"),
           shell("echo 'Again OK'; exit 0"),
           shell("echo 'Problem!'; exit 1"),
           shell("echo 'Should not execute'; exit 0")
           ])
    if ok:
        print("Done")
    else:
        print("An error occurred. Aborting")

if __name__ == '__main__': main()
