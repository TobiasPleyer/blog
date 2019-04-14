from subprocess import run, PIPE, STDOUT


def do_shell(cmd):
    process = run(cmd, stdout=PIPE, stderr=STDOUT, shell=True)
    return (process.returncode == 0, [f"Now running: {cmd}", process.stdout.decode('utf-8')])

def shell(cmd):
    return lambda: do_shell(cmd)

def run_in_sequence(cmds):
    stdouts = []
    for cmd in cmds:
        ok, stdout = cmd()
        stdouts += stdout
        if not ok:
            return (False, stdouts)
    return (True, stdouts)

def main():
    verbosity = 2
    ok, stdouts = run_in_sequence([
        shell("echo 'OK'; exit 0"),
        shell("echo 'Again OK'; exit 0"),
        shell("echo 'Problem!'; exit 1"),
        shell("echo 'Should not execute'; exit 0")
        ])
    interesting_stuff = list(
                            filter(lambda s: not s.startswith("Now running"),
                                   stdouts))
    if ok and verbosity > 1:
        print("STDOUTS")
        for s in interesting_stuff:
            print(s)
        print("Done")
    else:
        print("An error occurred. Aborting")
        print("STDOUTS")
        for s in interesting_stuff:
            print(s)

if __name__ == '__main__': main()
