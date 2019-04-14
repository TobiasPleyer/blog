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

def create_tag(refspec, tagname, message):
    return [shell(f"git show {refspec}"),
            shell(f"git tag -m '{message}' {tagname} {refspec}")]

def main():
    tagAction = (create_tag('df40cfe', 'v1.0', 'First release') +
                 create_tag('851a93b', 'v2.0', 'Second release') +
                 # Ups I made an error - tag already exists
                 create_tag('961090f', 'v1.0', 'Third release') +
                 create_tag('32d3085', 'v4.0', 'Forth release')
                )
    ok, stdouts = run_in_sequence(tagAction)
    print(f"Final result: {ok}")
    print("== INFO ==")
    for s in stdouts:
        print(s)

if __name__ == '__main__': main()
