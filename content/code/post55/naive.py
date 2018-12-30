from subprocess import run, PIPE, STDOUT


def shell(cmd):
    print(f"Now running: {cmd}")
    process = run(cmd, stdout=PIPE, stderr=STDOUT, shell=True)
    print(process.stdout.decode('utf-8'))
    return process.returncode == 0

def main():
    ok = shell("echo 'OK'; exit 0")
    if not ok:
        print("An error occurred. Aborting")
        return
    ok = shell("echo 'Again OK'; exit 0")
    if not ok:
        print("An error occurred. Aborting")
        return
    ok = shell("echo 'Problem!'; exit 1")
    if not ok:
        print("An error occurred. Aborting")
        return
    ok = shell("echo 'Should not execute'; exit 0")
    if not ok:
        print("An error occurred. Aborting")
        return
    print("Done")

if __name__ == '__main__': main()
