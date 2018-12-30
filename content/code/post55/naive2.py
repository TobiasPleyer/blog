from subprocess import run, PIPE, STDOUT


def shell(cmd):
    print(f"Now running: {cmd}")
    process = run(cmd, stdout=PIPE, stderr=STDOUT, shell=True)
    print(process.stdout.decode('utf-8'))
    if process.returncode == 0:
        return True
    else:
        print("An error occurred. Aborting")
        return False

def main():
    ok = shell("echo 'OK'; exit 0")
    if ok:
        ok = shell("echo 'Again OK'; exit 0")
        if ok:
            ok = shell("echo 'Problem!'; exit 1")
            if ok:
                ok = shell("echo 'Should not execute'; exit 0")
                if ok:
                    print("Done")

if __name__ == '__main__': main()
