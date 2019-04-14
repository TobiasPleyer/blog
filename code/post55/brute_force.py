from subprocess import run, PIPE, STDOUT


def shell(cmd):
    print(f"Now running: {cmd}")
    process = run(cmd, stdout=PIPE, stderr=STDOUT, shell=True, check=True)
    print(process.stdout.decode('utf-8'))

def main():
    try:
        shell("echo 'OK'; exit 0")
        shell("echo 'Again OK'; exit 0")
        shell("echo 'Problem!'; exit 1")
        shell("echo 'Should not execute'; exit 0")
    except:
        print("An error occurred. Aborting")
    else:
        print("Done")

if __name__ == '__main__': main()
