from transformers import *


# The low level function fetching the current branch info from git
def do_get_branch():
    process = run("git branch", stdout=PIPE, stderr=STDOUT, shell=True)
    stdout = process.stdout.decode('utf-8')
    if process.returncode > 0:
        return (left(""), [stdout])
    branch = stdout.strip().split()[-1]
    return (right(branch), [])

def get_branch():
    return EitherT(WriterT(IO(lambda: do_get_branch())))

action = get_branch() |(
         lambda b: (shell("echo 'action for master'") if b == 'master' else shell("echo 'Other action'")) >>
         shell("echo 'command independent of previous commands'")
         )

res, info = runAction(action)
print(f"Final result: {res}")
print("== INFO ==")
for i in info:
    print(i)
