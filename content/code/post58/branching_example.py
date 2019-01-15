from transformers import *
from do_final import do


def act_on_branch(branch):
    if branch == 'master':
        return shell("echo 'action for master'")
    else:
        return shell("echo 'Other action'")

action = eval(do("""
        bOut <- shell('git branch')
        let branch = bOut.strip().split()[-1]
        act_on_branch(branch)
        shell("echo 'command independent of previous commands'")
        shell(f"echo {branch}")
        cnt <- shell('git tag | wc -l')
        let n = int(cnt)
        shell(f"/bin/zsh -c 'for i in {{1..{n}}}; do; echo 'Branch!'; done'")
        """))

res, info = runAction(action)
print(f"Final result: {res}")
print("== INFO ==")
print(reduce(lambda x, y: x + '\n' + y, info))
