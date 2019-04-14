from transformers import *


def act_on_branch(branch):
    if branch == 'master':
        return shell("echo 'action for master'")
    else:
        return shell("echo 'Other action'")

def stackReturn(value):
    return EitherT(WriterT(IO.mreturn((right(value),[]))))

action = shell('git branch') |(
         # Extract the branch name from the git output
         lambda bOut: stackReturn(bOut.strip().split()[-1]) |(
         # Behave different depending on branch
         lambda branch: act_on_branch(branch) >>
         shell("echo 'command independent of previous commands'") >>
         # The 'branch' parameter remains in scope
         shell(f"echo {branch}") >>
         shell('git tag | wc -l') |(
         # Convert the string output to an integer
         lambda cnt: stackReturn(int(cnt)) |(
         lambda n: shell(f"/bin/zsh -c 'for i in {{1..{n}}}; do; echo 'Branch!'; done'")
         ))))

res, info = runAction(action)
print(f"Final result: {res}")
print("== INFO ==")
print(reduce(lambda x, y: x + '\n' + y, info))
