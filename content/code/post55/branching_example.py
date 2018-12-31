from transformers import *


# The low level function fetching the current branch info from git
def cmd_with_return(cmd):
    process = run(cmd, stdout=PIPE, stderr=STDOUT, shell=True)
    stdout = process.stdout.decode('utf-8')
    info = [f"Command run: {cmd}", stdout]
    if process.returncode > 0:
        return (left(""), info)
    return (right(stdout), info)

def get_branch():
    return EitherT(WriterT(IO(lambda: cmd_with_return("git branch"))))

def get_tag_count():
    return EitherT(WriterT(IO(lambda: cmd_with_return("git tag | wc -l"))))

def act_on_branch(branch):
    if branch == 'master':
        return shell("echo 'action for master'")
    else:
        return shell("echo 'Other action'")

def stackReturn(value):
    return EitherT(WriterT(IO.mreturn((right(value),[]))))

action = get_branch() |(
         # Extract the branch name from the git output
         lambda bOut: stackReturn(bOut.strip().split()[-1]) |(
         # Behave different depending on branch
         lambda branch: act_on_branch(branch) >>
         shell("echo 'command independent of previous commands'") >>
         # The 'branch' parameter remains in scope
         shell(f"echo {branch}") >>
         get_tag_count() |(
         # Convert the string output to an integer
         lambda cnt: stackReturn(int(cnt)) |(
         lambda n: shell(f"/bin/zsh -c 'for i in {{1..{n}}}; do; echo 'Branch!'; done'")
         ))))

res, info = runAction(action)
print(f"Final result: {res}")
print("== INFO ==")
for i in info:
    print(i)
