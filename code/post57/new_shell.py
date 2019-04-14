def do_shell(cmd):
    process = run(cmd, stdout=PIPE, stderr=PIPE, shell=True)
    stdout = process.stdout.decode('utf-8')
    stderr = process.stderr.decode('utf-8')
    info = [(3,f"Command run: {cmd}")]
    if stdout:
        info.append((2,stdout))
    if stderr:
        info.append((1,stderr))
    if process.returncode > 0:
        return (left(stderr), info)
    return (right(stdout), info)
