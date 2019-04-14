def main(verbosity_level):
    cmd1 = shell("echo 'OK'; exit 0")
    cmd2 = shell("echo 'some warning' >&2; echo 'Also OK'; exit 0")
    cmd3 = shell("echo 'Even better'; exit 0")
    full_action = cmd1 >> cmd2 >> cmd3
    res, info = runAction(full_action)
    print(f"Final result: {res}")
    print("== INFO ==")
    print(filter_log(verbosity_level, info))
    cmd4 = shell("echo 'Command failed' >&2; exit 1")
    full_action = cmd1 >> cmd4 >> cmd3
    res, info = runAction(full_action)
    print(f"Final result: {res}")
    print("== INFO ==")
    print(filter_log(verbosity_level, info))
