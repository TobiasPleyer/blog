import dis

def func():
    import simple
    from parent import sub
    return 0

print(dis.dis(func))
