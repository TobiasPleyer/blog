from transformers import *

def create_tag(refspec, tagname, message):
    return (shell(f"git show {refspec}") >>
            shell(f"git tag -m '{message}' {tagname} {refspec}"))

tagAction = (create_tag('df40cfe', 'v1.0', 'First release') >>
             create_tag('851a93b', 'v2.0', 'Second release') >>
             # Ups I made an error - tag already exists
             create_tag('961090f', 'v1.0', 'Third release') >>
             create_tag('32d3085', 'v4.0', 'Forth release')
            )

res, info = runAction(tagAction)
print(f"Final result: {res}")
print("== INFO ==")
for i in info:
    print(i)
