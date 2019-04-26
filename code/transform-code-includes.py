import re
import sys

reCodeInclude = re.compile(r'[:]+\s*{(?P<attrs>\.code-include\s+lexer="\w+")}\s*\n(?P<file>[\w\d/\\._-]+)\n[:]+\s*$', re.MULTILINE)

def rewriteCodeInclude(match):
    attrs = match.group('attrs')
    filename = match.group('file').strip().replace('\\','')
    return f'::: {{{attrs} file="{filename}"}}\n:::'

filename = sys.argv[1] 

with open(filename, 'r') as fp:
    content = fp.read()

new_content = reCodeInclude.sub(rewriteCodeInclude, content)

with open(filename, 'w') as fp:
    fp.write(new_content)
