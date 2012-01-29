#!/usr/bin/env python
'''
Good enough for now. Read a ReST file and extract all the code definitions
and print them out. Use this to transform avrvm.rest to avrvm.asm.
'''
import sys
from BeautifulSoup import BeautifulSoup
from docutils.core import publish_parts

if len(sys.argv) > 1:
    data = open(sys.argv[-1]).read()
else:
    data = sys.stdin.read()

parts = publish_parts(data, writer_name='html')
soup = BeautifulSoup(parts['html_body'])

for p in soup.findAll('pre', { "class" : "literal-block" }):
    for c in p.contents:
        sys.stdout.write(c)
