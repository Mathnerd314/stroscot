from fabricate import *

def build():
    run('gcc', '-O3', 'test.c', '-o', 'test')

def clean():
    autoclean()

main()