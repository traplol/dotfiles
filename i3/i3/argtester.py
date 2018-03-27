#!/usr/bin/python3
import sys, os, random
#call('echo "{}" | xclip -sel clip'.format(str(sys.argv)))
out = random.random()#str(sys.argv).replace('"', '\\"')
os.system('echo "{}" | xclip -sel clip'.format(out))
print(out)
print(out)

