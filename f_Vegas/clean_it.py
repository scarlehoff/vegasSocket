#!/usr/bin/env python3

""" Removes all the [remove_tag]-only stuff from Vegas.f90 """

# To clean openmp directives a simple ~$ sed -i '/!\$/ d' Vegas.f90 
# would suffice

remove_tag = "USE_NNLOJET"

file_in = "Vegas.f90"
write_down = True

lines = []
with open(file_in, 'r') as f:
    for line in f:
        if remove_tag in line:
            write_down = False
        if write_down:
            lines.append(line)
        if ("#endif" in line) and (not write_down):
            write_down = True

with open(file_in, 'w') as f:
    f.writelines(lines)
