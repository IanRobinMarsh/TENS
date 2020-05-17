#! /usr/bin/env python

import os

indir = "Pdf"
outdir = "Png"
quality = 300

def main():
    dir_list = os.listdir(".")
    for full_file_name in dir_list:
        base_name, extension = os.path.splitext(full_file_name)
        if extension == '.pdf': 
            cmd_str = ' '.join(['convert',
                                '-density 300',
				'-flatten',
                                full_file_name,
                                base_name + '.png'])
            print(cmd_str)  # echo command to terminal
            os.system(cmd_str)  # execute command

if __name__ == '__main__':
    main()