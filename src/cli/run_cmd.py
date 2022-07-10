""" cun a cli on a directory of files """
import glob
import os.path
import re
import sys
from subprocess import check_output, CalledProcessError

assert len(sys.argv) == 3, "usage: python run_cmd.py input_glob output_file"
program_name = sys.argv[0]
input_glob = sys.argv[1]
output_file_path_suffix = sys.argv[2]

print(f"program_name = {program_name}")
print(f"input_glob = {input_glob}")
print(f"output_file_path_suffix = {output_file_path_suffix}")

total = len(glob.glob(input_glob))
count = 0
for input_file_path in glob.glob(input_glob):
    count += 1
    print(f"count/total={count}/{total}")
    cmd = ["gp", "--quiet", input_file_path]
    input_file_path_base, input_file_path_ext = os.path.splitext(input_file_path)
    input_file_path_head, input_file_path_tail = os.path.split(input_file_path_base)

    try:
        out_bytes = check_output(cmd)
    except CalledProcessError as error_message:
        out_bytes = error_message
        print(f"unable to slice with error: {error_message}")
        continue

    out_str = out_bytes.decode("utf-8")
    out_str = re.sub(string=out_str, pattern=r"GP/PARI CALCULATOR.*WHATSOEVER\.", repl="")
    out_str = out_str.replace("Goodbye!", "")

    with open(f"{input_file_path_base}_{output_file_path_suffix}.{input_file_path_ext}", "w") as output_file:
        output_file.write(out_str)
